{-# LANGUAGE OverloadedStrings #-}

module GitHub.WebHook.Handler
  ( Handler(..)
  , Error(..)
  , runHandler
  ) where


import           Crypto.Hash

import           Data.Aeson (eitherDecodeStrict')
import           Data.Aeson.Types (parseEither)

import           Data.Monoid

import           Data.Text
import qualified Data.Text as T
import           Data.Text.Encoding

import           Data.ByteString
import qualified Data.ByteString.Char8 as BC8

import           Data.UUID

import           GitHub.Types



data Handler m = Handler
    { hSecretKey :: Maybe String
      -- ^ Optional key which is used to authenticate the incoming request.

    , hBody :: m ByteString
      -- ^ Action which is used to read the request body.

    , hHeader :: ByteString -> m (Maybe ByteString)
      -- ^ Action which is used to retrieve a particular header from the
      -- request.

    , hAction :: Either Error (UUID, Event) -> m ()
      -- ^ Action which is executed once we've processed all the information
      -- in the request. GitHub includes a unique UUID in each request.
    }


data Error
    = InvalidRequest
      -- ^ The incoming request is not well-formed. If that happens it means
      -- GitHub screwed something up, or changed the format of the request.

    | ParseError !Text
      -- ^ The request looks valid but we failed to parse the payload. This
      -- could be because our parsing code is wrong, or because GitHub added
      -- a new event type which we don't handle yet.

    | UnsignedRequest
      -- ^ We were expecting a signed request but no signature was included.
      -- Such requests are rejected beause we don't want to accept requests from
      -- untrusted sources.

    | InvalidSignature
      -- ^ A signature was included in the request but it did not match the
      -- secret key which was providid to the handler. Usually points to
      -- a configuration error on either end.


toParseError :: String -> Either Error Event
toParseError = Left . ParseError . T.pack


runHandler :: Monad m => Handler m -> m ()
runHandler h = do
    mbDelivery <- return . (fromASCIIBytes =<<) =<< hHeader h "X-GitHub-Delivery"

    res <- do
        rawBody     <- hBody h
        mbSignature <- hHeader h "X-Hub-Signature"

        authenticatedBody <- return $ case (hSecretKey h, mbSignature) of

            -- No secret key and no signature. Pass along the body unverified.
            (Nothing, Nothing) -> Right rawBody

            -- Signature is available but no secret key to verify it. This is
            -- not a fatal error, we can still process the event.
            (Nothing, Just _) -> Right rawBody

            -- Secret token is available but the request is not signed. Reject
            -- the request.
            (Just _, Nothing) -> Left UnsignedRequest

            -- Both the signature and secret token are available. Verify the
            -- signature and reject the request if that fails.
            (Just sc, Just sig) -> do
                let mac = hmac (BC8.pack sc) rawBody :: HMAC SHA1
                if sig == ("sha1=" <> digestToHexByteString (hmacGetDigest mac))
                    then Right rawBody
                    else Left InvalidSignature

        mbEventName <- hHeader h "X-GitHub-Event"
        return $ do
            eventName <- maybe (Left InvalidRequest) Right mbEventName
            body      <- authenticatedBody
            case eitherDecodeStrict' body of
                Left e -> toParseError e
                Right value -> either toParseError Right $
                    parseEither (eventParser $ decodeUtf8 eventName) value

    hAction h $ case mbDelivery of
        Nothing -> Left InvalidRequest
        Just uuid -> fmap ((,) uuid) res
