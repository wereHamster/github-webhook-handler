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

import           GitHub.Types



data Handler m = Handler
    { hSecretKey :: Maybe String
      -- ^ Optional key which is used to authenticate the incoming request.

    , hBody :: m ByteString
      -- ^ Action which is used to read the request body.

    , hHeader :: ByteString -> m (Maybe ByteString)
      -- ^ Action which is used to retrieve a particular header from the
      -- request.

    , hAction :: Either Error Event -> m ()
      -- ^ Action which is executed once we've processed all the information
      -- in the request.
    }


data Error
    = InvalidRequest
    | ParseError !Text
    | UnsignedRequest
    | InvalidSignature

toParseError :: String -> Either Error Event
toParseError = Left . ParseError . T.pack

runHandler :: Monad m => Handler m -> m ()
runHandler h = do
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

    hAction h res
