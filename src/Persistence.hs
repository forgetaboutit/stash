{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Persistence where

import           Control.Monad.IO.Class (MonadIO)
import           Core
import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as BS
import qualified Data.Serialize         as DS
import           Debug.Trace
import qualified Pipes                  as P
import           Pipes.ByteString       (fromHandle, toHandle)
import           Pipes.Cereal           (decode)
import           Pipes.Parse            (Parser, foldAll, parsed, runStateT)
import           Pipes.Safe             (Base, MonadCatch, MonadSafe, SafeT,
                                         runSafeT)
import qualified Pipes.Safe             as PSafe
import qualified System.IO              as IO
import           Types
import Data.Digest.Pure.SHA (integerDigest, hmacSha512)
import Control.Lens

-- | Serialized chunk of data
-- The chunk contains the size of the content as well as the its HMAC.
-- The size only includes the size of the content.  The invariants are
-- encoded in the @verifyChunk function.
data Chunk = Chunk
  { _size    :: Int
  , _content :: ByteString
  , _hmac    :: Integer
  } deriving (Show)

-- | Verifies a HMAC from a secret key
mkHmacVerifier :: ByteString -> ByteString -> Integer
mkHmacVerifier = hmac
  where
    hmac key msg = integerDigest $ hmacSha512 (view lazy key) (view lazy msg)

-- | Validates size and hash of a chunk
verifyChunk :: (ByteString -> Integer) -> Chunk -> Bool
verifyChunk hmac chunk = isHmacValid && isSizeValid
  where
    hmacContent = hmac $ _content chunk
    isHmacValid = _hmac chunk == hmacContent
    isSizeValid = _size chunk == (BS.length $ _content chunk)

-- -- | A command which can be persisted to a binary file
-- data SerializableCommand = SerializableCommand
--   { _size    :: Int
--   , _command :: Command
--   , _hash    :: Int -- Probably larger
--   } deriving (Show)
readFile' ::
     (PSafe.Base m ~ IO, MonadSafe m)
  => FilePath
  -> P.Producer' BS.ByteString m ()
readFile' file =
  PSafe.bracket (IO.openBinaryFile file IO.ReadMode) (IO.hClose) fromHandle

appendFile ::
     (PSafe.Base m ~ IO, MonadSafe m, MonadIO m, MonadCatch m)
  => FilePath
  -> P.Consumer' BS.ByteString m ()
appendFile file =
  PSafe.bracket (IO.openBinaryFile file IO.AppendMode) (IO.hClose) toHandle

mkParser ::
     (DS.Serialize r, Monad m) => Parser BS.ByteString m (Either String r)
mkParser = decode

intParser :: (Monad m) => Parser BS.ByteString m (Either String Integer)
intParser = mkParser

intProducer ::
     (Base m ~ IO, MonadSafe m)
  => P.Producer Integer m (String, P.Producer BS.ByteString m ())
intProducer = parsed intParser $ readFile' "unit.bin"

-- foo ::
--      IO ( [Integer]
--         , P.Producer Integer (SafeT IO) ( String
--                                         , P.Producer BS.ByteString (SafeT IO) ()))
-- foo =
--   runSafeT $ runStateT ((foldAll applyCommand) AppState Prelude.id) intProducer

-- bar :: IO ([Integer], String)
-- bar = do
--   (res, leftovers) <- foo
--   r <- drainLeftovers leftovers
--   return (res, r)
--   where
--     drainLeftovers ::
--          P.Producer Integer (SafeT IO) ( String
--                                        , P.Producer BS.ByteString (SafeT IO) ())
--       -> IO String
--     drainLeftovers c = do
--       nxt <- runSafeT $ P.next c
--       case nxt of
--         Left (r, _)   -> pure r
--         Right (_, c') -> trace "More values await" $ drainLeftovers c'
