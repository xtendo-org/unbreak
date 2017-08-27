-- |
-- Module       : Unbreak.Crypto
-- License      : AGPL-3
-- Maintainer   : XT
-- Stability    : Provisional
-- Portability  : POSIX
--
-- Simple interface to the cryptographic primitives that are provided by
-- the <https://hackage.haskell.org/package/cryptonite cryptonite> package.
module Unbreak.Crypto
    ( getRandomBytes
    , scrypt
    , encrypt
    , decrypt
    , encryptFileName
    , decryptFileName
    , module Crypto.Error
    ) where

import Prelude hiding ((++))
import System.IO
import Data.ByteString (ByteString, hGet)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Builder as B
import Data.Serialize.Get

import Data.ByteArray (convert)
import Crypto.Error
import Crypto.KDF.Scrypt
import qualified Crypto.Cipher.ChaChaPoly1305 as C

(++) :: Monoid m => m -> m -> m
(++) = mappend

-- | Read bytes from @\/dev\/urandom@.
getRandomBytes :: Int -> IO ByteString
getRandomBytes n = withFile "/dev/urandom" ReadMode $ \ h -> hGet h n

-- | The <https://www.tarsnap.com/scrypt.html scrypt>
-- key derivation function from "Crypto.KDF.Scrypt". The parameters are:
--
-- * CPU/memory cost parameter N = 16384 (2^14)
-- * SMix function parameter r = 8
-- * Parallelization parameter p = 1
-- * Intended output length dkLen = 32 (for use in ChaCha20-Poly1305)
scrypt
    :: ByteString -- ^ input
    -> ByteString -- ^ salt
    -> ByteString -- ^ output (32 bytes)
scrypt = generate (Parameters 16384 8 1 32)

-- | Encrypt the given 'ByteString' using the
-- <https://tools.ietf.org/html/rfc7539 ChaCha20-Poly1305> scheme
-- from "Crypto.Cipher.ChaChaPoly1305".
-- The resulting 'ByteString' is nonce (12 bytes) ++ ciphertext ++
-- the auth tag (16 bytes).
encrypt
    :: ByteString -- ^ nonce (12 random bytes, must be different each time)
    -> ByteString -- ^ the secret symmetric key
    -> ByteString -- ^ the plaintext to be encrypted
    -> CryptoFailable ByteString -- ^ ciphertext with a 128-bit tag attached
encrypt nonce key plaintext = (nonce ++) <$> encrypt' nonce key "" plaintext

encrypt'
    :: ByteString -- nonce (12 random bytes)
    -> ByteString -- symmetric key
    -> ByteString -- optional associated data (won't be encrypted)
    -> ByteString -- input plaintext to be encrypted
    -> CryptoFailable ByteString -- ciphertext with a 128-bit tag attached
encrypt' nonce key header plaintext = do
    st1 <- C.nonce12 nonce >>= C.initialize key
    let
        st2 = C.finalizeAAD $ C.appendAAD header st1
        (out, st3) = C.encrypt plaintext st2
        auth = C.finalize st3
    return $ out ++ Data.ByteArray.convert auth

-- | Encryption of unbreak file names.
encryptFileName
    :: ByteString -- ^ file name
    -> ByteString -- ^ key
    -> ByteString
encryptFileName key fileName = nonce ++ encryptNoAuth nonce key
    (mconcat [word32LEencode oLen, fileName, padding])
  where
    nonce = B.take 12 $ B.drop 10 $ scrypt fileName key
    oLen = B.length fileName
    -- 4: the length (in 32 bits) always occupies 4 bytes
    -- 12: the nonce always occupies 12 bytes
    -- -1: to make the result 48 when the input is 44, or 96 when 92, etc.
    goalLen = (((oLen + 15) `div` 48) + 1) * 48
    padding = B.pack $ replicate (goalLen - 16 - oLen) '\0'

-- | Encryption without the auth tag and without the optional header.
-- 'encrypt' is almost always the better choice. Use this function only when
-- you know what you are doing.
encryptNoAuth
    :: ByteString -- ^ nonce (12 random bytes)
    -> ByteString -- ^ the secret symmetric key
    -> ByteString -- ^ input plaintext to be encrypted
    -> ByteString -- ^ the resulting ciphertext
encryptNoAuth nonce key plaintext = throwCryptoError $ do
    st1 <- C.nonce12 nonce >>= C.initialize key
    let
        st2 = C.finalizeAAD st1
        (out, _) = C.encrypt plaintext st2
    return out

-- | Decrypt a 'ByteString' that is produced by the 'encrypt' function.
decrypt
    :: ByteString -- ^ the secret symmetric key
    -> ByteString -- ^ the input (nonce ++ ciphertext ++ tag)
    -> CryptoFailable ByteString -- ^ the decrypted plaintext
decrypt key input = decrypt' nonce key "" ciphertextWithTag
  where
    (nonce, ciphertextWithTag) = B.splitAt 12 input
    -- TODO: What if input is shorter than 12

decrypt'
    :: ByteString -- the nonce used for encryption
    -> ByteString -- symmetric key
    -> ByteString -- optional associated data
    -> ByteString -- ciphertext with the tag attached
    -> CryptoFailable ByteString -- ^ decrypted plaintext
decrypt' nonce key header input
    | B.length input < 16 =
        CryptoFailed CryptoError_AuthenticationTagSizeInvalid
    | otherwise = case decryptionAttempt of
        CryptoPassed (decrypted, auth)
            | Data.ByteArray.convert auth == tag -> return decrypted
            | otherwise -> CryptoFailed CryptoError_MacKeyInvalid
        CryptoFailed x -> CryptoFailed x
  where
    (ciphertext, tag) = B.splitAt (B.length input - 16) input
    decryptionAttempt = do
        st1 <- C.nonce12 nonce >>= C.initialize key
        let
            st2 = C.finalizeAAD $ C.appendAAD header st1
            (out, st3) = C.decrypt ciphertext st2
            auth = C.finalize st3
        return (out, auth)

-- | Decryption of unbreak file names.
decryptFileName
    :: ByteString
    -> ByteString
    -> ByteString
decryptFileName key encrypted = B.take oLen $ B.drop 4 decrypted
  where
    (nonce, ciphertext) = B.splitAt 12 encrypted
    decrypted = decryptNoAuth nonce key ciphertext
    oLen = word32LEdecode decrypted

-- | Decryption without the auth tag checking.
decryptNoAuth
    :: ByteString -- ^ the nonce used for encryption
    -> ByteString -- ^ the secret symmetric key
    -> ByteString -- ^ input ciphertext to be decrypted
    -> ByteString -- ^ the resulting plaintext
decryptNoAuth nonce key ciphertext = throwCryptoError $ do
    st1 <- C.nonce12 nonce >>= C.initialize key
    let
        st2 = C.finalizeAAD st1
        (out, _) = C.decrypt ciphertext st2
    return out

word32LEencode :: Int -> ByteString
word32LEencode = LB.toStrict . B.toLazyByteString . B.word32LE . fromIntegral

word32LEdecode :: ByteString -> Int
word32LEdecode = fromIntegral .
    either (const $ error "word32LE decode fail") id .
    runGet getWord32le
