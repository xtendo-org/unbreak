module Crypto
    ( getRandomBytes
    , scrypt
    , encrypt
    , decrypt
    , module Crypto.Error
    ) where

import Prelude hiding ((++))
import System.IO
import Data.ByteString (ByteString, hGet)
import qualified Data.ByteString.Char8 as B

import Data.ByteArray (convert)
import Crypto.Error
import Crypto.KDF.Scrypt
import qualified Crypto.Cipher.ChaChaPoly1305 as C

(++) :: Monoid m => m -> m -> m
(++) = mappend

getRandomBytes :: Int -> IO ByteString
getRandomBytes n = withFile "/dev/urandom" ReadMode $ \ h -> hGet h n

scrypt
    :: ByteString -- ^ input
    -> ByteString -- ^ salt
    -> ByteString -- ^ output (256-bit)
scrypt = generate (Parameters 128 8 1 32)

encrypt :: ByteString -> ByteString -> ByteString -> CryptoFailable ByteString
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

decrypt :: ByteString -> ByteString -> CryptoFailable ByteString
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
    | B.length input < 17 =
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
