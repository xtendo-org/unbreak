# Change Log

## 0.2.0 - 2015-12-09

- Add encryption of file names: File names may be a sensitive information as well. They are now encrypted with ChaCha20-Poly1305 and base64url-encoded.
- Avoid network overhead by checking file mtype: The file won't be sent if it hasn't been changed.

## 0.1.0 - 2015-12-05

- Prepended a single byte in the encrypted file format to indicate the "version number." The byte isn't really used for anything useful yet; it's for forward compatibility.
