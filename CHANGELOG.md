# Change Log

## 0.3.0 - 2015-12-15

- The command line arguments interface has changed to multi-mode. Users are now advised to set `u` as an alias of `unbreak open`.
    - `unbreak init`
    - `unbreak open <FILENAME>`
    - `unbreak logout`
    - `unbreak add [OPTIONS] <FILENAME>`
- Added `unbreak add`: Pick a local file, encrypt it, and put it in the remote storage
- Added `unbreak logout`: Explicitly remove the session files from the memory space.
- Added automatic removal of local temporary files in the memory space after `unbreapk open`.
- Added upload error remedy to `unbreak open`: the user's effort won't be immediately lost even when the upload fails. Now there is a chance to retry or save it somewhere else.
- Fixed file name encryption: Very silly elemetary cryptographic mistake. Eliminated the chance of adversary guessing the file name when there are multiple files with the same prefix.

## 0.2.0 - 2015-12-09

- Added encryption of file names: File names may be a sensitive information as well. They are now encrypted with ChaCha20-Poly1305 and base64url-encoded.
- Avoid network overhead by checking file mtype: The file won't be sent if it hasn't been changed.

## 0.1.0 - 2015-12-05

- Prepended a single byte in the encrypted file format to indicate the "version number." The byte isn't really used for anything useful yet; it's for forward compatibility.
