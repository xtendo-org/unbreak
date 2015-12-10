**unbreak** is a minimal helper utility to have files accessible, encrypted, and stored remotely, even with an unstable Internet connection. Questions, bug reports, feature requests, and pull requests are welcome.

## In a nutshell

1. You type `u cheatsheet.txt`
1. You start editing your cheat sheet text file that is encrypted and stored remotely.
1. You save and quit.
1. The new file is now encrypted and stored remotely.

## Installation

One binary file for x64 GNU/Linux.

```bash
curl -L https://github.com/kinoru/unbreak/releases/download/v0.2.0/u > ~/.local/bin/u && chmod +x ~/.local/bin/u
```

The recommendation is to set the file name to `u`, but you may use the full name `unbreak` or anything you prefer.

For the `u` executable to properly run, The following must be available on the client system.

- `/dev/urandom`
- `mktemp`
- `/dev/shm` writable by you
- `scp`
- The `$HOME` environment variable

It will probably work on any ordinary x64 GNU/Linux distribution.

## Usage

When you run `u` for the first time, the file `~/.unbreak.json` will be created with default configuration. Edit it and configure unbreak.

- `name`: This works as a salt to fight against the rainbow table attack to generate the master secret key. It's a salt, so it doesn't have to be secret. Just pick something memorable and unique. [xkcd offers a brilliant advice](https://xkcd.com/936/).
- `remote`: The path of the remote directory that will serve as a storage of all documents. (For now, it must exist.) The document name will be appended to this path when it is passed to the `scp` command.
- `editor`: The default value is [`vim`](https://e.xtendo.org/scs/vim).

Now you type `u cheatsheet.txt` to actually begin editing a file. Since this is your first time using Unbreak and there is no session yet, you'll be asked to type a password that will be used (with the salt in `~/.unbreak.json`) to generate the master key. The master key is stored under `/dev/shm` during the session.

## How it works

1. You type `u cheatsheet.txt`
    1. `scp username@your.remote.storage:path/to/docs/YLfZ9XA4YGYNRykW4Ns= /dev/shm/unbreak-Z9y8X/`
    1. Decrypt the file with your master key and save it as `cheatsheet.txt`
    1. `vim /dev/shm/unbreak-Z9y8X/cheatsheet.txt`
1. You edit your cheat sheet text file with your favorite text editor.
1. You save and quit.
    1. Encrypt the file with your master key and save it as `YLfZ9XA4YGYNRykW4Ns=`
    1. `scp /dev/shm/unbreak-Z9y8X/YLfZ9XA4YGYNRykW4Ns= username@your.remote.storage:path/to/docs/`

## Why?

Recently a type of operating system instances became very common. They are

- not maintained (in terms of electricity supply, thermal control, Internet connection, etc.) by you
- not in your vicinity
- not under your ownership

Some people seem to prefer the term "cloud."

What you want to do is, in the terminal, you would type `u comic.zip` and a comic book viewer will let you read the comic book that is encrypted and stored remotely. Or `u cheatsheet.txt` will open a text editor and let you edit your cheat sheet text file. After a while you will save and quit, then the document will be encrypted and stored in the remote operating system instance. Certain conditions apply:

- You want the whole process be resilient to an **unstable Internet connection**. Bringing your laptop around or even suspending it breaks the connection, so you want it to require the connection only when it's needed, that is, the opening and saving moment.
- Following the spirit of the excellent [Tarsnap](https://www.tarsnap.com/) service, you want the file to be **client-side-encrypted** before being sent to the remote system. The notion that you have _your_ instance in the "cloud" is an illusion; the company that runs this "cloud" service has a full technical (and, in many cases, even legal) access to your data.

Alternatives all fell short:

- Saving locally with full-disk encryption: The data becomes inaccessible from different (home/office) computers.
- sshfs: Very good, and very fragile with an unstable Internet connection. Sometimes it makes the whole I/O hang. (You have to manually `umount` with `-l` option.)
- Vim with netrw scp support: Can't encrypt/decrypt with a modern cipher like ChaCha20, unless you learn Vimscript and write a plugin of your own. Some editors don't even have the scripting feature at all.

## TODO

- Iteratee I/O in encryption and decryption
- File listing
- Automatically choose the right "editor" based on the file extension
- Arbitrarily choose file to upload
- Manual session logout
