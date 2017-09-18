**unbreak** is a tool that lets you have a secure and resilient remote file storage.

Current version: **0.4.0** ([change log](https://github.com/xtendo-org/unbreak/blob/master/CHANGELOG.md))

unbreak is released under AGPL-3. Questions, bug reports, feature requests, and pull requests are highly welcome.

## Advantages

- Zero setup on the server side: You only need a working SSH server and an empty directory.
- Extremely little setup on the client side: You only need a single executable and a short configuration file.
- Client-side encryption: Unless your client system is stolen, it is nearly impossible for anyone else to guess what's in your storage, even for the VPS providers like Google, Amazon, etc.
- Resilience to connection instability: You can move your laptop around, suspend it, change the IP address, and unbreak will work fine, hence the name.

## In a nutshell

1. You type `u cheatsheet.txt`
1. You start editing your cheat sheet text file.
1. You save and quit.
1. The edited file is encrypted and stored remotely.

## Installation

One binary file for x64 GNU/Linux.

```bash
curl -L \
    https://github.com/xtendo-org/unbreak/releases/download/v0.4.0/unbreak \
    > ~/.local/bin/unbreak \
    && chmod +x ~/.local/bin/unbreak
```

(Assuming `~/.local/bin` exists and is in your `$PATH`.)

You are also recommended to set alias `u` for `unbreak open`. Include in your shell configuration (`~/.bashrc` by default):

```bash
alias u="unbreak open"
```

### Requirements

For the `unbreak` executable to properly run, The following must be available on the client system.

- `/dev/urandom`
- `mktemp`
- `/dev/shm` (the default shared memory space) writable by you
- `ssh` and `scp`
- The `$HOME` environment variable

In other words, it will probably work on any ordinary x64 GNU/Linux distribution. Support for other POSIX systems (such as BSD or OS X) is planned.

### Build from source

- [Stack](http://haskellstack.org/) is the recommended method to build the latest development version of unbreak.

```bash
$ git clone --depth=1 https://github.com/xtendo-org/unbreak
$ cd unbreak
$ stack install
```

## Usage

Run `unbreak init` and the file `~/.unbreak.json` will be created with default configuration. Edit it and configure unbreak.

- `name`: This works as a salt to fight against the rainbow table attack to generate the master secret key. It's a salt, so it doesn't have to be secret. Just pick something memorable and unique. [xkcd offers a brilliant advice](https://xkcd.com/936/).
- `remote`: The path of the remote directory that will serve as a storage of all documents. (For now, it must exist.) The document name will be appended to this path when it is passed to the `scp` command.
- `editor`: The default value is [`vim`](https://e.xtendo.org/scs/vim).

Now you type `u cheatsheet.txt` to actually begin editing a file. Since this is your first time using unbreak and there is no session yet, you'll be asked to type a password that will be used to generate the master key (along with the salt in `~/.unbreak.json`). The master key is stored under `/dev/shm` during the session.

## How it works

1. You type `u cheatsheet.txt`
    1. Download the encrypted file from the remote storage with `scp`.
    1. Decrypt the file with your master key and save it temporarily as `cheatsheet.txt`
    1. Open this file with your chosen editor.
1. You edit your cheat sheet text file with your favorite text editor.
1. You save and quit.
    1. Encrypt the file with your master key and save it temporarily as `YLfZ9XA4YGYNRykW4Ns=`
    1. Upload the encrypted file to the remote storage with `scp`.
    1. Clean up the files and terminate.

## Why?

Recently a type of operating system instances became very common. They are

- not maintained (in terms of electricity supply, thermal control, Internet connection, etc.) by you
- not in your vicinity
- not under your ownership

Some people seem to prefer the term "cloud."

What you want to do is, in the terminal, you would type `u comic.zip` and a comic book viewer will let you read the comic book that is encrypted and stored remotely. Or `u cheatsheet.txt` will open a text editor and let you edit your cheat sheet text file. After a while you will save and quit, then the document will be encrypted and stored in the remote operating system instance. Certain conditions apply:

- You want the whole process be resilient to an **unstable Internet connection**. Bringing your laptop around or even suspending it breaks the connection, so you want it to require the connection only when it's needed, that is, the opening and saving moment.
- Following the spirit of the excellent [Tarsnap](https://www.tarsnap.com/) service, you want the **client-side encryption** before the file is sent to the remote system. The notion that you have _your_ instance in the "cloud" is an illusion; the company that runs this "cloud" service has a full technical (and, in many cases, even legal) access to your data.

Alternatives all fell short:

- Saving locally with full-disk encryption: The data becomes inaccessible from different (home/office) computers.
- sshfs: Very good, and very fragile with an unstable Internet connection. Sometimes it makes the whole I/O hang. (You have to manually `umount` with `-l` option.)
- Vim with netrw scp support: Can't encrypt/decrypt with a modern cipher like ChaCha20, unless you learn Vimscript and write a plugin of your own. Some editors don't even have the scripting feature at all.
