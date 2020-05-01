<!--- Copyright (c) 2020 Andrey Dubovik <andrei@dubovik.eu> --->

# Ansible

## Disclaimer

This library is very much work in progress. I will be keeping this readme updated as development continues.

The library runs on SBCL. I am still undecided whether to fully stick with SBCL or to provide a compatibility layer for other LISP platforms.

Currently, it is possible to download a popular torrent or two. Less popular torrents might not work as there is no upload functionality yet and the client will likely get choked. The code has evolved along with my understanding of the protocol, and while there is much scope for improvement, hopefully it is not too messy. There is no interface at the moment.

## Example Usage

**First**, load the package somehow, e.g.,

```lisp
(push *default-pathname-defaults* asdf:*central-registry*)
(require :ansible)
(in-package :ansible)
```

There will be some warnings as not all files are in a correct order...

**Second**, start a log service:

```lisp
(open-log "~/ansible/log.txt")
```

A log service is not strictly necessary, but right now there is no other way to see progress. At least this way, you can open the file with `less`, hit `F`, and enjoy the show.

**Third**, we will need to set the downloads directory. There are a few other parameters scattered in the code (always at the top of source files), but this one is the only one required.

```lisp
(defparameter *file-dir* "~/ansible/downloads/") ; don't forget the trailing slash, no proper handling of pathnames yet...
```

**Fourth**, initiate a download:

```lisp
(let ((torrent (read-torrent "/path/to/torrent/file")))
  (start torrent))
```

A SOCKS5 proxy can be used, in which case the tracker request as well as peer connections are tunnelled via SOCKS. (Has been tested with SSH tunnelling.)

```lisp
(let ((torrent (read-torrent "/path/to/torrent/file")))
  (start torrent :proxy '(#(127 0 0 1) . 1080)))
```

A blacklist can be specified in a functional manner (a contrived example, admittedly):

```lisp
(defun blacklist (address)
  (equalp address '(#(127 0 0 1) . 6881))) ; Do not initiate connections with 127.0.0.1:6881

(let ((torrent (read-torrent "/path/to/torrent/file")))
  (start torrent :blacklist #'blacklist))
```

**Fifth**, cross the fingers. If all goes well, eventually there will be a message saying `:event :finish`. All the started processes will still be dangling and can be shutdown manually in a crude but effective fashion:

```lisp
(dolist (sfx '("control" "tracker" "channel" "storage" "log"))
  (dolist (th (all-threads))
    (if (equal (thread-name th) (concatenate 'string "ansible-" sfx))
        (destroy-thread th))))
```

If the program is stopped before download completes, the whole download will need to be started anew. Obviously, reloading existing files is on my task list close to the top, but it's not at the very top.
