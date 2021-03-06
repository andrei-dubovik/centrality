<!--- Copyright (c) 2020 Andrey Dubovik <andrei@dubovik.eu> --->

# Centrality

## Disclaimer

This library is very much work in progress. I will be keeping this readme updated as development continues.

The library runs on SBCL. I am still undecided whether to fully stick with SBCL or to provide a compatibility layer for other LISP platforms.

Currently, it is possible to download a popular torrent or two. Less popular torrents might not work as there is no upload functionality yet and the client will likely get choked. The code has evolved along with my understanding of the protocol, and while there is much scope for improvement, hopefully it is not too messy. There is no interface at the moment.

## Example Usage

**First**, load the package somehow, e.g.,

```lisp
(push *default-pathname-defaults* asdf:*central-registry*)
(require :centrality)
(in-package :centrality)
```

There will be some warnings as not all files are in a correct order...

**Second**, optionally, start a log service:

```lisp
(start-log "~/centrality/log.txt")
```

**Third**, we will need to set the downloads directory.

```lisp
(defparameter *file-dir* "~/centrality/downloads/") ; don't forget the trailing slash, no proper handling of pathnames yet...
```
There are a few other configuration parameters available, see `src/params.lisp`.

**Fourth**, initiate a download:

```lisp
(defparameter *torrents* (make-thread-pool))

(let ((torrent (read-torrent "/path/to/torrent/file")))
  (start *torrents* torrent))
```

The above code will create a new thread for managing the specified torrent, and it will add a reference to that thread's mailbox to `*torrents*`. The torrent will start downloading immediately. To start downloading multiple torrents simultaneously, just call `(start)` for each of them.

A SOCKS5 proxy can be used, in which case the tracker request as well as peer connections are tunnelled via SOCKS. (Has been tested with SSH tunnelling.)

```lisp
(let ((torrent (read-torrent "/path/to/torrent/file")))
  (start *torrents* torrent :proxy '(#(127 0 0 1) . 1080)))
```

A blocklist can be specified in a functional manner (a contrived example, admittedly):

```lisp
(defun blocklist (address)
  (equalp address '(#(127 0 0 1) . 6881))) ; Do not initiate connections with 127.0.0.1:6881

(let ((torrent (read-torrent "/path/to/torrent/file")))
  (start *torrents* torrent :blocklist #'blocklist))
```

Additional trackers can be added, e.g.:

```lisp
(let ((torrent (read-torrent "/path/to/torrent/file")))
  (start *torrents* torrent
         :trackers '("http://tracker.archlinux.org:6969/announce"
                     "http://bttracker.debian.org:6969/announce")))
```

At the moment, UDP trackers are not supported.

**Fifth**, cross the fingers. If all goes well, the torrents will begin downloading. Basic progress report can be printed with

```lisp
(print-status *torrents*)
```

When the download finishes, all the started processes would still be dangling and can be shutdown manually in a crude but effective fashion:

```lisp
(dolist (thread (all-threads))
  (if (alexandria:starts-with-subseq "centrality-" (thread-name thread))
      (destroy-thread thread)))
```
