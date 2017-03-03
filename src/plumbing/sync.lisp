;;;; object.lisp
(defpackage #:git-api.plumbing.sync
  (:use #:cl #:alexandria #:git-api.utils)
  (:export))
  
(in-package #:git-api.plumbing.sync)

#|

git-daemon ((self git-repo))

    A really simple server for Git repositories.
git-fetch-pack ((self git-repo))

    Receive missing objects from another repository.
git-http-backend ((self git-repo))

    Server side implementation of Git over HTTP.
git-send-pack ((self git-repo))

    Push objects over Git protocol to another repository.
git-update-server-info ((self git-repo))

    Update auxiliary info file to help dumb servers.

The following are helper commands used by the above; end users typically do not use them directly.

git-http-fetch ((self git-repo))

    Download from a remote Git repository via HTTP.
git-http-push ((self git-repo))

    Push objects over HTTP/DAV to another repository.
git-parse-remote ((self git-repo))

    Routines to help parsing remote repository access parameters.
git-receive-pack ((self git-repo))

    Receive what is pushed into the repository.
git-shell ((self git-repo))

    Restricted login shell for Git-only SSH access.
git-upload-archive ((self git-repo))

    Send archive back to git-archive.
git-upload-pack ((self git-repo))

    Send objects packed back to git-fetch-pack.
|#
