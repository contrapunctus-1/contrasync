;;; rsync.el --- make rsync backups easier -*- lexical-binding: t; -*-

;; Author: contrapunctus <xmpp:contrapunctus@jabber.fr>
;; Maintainer: contrapunctus <xmpp:contrapunctus@jabber.fr>
;; Keywords: files
;; Version: 0.0.1

;; This is free and unencumbered software released into the public domain.
;;
;; Anyone is free to copy, modify, publish, use, compile, sell, or
;; distribute this software, either in source code form or as a compiled
;; binary, for any purpose, commercial or non-commercial, and by any
;; means.
;;
;; For more information, please refer to <https://unlicense.org>

;;; Commentary:
;;

;;; Code:

(defvar rsync-directory-alists nil
  "Alist of directories to be synced, in the form (\"souce\" . \"destination\").")

(defvar rsync-command "rsync"
  "Name of command to run. Can also be a path to the binary.")

(defvar rsync-common-options '("-A" "-H" "-P" "-X" "-a" "-h" "-s" "x" "--checksum" "--delete-after")
  "List of options to be used in all calls to rsync.
It is not designed to contain \"-n\"/\"--dry-run\".")

;; (make-process )

(provide 'rsync)

;;; rsync.el ends here

;; Local Variables:
;; nameless-current-name: "rsync"
;; End:
