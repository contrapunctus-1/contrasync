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
  "Alist of directories to be synced, in the form (\"SOURCE\" . \"DESTINATION\").
SOURCE will be appended to DESTINATION, so the destination path
will be DESTINATION/SOURCE/.")

(defvar rsync-command "rsync"
  "Name of command to run. Can also be a path to the binary.")

;; -P creates a progress bar. It is a nuisance at this stage, but maybe we can make a better progress bar using it?
(defvar rsync-arguments '("-A" "-H" "-P" "-X" "-a" "-h" "-s" "x"
                     "--checksum" "--delete-after")
  "List of options to be used in all calls to rsync.
It is not designed to contain \"-n\"/\"--dry-run\".")

(defun rsync-sentinel (proc event)
  (message "%s %s" proc event))

(defun rsync ()
  "Run rsync (with \"--dry-run\") for each pair of paths in `rsync-directory-alists'.
Display the rsync output in a buffer. The user may inspect the
output, and possibly accept it, which will run the same rsync
command again, but without the \"--dry-run.\""
  (cl-loop for (source . destination) in rsync-directory-alists do
    (let ((source      (expand-file-name source))
          (destination (expand-file-name destination)))
      (make-process :name "rsync"
                    :buffer (generate-new-buffer-name "rsync-output")
                    :command `(,rsync-command
                               ,@rsync-arguments
                               "--dry-run"
                               ,source
                               ,(concat destination source))
                    :connection-type 'pipe
                    :stderr "rsync-errors"))))

(provide 'rsync)

;;; rsync.el ends here

;; Local Variables:
;; nameless-current-name: "rsync"
;; End:
