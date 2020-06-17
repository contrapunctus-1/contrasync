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

(defvar rsync-max-procs 1
  "Number of rsync processes to run in parallel.")

(defvar rsync-directory-alists nil
  "Alist of directories to be synced, in the form (\"SOURCE\" . \"DESTINATION\").
With the default value of `rsync-command-function' (see
`rsync-command'), SOURCE will be appended to DESTINATION, so the
destination path will be DESTINATION/SOURCE/.")

(defvar rsync-command "rsync"
  "Name of command to run. Can also be a path to the binary.")

;; -P creates a progress bar. It is a nuisance at this stage, but maybe we can make a better progress bar using it?
(defvar rsync-arguments '("-A" "-H" "-P" "-X" "-a" "-h" "-s" "x"
                     "--checksum" "--delete-after")
  "List of options to be used in all calls to rsync.
It must not contain \"-n\"/\"--dry-run\" -
`rsync-command-function' will add that.")

(defvar rsync-buffer-name-function 'rsync-buffer-name
  "Function used to create rsync buffer names.
It is called with 2 arguments - SOURCE and DESTINATION, which are
paths from a `rsync-directory-alists' pair.")

(defun rsync-buffer-name (source destination)
  "Return a new, unique buffer name starting with \"rsync-output\".
SOURCE and DESTINATION are paths from a `rsync-directory-alists' pair."
  (generate-new-buffer-name
   (concat "rsync-output-"
           (file-name-nondirectory
            (directory-file-name source)))))

(defvar rsync-command-function 'rsync-command
  "Function used to generate the rsync command to be run.
Return value should be a list of strings, usually with
`rsync-command' as the first element, followed by
`rsync-arguments'.

It is called with 3 arguments - the SOURCE path, the DESTINATION
path, and DRY-RUN-P.

If DRY-RUN-P is non-nil, the function should include
\"--dry-run\"/\"-n\" in the arguments.")

(defun rsync-command (source destination dry-run-p)
  "Return the rsync command line to be run.
SOURCE and DESTINATION are paths from a `rsync-directory-alists' pair.
If DRY-RUN-P is non-nil, the \"--dry-run\" argument is added."
  (let ((source      (expand-file-name source))
        (destination (expand-file-name destination)))
    `(,rsync-command
      ,@rsync-arguments
      ,(if dry-run-p "--dry-run" "")
      ,source
      ,(concat destination source))))

;; TODO - have the sentinel remove the process from `rsync--active-procs'; maybe run `rsync' again

(defun rsync-sentinel (proc event)
  (message "%s %s" proc event))

(defvar rsync--alist-index 0)
(defvar rsync--active-procs nil
  "List of active rsync processes.")

(defun rsync ()
  "Run rsync (with \"--dry-run\") for each pair of paths in `rsync-directory-alists'.
Display the rsync output in a buffer (see
`rsync-buffer-name-function'). The user may then inspect the
output, and possibly accept it, which will run the same rsync
command again, but without the \"--dry-run.\""
  (interactive)
  (if rsync-directory-alists
      (cl-loop with directory-alists = (seq-drop rsync-directory-alists rsync--alist-index)
        for (source . destination) in directory-alists
        ;; TODO - don't start new processes for paths which already have running processes
        if (< (length rsync--active-procs) rsync-max-procs) do
        (->>
         (make-process
          :name    "rsync"
          :buffer  (funcall rsync-buffer-name-function source destination)
          :command (funcall rsync-command-function     source destination t)
          :connection-type 'pipe
          :stderr  "rsync-errors")
         (list)
         (append rsync--active-procs)
         (setq rsync--active-procs))
        (cl-incf rsync--alist-index)
        ;; reset `rsync--alist-index' at the end of the list
        if (= rsync--alist-index (length directory-alists))
        do (setq rsync--alist-index 0))
    (error "Please add some paths to `rsync-directory-alists' for `rsync' to synchronize")))

(provide 'rsync)

;;; rsync.el ends here

;; Local Variables:
;; nameless-current-name: "rsync"
;; End:
