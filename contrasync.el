;;; contrasync.el --- run rsync on user-defined paths -*- lexical-binding: t; -*-

;; Author: contrapunctus <xmpp:contrapunctus@jabber.fr>
;; Maintainer: contrapunctus <xmpp:contrapunctus@jabber.fr>
;; Keywords: files
;; Homepage: https://github.com/contrapunctus-1/contrasync.el
;; Package-Requires: ((dash "2.16.0") (emacs "25.1") (seq "2.20"))
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

(require 'dash)

(defgroup contrasync nil
  "Run rsync on user-defined paths."
  :group 'external)

(defcustom contrasync-max-procs 1
  "Number of rsync processes to run in parallel."
  :type 'integer)

(defcustom contrasync-disk-path ""
  "Path to the disk you're synchronizing to.

By default, this is used to construct the destination path."
  :type 'directory)

(defcustom contrasync-machine-name (->> (shell-command-to-string "hostname")
                              (replace-regexp-in-string "\n" ""))
  "Name of machine we're running on.
The default value is the output of hostname(1), but it can be any
string.

By default, this is used to construct the destination path. Thus, using path separators in this may lead to unexpected behaviour."
  :type 'string)

(defcustom contrasync-source-paths nil
  "Alist of directories to be synced, in the form (\"SOURCE\" . \"DESTINATION\").
By default, SOURCE is appended to DESTINATION, so the final
output path used is \"DESTINATION/SOURCE/\". See
`contrasync-command-line'."
  ;; TODO - add :type
  )

(defcustom contrasync-command "rsync"
  "Name of command to run. Can also be a path to the binary."
  :type '(file :must-match t))

;; -P creates a progress bar. It is a nuisance at this stage, but maybe we can make a better progress bar using it?
(defcustom contrasync-arguments '("-A" "-H" "-P" "-X" "-a" "-h" "-s" "-x"
                        "--checksum" "--delete-after")
  "List of options to be used in all calls to rsync.
It must not contain \"-n\"/\"--dry-run\" -
`contrasync-command-line-function' will add that."
  :type '(repeat string))

(defcustom contrasync-buffer-name-function 'contrasync-buffer-name
  "Function used to create contrasync buffer names.
It is called with 2 arguments - SOURCE and DESTINATION, which are
paths from a `contrasync-directory-alist' pair.

Please ensure that it uses `generate-new-buffer-name' to make it
unique."
  :type 'function)

(defun contrasync-buffer-name (source _destination)
  "Return a new, unique buffer name starting with \"contrasync-output\".
SOURCE and DESTINATION are paths from a `contrasync-directory-alist' pair."
  (generate-new-buffer-name
   (concat "contrasync-output-"
           (file-name-nondirectory
            (directory-file-name source)))))

(defcustom contrasync-command-line-function 'contrasync-command-line
  "Function used to generate the rsync command to be run.
Return value should be a list of strings, usually with
`contrasync-command' as the first element, followed by
`contrasync-arguments'.

It is called with 3 arguments - the SOURCE path, the DESTINATION
path, and DRY-RUN-P.

If DRY-RUN-P is non-nil, the function should include
\"--dry-run\"/\"-n\" in the arguments."
  :type 'function)

(defun contrasync-parse-paths ()
  "Return an alist of source-target path pairs, using `contrasync-source-paths'."
  (let ((disk    (if contrasync-disk-path    contrasync-disk-path    ""))
        (machine (if contrasync-machine-name contrasync-machine-name "")))
    (cl-loop for source-spec in contrasync-source-paths collect
      (cond
       ((stringp source-spec)
        (let* ((source (expand-file-name source-spec))
               (target (concat disk machine source)))
          (cons source target)))
       ((format-proper-list-p source-spec)
        (let ((source (expand-file-name (car source-spec)))
              (target (apply #'concat (cdr source-spec))))
          (cons source target)))
       ((consp source-spec)
        (let* ((source (expand-file-name (car source-spec)))
               (target (concat (cdr source-spec)
                               disk machine source)))
          (cons source target)))
       (t
        (error (concat "Source specifier must be a string,"
                       " a pair, or a proper list - %s")
               source-spec))))))

(defun contrasync-command-line (source destination dry-run-p)
  "Return the rsync command line to be run.
SOURCE and DESTINATION are paths from a `contrasync-directory-alist' pair.

If DRY-RUN-P is non-nil, the \"--dry-run\" argument is added."
  (let ((source      (expand-file-name source))
        (destination (expand-file-name destination)))
    `(,contrasync-command
      ,@contrasync-arguments
      ,(if dry-run-p "--dry-run" "")
      ,source
      ,destination)))

(defmacro contrasync-with-process-buffer (process &rest forms)
  "Run FORMS with the process buffer of PROC being current."
  (declare (indent defun))
  `(let ((process-buffer (process-buffer ,process)))
     (when (buffer-live-p process-buffer)
       (save-excursion
         (let ((inhibit-read-only t))
           (with-current-buffer process-buffer
             ,@forms))))))

(defun contrasync-sentinel (process event)
  (when (not (string-match-p "^open" event))
    (--> (remove process contrasync--active-procs)
         (setq contrasync--active-procs it)))
  (contrasync-with-process-buffer process
    (goto-char (point-max))
    (insert "STATUS " event)))

(defun contrasync-filter (process output)
  (let ((progress-regex "[0-9]+ files\\.\\.\\."))
    (contrasync-with-process-buffer process
      (when (string-match-p progress-regex output)
        (goto-char (point-min))
        (set-marker (process-mark process) (point))
        (re-search-forward progress-regex nil t)
        (delete-region (point-at-bol) (point-at-eol)))
      (insert
       (replace-regexp-in-string "" "" output)))))

(defun contrasync-display-command-line (command-line process)
  "Display the rsync command line in the buffer.
COMMAND-LINE is a list of strings returned by
`contrasync-command-line-function', and PROCESS is the rsync
process."
  (contrasync-with-process-buffer process
    (goto-char (point-min))
    (--> (-interpose " " command-line)
         (apply #'concat it)
         (insert it "\n"))
    (set-marker (process-mark process) (point))))

(defvar contrasync--alist-index 0)
(defvar contrasync--active-procs nil
  "List of active rsync processes.")
(defvar contrasync--current-source-destination nil)

(defun contrasync-make-process (source destination command-line dry-run-p)
  "Run rsync and return a process object.
SOURCE and DESTINATION are paths,
COMMAND-LINE is a list of strings returned by `contrasync-command-line-function',
DRY-RUN-P, if non-nil, means that rsync is to be called with \"--dry-run\"."
  (make-process
   :name     "contrasync"
   :buffer   (if dry-run-p
                 (funcall contrasync-buffer-name-function source destination)
               (current-buffer))
   :filter   #'contrasync-filter
   :command  command-line
   :connection-type 'pipe
   :stderr   "contrasync-errors"
   :sentinel #'contrasync-sentinel))

(defun contrasync ()
  "Run rsync (with \"--dry-run\") for each path in `contrasync-source-paths'.
Display the rsync output in a buffer (see
`contrasync-buffer-name-function'). The user may then inspect the
output, and possibly accept it, which will run the same rsync
command again, but without the \"--dry-run.\""
  (interactive)
  (if contrasync-source-paths
      (cl-loop with directory-alist = (contrasync-parse-paths)
        ;; ;; let's leave out pause/resume until we have a better MVP
        ;; (seq-drop (contrasync-parse-paths) contrasync--alist-index)
        for (source . destination) in directory-alist
        if (< (length contrasync--active-procs) contrasync-max-procs) do
        (let* ((command-line (funcall contrasync-command-line-function source destination t))
               (process      (contrasync-make-process source destination command-line t)))
          (--> (list process)
               (append it contrasync--active-procs)
               (setq contrasync--active-procs it))
          (--> (process-buffer process)
               (switch-to-buffer-other-window it))
          ;; insert the command in the beginning
          (contrasync-display-command-line command-line process)
          (contrasync-mode)
          (make-local-variable 'contrasync--current-source-destination)
          (setq contrasync--current-source-destination
                (cons source destination))
          (cl-incf contrasync--alist-index))
        else do
        (cl-return)
        ;; reset `contrasync--alist-index' at the end of the list
        if (= contrasync--alist-index (length directory-alist))
        do (setq contrasync--alist-index 0))
    (error
     (concat "Please add some paths to `contrasync-directory-alist'"
             " for `contrasync' to synchronize"))))

(defun contrasync-accept ()
  "Run the rsync command again, but without --dry-run."
  (interactive)
  (-let [(source . destination) contrasync--current-source-destination]
    (if (derived-mode-p 'contrasync-mode)
        (contrasync-make-process source destination
                       (funcall contrasync-command-line-function source destination)
                       nil)
      (error "`contrasync-accept' needs to be run from a `contrasync-mode' buffer"))))

(defvar contrasync-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'contrasync-accept)
    map)
  "Keymap used by `contrasync-mode'.")

(define-derived-mode contrasync-mode special-mode "Contrasync"
  "Major mode for buffers created by `contrasync'.")

(provide 'contrasync)

;;; contrasync.el ends here

;; Local Variables:
;; nameless-current-name: "contrasync"
;; End:
