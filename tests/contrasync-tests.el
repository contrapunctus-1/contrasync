;;; -*- lexical-binding: t; -*-
(require 'buttercup)
(require 'contrasync)

(describe
 "contrasync-parse-paths"
 (before-all
  (setq contrasync-disk-path    "/path/to/my-disk/"
        contrasync-source-paths `("~/foo/"
                        ("~/bar/" . "ssh:user@host:")
                        ("~/quux/" ,contrasync-disk-path "quux/"))
        user          (->> (shell-command-to-string "whoami")
                           (replace-regexp-in-string "\n" ""))))
 (it "works with strings, lists, and pairs"
     (expect (contrasync-parse-paths)
             :to-equal
             (list
              (cons (concat "/home/" user "/foo/")
                    (concat contrasync-disk-path contrasync-machine-name "/home/" user "/foo/"))
              (cons (concat "/home/" user "/bar/")
                    (concat "ssh:user@host:" contrasync-disk-path contrasync-machine-name "/home/" user "/bar/"))
              (cons (concat "/home/" user "/quux/")
                    (concat contrasync-disk-path "quux/"))))))

;; Local Variables:
;; nameless-current-name: "contrasync"
;; End:
