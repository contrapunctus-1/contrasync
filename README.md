# rsync.el
Script to partially automate my rsync backup workflow.

1. Primary focus is local syncing - may add remote support later
2. The user specifies a list of source-target directory pairs (`rsync-directory-alist`)
3. When the user runs `M-x rsync`, for each pair
   1. Run rsync with certain options, including `--dry-run`, so no changes occur, and `--delete-after`, so the deletions are shown together at the end
   2. Let user view the rsync output - they can now
      * search for a file in the source or the target directory (by name, or by hash)
      * refresh the buffer, running rsync with the same arguments (especially `--dry-run`) again
      * accept the changes - run rsync with same arguments but without -n
      * reject the changes and move to the next pair of directories
      * quit, which saves the iteration state. The user may resume with `M-x rsync`, even across Emacs sessions.

## Defining directories
NB - at the moment, the user is advised to add a "/" at the end in all paths; in the near future, rsync.el will take care of it.

Source/target directory pairs are defined with `rsync-directory-alist` -
```emacs-lisp
(setq rsync-directory-alist
      '(("~/.emacs.d/" . "/media/user/my-disk/")))
```
For this, `rsync.el` will run `rsync ... "/home/user/.emacs.d/" "/media/user/my-disk/home/user/.emacs.d/"`

### Reducing duplication
```emacs-lisp
(setq rsync-directory-alist
      (let ((disk "/media/user/my-disk/"))
        `(("~/.emacs.d/"  . ,disk)
          ("~/Documents/" . ,disk))))
```

### Specifying different target paths
```emacs-lisp
(setq rsync-directory-alist
      (let* ((disk           "/media/user/my-disk/")
             (machine-subdir (concat disk rsync-machine-name "/")))
        `(("~/.emacs.d/"  . ,machine-subdir)
          ("~/Documents/" . ,machine-subdir)
          ("~/phone/"     . ,disk))))
```
`rsync-machine-name` defaults to the hostname of the current machine. With this configuration -
* "~/.emacs.d" will be synced to "/media/user/my-disk/MACHINE-NAME/home/user/.emacs.d/"
* "~/Documents/" will be synced to "/media/user/my-disk/MACHINE-NAME/home/user/Documents/"
* "~/phone/" will be synced to "/media/user/my-disk/phone/" (NB - No...no it won't. It'll be "/media/user/my-disk/home/user/phone/" 😫)

### Machine-specific configurations
For those with multiple machines, with the same init.el on all of them -
```emacs-lisp
(setq rsync-directory-alist
      (let* ((disk           "/media/user/my-disk/")
             (machine-subdir (concat disk rsync-machine-name "/")))
        (pcase rsync-machine-name
          ("mnt-reform"
           `(("~/.emacs.d/" . ,machine-subdir)))
          ("pinebook"
           `(("~/Documents/" . ,machine-subdir))))))
```
Here, running `rsync`...
* ...on "mnt-reform" will only backup "~/.emacs.d/" to "/media/user/my-disk/mnt-reform/home/user/.emacs.d/"
* ...on "pinebook" will only backup "~/Documents/" to "/media/user/my-disk/pinebook/home/user/Documents/"

## If this does not match your workflow...
You may wish to see dired-rsync.

While `rsync.el` is written primarily with the author's workflow in mind, it is hoped that `rsync.el` is sufficiently extensible. Does customizing `rsync-command-line-function` meet your needs?

## License
rsync.el is released under your choice of [Unlicense](https://unlicense.org/) and the [WTFPL](http://www.wtfpl.net/).

(See files [LICENSE](LICENSE) and [LICENSE.1](LICENSE.1)).

## Thanks
pjb, wasamasa, and bhartrihari
