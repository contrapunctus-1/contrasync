# rsync.el
Script to partially automate my rsync backup workflow.

1. Primary focus is local syncing - may add remote support later
2. The user specifies a list of source-target directory pairs (`rsync-directory-alist`)
3. When the user runs `M-x rsync`, for each pair
   1. Run rsync with certain options, including `--dry-run`, so no changes occur, and `--delete-after`, so the deletions are shown together at the end
   2. Let user view the rsync output - they can now
      * search for a file in the source or the target directory (by name, or by hash)
      * accept the changes - run rsync with same arguments but without -n
      * reject the changes and move to the next pair of directories
      * quit, which saves the iteration state. The user may resume with `M-x rsync`, even across Emacs sessions.

## If this does not match your workflow...
You may wish to see dired-rsync.

While `rsync.el` is written primarily with the author's workflow in mind, it is hoped that `rsync.el` is sufficiently extensible. Does customizing `rsync-command-line-function` will meet your needs?

## License
rsync.el is released under your choice of [Unlicense](https://unlicense.org/) and the [WTFPL](http://www.wtfpl.net/).

(See files [LICENSE](LICENSE) and [LICENSE.1](LICENSE.1)).

## Thanks
pjb, wasamasa, and bhartrihari
