# rsync.el
Script to partially automate my rsync backup workflow.

1. The user specifies a list of source-target directory pairs.
2. For each pair
   1. Run rsync with certain options, including -n, so no changes occur, and --delete-after, so the deletions are shown together at the end
   2. Let user view the rsync output - they can now
      * search for a file in the source or the target directory (by name, or by hash)
      * accept the changes - run rsync with same arguments but without -n
      * reject the changes and move to the next pair of directories
      * quit

rsync.el can pause/resume its operations across Emacs sessions.

Multi-device use - ask user for "device name" (used to determine directory on hard disk) and "disk name" (used to determine disk path)

## License
rsync.el is released under your choice of [Unlicense](https://unlicense.org/) and the [WTFPL](http://www.wtfpl.net/).

(See files [LICENSE](LICENSE) and [LICENSE.1](LICENSE.1)).
