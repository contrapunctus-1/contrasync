# contrasync.el
WARNING - NOT READY FOR USE AT THE MOMENT

Script to partially automate my rsync backup workflow.

1. Primary focus is local syncing
2. The user specifies a list of source-target directory pairs (see `contrasync-source-paths`)
3. When the user runs `M-x contrasync`, for each pair
   1. Run rsync with certain options, including `--dry-run`, so no changes occur, and `--delete-after`, so the deletions are shown together at the end
   2. Let user view the rsync output - they can now
      * search for a file in the source or the target directory (by name, or by hash)
      * refresh the buffer, running rsync with the same arguments (especially `--dry-run`) again
      * accept the changes - run rsync with same arguments but without `--dry-run`
      * reject the changes and move to the next pair of directories
      * quit, which saves the iteration state. The user may resume with `M-x contrasync`, even across Emacs sessions.

The problems this solves for me -
1. I run rsync with `--dry-run` first, then later without it, because
   1. I run it with `--delete` (actually `--delete-after`), but want to ensure I'm not deleting something I want; and
   2. I also want to check for moved/renamed files, and move them myself if I can.
2. Removing and adding `--dry-run` manually is repetitive and error-prone
3. Output of `--dry-run` becomes difficult to manage if I'm doing it for my entire disk
4. For these reasons, updating my backups with rsync takes ages if I try to do it all at once; so I try to run it incrementally, every day, instead.
5. Keeping track of the state of #4 in my head is difficult and error-prone.

## Usage
NB - at the moment, the user is advised to add a "/" at the end in all paths; in the near future, contrasync.el will take care of it.

### Tutorial
1. Set `contrasync-disk-path` to the root of your destination drive.
2. contrasync also defines a "machine name" (by default, your hostname) in `contrasync-machine-name`. Modify it if you need, or set it to `nil` if you only have one machine.
3. Define source paths in `contrasync-source-paths`. The simplest way is to specify the source path as a string -
   ```emacs-lisp
   (setq contrasync-disk-path    "/media/user/my-disk/"
         contrasync-machine-name "my-laptop"
         contrasync-source-paths '("~/.emacs.d/"))
   ```
   For "~/.emacs.d/", contrasync will run `rsync ... "/home/user/.emacs.d/" "/media/user/my-disk/my-laptop/home/user/.emacs.d/"`.
4. Run `M-x contrasync` to run `rsync` (with `--dry-run`, so no files are actually changed) for each path you have specified.
5. See if the output is to your satisfaction. Run `M-x contrasync-accept` to run the same rsync command without `--dry-run`.

### How to sync to a remote destination
`contrasync-source-paths` can also contain pairs, whose `car` is the source path as a string, and the `cdr` is a remote protcol prefix as a string.
```emacs-lisp
(setq contrasync-source-paths
      '("~/.emacs.d/"
        ("~/Documents/" . "ssh:user@host:")))
```
Now,
* since "~/.emacs.d/" is specified as a string, contrasync will run the same command as above - `rsync ... "/home/user/.emacs.d/" "/media/user/my-disk/my-laptop/home/user/.emacs.d/"`;
* but since "~/Documents/" is specified as a pair, it will run `rsync ... "/home/user/Documents/" "ssh:user@host:/media/user/my-disk/"`

### How to sync to a custom destination path
If the default destination path pattern does not suit you, you can take the wheel and generate your own.

Instead of a string or a pair, specify the source path as a list of strings, where the first string is the source path, and all the remaining strings are concatenated to derive the destination path.

```emacs-lisp
(setq contrasync-disk-path    "/media/user/my-disk/"
      contrasync-machine-name "my-laptop"
      contrasync-source-paths
      `("~/.emacs.d/"
        ("~/Documents/" . "ssh:user@host:")
        ("~/phone/"     ,contrasync-disk-path "phone/")))
```
Now,
* since "~/.emacs.d/" is specified as a string, contrasync will run the same command as above - `rsync ... "/home/user/.emacs.d/" "/media/user/my-disk/my-laptop/home/user/.emacs.d/"`;
* since "~/Documents/" is specified as a pair, it will run `rsync ... "/home/user/Documents/" "ssh:user@host:/media/user/my-disk/"`
* but for "~/phone/", contrasync will run `rsync ... "/home/user/phone/" "/media/user/my-disk/phone/"`.

### How to sync from a remote source
No matter how the source path is specified, it can be prefixed with a remote protocol string, e.g. -

```emacs-lisp
(setq contrasync-source-paths
      '("ssh:user@host:~/.emacs.d/"
        ("ssh:user@host:~/Documents/" . ...)
        ("ssh:user@host:~/phone/" ...)))
```

## Alternatives
If this does not match your workflow...maybe [dired-rsync](https://github.com/stsquad/dired-rsync) does?

While contrasync is written primarily with the author's workflow in mind, it is hoped that it is sufficiently extensible. Does customizing `contrasync-command-line-function` meet your needs?

## License
contrasync.el is released under your choice of [Unlicense](https://unlicense.org/) and the [WTFPL](http://www.wtfpl.net/).

(See files [LICENSE](LICENSE) and [LICENSE.1](LICENSE.1)).

## Thanks
pjb, wasamasa, and bhartrihari for discussions and feedback

wasamasa for suggesting that I name it something other than "rsync.el"
