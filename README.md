magit-gerrit
============

Magit plugin for [Gerrit Code Review](https://www.gerritcodereview.com/):
review, vote, push and manage changes from Emacs.

- A **Reviews** section in `magit-status`, listing the changes of the project
  with their votes, with query presets (my changes, incoming reviews,
  starred...) and saved queries.
- A **change buffer** showing everything about a change: status, labels and
  submit requirements, reviewers, dependencies, files, patchsets, messages and
  inline comments.
- **Voting** on any label (including custom ones), submitting, abandoning,
  restoring, rebasing and moving changes, managing reviewers, CCs, topics and
  the work-in-progress state.
- A **push menu** with Gerrit's push options: target branch, topic, hashtags,
  reviewers, CC, WIP/ready, private, notifications, message.
- **Patchsets**: view their diff, check them out, cherry-pick them.
- The Gerrit change of a commit in **revision and log buffers**.
- Installation of Gerrit's **commit-msg hook**.
- Browsing changes in the web interface and files/commits in **Gitiles**.

magit-gerrit uses Gerrit's SSH interface (`ssh -p 29418 host gerrit ...`), so
it works with the SSH key you already use to push.


Requirements
------------

- Emacs 29.1 or later, Magit 4.0 or later, Transient 0.5 or later.
- SSH access to Gerrit, with a key that doesn't need a password prompt (use
  `ssh-agent`): commands run without a terminal.
- A recent Gerrit (3.2 or later) for setting topics with `gerrit set-topic`;
  everything else works with older versions.


Installation
------------

With Emacs 30 or later:

```elisp
(use-package magit-gerrit
  :vc (:url "https://github.com/abougouffa/magit-gerrit")
  :after magit
  :config (magit-gerrit-auto-enable))
```

With Emacs 29, install it with `M-x package-vc-install RET
https://github.com/abougouffa/magit-gerrit RET`, then:

```elisp
(with-eval-after-load 'magit
  (require 'magit-gerrit)
  (magit-gerrit-auto-enable))
```

Otherwise, put `magit-gerrit.el` in your `load-path`.


Setup
-----

`magit-gerrit-auto-enable` enables `magit-gerrit-mode` in the status, log and
revision buffers of repositories hosted on Gerrit.  You can also enable it in
a status buffer with `M-x magit-gerrit-mode`.

A repository is recognized as hosted on Gerrit when its `origin` (or `gerrit`)
remote is an SSH URL on Gerrit's port, for example
`ssh://john@review.example.com:29418/project`.  The SSH credentials
(`john@review.example.com`) are deduced from that URL.  Otherwise, configure
the repository with a `.dir-locals.el` file at its root:

```elisp
((magit-mode . ((magit-gerrit-remote . "gerrit")            ; Gerrit remote
                (magit-gerrit-ssh-creds . "john@review.example.com")
                (magit-gerrit-port . 29418)
                (magit-gerrit-force-enable . t)              ; e.g. HTTPS remotes
                (magit-gerrit-web-url . "https://review.example.com/r"))))
```

These variables are declared safe, Emacs won't ask before applying them.


Usage
-----

Press `_` (`magit-gerrit-popup-prefix`) in a Magit buffer to open the Gerrit
menu; it is also listed in Magit's dispatch menu (`?` or `h`).  The commands
act on the change at point: in the Reviews section, in a change buffer or on
the "Gerrit:" line of a revision buffer.

A typical workflow:

1. Commit (once per repository, `_ H` installs the commit-msg hook which adds
   the `Change-Id` trailer to commits).
2. `_ P p`: push the commit at point (or HEAD) for review.
3. `_ A`: add reviewers, or pass them with `-r` in the push menu.
4. Watch the votes in the Reviews section, `RET` on a change to read the
   comments, push new patchsets.
5. `_ v`: vote, `_ S`: submit.

### The Gerrit menu (`_`)

| Key   | Command                       | Key   | Command                         |
|-------|-------------------------------|-------|---------------------------------|
| `RET` | Show the change               | `A`   | Add reviewers                   |
| `b`   | Browse the change             | `K`   | Remove reviewers                |
| `v`   | Vote                          | `c`   | Add CC                          |
| `S`   | Submit                        | `T`   | Set (or remove) the topic       |
| `u`   | Rebase                        | `w`   | Mark work in progress           |
| `M`   | Move to another branch        | `W`   | Mark ready for review           |
| `B`   | Abandon                       | `d`   | View the diff of the patchset   |
| `R`   | Restore                       | `D`   | Check out the patchset          |
| `P`   | Push menu                     | `F`   | Cherry-pick the patchset        |
| `/`   | Query menu                    | `o`   | Show the change of the commit   |
| `H`   | Install the commit-msg hook   | `O`   | Browse the change of the commit |
| `y`   | Copy the URL                  | `g`   | Browse in Gitiles               |
| `Y`   | Copy the URL and subject      | `r`   | Copy the patchset ref           |

Arguments: `-m` message (for votes, submit, abandon, restore, move, WIP/ready),
`-n` who gets notified, `-s` submit after voting.

With a prefix argument, `d`, `D`, `F` and `r` ask for the patchset number.
In a change buffer, they act on the patchset at point.

### Voting

`v` reads one or more votes like `Code-Review=+2,Verified=+1`, with completion
on the labels of the change.  Gerrit's SSH interface doesn't tell which values
a label accepts, so they come from `magit-gerrit-label-values`:

```elisp
(setopt magit-gerrit-label-values
        '(("Code-Review" -2 . 2) ("Verified" -1 . 1) ("Zuul-Gate" -1 . 1)))
```

The labels shown as columns of the Reviews section are set with
`magit-gerrit-review-labels`, as `(LABEL ABBREVIATION)` pairs.

### The push menu (`_ P`)

| Key  | Argument                 | Key  | Argument                  |
|------|--------------------------|------|---------------------------|
| `-b` | Target branch            | `-w` | Work in progress          |
| `-t` | Topic                    | `-R` | Ready for review          |
| `-h` | Hashtags (`a,b`)         | `-p` | Private                   |
| `-r` | Reviewers                | `-P` | Remove private            |
| `-c` | CC                       | `-e` | Publish draft comments    |
| `-m` | Message                  | `-S` | Signed push               |
| `-n` | Who gets notified        |      |                           |

`p` pushes for review, `w` pushes as work in progress.  The target branch
defaults to the upstream branch of the current branch, when it is on the
Gerrit remote.  Before pushing a commit without `Change-Id`, magit-gerrit
offers to install the commit-msg hook (see `magit-gerrit-check-change-id`).

### The query menu (`_ /`)

| Key | Query                                 | Key | Query                       |
|-----|---------------------------------------|-----|-----------------------------|
| `o` | Open changes (default)                | `t` | Changes of a topic          |
| `m` | My open changes                       | `e` | Edit the query              |
| `i` | Incoming reviews                      | `l` | Saved query                 |
| `s` | Starred                               | `S` | Save a query                |
| `w` | Watched                               | `a` | Toggle all projects         |
| `M` | Merged during the last week           |     |                             |

The query applies to the current repository, or to all of them with a prefix
argument.  Queries use [Gerrit's search
syntax](https://gerrit-review.googlesource.com/Documentation/user-search.html);
save your own in `magit-gerrit-saved-queries`.

### The change buffer

`RET` on a change opens it.  There, `RET` shows the patchset, the diff of the
file or inline comment, or the dependency at point, and `g` fetches the change
again.  The sections are inserted by `magit-gerrit-change-sections-hook`.

### Revision and log buffers

Commit buffers show a "Gerrit:" line with the change of the commit (and of its
cherry-picks), unless `magit-gerrit-show-revision-change` is nil.  Log
buffers annotate commits with their change, status and votes when
`magit-gerrit-annotate-log` is non-nil.  Changes are looked up by `Change-Id`,
in batches, and cached for `magit-gerrit-change-cache-ttl` seconds.

### Gitiles

`_ g` opens in Gitiles the file of the current buffer (at the current line),
or the commit or file at point in Magit buffers.  Files are shown at the
upstream branch; with a prefix argument, at the HEAD commit.  The Gitiles URL
is `magit-gerrit-gitiles-base-url`, by default `https://HOST/plugins/gitiles`.


Options
-------

All the options are in the `magit-gerrit` customization group (`M-x
customize-group RET magit-gerrit`).  The most useful:

| Option                                   | Default  | Description                                     |
|------------------------------------------|----------|-------------------------------------------------|
| `magit-gerrit-popup-prefix`              | `"_"`    | Key of the Gerrit menu                          |
| `magit-gerrit-review-labels`             | CR, Ve   | Labels shown in the Reviews section             |
| `magit-gerrit-label-values`              | CR, Ve   | Values accepted by each label                   |
| `magit-gerrit-extra-options`             | `nil`    | Query terms added to every query of the section |
| `magit-gerrit-saved-queries`             | `nil`    | Saved queries                                   |
| `magit-gerrit-async-reviews`             | `nil`    | Fetch the Reviews section in the background     |
| `magit-gerrit-annotate-log`              | `nil`    | Annotate commits in log buffers                 |
| `magit-gerrit-show-revision-change`      | `t`      | Show the change in revision buffers             |
| `magit-gerrit-collapse-patchset-section` | `nil`    | Collapse the Reviews section initially          |
| `magit-gerrit-web-url`                   | deduced  | Web interface (for the commit-msg hook)         |
| `magit-gerrit-gitiles-base-url`          | deduced  | Gitiles URL                                     |
| `magit-gerrit-signed-push-p`             | `nil`    | Sign pushes by default                          |
| `magit-gerrit-use-push-url`              | `nil`    | Use the push URL of the remote                  |
| `magit-gerrit-ssh-program`               | `"ssh"`  | SSH program                                     |

Every SSH command is logged, with its errors, in the `*magit-gerrit-log*`
buffer.


Upgrading from 0.1
------------------

- `RET` on a change opens the change buffer; browsing it is `_ b`.
- `C` (Code review) and `V` (Verify) are replaced by `v` (Vote).
- `_ P` opens the push menu.  Pushing drafts is replaced by pushing as work in
  progress (Gerrit removed drafts in 2.15).
- Keys are only bound in buffers where `magit-gerrit-mode` is enabled, Magit's
  global keymap isn't modified anymore.
- In `magit-gerrit-gitiles-browse`, the line is always included and the prefix
  argument selects the permalink.
- Magit 4.0 is required.
- Renamed commands keep working as obsolete aliases.


Development
-----------

```sh
make deps   # install the dependencies in .elpa (needs network)
make all    # byte-compile, checkdoc, package-lint and tests
```

The tests don't need a Gerrit server: they use a fake `ssh` program that parses
commands like Gerrit's SSH daemon and answers with JSON fixtures.  They need
Git, Python 3 and a POSIX shell.


Authors
-------

Brian Fransioli (assem@terranpro.org), Abdelhak Bougouffa and contributors.

License: GPL-3.0-or-later.
