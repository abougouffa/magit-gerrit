magit-gerrit
============

Magit plugin for [Gerrit Code Review](https://www.gerritcodereview.com/):
review, vote, comment, push and manage changes from Emacs.

- A **Reviews** section in `magit-status`, listing the changes of the project
  with their votes, with query presets (my changes, incoming reviews,
  starred...) and saved queries.
- A **change buffer** showing everything about a change: status, labels and
  submit requirements, reviewers, dependencies, files, patchsets, messages and
  inline comments.
- **Voting** on any label (including custom ones), submitting, abandoning,
  restoring, rebasing and moving changes, managing reviewers, CCs, topics and
  the work-in-progress state.
- **Inline comments** on the lines of a patchset diff; with the REST API,
  comment threads, replies (`Done`, `Ack`), resolution and draft comments.
- With the REST API: **hashtags, stars, private changes**, editing the
  **commit message**, **reverting** and **cherry-picking** on the server.
- A **push menu** with Gerrit's push options: target branch, topic, hashtags,
  reviewers, CC, WIP/ready, private, notifications, message.
- **Patchsets**: view their diff, check them out, cherry-pick them.
- The Gerrit change of a commit in **revision and log buffers**.
- Installation of Gerrit's **commit-msg hook**.
- Browsing changes in the web interface and files/commits in **Gitiles**.

magit-gerrit talks to Gerrit through a **backend**: Gerrit's SSH commands
(`ssh -p 29418 host gerrit ...`, works with the SSH key you already use to
push) or Gerrit's **REST API** over HTTPS, which offers more features.  The
menus show the commands the backend doesn't support as unavailable.

| Feature                                                 | SSH | REST |
|---------------------------------------------------------|:---:|:----:|
| Reviews section, change buffer, Change-Id lookups       |  ✓  |  ✓   |
| Votes, submit, abandon, restore, rebase, move           |  ✓  |  ✓   |
| Reviewers, CCs, topic, work in progress                 |  ✓  |  ✓   |
| New inline comments (from a diff line, file, or prompt) |  ✓  |  ✓   |
| Comment threads, replies, Done/Ack, resolving           |     |  ✓   |
| Draft comments: save, edit, delete, publish             |     |  ✓   |
| Values of the labels and permitted votes from the server|     |  ✓   |
| Reviewer completion with suggestions of the server      |     |  ✓   |
| Hashtags, stars, private changes                        |     |  ✓   |
| Edit the commit message, revert, cherry-pick on server  |     |  ✓   |
| Submit requirements                                     | records | ✓ |
| HTTPS remotes (e.g. `*.googlesource.com`)               |     |  ✓   |

Pushing, fetching and checking out patchsets use Git, whatever the backend.


Requirements
------------

- Emacs 29.1 or later, Magit 4.0 or later, Transient 0.5 or later.
- For the SSH backend: SSH access to Gerrit, with a key that doesn't need a
  password prompt (use `ssh-agent`): commands run without a terminal.  A
  recent Gerrit (3.2 or later) for setting topics with `gerrit set-topic`;
  everything else works with older versions.
- For the REST backend: HTTP(S) access to Gerrit and, to modify changes, the
  *HTTP password* generated in your Gerrit settings (*Settings → HTTP
  Credentials*).  Reading works anonymously when the server allows it.


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

Otherwise, put `magit-gerrit.el`, `magit-gerrit-ssh.el` and
`magit-gerrit-rest.el` in your `load-path`.


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

### Choosing a backend

`magit-gerrit-backend` selects the backend: `auto` (the default), `ssh` or
`rest`.  In `auto` mode, the REST backend is used when it is configured for
the host of the remote, the SSH backend otherwise (see
`magit-gerrit-auto-backends`):

- `magit-gerrit-rest-url`: the URL of the Gerrit server, when its host is
  the one of the remote, e.g. for `ssh://john@review.example.com:29418/p`:

  ```elisp
  ((magit-mode . ((magit-gerrit-rest-url . "https://review.example.com"))))
  ```

- `magit-gerrit-rest-hosts`: the hosts whose remotes (SSH or HTTPS) are
  served by Gerrit, as `"HOST"` (server at `https://HOST`) or
  `("HOST" . "URL")`.  This also enables magit-gerrit for HTTPS remotes:

  ```elisp
  (setopt magit-gerrit-rest-hosts '("review.example.com"
                                    ("git.example.org" . "https://example.org/r")))
  ```

- The remotes on `HOST.googlesource.com` use `https://HOST-review.googlesource.com`
  (see `magit-gerrit-rest-googlesource`).

To use the REST API for every repository, `(setopt magit-gerrit-backend 'rest)`:
the URL is then deduced from the remote (`https://HOST`).

The REST backend authenticates with HTTP basic authentication, using the
*HTTP password* of your Gerrit settings (not your account password).  Store
it in an auth-source file such as `~/.authinfo.gpg`:

```
machine review.example.com login john password GENERATED-HTTP-PASSWORD
```

Set `magit-gerrit-rest-user` to choose among several logins for a host, or
`magit-gerrit-rest-auth` to `anonymous` or to a function returning
`(USER . PASSWORD)`.  Without credentials, requests are anonymous: changes can
be read (if the server allows it) but not modified.


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
5. In the diff of a patchset (`RET` on a file of the change buffer), `_ C`
   comments on the line at point; in the change buffer, `_ a` replies to the
   comment at point and `_ f` replies "Done" and resolves it.
6. `_ v`: vote, `_ S`: submit.

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
| `C`   | Comment (see below)           | `a`   | Reply to the comment at point   |
| `f`   | Reply "Done" and resolve      | `k`   | Reply "Ack" and resolve         |
| `p`   | Publish the draft comments    | `e`   | Edit the draft at point         |
| `x`   | Delete the draft at point     | `#`   | Set hashtags                    |
| `I`   | Toggle private                | `*`   | Star or unstar                  |
| `E`   | Edit the commit message       | `V`   | Revert (creates a change)       |
| `L`   | Cherry-pick to a branch on the server |  |                            |

Arguments: `-m` message (for votes, submit, abandon, restore, move, WIP/ready,
comments, publishing drafts; the commit message of reverts and cherry-picks),
`-n` who gets notified, `-s` submit after voting, `-d` save comments as
drafts, `-u`/`-r` mark a comment unresolved/resolved.

With a prefix argument, `d`, `D`, `F` and `r` ask for the patchset number.
In a change buffer, they act on the patchset at point.

### Voting

`v` reads one or more votes like `Code-Review=+2,Verified=+1`, with completion
on the labels of the change.  With the REST backend, the values come from the
server, and only the votes you are permitted to give are offered.  Gerrit's
SSH interface doesn't tell which values a label accepts, so with the SSH
backend they come from `magit-gerrit-label-values`:

```elisp
(setopt magit-gerrit-label-values
        '(("Code-Review" -2 . 2) ("Verified" -1 . 1) ("Zuul-Gate" -1 . 1)))
```

The labels shown as columns of the Reviews section are set with
`magit-gerrit-review-labels`, as `(LABEL ABBREVIATION)` pairs.

### Comments

`_ C` adds an inline comment:

- in the diff of a patchset (`RET` on a file or comment of the change buffer)
  or a revision buffer, on the line at point; on a removed line, the comment
  is on the old version (the base of the patchset);
- in the change buffer, on the file or comment at point (the line is read);
- elsewhere, on a file and line read for the change at point.

The comment is written in a buffer: `C-c C-c` sends it, `C-c C-k` cancels
(set `magit-gerrit-read-message-function` to use the minibuffer instead).
New comments are unresolved, unless `-r` is given; with `-d`, they are saved
as drafts.

With the REST backend, the change buffer groups the inline comments in
threads, showing the unresolved ones; `_ a` replies to the comment at point
(the thread stays unresolved if it was, unless `-r` or `-u` is given), `_ f`
and `_ k` reply "Done" and "Ack" and resolve the thread.  Your drafts are
listed in the change buffer: `RET` (or `_ e`) edits the draft at point, `k`
(or `_ x`) deletes it, `_ p` publishes them all, with the `-m` message.

With the SSH backend, `_ C` posts the comments with `gerrit review --json`;
Gerrit's SSH interface doesn't give the identifiers of comments, so they can't
be replied to.

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
| `magit-gerrit-backend`                   | `auto`   | Backend: `auto`, `ssh` or `rest`                |
| `magit-gerrit-rest-url`                  | deduced  | Gerrit server for the REST API                  |
| `magit-gerrit-rest-hosts`                | `nil`    | Hosts served by Gerrit (REST)                   |
| `magit-gerrit-rest-auth`                 | auth-source | Credentials of the REST API                  |
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
| `magit-gerrit-read-message-function`     | buffer   | How comments and messages are written           |

Every SSH command and REST request is logged, with its errors, in the
`*magit-gerrit-log*` buffer.  After changing the configuration of a
repository, `M-x magit-gerrit-reset-backend` makes its backend again.


Upgrading from 0.2
------------------

- The SSH code moved to `magit-gerrit-ssh.el`, loaded by `magit-gerrit.el`
  with `magit-gerrit-rest.el`: install the three files.  The user options
  are unchanged.
- For code calling internal functions: `magit-gerrit--ssh-call` is now
  `magit-gerrit-ssh-call`, and `magit-gerrit--query-changes` takes keywords
  of `magit-gerrit-query-options` (such as `:current-patch-set`) instead of
  the flags of `gerrit query`.


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
commands like Gerrit's SSH daemon, and a fake HTTP server implementing the
REST endpoints (authentication, anti-XSSI prefix, refused options), both
answering with JSON fixtures.  They need Git, Python 3 and a POSIX shell.

### Architecture

`magit-gerrit.el` is the frontend: buffers, menus and commands.  It talks to
Gerrit through a backend object, whose type derives from
`magit-gerrit-backend` and implements the generic functions
`magit-gerrit-backend-*` (query, vote, post comments...).  The backends
exchange changes in the format of the JSON of `gerrit query`, with optional
attributes for what only some backends provide (comment ids, drafts, label
values...); the frontend shows what is available.  A backend declares its
features with `magit-gerrit-backend-features`: the menus and commands check
them, and the operations it doesn't implement signal a clear error.

A new backend is a `cl-defstruct` including `magit-gerrit-backend`, methods
for the generic functions it supports, and a constructor registered with
`magit-gerrit-register-backend`; see `magit-gerrit-ssh.el` and
`magit-gerrit-rest.el`.


Credits
-------

This package started as a fork of Brian Fransioli's `magit-gerrit`.

License: GPL-3.0-or-later.
