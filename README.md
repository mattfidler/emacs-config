# emacs-config

Matt's Emacs configuration, plus the few things outside Emacs it depends on.

`emacs-config.el` is the configuration itself; `setup.sh` (Ubuntu) and `wsl.sh`
(WSL) build Emacs and install everything around it.  `msys.md`, `wsl.md` and
`kobo.md` are notes for those platforms.

## Mail

Gmail is read in mu4e, out of a maildir rather than over the network: `mbsync`
syncs `~/.mail/gmail`, `mu` indexes it, and mu4e (`<apps> p`, or `M-x mu4e`)
reads the index.  Search is instant and works with no connection; sending goes
straight to gmail's smtp server.

| file | goes to | what it does |
| --- | --- | --- |
| `mbsyncrc` | `~/.mbsyncrc` | which gmail folders to sync, and where to put them |
| `authinfo-pass` | `~/.local/bin/authinfo-pass` | prints a password from `~/.authinfo.gpg`, so mbsync and Emacs share one copy of it |

The account needs an **app password** -- gmail refuses a plain account password
over IMAP.  Make one at <https://myaccount.google.com/apppasswords> (the account
must have 2-Step Verification on), and put it in `~/.authinfo.gpg`:

```
machine imap.gmail.com login matthew.fidler@gmail.com port 993 password "abcd efgh ijkl mnop"
machine smtp.gmail.com login matthew.fidler@gmail.com port 587 password "abcd efgh ijkl mnop"
```

Quote it if it has spaces in it.  One entry is enough -- `authinfo-pass` falls
back from the imap machine to the smtp one -- but Emacs wants the smtp line to
send.  Check what the file holds without putting a password on screen:

```sh
authinfo-pass --list
```

`~/.authinfo.gpg` has to be encrypted to a key that has **not** expired.  An
expired key still decrypts, so reading keeps working and only saving breaks --
gpg will not encrypt to it, so Emacs cannot write the file back.  Check with
`gpg --list-keys`, and name the key to use on the first line of the file
itself, by key id rather than address, since one address can own several keys:

```
# -*- epa-file-encrypt-to: ("8CB11DF7273ADB54") -*-
```

To move an existing file to another key:

```sh
umask 077
gpg --quiet --decrypt ~/.authinfo.gpg | gpg --encrypt --recipient KEYID --output ~/.authinfo.gpg.new
AUTHINFO=~/.authinfo.gpg.new authinfo-pass --list    # verify before replacing
cp ~/.authinfo.gpg ~/.authinfo.gpg.bak && mv ~/.authinfo.gpg.new ~/.authinfo.gpg
```

Then install and take the first sync:

```sh
sudo apt install isync maildir-utils mu4e   # setup.sh does this too
mkdir -p ~/.mail/gmail
mbsync -a
mu init --maildir=~/.mail/gmail --my-address=matthew.fidler@gmail.com
mu index
```

`mbsync -a` runs again every five minutes from mu4e, or on demand with `U`.
`j` then `i`, `s`, `d`, `t` or `*` jumps to a folder.

Every one of those syncs decrypts `~/.authinfo.gpg`, so gpg-agent should hold
the passphrase for longer than the gap between them -- otherwise a sync stops to
ask, from inside Emacs, every ten minutes.  In `~/.gnupg/gpg-agent.conf`:

```
default-cache-ttl 28800
max-cache-ttl 28800
```

then `gpgconf --reload gpg-agent`.

Two notes on where things live.  Ubuntu hands mu4e's elisp to dh-elpa, which
byte-compiles it into `site-lisp/elpa` for each *packaged* Emacs -- and an Emacs
built into `/usr/local` is not one of those, so the only copy is the source
under `site-lisp/elpa-src/mu4e-*`.  `emacs-config.el` looks in all three places.
And mu4e must match the `mu` it talks to, so both come from the same place:
either both from apt, or both from a build of your own.

Since mu4e 1.7 the message view is Gnus', and the old `mu4e-view-show-images`
and friends do nothing.  Remote images stay blocked, which is what stops a
newsletter knowing you opened it; `gnus-blocked-images` is the knob if you want
them.

### Folders, and where archiving went

`~/.mbsyncrc` syncs Inbox, Sent, Drafts, Trash and Starred, and deliberately
skips `[Gmail]/All Mail`, which holds a copy of every message the account has
ever seen.  Local folders get plain names -- `/Inbox`, `/Sent` -- rather than
gmail's bracketed ones.

That leaves mu4e's refile mark (`r`) with nowhere local to put a message, so it
is pointed back at `/Inbox` and does nothing.  Archive with the delete mark
(`D`) instead: leaving the inbox *is* archiving on gmail, so the message drops
out of `/Inbox` and stays in All Mail, reachable from the web and from search
there.  `d` is different -- it moves to `/Trash`, which gmail empties after
thirty days.

To have All Mail locally after all, add a channel for it to `~/.mbsyncrc`,
point `mu4e-refile-folder` at it, and expect a first sync measured in hours.

## Coding agents

Two agents run here, and they run the same way: inside Emacs on the `eat`
terminal, and inside a tmux session of their own, so a conversation survives a
dropped ssh connection or an Emacs restart.

| agent | CLI | Emacs side | tmux server |
| --- | --- | --- | --- |
| Claude Code | `claude` | `claude-code.el` | `tmux -L claude` |
| Antigravity | `agy` | `emacs-config.el` | `tmux -L antigravity` |

Three files make that work, and each one serves both agents: they work out which
agent they are from the name they were called by, so they are installed once and
linked under one name per agent.

| file | goes to | what it does |
| --- | --- | --- |
| `ai-tmux` | `~/.local/bin/{claude,antigravity}-tmux` | starts (or re-attaches to) one tmux session per directory on that agent's private tmux server |
| `ai-tmux.conf` | `~/.config/ai-tmux.conf` | those servers' configuration: no prefix, no status line, no keys of their own |
| `ai-wt` | `~/.local/bin/{claude,antigravity}-wt` | starts an agent on a fresh git worktree of the current repository |

`setup.sh` installs all three.  On a machine that already has them:

```sh
cd ~/src/emacs-config
install -m 755 ai-tmux ai-wt ~/.local/bin/
ln -sfn ai-tmux ~/.local/bin/claude-tmux
ln -sfn ai-tmux ~/.local/bin/antigravity-tmux
ln -sfn ai-wt   ~/.local/bin/claude-wt
ln -sfn ai-wt   ~/.local/bin/antigravity-wt
install -m 644 ai-tmux.conf ~/.config/ai-tmux.conf
```

Each agent reads its own environment: `CLAUDE_TMUX_SESSION`, `CLAUDE_TMUX_THEME`,
`CLAUDE_TMUX_PROGRAM`, `CLAUDE_TMUX_CONF`, and the same four under
`ANTIGRAVITY_TMUX_`.

Running sessions keep the configuration they started with.  To pick up a change
without losing a conversation, detach (kill the Emacs buffer), then

```sh
tmux -L claude kill-server   # ends every background session, or
tmux -L claude kill-session -t NAME
```

and start the agent again -- or, from Emacs, `M-x claude-tmux-kill` and
`M-x antigravity-tmux-kill`.

### Commands

Claude keeps claude-code.el's own map on `C-c c`.  Antigravity's commands are on
`C-c a`, and are the same four:

| | Claude | Antigravity |
| --- | --- | --- |
| start, or return to this project's agent | `M-x claude` | `C-c a a` |
| switch between this Emacs's agent buffers | `C-c c b` | `C-c a b` |
| attach to a background tmux session | `M-x claude-tmux-switch` | `C-c a s` |
| end a background tmux session | `M-x claude-tmux-kill` | `C-c a k` |
| start on a fresh git worktree | `M-x claude-wt` | `C-c a w` |

Switching lists every session on that agent's tmux server -- including ones
started from another Emacs, another machine's ssh connection, or a plain
terminal -- annotated with the directory it was started in and whether something
is already viewing it.

Everything that is not particular to one agent is shared in `emacs-config.el`:
`ai-term--directory` (which directory a buffer belongs to), `ai-term--theme`,
`ai-tmux--sessions` / `--read-session` / `--kill`, and `ai-wt--worktree`.  Each
agent's commands are a few lines on top.  Antigravity needs no package of its
own: `antigravity--start` is an `eat-make` of `antigravity-tmux` in the project
root, set up like the claude buffers.

### Copying out of an agent

Claude copies with `tmux load-buffer -w -`, and tmux only honours the `-w` --
hand this on to the terminal I am running in -- for terminals it believes
understand OSC 52.  It decides that from `terminal-features`, whose built-in
clipboard entry matches `xterm*` only; eat calls itself `eat-truecolor`, so
copies stopped at the tmux paste buffer ("copied to tmux buffer") and could not
be pasted anywhere else.  `ai-tmux.conf` therefore claims the feature for every
terminal, and forwards OSC 52 sequences programs send themselves:

```tmux
set -as terminal-features ",*:clipboard"
set -g set-clipboard on
```

The other half is in `emacs-config.el`.  tmux sends OSC 52 without naming a
selection, and eat reads a missing name as xterm's `s0` target, which only
reaches the kill ring; `eat-osc52-select-means-clipboard` reads an unnamed
selection as the clipboard instead.  It also picks the frame to copy from:
`kill-new` hands the text to the window system of whichever frame is selected
when it runs, and in a daemon that is the initial text frame, which owns no
clipboard at all -- so a copy landed in the kill ring and nowhere else.
`eat-osc52--clipboard-frame` prefers the selected frame, then the one showing the
terminal, then any graphical frame.  Each copy says how many characters it took,
so a copy that never reaches Emacs is told apart from one that arrives and goes
nowhere.

This is all inside Emacs, so it only works in an Emacs that has loaded the
current `emacs-config.el`.  A long-running `emacs --daemon` started before these
lines were written will keep saying "copied to tmux buffer" and put nothing on
the clipboard until it is restarted.

### Colours

`claude-code-theme-environment` exports `CLAUDE_TMUX_THEME=dark` or `light` from
the background mode of the frame claude is started in, and `claude-tmux` turns
that into `--settings '{"theme":"..."}'`.  A solarized-dark Emacs gets a dark
claude, a solarized-light one a light claude.  Only new sessions take it; inside
an existing session, say `/theme`.  `agy` has no such switch -- it reads the
terminal's own background colour, which eat answers for it -- so
`ANTIGRAVITY_TMUX_THEME` is exported the same way but goes unused for now.

### Terminal size

`C-<wheel-up>` and `C-<wheel-down>` zoom a terminal buffer, and the terminal is
resized to match, so the agent reflows to the new number of rows and columns.
`C-c +`, `C-c -` and `C-c 0` do the same from the keyboard.  Agent buffers also
run `my-eat-fit-columns-mode` (`C-c f`), which shrinks the text on its own while
the window is too narrow to show `my-eat-min-columns` columns -- agents wrap code
snippets and diffs to the width of their terminal, so a narrow window otherwise
mangles them.  Zooming by hand sets the size the mode grows back to.
