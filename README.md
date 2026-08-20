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

## Claude Code

Claude Code runs inside Emacs (`claude-code.el` on the `eat` terminal), and
inside a tmux session of its own, so a conversation survives a dropped ssh
connection or an Emacs restart.  Three files make that work:

| file | goes to | what it does |
| --- | --- | --- |
| `claude-tmux` | `~/.local/bin/claude-tmux` | starts (or re-attaches to) one tmux session per directory on the private `claude` tmux server |
| `claude-tmux.conf` | `~/.config/claude-tmux.conf` | that server's configuration: no prefix, no status line, no keys of its own |
| `claude-wt` | `~/.local/bin/claude-wt` | starts claude on a fresh git worktree of the current repository |

`setup.sh` installs all three.  On a machine that already has them, copy the
files over by hand:

```sh
cd ~/src/emacs-config
cp claude-tmux claude-wt ~/.local/bin/
chmod +x ~/.local/bin/claude-tmux ~/.local/bin/claude-wt
cp claude-tmux.conf ~/.config/claude-tmux.conf
```

Running claude sessions keep the configuration they started with.  To pick up a
change without losing a conversation, detach (kill the Emacs buffer), then

```sh
tmux -L claude kill-server   # ends every background session, or
tmux -L claude kill-session -t NAME
```

and start claude again -- or, from Emacs, `M-x claude-tmux-kill`.

### Copying out of claude

Claude copies with `tmux load-buffer -w -`, and tmux only honours the `-w` --
hand this on to the terminal I am running in -- for terminals it believes
understand OSC 52.  It decides that from `terminal-features`, whose built-in
clipboard entry matches `xterm*` only; eat calls itself `eat-truecolor`, so
copies stopped at the tmux paste buffer ("copied to tmux buffer") and could not
be pasted anywhere else.  `claude-tmux.conf` therefore claims the feature for
every terminal, and forwards OSC 52 sequences programs send themselves:

```tmux
set -as terminal-features ",*:clipboard"
set -g set-clipboard on
```

The other half is in `emacs-config.el`: tmux sends OSC 52 without naming a
selection, and eat reads a missing name as xterm's `s0` target, which only
reaches the kill ring.  `eat-osc52-select-means-clipboard` reads an unnamed
selection as the clipboard, so a copy inside claude can be pasted outside Emacs
as well.

### Colours

`claude-code-theme-environment` exports `CLAUDE_TMUX_THEME=dark` or `light`
from the background mode of the frame claude is started in, and `claude-tmux`
turns that into `--settings '{"theme":"..."}'`.  A solarized-dark Emacs gets a
dark claude, a solarized-light one a light claude.  Only new sessions take it;
inside an existing session, say `/theme`.

### Terminal size

`C-<wheel-up>` and `C-<wheel-down>` zoom a terminal buffer, and the terminal is
resized to match, so claude reflows to the new number of rows and columns.
`C-c +`, `C-c -` and `C-c 0` do the same from the keyboard.  Claude buffers also
run `my-eat-fit-columns-mode` (`C-c f`), which shrinks the text on its own while
the window is too narrow to show `my-eat-min-columns` columns -- claude wraps
code snippets and diffs to the width of its terminal, so a narrow window
otherwise mangles them.  Zooming by hand sets the size the mode grows back to.
