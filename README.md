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
| `gpg-agent.conf` | `~/.gnupg/gpg-agent.conf` | caches the key passphrase for a working day, so background syncs do not keep asking |

mu4e runs `mbsync -a` every five minutes, mbsync asks `authinfo-pass` for the
app password, and `authinfo-pass` has to decrypt `~/.authinfo.gpg` to answer it.
On gpg's defaults -- ten minutes of cache, two hours at the outside -- that puts
a pinentry prompt on screen about once an hour, for mail arriving in the
background.  `gpg-agent.conf` sets both to eight hours, so it asks once a
working day; the trade is that the passphrase sits in gpg-agent's memory that
long.  Nothing prompts until the first decryption of a session, so the first
`mbsync` after a reboot is where it asks.

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

Five agents run here, and they run the same way: inside Emacs on the `eat`
terminal, and inside a tmux session of their own, so a conversation survives a
dropped ssh connection or an Emacs restart.

| agent | CLI | Emacs side | tmux server |
| --- | --- | --- | --- |
| Claude Code | `claude` | `claude-code.el` | `tmux -L claude` |
| Antigravity | `agy` | `emacs-config.el` | `tmux -L antigravity` |
| GitHub Copilot | `copilot` | `emacs-config.el` | `tmux -L copilot` |
| opencode | `opencode` | `emacs-config.el` | `tmux -L opencode` |
| kilo | `kilo` (also `kilocode`) | `emacs-config.el` | `tmux -L kilo` |

Four files make that work, and each one serves every agent: they work out which
agent they are from the name they were called by, so they are installed once and
linked under one name per agent.

| file | goes to | what it does |
| --- | --- | --- |
| `ai-tmux` | `~/.local/bin/{claude,antigravity,copilot,opencode,kilo}-tmux` | starts (or re-attaches to) one tmux session per directory on that agent's private tmux server |
| `ai-tmux.conf` | `~/.config/ai-tmux.conf` | those servers' configuration: no prefix, no status line, no keys of their own |
| `ai-wt` | `~/.local/bin/{claude,antigravity,copilot,opencode,kilo}-wt` | starts an agent on a fresh git worktree of the current repository |
| `ai-pr` | `~/.local/bin/{claude,antigravity,copilot,opencode,kilo}-pr` | checks a pull request out into a worktree of its own and starts an agent in it |

`setup.sh` installs all four.  On a machine that already has them:

```sh
cd ~/src/emacs-config
install -m 755 ai-tmux ai-wt ai-pr ~/.local/bin/
for agent in claude antigravity agy copilot opencode kilo kilocode; do
  ln -sfn ai-tmux ~/.local/bin/$agent-tmux
  ln -sfn ai-wt   ~/.local/bin/$agent-wt
  ln -sfn ai-pr   ~/.local/bin/$agent-pr
done
install -m 644 ai-tmux.conf ~/.config/ai-tmux.conf
install -m 755 emacsreset ~/.local/bin/
install -m 644 bash-emacs.sh ~/.config/bash-emacs.sh
echo '[ -f "$HOME/.config/bash-emacs.sh" ] && . "$HOME/.config/bash-emacs.sh"' >> ~/.bashrc
```

opencode's installer puts its binary in `~/.opencode/bin` and adds that to `PATH`
in `~/.bashrc` -- a file only an *interactive* shell reads.  systemd starts
`emacs --daemon` with neither that directory nor `~/.local/bin`, so opencode runs
perfectly from a terminal and is invisible to `executable-find` and to every
subprocess Emacs starts.  `emacs-config.el` puts both directories back for this
Emacs and its children, just above `;;; Coding agents in a terminal.`

Each agent reads its own environment: `CLAUDE_TMUX_SESSION`, `CLAUDE_TMUX_THEME`,
`CLAUDE_TMUX_PROGRAM`, `CLAUDE_TMUX_CONF`, and the same four under
`ANTIGRAVITY_TMUX_`, `COPILOT_TMUX_`, `OPENCODE_TMUX_` and `KILO_TMUX_`.  Only
claude takes the theme as a switch (`--settings`); the others read the terminal's
own background colour, which eat answers for them.

Running sessions keep the configuration they started with.  To pick up a change
without losing a conversation, detach (kill the Emacs buffer), then

```sh
tmux -L claude kill-server   # ends every background session, or
tmux -L claude kill-session -t NAME
```

and start the agent again -- or, from Emacs, `M-x claude-tmux-kill`,
`M-x antigravity-tmux-kill`, `M-x copilot-cli-tmux-kill`,
`M-x opencode-tmux-kill` and `M-x kilo-tmux-kill`.

### One key: `<apps> k h`

`claude-dwim` is the whole of it from the keyboard, and what it does depends on
where it is called from:

| where you are | what it does |
| --- | --- |
| in an agent terminal | `ai-tmux-list` -- every *other* conversation |
| in magit, on the trunk | cuts a worktree and starts Claude in it |
| anywhere a conversation for this directory exists | goes back to it |
| anywhere else | cuts a worktree, so the agent never churns the checkout you are reading |

The trunk is whatever `origin/HEAD` points at, or `main` or `master` where the
clone never set it.  Being *in magit on the trunk* is read as the start of a new
task, so it cuts a worktree even when the trunk already has an agent of its own;
elsewhere on the trunk it will still cut one, but only because there is nothing
here to go back to.

"A conversation for this directory" is a buffer in this Emacs when there is one,
and failing that a tmux session started here by an Emacs that has since gone --
`claude-tmux` re-attaches rather than starting a second claude, so the way back
after a restart is the same key.

`agy-dwim`, `copilot-cli-dwim`, `opencode-dwim` and `kilo-dwim` are the same
thing for the other four agents.  `claude-dwim` is on `<apps> k h` in
`transient-apps`, `agy-dwim` on `<apps> k H`, `copilot-cli-dwim` on `<apps> k C`,
`opencode-dwim` on `<apps> k O` and `kilo-dwim` on `<apps> k K`.

### The session list

`C-c a i` (`ai-tmux-list`) is to conversations what `ibuffer` is to buffers: a
`tabulated-list-mode` buffer of every session on every agent's tmux server, most
recently used first -- including ones started from another Emacs, another
machine's ssh connection, or a plain terminal -- with the directory it was
started in and whether something is already viewing it.

| key | |
| --- | --- |
| `RET`, `o` | show this session in this Emacs, starting a client if there is none |
| `d`, `u`, `U`, `x` | mark for ending, unmark, unmark all, end the marked ones |
| `k` | end this one |
| `g` | re-read the list from tmux |
| `q` | bury it |

`C-c a j` (`ai-tmux-switch`) is the same set of sessions from the minibuffer for
when the name is already known; candidates there are named `agent/session`, since
two agents working in the same directory derive the same session name.

### Commands

Claude keeps claude-code.el's own map on `C-c c`; the other four have no package
and so no map, and borrow `C-c a` -- Antigravity the plain letters, and each
agent after it a prefix of its own (Copilot `C-c a o`, opencode `C-c a e`, kilo
`C-c a l`) rather than another case of every letter, with Antigravity's letters
repeated under it:

| | Claude | Antigravity | Copilot | opencode | kilo |
| --- | --- | --- | --- | --- | --- |
| the right thing for where you are | `C-c a d`, `<apps> k h` | `C-c a D`, `<apps> k H` | `C-c a O`, `<apps> k C` | `C-c a E`, `<apps> k O` | `C-c a L`, `<apps> k K` |
| the list of every session | `C-c a i` | `C-c a i` | `C-c a i` | `C-c a i` | `C-c a i` |
| start, or return to this project's agent | `C-c a c` | `C-c a a` | `C-c a o o` | `C-c a e e` | `C-c a l l` |
| switch between this Emacs's agent buffers | `C-c c b` | `C-c a b` | `C-c a o b` | `C-c a e b` | `C-c a l b` |
| attach to a background tmux session | `M-x claude-tmux-switch` | `C-c a s` | `C-c a o s` | `C-c a e s` | `C-c a l s` |
| end a background tmux session | `M-x claude-tmux-kill` | `C-c a k` | `C-c a o k` | `C-c a e k` | `C-c a l k` |
| start on a fresh git worktree | `M-x claude-wt` | `C-c a w` | `C-c a o w` | `C-c a e w` | `C-c a l w` |
| work on a pull request | `C-c a p` | `C-c a P` | `C-c a o p` | `C-c a e p` | `C-c a l p` |
| re-open a conversation the agent remembers | `C-c a r` | `C-c a R` | `C-c a o r` | `C-c a e r` | `C-c a l r` |
| ...from another directory | `C-u C-u C-c c R` (asks which) | `C-u C-c a R` (lists all) | -- | `C-u C-c a e r` (this project) | `C-u C-c a l r` (lists all) |

The prefixes are what was left rather than what one would choose: `o` went to
Copilot first, so opencode took `e`, and `k` being Antigravity's kill and `i` the
session list, kilo took `l`.  Each is the first letter of the agent's own name
still going spare.

`ai-tmux-agents` is the list all of this walks: each entry pairs an agent's tmux
server with the function that shows one of its sessions, so a sixth agent is one
line there and a `--attach` function of its own.

Everything that is not particular to one agent is shared in `emacs-config.el`:
`ai-term--directory` (which directory a buffer belongs to), `ai-term--theme`,
`ai-tmux--sessions` / `--read-session` / `--kill`, `ai-wt--worktree`, and --
for the four agents with no package -- `ai-term--buffer-name` /
`--buffers-for-directory` / `--all-buffers` / `--read-buffer` / `--start`, which
name a buffer after the directory an agent works in, find the ones already
running, and `eat-make` another on that agent's `<agent>-tmux`; plus
`ai-term--read-conversation` and the two filters around it, the picker of past
conversations that the agents without one of their own share, and
`ai-term--cli-conversations`, which reads a session list out of an
opencode-shaped CLI for both of the agents that have one.  Each agent's commands
are a few lines on top: `antigravity--start`, `copilot-cli--start`,
`opencode--start` and `kilo--start` are one line apiece, and Copilot -- which has
a resume picker of its own -- is the whole agent in about a hundred lines.

Copilot's commands are spelled `copilot-cli-` rather than `copilot-`, because
`copilot.el` (the inline completion) and `copilot-chat.el` are both loaded here
and own the `copilot-` prefix between them.  The CLI it runs is still plain
`copilot`, and the tmux server, the buffer names and the environment variables
all say `copilot`.  opencode has no such neighbour, so its commands are plain
`opencode-`.

### Conversations the agent remembers

Every command above finds a conversation through tmux: a session is still
running, so there is something to re-attach to.  `C-c a r` (`claude-resume`),
`C-c a R` (`antigravity-resume`, also `agy-resume`), `C-c a o r`
(`copilot-cli-resume`), `C-c a e r` (`opencode-resume`) and `C-c a l r`
(`kilo-resume`) are the way back in when there is not -- after a reboot, or a
`C-c a k` -- since all five agents keep their own history of what was said, quite
apart from the sessions they were said in.

Claude and Copilot have pickers of their own: `claude --resume` and
`copilot --resume` list the conversations they remember and re-open the one you
choose, so those two commands are a switch and nothing else.

The other three have to be given the picker, and for opposite reasons.

Antigravity will not list its conversations at all -- `agy` re-opens one named by
id (`--conversation`), or the most recent one (`--continue`), and that is the
whole of it -- so `antigravity-resume` builds the list itself, out of
`antigravity-history-file`, `~/.gemini/antigravity-cli/history.jsonl`, which is
the CLI's own record of what it has been asked: one JSON object a line, with the
conversation, the directory and the prompt.  Each conversation is offered by what
was first asked of it, annotated with when it was last spoken to, most recent
first (which is also what a bare `RET` takes), and picking one runs
`agy --conversation` on it.  By default only this directory's are listed -- this
one or a subdirectory, since agy asked from a subdirectory records that
subdirectory; with `C-u` every directory's are, annotated with where they were,
and the one you pick starts in the directory it belongs to.

opencode and kilo *will* list them -- `session list --format json` prints the id,
title, directory and times of every root session, newest first -- but their own
picker lives inside the TUI, which is no help when the reason you are looking is
that there is no TUI running.  So `opencode-resume` and `kilo-resume` read that
list, offer each conversation by the title the agent gave it, and start
`--session ID` on the one you pick.  If the CLI is missing, or too old to know
the switches, it prints nothing that parses and both fall back to `--continue` --
as they do for anything printed with a zero status that is not an array of
sessions, an `{"error": ...}` from a CLI that is not logged in being the one you
would actually hit.

kilo is opencode's fork, so the two are the same CLI here: same switches, same
JSON, and one `ai-term--cli-conversations` reading both.  They differ in one
place.  opencode scopes its list to the current project and has no switch to
widen it, so `C-u C-c a e r` means the whole *project*; kilo's `session list`
takes an `--all`, so `C-u C-c a l r` means every project on the machine, the way
`C-u C-c a R` does for Antigravity.

Two things are true of all three of these built lists.  Conversations whose
directory has since been deleted are left out: there is nowhere to start the
agent, so offering them would only fail.  On a machine that cuts a worktree per
pull request that is most of them -- 10 of 17 here -- which is worth knowing
before wondering where a conversation went.  And an id that turns out to be stale
is the CLI's problem, not ours: it says so and starts a fresh conversation.

Back to Antigravity for the one thing peculiar to it.  That its CLI writes down
the *first* prompt of a session before the conversation has an id is worked
around rather than solved: the line without an id is taken to open the
conversation named on the next line, which is right unless two sessions in the
same directory interleave.  A conversation that opened with a slash command has
nothing to be called and is listed by the head of its id instead.  One-shot
`agy -p` runs -- the hundreds a review skill leaves behind -- never reach this
file, which is what makes the list worth reading.  If the file is missing,
`antigravity-resume` falls back to `--continue`.

Either way the conversation comes back on a tmux session of its own, so it does
not disturb the one already running in that directory.  That is `ai-tmux`
noticing it was given arguments: switches like `--resume` only mean anything to
an agent that is starting, and `new-session -A` would have attached to the
running session and dropped them on the floor, so an argument makes it take a
session name nothing has instead -- `<dir>-<hash>-2`, `-3`.  Naming a session
outright with `<AGENT>_TMUX_SESSION` still goes back to that one, switches and
all, so ask for one or the other.  The same now goes for `claude-wt` and
`claude-pr` on the shell when given trailing agent args.

The one cost: a resumed conversation is no longer the session `C-c a c`,
`C-c a a`, `C-c a o o`, `C-c a e e` or `C-c a l l` finds, since those start an agent and let it derive the
directory's own session name.  `C-c a d` and its friends do find it -- the dwim
rejoin looks a session up by the directory it was started in and then attaches to
it *by name*, which is the whole difference -- and so do `C-c a i` and `C-c a j`,
the session list, which knows every session by name.

### Pull requests

`C-c a p` (`claude-pr`), `C-c a P` (`agy-pr`), `C-c a o p` (`copilot-cli-pr`),
`C-c a e p` (`opencode-pr`) and `C-c a l p` (`kilo-pr`) put a pull request in a
worktree of its own and set the agent loose in it.  `gh` does the checkout, so a pull
request from a fork works and the branch is set up to push back to the right
place; the agent can read, build, commit and push it while the checkout you are
reading stays as you left it.

Asking with no argument completes over the repository's open pull requests,
newest first, annotated with the title and who opened it; a number, a `#number`
or the URL of one all work, so a closed pull request can still be typed in.  The
worktree is `~/src/<repo>-pr<N>` -- a flat sibling of the repository even when
asked for from inside another worktree -- and asking again for the same one
re-enters it, which re-attaches to the conversation already living there.

`agy` works as a name for antigravity throughout, since that is what the CLI is
called.  On the shell `agy-pr`, `agy-wt` and `agy-tmux` are the same scripts as
the `antigravity-` ones; in Emacs `agy`, `agy-select-buffer`, `agy-tmux-switch`,
`agy-tmux-kill` and `agy-wt` are aliases of the `antigravity-` commands, so
`M-x agy` reaches the whole set rather than just the two that were written with
the short name.  Either name starts the same thing: an eat buffer on a detachable
tmux session, which outlives the buffer it is shown in.

`kilocode` works as a name for kilo the same way, since the CLI answers to both.
On the shell `kilocode-tmux`, `kilocode-wt` and `kilocode-pr` are the same
scripts as the `kilo-` ones and resolve to the same agent, so both names land on
the same `tmux -L kilo` session rather than two servers; in Emacs `kilocode`,
`kilocode-resume`, `kilocode-select-buffer`, `kilocode-tmux-switch`,
`kilocode-tmux-kill`, `kilocode-wt` and `kilocode-pr` are aliases.  `kilo` is the
name used everywhere else here, because it is the one the CLI spells its own
`~/.config/kilo` and `~/.local/share/kilo` with.

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

## Emacs from the shell

A second Emacs is a second copy of every package and of the native-compilation
cache, and it cannot see the buffers -- agent terminals included -- that the
running one already has.  So `emacs` in bash is a client of the daemon:

| file | goes to | what it does |
| --- | --- | --- |
| `bash-emacs.sh` | `~/.config/bash-emacs.sh` | makes `emacs` an `emacsclient`, sourced from `~/.bashrc` |
| `emacs.service` | `~/.config/systemd/user/emacs.service` | the `systemd --user` unit `emacs --daemon` runs under |
| `emacsreset` | `~/.local/bin/emacsreset` | stops the daemon, so the next `emacs` starts a fresh one |

`emacs` opens a graphical frame where there is a display to put one on and a
terminal frame where there is not, and brings the daemon up first if it is not
running -- through `systemctl --user start emacs` on machines where systemd
manages it, so it is supervised the same way either way.  The calls a client
cannot serve are handed to the real binary untouched: `--batch`, `--script`, `-Q`
and `-q` have no init to share, and `--daemon` is what we would be connecting to.
So `emacs --batch` in a script still starts its own Emacs, as it must.

### A graphical frame over `ssh -X`

A client picks `-c` over `-t` whenever `$DISPLAY` or `$WAYLAND_DISPLAY` is set,
which is true of an `ssh -X`/MobaXterm session too -- but the daemon itself,
not the client, is what actually has to authenticate to that display, and it
only had one `XAUTHORITY` to try.  `systemd --user` imports the graphical
session's own into every unit by default, which points at the Xwayland cookie
file for the *local* display and has no entry for a forwarded one; `ssh -X`
writes its cookie to the ordinary `~/.Xauthority` instead.  A daemon that only
knows the first can't open the second: `x-open-connection` failed with
"Display ... can't be opened", and `server-create-window-system-frame` in
server.el swallows exactly that error and falls back to a *tty* frame in the
client's terminal without saying why -- which is what made this look like
`emacs` was choosing the console on its own.

`emacs.service` points `XAUTHORITY` at `~/.Xauthority` instead, so it is the
one file the daemon ever authenticates through, and `_emacs_merge_xauth` (in
`bash-emacs.sh`, run every time `emacs` needs the daemon) merges the local
Xwayland cookie into it -- needed because Wayland hands out a fresh, randomly
named cookie file every graphical login, so the merge has to happen again each
time rather than once. `ssh -X` already writes straight into `~/.Xauthority`,
so a forwarded session needs no merge at all.

`emacsreset` is how a change to `emacs-config.el` gets picked up.  The daemon
holds the config it was started with for as long as it runs, which is why a
fortnight-old daemon was still copying to the tmux buffer and nowhere else long
after that was fixed.  It saves every modified buffer first -- it restarts the
process, not the work -- and the agent conversations are in tmux rather than in
Emacs, so they survive it; `C-c a i` brings them back.

```sh
emacsreset          # the daemon
emacsreset NAME     # some other server, from `M-x server-start'
```

## R formatting with Air

[Air](https://posit-dev.github.io/air/) formats R code, and Emacs runs it
through eglot as a language server.  `setup.sh` and `wsl.sh` install it into
`~/.local/bin`; on a machine that already has Emacs:

```sh
curl -LsSf https://github.com/posit-dev/air/releases/latest/download/air-installer.sh | AIR_NO_MODIFY_PATH=1 sh
```

Every local R file starts `air language-server`, so `M-x eglot-format` formats
the buffer, or the region when there is one, in any package.  Air does nothing
but format, so eglot is kept away from what ESS already does: flymake still
runs lintr, and `M-.` and imenu still go through ESS.  R chunks inside Rmd, qmd
and Rnw files are left alone.  polymode keeps them in indirect buffers that
borrow the document's file name, and eglot would otherwise adopt them once Air
is running for the project -- after which `eglot-format` in a chunk formats the
whole document as R and wrecks the markdown around it.  Where no `air.toml` sets the indent, Air takes it from the
editor, and eglot offers `tab-width` -- 8 -- for that; `my-air-indent-as-ess`
hands it `ess-indent-offset` instead, so R still comes back indented by 2.

Saving formats the file only in a project with an `air.toml` (or `.air.toml`)
at or above it.  That file is how a package says it has moved to Air, and it is
also where the style lives -- `line-width`, `indent-width`, and `skip` for calls
such as `ini()` and `model()` whose DSL formatting would mangle.  Without one,
saving a one-line change in a package still on the old style would restyle the
whole file and bury the change in the diff.  To move a package over, add the
file and format it all at once, so the restyle lands as a commit of its own:

```sh
air format .
```
