# emacs-config

Matt's Emacs configuration, plus the few things outside Emacs it depends on.

`emacs-config.el` is the configuration itself; `setup.sh` (Ubuntu) and `wsl.sh`
(WSL) build Emacs and install everything around it.  `msys.md`, `wsl.md` and
`kobo.md` are notes for those platforms.

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
