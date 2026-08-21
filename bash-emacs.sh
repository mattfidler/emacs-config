# bash-emacs.sh -- run Emacs through the daemon, and nothing else.
#
# Sourced from ~/.bashrc.  A second Emacs is a second copy of every package and
# of the native-compilation cache, and it cannot see the buffers -- agent
# terminals included -- that the running one already has, so `emacs' here is a
# client of the daemon rather than a new Emacs: a graphical frame where there is
# a display to put one on, a terminal frame where there is not.  The daemon is
# started if it is not running yet, through systemd on machines where systemd
# manages it, so it is supervised the same way either way.
#
# The calls that cannot be served by a client are handed to the real binary
# untouched -- --batch, --script, -Q and -q have no init to share, and --daemon
# is what we would be connecting to -- so scripts keep working.
#
# emacsreset (~/.local/bin/emacsreset) stops the daemon; the next `emacs' starts
# a fresh one.

# The daemon reads only ~/.Xauthority (see emacs.service) so that a client on
# any display -- local or ssh -X forwarded -- can authenticate to it.  An ssh
# session's cookie already lands there by itself, but a local graphical login
# gets its own, differently-named Xwayland/X auth file instead, which the
# daemon never sees; merge it in so a local `emacsclient -c' still works.
# Cheap and idempotent, so it is fine to run on every shell that has one.
_emacs_merge_xauth() {
    local xauth_file="${XAUTHORITY:-$HOME/.Xauthority}"
    [ "$xauth_file" = "$HOME/.Xauthority" ] && return 0
    [ -r "$xauth_file" ] || return 0
    command -v xauth >/dev/null 2>&1 || return 0
    xauth -f "$HOME/.Xauthority" merge "$xauth_file" >/dev/null 2>&1
}

# Bring the daemon up if it is not already answering.  Prints nothing on the way.
_emacs_ensure_daemon() {
    _emacs_merge_xauth
    emacsclient --suppress-output --eval t >/dev/null 2>&1 && return 0

    if systemctl --user list-unit-files emacs.service >/dev/null 2>&1; then
        systemctl --user start emacs >/dev/null 2>&1
    else
        command emacs --daemon >/dev/null 2>&1
    fi

    # `systemctl start' returns once the daemon has forked, but the socket can
    # take a moment longer on a cold start that has packages to compile.
    local i
    for i in $(seq 1 60); do
        emacsclient --suppress-output --eval t >/dev/null 2>&1 && return 0
        sleep 0.5
    done

    printf "emacs: the daemon did not come up; start it with 'emacs --daemon'\n" >&2
    return 1
}

emacs() {
    local arg terminal=0
    local -a args=()

    for arg in "$@"; do
        case $arg in
            # No init to share, or already a daemon: give these to the binary.
            -Q|-q|--quick|--no-init-file|--no-site-file|-batch|--batch|\
            --script|--script=*|-scriptload|--version|--help|\
            --daemon|--daemon=*|--fg-daemon|--fg-daemon=*|--bg-daemon|--bg-daemon=*)
                command emacs "$@"
                return $?
                ;;
            # Asked for a terminal frame; emacsclient spells it the same way.
            -nw|-t|--tty|--no-window-system)
                terminal=1
                ;;
            *)
                args+=("$arg")
                ;;
        esac
    done

    _emacs_ensure_daemon || return $?

    if [ "$terminal" -eq 1 ] ||
           { [ -z "${DISPLAY-}" ] && [ -z "${WAYLAND_DISPLAY-}" ]; }; then
        emacsclient -t ${args[@]+"${args[@]}"}
    else
        emacsclient -c ${args[@]+"${args[@]}"}
    fi
}
