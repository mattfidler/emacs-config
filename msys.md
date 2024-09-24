# Installing with msys

Before beginning you may wish to update your package database:

```sh
pacman -Fy
```

You can also update the installed packages with:

```sh
pacman -Suy
```

This will give you emacs from any msys system with pacman:

```sh
pacman -S mingw-w64-x86_64-emacs
```

Unfortunately any shell would need msys to be loaded first, so to have
the most functionality you will need to run from a msys shell.

There is some discussion on msys launchers
[here](https://www.msys2.org/wiki/Launchers/).

This will allow you to access the shell command from within emacs
(otherwise it will simply show nothing).  It will also allow you to
use the stack from msys including `aspell`.

This can be installed with:

```sh
pacman -S mingw-w64-x86_64-aspell-en
```

Other packages that may be helpful are

```sh
pacman -S mingw-w64-x86_64-ripgrep
```

Also need `curl` installed

```sh
pacman -S mingw-w64-x86_64-curl
```

For using R, add the R path to the `exec-path`:

```emacs-lisp
(when (file-exists-p "c:/Progra~1/R/R-4.4.0/bin/x64")
  (add-to-list 'exec-path "c:\\Progra~1\\R\\R-4.4.0\\bin\\x64"))
```
