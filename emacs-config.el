;;; emacs-config.el --- Sample emacs configuration
;;
;; Filename: emacs-config.el
;; Description: Suggested ergoemacs-mode configuration
;; Author: Matthew L. Fidler
;; Maintainer: Matthew L. Fidler
;; Created: Fri Sep  4 09:37:09 2015 (-0500)
;; Version: 0.1
;; Package-Requires: (ergoemacs-mode)
;;
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(setq nvs nil)

(when (file-exists-p "/bin/bash")
  (setq-default explicit-shell-file-name "/bin/bash")
  (setq-default shell-file-name "/bin/bash")
  (setenv "ESHELL" "/bin/bash"))

(let ((site-lisp-dir "/CHBS/apps/EB/software/Emacs/29.4-GCCcore-11.2.0-extensions/site-lisp/elpa/"))
  (when (file-exists-p site-lisp-dir)
    (add-to-list 'load-path site-lisp-dir)
    (require 'ess-site)
    (setq nvs t)))

(let ((site-lisp-dir "~/src/ESS/lisp/"))
  (when (file-exists-p site-lisp-dir)
    (add-to-list 'load-path site-lisp-dir)
    (require 'ess-site)))



(when (eq system-type 'windows-nt)
  (require 'tramp)
  (setq tramp-default-method "plinkx")

  (setenv "NODE_TLS_REJECT_UNAUTHORIZED" "0")
  (defun add-to-win-path (path)
    (let ((win-path (replace-regexp-in-string "[/]" "\\\\" path)))
      (when (file-exists-p path)
        (add-to-list 'exec-path win-path)
        (setenv "PATH" (concat "\"" win-path "\";" (getenv "PATH"))))))

  (add-to-win-path "C:/Progra~1/Git/usr/bin")
  (let* ((rstudio-bin "C:/Program Files/RStudio"))
    (add-to-win-path (concat rstudio-bin "/resources/app/bin/quarto/bin"))
    (add-to-win-path (concat rstudio-bin "/resources/app/bin/quarto/bin/tools"))
    (add-to-win-path (concat rstudio-bin "/resources/app/bin/gnudiff"))
    (add-to-win-path (concat rstudio-bin "/resources/app/bin/gnugrep/3.0"))
    (add-to-win-path (concat rstudio-bin "/resources/app/bin/node")))

  (dolist (p '("C:/Program Files/R/R-4.4.0/bin/x64"
               "C:/R/extra/bin"
               "C:/Program Files/nodejs"))
    (add-to-win-path p))
  (when (file-exists-p "C:/Program Files/RStudio/resources/app/bin/node")
    (add-to-list 'exec-path "C:\\Program Files\\RStudio\\resources\\app\\bin\\node")
    (setq copilot-node-executable "C:\\Program Files\\RStudio\\resources\\app\\bin\\node\\node.exe")))

(when (file-exists-p "~/src/org-mode")
  (add-to-list 'load-path "~/src/org-mode")
  (require 'org))

(unless (or (file-exists-p "c:/WINDOWS/System32/WindowsPowerShell/v1.0/powershell.exe")
            (file-exists-p "~/.emacs.d/.cache/copilot"))
  (unless (package-installed-p 'quelpa)
    (with-temp-buffer
      (url-insert-file-contents "https://raw.githubusercontent.com/quelpa/quelpa/master/quelpa.el")
      (eval-buffer)
      (quelpa-self-upgrade)))

  (defvar bootstrap-version)
  (let ((bootstrap-file
         (expand-file-name
          "straight/repos/straight.el/bootstrap.el"
          (or (bound-and-true-p straight-base-dir)
              user-emacs-directory)))
        (bootstrap-version 7))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage)))

(when (package-installed-p 'quelpa)
  (quelpa
   '(quelpa-use-package
     :fetcher git
     :url "https://github.com/quelpa/quelpa-use-package.git")))

(condition-case nil
    (require 'quelpa-use-package)
  (error nil))

(use-package nerd-icons
  :ensure t
  :config
  (setq nerd-icons-font-family "Symbols Nerd Font Mono"))

;; Windows emacs unicode does not work so well for me, try to fix it
;; here
(when (eq system-type 'windows-nt)
  (use-package persistent-soft
    :quelpa (persistent-soft
             :fetcher github
             :repo "rolandwalker/persistent-soft"
             :branch "master"
             :files ("*.el")))

  (use-package font-utils
    :quelpa (font-utils
             :fetcher github
             :repo "rolandwalker/font-utils"
             :branch "master"
             :files ("*.el"))
    :after persistent-soft)

  (use-package ucs-utils
    :quelpa (ucl-utils
             :fetcher github
             :repo "rolandwalker/ucs-utils"
             :branch "master"
             :files ("*.el"))
    :after font-utils)

  (use-package list-utils
    :quelpa (list-utils
             :fetcher github
             :repo "rolandwalker/list-utils"
             :branch "master"
             :files ("*.el"))
    :after ucs-utils)

  (use-package unicode-fonts
    :quelpa (unicode-fonts
             :fetcher github
             :repo "rolandwalker/unicode-fonts"
             :branch "master"
             :files ("*.el"))
    :after list-utils
    :config
    (unicode-fonts-setup)))

(use-package kind-icon
  :ensure t
  :after company
  :config
  (let* ((kind-func (lambda (cand) (company-call-backend 'kind cand)))
         (formatter (kind-icon-margin-formatter `((company-kind . ,kind-func)))))
    (defun my-company-kind-icon-margin (cand _selected)
      (funcall formatter cand))
    (setq company-format-margin-function #'my-company-kind-icon-margin)))

(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))

(use-package doom-modeline
  :ensure t
  :config
  (doom-modeline-mode 1))

(use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :ensure t
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))

  ;; The :init section is always executed.
  :init

  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))

(use-package nerd-icons-completion
  :ensure t
  :after nerd-icons
  :config
  (nerd-icons-completion-mode 1)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package treemacs-nerd-icons
  :ensure t
  :after nerd-icons
  :config
  (treemacs-load-theme "nerd-icons"))

(use-package nerd-icons-ibuffer
  :ensure t
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

(use-package page-break-lines
  :ensure t
  :config
  (global-page-break-lines-mode t))

(use-package which-key
  :ensure t
  :config
  (setq which-key-idle-delay 0.5)
  (which-key-mode))

(use-package projectile
  :ensure t
  :config
  (projectile-mode +1)

  (defun projectile-is-rstudio-p (dir)
    (condition-case nil
        (let ((found nil))
          (dolist (f (directory-files dir))
            (when (string-match "\\.Rproj$" f)
              (setq found t)))
          found)
      (error nil)))

  (projectile-register-project-type 'rstudio-project #'projectile-is-rstudio-p
                                    ;; :compile "R CMD INSTALL ."
                                    ;; :test "R CMD check ."
                                    ;; :run "Rscript -e 'devtools::load_all()'"
                                    ;; :test-suffix "_test"
                                    )
  :init
  (with-eval-after-load 'projectile
    (define-key projectile-command-map (kbd "C-r") 'projectile-replace-regexp))
  (if (and (file-directory-p "~/src")
           (file-directory-p "~/projects"))
      (setq projectile-project-search-path '("~/src"
                                             "~/projects"))
    (if (file-directory-p "~/src")
        (setq projectile-project-search-path '("~/src"))
      (if (file-directory-p "~/projects")
          (setq projectile-project-search-path '("~/projects")))))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-center-content t)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-set-navigator t)
  (setq dashboard-projects-backend 'projectile)
  (setq initial-buffer-choice (lambda () (get-buffer-create dashboard-buffer-name)))
  (setq dashboard-items '((projects . 5)
                          (recents  . 5)
                          (bookmarks . 5)
                          (agenda . 5)
                          (registers . 5)))
  (setq dashboard-set-init-info t)
  (setq dashboard-set-footer nil)
  (setq dashboard-set-navigator t))

(use-package dirvish
  :ensure t
  :init
  (dirvish-override-dired-mode)
  (require 'dervish-vc)
  :config
  (setq dirvish-mode-line-format
        '(:left (sort symlink) :right (omit yank index)))
  (setq dirvish-mode-line-height 10)
  (setq dirvish-attributes
        '(nerd-icons file-time file-size collapse subtree-state vc-state git-msg))
  (setq dirvish-subtree-state-style 'nerd)
  (setq delete-by-moving-to-trash t)
  (setq dirvish-path-separators (list
                                 (format "  %s " (nerd-icons-codicon "nf-cod-home"))
                                 (format "  %s " (nerd-icons-codicon "nf-cod-root_folder"))
                                 (format " %s " (nerd-icons-faicon "nf-fa-angle_right"))))
  (setq dired-listing-switches
        "-l --almost-all --human-readable --group-directories-first --no-group")
  )


(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(unless (file-exists-p "~/.emacs.d/.cache/copilot")


  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package)))

(setq use-package-always-ensure t)

(eval-when-compile
  (require 'use-package))

(if (file-exists-p "~/src/ergoemacs-mode")
    (add-to-list 'load-path "~/src/ergoemacs-mode")
  (add-to-list 'load-path "~/.emacs.d/ergoemacs-mode"))

(require 'ergoemacs-mode)

(define-key ergoemacs-user-keymap (kbd "C-p") 'projectile-command-map)

;;; Reaching a command from the menu key.
;;
;; `ergoemacs-define-key' translates the key it is given from a us layout to
;; this one, and `ergoemacs-override-keymap' is ergoemacs-mode's own map, which
;; it rewrites whenever it installs a theme.  Both of those bit the two bindings
;; below.  Passing <apps> as the key and the letter as EXTRA-KEYS translates
;; <apps> to <menu> and appends the letter untranslated, so `<apps> p' asked for
;; <menu> p -- and ergoemacs's reduction theme binds `<apps> r' to `goto-map',
;; which on colemak translates to that very key.  Same map, same sequence, and
;; the theme is written last, so mu4e lost and <apps> p ran goto-map.
;;
;; So bind the way `transient-apps' below is bound and has always worked: a
;; plain `define-key' of the literal sequence, in `ergoemacs-user-keymap', which
;; is the user's own map and outranks the override one.  Both spellings of the
;; key, since X sends <menu> and Windows sends <apps>.

(defun my-define-menu-key (letter command)
  "Bind the menu key followed by LETTER to COMMAND, however the key arrives."
  (define-key ergoemacs-user-keymap (kbd (concat "<apps> " letter)) command)
  (define-key ergoemacs-user-keymap (kbd (concat "<menu> " letter)) command))



(use-package treemacs
  :ensure t
  :config
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (setq treemacs-fringe-indicator-mode t)
  (setq treemacs-git-mode 'deferred)
  (setq treemacs-resize-icons 10)
  (setq treemacs-silent-refresh t)
  (setq treemacs-silent-filewatch t)
  (setq treemacs-width 22)
  (setq treemacs-position 'left)
  (setq treemacs-follow-after-init t)
  (setq treemacs-is-never-other-window t)
  (setq treemacs-no-png-images t)
  (setq treemacs-no-delete-other-windows t)
  (setq treemacs-missing-project-action 'ask)
  (setq treemacs-recenter-after-project-expand 'on-distance)
  (setq treemacs-recenter-after-file-follow 'on-distance)
  (setq treemacs-recenter-after-tag-follow 'on-distance)
  (setq treemacs-recenter-after-project-jump 'on-distance)
  (setq treemacs-recenter-after-project-collapse 'on-distance)
  (setq treemacs-recenter-after-file-follow 'on-distance)
  (setq treemacs-recenter-after-tag-follow 'on-distance)
  (setq treemacs-recenter-after-project-jump 'on-distance)
  (setq treemacs-recenter-after-project-collapse 'on-distance)
  (setq treemacs-recenter-after-project-expand 'on-distance)
  (setq treemacs-recenter-after-project-collapse 'on-distance)
  (setq treemacs-recenter-after-file-follow 'on-distance)
  (setq treemacs-recenter-after-tag-follow 'on-distance)
  (setq treemacs-recenter-after-project-jump 'on-distance)
  (setq treemacs-recenter-after-project-collapse 'on-distance)
  (setq treemacs-recenter-after-project-expand 'on-distance)
  (setq treemacs-recenter-after-project-collapse 'on-distance)
  (setq treemacs-recenter-after-file-follow 'on-distance)
  (setq treemacs-recenter-after-tag-follow 'on-distance)
  (setq treemacs-recenter-after-project-jump 'on-distance)
  (setq treemacs-recenter-after-project-collapse 'on-distance)
  (setq treemacs-recenter-after-project-expand 'on-distance)
  (setq treemacs-recenter-after-project-collapse 'on-distance)
  (setq treemacs-recenter-after-file-follow 'on-distance)
  (setq treemacs-recenter-after-tag-follow 'on-distance)
  (setq treemacs-show-hidden-files t)
  (my-define-menu-key "q" #'treemacs))

(use-package powershell)

(use-package transient
  :config
  (transient-define-prefix transient-apps ()
    "Apps"
    ["Applications"
     ("c" "Calc" calc)
     ("d" "dired" dired-jump)
     ("p" "Powershell" powershell)
     ("b" "Buffer" consult-buffer)
     ("g" "grep" grep)
     ("G" "consult ripgrep" consult-ripgrep)
     ("m" "magit" magit-status)
     ("o" "open externally" ergoemacs-open-in-external-app)
     ("s" "shell" shell)
     ("t" "treemacs" treemacs)
     ("r" "R" R)
     ("e" "mc/edit-lline" mc/edit-lines)
     ("a" "avy goto word" avy-goto-word-or-subword-1)
     ("l" "avy goto line" avy-goto-line)
     ;; <apps> k h: the agent for wherever point is -- see `claude-dwim'.
     ("h" "claude" claude-dwim)
     ("H" "antigravity" agy-dwim)
     ("C" "copilot" copilot-cli-dwim)
     ("O" "opencode" opencode-dwim)
     ("K" "kilo" kilo-dwim)
     ])
  ;;(define-key ergoemacs-override-keymap (kbd "<menu> k") nil)
  ;;(define-key ergoemacs-override-keymap (kbd "<apps> k") nil)
  ;;(ergoemacs-define-key ergoemacs-user-keymap (kbd "<menu> n") 'R (kbd "r"))
  (define-key ergoemacs-user-keymap (kbd "<apps> k") 'transient-apps)
  (define-key ergoemacs-user-keymap (kbd "<menu> k") 'transient-apps))

;;; Mail: gmail as a maildir.
;;
;; mbsync (~/.mbsyncrc) keeps ~/.mail/gmail in step with gmail, mu indexes it
;; and mu4e reads it, so mail is searchable and readable with no network.  The
;; app password lives in ~/.authinfo.gpg and nowhere else: mbsync asks
;; ~/.local/bin/authinfo-pass for it, Emacs asks auth-source for it.  README.md
;; has the install and the first sync.
;;
;; mu4e ships with mu, so it is wherever the mu indexing the mail came from: a
;; build of your own puts it in /usr/local/share/emacs/site-lisp/mu4e, while the
;; ubuntu package hands it to dh-elpa, which byte-compiles it into site-lisp/elpa
;; for each *packaged* Emacs -- and this Emacs, built into /usr/local, is not one
;; of those, so the only copy is the source in site-lisp/elpa-src.  Take whichever
;; exists.  Where there is no mu there is no mail, and this whole block sits out.

(let ((dir (seq-find
            (lambda (d) (file-exists-p (expand-file-name "mu4e.el" d)))
            (append '("/usr/local/share/emacs/site-lisp/mu4e")
                    (file-expand-wildcards "/usr/share/emacs/site-lisp/elpa/mu4e-*")
                    (file-expand-wildcards "/usr/share/emacs/site-lisp/elpa-src/mu4e-*")))))
  (when dir
    (add-to-list 'load-path dir)))

(when (and (executable-find "mu") (require 'mu4e nil t))
  (setq user-full-name "Matthew L. Fidler"
        user-mail-address "matthew.fidler@gmail.com"

        ;; Folder names are the local ones from ~/.mbsyncrc, not gmail's own
        ;; bracketed ones.
        mu4e-sent-folder "/Sent"
        mu4e-drafts-folder "/Drafts"
        mu4e-trash-folder "/Trash"
        ;; Leaving the inbox is what archiving is on gmail, and the delete mark
        ;; (D) does exactly that: the message goes out of Inbox and stays in All
        ;; Mail.  Refile has nowhere local to go while All Mail is not synced,
        ;; so keep it in the inbox rather than have it invent a folder mbsync
        ;; will never look at.
        mu4e-refile-folder "/Inbox"
        ;; mbsync must be free to rename a file when flags change.
        mu4e-change-filenames-when-moving t

        mu4e-get-mail-command "mbsync -a"
        mu4e-update-interval 300
        mu4e-index-cleanup t
        mu4e-index-lazy-check nil
        ;; Gmail hands the same message out under several labels; this was
        ;; mu4e-headers-skip-duplicates before 1.10 renamed it.
        mu4e-search-skip-duplicates t

        ;; gmail files a copy of everything sent through its smtp server, so
        ;; keeping our own would show every sent message twice.
        mu4e-sent-messages-behavior 'delete

        ;; Jump to a folder with "j" and one of these.
        mu4e-maildir-shortcuts '((:maildir "/Inbox"   :key ?i)
                                 (:maildir "/Sent"    :key ?s)
                                 (:maildir "/Drafts"  :key ?d)
                                 (:maildir "/Trash"   :key ?t)
                                 (:maildir "/Starred" :key ?*))

        mu4e-attachment-dir "~/Downloads"
        mu4e-use-fancy-chars t
        mu4e-compose-signature-auto-include nil
        mu4e-confirm-quit nil

        message-send-mail-function 'smtpmail-send-it
        message-sendmail-envelope-from 'header
        message-citation-line-format "On %a, %b %d %Y, %f wrote:\n"
        message-kill-buffer-on-exit t
        smtpmail-smtp-server "smtp.gmail.com"
        smtpmail-smtp-service 587
        smtpmail-stream-type 'starttls
        smtpmail-smtp-user "matthew.fidler@gmail.com")

  (my-define-menu-key "p" #'mu4e))


(use-package ergoemacs-mode
  :init
  (setq ergoemacs-theme "reduction"
        ergoemacs-keyboard-layout "colemak"
        ergoemacs-beginning-or-end-of-line-and-what 'page
        ergoemacs-smart-paste t))

(use-package editorconfig)
(use-package jsonrpc)

(use-package markdown-mode
  :ensure t
  :config
  (add-hook 'markdown-mode-hook
            (lambda ()
              (setq markdown-command "pandoc -f markdown -t html -s"))))

(use-package consult
  :ensure t
  :after transient
  :config
  (defun my/consult-flymake-or-flycheck()
    (interactive)
    (if (bound-and-true-p flymake-mode)
        (consult-flymake)
      (consult-flycheck)))

  (transient-define-prefix consult-prefix ()
    "Consult prefix"
    ["Actions"
     ("b" "Buffer" consult-buffer)
     ("f" "File" consult-file)
     ("g" "Grep" consult-ripgrep)
     ("l" "Line" consult-line)
     ("m" "Mark" consult-mark)
     ("o" "Outline" consult-outline)
     ("r" "Register" consult-register)
     ("c" "Complex Command" consult-complex-command)
     ("i" "Imenu" consult-imenu)
     ("k" "Global Mark" consult-global-mark)
     ("d" "Yank Pop" consult-yank-pop)
     ("p" "Project" consult-project)])

  ;; (global-set-key (kbd "C-x b") 'consult-buffer)
  ;; (global-set-key (kbd "C-x 4 b") 'consult-buffer-other-window)
  ;; (global-set-key (kbd "C-x 5 b") 'consult-buffer-other-frame)

  ;; (global-set-key (kbd "<menu> f k") 'consult-global-mark)
  ;; (global-set-key (kbd "<menu> f M-k") 'consult-global-mark)
  ;; (global-set-key (kbd "<menu> f i") 'consult-imenu)
  ;; (global-set-key (kbd "<menu> f M-i") 'consult-imenu)
  ;; (global-set-key (kbd "<menu> f f") 'my/consult-flymake-or-flycheck)
  ;; (global-set-key (kbd "<menu> f f") 'my/consult-flymake-or-flycheck)
  ;; (global-set-key (kbd "<menu> f r") 'consult-ripgrep)
  ;; (global-set-key (kbd "<menu> f M-r") 'consult-ripgrep)
  ;; (ergoemacs-define-key ergoemacs-override-keymap (kbd "<menu> n") 'grep (kbd "g"))
  ;; (global-set-key (kbd "<menu> f l") 'consult-line)
  ;; (global-set-key (kbd "<menu> f M-l") 'consult-line)
  ;; (global-set-key (kbd "<menu> f c") 'consult-complex-command)
  ;; (global-set-key (kbd "<menu> f M-c") 'consult-complex-command)
  ;; (global-set-key (kbd "<menu> f s") 'consult-isearch)
  ;; (global-set-key (kbd "<menu> f M-s") 'consult-isearch)
  ;; (global-set-key (kbd "<menu> f b") 'consult-bookmark)
  ;; (global-set-key (kbd "<menu> f M-b") 'consult-bookmark)
  ;; (global-set-key (kbd "<menu> f d") 'consult-yank-pop)
  ;; (global-set-key (kbd "<menu> f M-d") 'consult-yank-pop)
  ;; (global-set-key (kbd "<menu> f p") 'consult-project)
  )

(use-package request)



(when (version< "24.4" emacs-version)
  (use-package electric-operator
    :ensure t))

(use-package magit-ido
  :ensure t)

(use-package golden-ratio
  :ensure t
  :config
  (defun ergoemacs-golden-ratio-exclude-minimap-p ()
    (memq this-command '(minimap-toggle)))
  (setq golden-ratio-inhibit-functions '(ergoemacs-golden-ratio-exclude-minimap-p))
  (setq golden-ratio-exclude-modes '("bs-mode"
                                     "calc-mode"
                                     "ediff-mode"
                                     "dired-mode"
                                     "gud-mode"
                                     "gdb-locals-mode"
                                     "gdb-registers-mode"
                                     "gdb-breakpoints-mode"
                                     "gdb-threads-mode"
                                     "gdb-frames-mode"
                                     "gdb-inferior-io-mode"
                                     "gud-mode"
                                     "gdb-inferior-io-mode"
                                     "gdb-disassembly-mode"
                                     "gdb-memory-mode"
                                     "restclient-mode"
                                     "speedbar-mode"
                                     "minimap-mode"))
  (setq golden-ratio-extra-commands
        (append golden-ratio-extra-commands
                '(ace-window
                  switch-window
                  delete-other-window
                  ace-delete-window
                  ace-select-window
                  ace-swap-window
                  ace-maximize-window
                  avy-pop-mark
                  windmove-left
                  windmove-right
                  windmove-up
                  windmove-down
                  select-window-0
                  select-window-1
                  select-window-2
                  select-window-3
                  select-window-4
                  select-window-5
                  select-window-6
                  select-window-7
                  select-window-8
                  select-window-9
                  buf-move-left
                  buf-move-right
                  buf-move-up
                  buf-move-down
                  ess-eval-buffer-and-go
                  ess-eval-function-and-go
                  ess-eval-line-and-go)))
  (add-to-list 'golden-ratio-exclude-buffer-regexp "^\\*[hH]elm.*")
  (add-to-list 'golden-ratio-exclude-buffer-regexp "^\\*MINIMAP\\*")
  (add-to-list 'golden-ratio-exclude-buffer-names " *NeoTree*")
  (add-to-list 'golden-ratio-exclude-buffer-names "*LV*")
  (add-to-list 'golden-ratio-exclude-buffer-names " *which-key*")
  (golden-ratio-mode 1))

;; ido mode
(dolist (ext '("elc" "exe" "com" "org_archive" "png" "gif" "csv" "jpg" "jpeg"))
  (push ext completion-ignored-extensions))
(setq ido-enable-prefix t
      ido-enable-flex-matching t
      ido-create-new-buffer 'always
      ido-use-filename-at-point nil
      ido-enable-tramp-completion t
      ido-everywhere t
      org-completion-use-ido t
      ido-max-prospects 10
      ido-use-virtual-buffers t
      ido-default-file-method 'selected-window
      ido-ignore-extensions t
      ido-file-extensions-order '(".org" ".R" ".ctl" ".pltc" ".nsi" ".txt" ".py" ".emacs" ".xml" ".el" ".ini" ".cfg" ".cnf" ".nsi" ".nsh")
      org-completion-use-ido t
      magit-completing-read-function 'magit-ido-completing-read
      gnus-completing-read-function 'gnus-ido-completing-read
      ido-enable-flex-matching t
      ido-use-faces nil
      flx-ido-threshold 10000
      gc-cons-threshold 20000000)

(ido-mode 1)

(when (version< "24.4" emacs-version)
  (use-package ido-completing-read+
    :ensure t
    :config
    (ido-ubiquitous-mode 1)))

(use-package smex
  :ensure t
  :config
  (smex-initialize))

(use-package ido-vertical-mode
  :ensure t
  :config
  (ido-vertical-mode))

(use-package flx-ido
  :ensure t
  :config
  (flx-ido-mode 1))

(when (executable-find "rg")
  (use-package rg
    :ensure t))

(when (executable-find "ag")
  (use-package ag
    :ensure t))

(use-package ligature
  :ensure t
  :config
  ;; Enable the www ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))

  ;; Enable ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\" "{-" "::"
                                       ":::" ":=" "!!" "!=" "!==" "-}" "----" "-->" "->" "->>"
                                       "-<" "-<<" "-~" "#{" "#[" "##" "###" "####" "#(" "#?" "#_"
                                       "#_(" ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*" "/**"
                                       "/=" "/==" "/>" "//" "///" "&&" "||" "||=" "|=" "|>" "^=" "$>"
                                       "++" "+++" "+>" "=:=" "==" "===" "==>" "=>" "=>>" "<="
                                       "=<<" "=/=" ">-" ">=" ">=>" ">>" ">>-" ">>=" ">>>" "<*"
                                       "<*>" "<|" "<|>" "<$" "<$>" "<!--" "<-" "<--" "<->" "<+"
                                       "<+>" "<=" "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<"
                                       "<~" "<~~" "</" "</>" "~@" "~-" "~>" "~~" "~~>" "%%"))
  (global-ligature-mode 't))

(setq-default indent-tabs-mode nil
              indicate-empty-lines t
              imenu-auto-rescan t
              text-mode-hook 'turn-on-auto-fill
              text-mode-hook 'turn-on-flyspell)

(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(setq visible-bell t
      echo-keystrokes 0.1
      font-lock-maximum-decoration t
      inhibit-startup-message t
      color-theme-is-global t
      delete-by-moving-to-trash t
      shift-select-mode t
      truncate-partial-width-windows nil
      uniquify-buffer-name-style 'forward
      whitespace-style '(trailing lines space-before-tab indentation space-after-tab)
      whitespace-line-column 100
      ediff-window-setup-function 'ediff-setup-windows-plain
      xterm-mouse-mode t
      save-place-file (expand-file-name "saveplace.el" user-emacs-directory)
      save-place t)
(transient-mark-mode t)
(defalias 'yes-or-no-p 'y-or-n-p)

(if (version< "24.4" emacs-version)
    (progn
      (use-package company
        :ensure t
        :init

        (setq company-selection-wrap-around t
              company-tooltip-align-annotations t
              company-idle-delay 0.45
              company-minimum-prefix-length 3
              company-tooltip-limit 10))
      (use-package company-box)
      :init
      (add-hook 'prog-mode-hook 'company-box-mode))
  (when (file-exists-p "~/.emacs.d/company-mode")
    (add-to-list 'load-path "~/.emacs.d/company-mode")
    (require 'company)
    (add-hook 'after-init-hook 'global-company-mode)
    (setq company-selection-wrap-around t
          company-tooltip-align-annotations t
          company-idle-delay 0.45
          company-minimum-prefix-length 3
          company-tooltip-limit 10)))

(setq set-mark-command-repeat-pop t)

(savehist-mode 1)

(if (version< "29.0" emacs-version)
    (add-hook 'prog-mode-hook #'display-line-numbers-mode)
  (global-linum-mode 1)
  (use-package linum-off
    :ensure t
    :config
    (global-linum-mode 1)))
(global-subword-mode 1)

(defun ergoemacs-remove-elc-on-save ()
  "If you're saving an elisp file, likely the .elc is no longer valid."
  (make-local-variable 'after-save-hook)
  (add-hook 'after-save-hook
            (lambda ()
              (if (file-exists-p (concat buffer-file-name "c"))
                  (delete-file (concat buffer-file-name "c"))))))
(add-hook 'emacs-lisp-mode-hook #'ergoemacs-remove-elc-on-save)
(add-hook 'emacs-lisp-mode-hook #'turn-on-eldoc-mode)

(use-package visual-regexp
  :ensure t
  :commands (vr/query-replace vr/replace)
  :init
  (global-set-key [remap query-replace] 'vr/query-replace)
  (global-set-key [remap replace] 'vr/replace))


(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package rainbow-mode
  :ensure t
  :config
  (rainbow-mode t))

;; (use-package indent-guide
;;   :ensure t
;;   :config
;;   (indent-guide-global-mode 1))

(when (version<  "24.4" emacs-version)
  (use-package page-break-lines
    :ensure t
    :config
    (global-page-break-lines-mode t)))

(use-package volatile-highlights
  :ensure t
  :config
  (volatile-highlights-mode))

;; (use-package tabbar
;;   :ensure t)

;; (use-package tabbar-ruler
;;   :ensure t
;;   :config
;;   (setq tabbar-ruler-global-tabbar t ; If you want tabbar
;;         ;;tabbar-ruler-global-ruler t ; if you want a global ruler
;;         ;;tabbar-ruler-popup-menu nil ; If you want a popup menu.
;;         ;;tabbar-ruler-popup-toolbar nil ; If you want a popup toolbar
;;         ;;tabbar-ruler-popup-scrollbar nil
;;         ;; tabbar-ruler-style 'firefox-circle
;;         ) ; Popup scrollbar
;;   (require 'tabbar)
;;   (require 'tabbar-ruler))

(use-package snap-indent
  :ensure t
  :hook (prog-mode . snap-indent-mode)
  :custom ((snap-indent-format 'untabify)
           (snap-indent-on-save nil)))

(global-visual-line-mode 1)

(global-hl-line-mode 1)

(modify-all-frames-parameters (list (cons 'cursor-type 'bar)))


(defun del-trailing()
  "Delete trailing whitespace."
  (add-hook 'write-file-hooks 'delete-trailing-whitespace))

(add-hook 'prog-mode-hook 'del-trailing)


                                        ;(setq pop-up-frames 'graphic-only)

;; (when (display-graphic-p)
;;   (if (version< "24.4" emacs-version)
;;       :ensure t
;;       :init
;;       (setq tabbar-ruler-global-tabbar t ; If you want tabbar
;;             ;;tabbar-ruler-global-ruler t ; if you want a global ruler
;;             ;;tabbar-ruler-popup-menu nil ; If you want a popup menu.
;;             ;;tabbar-ruler-popup-toolbar nil ; If you want a popup toolbar
;;             ;;tabbar-ruler-popup-scrollbar nil
;;             ;; tabbar-ruler-style 'firefox-circle
;;             ) ; Popup scrollbar
;;       )
;;   (when (file-exists-p "~/.emacs.d/tabbar")
;;     (add-to-list 'load-path "~/.emacs.d/tabbar")
;;     (require 'tabbar))
;;   (when (file-exists-p "~/.emacs.d/mode-icons")
;;     (add-to-list 'load-path "~/.emacs.d/mode-icons")
;;     (require 'mode-icons))
;;   (when (file-exists-p "~/.emacs.d/tabbar-ruler.el")
;;     (add-to-list 'load-path "~/.emacs.d/tabbar-ruler.el")
;;     (require 'tabbar-ruler)
;;     (setq tabbar-ruler-global-tabbar t ; If you want tabbar
;;           ;;tabbar-ruler-global-ruler t ; if you want a global ruler
;;           ;;tabbar-ruler-popup-menu nil ; If you want a popup menu.
;;           ;;tabbar-ruler-popup-toolbar nil ; If you want a popup toolbar
;;           ;;tabbar-ruler-popup-scrollbar nil
;;           ;; tabbar-ruler-style 'firefox-circle
;;           ) ; Popup scrollbar
;;     ))

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default tab-always-indent 'complete)

(setq minibuffer-prompt-properties (quote (read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt)))


(setq whitespace-style (quote (spaces tabs newline space-mark tab-mark newline-mark)) ;; make whitespace-mode use just basic coloring
      whitespace-display-mappings '((space-mark 32 [183] [46]) ; 32 SPACE, 183 MIDDLE DOT 「·」, 46 FULL STOP 「.」
                                    (newline-mark 10 [182 10]) ; 10 LINE FEED
                                    (tab-mark 9 [9655 9] [92 9]) ; 9 TAB, 9655 WHITE RIGHT-POINTING TRIANGLE 「▷」
                                    ))

(mouse-wheel-mode t)

(savehist-mode 1)

(setq set-mark-command-repeat-pop t)
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))

(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(when (version< "24.4" emacs-version)
  (use-package keyfreq
    :ensure t
    :init
    (setq keyfreq-file (expand-file-name ".emacs.keyfreq" user-emacs-directory)
          keyfreq-file-lock (expand-file-name ".emacs.keyfreq.lock" user-emacs-directory))
    :config
    (keyfreq-mode 1)
    (keyfreq-autosave-mode 1)))

(auto-compression-mode t)


(global-subword-mode 1)


(define-key emacs-lisp-mode-map (kbd "C-c v") #'eval-buffer)
(define-key emacs-lisp-mode-map (kbd "C-c C-c") #'eval-defun)

(defun ergoemacs-remove-elc-on-save ()
  "If you're saving an elisp file, likely the .elc is no longer valid."
  (make-local-variable 'after-save-hook)
  (add-hook 'after-save-hook
            (lambda ()
              (if (file-exists-p (concat buffer-file-name "c"))
                  (delete-file (concat buffer-file-name "c"))))))
(add-hook 'emacs-lisp-mode-hook #'ergoemacs-remove-elc-on-save)
(add-hook 'emacs-lisp-mode-hook #'turn-on-eldoc-mode)


(if (version< "24.4" emacs-version)
    (progn
      (use-package magit
        :ensure t
        :after nerd-icons
        :commands (magit-status)
        ;; (add-hook 'magit-mode-hook #'turn-on-magit-gh-pulls)
        :custom
        (magit-format-file-function #'magit-format-file-nerd-icons))
      (use-package forge
        :after magit))
  (when (file-exists-p "~/.emacs.d/magit")
    (add-to-list 'load-path "~/.emacs.d/magit")
    (require 'magit)))

(use-package zenburn-theme
  :ensure t)
(use-package solarized-theme
  :ensure t)

(electric-pair-mode 1)

(when (version< "24.4" emacs-version)
  (use-package multiple-cursors
    :ensure t
    :config
    (ergoemacs-define-key ergoemacs-user-keymap (kbd "M-*") 'mc/mark-next-like-this)
    (ergoemacs-define-key ergoemacs-user-keymap (kbd "M-&") 'mc/edit-lines)))

(use-package avy
  :ensure t
  :config
  (ergoemacs-define-key ergoemacs-user-keymap (kbd "M-,") 'avy-goto-word-or-subword-1)
  (ergoemacs-define-key ergoemacs-user-keymap (kbd "M-.") 'avy-goto-line))

(if (version< "24.4" emacs-version)
    (use-package expand-region
      :commands (er/expand-region er/contract-region er/mark-inside-quotes)
      :ensure t
      :config
      (define-key ergoemacs-user-keymap (kbd "M-8") 'er/expand-region)
      (define-key ergoemacs-user-keymap (kbd "M-*") 'er/mark-inside-quotes))
  (when (file-exists-p "~/.emacs.d/expand-region.el")
    (add-to-list 'load-path "~/.emacs.d/expand-region.el")
    (use-package expand-region
      :commands (er/expand-region er/contract-region er/mark-inside-quotes)
      :config
      (define-key ergoemacs-user-keymap (kbd "M-8") 'er/expand-region)
      (define-key ergoemacs-user-keymap (kbd "M-*") 'er/mark-inside-quotes))))

(if (file-exists-p "~/src/ESS")
    (progn
      (add-to-list 'load-path "~/src/ESS/site-lisp")
      (add-to-list 'load-path "~/src/ESS/lisp"))
  (add-to-list 'load-path "~/.emacs.d/ESS/site-lisp")
  (add-to-list 'load-path "~/.emacs.d/ESS/lisp"))
                                        ;(unwind-protect (require 'ess)
(use-package ess
  :ensure nil
  :mode (("\\.sp\\'"          . S-mode) ;; re: Don MacQueen <macq@llnl.gov>
         ("/R/.*\\.q\\'"      . R-mode) ;; R/*.q is R code (e.g., in package)
         ("\\.[qsS]\\'"       . S-mode) ;; s,S [see ess-restore-asm-extns above!]
         ("\\.ssc\\'"         . S-mode) ;; Splus (>= 4.x) script files.
         ("\\.SSC\\'"         . S-mode) ;; ditto for windoze
         ("\\.[rR]\\'"        . R-mode)
         ("\\.[rR]nw\\'"      . Rnw-mode)
         ("\\.[sS]nw\\'"      . Snw-mode); currently identical to Rnw-mode
         ("\\.[rR]profile\\'" . R-mode)
         ("NAMESPACE\\'"      . R-mode)
         ("CITATION\\'"       . R-mode)
         ("\\.omg\\'"         . omegahat-mode)
         ("\\.hat\\'"         . omegahat-mode)
         ("\\.lsp\\'"         . XLS-mode)
         ("\\.do\\'"          . STA-mode)
         ("\\.ado\\'"         . STA-mode)
         ("\\.[Ss][Aa][Ss]\\'"        . SAS-mode)
         ;; Many .log/.lst files, not just SAS
         ;;("\\.log\\'"       . SAS-log-mode)
         ;;("\\.[Ll][Ss][Tt]\\'"      . SAS-listing-mode)
         ("\\.[Ss]t\\'"       . S-transcript-mode)
         ("\\.Sout"           . S-transcript-mode)
         ;;("\\.[Rr]t\\'"       . R-transcript-mode)
         ("\\.[Rr]out"        . R-transcript-mode)
         ("\\.Rd\\'"          . Rd-mode)
         ("\\.[Bb][Uu][Gg]\\'"         . ess-bugs-mode)
         ("\\.[Bb][Oo][Gg]\\'"         . ess-bugs-mode)
         ("\\.[Bb][Mm][Dd]\\'"         . ess-bugs-mode)
         ("\\.[Jj][Aa][Gg]\\'"         . ess-jags-mode)
         ("\\.[Jj][Oo][Gg]\\'"         . ess-jags-mode)
         ("\\.[Jj][Mm][Dd]\\'"         . ess-jags-mode)
         ;;("\\.[Rr][mM][Dd]\\'"         . poly-markdown+r-mode)
         )
  :commands (R)
  :interpreter (("Rscript" . r-mode)
                ("R" . r-mode))
  :custom
  (ess-indent-offset 2 t nil "Customized with use-package ess")
  (ess-r-flymake-linters
   '("closed_curly_linter = NULL" "commas_linter = NULL" "commented_code_linter = NULL" "infix_spaces_linter = NULL" "line_length_linter = NULL" "object_length_linter = NULL" "object_name_linter(styles = \"camelCase\")" "object_usage_linter = NULL" "open_curly_linter = NULL" "pipe_continuation_linter = NULL" "single_quotes_linter = NULL" "spaces_inside_linter = NULL" "spaces_left_parentheses_linter = NULL" "trailing_blank_lines_linter = NULL" "trailing_whitespace_linter = NULL"))
  :config
  (require 'ess-site)
  (require 'ess-autoloads)

  ;; https://github.com/chainsawriot/ess-rproj/blob/v0.0/ess-rproj.el
  ;; can't seem to load using melpa, so copy and modify here:
  (require 'ess)

  (defun read-proj (rproj)
    (with-temp-buffer
      (insert-file-contents rproj)
      (split-string (buffer-string) "\n" t)))


  (defun seek-rproj (directory)
    (car (directory-files (expand-file-name directory) t "\\.[Rr]proj$")))


  (defun get-rproj ()
    "if default directory is an R package, return full path to the root directory; otherwise, return full path of default directory"
    (setq-local root (plist-get (ess-r-package-info default-directory) :root))
    (cond ((null root) (seek-rproj (expand-file-name default-directory)))
          ((stringp root) (seek-rproj (expand-file-name root)))))

  (defun set-ess-indent-rproj ()
    (interactive)
    (setq rproj (get-rproj))
    (if (null rproj)
        (message "R Project file not found.")
      (progn
        (setq-local ess-indent-level
                    (string-to-number (nth 1 (seq-find (lambda (x) (string= (car x) "NumSpacesForTab"))
                                                       (mapcar #'(lambda (x) (split-string x ": ")) (read-proj rproj))))))
        (message "R Project file found. Set indentation to: %s." ess-indent-level))))

;;;###autoload
  (define-minor-mode ess-rproj
    "TBA"
    :lighter " rproj")

  (add-hook 'ess-mode-hook #'set-ess-indent-rproj)

  ;; Lets you do 'C-c C-c Sweave' from your Rnw file
  (defun ergoemacs-add-Sweave ()
    (add-to-list 'TeX-command-list
                 '("Sweave" "R CMD Sweave %s"
                   TeX-run-command nil (latex-mode) :help "Run Sweave") t)
    (add-to-list 'TeX-command-list
                 '("LatexSweave" "%l %(mode) %s"
                   TeX-run-TeX nil (latex-mode) :help "Run Latex after Sweave") t)
    (setq TeX-command-default "Sweave"))
  (add-hook 'Rnw-mode-hook 'emacsmate-add-Sweave)

  (add-hook 'emacs-startup-hook
            (lambda()
              (setq reftex-file-extensions
                    '(("Snw" "Rnw" "nw" "tex" ".tex" ".ltx") ("bib" ".bib"))
                    TeX-file-extensions
                    '("Snw" "Rnw" "nw" "tex" "sty" "cls" "ltx" "texi" "texinfo")
                    )))

  (defun myindent-ess-hook ()
    (setq ess-indent-level 2)
    (setq ess-offset-arguments-newline '(prev-line 2)))
  (add-hook 'ess-mode-hook 'myindent-ess-hook)
  (add-hook 'ess-mode-hook
            (lambda()
              (ess-set-style 'RStudio 'quiet)
              (add-hook 'local-write-file-hooks
                        (lambda ()
                          (ess-nuke-trailing-whitespace)))
              (ess-roxy-mode 1)
              ;;; This interferes with lintr infix operators
              ;;(electric-operator-mode)
              (run-hooks 'prog-mode-hook)
              (set (make-variable-buffer-local 'ess-indent-level) 2)
              (setq ess-offset-arguments-newline '(prev-line 2))))
  ;; Setup ASCII colors
  (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

  (setq ansi-color-for-comint-mode 'filter
        comint-scroll-to-bottom-on-input t
        comint-scroll-to-bottom-on-output t
        comint-move-point-for-output t)
  (add-to-list 'comint-output-filter-functions 'ansi-color-process-output)

  ;;Remove ESS smart underscore
  ;; (ess-toggle-underscore nil)
  :custom
  (ess-ask-for-ess-directory nil)
  (ess-indent-level 2)
  (ess-local-process-name "R")
  (ansi-color-for-comint-mode 'filter)
  (comint-scroll-to-bottom-on-input t)
  (comint-scroll-to-bottom-on-output t)
  (comint-move-point-for-output t)
  (ess-nuke-trailing-whitespace-p t)
  (ess-roxy-str "#'")
  (inferior-R-args "--no-save --quiet")
  (ess-insert-assign nil)
  (ess-user-full-name "Matthew L. Fidler")
  (ess-style 'RStudio)
  (ess-indent-with-fancy-comments nil)
  (ess-roxy-template-alist
   (list (cons "description"  " ")
         (cons "details" " ")
         (cons "param"  "")
         (cons "return" "")
         (cons "export" "")
         (cons "author" ess-user-full-name)
         (cons "examples" ""))))

(when (version< "24.4" emacs-version)
  (use-package poly-R
    :ensure t)

  (use-package poly-markdown
    :mode ("\\.[Rr][mM][Dd][Hh]\\'"         . poly-markdown+r-mode)
    :ensure t)

  ;; (use-package flycheck
  ;;   :config
  ;;   (global-flycheck-mode 1))

  (use-package undo-fu
    :ensure t
    :config
    (global-set-key [remap ergoemacs-redo] 'undo-fu-only-redo)
    (global-set-key [remap undo] 'undo-fu-only-undo)))

(use-package yaml-mode
  :ensure t
  :config
  (add-hook 'yaml-mode-hook
            (lambda ()
              (define-key yaml-mode-map "\C-m" 'newline-and-indent))))

(setq custom-safe-themes t)

(if (version< "24.4" emacs-version)
    (use-package smart-mode-line
      :ensure t
      :config
      (sml/setup))
  (when (file-exists-p "~/.emacs.d/rich-minority")
    (add-to-list 'load-path "~/.emacs.d/rich-minority")
    (require 'rich-minority))
  (when (file-exists-p "~/.emacs.d/smart-mode-line")
    (add-to-list 'load-path "~/.emacs.d/smart-mode-line")
    (require 'smart-mode-line)
    (sml/setup)))

(global-set-key (kbd "<f11>") 'toggle-frame-fullscreen)
(global-set-key (kbd "<f10>") 'menu-bar-mode)
(global-set-key (kbd "<f12>") 'tool-bar-mode)
(global-set-key (kbd "<f9>") 'tabbar-mode)

(menu-bar-mode 0)
(tool-bar-mode 0)

;;(ergoemacs-define-key ergoemacs-user-keymap (kbd "<menu> n") 'R (kbd "r"))


(add-hook 'inferior-ess-mode-hook
          (lambda ()
            (ansi-color-for-comint-mode-on)))

(ergoemacs-mode 1)

(use-package quarto-mode
  :ensure t
  :mode (("\\.qmd" . poly-quarto-mode)))

(use-package w3m
  :ensure t)


(use-package shell-maker)
(use-package dall-e-shell
  :after (shell-maker)
  :config
  (require 'dall-e-shell))

(when nvs
  (load "~/emacs-config/clearcase"))


;; Always solarized-dark, regardless of where this frame was opened from.
(if (daemonp)
    (add-hook 'after-make-frame-functions
              (defun my/theme-init-daemon (frame)
                (with-selected-frame frame
                  (load-theme 'solarized-dark t))
                ;; Run this hook only once.
                (remove-hook 'after-make-frame-functions
                             #'my/theme-init-daemon)
                (fmakunbound 'my/theme-init-daemon)))
  (load-theme 'solarized-dark t))

(defun my/send-apps-key ()
  "Send apps key"
  (interactive)
  (if (eq system-type 'windows-nt)
      (push 'apps unread-command-events)
    (push 'menu unread-command-events)))

(define-key ergoemacs-user-keymap (kbd "M-=") 'my/send-apps-key)


(unless nvs
  (use-package copilot-chat
    :quelpa (copilot-chat
             :fetcher github
             :repo "chep/copilot-chat.el"
             :branch "master"
             :files ("*.el"))
    :after (request shell-maker)
    ;; :custom
    ;; (copilot-chat-frontend 'shell-maker)
    :config
    ;; (require 'copilot-chat-shell-maker)
    ;; (setq copilot-chat-shell-maker-use-polymode t)
    (if (file-exists-p "c:/Windows/System32/curl.exe")
        (setq copilot-chat-curl-program "c:/Windows/System32/curl.exe")
      (setq copilot-chat-backend 'request))
    ;; (push '(shell-maker . copilot-chat-shell-maker-init) copilot-chat-frontend-list)
    ;; (copilot-chat-shell-maker-init)
    (define-key ergoemacs-user-keymap (kbd "<menu> n") 'copilot-chat)
    (define-key ergoemacs-user-keymap (kbd "<apps> n") 'copilot-chat)
    ;; (require 'copilot-chat)
    ;; (require 'copilot-chat-org)

    ;; Hacks to ask my own questions
    (defun copilot-chat-roxygen2()
      "Ask Copilot to fix the current selected code."
      (interactive)
      (copilot-chat--ask-region 'roxygen2))

    (setq copilot-chat-prompt-roxygen2
          "Would you please describe the following code using roxygen2 and use @author Matthew L. Fidler; if the function starts with a '.' do not export and use @noRd but still document each parameter and the title/description of the functions, otherwise use @export\n")

    (defun copilot-chat--prompts ()
      "Return assoc list of promts for each command."
      `((explain . ,copilot-chat-prompt-explain)
        (review . ,copilot-chat-prompt-review)
        (doc . ,copilot-chat-prompt-doc)
        (fix . ,copilot-chat-prompt-fix)
        (optimize . ,copilot-chat-prompt-optimize)
        (test . ,copilot-chat-prompt-test)
        (roxygen2 . ,copilot-chat-prompt-roxygen2)))

    (transient-define-prefix copilot-chat ()
      "Copilot Chat"
      ["Copilot Chat Actions"
       ("c" "Display/Open" copilot-chat-display)
       ("e" "Explain" copilot-chat-explain)
       ("r" "Review" copilot-chat-review)
       ("d" "Doc" copilot-chat-doc)
       ("f" "Fix" copilot-chat-fix)
       ("o" "Optimize" copilot-chat-optimize)
       ("x" "roxygen describe" copilot-chat-roxygen2)
       ("t" "Test" copilot-chat-test)]))

  (use-package copilot
    :quelpa (copilot :fetcher github
                     :repo "copilot-emacs/copilot.el"
                     :branch "main"
                     :files ("*.el"))
    :config
    (unless (file-exists-p "~/.emacs.d/.cache/copilot")
      (copilot-install-server))
    (when (file-exists-p "/usr/local/bin/node")
      (setq copilot-node-executable "/usr/local/bin/node"))
    (add-hook 'prog-mode-hook 'copilot-mode)
    (define-key copilot-mode-map (kbd "M-[")
                'copilot-next-completion)
    (define-key copilot-mode-map (kbd "M-]")
                'copilot-previous-completion)
    (define-key copilot-mode-map (kbd "C-<right>")
                'copilot-accept-completion-by-word)
    (define-key copilot-mode-map (kbd "C-<down>")
                'copilot-accept-completion-by-line)
    (define-key copilot-mode-map (kbd "C-<left>") #'copilot-complete)
    (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
    (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)))

(add-to-list 'package-archives
             '("nongnu" . "https://elpa.nongnu.org/nongnu/"))


(use-package eat
  :ensure t)

;; Pasting, undoing and killing words inside an eat buffer used to need advice
;; here; `ergoemacs-term.el' in ergoemacs-mode now handles every terminal
;; emulator, so there is nothing left to do but load eat.

;;; Copying out of a terminal.
;;
;; A program copies by sending OSC 52, naming the selection it means.  tmux
;; names none -- it sends "\e]52;;<data>" -- and eat reads a missing name as
;; xterm's "s0", the select target, which it puts in the kill ring and nowhere
;; else.  So a copy inside claude could not be pasted into anything but Emacs.
;; Read an unnamed selection as the clipboard, which is what every program
;; that sends one means by it.

(defun eat-osc52--clipboard-frame ()
  "Return a frame whose display can own the clipboard, or nil for none.

`kill-new' hands the text to the window system of whichever frame is
selected when it runs, and the frame selected when a copy arrives is
whichever one Emacs happened to be in -- in a daemon that is the initial
text frame, which owns no clipboard at all.  Prefer the selected frame,
then the one showing this terminal, then any graphical frame."
  (or (and (display-graphic-p) (selected-frame))
      (let ((window (get-buffer-window nil t)))
        (and window
             (display-graphic-p (window-frame window))
             (window-frame window)))
      (seq-find #'display-graphic-p (frame-list))))

(defun eat-osc52-select-means-clipboard (fn terminal selection data)
  "Around FN, call `eat--manipulate-kill-ring' with the clipboard.
SELECTION is remapped to `:clipboard' when it is the unnamed `:select'
target; TERMINAL and DATA are passed through untouched.  The call is made
from `eat-osc52--clipboard-frame', so the text reaches the X clipboard
and not just the kill ring, and it says how much it copied, so a copy
that never arrives is told apart from one that arrives and goes nowhere."
  (let* ((selection (if (eq selection :select) :clipboard selection))
         (frame (and (eq selection :clipboard) (stringp data)
                     (eat-osc52--clipboard-frame))))
    (if (and frame (not (eq frame (selected-frame))))
        (with-selected-frame frame (funcall fn terminal selection data))
      (funcall fn terminal selection data))
    (when (and (stringp data) (eq selection :clipboard))
      (message "Copied %d characters out of the terminal%s" (length data)
               (if frame "" " (kill ring only: no graphical frame)")))))

(with-eval-after-load 'eat
  (advice-add 'eat--manipulate-kill-ring :around
              #'eat-osc52-select-means-clipboard))

;;; Zooming a terminal, and keeping it wide enough to read code in.
;;
;; C-<wheel-up> and C-<wheel-down> run `mouse-wheel-text-scale' everywhere else,
;; but two things stop them in a terminal.  A program that asks for mouse
;; reporting -- claude does -- makes eat turn on `eat--mouse-modifier-click-mode',
;; whose keymap grabs every modified mouse event and forwards it to the program,
;; and being a minor mode keymap it shadows both the global binding and
;; `eat-mode-map'.  Then, even once the text does scale, nothing tells the
;; program about it: Emacs only resizes a process's pty from
;; `window-configuration-change-hook', and a change of text size is not a
;; configuration change, so claude would keep drawing at the old row and column
;; count.  Fix both, and then use them to fix a third annoyance -- code
;; snippets, diffs and tables arriving wrapped because the terminal is too
;; narrow -- by shrinking the text on its own until enough columns fit.

(require 'face-remap)

(defcustom my-eat-min-columns 100
  "Columns `my-eat-fit-columns-mode' tries to keep available in a terminal.
Claude wraps everything it prints to the width of its terminal, so a
window narrower than this mangles code snippets and diffs."
  :type 'natnum
  :group 'eat)

(defcustom my-eat-min-text-scale -4
  "How far `my-eat-fit-columns-mode' may shrink the text.
It gives up here even when `my-eat-min-columns' still does not fit,
rather than scaling down to something unreadable."
  :type 'integer
  :group 'eat)

(defvar-local my-eat-text-scale-preferred 0
  "Text scale this terminal uses when its window is wide enough.
Zooming by hand sets it, and `my-eat-fit-columns-mode' never zooms past
it: the mode only shrinks the text below this, and only for as long as
the window is too narrow.")

(defvar my-eat--inhibit-resize nil
  "Non-nil while trying text scales out, so the terminal is resized once.")

(defvar my-eat--fitting nil
  "Non-nil while `my-eat-fit-columns' runs, to keep it out of its own hooks.")

(defun my-eat-sync-terminal-size ()
  "Tell the program in this terminal how big its window is now."
  (when (and (derived-mode-p 'eat-mode)
             (bound-and-true-p eat-terminal)
             (not my-eat--inhibit-resize))
    (window--adjust-process-windows)))

(defun my-eat--text-scale-step (step)
  "Change the text scale by STEP.
Return nil instead of signalling when the font cannot go that small or
that large, so a loop stepping through sizes just stops there."
  (condition-case nil
      (progn (text-scale-increase step) t)
    (user-error nil)))

(defun my-eat-fit-columns (&optional window)
  "Shrink the text in WINDOW until `my-eat-min-columns' columns fit.
Grow it back, up to `my-eat-text-scale-preferred', once the window is
wide enough to afford it."
  (interactive)
  (let ((window (or window (selected-window))))
    ;; A text terminal has one font in one size; there is nothing to trade.
    (when (display-graphic-p (window-frame window))
      (let ((my-eat--inhibit-resize t))
        (with-selected-window window
          ;; Never end up more zoomed in than asked for.
          (when (> text-scale-mode-amount my-eat-text-scale-preferred)
            (text-scale-set my-eat-text-scale-preferred))
          (while (and (< (window-max-chars-per-line window) my-eat-min-columns)
                      (> text-scale-mode-amount my-eat-min-text-scale)
                      (my-eat--text-scale-step -1)))
          ;; Take back each step that still leaves room for the target width.
          (while (and (< text-scale-mode-amount my-eat-text-scale-preferred)
                      (let ((amount text-scale-mode-amount))
                        (and (my-eat--text-scale-step 1)
                             (or (>= (window-max-chars-per-line window)
                                     my-eat-min-columns)
                                 (progn (text-scale-set amount) nil))))))))
      (with-selected-window window
        (my-eat-sync-terminal-size)))))

(defun my-eat-fit-columns--window-change ()
  "Refit this terminal after the window showing it changed."
  (unless my-eat--fitting
    (when-let* ((window (if (eq (window-buffer) (current-buffer))
                            (selected-window)
                          (get-buffer-window nil t))))
      (let ((my-eat--fitting t))
        (my-eat-fit-columns window)))))

(define-minor-mode my-eat-fit-columns-mode
  "Keep at least `my-eat-min-columns' columns available in this terminal.
While the window is too narrow the text shrinks until that many columns
fit, so a code snippet claude prints arrives unwrapped; when the window
grows again the text grows back to `my-eat-text-scale-preferred'."
  :lighter " fit"
  (if my-eat-fit-columns-mode
      (progn
        (add-hook 'window-configuration-change-hook
                  #'my-eat-fit-columns--window-change nil t)
        (my-eat-fit-columns--window-change))
    (remove-hook 'window-configuration-change-hook
                 #'my-eat-fit-columns--window-change t)
    (text-scale-set my-eat-text-scale-preferred)
    (my-eat-sync-terminal-size)))

(defun my-eat--text-scale (step event)
  "Set the text scale of the terminal EVENT happened in.
STEP is added to the current scale, or nil to go back to the default
size.  The scale reached this way becomes the one
`my-eat-fit-columns-mode' returns to."
  (let ((window (or (and (consp event)
                         (let ((w (posn-window (event-start event))))
                           (and (window-live-p w) w)))
                    (selected-window))))
    (with-selected-window window
      (text-scale-set (if step (+ text-scale-mode-amount step) 0))
      (setq my-eat-text-scale-preferred text-scale-mode-amount)
      (let ((gave-up (and my-eat-fit-columns-mode
                          (< (window-max-chars-per-line window)
                             my-eat-min-columns))))
        (cond
         ;; Text this big leaves fewer columns than the mode insists on, and
         ;; the request was explicit, so stop fitting rather than undo it.
         (gave-up (my-eat-fit-columns-mode -1))
         (my-eat-fit-columns-mode (my-eat-fit-columns window))
         (t (my-eat-sync-terminal-size)))
        (message "Text scale %+d, %d columns%s"
                 text-scale-mode-amount (window-max-chars-per-line window)
                 (if gave-up ", fitting off (C-c f)" ""))))))

(defun my-eat-text-scale-increase (&optional event)
  "Make the text in this terminal bigger and resize the terminal to match.
With a mouse EVENT, act on the terminal under the pointer."
  (interactive (list last-input-event))
  (my-eat--text-scale 1 event))

(defun my-eat-text-scale-decrease (&optional event)
  "Make the text in this terminal smaller and resize the terminal to match.
With a mouse EVENT, act on the terminal under the pointer."
  (interactive (list last-input-event))
  (my-eat--text-scale -1 event))

(defun my-eat-text-scale-reset (&optional event)
  "Undo any zooming in this terminal.
With a mouse EVENT, act on the terminal under the pointer."
  (interactive (list last-input-event))
  (my-eat--text-scale nil event))

(defun my-eat--setup-text-scale ()
  "Resize this terminal whenever its text is scaled by any means."
  (add-hook 'text-scale-mode-hook #'my-eat-sync-terminal-size nil t))

(with-eval-after-load 'eat
  (add-hook 'eat-mode-hook #'my-eat--setup-text-scale)
  ;; The mouse map has to be bound too, not just `eat-mode-map': it is the one
  ;; that steals C-<wheel-...> while the program is reading the mouse.
  (dolist (map (list eat-mode-map eat--mouse-modifier-click-mode-map))
    (define-key map [C-wheel-up] #'my-eat-text-scale-increase)
    (define-key map [C-wheel-down] #'my-eat-text-scale-decrease)
    ;; On a text terminal (`xterm-mouse-mode') the wheel arrives as buttons 4
    ;; and 5 instead.
    (define-key map [C-mouse-4] #'my-eat-text-scale-increase)
    (define-key map [C-mouse-5] #'my-eat-text-scale-decrease))
  ;; C-+ and friends belong to the program in semi-char mode, but C-c is eat's
  ;; own prefix and falls through to `eat-mode-map' for keys eat does not use.
  (define-key eat-mode-map [?\C-c ?+] #'my-eat-text-scale-increase)
  (define-key eat-mode-map [?\C-c ?=] #'my-eat-text-scale-increase)
  (define-key eat-mode-map [?\C-c ?-] #'my-eat-text-scale-decrease)
  (define-key eat-mode-map [?\C-c ?0] #'my-eat-text-scale-reset)
  (define-key eat-mode-map [?\C-c ?f] #'my-eat-fit-columns-mode))

(use-package monet
  :vc (:url "https://github.com/stevemolitor/monet" :rev :newest))

;; systemd starts `emacs --daemon' with a minimal PATH, so the directories the
;; agents live in are invisible to `executable-find' and to every subprocess
;; Emacs starts.  Two of them are missing: ~/.local/bin (claude, claude-tmux,
;; gh, ...), which systemd never had; and ~/.opencode/bin, where opencode's
;; installer drops its binary and which it adds to PATH in ~/.bashrc -- a file
;; only an interactive shell reads, so opencode runs from a terminal and is
;; nowhere to be found from the daemon.  Put both back, for this Emacs and its
;; children.
(dolist (bin (list (expand-file-name "~/.local/bin")
                   (expand-file-name "~/.opencode/bin")))
  (when (file-directory-p bin)
    (add-to-list 'exec-path bin)
    (let ((path (or (getenv "PATH") "")))
      (unless (member bin (split-string path path-separator t))
        (setenv "PATH" (concat bin path-separator path))))))

;;; Coding agents in a terminal.
;;
;; Every agent here runs the same way: an eat buffer showing a client attached to
;; a detachable tmux session of the agent's own (~/.local/bin/ai-tmux, installed
;; once per agent as claude-tmux, antigravity-tmux, copilot-tmux, opencode-tmux,
;; kilo-tmux), so a conversation outlives both a dropped ssh connection and an
;; Emacs restart, and starting the agent again in the same directory re-attaches
;; to it.
;;
;; claude-code.el is the Emacs half for Claude; the much smaller half the other
;; four need is below.  Everything that is not particular to one agent -- which
;; directory a buffer belongs to, the theme to start in, listing and switching to
;; and ending background sessions, cutting a worktree to start one in -- is
;; shared, and each agent's commands are a few lines on top of it.

(defun ai-term--directory ()
  "Root directory an agent started from this buffer should work in.

The project root when there is one, else the visited file's directory,
else `default-directory'.  This repeats the choice `claude-code--directory'
makes, so Antigravity agrees with Claude about which directory a buffer
belongs to without having to load claude-code.el to ask."
  (let ((project (project-current)))
    (cond
     (project (project-root project))
     ((buffer-file-name) (file-name-directory (buffer-file-name)))
     (t default-directory))))

(defun ai-term--theme ()
  "Whether this Emacs is a light or a dark one, as an agent theme name."
  (if (eq (frame-parameter nil 'background-mode) 'dark) "dark" "light"))

;;;; A terminal for an agent that has no Emacs package
;;
;; claude-code.el is all of this for Claude.  Antigravity, Copilot, opencode and
;; kilo have no package, and want the same few things: name a buffer after the
;; directory the agent works in, find the ones already running, and start another
;; on that agent's <agent>-tmux.  So here they are once, taking the agent as an
;; argument, and each agent's own commands are a line apiece on top.

(defun ai-term--buffer-name (agent directory &optional instance)
  "Name of the AGENT buffer working in DIRECTORY.
INSTANCE distinguishes a second agent started in the same directory."
  ;; Always a directory name, trailing slash and all, so a buffer is found
  ;; again whether the caller had one or not -- and so the names read the same
  ;; as claude-code.el's.
  (let ((dir (abbreviate-file-name
              (file-name-as-directory (file-truename directory)))))
    (if instance
        (format "*%s:%s:%s*" agent dir instance)
      (format "*%s:%s*" agent dir))))

(defun ai-term--buffers-for-directory (agent directory)
  "Live AGENT terminals working in DIRECTORY."
  (let ((regexp (concat "\\`"
                        (regexp-quote
                         (string-trim-right
                          (ai-term--buffer-name agent directory) "\\*"))
                        "\\(?::[^*]+\\)?\\*\\'")))
    (seq-filter (lambda (buffer)
                  (and (string-match-p regexp (buffer-name buffer))
                       (get-buffer-process buffer)))
                (buffer-list))))

(defun ai-term--all-buffers (agent)
  "Every live AGENT terminal in this Emacs."
  (let ((prefix (format "*%s:" agent)))
    (seq-filter (lambda (buffer)
                  (and (string-prefix-p prefix (buffer-name buffer))
                       (get-buffer-process buffer)))
                (buffer-list))))

(defun ai-term--unused-buffer-name (agent directory)
  "An AGENT buffer name for DIRECTORY that no buffer has taken."
  (if (not (get-buffer (ai-term--buffer-name agent directory)))
      (ai-term--buffer-name agent directory)
    (let ((n 2))
      (while (get-buffer (ai-term--buffer-name agent directory
                                               (number-to-string n)))
        (setq n (1+ n)))
      (ai-term--buffer-name agent directory (number-to-string n)))))

(defun ai-term--read-buffer (prompt buffers)
  "Read one of BUFFERS with PROMPT, or return it when there is only one."
  (if (cdr buffers)
      (get-buffer (completing-read prompt (mapcar #'buffer-name buffers) nil t))
    (car buffers)))

(defun ai-term--start (agent program directory &optional session switches)
  "Open a terminal on AGENT, running PROGRAM in DIRECTORY, and return its buffer.

PROGRAM is normally the agent's <agent>-tmux, so the conversation lives
on a detachable tmux session rather than in the buffer.

SESSION names a background tmux session to re-attach to.  Without one,
<agent>-tmux derives the session from DIRECTORY, so a session already
running there is re-entered instead of duplicated.

SWITCHES are extra command line arguments for the agent itself, such as
\"--resume\".  Giving any, and no SESSION, makes <agent>-tmux start a
session of its own rather than re-enter the one running in DIRECTORY,
since switches only mean anything to an agent that is starting.  A
SESSION that already exists is attached to whatever the switches say, so
they are dropped: ask for one or the other, not both."
  (require 'eat)
  (unless (executable-find program)
    (user-error "%s program `%s' not found in PATH" (capitalize agent) program))
  (let* ((directory (file-name-as-directory (expand-file-name directory)))
         (default-directory directory)
         (name (ai-term--unused-buffer-name agent directory))
         ;; CLAUDE_TMUX_*, ANTIGRAVITY_TMUX_*, COPILOT_TMUX_*, OPENCODE_TMUX_*,
         ;; KILO_TMUX_*: the same name ai-tmux derives from the agent it was
         ;; called as.
         (prefix (upcase (replace-regexp-in-string "-" "_" agent)))
         ;; Without this the terminal flickers while the agent redraws.
         (process-adaptive-read-buffering nil)
         (process-environment
          (append (list (format "%s_TMUX_THEME=%s" prefix (ai-term--theme)))
                  (and session
                       (list (format "%s_TMUX_SESSION=%s" prefix session)))
                  process-environment))
         (buffer (apply #'eat-make (string-trim name "\\*" "\\*")
                        program nil switches)))
    (with-current-buffer buffer
      ;; Nothing in here is a shell, and a scroll back through the conversation
      ;; should not run off the end of what eat kept.
      (setq-local eat-enable-directory-tracking nil)
      (setq-local eat-enable-shell-prompt-annotation nil)
      (setq-local eat-term-scrollback-size nil)
      ;; The agents wrap code snippets to the width of their terminal, so
      ;; shrink the text rather than let a narrow window wrap them, exactly as
      ;; the claude buffers do.
      (my-eat-fit-columns-mode 1))
    buffer))

;;;; The conversations an agent remembers
;;
;; tmux finds a conversation that is still running; this finds one that is not.
;; Three of the agents need the same picker for it -- Antigravity, which will not
;; list its conversations at all, and opencode and its fork kilo, which list them
;; but leave joining the list to the re-opening switch as an exercise -- so it is
;; here once, over a conversation spelled (ID DIRECTORY TIME TEXT): TIME is when
;; it was last spoken to, in milliseconds, and TEXT is whatever the agent has by
;; way of a description of it.

(defun ai-term--conversation-id (conversation)
  "The leading eight characters of CONVERSATION's id, which is enough to tell
it from another in the list."
  (let ((id (or (nth 0 conversation) "")))
    (substring id 0 (min 8 (length id)))))

(defun ai-term--conversation-label (conversation)
  "What the picker calls CONVERSATION: the first line of its description.

Falls back to the head of its id, for a conversation that has no
description worth reading -- one that opened with a slash command, say."
  (let* ((text (or (nth 3 conversation) ""))
         (line (string-trim (or (car (split-string text "\n" t)) ""))))
    (if (string-empty-p line)
        (ai-term--conversation-id conversation)
      (truncate-string-to-width line 72 nil nil t))))

(defun ai-term--read-conversation (prompt conversations &optional show-directory)
  "Read one of CONVERSATIONS with PROMPT, and return it.

CONVERSATIONS is most recently used first, and is offered in that order.
Each is annotated with when it was last spoken to, and with SHOW-DIRECTORY
where it was -- which is what makes a list spanning directories worth
reading."
  (let* ((candidates
          (let (alist)
            (dolist (conversation conversations (nreverse alist))
              (let ((label (ai-term--conversation-label conversation)))
                ;; Two conversations can open with the same question.
                (when (assoc label alist)
                  (setq label (format "%s  [%s]" label
                                      (ai-term--conversation-id conversation))))
                (push (cons label conversation) alist)))))
         (width (apply #'max 0 (mapcar (lambda (c) (string-width (car c)))
                                       candidates)))
         (table
          (lambda (string pred action)
            (if (eq action 'metadata)
                `(metadata
                  (category . ai-conversation)
                  ;; keep the caller's most-recent-first order
                  (display-sort-function . identity)
                  (cycle-sort-function . identity)
                  (annotation-function
                   . ,(lambda (candidate)
                        (when-let* ((conversation (cdr (assoc candidate candidates))))
                          (concat (make-string
                                   (1+ (- width (string-width candidate))) ?\s)
                                  (propertize
                                   (concat (format-time-string
                                            "%Y-%m-%d %H:%M"
                                            (/ (nth 2 conversation) 1000))
                                           (and show-directory
                                                (concat "  " (abbreviate-file-name
                                                              (nth 1 conversation)))))
                                   'face 'completions-annotations))))))
              (complete-with-action action candidates string pred)))))
    ;; CONVERSATIONS is most recent first, and `completing-read' hands back the
    ;; empty string for an empty minibuffer whatever REQUIRE-MATCH says, so make
    ;; the newest the default rather than let a bare RET pick nothing.
    (let ((default (car (car candidates))))
      (or (cdr (assoc (completing-read prompt table nil t nil nil default)
                      candidates))
          (user-error "No conversation chosen")))))

(defun ai-term--cli-conversations (directory program &rest args)
  "Conversations PROGRAM lists when run with ARGS in DIRECTORY, newest first.

For the agents descended from opencode -- opencode itself and kilo, its
fork -- whose `session list --format json' prints a JSON array of
objects with id, title, directory and an `updated' in milliseconds,
most recently spoken to first.  Returned as (ID DIRECTORY TIME TEXT), the
spelling `ai-term--read-conversation' wants.

A CLI that is not installed, or too old to know the switches, prints
nothing that parses and is taken to remember nothing.  So does one with
no sessions to print, which is the same answer by a different route and
wants the same fallback."
  (let ((default-directory (file-name-as-directory directory)))
    (with-temp-buffer
      (when (and (executable-find program)
                 ;; stdout here, stderr nowhere: a log line or a login warning
                 ;; printed over the JSON would only make it unparseable.
                 (eq 0 (apply #'call-process program nil '(t nil) nil args)))
        (goto-char (point-min))
        (let ((sessions (ignore-errors
                          ;; No sessions at all prints nothing, which is not
                          ;; JSON; that reads as the empty list it means.
                          (json-parse-buffer :object-type 'alist
                                             :array-type 'list
                                             :null-object nil
                                             :false-object nil))))
          (seq-filter
           (lambda (conversation)
             (and (stringp (nth 0 conversation))
                  (stringp (nth 1 conversation))
                  (numberp (nth 2 conversation))))
           (mapcar (lambda (session)
                     (list (alist-get 'id session)
                           (alist-get 'directory session)
                           (alist-get 'updated session)
                           (alist-get 'title session)))
                   ;; Sessions are a JSON array of objects, so each element is
                   ;; an alist and its car a cons.  Anything else printed with
                   ;; a zero status -- an error envelope, `{"error": ...}',
                   ;; some later version wrapping the array in an object -- is
                   ;; a list as well, and would walk off the end of `alist-get'
                   ;; rather than fall back.  Keep only what is shaped right.
                   (seq-filter (lambda (session)
                                 (and (consp session) (consp (car session))))
                               (and (listp sessions) sessions)))))))))

(defun ai-term--live-conversations (conversations)
  "The CONVERSATIONS there is still somewhere to start an agent in.

A conversation whose directory has since been deleted -- half of them, on
a machine that cuts a worktree per pull request -- cannot be walked back
into, so offering it would only fail."
  (seq-filter (lambda (conversation)
                (let ((dir (nth 1 conversation)))
                  (and (stringp dir) (file-directory-p dir))))
              conversations))

(defun ai-term--conversations-under (conversations directory)
  "The CONVERSATIONS held in DIRECTORY, or in a subdirectory of it.

An agent asked from a subdirectory records that subdirectory, and the
conversation still belongs to the project."
  (let ((this (file-name-as-directory (file-truename directory))))
    (seq-filter (lambda (conversation)
                  (string-prefix-p this (file-name-as-directory
                                         (file-truename (nth 1 conversation)))))
                conversations)))

(defvar ai-tmux-agents
  '(("claude" . claude-tmux--attach)
    ("antigravity" . antigravity-tmux--attach)
    ("copilot" . copilot-cli-tmux--attach)
    ("opencode" . opencode-tmux--attach)
    ("kilo" . kilo-tmux--attach))
  "The agents that keep their sessions on a tmux server of their own.

Each element is (SOCKET . ATTACH), where SOCKET names the tmux server --
`tmux -L claude', `tmux -L antigravity' -- and ATTACH is the function
`ai-tmux--attach' calls with that agent's (NAME DIRECTORY ATTACHED) to
show the session in this Emacs.")

(defun ai-tmux--server-sessions (socket)
  "Return the sessions on tmux server SOCKET, in no particular order.

Each element is (ACTIVITY NAME DIRECTORY ATTACHED): ACTIVITY is the time
the session was last used, and ATTACHED says whether some client -- an
Emacs buffer or a terminal -- is currently viewing it."
  (mapcar (lambda (line)
            (pcase-let ((`(,name ,dir ,attached ,activity) (split-string line "\t")))
              (list (string-to-number (or activity "0"))
                    name dir (not (equal attached "0")))))
          (split-string
           (shell-command-to-string
            (concat "tmux -L " (shell-quote-argument socket) " ls -F "
                    "'#{session_name}\t#{session_path}\t"
                    "#{session_attached}\t#{session_activity}' 2>/dev/null"))
           "\n" t)))

(defun ai-tmux--sessions (socket)
  "Return the background agent sessions on SOCKET, most recently used first.

SOCKET is the tmux server an agent keeps its sessions on: \"claude\" for
Claude, \"antigravity\" for Antigravity.  Each element is (NAME DIRECTORY
ATTACHED)."
  (mapcar #'cdr (sort (ai-tmux--server-sessions socket)
                      (lambda (a b) (> (car a) (car b))))))

(defun ai-tmux--all-sessions ()
  "Return every agent's background sessions, most recently used first.

Each element is (AGENT NAME DIRECTORY ATTACHED), with AGENT the tmux
server the session lives on -- one list across every agent in
`ai-tmux-agents', so a session can be picked without first having to
remember which agent left it."
  (mapcar #'cdr
          (sort (mapcan (lambda (agent)
                          (mapcar (lambda (session)
                                    (cons (car session)
                                          (cons (car agent) (cdr session))))
                                  (ai-tmux--server-sessions (car agent))))
                        ai-tmux-agents)
                (lambda (a b) (> (car a) (car b))))))

(defun ai-tmux--read-session (socket prompt)
  "Read a background agent session on SOCKET with PROMPT.
Returns the (NAME DIRECTORY ATTACHED) entry, annotated with the
directory the session was started in."
  (let* ((sessions (or (ai-tmux--sessions socket)
                       (user-error "No background %s sessions" socket)))
         (width (apply #'max (mapcar (lambda (s) (length (car s))) sessions)))
         (table
          (lambda (string pred action)
            (if (eq action 'metadata)
                `(metadata
                  (category . ai-tmux-session)
                  ;; keep `ai-tmux--sessions' most-recent-first order
                  (display-sort-function . identity)
                  (cycle-sort-function . identity)
                  (annotation-function
                   . ,(lambda (cand)
                        (let ((session (assoc cand sessions)))
                          (concat (make-string (1+ (- width (length cand))) ?\s)
                                  (propertize (or (nth 1 session) "")
                                              'face 'completions-annotations)
                                  (and (nth 2 session) "  [attached]"))))))
              (complete-with-action action sessions string pred)))))
    (assoc (completing-read prompt table nil t) sessions)))

(defun ai-tmux--kill (socket session)
  "End the background agent SESSION on SOCKET.

Killing an agent's buffer only detaches from tmux -- the agent keeps
running so it can be re-attached.  This actually stops it."
  (let ((name (if (consp session) (car session) session)))
    (call-process "tmux" nil nil nil "-L" socket "kill-session" "-t" name)
    (message "Ended %s session %s" socket name)))

(defun ai-tmux--attach (session)
  "Show the agent SESSION, (AGENT NAME DIRECTORY ATTACHED), in this Emacs.

Hands it to the attach function `ai-tmux-agents' names for that agent,
which starts a client for the session when this Emacs has none."
  (pcase-let* ((`(,agent ,name ,dir ,attached) session)
               (attach (cdr (assoc agent ai-tmux-agents))))
    (unless attach (user-error "No way to attach to a %s session" agent))
    (funcall attach (list name dir attached))))

(defun ai-tmux--session-for-directory (socket directory)
  "The session on SOCKET that was started in DIRECTORY, if there is one.

This is how a conversation is found again after the Emacs showing it has
gone: the buffer is not there any more, but the session is."
  (let ((dir (file-truename (file-name-as-directory directory))))
    (seq-find (lambda (session)
                (let ((sdir (nth 1 session)))
                  (and sdir (file-directory-p sdir)
                       (equal (file-truename (file-name-as-directory sdir)) dir))))
              (ai-tmux--sessions socket))))

;;;; The list of sessions
;;
;; What ibuffer is to buffers: every conversation on the machine in one
;; tabulated list, whichever agent left it and whichever Emacs -- or ssh
;; connection, or plain terminal -- it was started from.

(defvar ai-tmux-list-buffer-name "*AI sessions*"
  "Name of the buffer `ai-tmux-list' shows the sessions in.")

(defvar ai-tmux-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'ai-tmux-list-attach)
    (define-key map "o" #'ai-tmux-list-attach)
    (define-key map "d" #'ai-tmux-list-mark-kill)
    (define-key map "u" #'ai-tmux-list-unmark)
    (define-key map "U" #'ai-tmux-list-unmark-all)
    (define-key map "x" #'ai-tmux-list-execute)
    (define-key map "k" #'ai-tmux-list-kill)
    map)
  "Keymap for `ai-tmux-list-mode'.

`g' and `q' come from `special-mode': the list is re-read from tmux
rather than cached, so reverting it shows what is running now.")

(defun ai-tmux-list--entries ()
  "Rows for `tabulated-list-entries', most recently used session first."
  (mapcar (lambda (session)
            (pcase-let ((`(,agent ,name ,dir ,attached) session))
              (list session
                    (vector agent
                            name
                            (if attached "attached" "")
                            (if dir (abbreviate-file-name dir) "")))))
          (ai-tmux--all-sessions)))

(defun ai-tmux-list--refresh ()
  "Re-read the sessions from every agent's tmux server."
  (setq tabulated-list-entries (ai-tmux-list--entries)))

(define-derived-mode ai-tmux-list-mode tabulated-list-mode "AI sessions"
  "Major mode for the list of background agent sessions.

\\{ai-tmux-list-mode-map}"
  (setq tabulated-list-format
        [("Agent" 12 t) ("Session" 38 t) ("" 9 t) ("Directory" 0 t)])
  ;; Leave the sort key unset: `ai-tmux--all-sessions' hands them over most
  ;; recently used first, which is the order worth having by default.  The
  ;; column headers still sort when clicked.
  (setq tabulated-list-padding 2)
  (add-hook 'tabulated-list-revert-hook #'ai-tmux-list--refresh nil t)
  (tabulated-list-init-header))

(defun ai-tmux-list--session ()
  "The session on this line, or an error when there is none."
  (or (tabulated-list-get-id) (user-error "No session on this line")))

(defun ai-tmux-list-attach ()
  "Show the session on this line in this Emacs."
  (interactive)
  (ai-tmux--attach (ai-tmux-list--session)))

(defun ai-tmux-list-mark-kill ()
  "Mark the session on this line to be ended by \\[ai-tmux-list-execute]."
  (interactive)
  (ai-tmux-list--session)
  (tabulated-list-put-tag "D" t))

(defun ai-tmux-list-unmark ()
  "Remove the mark from the session on this line."
  (interactive)
  (tabulated-list-put-tag " " t))

(defun ai-tmux-list-unmark-all ()
  "Remove every mark in the list."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (tabulated-list-put-tag " " t))))

(defun ai-tmux-list--marked ()
  "Every session marked for killing, in the order they appear."
  (let (marked)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when (eq (char-after) ?D)
          (when-let* ((session (tabulated-list-get-id)))
            (push session marked)))
        (forward-line 1)))
    (nreverse marked)))

(defun ai-tmux-list-kill ()
  "End the session on this line.

Killing an agent's buffer only detaches from tmux; this stops the agent."
  (interactive)
  (pcase-let ((`(,agent ,name ,_dir ,_attached) (ai-tmux-list--session)))
    (when (yes-or-no-p (format "End %s session %s? " agent name))
      (ai-tmux--kill agent name)
      (revert-buffer))))

(defun ai-tmux-list-execute ()
  "End every session marked with \\[ai-tmux-list-mark-kill]."
  (interactive)
  (let ((marked (or (ai-tmux-list--marked) (user-error "Nothing marked"))))
    (when (yes-or-no-p (format "End %d marked session%s? " (length marked)
                               (if (cdr marked) "s" "")))
      (dolist (session marked)
        (ai-tmux--kill (nth 0 session) (nth 1 session)))
      (revert-buffer))))

;;;###autoload
(defun ai-tmux-list ()
  "List every agent's background tmux sessions, the way ibuffer lists buffers.

Every session on every agent's tmux server, most recently used first --
including ones started from another Emacs, another machine's ssh
connection, or a plain terminal -- with the directory it was started in
and whether something is already viewing it.

\\<ai-tmux-list-mode-map>\\[ai-tmux-list-attach] shows the one at point in
this Emacs, \\[ai-tmux-list-kill] ends it, \\[ai-tmux-list-mark-kill] and
\\[ai-tmux-list-execute] end several, and \\[revert-buffer] re-reads the
list from tmux."
  (interactive)
  (let ((buffer (get-buffer-create ai-tmux-list-buffer-name)))
    (with-current-buffer buffer
      (ai-tmux-list-mode)
      (ai-tmux-list--refresh)
      (tabulated-list-print))
    (pop-to-buffer buffer)))

(defun ai-tmux-switch (session)
  "Jump to any agent's background tmux SESSION, from the minibuffer.

`ai-tmux-list' is the same set of sessions as a buffer to look through;
this is the one-line version for when the name is already known.
Candidates are named agent/session, since two agents working in the same
directory derive the same session name."
  (interactive
   (list
    (let* ((sessions (or (ai-tmux--all-sessions)
                         (user-error "No background agent sessions")))
           (candidates (mapcar (lambda (session)
                                 (cons (format "%s/%s" (nth 0 session) (nth 1 session))
                                       session))
                               sessions))
           (width (apply #'max (mapcar (lambda (c) (length (car c))) candidates)))
           (table
            (lambda (string pred action)
              (if (eq action 'metadata)
                  `(metadata
                    (category . ai-tmux-session)
                    ;; keep `ai-tmux--all-sessions' most-recent-first order
                    (display-sort-function . identity)
                    (cycle-sort-function . identity)
                    (annotation-function
                     . ,(lambda (cand)
                          (let ((session (cdr (assoc cand candidates))))
                            (concat (make-string (1+ (- width (length cand))) ?\s)
                                    (propertize (or (nth 2 session) "")
                                                'face 'completions-annotations)
                                    (and (nth 3 session) "  [attached]"))))))
                (complete-with-action action candidates string pred))))
           (choice (completing-read "Agent session: " table nil t)))
      (cdr (assoc choice candidates)))))
  (ai-tmux--attach session))

(defun ai-wt--git (dir &rest args)
  "Run git with ARGS in DIR and return its output, trimmed.
Signal an error carrying git's own message when the command fails."
  (with-temp-buffer
    (let* ((default-directory (file-name-as-directory dir))
           (status (apply #'call-process "git" nil t nil args)))
      (unless (eq status 0)
        (user-error "git %s: %s" (string-join args " ")
                    (string-trim (buffer-string))))
      (string-trim (buffer-string)))))

(defun ai-wt--branch-p (dir branch)
  "Return non-nil when BRANCH already exists in the repository at DIR."
  (let ((default-directory (file-name-as-directory dir)))
    (eq 0 (call-process "git" nil nil nil "show-ref" "--verify" "--quiet"
                        (concat "refs/heads/" branch)))))

(defun ai-wt--trunk (dir)
  "The name of the trunk branch of the repository at DIR, or nil.

Whatever origin's HEAD points at -- main here, master elsewhere -- and
failing that whichever of the two the repository actually has, since
origin/HEAD is only set when the clone bothered to ask for it."
  (or (let ((ref (ignore-errors
                   (ai-wt--git dir "symbolic-ref" "--quiet" "--short"
                               "refs/remotes/origin/HEAD"))))
        (and ref (string-prefix-p "origin/" ref)
             (substring ref (length "origin/"))))
      (seq-find (lambda (branch) (ai-wt--branch-p dir branch))
                '("main" "master"))))

(defun ai-wt--on-trunk-p (dir)
  "Return non-nil when DIR is a checkout sitting on its repository's trunk.

That is: the branch new work is cut from, rather than a topic branch or a
worktree already cut for something."
  (let ((branch (ignore-errors
                  (ai-wt--git dir "symbolic-ref" "--quiet" "--short" "HEAD"))))
    (and branch (equal branch (ai-wt--trunk dir)))))

(defun ai-wt--worktree (name)
  "Return a git worktree of this repository called NAME, making it if needed.

Visiting ~/src/dir, NAME of \"feature-x\" puts the new branch feature-x
in ~/src/dir-feature-x, so an agent can work on its own checkout while
~/src/dir stays as you left it.

The branch is cut from the current HEAD.  An existing branch of that name
is checked out rather than recreated, and an existing worktree is simply
re-entered -- which, since the agents run under tmux, re-attaches to the
session already living there."
  (let* ((name (string-trim name))
         (_ (when (string-empty-p name) (user-error "No name given")))
         (root (directory-file-name
                (ai-wt--git default-directory "rev-parse" "--show-toplevel")))
         ;; feature/foo -> dir-foo, so the worktree stays a flat sibling.
         (leaf (replace-regexp-in-string
                "[^A-Za-z0-9._-]" "-" (file-name-nondirectory name)))
         (worktree (expand-file-name
                    (concat (file-name-nondirectory root) "-" leaf)
                    (file-name-directory root))))
    (cond
     ((file-directory-p worktree)
      (message "Re-using existing worktree %s" worktree))
     ((file-exists-p worktree)
      (user-error "%s exists and is not a directory" worktree))
     ((ai-wt--branch-p root name)
      (ai-wt--git root "worktree" "add" "--" worktree name))
     (t
      (ai-wt--git root "worktree" "add" "-b" name "--" worktree "HEAD")))
    (file-name-as-directory worktree)))

(defun ai-pr--root ()
  "The main worktree of this repository, not whichever one point is in.

Pull request worktrees are cut beside the repository, so they stay flat
siblings in ~/src even when this is asked for from inside another one."
  (file-name-as-directory
   (directory-file-name
    (file-name-directory
     (ai-wt--git default-directory "rev-parse" "--path-format=absolute"
                 "--git-common-dir")))))

(defun ai-pr--open ()
  "The repository's open pull requests, as (NUMBER AUTHOR TITLE), newest first."
  (let ((default-directory (ai-pr--root)))
    (mapcar (lambda (line) (split-string line "\t"))
            (split-string
             (shell-command-to-string
              (concat "gh pr list --limit 50 "
                      "--json number,author,title "
                      "--jq '.[] | \"\\(.number)\\t\\(.author.login)\\t\\(.title)\"' "
                      "2>/dev/null"))
             "\n" t))))

(defun ai-pr--read (prompt)
  "Read one of this repository's open pull requests with PROMPT.

Returns the number as a string.  Anything that is not a number is taken
as one anyway, so a pull request that `gh' did not list -- a closed one,
or one in a repository it cannot reach -- can still be typed in."
  (let* ((pulls (ai-pr--open))
         (width (apply #'max 1 (mapcar (lambda (p) (length (car p))) pulls)))
         (table
          (lambda (string pred action)
            (if (eq action 'metadata)
                `(metadata
                  (category . ai-pull-request)
                  ;; gh hands them over newest first; keep that.
                  (display-sort-function . identity)
                  (cycle-sort-function . identity)
                  (annotation-function
                   . ,(lambda (cand)
                        (let ((pull (assoc cand pulls)))
                          (concat (make-string (1+ (- width (length cand))) ?\s)
                                  (propertize (or (nth 2 pull) "")
                                              'face 'completions-annotations)
                                  (and (nth 1 pull)
                                       (concat "  " (nth 1 pull))))))))
              (complete-with-action action pulls string pred)))))
    (string-trim (completing-read prompt table nil nil))))

(defun ai-pr--worktree (pr)
  "Return a worktree of this repository holding pull request PR, making it if needed.

The branch is checked out by `gh', which knows how to reach a pull
request from a fork and how to set the branch up so that pushing it goes
back to the right place.  An existing worktree is re-entered -- which,
since the agents run under tmux, re-attaches to the conversation already
living there."
  ;; A number, a #number, or the URL of one.  Stripped in that order rather
  ;; than as one alternation: the leading-# branch matches the empty string,
  ;; and a zero-width match at position 0 leaves the first character behind.
  (let* ((pr (string-trim pr))
         (pr (replace-regexp-in-string "\\`.*/" "" pr))
         (pr (replace-regexp-in-string "\\`#+" "" pr))
         (_ (unless (string-match-p "\\`[0-9]+\\'" pr)
              (user-error "Not a pull request number: %s" pr)))
         (root (directory-file-name (ai-pr--root)))
         (worktree (expand-file-name
                    (format "%s-pr%s" (file-name-nondirectory root) pr)
                    (file-name-directory root))))
    (cond
     ((file-directory-p worktree)
      (message "Re-using existing worktree %s" worktree))
     ((file-exists-p worktree)
      (user-error "%s exists and is not a directory" worktree))
     (t
      (unless (executable-find "gh")
        (user-error "gh is not installed; it is what fetches a pull request"))
      (ai-wt--git root "worktree" "add" "--detach" "--" worktree "HEAD")
      (let ((default-directory (file-name-as-directory worktree)))
        (unless (eq 0 (call-process "gh" nil nil nil "pr" "checkout" pr))
          ;; Leave nothing behind when the checkout fails.
          (ai-wt--git root "worktree" "remove" "--force" "--" worktree)
          (user-error "Could not check out pull request %s" pr)))))
    (file-name-as-directory worktree)))

;;;; Claude

(defun claude-code-theme-environment (&rest _)
  "Tell `claude-tmux' whether this Emacs is a light or a dark one.
It turns CLAUDE_TMUX_THEME into claude's own --settings theme, so a
solarized-dark Emacs gets a dark claude and a solarized-light one a light
claude instead of whatever theme was picked last."
  (list (format "CLAUDE_TMUX_THEME=%s" (ai-term--theme))))

(use-package claude-code :ensure t
  :vc (:url "https://github.com/stevemolitor/claude-code.el" :rev :newest)
  :config
  ;; optional IDE integration with Monet
  (add-hook 'claude-code-process-environment-functions #'monet-start-server-function)
  (monet-mode 1)

  (add-hook 'claude-code-process-environment-functions
            #'claude-code-theme-environment)

  ;; Claude wraps code snippets to the width of its terminal, so shrink the text
  ;; rather than let a narrow window wrap them.  C-<wheel> still overrides this;
  ;; the size it is left at becomes the one claude buffers zoom back to.
  (add-hook 'claude-code-start-hook #'my-eat-fit-columns-mode)

  ;; Run claude inside its own detachable tmux session (~/.local/bin/claude-tmux)
  ;; so a conversation outlives both a dropped ssh connection and an Emacs
  ;; restart.  Starting claude again in the same directory re-attaches to it.
  (setq claude-code-program "claude-tmux")

  ;; Must be nil when claude runs under tmux: the default suppresses pty resize
  ;; events for height-only changes, and tmux clips its pane to the pty size, so
  ;; claude would stay stuck in a few rows at the top of a tall window.
  (setq claude-code-optimize-window-resize nil)

  (claude-code-mode)
  :bind-keymap ("C-c c" . claude-code-command-map)

  ;; Optionally define a repeat map so that "M" will cycle thru Claude auto-accept/plan/confirm modes after invoking claude-code-cycle-mode / C-c M.
  :bind
  (:repeat-map my-claude-code-map ("M" . claude-code-cycle-mode)))

;; `claude-code-program-switches' is claude-code.el's, and is let-bound below
;; before that package has necessarily been loaded.  Declaring it here keeps the
;; binding dynamic -- which is what makes it reach the process -- however this
;; file comes to be evaluated.
(defvar claude-code-program-switches)

(defun claude-code--start-in (directory &optional session switches)
  "Start Claude working in DIRECTORY, in a new buffer.

SESSION names a background tmux session to re-attach to.  Without one,
claude-tmux derives the session from DIRECTORY, so a session already
running there is re-entered instead of duplicated.

SWITCHES are extra command line arguments for claude itself, such as
\"--resume\".  Giving any, and no SESSION, makes claude-tmux start a
session of its own rather than re-enter the one running in DIRECTORY,
since switches only mean anything to an agent that is starting.  A
SESSION that already exists is attached to whatever the switches say, so
they are dropped: ask for one or the other, not both."
  (require 'claude-code)
  (let* ((start-dir (file-name-as-directory (expand-file-name directory)))
         (default-directory start-dir)
         (claude-code-program-switches
          (append claude-code-program-switches switches))
         (process-environment
          (append (and session (list (concat "CLAUDE_TMUX_SESSION=" session)))
                  process-environment)))
    (cl-letf (((symbol-function 'claude-code--directory) (lambda () start-dir)))
      (claude-code '(4)))))

(defun claude (&optional arg)
  "Attach to this project's Claude session, starting one if needed.

Re-uses the running Claude buffer for the current project when there is
one, so this is also the way back in after an Emacs restart or a dropped
ssh connection.  With prefix ARG, always start a new instance."
  (interactive "P")
  (require 'claude-code)
  (let* ((dir (claude-code--directory))
         (buffers (and (null arg) dir
                       (claude-code--find-claude-buffers-for-directory dir))))
    (cond
     ((null buffers) (claude-code '(4)))
     ((= 1 (length buffers)) (pop-to-buffer (car buffers)))
     (t (call-interactively #'claude-code-select-buffer)))))

(defun claude-resume ()
  "Start Claude on one of this project's past conversations.

`claude --resume': claude lists the conversations it has had in this
directory and re-opens the one you pick, which is the way back to a
conversation whose tmux session is gone -- after a reboot, or a
`claude-tmux-kill'.  Use `claude' for the one running right now.

The resumed conversation gets a tmux session of its own, so the one
already running here, if any, is left alone."
  (interactive)
  (require 'claude-code)
  (claude-code--start-in (claude-code--directory) nil '("--resume")))

(defun claude-tmux-switch (session)
  "Attach to a background claude tmux SESSION in this Emacs.

Lists every session on the claude tmux server -- including ones started
from another Emacs, another machine's ssh connection, or a plain
terminal -- and re-attaches to the one you pick.  When this Emacs is
already showing that session, pop to its buffer instead of attaching a
second client to it."
  (interactive (list (ai-tmux--read-session "claude" "Claude session: ")))
  (claude-tmux--attach session))

(defun claude-tmux--attach (session)
  "Show the claude tmux SESSION, (NAME DIRECTORY ATTACHED), in this Emacs."
  (require 'claude-code)
  (pcase-let* ((`(,name ,dir ,attached) session)
               (dir (and dir (file-name-as-directory dir)))
               (live (and attached dir (file-directory-p dir)
                          (claude-code--find-claude-buffers-for-directory dir))))
    (cond
     ((= 1 (length live)) (pop-to-buffer (car live)))
     (live (call-interactively #'claude-code-select-buffer))
     ;; claude-tmux re-attaches to CLAUDE_TMUX_SESSION when it exists, so name
     ;; the session explicitly rather than relying on it being derivable from
     ;; the directory (which may be gone, or shared by several sessions).
     (t (claude-code--start-in (if (and dir (file-directory-p dir))
                                   dir
                                 default-directory)
                               name)))))

(defun claude-tmux-kill (session)
  "End the background claude tmux SESSION.

Killing a Claude buffer only detaches from tmux -- the claude process
keeps running so it can be re-attached.  Use this to actually stop it."
  (interactive (list (ai-tmux--read-session "claude" "End claude session: ")))
  (ai-tmux--kill "claude" session))

(defun claude-wt (name)
  "Start Claude on a fresh git worktree of this repository, named NAME.

See `ai-wt--worktree' for how the worktree and its branch are chosen."
  (interactive (list (read-string "Worktree/branch name: ")))
  (claude-code--start-in (ai-wt--worktree name)))

(defun claude-pr (pr)
  "Start Claude on pull request PR of this repository, in a worktree of its own.

The pull request is checked out in ~/src/<repo>-pr<PR>, so Claude can
read, build, commit and push the branch while the checkout being read
stays as it was.  See `ai-pr--worktree'."
  (interactive (list (ai-pr--read "Claude on pull request: ")))
  (claude-code--start-in (ai-pr--worktree pr)))

;;;; Antigravity
;;
;; Antigravity has no Emacs package, and needs none: it is a terminal program
;; like claude, so an eat buffer running antigravity-tmux in the project root is
;; the whole of it.  These are the same four commands Claude has -- start or
;; return to this project's agent, switch to a background session, end one, and
;; start one on a fresh worktree -- over the shared code above.

(defgroup antigravity nil
  "Run Antigravity's `agy' CLI in an Emacs terminal."
  :group 'tools)

(defcustom antigravity-program "antigravity-tmux"
  "Program `antigravity' runs in an eat terminal.

The default, ~/.local/bin/antigravity-tmux, is ~/.local/bin/ai-tmux under
the name that makes it run `agy' in a detachable tmux session; set this to
\"agy\" to run the CLI directly and lose the conversation with the buffer."
  :type 'string
  :group 'antigravity)

(defcustom antigravity-history-file
  (expand-file-name "~/.gemini/antigravity-cli/history.jsonl")
  "Where `agy' writes down what it has been asked, one JSON object a line.

This is the only list of past conversations there is: the CLI has no
`claude --resume' of its own, and the summary database beside this file
stopped being written to.  `antigravity-resume' reads it to offer the
conversations started at the agy prompt -- which are the ones worth
resuming, the hundreds of one-shot `agy -p' runs never appearing here."
  :type 'file
  :group 'antigravity)

(defun antigravity--start (directory &optional session switches)
  "Open an Antigravity terminal working in DIRECTORY and return its buffer.

SESSION and SWITCHES mean what they do in `ai-term--start', which this
is Antigravity's name for."
  (ai-term--start "antigravity" antigravity-program directory session switches))

(defun antigravity (&optional arg)
  "Attach to this project's Antigravity session, starting one if needed.

Re-uses the running Antigravity buffer for the current project when there
is one, so this is also the way back in after an Emacs restart or a
dropped ssh connection.  With prefix ARG, always start a new instance."
  (interactive "P")
  (let* ((dir (ai-term--directory))
         (buffers (and (null arg) dir
                       (ai-term--buffers-for-directory "antigravity" dir))))
    (pop-to-buffer
     (if buffers
         (ai-term--read-buffer "Antigravity buffer: " buffers)
       (antigravity--start dir)))))

(defun antigravity--history ()
  "Conversations `agy' remembers being asked something in, newest first.

Each element is (ID DIRECTORY TIME TEXT): TIME is when the conversation
was last spoken to, in milliseconds, and TEXT is what was first asked of
it, which is the only description of it there is."
  (when (file-readable-p antigravity-history-file)
    (let ((conversations (make-hash-table :test 'equal))
          (order nil)
          (opening nil))
      (with-temp-buffer
        (insert-file-contents antigravity-history-file)
        (goto-char (point-min))
        (while (not (eobp))
          (let* ((line (buffer-substring-no-properties
                        (line-beginning-position) (line-end-position)))
                 (entry (and (string-prefix-p "{" line)
                             (ignore-errors
                               ;; A JSON null reads as nil rather than :null, so
                               ;; a key that is there but empty is as good as
                               ;; missing, which is what every test below wants.
                               (json-parse-string line :object-type 'alist
                                                  :null-object nil
                                                  :false-object nil))))
                 (id (alist-get 'conversationId entry))
                 (dir (alist-get 'workspace entry))
                 (time (alist-get 'timestamp entry))
                 ;; /exit and friends say nothing about what a conversation
                 ;; was for, so they never become its description.
                 (text (and (not (equal "slash_command" (alist-get 'type entry)))
                            (let ((display (alist-get 'display entry)))
                              (and (stringp display) display)))))
            (cond
             ;; A blank or half-written line says nothing about the session it
             ;; fell in the middle of, so forget what was being held.
             ((not (and entry (stringp dir) (numberp time)))
              (setq opening nil))
             ;; The first thing said in a session is written down before the
             ;; conversation has an id, so hold on to it: the next line to
             ;; mention an id we have not seen is that same conversation, and
             ;; this was what opened it.
             ((not (stringp id)) (setq opening (cons dir text)))
             (t
              (let ((known (gethash id conversations)))
                (unless known
                  (setq known (list id dir time
                                    (or (and (equal (car opening) dir)
                                             (cdr opening))
                                        text)))
                  (puthash id known conversations)
                  (push id order))
                (setf (nth 2 known) time)
                (unless (nth 3 known) (setf (nth 3 known) text)))
              (setq opening nil))))
          (forward-line 1)))
      (sort (mapcar (lambda (id) (gethash id conversations)) (nreverse order))
            (lambda (a b) (> (nth 2 a) (nth 2 b)))))))

(defun antigravity-resume (&optional arg)
  "Start Antigravity on one of the conversations it has had here.

Antigravity has no picker of its own to match `claude --resume': its CLI
re-opens a conversation named by id, or the most recent one, but will not
list them.  So this reads the list out of `antigravity-history-file' --
what was first asked of each conversation, and when it was last spoken to
-- and runs `agy --conversation ID' on the one you pick.  It is the way
back into a conversation whose tmux session is gone, after a reboot or an
`antigravity-tmux-kill'; use `antigravity' for the one running right now.

Only this directory's conversations are offered -- this one or a
subdirectory of it.  With prefix ARG, every directory's are, and the one
you pick is started in the directory it belongs to.  Either way a
conversation whose directory has since been deleted is left out, there
being nowhere to start it.

The conversation comes back on a tmux session of its own, so the one
already running here, if any, is left alone."
  (interactive "P")
  (let* ((dir (ai-term--directory))
         (history (antigravity--history))
         (live (ai-term--live-conversations history))
         (here (if arg live (ai-term--conversations-under live dir))))
    (cond
     ((null history)
      ;; No history to read -- a fresh install, or a CLI that has stopped
      ;; keeping one.  Ask for the most recent conversation instead, which is
      ;; the one thing agy will do without being told an id.
      (message "No conversation history in %s; continuing the most recent one"
               (abbreviate-file-name antigravity-history-file))
      (pop-to-buffer (antigravity--start dir nil '("--continue"))))
     ((null here)
      ;; Test what is left rather than what was asked for: with every
      ;; conversation's directory gone, pointing at %s would only lead to the
      ;; other message.
      (if live
          (user-error "No Antigravity conversations in %s (%s for every directory)"
                      (abbreviate-file-name dir)
                      (substitute-command-keys "\\[universal-argument]"))
        (user-error "Every Antigravity conversation was in a directory that is gone")))
     (t
      (let ((conversation (ai-term--read-conversation
                           "Resume Antigravity conversation: " here arg)))
        (pop-to-buffer (antigravity--start (nth 1 conversation) nil
                                           (list "--conversation"
                                                 (nth 0 conversation)))))))))

(defun antigravity-select-buffer ()
  "Switch to one of the Antigravity terminals running in this Emacs."
  (interactive)
  (let ((buffers (or (ai-term--all-buffers "antigravity")
                     (user-error "No Antigravity buffers"))))
    (pop-to-buffer (ai-term--read-buffer "Antigravity buffer: " buffers))))

(defun antigravity-tmux-switch (session)
  "Attach to a background antigravity tmux SESSION in this Emacs.

Lists every session on the antigravity tmux server -- including ones
started from another Emacs, another machine's ssh connection, or a plain
terminal -- and re-attaches to the one you pick.  When this Emacs is
already showing that session, pop to its buffer instead of attaching a
second client to it."
  (interactive (list (ai-tmux--read-session "antigravity" "Antigravity session: ")))
  (antigravity-tmux--attach session))

(defun antigravity-tmux--attach (session)
  "Show the antigravity tmux SESSION, (NAME DIRECTORY ATTACHED), in this Emacs."
  (pcase-let* ((`(,name ,dir ,attached) session)
               (dir (and dir (file-name-as-directory dir)))
               (live (and attached dir (file-directory-p dir)
                          (ai-term--buffers-for-directory "antigravity" dir))))
    (pop-to-buffer
     (if live
         (ai-term--read-buffer "Antigravity buffer: " live)
       ;; antigravity-tmux re-attaches to ANTIGRAVITY_TMUX_SESSION when it
       ;; exists, so name the session explicitly rather than relying on it being
       ;; derivable from the directory (which may be gone, or shared by several
       ;; sessions).
       (antigravity--start (if (and dir (file-directory-p dir))
                               dir
                             default-directory)
                           name)))))

(defun antigravity-tmux-kill (session)
  "End the background antigravity tmux SESSION.

Killing an Antigravity buffer only detaches from tmux -- the agent keeps
running so it can be re-attached.  Use this to actually stop it."
  (interactive (list (ai-tmux--read-session "antigravity" "End antigravity session: ")))
  (ai-tmux--kill "antigravity" session))

(defun antigravity-wt (name)
  "Start Antigravity on a fresh git worktree of this repository, named NAME.

See `ai-wt--worktree' for how the worktree and its branch are chosen."
  (interactive (list (read-string "Worktree/branch name: ")))
  (pop-to-buffer (antigravity--start (ai-wt--worktree name))))

(defun agy-pr (pr)
  "Start Antigravity on pull request PR of this repository, in its own worktree.

`claude-pr' for the other agent; see `ai-pr--worktree'."
  (interactive (list (ai-pr--read "Antigravity on pull request: ")))
  (pop-to-buffer (antigravity--start (ai-pr--worktree pr))))

;;;; agy
;;
;; The CLI is called agy, and so are the two commands that were written with it
;; in mind (`agy-dwim', `agy-pr'), but everything else here is spelled out as
;; antigravity -- which means M-x agy finds two commands and misses six.  Give
;; the whole set the short name as well, so the agent can be reached by the name
;; it actually goes by.  Both names run the same code, and starting one this way
;; is a tmux session like any other: `antigravity--start' runs antigravity-tmux,
;; so the conversation outlives the buffer, the Emacs and the ssh connection.

(dolist (pair '((agy                . antigravity)
                (agy-resume         . antigravity-resume)
                (agy-select-buffer  . antigravity-select-buffer)
                (agy-tmux-switch    . antigravity-tmux-switch)
                (agy-tmux-kill      . antigravity-tmux-kill)
                (agy-wt             . antigravity-wt)))
  (defalias (car pair) (cdr pair)
    (format "Alias for `%s'." (cdr pair))))

;;;; Copilot
;;
;; GitHub's `copilot' CLI is a terminal program with no Emacs package, like
;; agy, so an eat buffer running copilot-tmux in the project root is the whole
;; of it.  Unlike agy it has a picker for its own past conversations
;; (`copilot --resume'), as claude does, so this is the smallest of the three
;; agents: the shared code above, plus the name of the program to run.
;;
;; Everything here is spelled copilot-cli- rather than copilot-.  copilot.el
;; (the inline completion) and copilot-chat.el are both loaded in this config
;; and own the `copilot-' prefix between them; commands of ours landing in the
;; middle of theirs would be a collision waiting for their next release.  The
;; CLI it runs is still plain `copilot'.

(defgroup copilot-cli nil
  "Run GitHub Copilot's `copilot' CLI in an Emacs terminal."
  :group 'tools)

(defcustom copilot-cli-program "copilot-tmux"
  "Program `copilot-cli' runs in an eat terminal.

The default, ~/.local/bin/copilot-tmux, is ~/.local/bin/ai-tmux under the
name that makes it run `copilot' in a detachable tmux session; set this
to \"copilot\" to run the CLI directly and lose the conversation with the
buffer."
  :type 'string
  :group 'copilot-cli)

(defun copilot-cli--start (directory &optional session switches)
  "Open a Copilot terminal working in DIRECTORY and return its buffer.

SESSION and SWITCHES mean what they do in `ai-term--start', which this
is Copilot's name for."
  (ai-term--start "copilot" copilot-cli-program directory session switches))

(defun copilot-cli (&optional arg)
  "Attach to this project's Copilot session, starting one if needed.

Re-uses the running Copilot buffer for the current project when there is
one, so this is also the way back in after an Emacs restart or a dropped
ssh connection.  With prefix ARG, always start a new instance."
  (interactive "P")
  (let* ((dir (ai-term--directory))
         (buffers (and (null arg) dir
                       (ai-term--buffers-for-directory "copilot" dir))))
    (pop-to-buffer
     (if buffers
         (ai-term--read-buffer "Copilot buffer: " buffers)
       (copilot-cli--start dir)))))

(defun copilot-cli-resume ()
  "Start Copilot on one of the conversations it has had here.

`copilot --resume': the CLI lists the sessions it remembers and re-opens
the one you pick, the way `claude --resume' does -- which is the way back
to a conversation whose tmux session is gone, after a reboot or a
`copilot-cli-tmux-kill'.  Use `copilot-cli' for the one running right
now.

The resumed conversation gets a tmux session of its own, so the one
already running here, if any, is left alone."
  (interactive)
  (pop-to-buffer (copilot-cli--start (ai-term--directory) nil '("--resume"))))

(defun copilot-cli-select-buffer ()
  "Switch to one of the Copilot terminals running in this Emacs."
  (interactive)
  (let ((buffers (or (ai-term--all-buffers "copilot")
                     (user-error "No Copilot buffers"))))
    (pop-to-buffer (ai-term--read-buffer "Copilot buffer: " buffers))))

(defun copilot-cli-tmux-switch (session)
  "Attach to a background copilot tmux SESSION in this Emacs.

Lists every session on the copilot tmux server -- including ones started
from another Emacs, another machine's ssh connection, or a plain
terminal -- and re-attaches to the one you pick.  When this Emacs is
already showing that session, pop to its buffer instead of attaching a
second client to it."
  (interactive (list (ai-tmux--read-session "copilot" "Copilot session: ")))
  (copilot-cli-tmux--attach session))

(defun copilot-cli-tmux--attach (session)
  "Show the copilot tmux SESSION, (NAME DIRECTORY ATTACHED), in this Emacs."
  (pcase-let* ((`(,name ,dir ,attached) session)
               (dir (and dir (file-name-as-directory dir)))
               (live (and attached dir (file-directory-p dir)
                          (ai-term--buffers-for-directory "copilot" dir))))
    (pop-to-buffer
     (if live
         (ai-term--read-buffer "Copilot buffer: " live)
       ;; copilot-tmux re-attaches to COPILOT_TMUX_SESSION when it exists, so
       ;; name the session explicitly rather than relying on it being derivable
       ;; from the directory (which may be gone, or shared by several sessions).
       (copilot-cli--start (if (and dir (file-directory-p dir))
                               dir
                             default-directory)
                           name)))))

(defun copilot-cli-tmux-kill (session)
  "End the background copilot tmux SESSION.

Killing a Copilot buffer only detaches from tmux -- the agent keeps
running so it can be re-attached.  Use this to actually stop it."
  (interactive (list (ai-tmux--read-session "copilot" "End copilot session: ")))
  (ai-tmux--kill "copilot" session))

(defun copilot-cli-wt (name)
  "Start Copilot on a fresh git worktree of this repository, named NAME.

See `ai-wt--worktree' for how the worktree and its branch are chosen."
  (interactive (list (read-string "Worktree/branch name: ")))
  (pop-to-buffer (copilot-cli--start (ai-wt--worktree name))))

(defun copilot-cli-pr (pr)
  "Start Copilot on pull request PR of this repository, in its own worktree.

`claude-pr' for the other agent; see `ai-pr--worktree'."
  (interactive (list (ai-pr--read "Copilot on pull request: ")))
  (pop-to-buffer (copilot-cli--start (ai-pr--worktree pr))))

;;;; opencode
;;
;; opencode is a terminal program with no Emacs package, like agy and copilot,
;; so an eat buffer running opencode-tmux in the project root is most of it.
;; The one thing it wants of its own is a resume picker: `opencode --session ID'
;; re-opens a conversation and `opencode session list' prints the ids, but the
;; CLI never joins the two up.  So `opencode-resume' joins them, over the same
;; picker `antigravity-resume' uses.

(defgroup opencode nil
  "Run the `opencode' CLI in an Emacs terminal."
  :group 'tools)

(defcustom opencode-program "opencode-tmux"
  "Program `opencode' runs in an eat terminal.

The default, ~/.local/bin/opencode-tmux, is ~/.local/bin/ai-tmux under the
name that makes it run `opencode' in a detachable tmux session; set this
to \"opencode\" to run the CLI directly and lose the conversation with the
buffer."
  :type 'string
  :group 'opencode)

(defcustom opencode-cli-program "opencode"
  "The `opencode' CLI itself, which `opencode-resume' asks for its sessions.

`opencode-program' is the wrapper that runs the agent under tmux; this one
is only ever asked to print, so it is the CLI and never the wrapper."
  :type 'string
  :group 'opencode)

(defun opencode--start (directory &optional session switches)
  "Open an opencode terminal working in DIRECTORY and return its buffer.

SESSION and SWITCHES mean what they do in `ai-term--start', which this is
opencode's name for."
  (ai-term--start "opencode" opencode-program directory session switches))

(defun opencode (&optional arg)
  "Attach to this project's opencode session, starting one if needed.

Re-uses the running opencode buffer for the current project when there is
one, so this is also the way back in after an Emacs restart or a dropped
ssh connection.  With prefix ARG, always start a new instance."
  (interactive "P")
  (let* ((dir (ai-term--directory))
         (buffers (and (null arg) dir
                       (ai-term--buffers-for-directory "opencode" dir))))
    (pop-to-buffer
     (if buffers
         (ai-term--read-buffer "opencode buffer: " buffers)
       (opencode--start dir)))))

(defun opencode--conversations (directory)
  "Conversations opencode remembers from DIRECTORY's project, newest first.

`opencode session list --format json' is the whole of the source: it
prints the project's root sessions -- the ones started at the prompt,
rather than the children an agent spawns for itself -- most recently
spoken to first, each with the directory it was held in.  opencode scopes
that list to the current project and has no switch to widen it, which is
what makes `opencode-resume''s prefix argument mean the project rather
than the machine."
  (ai-term--cli-conversations directory opencode-cli-program
                              "session" "list" "--format" "json"))

(defun opencode-resume (&optional arg)
  "Start opencode on one of the conversations it has had here.

opencode's own picker is inside the TUI, which is no help when there is no
TUI running: the CLI re-opens a conversation named by id (`--session'), or
the most recent one (`--continue'), and lists them only under a separate
`opencode session list'.  So this reads that list -- the title opencode
gave each conversation, and when it was last spoken to -- and starts
`opencode --session' on the one you pick.  It is the way back into a
conversation whose tmux session is gone, after a reboot or an
`opencode-tmux-kill'; use `opencode' for the one running right now.

The list is opencode's own, so it is this project's conversations rather
than the machine's.  Only the ones held in this directory or a
subdirectory of it are offered; with prefix ARG the whole project's are,
annotated with where they were, and the one you pick starts in the
directory it belongs to.  Either way a conversation whose directory has
since been deleted is left out, there being nowhere to start it.

The conversation comes back on a tmux session of its own, so the one
already running here, if any, is left alone."
  (interactive "P")
  (let* ((dir (ai-term--directory))
         (conversations (opencode--conversations dir))
         (live (ai-term--live-conversations conversations))
         (here (if arg live (ai-term--conversations-under live dir))))
    (cond
     ((null conversations)
      ;; Nothing listed -- a fresh install, a project opencode has never been
      ;; run in, or a CLI that cannot be asked.  Ask for the most recent
      ;; conversation instead, which it will do without being told an id.
      (message "opencode listed no conversations; continuing the most recent")
      (pop-to-buffer (opencode--start dir nil '("--continue"))))
     ((null here)
      ;; Test what is left rather than what was asked for: with every
      ;; conversation's directory gone, pointing at %s would only lead to the
      ;; other message.
      (if live
          (user-error "No opencode conversations in %s (%s for the whole project)"
                      (abbreviate-file-name dir)
                      (substitute-command-keys "\\[universal-argument]"))
        (user-error "Every opencode conversation was in a directory that is gone")))
     (t
      (let ((conversation (ai-term--read-conversation
                           "Resume opencode conversation: " here arg)))
        (pop-to-buffer (opencode--start (nth 1 conversation) nil
                                        (list "--session"
                                              (nth 0 conversation)))))))))

(defun opencode-select-buffer ()
  "Switch to one of the opencode terminals running in this Emacs."
  (interactive)
  (let ((buffers (or (ai-term--all-buffers "opencode")
                     (user-error "No opencode buffers"))))
    (pop-to-buffer (ai-term--read-buffer "opencode buffer: " buffers))))

(defun opencode-tmux-switch (session)
  "Attach to a background opencode tmux SESSION in this Emacs.

Lists every session on the opencode tmux server -- including ones started
from another Emacs, another machine's ssh connection, or a plain
terminal -- and re-attaches to the one you pick.  When this Emacs is
already showing that session, pop to its buffer instead of attaching a
second client to it."
  (interactive (list (ai-tmux--read-session "opencode" "opencode session: ")))
  (opencode-tmux--attach session))

(defun opencode-tmux--attach (session)
  "Show the opencode tmux SESSION, (NAME DIRECTORY ATTACHED), in this Emacs."
  (pcase-let* ((`(,name ,dir ,attached) session)
               (dir (and dir (file-name-as-directory dir)))
               (live (and attached dir (file-directory-p dir)
                          (ai-term--buffers-for-directory "opencode" dir))))
    (pop-to-buffer
     (if live
         (ai-term--read-buffer "opencode buffer: " live)
       ;; opencode-tmux re-attaches to OPENCODE_TMUX_SESSION when it exists, so
       ;; name the session explicitly rather than relying on it being derivable
       ;; from the directory (which may be gone, or shared by several sessions).
       (opencode--start (if (and dir (file-directory-p dir))
                            dir
                          default-directory)
                        name)))))

(defun opencode-tmux-kill (session)
  "End the background opencode tmux SESSION.

Killing an opencode buffer only detaches from tmux -- the agent keeps
running so it can be re-attached.  Use this to actually stop it."
  (interactive (list (ai-tmux--read-session "opencode" "End opencode session: ")))
  (ai-tmux--kill "opencode" session))

(defun opencode-wt (name)
  "Start opencode on a fresh git worktree of this repository, named NAME.

See `ai-wt--worktree' for how the worktree and its branch are chosen."
  (interactive (list (read-string "Worktree/branch name: ")))
  (pop-to-buffer (opencode--start (ai-wt--worktree name))))

(defun opencode-pr (pr)
  "Start opencode on pull request PR of this repository, in its own worktree.

`claude-pr' for the other agent; see `ai-pr--worktree'."
  (interactive (list (ai-pr--read "opencode on pull request: ")))
  (pop-to-buffer (opencode--start (ai-pr--worktree pr))))

;;;; kilo
;;
;; kilo is opencode's fork, and it shows: `kilo' takes the same switches as
;; `opencode', keeps its sessions the same way, and prints them from the same
;; `session list --format json'.  So this is the opencode section again with one
;; difference -- kilo's session list takes `--all', which opencode has no switch
;; for, so a prefix argument here reaches every project rather than stopping at
;; this one.
;;
;; The CLI answers to `kilo' and `kilocode' both.  `kilo' is the name everything
;; else it owns is spelled with -- ~/.config/kilo, ~/.local/share/kilo -- so it
;; is the name used here, and kilocode-tmux is linked to it the way agy is to
;; antigravity.

(defgroup kilo nil
  "Run the `kilo' CLI in an Emacs terminal."
  :group 'tools)

(defcustom kilo-program "kilo-tmux"
  "Program `kilo' runs in an eat terminal.

The default, ~/.local/bin/kilo-tmux, is ~/.local/bin/ai-tmux under the
name that makes it run `kilo' in a detachable tmux session; set this to
\"kilo\" to run the CLI directly and lose the conversation with the
buffer."
  :type 'string
  :group 'kilo)

(defcustom kilo-cli-program "kilo"
  "The `kilo' CLI itself, which `kilo-resume' asks for its sessions.

`kilo-program' is the wrapper that runs the agent under tmux; this one is
only ever asked to print, so it is the CLI and never the wrapper."
  :type 'string
  :group 'kilo)

(defun kilo--start (directory &optional session switches)
  "Open a kilo terminal working in DIRECTORY and return its buffer.

SESSION and SWITCHES mean what they do in `ai-term--start', which this is
kilo's name for."
  (ai-term--start "kilo" kilo-program directory session switches))

(defun kilo (&optional arg)
  "Attach to this project's kilo session, starting one if needed.

Re-uses the running kilo buffer for the current project when there is
one, so this is also the way back in after an Emacs restart or a dropped
ssh connection.  With prefix ARG, always start a new instance."
  (interactive "P")
  (let* ((dir (ai-term--directory))
         (buffers (and (null arg) dir
                       (ai-term--buffers-for-directory "kilo" dir))))
    (pop-to-buffer
     (if buffers
         (ai-term--read-buffer "kilo buffer: " buffers)
       (kilo--start dir)))))

(defun kilo--conversations (directory &optional everywhere)
  "Conversations kilo remembers from DIRECTORY, newest first.

This project's, or with EVERYWHERE every project's: `kilo session list'
takes an `--all' that opencode's does not, so the wider list is the CLI's
own answer rather than something assembled here."
  (apply #'ai-term--cli-conversations directory kilo-cli-program
         "session" "list" "--format" "json"
         (and everywhere '("--all"))))

(defun kilo-resume (&optional arg)
  "Start kilo on one of the conversations it has had here.

kilo's own picker is inside the TUI, which is no help when the reason you
are looking is that no TUI is running: the CLI re-opens a conversation
named by id (`--session'), or the most recent one (`--continue'), and
lists them only under a separate `kilo session list'.  So this reads that
list -- the title kilo gave each conversation, and when it was last spoken
to -- and starts `kilo --session' on the one you pick.  It is the way back
into a conversation whose tmux session is gone, after a reboot or a
`kilo-tmux-kill'; use `kilo' for the one running right now.

Only this directory's conversations are offered -- this one or a
subdirectory of it.  With prefix ARG every project's are, annotated with
where they were, and the one you pick is started in the directory it
belongs to.  Either way a conversation whose directory has since been
deleted is left out, there being nowhere to start it.

The conversation comes back on a tmux session of its own, so the one
already running here, if any, is left alone."
  (interactive "P")
  (let* ((dir (ai-term--directory))
         (conversations (kilo--conversations dir arg))
         (live (ai-term--live-conversations conversations))
         (here (if arg live (ai-term--conversations-under live dir))))
    (cond
     ((null conversations)
      ;; Nothing listed -- a fresh install, a project kilo has never been run
      ;; in, or a CLI that cannot be asked.  Ask for the most recent
      ;; conversation instead, which it will do without being told an id.
      (message "kilo listed no conversations; continuing the most recent")
      (pop-to-buffer (kilo--start dir nil '("--continue"))))
     ((null here)
      ;; Test what is left rather than what was asked for: with every
      ;; conversation's directory gone, pointing at %s would only lead to the
      ;; other message.
      (if live
          (user-error "No kilo conversations in %s (%s for every project)"
                      (abbreviate-file-name dir)
                      (substitute-command-keys "\\[universal-argument]"))
        (user-error "Every kilo conversation was in a directory that is gone")))
     (t
      (let ((conversation (ai-term--read-conversation
                           "Resume kilo conversation: " here arg)))
        (pop-to-buffer (kilo--start (nth 1 conversation) nil
                                    (list "--session" (nth 0 conversation)))))))))

(defun kilo-select-buffer ()
  "Switch to one of the kilo terminals running in this Emacs."
  (interactive)
  (let ((buffers (or (ai-term--all-buffers "kilo")
                     (user-error "No kilo buffers"))))
    (pop-to-buffer (ai-term--read-buffer "kilo buffer: " buffers))))

(defun kilo-tmux-switch (session)
  "Attach to a background kilo tmux SESSION in this Emacs.

Lists every session on the kilo tmux server -- including ones started from
another Emacs, another machine's ssh connection, or a plain terminal --
and re-attaches to the one you pick.  When this Emacs is already showing
that session, pop to its buffer instead of attaching a second client to
it."
  (interactive (list (ai-tmux--read-session "kilo" "kilo session: ")))
  (kilo-tmux--attach session))

(defun kilo-tmux--attach (session)
  "Show the kilo tmux SESSION, (NAME DIRECTORY ATTACHED), in this Emacs."
  (pcase-let* ((`(,name ,dir ,attached) session)
               (dir (and dir (file-name-as-directory dir)))
               (live (and attached dir (file-directory-p dir)
                          (ai-term--buffers-for-directory "kilo" dir))))
    (pop-to-buffer
     (if live
         (ai-term--read-buffer "kilo buffer: " live)
       ;; kilo-tmux re-attaches to KILO_TMUX_SESSION when it exists, so name the
       ;; session explicitly rather than relying on it being derivable from the
       ;; directory (which may be gone, or shared by several sessions).
       (kilo--start (if (and dir (file-directory-p dir))
                        dir
                      default-directory)
                    name)))))

(defun kilo-tmux-kill (session)
  "End the background kilo tmux SESSION.

Killing a kilo buffer only detaches from tmux -- the agent keeps running
so it can be re-attached.  Use this to actually stop it."
  (interactive (list (ai-tmux--read-session "kilo" "End kilo session: ")))
  (ai-tmux--kill "kilo" session))

(defun kilo-wt (name)
  "Start kilo on a fresh git worktree of this repository, named NAME.

See `ai-wt--worktree' for how the worktree and its branch are chosen."
  (interactive (list (read-string "Worktree/branch name: ")))
  (pop-to-buffer (kilo--start (ai-wt--worktree name))))

(defun kilo-pr (pr)
  "Start kilo on pull request PR of this repository, in its own worktree.

`claude-pr' for the other agent; see `ai-pr--worktree'."
  (interactive (list (ai-pr--read "kilo on pull request: ")))
  (pop-to-buffer (kilo--start (ai-pr--worktree pr))))

;;;; kilocode
;;
;; The CLI answers to both names, so the commands do too -- `M-x kilocode' finds
;; the whole set rather than nothing at all.  Both names run the same code, on
;; the same tmux server: it is `kilo' underneath either way.

(dolist (pair '((kilocode               . kilo)
                (kilocode-resume        . kilo-resume)
                (kilocode-select-buffer . kilo-select-buffer)
                (kilocode-tmux-switch   . kilo-tmux-switch)
                (kilocode-tmux-kill     . kilo-tmux-kill)
                (kilocode-wt            . kilo-wt)
                (kilocode-pr            . kilo-pr)))
  (defalias (car pair) (cdr pair)
    (format "Alias for `%s'." (cdr pair))))

;;;; One key for all of it
;;
;; The three things worth doing with an agent depend entirely on where you are
;; standing when you ask, so ask for all three with one key -- one such key per
;; agent, since which agent is not one of the three things.

(defun ai-dwim (rejoin worktree)
  "The body of `claude-dwim', `agy-dwim' and the other agents' dwims.

REJOIN is called with no arguments to show the conversation belonging to
this directory, and returns nil when there is none to show.  WORKTREE is
called with no arguments to cut a fresh worktree and start an agent in it.

Which of them runs depends on where it was called from:

  in an agent terminal   every other conversation, in `ai-tmux-list'
  in magit, on the trunk a worktree, because this is where work starts
  anywhere with a session that conversation
  anywhere else          a worktree, so the agent never churns a checkout
                         you are reading"
  (cond
   ;; Already looking at one conversation: the useful thing from here is all
   ;; the others.
   ((derived-mode-p 'eat-mode 'vterm-mode) (ai-tmux-list))
   ;; Magit on the trunk is where a new task starts, and a new task gets a
   ;; worktree even when the trunk already has an agent of its own.
   ((and (derived-mode-p 'magit-mode)
         (ai-wt--on-trunk-p (ai-term--directory)))
    (funcall worktree))
   ;; A conversation for this directory, running here or left running
   ;; somewhere else.
   ((funcall rejoin))
   (t (funcall worktree))))

(defun claude-dwim--rejoin ()
  "Show this directory's Claude conversation, or return nil for none.

A buffer in this Emacs when there is one; failing that a tmux session
started here by an Emacs that has since gone, which claude-tmux
re-attaches to rather than starting a second claude.

The session is named rather than left to be derived from the directory:
a conversation that came back through `claude-resume' is running under
`<dir>-<hash>-2', which is this directory's session but not this
directory's name."
  (require 'claude-code)
  (let* ((dir (claude-code--directory))
         (buffers (and dir (claude-code--find-claude-buffers-for-directory dir)))
         (session (and dir (null buffers)
                       (ai-tmux--session-for-directory "claude" dir))))
    (cond
     ((= 1 (length buffers)) (pop-to-buffer (car buffers)) t)
     (buffers (call-interactively #'claude-code-select-buffer) t)
     (session (claude-code--start-in dir (nth 0 session)) t))))

(defun claude-dwim ()
  "Do the useful thing with Claude for wherever this was called from.

In an agent terminal, list every conversation on the machine.  In magit
on the trunk, cut a worktree and start Claude in it.  Anywhere else, go
back to this directory's conversation, and cut a worktree for one when
there is none -- so Claude works on a checkout of its own rather than the
one being read.  See `ai-dwim'."
  (interactive)
  (ai-dwim #'claude-dwim--rejoin
           (lambda () (call-interactively #'claude-wt))))

(defun agy-dwim--rejoin ()
  "Show this directory's Antigravity conversation, or return nil for none.

See `claude-dwim--rejoin' for why the session is named."
  (let* ((dir (ai-term--directory))
         (buffers (and dir (ai-term--buffers-for-directory
                            "antigravity" dir)))
         (session (and dir (null buffers)
                       (ai-tmux--session-for-directory "antigravity" dir))))
    (cond
     (buffers (pop-to-buffer (ai-term--read-buffer "Antigravity buffer: "
                                                   buffers))
              t)
     (session (pop-to-buffer (antigravity--start dir (nth 0 session))) t))))

(defun agy-dwim ()
  "Do the useful thing with Antigravity for wherever this was called from.

`claude-dwim' for the other agent; see `ai-dwim' for what it decides."
  (interactive)
  (ai-dwim #'agy-dwim--rejoin
           (lambda () (call-interactively #'antigravity-wt))))

(defun copilot-cli-dwim--rejoin ()
  "Show this directory's Copilot conversation, or return nil for none.

See `claude-dwim--rejoin' for why the session is named."
  (let* ((dir (ai-term--directory))
         (buffers (and dir (ai-term--buffers-for-directory "copilot" dir)))
         (session (and dir (null buffers)
                       (ai-tmux--session-for-directory "copilot" dir))))
    (cond
     (buffers (pop-to-buffer (ai-term--read-buffer "Copilot buffer: " buffers))
              t)
     (session (pop-to-buffer (copilot-cli--start dir (nth 0 session))) t))))

(defun copilot-cli-dwim ()
  "Do the useful thing with Copilot for wherever this was called from.

`claude-dwim' for the other agent; see `ai-dwim' for what it decides."
  (interactive)
  (ai-dwim #'copilot-cli-dwim--rejoin
           (lambda () (call-interactively #'copilot-cli-wt))))

(defun opencode-dwim--rejoin ()
  "Show this directory's opencode conversation, or return nil for none.

See `claude-dwim--rejoin' for why the session is named."
  (let* ((dir (ai-term--directory))
         (buffers (and dir (ai-term--buffers-for-directory "opencode" dir)))
         (session (and dir (null buffers)
                       (ai-tmux--session-for-directory "opencode" dir))))
    (cond
     (buffers (pop-to-buffer (ai-term--read-buffer "opencode buffer: " buffers))
              t)
     (session (pop-to-buffer (opencode--start dir (nth 0 session))) t))))

(defun opencode-dwim ()
  "Do the useful thing with opencode for wherever this was called from.

`claude-dwim' for the other agent; see `ai-dwim' for what it decides."
  (interactive)
  (ai-dwim #'opencode-dwim--rejoin
           (lambda () (call-interactively #'opencode-wt))))

(defun kilo-dwim--rejoin ()
  "Show this directory's kilo conversation, or return nil for none.

See `claude-dwim--rejoin' for why the session is named."
  (let* ((dir (ai-term--directory))
         (buffers (and dir (ai-term--buffers-for-directory "kilo" dir)))
         (session (and dir (null buffers)
                       (ai-tmux--session-for-directory "kilo" dir))))
    (cond
     (buffers (pop-to-buffer (ai-term--read-buffer "kilo buffer: " buffers))
              t)
     (session (pop-to-buffer (kilo--start dir (nth 0 session))) t))))

(defun kilo-dwim ()
  "Do the useful thing with kilo for wherever this was called from.

`claude-dwim' for the other agent; see `ai-dwim' for what it decides."
  (interactive)
  (ai-dwim #'kilo-dwim--rejoin
           (lambda () (call-interactively #'kilo-wt))))

;; The rest of kilo's commands are aliased to kilocode where they are defined;
;; this one is defined here, after that block, so it is aliased here.
(defalias 'kilocode-dwim 'kilo-dwim "Alias for `kilo-dwim'.")

(defvar copilot-cli-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map "o" #'copilot-cli)
    (define-key map (kbd "RET") #'copilot-cli)
    (define-key map "d" #'copilot-cli-dwim)
    (define-key map "b" #'copilot-cli-select-buffer)
    (define-key map "s" #'copilot-cli-tmux-switch)
    (define-key map "k" #'copilot-cli-tmux-kill)
    (define-key map "w" #'copilot-cli-wt)
    (define-key map "p" #'copilot-cli-pr)
    (define-key map "r" #'copilot-cli-resume)
    map)
  "Keymap for the Copilot commands, bound to \\`C-c a o'.

Claude has claude-code.el's map on \\`C-c c' and Antigravity has the
lowercase letters of `ai-command-map'; every agent after those two gets a
prefix of its own rather than another case of every letter, and repeats
Antigravity's letters under it -- so \\`C-c a o o' starts Copilot,
\\`C-c a o w' cuts a worktree, and so on.  `opencode-command-map' is the
same again on \\`C-c a e'.")

(defvar opencode-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map "e" #'opencode)
    (define-key map (kbd "RET") #'opencode)
    (define-key map "d" #'opencode-dwim)
    (define-key map "b" #'opencode-select-buffer)
    (define-key map "s" #'opencode-tmux-switch)
    (define-key map "k" #'opencode-tmux-kill)
    (define-key map "w" #'opencode-wt)
    (define-key map "p" #'opencode-pr)
    (define-key map "r" #'opencode-resume)
    map)
  "Keymap for the opencode commands, bound to \\`C-c a e'.

Copilot's `copilot-cli-command-map' laid out for a fourth agent.  The
prefix is `e' because `o' went to Copilot first and `c', `p' and `d' to
Claude, pull requests and the dwims: of the letters of opencode's own name
still going spare it is the first.  \\`C-c a e e' starts the agent,
\\`C-c a e w' cuts a worktree, and so on.")

(defvar kilo-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map "l" #'kilo)
    (define-key map (kbd "RET") #'kilo)
    (define-key map "d" #'kilo-dwim)
    (define-key map "b" #'kilo-select-buffer)
    (define-key map "s" #'kilo-tmux-switch)
    (define-key map "k" #'kilo-tmux-kill)
    (define-key map "w" #'kilo-wt)
    (define-key map "p" #'kilo-pr)
    (define-key map "r" #'kilo-resume)
    map)
  "Keymap for the kilo commands, bound to \\`C-c a l'.

`opencode-command-map' laid out for a fifth agent.  The prefix is `l'
because `k' is Antigravity's kill, `i' the session list and `o' Copilot:
of the letters of kilo's own name it is the one left.  \\`C-c a l l'
starts the agent, \\`C-c a l w' cuts a worktree, and so on.")

(defvar ai-command-map
  (let ((map (make-sparse-keymap)))
    ;; Across every agent.
    (define-key map "i" #'ai-tmux-list)
    (define-key map "j" #'ai-tmux-switch)
    (define-key map "d" #'claude-dwim)
    (define-key map "D" #'agy-dwim)
    ;; Claude keeps claude-code.el's own map on C-c c; this is only the way in.
    (define-key map "c" #'claude)
    ;; Copilot, opencode and kilo, whose commands are one key further in: see
    ;; `copilot-cli-command-map', `opencode-command-map', `kilo-command-map'.
    (define-key map "o" copilot-cli-command-map)
    (define-key map "O" #'copilot-cli-dwim)
    (define-key map "e" opencode-command-map)
    (define-key map "E" #'opencode-dwim)
    (define-key map "l" kilo-command-map)
    (define-key map "L" #'kilo-dwim)
    ;; Antigravity, which has no map of its own.
    (define-key map "a" #'antigravity)
    (define-key map "b" #'antigravity-select-buffer)
    (define-key map "s" #'antigravity-tmux-switch)
    (define-key map "k" #'antigravity-tmux-kill)
    (define-key map "w" #'antigravity-wt)
    ;; A pull request, in a worktree of its own.
    (define-key map "p" #'claude-pr)
    (define-key map "P" #'agy-pr)
    ;; A conversation whose session is gone: back in through the agent's own
    ;; history rather than tmux's.
    (define-key map "r" #'claude-resume)
    (define-key map "R" #'antigravity-resume)
    map)
  "Keymap for the coding-agent commands, bound to \\`C-c a'.")

(global-set-key (kbd "C-c a") ai-command-map)

(provide 'emacs-config)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; emacs-config.el ends here
;; Local Variables:
;; coding: utf-8-emacs
;; End:
