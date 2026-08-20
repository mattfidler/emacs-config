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
  (ergoemacs-define-key ergoemacs-override-keymap (kbd "<apps>")  'treemacs (kbd "q")))

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

  (ergoemacs-define-key ergoemacs-override-keymap (kbd "<apps>") 'mu4e (kbd "p")))


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


;; Dark is for remote sessions, light is for local sessions.
(when nvs
  (if (daemonp)
      (add-hook 'after-make-frame-functions
                (defun my/theme-init-daemon (frame)
                  (with-selected-frame frame
                    (load-theme 'zenburn t))
                  ;; Run this hook only once.
                  (remove-hook 'after-make-frame-functions
                               #'my/theme-init-daemon)
                  (fmakunbound 'my/theme-init-daemon)))
    (load-theme 'zenburn t)))

;; For the daemon it has emacs' original environtmental variables so
;; this does not work
(when (and (not nvs)
           (or (getenv "SSH_CONNECTION") (getenv "SSH_CLIENT")))
  (load-theme 'solarized-dark t))

(when (and (not nvs)
           (not (or (getenv "SSH_CONNECTION") (getenv "SSH_CLIENT"))))
  (if (daemonp)
      (add-hook 'after-make-frame-functions
                (defun my/theme-init-daemon (frame)
                  (with-selected-frame frame
                    (load-theme 'solarized-light t))
                  ;; Run this hook only once.
                  (remove-hook 'after-make-frame-functions
                               #'my/theme-init-daemon)
                  (fmakunbound 'my/theme-init-daemon)))
    (load-theme 'solarized-light t)))

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

(defun eat-osc52-select-means-clipboard (fn terminal selection data)
  "Around FN, call `eat--manipulate-kill-ring' with the clipboard.
SELECTION is remapped to `:clipboard' when it is the unnamed `:select'
target; TERMINAL and DATA are passed through untouched."
  (funcall fn terminal (if (eq selection :select) :clipboard selection) data))

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

;; systemd starts `emacs --daemon' with a minimal PATH, so ~/.local/bin (claude,
;; claude-tmux, gh, ...) is invisible to `executable-find' and to every
;; subprocess Emacs starts.  Put it back, for this Emacs and its children.
(let ((bin (expand-file-name "~/.local/bin")))
  (when (file-directory-p bin)
    (add-to-list 'exec-path bin)
    (let ((path (or (getenv "PATH") "")))
      (unless (member bin (split-string path path-separator t))
        (setenv "PATH" (concat bin path-separator path))))))

(defun claude-code-theme-environment (&rest _)
  "Tell `claude-tmux' whether this Emacs is a light or a dark one.
It turns CLAUDE_TMUX_THEME into claude's own --settings theme, so a
solarized-dark Emacs gets a dark claude and a solarized-light one a light
claude instead of whatever theme was picked last."
  (list (format "CLAUDE_TMUX_THEME=%s"
                (if (eq (frame-parameter nil 'background-mode) 'dark)
                    "dark"
                  "light"))))

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

(defun claude-tmux--sessions ()
  "Return the background claude tmux sessions, most recently used first.

Each element is (NAME DIRECTORY ATTACHED), where ATTACHED says whether
some client -- an Emacs buffer or a terminal -- is currently viewing it."
  (mapcar
   #'cdr
   (sort
    (mapcar (lambda (line)
              (pcase-let ((`(,name ,dir ,attached ,activity) (split-string line "\t")))
                (list (string-to-number (or activity "0"))
                      name dir (not (equal attached "0")))))
            (split-string
             (shell-command-to-string
              (concat "tmux -L claude ls -F "
                      "'#{session_name}\t#{session_path}\t"
                      "#{session_attached}\t#{session_activity}' 2>/dev/null"))
             "\n" t))
    (lambda (a b) (> (car a) (car b))))))

(defun claude-tmux--read-session (prompt)
  "Read a background claude tmux session with PROMPT.
Returns the (NAME DIRECTORY ATTACHED) entry, annotated with the
directory the session was started in."
  (let* ((sessions (or (claude-tmux--sessions)
                       (user-error "No background claude sessions")))
         (width (apply #'max (mapcar (lambda (s) (length (car s))) sessions)))
         (table
          (lambda (string pred action)
            (if (eq action 'metadata)
                `(metadata
                  (category . claude-tmux-session)
                  ;; keep `claude-tmux--sessions' most-recent-first order
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

(defun claude-tmux-switch (session)
  "Attach to a background claude tmux SESSION in this Emacs.

Lists every session on the claude tmux server -- including ones started
from another Emacs, another machine's ssh connection, or a plain
terminal -- and re-attaches to the one you pick.  When this Emacs is
already showing that session, pop to its buffer instead of attaching a
second client to it."
  (interactive (list (claude-tmux--read-session "Claude session: ")))
  (require 'claude-code)
  (pcase-let* ((`(,name ,dir ,attached) session)
               (dir (and dir (file-name-as-directory dir)))
               (live (and attached dir (file-directory-p dir)
                          (claude-code--find-claude-buffers-for-directory dir))))
    (cond
     ((= 1 (length live)) (pop-to-buffer (car live)))
     (live (call-interactively #'claude-code-select-buffer))
     (t
      ;; claude-tmux re-attaches to CLAUDE_TMUX_SESSION when it exists, so name
      ;; the session explicitly rather than relying on it being derivable from
      ;; the directory (which may be gone, or shared by several sessions).
      (let* ((default-directory (if (and dir (file-directory-p dir))
                                    dir
                                  default-directory))
             (start-dir default-directory)
             (process-environment (cons (concat "CLAUDE_TMUX_SESSION=" name)
                                        process-environment)))
        (cl-letf (((symbol-function 'claude-code--directory) (lambda () start-dir)))
          (claude-code '(4))))))))

(defun claude-tmux-kill (session)
  "End the background claude tmux SESSION.

Killing a Claude buffer only detaches from tmux -- the claude process
keeps running so it can be re-attached.  Use this to actually stop it."
  (interactive (list (claude-tmux--read-session "End claude session: ")))
  (let ((name (if (consp session) (car session) session)))
    (call-process "tmux" nil nil nil "-L" "claude" "kill-session" "-t" name)
    (message "Ended claude session %s" name)))

(defun claude-wt--git (dir &rest args)
  "Run git with ARGS in DIR and return its output, trimmed.
Signal an error carrying git's own message when the command fails."
  (with-temp-buffer
    (let* ((default-directory (file-name-as-directory dir))
           (status (apply #'call-process "git" nil t nil args)))
      (unless (eq status 0)
        (user-error "git %s: %s" (string-join args " ")
                    (string-trim (buffer-string))))
      (string-trim (buffer-string)))))

(defun claude-wt--branch-p (dir branch)
  "Return non-nil when BRANCH already exists in the repository at DIR."
  (let ((default-directory (file-name-as-directory dir)))
    (eq 0 (call-process "git" nil nil nil "show-ref" "--verify" "--quiet"
                        (concat "refs/heads/" branch)))))

(defun claude-wt (name)
  "Start Claude on a fresh git worktree of this repository, named NAME.

Visiting ~/src/dir, NAME of \"feature-x\" puts the new branch feature-x
in ~/src/dir-feature-x and opens a Claude buffer there, so an agent can
work on its own checkout while ~/src/dir stays as you left it.

The branch is cut from the current HEAD.  An existing branch of that
name is checked out rather than recreated, and an existing worktree is
simply re-entered -- which, since Claude runs under tmux, re-attaches to
the session already living there."
  (interactive (list (read-string "Worktree/branch name: ")))
  (require 'claude-code)
  (let* ((name (string-trim name))
         (_ (when (string-empty-p name) (user-error "No name given")))
         (root (directory-file-name
                (claude-wt--git default-directory
                                "rev-parse" "--show-toplevel")))
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
     ((claude-wt--branch-p root name)
      (claude-wt--git root "worktree" "add" "--" worktree name))
     (t
      (claude-wt--git root "worktree" "add" "-b" name "--" worktree "HEAD")))
    (let* ((default-directory (file-name-as-directory worktree))
           (start-dir default-directory))
      (cl-letf (((symbol-function 'claude-code--directory) (lambda () start-dir)))
        (claude-code '(4))))))

(provide 'emacs-config)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; emacs-config.el ends here
;; Local Variables:
;; coding: utf-8-emacs
;; End:
