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

(when (eq system-type 'windows-nt)
  (setenv "NODE_TLS_REJECT_UNAUTHORIZED" "0")
  (when (file-exists-p "C:/Progra~1/Git/usr/bin")
    (add-to-list 'exec-path "C:\\Progra~1\\Git\\usr\\bin"))
  (let* ((git-path (concat "C:/Users/"
                         (downcase (user-login-name))
                         "/AppData/Local/Programs/Git/bin"))
         (git-win-path (replace-regexp-in-string "[/]" "\\\\" git-path)))
    (when (file-exists-p git-path)
      (add-to-list 'exec-path git-win-path)
      (setenv "PATH" (concat git-win-path ";" (getenv "PATH")))))

  (when (file-exists-p "C:/R/Rstudio/bin/gnugrep")
    (add-to-list 'exec-path "C:\\R\\Rstudio\\bin\\gnugrep"))
  (when (file-exists-p "c:/R/hunspell/bin")
    (add-to-list 'exec-path "c:\\R\\hunspell\\bin")
    (setq ispell-program-name "c:\\R\\hunspell\\bin\\hunspell.exe"))

  ;; (when (file-exists-p "c:/Rtools43/usr/bin")
  ;;   (add-to-list 'exec-path "c:\\Rtools43\\usr\\bin"))


  (when (file-exists-p "c:/R/R-4.3.0/bin/x64")
    (add-to-list 'exec-path "c:\\R\\R-4.3.0\\bin\\x64"))

  (when (file-exists-p "c:/Progra~1/R/R-4.4.0/bin/x64")
    (add-to-list 'exec-path "c:\\Progra~1\\R\\R-4.4.0\\bin\\x64"))


  (when (file-exists-p "C:/Program Files/RStudio/resources/app/bin/quarto")
    (add-to-list 'exec-path "C:\\Program Files\\RStudio\\resources\\app\\bin\\quarto"))

  (when (file-exists-p "C:/Program Files/RStudio/resources/app/bin/node")
    (add-to-list 'exec-path "C:\\Program Files\\RStudio\\resources\\app\\bin\\node")
    (setq copilot-node-executable "C:\\Program Files\\RStudio\\resources\\app\\bin\\node\\node.exe"))

  (when (file-exists-p "C:/Program Files/nodejs")
    (add-to-list 'exec-path "C:\\Program Files\\nodejs")
    (setq copilot-node-executable "C:\\Program Files\\nodejs\\node.exe")
    (setenv "PATH" (concat "\"C:\\Program Files\\nodejs\\\";" (getenv "PATH"))))

  (let ((rstudio-bin-1 "C:/R/Rstudio/bin/")
        (rstudio-bin-2 "C:\\R\\Rstudio\\bin\\"))
    (when (file-exists-p (concat rstudio-bin-1 "gnugrep"))
      (add-to-list 'exec-path (concat rstudio-bin-2 "gnugrep")))
    (when (file-exists-p (concat rstudio-bin-1 "gnudiff"))
      (add-to-list 'exec-path (concat rstudio-bin-2 "gnudiff")))
    (when (file-exists-p (concat rstudio-bin-1 "quarto/bin"))
      (add-to-list 'exec-path (concat rstudio-bin-2 "quarto\\bin")))))






(when (file-exists-p "~/src/org-mode")
  (add-to-list 'load-path "~/src/org-mode")
  (require 'org))

(unless (file-exists-p "c:/WINDOWS/System32/WindowsPowerShell/v1.0/powershell.exe")
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
  :config
  (setq nerd-icons-font-family "Symbols Nerd Font Mono"))

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
  :if (display-graphic-p))

(use-package doom-modeline
  :ensure t
  :config
  (doom-modeline-mode 1))

(use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
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
  :config
  (nerd-icons-completion-mode 1)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package treemacs-nerd-icons
  :config
  (treemacs-load-theme "nerd-icons"))

(use-package nerd-icons-ibuffer
  :ensure t
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

(use-package page-break-lines
  :ensure t
  :config
  (global-page-break-lines-mode t))

(use-package projectile
  :ensure t
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-center-content t)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-set-navigator t)
  (setq dashboard-items '((recents  . 5)
                          (bookmarks . 5)
                          (projects . 5)
                          (agenda . 5)
                          (registers . 5)))
  (setq dashboard-set-init-info t)
  (setq dashboard-set-footer nil)
  (setq dashboard-set-navigator t)
  (setq dashboard-navigator-buttons
        `(((,(all-the-icons-faicon "home" :height 1.1 :v-adjust 0.0)
            "Custom home"
            "Browse the web"
            (lambda (&rest _) (browse-url "https://google.com")))
           (,(all-the-icons-faicon "book" :height 1.1 :v-adjust 0.0)
            "Info"
            "Browse the web"
            (lambda (&rest _) (browse-url "https://google.com")))
           (,(all-the-icons-faicon "cog" :height 1.1 :v-adjust 0.0)
            "Settings"
            "Browse the web"
            (lambda (&rest _) (browse-url "https://google.com")))))))


(use-package dirvish
  :init
  (dirvish-override-dired-mode)
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
  (dirvish-peek-mode) ; Preview files in minibuffer
  (dirvish-side-follow-mode) ; similar to `treemacs-follow-mode'
  )


(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(setq use-package-always-ensure t)

(eval-when-compile
  (require 'use-package))

(if (file-exists-p "~/src/ergoemacs-mode")
    (add-to-list 'load-path "~/src/ergoemacs-mode")
  (add-to-list 'load-path "~/.emacs.d/ergoemacs-mode"))

(require 'ergoemacs-mode)

(use-package treemacs
  :ensure t
  :config
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode t)
  (treemacs-git-mode 'deferred)
  (treemacs-resize-icons 10)
  (treemacs-indentation 1)
  (treemacs-show-hidden-files t)
  (treemacs-silent-refresh t)
  (treemacs-silent-filewatch t)
  (treemacs-width 22)
  (treemacs-position 'left)
  (treemacs-follow-after-init t)
  (treemacs-is-never-other-window t)
  (treemacs-no-png-images t)
  (treemacs-no-delete-other-windows t)
  (treemacs-missing-project-action 'ask)
  (treemacs-recenter-after-project-expand 'on-distance)
  (treemacs-recenter-after-file-follow 'on-distance)
  (treemacs-recenter-after-tag-follow 'on-distance)
  (treemacs-recenter-after-project-jump 'on-distance)
  (treemacs-recenter-after-project-collapse 'on-distance)
  (treemacs-recenter-after-file-follow 'on-distance)
  (treemacs-recenter-after-tag-follow 'on-distance)
  (treemacs-recenter-after-project-jump 'on-distance)
  (treemacs-recenter-after-project-collapse 'on-distance)
  (treemacs-recenter-after-project-expand 'on-distance)
  (treemacs-recenter-after-project-collapse 'on-distance)
  (treemacs-recenter-after-file-follow 'on-distance)
  (treemacs-recenter-after-tag-follow 'on-distance)
  (treemacs-recenter-after-project-jump 'on-distance)
  (treemacs-recenter-after-project-collapse 'on-distance)
  (treemacs-recenter-after-project-expand 'on-distance)
  (treemacs-recenter-after-project-collapse 'on-distance)
  (treemacs-recenter-after-file-follow 'on-distance)
  (treemacs-recenter-after-tag-follow 'on-distance)
  (treemacs-recenter-after-project-jump 'on-distance)
  (treemacs-recenter-after-project-collapse 'on-distance)
  (treemacs-recenter-after-project-expand 'on-distance)
  (treemacs-recenter-after-project-collapse 'on-distance)
  (treemacs-recenter-after-file-follow 'on-distance)
  (treemacs-recenter-after-tag-follow 'on-distance)
  (treemacs-recenter-after-project-j)
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

(when (file-exists-p "/usr/local/share/emacs/site-lisp/mu4e")
  (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")
  (require 'mu4e)
  (setq mu4e-maildir "~/.mail/gmail"
        message-send-mail-function 'smtpmail-send-it
        message-citation-line-format "On %a, %b %d %Y, %f wrote:\n"
        message-kill-buffer-on-exit t
        mu4e-use-fancy-chars t
        mu4e-view-prefer-html t
        mu4e-view-show-images t
        mu4e-view-show-addresses t
        mu4e-view-image-max-width 800
        mu4e-attachment-dir "~/Downloads"
        mu4e-change-filenames-when-moving t
        mu4e-compose-signature-auto-include nil
        mu4e-drafts-folder "/[Gmail].Drafts"
        mu4e-get-mail-command "mbsync -a"
        mu4e-headers-skip-duplicates t
        mu4e-index-cleanup t
        mu4e-index-lazy-check nil
        message-sendmail-envelope-from 'header
        smtpmail-smtp-service 587
        mu4e-refile-folder "/[Gmail].All Mail"
        mu4e-sent-folder "/[Gmail].Sent Mail"
        mu4e-trash-folder "/[Gmail].Trash"
        mu4e-update-interval (* 6 60 60)
        mu4e-update-interval 300
        mu4e-view-show-addresses t
        mu4e-view-show-images t
        smtpmail-smtp-server "smtp.gmail.com"
        smtpmail-smtp-service 587
        smtpmail-stream-type 'starttls
        user-full-name "Matthew L. Fidler"
        user-mail-address "matthew.fidler@gmail.com"
        smtpmail-smtp-user "matthew.fidler@gmail.com")
  (ergoemacs-define-key ergoemacs-override-keymap (kbd "<apps>")  'mu4e (kbd "p")))


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

(use-package indent-guide
  :ensure t
  :config
  (indent-guide-global-mode 1))

(when (version<  "24.4" emacs-version)
  (use-package page-break-lines
    :ensure t
    :config
    (global-page-break-lines-mode t)))

(use-package volatile-highlights
  :ensure t
  :config
  (volatile-highlights-mode))

(use-package tabbar
  :ensure t)

(use-package tabbar-ruler
  :ensure t
  (setq tabbar-ruler-global-tabbar t ; If you want tabbar
        ;;tabbar-ruler-global-ruler t ; if you want a global ruler
        ;;tabbar-ruler-popup-menu nil ; If you want a popup menu.
        ;;tabbar-ruler-popup-toolbar nil ; If you want a popup toolbar
        ;;tabbar-ruler-popup-scrollbar nil
        ;; tabbar-ruler-style 'firefox-circle
        ) ; Popup scrollbar
  (require 'tabbar)
  (require 'tabbar-ruler))

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
      :commands (magit-status)
      ;; (add-hook 'magit-mode-hook #'turn-on-magit-gh-pulls)
      )
      (use-package magit-file-icons
        :ensure t
        :after magit
        :init
        (magit-file-icons-mode 1)
        :custom
        ;; These are the default values:
        (magit-file-icons-enable-diff-file-section-icons t)
        (magit-file-icons-enable-untracked-icons t)
        (magit-file-icons-enable-diffstat-icons t))
      (use-package forge
        :after magit))
  (when (file-exists-p "~/.emacs.d/magit")
    (add-to-list 'load-path "~/.emacs.d/magit")
    (require 'magit)))

(when (display-graphic-p)
  (if (version< "24.4" emacs-version)
      (use-package solarized-theme
        :ensure t
        :config
        ;; Dark is for remote sessions, light is for local sessions.
        (if (or (getenv "SSH_CONNECTION") (getenv "SSH_CLIENT"))
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
        )
    (when (file-exists-p "~/.emacs.d/emacs-color-theme-solarized")
      (add-to-list 'load-path "~/.emacs.d/emacs-color-theme-solarized")
      (add-to-list 'custom-theme-load-path "~/.emacs.d/emacs-color-theme-solarized")
      (load-theme 'solarized t)))
  )

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
  (ess-toggle-underscore nil)
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

;;(ergoemacs-define-key ergoemacs-user-keymap (kbd "<menu> n") 'R (kbd "r"))


(add-hook 'inferior-ess-mode-hook
          (lambda ()
            (ansi-color-for-comint-mode-on)))

(ergoemacs-mode 1)

(use-package quarto-mode
  :mode (("\\.qmd" . poly-quarto-mode)))



(use-package shell-maker)
(use-package dall-e-shell
  :after (shell-maker)
  :config
  (require 'dall-e-shell))

;; does not work work proxy yet
(use-package copilot-chat
  :quelpa (copilot-chat
           :fetcher github
           :repo "chep/copilot-chat.el"
           :branch "master"
           :files ("*.el"))
  :after (request shell-maker)
  :custom
  (copilot-chat-frontend 'shell-maker)
  :config
  (require 'copilot-chat-shell-maker)
  (when (file-exists-p "c:/rtools44/mingw64/bin/curl.exe")
    (setq copilot-chat-curl-program "c:/rtools44/mingw64/bin/curl.exe")
    (setq copilot-chat-curl-proxy-insecure t)
    (setq copilot-chat-curl-proxy "http://na-usi1-proxy-user.na.novartis.net:2011"))
  (push '(shell-maker . copilot-chat-shell-maker-init) copilot-chat-frontend-list)
  (copilot-chat-shell-maker-init)
  (define-key ergoemacs-user-keymap (kbd "<menu> n") 'copilot-chat)
  (define-key ergoemacs-user-keymap (kbd "<apps> n") 'copilot-chat)
  ;; (require 'copilot-chat-org)
  (defun copilot-chat-roxygen2()
    "ask copilot to describe the code using roxygen2."
    (interactive)
    (let ((code (buffer-substring-no-properties (region-beginning) (region-end))))
      (with-current-buffer (copilot-chat-get-shell-buffer)
        (insert (concat "Would you please describe the following code using roxygen2 and use @author Matthew L. Fidler:\n" code))
        (shell-maker-submit))))

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
  (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion))

(provide 'emacs-config)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; emacs-config.el ends here
;; Local Variables:
;; coding: utf-8-emacs
;; End:
