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
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)
(eval-when-compile
  (require 'use-package))

(if (file-exists-p "~/src/ergoemacs-mode")
    (add-to-list 'load-path "~/src/ergoemacs-mode")
  (add-to-list 'load-path "~/.emacs.d/ergoemacs-mode"))

(use-package ergoemacs-mode
  :init
  (setq ergoemacs-theme "reduction"
        ergoemacs-keyboard-layout "colemak"
        ergoemacs-beginning-or-end-of-line-and-what 'page
        ergoemacs-smart-paste t))

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
    (use-package company
      :ensure t
      :init
      (add-hook 'after-init-hook 'global-company-mode))
  (when (file-exists-p "~/.emacs.d/company-mode")
    (add-to-list 'load-path "~/.emacs.d/company-mode")
    (require 'company)
    (add-hook 'after-init-hook 'global-company-mode)))

(setq set-mark-command-repeat-pop t)

(savehist-mode 1)

(global-linum-mode 1)
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

(global-visual-line-mode 1)

(global-hl-line-mode 1)

(modify-all-frames-parameters (list (cons 'cursor-type 'bar)))

                                        ;(setq pop-up-frames 'graphic-only)

(if (version< "24.4" emacs-version)
  (use-package tabbar-ruler
    :ensure t
    :init
    (setq tabbar-ruler-global-tabbar t ; If you want tabbar
          ;;tabbar-ruler-global-ruler t ; if you want a global ruler
          ;;tabbar-ruler-popup-menu nil ; If you want a popup menu.
          ;;tabbar-ruler-popup-toolbar nil ; If you want a popup toolbar
          ;;tabbar-ruler-popup-scrollbar nil
          ;; tabbar-ruler-style 'firefox-circle
	      ) ; Popup scrollbar
    )
  (when (file-exists-p "~/.emacs.d/tabbar")
    (add-to-list 'load-path "~/.emacs.d/tabbar")
    (use-package tabbar))
  (when (file-exists-p "~/.emacs.d/mode-icons")
    (add-to-list 'load-path "~/.emacs.d/mode-icons")
    (require 'mode-icons))
  (when (file-exists-p "~/.emacs.d/tabbar-ruler.el")
    (add-to-list 'load-path "~/.emacs.d/tabbar-ruler.el")
    (use-package tabbar-ruler
      :init
      (setq tabbar-ruler-global-tabbar t ; If you want tabbar
            ;;tabbar-ruler-global-ruler t ; if you want a global ruler
            ;;tabbar-ruler-popup-menu nil ; If you want a popup menu.
            ;;tabbar-ruler-popup-toolbar nil ; If you want a popup toolbar
            ;;tabbar-ruler-popup-scrollbar nil
            ;; tabbar-ruler-style 'firefox-circle
	        ) ; Popup scrollbar
      )))

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

(use-package linum-off
  :ensure t
  :config
  (global-linum-mode 1))

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
    (use-package magit
      :ensure t
      :commands (magit-status)
      ;; (add-hook 'magit-mode-hook #'turn-on-magit-gh-pulls)
      )
  (when (file-exists-p "~/.emacs.d/magit")
    (add-to-list 'load-path "~/.emacs.d/magit")
    (require 'magit)))

(if (version< "24.4" emacs-version)
    (use-package solarized-theme
      :ensure t
      :config
      (load-theme 'solarized-light t))
  ;; (when (file-exists-p "~/.emacs.d/solarized-emacs")
  ;;   (add-to-list 'load-path "~/.emacs.d/solarized-emacs")
  ;;   (add-to-list 'custom-theme-load-path "~/.emacs.d/solarized-emacs")
  ;;   (load-theme 'solarized-light t))
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
  (ergoemacs-define-key ergoemacs-user-keymap (kbd "M-,") 'avy-goto-word-or-subword-1))

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

(use-package ess-site
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
                    ess-ask-for-ess-directory nil
                    ess-indent-level 2
                    ess-local-process-name "R"
                    ansi-color-for-comint-mode 'filter
                    comint-scroll-to-bottom-on-input t
                    comint-scroll-to-bottom-on-output t
                    comint-move-point-for-output t
                    ess-nuke-trailing-whitespace-p t
                    ess-roxy-str "#'"
                    inferior-R-args "--no-save --quiet"
                    ess-insert-assign nil
                    ess-user-full-name "Matthew L. Fidler"
                    inferior-R-args "--no-save --quiet"
                    ess-roxy-template-alist
                    ;; (list (cons "description"  " ")
                    ;;       (cons "details" " ")
                    ;;       (cons "param"  "")
                    ;;       (cons "return" "")
                    ;;       (cons "author" ess-user-full-name)
                    ;;       (cons "examples" ""))
                    )))

  (defun myindent-ess-hook ()
  (setq ess-indent-level 2)
  (setq ess-offset-arguments-newline '(prev-line 2))
)
(add-hook 'ess-mode-hook 'myindent-ess-hook)
  (add-hook 'ess-mode-hook
	        (lambda()
		      (ess-set-style 'RStudio 'quiet)
		      (add-hook 'local-write-file-hooks
                        (lambda ()
                          (ess-nuke-trailing-whitespace)))
              (ess-roxy-mode 1)
		      (electric-operator-mode)
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

    (add-hook 'inferior-ess-mode-hook
              (lambda()
                (ansi-color-for-comint-mode-on)))

    ;;Remove ESS smart underscore
    (ess-toggle-underscore nil))

(when (version< "24.4" emacs-version)
  (use-package poly-R
    :ensure t)

  (use-package poly-markdown
    :mode ("\\.[Rr][mM][Dd][Hh]\\'"         . poly-markdown+r-mode)
    :ensure t)

  (use-package flycheck
    :config
    (global-flycheck-mode 1))

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

(ergoemacs-define-key ergoemacs-user-keymap (kbd "<menu> n") 'R (kbd "r"))

(when (file-exists-p "C:/R/Rstudio/bin/gnugrep")
  (add-to-list 'exec-path "C:\\R\\Rstudio\\bin\\gnugrep"))
(when (file-exists-p "c:/R/hunspell/bin")
  (add-to-list 'exec-path "c:\\R\\hunspell\\bin")
  (setq ispell-program-name "c:\\R\\hunspell\\bin\\hunspell.exe"))

(let ((rstudio-bin-1 "C:/R/Rstudio/bin/")
      (rstudio-bin-2 "C:\\R\\Rstudio\\bin\\"))
  (when (file-exists-p (concat rstudio-bin-1 "gnugrep"))
    (add-to-list 'exec-path (concat rstudio-bin-2 "gnugrep")))
  (when (file-exists-p (concat rstudio-bin-1 "gnudiff"))
    (add-to-list 'exec-path (concat rstudio-bin-2 "gnudiff")))
  (when (file-exists-p (concat rstudio-bin-1 "quarto/bin"))
    (add-to-list 'exec-path (concat rstudio-bin-2 "quarto\\bin")))
  )

(when (file-exists-p "c:/R/R-4.2.1/bin/x64")
  (add-to-list 'exec-path "c:\\R\\R-4.2.1\\bin\\x64"))

(ergoemacs-mode 1)
(provide 'emacs-config)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; emacs-config.el ends here
;; Local Variables:
;; coding: utf-8-emacs
;; End:

