(defconst my/initial-gc-cons-threshold gc-cons-threshold
  "Initial value of `gc-cons-threshold' at start-up time.")
(setq gc-cons-threshold (* 128 1024 1024))
(add-hook 'after-init-hook
          (lambda () (setq gc-cons-threshold my/initial-gc-cons-threshold)))
(setq gnutls-min-prime-bits 4096)


;; Packages repositories

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))
(add-to-list 'load-path "~/.emacs.d/personal/packages/")
(package-initialize)


;; Initialise 'use-package and 'general
;; ==================================
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package)
  (require 'use-package-ensure)
  (setq use-package-always-ensure t))

(use-package quelpa
  :config
  :custom
  (quelpa-upgrade-interval 7)
  (quelpa-checkout-melpa-p nil)
  (quelpa-self-upgrade-p nil))

(use-package quelpa-use-package)

(use-package general)

(use-package key-chord)

(use-package restart-emacs)




;; Built-in settings
;; ================
(use-package my/settings
  :ensure nil
  ;; ===== INITIAL =====
  :init
  (setq
   locale-coding-system 'utf-8
   require-final-newline t
   column-number-mode t
   history-length 250
   tab-always-indent 'complete
   confirm-nonexistent-file-or-buffer nil
   vc-follow-symlinks nil
   recentf-max-saved-items 5000
   kill-ring-max 5000
   mark-ring-max 5000
   mouse-autoselect-window -0.1
   indicate-buffer-boundaries 'left
   split-height-threshold 110
   split-width-threshold 160
   show-paren-delay 0
   load-prefer-newer 5
   eval-expression-print-length nil
   eval-expression-print-level nil
   custom-file (expand-file-name "personal/custom.el" user-emacs-directory)
   abbrev-file-name (expand-file-name "personal/abbrev_defs" user-emacs-directory)
   inhibit-splash-screen t
   inhibit-startup-message t
   inhibit-default-init t
   mac-command-modifier 'meta
   mac-option-modifier 'super
   mac-control-modifier 'control
   auto-revert-verbose nil)
  (setq-default
   tab-width 4
   indent-tabs-mode nil
   c-basic-offset 4)

  ;; disable full 'yes' and 'no' answers
  (defalias 'yes-or-no-p 'y-or-n-p)

  ;; New shells shall spawn in side windows
  (add-to-list
   'display-buffer-alist
   '("*eshell" (display-buffer-in-side-window) (side . bottom)))

  (provide 'my/settings)

  ;; ===== CONFIGURATIONS =====
  :config

  ;; mode-line time display
  (setq display-time-24hr-format t
        display-time-day-and-date nil
        display-time-default-load-average nil
        display-time-use-mail-icon t)
  (display-time)

  ;; Display buffer size
  (size-indication-mode)

  ;; encoding
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-language-environment "UTF-8")
  (prefer-coding-system 'utf-8)

  ;; hide escape sequences (ANSI-colors) in compilation buffers
  (require 'ansi-color)
  (defun my/colorize-compilation ()
    "Colorize from `compilation-filter-start' to `point'."
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region
       compilation-filter-start (point))))

  (add-hook 'compilation-filter-hook #'my/colorize-compilation)

  ;; stop truncate lines in prog-mode
  (add-hook 'prog-mode-hook 'toggle-truncate-lines)

  ;; enable or disable modes
  (electric-pair-mode +1)
  (blink-cursor-mode +1)
  (put 'narrow-to-region 'disabled nil)

  ;; fonts - depending on the OS
  (cl-case system-type
    (darwin
     (set-frame-font "Hack Nerd Font" nil t)
     (mac-auto-operator-composition-mode)
     (custom-set-faces '(variable-pitch ((t (:height 1.2 :family "Avenir Next"))))))
    (gnu/linux
     (set-frame-font "Source Code Variable")
     (custom-set-faces '(variable-pitch ((t (:height 1.2 :family "Noto Sans"))))))
    (windows-nt
     (set-frame-font "Source Code Pro")
     (custom-set-faces '(variable-pitch ((t (:height 1.2 :family "DejaVu Sans")))))))

  ;; Help-mode
  (setq help-window-select 'other)

  :general
  ("C-x k" 'kill-this-buffer
   "C-/" 'undo-only))





(use-package better-defaults
  ;; Disables menu-bar, tool-bar and scroll-bar
  ;; Changes backup dir
  ;; Misc changes
  )

(use-package load-relative
  ;; Load ELisp file using relative location
  :pin gnu)

(use-package super-save
  ;; Autosave
  :config
  (super-save-mode +1)
  :custom
  (auto-save-default nil)
  (super-save-remote-files nil)
  (super-save-auto-save-when-idle t)
  (super-save-idle-duration 30))





;; Shell

(use-package exec-path-from-shell
  ;; Emacs exec paths should be same as shells
  :when (memq window-system '(mac ns x))
  :init
  (setq exec-path-from-shell-arguments nil)
  :config
  (exec-path-from-shell-initialize))

(use-package eshell
  :demand
  :hook ((eshell-mode . my-eshell-setup)
         (eshell-mode . eldoc-mode))
  :init
  (setq eshell-save-history-on-exit t)
  (setq eshell-destroy-buffer-when-process-dies t)
  (setq eshell-where-to-jump 'begin)
  (setq eshell-hist-ignoredups t)
  (setq eshell-history-size 10000)
  (defun my-eshell-setup ()
    (setq-local eldoc-idle-delay 3)
    (setenv "PAGER" "cat")
    (setenv "EDITOR" "emacsclient"))
  :config
  (use-package esh-help
    :config (setup-esh-help-eldoc))
  :general
  ("C-c s" 'eshell))





;; Version Control

(use-package magit
  :init
  (setq magit-no-confirm '(stage-all-changes))
  (setq magit-push-always-verify nil)
  (setq git-commit-finish-query-functions nil)
  (setq magit-save-some-buffers nil)    ;don't ask to save buffers
  (setq magit-set-upstream-on-push t)   ;ask to set upstream
  (setq magit-diff-refine-hunk 'all) ;show word-based diff for all hunks
  (setq magit-default-tracking-name-function
        'magit-default-tracking-name-branch-only) ;don't track with origin-*
  :general
  ("C-x g" 'magit-status
   "C-x M-g" 'magit-dispatch))

(use-package diff-hl
  :config
  (global-diff-hl-mode 1)
  (eval-after-load 'magit
    (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)))

(use-package autorevert
  :hook (dired-mode . auto-revert-mode)
  :config
  (global-auto-revert-mode 1))





;; Window

(use-package ace-window
  :custom
  (aw-scope 'frame)
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :general
  ("C-x o" 'ace-window)
  ("s-o" 'ace-window))

(use-package eyebrowse
  :init
  (setq eyebrowse-new-workspace t)
  :config
  (eyebrowse-mode t)
  :general
  ("M-1" 'eyebrowse-switch-to-window-config-1)
  ("M-2" 'eyebrowse-switch-to-window-config-2)
  ("M-3" 'eyebrowse-switch-to-window-config-3))

;; multi-frame management independent of window systems
(use-package frame
  :ensure nil
  :config
  (modify-all-frames-parameters '((width . 120)
                                  (height . 60)
                                  (alpha . (98. 90))))
  ;; better frame title
  (setq frame-title-format
        '((:eval (if (buffer-file-name)
                     (abbreviate-file-name (buffer-file-name))
                   "%b")))))

(use-package winner
  :config
  (winner-mode 1)
  :general
  (:prefix "C-c"
           "w n" 'winner-redo
           "w p" 'winner-undo))

(use-package shackle)




;; File, buffer and pointer navigation

(use-package ivy
  :init
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  :config
  (ivy-mode t)
  :general
  ("C-s" 'swiper
   "C-c C-r" 'ivy-resume))

(use-package counsel
  :config
  (counsel-mode t)
  :general
  ("M-x" 'counsel-M-x
   "C-x C-f" 'counsel-find-file
   "C-S-s" 'counsel-ag)
  (minibuffer-local-map
   "C-r" 'counsel-minibuffer-history))

(use-package swiper
  :general
  ("C-s" 'swiper))

(use-package avy
  ;; Jump to things in Emacs tree-style
  :general
  ("C-c g" 'avy-goto-char-timer
   "C-." 'avy-goto-char-in-line))





;; Project management

(use-package projectile
  :demand
  :init
  (setq projectile-switch-project-action 'projectile-dired)
  (setq projectile-enable-caching t)
  (setq projectile-mode-line '(:eval (format " <%s>" (projectile-project-name))))
  :config
  (setq projectile-completion-system 'ivy)
  :general
  ("C-c p" 'projectile-command-map))

(use-package counsel-projectile
  :config
  (counsel-projectile-mode))





;; Auto-completion

(use-package company
  :init
  (setq company-idle-delay 0.3
        company-tooltip-limit 20
        company-minimum-prefix-length 2)
  :hook (prog-mode . company-mode)
  :config
  (setq tab-always-indent 'complete)
  (defvar completion-at-point-functions-saved nil)

  (defun company-indent-for-tab-command (&optional arg)
    (interactive "P")
    (let ((completion-at-point-functions-saved completion-at-point-functions)
          (completion-at-point-functions '(company-complete-common-wrapper)))
      (indent-for-tab-command arg)))

  (defun company-complete-common-wrapper ()
    (let ((completion-at-point-functions completion-at-point-functions-saved))
      (company-complete-common)))

  (add-to-list 'company-backends 'company-dabbrev t)
  (add-to-list 'company-backends 'company-ispell t)
  (add-to-list 'company-backends 'company-files t)
  (add-to-list 'company-begin-commands 'outshine-self-insert-command)
  (setq company-backends (remove 'company-ropemacs company-backends))

  (defun my-company-elisp-setup ()
    (set (make-local-variable 'company-backends)
         '((company-capf :with company-dabbrev-code))))

  ;; Usage based completion sorting
  (use-package company-statistics
    :hook ((emacs-lisp-mode lisp-interaction-mode) . my-company-elisp-setup)
    :config (company-statistics-mode)))

;; Popup documentation for completion candidates
(use-package company-quickhelp
  :init
  (setq company-quickhelp-use-propertized-text t)
  (setq company-quickhelp-delay 1)
  :config
  (company-quickhelp-mode 1))





;; File organisation
(use-package dired
  :demand
  :ensure nil
  :init
  (setq dired-auto-revert-buffer t)
  (setq dired-listing-switches "-alh")
  (setq dired-no-confirm
        '(byte-compile chgrp chmod chown copy load move symlink))
  (setq dired-deletion-confirmer (lambda (x) t))
  :custom
  (dired-dwim-target t))

(use-package dired-hacks-utils)

(use-package dired-open)

(use-package dired-subtree
  :general
  (:keymaps 'dired-mode-map "TAB" 'dired-subtree-cycle))

(use-package dired-filter
  :general
  (:keymaps 'dired-mode-map
            "f" 'dired-filter-mode))

(use-package diredfl
  :hook (dired-mode . diredfl-global-mode))




;; Snippets

(use-package yasnippet
  :init
  (setq yas-snippet-dirs `(,(expand-file-name "personal/snippets" user-emacs-directory))))

(use-package auto-yasnippet)




;; General editing

(use-package expand-region
  :general
  ("C-@" 'er/expand-region
   "C-;" 'er/expand-region))

(use-package aggressive-indent
  ;; Aggressively indent lines
  :config
  (global-aggressive-indent-mode 1))

(use-package display-line-numbers
  ;; Display line numbers for programming languages
  :hook (prog-mode . display-line-numbers-mode))

(use-package outshine
  :commands outshine-hook-function
  :hook ((outline-minor-mode . outshine-mode)
         (emacs-lisp-mode . outline-minor-mode))
  :init
  (setq outshine-imenu-show-headlines-p nil))

(use-package highlight-thing
  :hook (prog-mode . highlight-thing-mode)
  :custom
  (highlight-thing-limit-to-defun t)
  (highlight-thing-exclude-thing-under-point t))

(use-package format-all
  :hook (prog-mode . format-all-mode))

(use-package hungry-delete
  :config
  (global-hungry-delete-mode))

(use-package flycheck
  :hook (emacs-lisp-mode . flycheck-mode))





;; Emacs Lisp

(use-package lispy
  :hook
  (emacs-lisp-mode . lispy-mode )
  :config
  (defun my/conditionally-enable-lispy ()
    (when (eq this-command 'eval-expression)
      (lispy-mode 1)))
  (add-hook 'minibuffer-setup-hook 'my/conditionally-enable-lispy)
  :general
  (:keymaps 'lispy-mode-map
            "\"" 'lispy-doublequote))

(use-package elisp-refs)

(use-package elisp-slime-nav
  :hook ((emacs-lisp-mode ielm-mode) . elisp-slime-nav-mode))

(use-package highlight-defined
  :hook (emacs-lisp-mode . highlight-defined-mode))

(use-package rainbow-delimiters
  :hook (emacs-lisp-mode . rainbow-delimiters-mode))

(use-package eldoc
  :hook (emacs-lisp-mode . eldoc-mode)
  :hook (lisp-interaction-mode-hook . eldoc-mode))

(use-package page-break-lines
  ;; Display ^L (C-q C-l) page breaks as tidy horizontal lines
  :config
  (global-page-break-lines-mode))

(use-package eval-sexp-fu
  :init
  (setq eval-sexp-fu-flash-duration 0.4)
  :config
  (turn-on-eval-sexp-fu-flash-mode)
  :general
  (:keymaps 'emacs-lisp-mode-map
            "C-c C-c" 'eval-sexp-fu-eval-sexp-inner-list
            "C-c C-e" 'eval-sexp-fu-eval-sexp-inner-sexp)
  (:keymaps 'lisp-interaction-mode-map
            "C-c C-c" 'eval-sexp-fu-eval-sexp-inner-list
            "C-c C-e" 'eval-sexp-fu-eval-sexp-inner-sexp))

(use-package eros
  :hook (emacs-lisp-mode . eros-mode )
  :hook (lisp-interaction-mode . eros-mode))

(use-package ielm
  :config
  (add-hook 'inferior-emacs-lisp-mode-hook
            (lambda ()
              (turn-on-eldoc-mode))))

(use-package ipretty
  :config (ipretty-mode t))





;; Haskell

(use-package haskell-mode
  :custom
  (haskell-process-type 'stack-ghci)
  (haskell-stylish-on-save t))

(use-package dante
  :after haskell-mode
  :hook (haskell-mode . dante-mode)
  :config
  (add-to-list dante-methods-alist '(stack-ghci "stack ghci") t))




;; Elm

(use-package elm-mode
  :disabled
  :init
  (setq elm-tags-on-save t))





;; Json

(use-package json-mode
  :mode "\\.json\\'")





;; Documents

(use-package pdf-tools
  :init
  (pdf-loader-install)
  :config
  (setq pdf-view-use-scaling t)
  :bind
  (:map pdf-view-mode-map
        ("k" . pdf-view-previous-line-or-previous-page)
        ("j" . pdf-view-next-line-or-next-page)))





;; Org-mode

(load-relative "config-org.el")





;; References

(use-package helpful
  :general
  ("C-h f" 'helpful-callable
   "C-h k" 'helpful-key
   "C-h v" 'helpful-variable))

(use-package which-key
  :custom
  (which-key-show-docstrings 'docstring-only)
  (which-key-max-description-length nil)
  (which-key-side-window-max-height 0.75)
  (which-key-idle-delay 2.0)
  :config
  (which-key-mode))

(use-package discover-my-major
  :general
  ("C-h C-m" 'discover-my-major))





;; Load custom.el
;; Don't load for now
(load custom-file)





;; Theme

(use-package zenburn-theme
  :config
  (load-theme 'zenburn t)
  :custom-face
  (mode-line-buffer-id ((t (:height 1.0 :family "Optima"))))
  (default ((t (:family "Fira Code"))))
  (org-document-info-keyword ((t (:height 0.8 :inherit shadow))))
  (org-document-title ((t (:height 2.0 :weight normal :foreground "#8CD0D3" :family "Optima"))))
  (org-drawer ((t (:height 0.8 :foreground "LightSkyBlue"))))
  (org-meta-line ((t (:height 0.8 :inherit shadow))))
  (org-special-keyword ((t (:height 0.8))))
  (org-ellipsis ((t (:underline nil :height 0.5))))
  (org-level-1 ((t (:height 1.2 :weight medium :inherit outline-1))))
  (org-level-2 ((t (:height 1.1 :inherit outline-2))))
  (org-level-3 ((t (:height 1.1 :inherit outline-3))))
  (org-level-4 ((t (:height 1.0 :inherit outline-4))))
  (org-level-5 ((t (:height 1.0 :inherit outline-5))))
  (org-level-6 ((t (:height 1.0 :inherit outline-6))))
  (org-level-7 ((t (:height 1.0 :inherit outline-7))))
  (org-level-8 ((t (:height 1.0 :inherit outline-8))))
  (org-block ((t (:height 0.8))))
  (org-checkbox-statistics-todo ((t (:height 0.8 :inherit (org-todo)))))
  (org-checkbox-statistics-done ((t (:height 0.8 :inherit (org-done)))))
  (org-tag ((t (:height 0.7))))
  (org-table ((t (:foreground "#9FC59F" :height 0.8))))
  (org-done ((t (:foreground "LightGreen")))))

(use-package smart-mode-line
  :config
  (sml/setup)
  :custom
  (sml/theme 'dark)
  (sml/name-width 40)
  (sml/mode-width 20)
  (sml/shorten-modes t)
  (sml/vc-mode-show-backend t)
  (sml/no-confirm-load-theme t)
  (rm-whitelist '())
  (rm-blacklist '(" ElDoc" " super-save" " ivy" " counsel" " company" " PgLn" " BufFace" " anki-editor"
                  " Outl" " WK" " FmtAll" " hlt" " Rbow" " SliNav" " h" " =>" " Hi" " OVP" " Ind" " yas"))
  (sml/extra-filler 0)
  (rm-text-properties '(("Outshine" 'display " 🌻")
                        ("Org-roam" 'display " 🏄🏼‍♂️"))))

(use-package nyan-mode
  :disabled
  :config
  (nyan-mode 1)
  :custom
  (nyan-animate-nyancat nil)
  (nyan-wavy-trail nil)
  (nyan-minimum-window-width 125))

(use-package shr
  ;; increase contrast between similar colors
  :custom
  (shr-color-visible-luminance-min 60))

(use-package hl-line
  :hook ((dired-mode package-menu-mode prog-mode) . hl-line-mode))

(use-package rainbow-mode
  :hook (emacs-lisp-mode css-mode js-mode text-mode))

(use-package all-the-icons)
(use-package all-the-icons-ivy
  :init
  (all-the-icons-ivy-setup))
