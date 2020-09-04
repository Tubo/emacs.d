(defconst my/initial-gc-cons-threshold gc-cons-threshold
  "Initial value of `gc-cons-threshold' at start-up time.")
(setq gc-cons-threshold (* 128 1024 1024))
(add-hook 'after-init-hook
          (lambda () (setq gc-cons-threshold my/initial-gc-cons-threshold)))
(setq gnutls-min-prime-bits 4096)




;; Initialise 'use-package and 'general

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-use-package-by-default t)
(straight-use-package 'use-package)
(add-to-list 'load-path "~/.emacs.d/config/packages/")

(use-package general)
(use-package key-chord)
(use-package restart-emacs)




;; Built-in settings

(use-package my/settings
  :straight nil
  :init
  (setq
   locale-coding-system 'utf-8
   require-final-newline t
   column-number-mode t
   history-length 250
   tab-always-indent 'complete
   confirm-nonexistent-file-or-buffer nil
   vc-follow-symlinks nil
   vc-handled-backends '(Git Hg)
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
   abbrev-file-name (expand-file-name "config/abbrev_defs" user-emacs-directory)
   inhibit-splash-screen t
   inhibit-startup-message t
   inhibit-default-init t
   mac-command-modifier 'meta
   mac-option-modifier 'super
   mac-control-modifier 'control
   hi-lock-file-patterns-policy #'(lambda (_) t))

  (setq
   ;; Vertical Scroll
   scroll-step 1
   scroll-margin 1
   scroll-conservatively 101
   scroll-up-aggressively 0.01
   scroll-down-aggressively 0.01
   auto-window-vscroll nil
   fast-but-imprecise-scrolling nil
   mouse-wheel-scroll-amount '(1 ((shift) . 1))
   mouse-wheel-progressive-speed nil
   ;; Horizontal Scroll
   hscroll-step 1
   hscroll-margin 1)

  (setq-default
   tab-width 4
   indent-tabs-mode nil
   c-basic-offset 4
   compilation-always-kill t
   compilation-ask-about-save nil
   compilation-scroll-output t)

  ;; Disable full 'yes' and 'no' answers
  (defalias 'yes-or-no-p 'y-or-n-p)

  (provide 'my/settings)

  :config
  ;; Mode-line time display
  (setq display-time-24hr-format t
        display-time-day-and-date nil
        display-time-default-load-average nil
        display-time-use-mail-icon t)
  (display-time)

  ;; Display buffer size
  (size-indication-mode 1)

  ;; Encoding
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-language-environment "UTF-8")
  (prefer-coding-system 'utf-8)

  ;; Hide escape sequences (ANSI-colors) in compilation buffers
  (require 'ansi-color)
  (defun my/colorize-compilation ()
    "Colorize from `compilation-filter-start' to `point'."
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region
       compilation-filter-start (point))))
  (add-hook 'compilation-filter-hook #'my/colorize-compilation)

  ;; Truncate lines in prog-mode
  (add-hook 'prog-mode-hook (lambda () (toggle-truncate-lines +1)))
  ;; Enable or disable modes
  (electric-pair-mode +1)
  (blink-cursor-mode +1)
  (put 'narrow-to-region 'disabled nil)

  ;; Help-mode
  (setq help-window-select 'other)

  ;; Fonts - depending on the OS
  (cl-case system-type
    (darwin
     (add-to-list 'default-frame-alist '(font . "Hack Nerd Font-12"))
     (mac-auto-operator-composition-mode t)
     (custom-set-faces '(variable-pitch ((t (:height 1.2 :family "Avenir Next"))))))
    (gnu/linux
     (set-frame-font "Noto Mono Nerd Font")
     (custom-set-faces '(variable-pitch ((t (:height 1.0 :family "Noto Sans Nerd Font"))))))
    (windows-nt
     (set-frame-font "Source Code Pro")
     (custom-set-faces '(variable-pitch ((t (:height 1.2 :family "DejaVu Sans")))))))

  :general
  ("C-c c" 'calendar
   "C-x c" 'calc
   "C-x k" 'kill-this-buffer
   "C-x K" 'kill-buffer-and-window
   "C-/" 'undo-only))




(use-package better-defaults
  ;; Disables menu-bar, tool-bar and scroll-bar
  ;; Changes backup dir
  ;; Misc changes
  )

(use-package load-relative
  ;; Load ELisp file using relative location
  )

(use-package super-save
  ;; Autosave
  :config
  (super-save-mode +1)
  :custom
  (auto-save-default nil)
  (super-save-remote-files nil)
  (super-save-auto-save-when-idle t)
  (super-save-idle-duration 30))

(use-package crux
  :config
  (defalias 'rename-file-and-buffer #'crux-rename-file-and-buffer)
  (crux-with-region-or-buffer indent-region)
  (crux-with-region-or-buffer untabify)
  (crux-with-region-or-point-to-eol kill-ring-save)
  :general
  ("C-a" 'crux-move-beginning-of-line
   "C-c o" 'crux-open-with
   [(shift return)] 'crux-smart-open-line
   "C-k" 'crux-smart-kill-line))





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

  (add-to-list
   'display-buffer-alist
   '("*eshell" (display-buffer-in-side-window) (side . bottom)))

  (defun my-eshell-setup ()
    (setq-local eldoc-idle-delay 3)
    (setenv "PAGER" "cat")
    (setenv "EDITOR" "emacsclient"))
  :config
  (use-package esh-help
    :config (setup-esh-help-eldoc))
  :general
  ("C-c s" 'eshell))

(use-package vterm
  :disabled t)




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
  (global-auto-revert-mode 1)
  :custom
  (auto-revert-verbose nil)
  (auto-revert-use-notify nil))




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


(use-package frame
  ;; Multi-frame management independent of window systems
  :straight nil
  :config
  (modify-all-frames-parameters '((width . 120)
                                  (height . 0.8)
                                  (alpha . (98. 90))))
  ;; Better frame title
  (setq frame-title-format
        '((:eval (if (buffer-file-name)
                     (abbreviate-file-name (buffer-file-name))
                   "%b")))))

(use-package winner
  :config
  (winner-mode 1)
  :custom
  (winner-dont-bind-my-keys t)
  (winner-boring-buffers
   '("*Completions*"
     "*Compile-Log*"
     "*inferior-lisp*"
     "*Fuzzy Completions*"
     "*Apropos*"
     "*Help*"
     "*cvs*"
     "*Buffer List*"
     "*Ibuffer*"
     "*esh command on file*"))
  :general
  ("s-n" 'winner-redo
   "s-p" 'winner-undo))

(use-package windmove
  :straight nil
  :general
  ("s-i" 'windmove-up
   "s-k" 'windmove-down
   "s-j" 'windmove-left
   "s-l" 'windmove-right))


(use-package shackle)




;; File, buffer and pointer navigation

(use-package ivy
  :config
  (ivy-mode 1)
  :custom
  (ivy-use-virtual-buffers t)
  (enable-recursive-minibuffers t)
  :general
  ("C-c C-r" 'ivy-resume
   "C-x B" 'ivy-switch-buffer-other-window)
  (:keymaps 'ivy-minibuffer-map
            "M-RET" 'ivy-immediate-done))

(use-package ivy-hydra
  :after ivy)

(use-package amx)

(use-package counsel
  :after ivy
  :config
  (counsel-mode 1)
  :general
  ("C-x C-f" 'counsel-find-file
   "C-x d" 'counsel-dired
   "C-S-s" 'counsel-ag)
  (minibuffer-local-map
   "C-r" 'counsel-minibuffer-history))

(use-package swiper
  :after ivy
  :general
  ("C-s" 'swiper)
  ("C-r" 'swiper))

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









;; File organisation

(use-package dired
  :demand
  :straight nil
  :hook (dired-mode . (lambda () (setq truncate-lines t)))
  :custom
  (dired-recursive-deletes 'always)
  (dired-recursive-copies 'always)
  (dired-deletion-confirmer #'y-or-n-p)
  (dired-no-confirm '(byte-compile chgrp chmod chown copy load move symlink))
  (dired-dwim-target t)
  (dired-listing-switches "-lh")
  (dired-auto-revert-buffer t)
  (delete-by-moving-to-trash t)
  :general
  (:keymaps 'dired-mode-map
            "RET" #'dired-find-alternate-file))

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

(use-package tramp
  :straight nil
  :custom
  (tramp-verbose 2))




;; Snippets

(use-package yasnippet
  :init
  (setq yas-snippet-dirs `(,(expand-file-name "config/snippets" user-emacs-directory))))

(use-package auto-yasnippet)




;; General editing

(use-package rime
  :straight (rime :type git
                  :host github
                  :repo "DogLooksGood/emacs-rime"
                  :files ("*.el" "Makefile" "lib.c"))
  :custom
  (rime-librime-root "~/.emacs.d/rime-1.5.3-osx/dist")
  (rime-user-data-dir "~/.emacs.d/config/rime")
  (default-input-method "rime")
  (rime-show-candidate 'minibuffer)
  :general
  (:keymaps 'rime-mode-map
            "C-`" 'rime-send-keybinding))

(use-package expand-region
  :general
  ("C-@" 'er/expand-region
   "C-;" 'er/expand-region))

(use-package multiple-cursors
  :general
  ("M-<down-mouse-1>" 'nil
   "M-<mouse-1>" 'mc/add-cursor-on-click))

(use-package aggressive-indent
  ;; Aggressively indent lines
  :config
  (global-aggressive-indent-mode 1))

(use-package display-line-numbers
  ;; Display line numbers for programming languages
  :hook (prog-mode . display-line-numbers-mode))

(use-package highlight-thing
  :hook (prog-mode . highlight-thing-mode)
  :custom
  (highlight-thing-limit-to-defun t)
  (highlight-thing-exclude-thing-under-point t))

(use-package hungry-delete
  :config
  (global-hungry-delete-mode))

(use-package flycheck
  :disabled
  :hook (emacs-lisp-mode . flycheck-mode))





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




;; Help

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




;; Theme

(use-package zenburn-theme
  :config
  (load-theme 'zenburn t)
  :custom-face
  (mode-line-buffer-id ((t (:height 1.0 :family "Optima"))))
  (org-column-title ((t (:family "Hack Nerd Font" :height 3))))
  (org-column ((t (:height 3))))
  (org-document-info-keyword ((t (:height 0.8 :inherit shadow))))
  (org-document-info-keyword ((t (:height 0.8 :inherit shadow))))
  (org-document-title ((t (:height 1.5 :weight normal :foreground "LightSkyBlue" :family "Optima"))))
  (org-drawer ((t (:height 0.8 :inherit shadow))))
  (org-block-begin-line ((t (:height 0.8 :inherit shadow))))
  (org-block-end-line ((t (:height 0.8 :inherit shadow))))
  (org-meta-line ((t (:height 0.8 :foreground "gainsboro" :inherit shadow))))
  (org-special-keyword ((t (:height 0.8))))
  (org-property-value ((t (:foreground "DarkGray" :inherit org-special-keyword))))
  (org-ellipsis ((t (:underline nil :height 0.5))))
  (org-level-1 ((t (:height 1.2 :weight medium :inherit outline-1))))
  (org-level-2 ((t (:height 1.1 :inherit outline-2))))
  (org-level-3 ((t (:height 1.0 :inherit outline-3))))
  (org-level-4 ((t (:height 1.0 :inherit outline-4))))
  (org-level-5 ((t (:height 1.0 :inherit outline-5))))
  (org-level-6 ((t (:height 1.0 :inherit outline-6))))
  (org-level-7 ((t (:height 1.0 :inherit outline-7))))
  (org-level-8 ((t (:height 1.0 :inherit outline-8))))
  (org-block ((t (:height 0.8))))
  (org-checkbox-statistics-todo ((t (:height 0.8 :inherit (org-todo)))))
  (org-checkbox-statistics-done ((t (:height 0.8 :inherit (org-done)))))
  (org-tag ((t (:height 0.8 :inherit shadow))))
  (org-table ((t (:foreground "#9FC59F" :height 0.8))))
  (org-done ((t (:foreground "LightGreen"))))
  (org-code ((t (:height 0.9 :foreground "gainsboro"))))
  (org-date ((t (:height 0.8 :underline (:color foreground-color :style line) :foreground "#8CD0D3")))))

(use-package smart-mode-line
  :init
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
  (rm-text-properties '(("Org-roam" 'display " 🏄🏼‍♂️"))))

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





(load-relative "config-prog.el")
(load-relative "config-org.el")

(setq custom-file (expand-file-name "config/custom.el" user-emacs-directory))
(load custom-file)
