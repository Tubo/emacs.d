
;; Optimisation

(defconst my/initial-gc-cons-threshold gc-cons-threshold
  "Initial value of `gc-cons-threshold' at start-up time.")
(setq gc-cons-threshold (* 128 1024 1024))
(add-hook 'after-init-hook
          (lambda () (setq gc-cons-threshold my/initial-gc-cons-threshold)))
(setq gnutls-min-prime-bits 4096)




;; Initialise 'use-package, 'straight and 'general

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
(use-package restart-emacs)




;; Built-in settings

(use-package my/settings
  :straight nil
  ;; ----------
  :init
  ;; ----------
  (setq
   locale-coding-system 'utf-8
   require-final-newline t
   column-number-mode t
   tab-always-indent 'complete
   confirm-nonexistent-file-or-buffer nil
   ;; Version control
   vc-follow-symlinks nil
   vc-handled-backends '(Git Hg)
   ;; Rings
   history-length 250
   recentf-max-saved-items 5000
   kill-ring-max 5000
   mark-ring-max 5000
   ;; Windows
   display-buffer-alist '((".*" (display-buffer-reuse-window display-buffer-same-window)))
   display-buffer-reuse-frames t      ; reuse windows in other frames
   even-window-sizes nil              ; display-buffer: avoid resizing
   mouse-autoselect-window -0.1
   indicate-buffer-boundaries 'left
   split-height-threshold 110
   split-width-threshold 160
   ;; Editing
   show-paren-delay 0
   abbrev-file-name (expand-file-name "config/abbrev_defs" user-emacs-directory)
   hi-lock-file-patterns-policy #'(lambda (_) t)
   ;; Loading
   load-prefer-newer 5
   ;; Eval
   eval-expression-print-length nil
   eval-expression-print-level nil
   ;; Startup
   inhibit-splash-screen t
   inhibit-startup-message t
   inhibit-default-init t
   ;; MacOS
   mac-command-modifier 'meta
   mac-option-modifier 'super
   mac-control-modifier 'control
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

  ;; ----------
  :config
  ;; ----------

  ;; Fonts - depending on the OS
  (cl-case system-type
    (darwin
     (add-to-list 'default-frame-alist '(font . "Hack Nerd Font-12"))
     (mac-auto-operator-composition-mode t)
     (custom-set-faces '(variable-pitch ((t (:height 1.2 :family "Avenir Next"))))))
    (gnu/linux
     (set-frame-font "Hack Nerd Font")
     (custom-set-faces '(variable-pitch ((t (:height 1.0 :family "DejaVu Sans"))))))
    (windows-nt
     (set-frame-font "Cascadia Code")
     (custom-set-faces '(variable-pitch ((t (:height 1.2 :family "Gill Sans MT")))))))
  
  ;; Mode-line time display
  (setq display-time-24hr-format t
        display-time-day-and-date nil
        display-time-default-load-average nil
        display-time-use-mail-icon t)
  (display-time)

  ;; Display buffer size
  (size-indication-mode +1)

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

  ;; Emoji
  (set-fontset-font t 'symbol "Apple Color Emoji")
  (set-fontset-font t 'symbol "NotoColorEmoji Nerd" nil 'append)
  (set-fontset-font t 'symbol "NotoEmoji Nerd" nil 'append)
  (set-fontset-font t 'symbol "Segoe UI Emoji" nil 'append)
  (set-fontset-font t 'symbol "Symbola" nil 'append)
  
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



;; Org-mode

(load-relative "config-org.el")




;; Shell

(use-package exec-path-from-shell
  ;; Emacs exec paths should be same as shells
  :when (memq window-system '(mac ns x))
  :init
  (setq exec-path-from-shell-arguments nil)
  :config
  (exec-path-from-shell-initialize))





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
  ("M-3" 'eyebrowse-switch-to-window-config-3)
  ("M-4" 'eyebrowse-switch-to-window-config-4)
  ("M-5" 'eyebrowse-switch-to-window-config-5))


(use-package frame
  ;; Multi-frame management independent of window systems
  :straight nil
  :config
  (modify-all-frames-parameters '((width . 0.5)
                                  (height . 1.0)
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
  (org-column-title ((t (:family "Hack Nerd Font" :height 3.0))))
  (org-column ((t (:height 3))))
  (org-document-title ((t (:height 1.2 :weight normal :foreground "LightSkyBlue"))))
  (org-document-info-keyword ((t (:height 0.8 :inherit shadow))))
  (org-document-info-keyword ((t (:height 0.8 :inherit shadow))))
  (org-meta-line ((t (:height 0.8 :foreground "gainsboro" :inherit shadow))))
  (org-special-keyword ((t (:height 0.8))))
  (org-property-value ((t (:foreground "DarkGray" :inherit org-special-keyword))))
  (org-ellipsis ((t (:underline nil :height 0.5))))
  (org-level-1 ((t (:height 1.1 :weight semi-bold :inherit outline-1))))
  (org-level-2 ((t (:height 1.1 :inherit outline-2))))
  (org-level-3 ((t (:height 1.1 :inherit outline-3))))
  (org-level-4 ((t (:height 1.1 :inherit outline-4))))
  (org-level-5 ((t (:height 1.1 :inherit outline-5))))
  (org-level-6 ((t (:height 1.1 :inherit outline-6))))
  (org-level-7 ((t (:height 1.1 :inherit outline-7))))
  (org-level-8 ((t (:height 1.1 :inherit outline-8))))
  (org-checkbox-statistics-todo ((t (:height 0.8 :inherit (org-todo)))))
  (org-checkbox-statistics-done ((t (:height 0.8 :inherit (org-done)))))
  (org-tag ((t (:height 0.8 :inherit shadow))))
  (org-table ((t (:foreground "#9FC59F" :height 0.8))))
  (org-todo ((t (:height 0.8))))
  (org-done ((t (:height 0.8 :foreground "LightGreen"))))
  (org-block-begin-line ((t (:height 0.8 :inherit shadow))))
  (org-block-end-line ((t (:height 0.8 :inherit shadow))))
  (org-drawer ((t (:height 0.8 :inherit shadow))))
  (org-code ((t (:inherit org-block))))
  (org-block ((t (:height 0.8))))
  (org-quote ((t (:family "Georgia" :height 1.2))))
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
  (rm-text-properties '(("Org-roam" 'display " ⛵"))))

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





;; Autocompletion

(use-package company
  :disabled
  :config
  (defvar completion-at-point-functions-saved nil)

  (defun my/yas-expand-next-field-complete ()
    (interactive)
    (if yas-minor-mode
        (let ((old-point (point))
              (old-tick (buffer-chars-modified-tick)))
          (yas-expand)
          (when (and (eq old-point (point))
                     (eq old-tick (buffer-chars-modified-tick)))
            (ignore-errors (yas-next-field))
            (when (and (eq old-point (point))
                       (eq old-tick (buffer-chars-modified-tick)))
              (company-complete-common))))
      (company-complete-common)))

  (setq company-backends (remove 'company-ropemacs company-backends))

  ;; Usage based completion sorting
  (defun my/company-elisp-setup ()
    (set (make-local-variable 'company-backends)
         '((company-capf :with company-dabbrev-code))))
  
  (use-package company-statistics
    :hook ((emacs-lisp-mode lisp-interaction-mode) . my/company-elisp-setup)
    :config (company-statistics-mode))

  :custom
  (company-idle-delay 0.3)
  (company-tooltip-limit 10)
  (company-minimum-prefix-length 2)
  (tab-always-indent 'complete)
  :general
  (:keymaps 'company-active-map
            "TAB" 'my/yas-expand-next-field-complete))

(use-package company-box
  :diminish
  :after company
  :hook (company-mode . company-box-mode)
  :functions (my-company-box--make-line
              my-company-box-icons--elisp)
  :commands (company-box--get-color
             company-box--resolve-colors
             company-box--add-icon
             company-box--apply-color
             company-box--make-line
             company-box-icons--elisp)
  :custom
  (company-box-backends-colors nil)
  (company-box-show-single-candidate t)
  (company-box-max-candidates 50)
  (company-box-doc-delay 0.3)
  :config
  ;; Support `company-common'
  (defun my-company-box--make-line (candidate)
    (-let* (((candidate annotation len-c len-a backend) candidate)
            (color (company-box--get-color backend))
            ((c-color a-color i-color s-color) (company-box--resolve-colors color))
            (icon-string (and company-box--with-icons-p (company-box--add-icon candidate)))
            (candidate-string (concat (propertize (or company-common "") 'face 'company-tooltip-common)
                                      (substring (propertize candidate 'face 'company-box-candidate) (length company-common) nil)))
            (align-string (when annotation
                            (concat " " (and company-tooltip-align-annotations
                                             (propertize " " 'display `(space :align-to (- right-fringe ,(or len-a 0) 1)))))))
            (space company-box--space)
            (icon-p company-box-enable-icon)
            (annotation-string (and annotation (propertize annotation 'face 'company-box-annotation)))
            (line (concat (unless (or (and (= space 2) icon-p) (= space 0))
                            (propertize " " 'display `(space :width ,(if (or (= space 1) (not icon-p)) 1 0.75))))
                          (company-box--apply-color icon-string i-color)
                          (company-box--apply-color candidate-string c-color)
                          align-string
                          (company-box--apply-color annotation-string a-color)))
            (len (length line)))
      (add-text-properties 0 len (list 'company-box--len (+ len-c len-a)
                                       'company-box--color s-color)
                           line)
      line))
  (advice-add #'company-box--make-line :override #'my-company-box--make-line)

  ;; Prettify icons
  (defun my-company-box-icons--elisp (candidate)
    (when (derived-mode-p 'emacs-lisp-mode)
      (let ((sym (intern candidate)))
        (cond ((fboundp sym) 'Function)
              ((featurep sym) 'Module)
              ((facep sym) 'Color)
              ((boundp sym) 'Variable)
              ((symbolp sym) 'Text)
              (t . nil)))))
  (advice-add #'company-box-icons--elisp :override #'my-company-box-icons--elisp)

  (declare-function all-the-icons-faicon 'all-the-icons)
  (declare-function all-the-icons-material 'all-the-icons)
  (declare-function all-the-icons-octicon 'all-the-icons)
  (setq company-box-icons-all-the-icons
        `((Unknown . ,(all-the-icons-material "find_in_page" :height 0.85 :v-adjust -0.2))
          (Text . ,(all-the-icons-faicon "text-width" :height 0.8 :v-adjust -0.05))
          (Method . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.05 :face 'all-the-icons-purple))
          (Function . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.05 :face 'all-the-icons-purple))
          (Constructor . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.05 :face 'all-the-icons-purple))
          (Field . ,(all-the-icons-octicon "tag" :height 0.8 :v-adjust 0 :face 'all-the-icons-lblue))
          (Variable . ,(all-the-icons-octicon "tag" :height 0.8 :v-adjust 0 :face 'all-the-icons-lblue))
          (Class . ,(all-the-icons-material "settings_input_component" :height 0.85 :v-adjust -0.2 :face 'all-the-icons-orange))
          (Interface . ,(all-the-icons-material "share" :height 0.85 :v-adjust -0.2 :face 'all-the-icons-lblue))
          (Module . ,(all-the-icons-material "view_module" :height 0.85 :v-adjust -0.2 :face 'all-the-icons-lblue))
          (Property . ,(all-the-icons-faicon "wrench" :height 0.8 :v-adjust -0.05))
          (Unit . ,(all-the-icons-material "settings_system_daydream" :height 0.85 :v-adjust -0.2))
          (Value . ,(all-the-icons-material "format_align_right" :height 0.85 :v-adjust -0.2 :face 'all-the-icons-lblue))
          (Enum . ,(all-the-icons-material "storage" :height 0.85 :v-adjust -0.2 :face 'all-the-icons-orange))
          (Keyword . ,(all-the-icons-material "filter_center_focus" :height 0.85 :v-adjust -0.2))
          (Snippet . ,(all-the-icons-material "format_align_center" :height 0.85 :v-adjust -0.2))
          (Color . ,(all-the-icons-material "palette" :height 0.85 :v-adjust -0.2))
          (File . ,(all-the-icons-faicon "file-o" :height 0.85 :v-adjust -0.05))
          (Reference . ,(all-the-icons-material "collections_bookmark" :height 0.85 :v-adjust -0.2))
          (Folder . ,(all-the-icons-faicon "folder-open" :height 0.85 :v-adjust -0.05))
          (EnumMember . ,(all-the-icons-material "format_align_right" :height 0.85 :v-adjust -0.2 :face 'all-the-icons-lblue))
          (Constant . ,(all-the-icons-faicon "square-o" :height 0.85 :v-adjust -0.05))
          (Struct . ,(all-the-icons-material "settings_input_component" :height 0.85 :v-adjust -0.2 :face 'all-the-icons-orange))
          (Event . ,(all-the-icons-faicon "bolt" :height 0.8 :v-adjust -0.05 :face 'all-the-icons-orange))
          (Operator . ,(all-the-icons-material "control_point" :height 0.85 :v-adjust -0.2))
          (TypeParameter . ,(all-the-icons-faicon "arrows" :height 0.8 :v-adjust -0.05))
          (Template . ,(all-the-icons-material "format_align_center" :height 0.85 :v-adjust -0.2)))
        company-box-icons-alist 'company-box-icons-all-the-icons))





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






;; Json

(use-package json-mode
  :mode "\\.json\\'")

;; Yaml
(use-package yaml-mode
  :mode "\\.yml\\'"
  :general
  (:keymaps 'yaml-mode-map
            "C-m" 'newline-and-indent))



(setq custom-file (expand-file-name "config/custom.el" user-emacs-directory))
(load custom-file)
(desktop-save-mode +1)
