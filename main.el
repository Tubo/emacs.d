(defconst my/initial-gc-cons-threshold gc-cons-threshold
  "Initial value of `gc-cons-threshold' at start-up time.")
(setq gc-cons-threshold (* 128 1024 1024))
(add-hook 'after-init-hook
          (lambda () (setq gc-cons-threshold my/initial-gc-cons-threshold)))
(setq gnutls-min-prime-bits 4096)


;; Packages repositories
;; ========
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
(eval-when-compile (require 'use-package))

(use-package quelpa
  :ensure t
  :config
  :custom
  (quelpa-upgrade-interval 7)
  (quelpa-checkout-melpa-p nil)
  (quelpa-self-upgrade-p nil))

(use-package quelpa-use-package
  :ensure t)

(use-package general
  :ensure t)





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
   mac-control-modifier 'control)
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
        display-time-default-load-average nil
        display-time-use-mail-icon t)
  (display-time)
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
  (blink-cursor-mode -1)
  (put 'narrow-to-region 'disabled nil)

  ;; fonts - depending on the OS
  (cl-case system-type
    (darwin (add-to-list 'default-frame-alist '(font . "Monaco")))
    (gnu/linux (add-to-list 'default-frame-alist '(font . "Source Code Variable")))
    (windows-nt (add-to-list 'default-frame-alist '(font . "Source Code Pro")))))





(use-package better-defaults
  ;; Disables menu-bar, tool-bar and scroll-bar
  ;; Changes backup dir
  ;; Misc changes
  :ensure t)

(use-package load-relative
  ;; Load ELisp file using relative location
  :pin gnu
  :ensure t)

(use-package super-save
  ;; Autosave
  :ensure t
  :config
  (setq auto-save-default nil)
  (setq super-save-remote-files nil)
  (setq super-save-auto-save-when-idle t)
  (super-save-mode +1))





;; Shell
;; ======
(use-package exec-path-from-shell
  ;; Emacs exec paths should be same as shells
  :ensure t
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
    :ensure t
    :config (setup-esh-help-eldoc)))





;; Version Control
;; ==========
(use-package magit
  :ensure t
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
   "<f1>" 'magit-status
   "C-x M-g" 'magit-dispatch))

(use-package evil-magit
  :ensure t
  :after (magit evil))

(use-package diff-hl
  :ensure t
  :config
  (global-diff-hl-mode 1)
  (eval-after-load 'magit
    (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)))

(use-package autorevert
  :hook (dired-mode . auto-revert-mode)
  :config
  (global-auto-revert-mode 1))





;; Window navigation
;; ==========
(use-package ace-window
  :ensure t
  :config
  (setq aw-scope 'frame)
  :general
  ("s-o" 'ace-window)
  ("<f9>" 'ace-window))

(use-package eyebrowse
  :ensure t
  :init
  (setq eyebrowse-new-workspace t)
  (setq eyebrowse-wrap-around t)
  :config
  (eyebrowse-mode t))

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





;; File, buffer and pointer navigation
;; ======================
(use-package ivy
  :ensure t
  :init
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  :config
  (ivy-mode t)
  :general
  ("C-s" 'swiper
   "C-c C-r" 'ivy-resume))

(use-package counsel
  :ensure t
  :config
  (counsel-mode t)
  :general
  ("M-x" 'counsel-M-x
   "C-x C-f" 'counsel-find-file)
  (minibuffer-local-map
   "C-r" 'counsel-minibuffer-history))

(use-package swiper
  :ensure t
  :general
  ("C-s" 'swiper))

(use-package avy
  ;; Jump to things in Emacs tree-style
  :ensure t)





;; Project management
;; ===============
(use-package projectile
  :ensure t
  :demand
  :init
  (setq projectile-switch-project-action 'projectile-dired)
  (setq projectile-completion-system 'ido)
  (setq projectile-enable-caching t)
  (setq projectile-mode-line '(:eval (format " <%s>" (projectile-project-name))))
  :config
  (projectile-global-mode)
  (setq projectile-completion-system 'ivy)
  :general
  ("C-c p" 'projectile-command-map))

(use-package counsel-projectile
  :ensure t)





;; Evil
;; ====
(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode t))

(use-package evil-collection
  :after (evil)
  :ensure t
  :config
  (dolist (mode '(lispy))
    (delq mode evil-collection-mode-list))
  (evil-collection-init))





;; Auto-completion
;; ===============
(use-package company
  :ensure t
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
    :ensure t
    :hook ((emacs-lisp-mode lisp-interaction-mode) . my-company-elisp-setup)
    :config (company-statistics-mode)))

;; Popup documentation for completion candidates
(use-package company-quickhelp
  :ensure t
  :init
  (setq company-quickhelp-use-propertized-text t)
  (setq company-quickhelp-delay 1)
  :config
  (company-quickhelp-mode 1))





;; File organisation
;; =============
(use-package dired
  :ensure nil
  :demand
  :init
  (setq dired-auto-revert-buffer t)
  (setq dired-listing-switches "-alh")
  (setq dired-no-confirm
        '(byte-compile chgrp chmod chown copy delete load move symlink))
  (setq dired-deletion-confirmer (lambda (x) t)))

(use-package dired-hacks-utils
  :ensure t)

(use-package dired-open
  :ensure t)

(use-package dired-subtree
  :ensure t
  :general
  ('normal dired-mode-map "TAB" 'dired-subtree-cycle))

(use-package dired-filter
  :ensure t
  :general
  ('normal dired-mode-map
           "f" 'dired-filter-mode))

(use-package dired-rainbow
  :ensure t
  :config
  (dired-rainbow-define-chmod directory "#6cb2eb" "d.*")
  (dired-rainbow-define html "#eb5286" ("css" "less" "sass" "scss" "htm" "html" "jhtm" "mht" "eml" "mustache" "xhtml"))
  (dired-rainbow-define xml "#f2d024" ("xml" "xsd" "xsl" "xslt" "wsdl" "bib" "json" "msg" "pgn" "rss" "yaml" "yml" "rdata"))
  (dired-rainbow-define document "#9561e2" ("docm" "doc" "docx" "odb" "odt" "pdb" "pdf" "ps" "rtf" "djvu" "epub" "odp" "ppt" "pptx"))
  (dired-rainbow-define markdown "#ffed4a" ("org" "etx" "info" "markdown" "md" "mkd" "nfo" "pod" "rst" "tex" "textfile" "txt"))
  (dired-rainbow-define database "#6574cd" ("xlsx" "xls" "csv" "accdb" "db" "mdb" "sqlite" "nc"))
  (dired-rainbow-define media "#de751f" ("mp3" "mp4" "MP3" "MP4" "avi" "mpeg" "mpg" "flv" "ogg" "mov" "mid" "midi" "wav" "aiff" "flac"))
  (dired-rainbow-define image "#f66d9b" ("tiff" "tif" "cdr" "gif" "ico" "jpeg" "jpg" "png" "psd" "eps" "svg"))
  (dired-rainbow-define log "#c17d11" ("log"))
  (dired-rainbow-define shell "#f6993f" ("awk" "bash" "bat" "sed" "sh" "zsh" "vim"))
  (dired-rainbow-define interpreted "#38c172" ("py" "ipynb" "rb" "pl" "t" "msql" "mysql" "pgsql" "sql" "r" "clj" "cljs" "scala" "js"))
  (dired-rainbow-define compiled "#4dc0b5" ("asm" "cl" "lisp" "el" "c" "h" "c++" "h++" "hpp" "hxx" "m" "cc" "cs" "cp" "cpp" "go" "f" "for" "ftn" "f90" "f95" "f03" "f08" "s" "rs" "hi" "hs" "pyc" ".java"))
  (dired-rainbow-define executable "#8cc4ff" ("exe" "msi"))
  (dired-rainbow-define compressed "#51d88a" ("7z" "zip" "bz2" "tgz" "txz" "gz" "xz" "z" "Z" "jar" "war" "ear" "rar" "sar" "xpi" "apk" "xz" "tar"))
  (dired-rainbow-define packaged "#faad63" ("deb" "rpm" "apk" "jad" "jar" "cab" "pak" "pk3" "vdf" "vpk" "bsp"))
  (dired-rainbow-define encrypted "#ffed4a" ("gpg" "pgp" "asc" "bfe" "enc" "signature" "sig" "p12" "pem"))
  (dired-rainbow-define fonts "#6cb2eb" ("afm" "fon" "fnt" "pfb" "pfm" "ttf" "otf"))
  (dired-rainbow-define partition "#e3342f" ("dmg" "iso" "bin" "nrg" "qcow" "toast" "vcd" "vmdk" "bak"))
  (dired-rainbow-define vc "#0074d9" ("git" "gitignore" "gitattributes" "gitmodules"))
  (dired-rainbow-define-chmod executable-unix "#38c172" "-.*x.*"))






;; Snippets
;; ===============
(use-package yasnippet
  :ensure t
  :init
  (setq yas-snippet-dirs `(,(expand-file-name "personal/snippets" user-emacs-directory))))





;; General programming
;; ===============
(use-package aggressive-indent
  ;; Aggressively indent lines
  :ensure
  :config
  (global-aggressive-indent-mode 1))

(use-package display-line-numbers
  ;; Display line numbers for programming languages
  :hook (prog-mode . display-line-numbers-mode))

(use-package outshine
  :ensure t
  :commands outshine-hook-function
  :hook ((outline-minor-mode . outshine-mode)
         (emacs-lisp-mode . outline-minor-mode))
  :init
  (setq outshine-imenu-show-headlines-p nil))

(use-package highlight-thing
  :ensure t
  :hook (prog-mode . highlight-thing-mode)
  :custom
  (highlight-thing-limit-to-defun t)
  (highlight-thing-exclude-thing-under-point t))





;; Emacs Lisp
;; ==========
(use-package lispy
  :ensure t
  :hook
  (emacs-lisp-mode . lispy-mode ))

(use-package lispyville
  :ensure t
  :after lispy
  :hook (lispy-mode . lispyville-mode)
  :config
  (lispyville-set-key-theme
   '(operators c-w slurp/barf-lispy wrap prettify text-objects
               commentary additional additional-insert)))

(defun my/conditionally-enable-lispy ()
  (when (eq this-command 'eval-expression)
    (lispy-mode 1)))
(add-hook 'minibuffer-setup-hook 'my/conditionally-enable-lispy)

(use-package elisp-refs
  :ensure t)

(use-package elisp-slime-nav
  :ensure t
  :hook ((emacs-lisp-mode ielm-mode) . turn-on-elisp-slime-nav-mode))

(use-package highlight-defined
  :ensure t
  :hook (emacs-lisp-mode . highlight-defined-mode))

(use-package rainbow-delimiters
  :ensure t
  :hook (emacs-lisp-mode . rainbow-delimiters-mode))

(use-package eldoc
  :init
  :hook (emacs-lisp-mode . eldoc-mode)
  :hook (lisp-interaction-mode-hook . eldoc-mode))

(use-package page-break-lines
  ;; Display ^L page breaks as tidy horizontal lines
  :ensure t
  :config
  (global-page-break-lines-mode))

(use-package eval-sexp-fu
  :ensure t
  :general
  (:keymaps 'lisp-interaction-mode-map
            "C-c C-c" 'eval-sexp-fu-eval-sexp-inner-list
            "C-c C-e" 'eval-sexp-fu-eval-sexp-inner-sexp)
  (:keymaps 'emacs-lisp-mode-map
            "C-c C-c" 'eval-sexp-fu-eval-sexp-inner-list
            "C-c C-e" 'eval-sexp-fu-eval-sexp-inner-sexp)
  :init
  (setq eval-sexp-fu-flash-duration 0.4)
  :config
  (turn-on-eval-sexp-fu-flash-mode))

(use-package eros
  :ensure t
  :hook (emacs-lisp-mode . eros-mode )
  :hook (lisp-interaction-mode . eros-mode))

(use-package ielm
  :config
  (add-hook 'inferior-emacs-lisp-mode-hook
            (lambda ()
              (turn-on-eldoc-mode))))

(use-package ipretty
  :ensure t
  :config (ipretty-mode t))





;; Elm
;; =====
(use-package elm-mode
  :disabled
  :ensure t
  :init
  (setq elm-tags-on-save t))





;; Json
;; =====
(use-package json-mode
  :ensure t
  :mode "\\.json\\'")





;; Documents
;; ===============
(use-package pdf-tools
  :ensure t
  :init
  (with-eval-after-load 'evil
    (evil-set-initial-state 'pdf-view-mode 'emacs))
  (pdf-loader-install)
  :config
  (setq pdf-view-use-scaling t)
  :bind
  (:map pdf-view-mode-map
        ("k" . pdf-view-previous-line-or-previous-page)
        ("j" . pdf-view-next-line-or-next-page)))





;; Org-mode
;; ==========
(load-relative "config-org.el")





;; References
;; ============
(use-package helpful
  :ensure t
  :general
  ("C-h f" 'helpful-callable
   "C-h k" 'helpful-key
   "C-h v" 'helpful-variable))

(use-package which-key
  :ensure t
  :custom
  (which-key-show-docstrings 'docstring-only)
  (which-key-max-description-length nil)
  (which-key-side-window-max-height 0.75)
  (which-key-idle-delay 2.0)
  :config
  (which-key-mode))

(use-package discover-my-major
  :ensure t
  :init
  (with-eval-after-load 'evil
    (evil-set-initial-state 'makey-key-mode 'emacs))
  :general ("C-h C-m" 'discover-my-major))





;; Focus
;; ===============
(use-package darkroom
  :ensure t
  :general ("S-<f11>" 'darkroom-tentative-mode)
  :custom
  (darkroom-text-scale-increase 1.5)
  (darkroom-margins-if-failed-guess 0.05))





;; Load custom.el
;; ==============
(load custom-file)





;; Theme
;; =====
(use-package zenburn-theme
  :ensure t
  :config
  (setq zenburn-scale-org-headlines t)
  (load-theme 'zenburn t))

(use-package smart-mode-line
  :ensure t
  :init
  (setq rm-whitelist '(" LYVLE"))
  (setq sml/name-width 30)
  (setq sml/vc-mode-show-backend t)
  (setq sml/no-confirm-load-theme t)
  :config
  (sml/setup)
  :custom
  (rm-text-properties '(("LYVLE" 'display " 🍰"))))

(use-package smart-mode-line-powerline-theme
  :disabled
  :ensure t)

(use-package shr
  ;; increase contrast between similar colors
  :custom
  (shr-color-visible-luminance-min 60))

(use-package hl-line
  :hook ((dired-mode package-menu-mode prog-mode) . hl-line-mode))

(use-package rainbow-mode
  :ensure t
  :hook (emacs-lisp-mode css-mode js-mode text-mode))

(use-package all-the-icons)
(use-package all-the-icons-ivy
  :ensure t
  :init
  (all-the-icons-ivy-setup))





;; Keybindings
;; ===========

;;; Custom keybindings
(general-def
  "C-x k" 'kill-this-buffer)

(general-create-definer my-leader-def
  :prefix "SPC")

(my-leader-def
  ;; Global Keybindings
  :keymaps 'normal
  "a" 'org-agenda
  "b" 'ivy-switch-buffer
  "c" 'org-capture
  "d" 'counsel-dired
  "e" 'counsel-find-file
  "g" 'magit-status
  "k" 'kill-buffer-and-window
  "o" 'ace-window
  "p" 'counsel-projectile
  "P" 'projectile-command-map
  "r" 'ivy-resume
  "s" 'swiper
  "S" 'counsel-ag
  "v" 'org-brain-visualize
  "1" 'eyebrowse-switch-to-window-config-1
  "2" 'eyebrowse-switch-to-window-config-2
  "3" 'eyebrowse-switch-to-window-config-3
  "/" 'avy-goto-char-timer
  "\\" 'eshell)

(general-create-definer my-local-leader-def
  ;; Local keybindings
  :prefix "SPC SPC")

(my-local-leader-def
  ;; Org-mode
  :states 'normal
  :keymaps 'org-mode-map
  "p" 'org-insert-link
  "r" 'avy-org-refile-as-child
  "R" 'org-refile
  "c" 'anki-editor-cloze-dwim
  "h" 'counsel-imenu
  "t" 'counsel-org-tag
  "y" 'yas-insert-snippet)

(my-local-leader-def
  ;; Emacs Lisp mode
  :states '(normal visual)
  :keymaps '(emacs-lisp-mode-map lisp-interaction-mode-map)
  ","  #'lispyville-comment-or-uncomment
  "."  #'lispyville-comment-and-clone-dwim
  "ci" #'lispyville-comment-or-uncomment-line
  "(" #'lispyville-wrap-round)



