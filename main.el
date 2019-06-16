(defconst ts/initial-gc-cons-threshold gc-cons-threshold
  "Initial value of `gc-cons-threshold' at start-up time.")
(setq gc-cons-threshold (* 128 1024 1024))
(add-hook 'after-init-hook
          (lambda () (setq gc-cons-threshold ts/initial-gc-cons-threshold)))
(setq gnutls-min-prime-bits 4096)


;; Packages
;; ========
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
	("melpa-stable" . "https://stable.melpa.org/packages/")
	("org" . "http://orgmode.org/elpa/")
	("gnu" . "https://elpa.gnu.org/packages/")))

(add-to-list 'load-path "~/.emacs.d/packages/")
(package-initialize)


;; Fonts
;; =====
(cond ((eq system-type 'darwin)
       (add-to-list 'default-frame-alist '(font . "Monaco")))
      ((eq system-type 'gnu/linux)
       (add-to-list 'default-frame-alist '(font . "Source Code Variable")))
      ((eq system-type 'windows-nt)
       (add-to-list 'default-frame-alist '(font . "Source Code Pro"))))


;; Initialise 'use-package and 'general
;; ==================================
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))
(use-package general
  :ensure t)


;; Initial settings
;; ================
(setq inhibit-default-init 't)
(setq vc-follow-symlinks nil)
(setq shell-command-switch "-ic")
(setq inhibit-splash-screen t
      inhibit-startup-message t)
(electric-pair-mode)


;; Good packages to start off with
;; ===============================
(use-package better-defaults
  :ensure t)
(use-package load-relative
  :pin gnu
  :ensure t)


;; Magit
;; =====
(use-package magit
  :ensure t)


;; Ace window
;; ==========
(use-package ace-window
  :ensure t)


;; Ivy / Swiper / Counsel
;; ======================
(use-package ivy
  :ensure t
  :init
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  :config
  (ivy-mode t)
  )
(use-package counsel
  :ensure t
  :config
  (counsel-mode t)
  )
(use-package swiper :ensure t)

;; Evil
;; ====
(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode t)
  )
(use-package evil-collection
  :disabled
  :after evil
  :ensure t
  :config
  (evil-collection-init)
  )


;; Dired-related
;; =============
(use-package dired-hacks-utils
  :ensure t)
(use-package dired-open
  :ensure t)

;; Ranger
(use-package ranger
  :ensure t
  :config
  (ranger-override-dired-mode t)
  )

;; Yasnippet
;; =========
(use-package yasnippet
  :ensure t
  :init
  (setq yas-snippet-dirs
        '((expand-file-name "personal/snippets" user-emacs-directory))
        )
  )


;; Elm
;; ===
(use-package elm-mode
  :disabled
  :ensure t
  :init
  (setq elm-tags-on-save t)
  )


;; Anki
;; ====
(use-package anki-editor
  :ensure t
  :custom
  (anki-editor-create-decks t)
  )


;; Org-mode related
;; ================
(load-relative "config-org.el")


;; Load custom.el
;; ==============
(setq custom-file (expand-file-name "personal/custom.el" user-emacs-directory))
(load custom-file)


;; Theme
;; =====
(use-package zenburn-theme
  :ensure t
  :config
  (load-theme 'zenburn t)
  )


;; Keybindings
;; ===========
;; Custom keybindings
(general-def
  "C-x k" 'kill-this-buffer)

;; Magic keybindings
(general-def
  "C-x g" 'magit-status
  "C-x M-g" 'magit-dispatch)

;; Ivy keybindings
(general-def
  "C-s" 'swiper
  "C-c C-r" 'ivy-resume
  )
(general-def
  :keymaps 'minibuffer-local-map
  "C-r" 'counsel-minibuffer-history
  )

;; Org keybindings
(general-def
  "C-c l" 'org-store-link
  "C-c a" 'org-agenda
  "C-c c" 'org-capture
  "C-c b" 'org-iswitchb
  "C-'" 'org-cycle-agenda-files
  )
(general-def
  :keymaps 'org-mode-map
  "C-c C-q" 'counsel-org-tag
  "M-i" 'counsel-imenu
  )

