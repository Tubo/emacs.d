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
       (progn
         (add-to-list 'default-frame-alist '(font . "Monaco"))
         ))
      ((eq system-type 'gnu/linux)
       (progn
         (add-to-list 'default-frame-alist '(font . "Source Code Variable"))
         ))
      ((eq system-type 'windows-nt)
       (progn
         (add-to-list 'default-frame-alist '(font . "Source Code Pro"))
         )))


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

;; Emacs exec paths should be same as shells
(use-package exec-path-from-shell
  :ensure t
  :when (memq window-system '(mac ns x))
  :init
  (setq exec-path-from-shell-arguments nil)
  :config
  (exec-path-from-shell-initialize))


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


;; Eyebrowse
;; =========
(use-package eyebrowse
  :ensure t
  :init
  (setq eyebrowse-new-workspace t)
  (setq eyebrowse-wrap-around t)
  :config
  (eyebrowse-setup-evil-keys)
  (eyebrowse-mode t))


;; Helpful
;; =======
(use-package helpful
  :ensure t)


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
(use-package smart-mode-line
  :ensure t
  :config
  (sml/setup))
(use-package smart-mode-line-powerline-theme
  :ensure t)

;; Keybindings
;; ===========
;; Custom keybindings
(general-def
  "C-x k" 'kill-this-buffer)

;; * Prefix Keybindings
;; :prefix can be used to prevent redundant specification of prefix keys
;; again, variables are not necessary and likely not useful if you are only
;; using a definer created with `general-create-definer' for the prefixes
;; (defconst my-leader "SPC")
;; (defconst my-local-leader "SPC m")

(general-create-definer my-leader-def
  ;; :prefix my-leader
  :prefix "SPC")

(general-create-definer my-local-leader-def
  ;; :prefix my-local-leader
  :prefix "SPC m")

;; ** Global Keybindings
(my-leader-def
  :keymaps 'normal
  "a" 'org-agenda
  "b" 'counsel-switch-buffer
  "c" 'org-capture
  "e" 'counsel-find-file
 )

;; ** Mode Keybindings
(my-local-leader-def
  :states 'normal
  :keymaps 'org-mode-map
  "y" 'org-store-link
  "p" 'org-insert-link
  )

;; Magit keybindings
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

;; Helpful keybindings
(general-def
  "C-h f" 'helpful-callable
  "C-h k" 'helpful-key
  "C-h v" 'helpful-variable)
