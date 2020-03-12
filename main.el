(defconst ts/initial-gc-cons-threshold gc-cons-threshold
  "Initial value of `gc-cons-threshold' at start-up time.")
(setq gc-cons-threshold (* 128 1024 1024))
(add-hook 'after-init-hook
          (lambda () (setq gc-cons-threshold ts/initial-gc-cons-threshold)))
(setq gnutls-min-prime-bits 4096)


;; Packages repositories
;; ========
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
	("org" . "http://orgmode.org/elpa/")
	("gnu" . "https://elpa.gnu.org/packages/"))
      )
(add-to-list 'load-path "~/.emacs.d/personal/packages/")
(package-initialize)


;; Fonts - depending on the OS
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
;; New shells shall spawn in side windows
(add-to-list 'display-buffer-alist
             '("*eshell" (display-buffer-in-side-window) (side . bottom)))
;; Scratch buffer default to org-mode
(setq initial-major-mode 'org-mode)
(setq initial-scratch-message "# This is a scratchpad.")

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
  :ensure t
  :general
  (
   "C-x g" 'magit-status
   "C-x M-g" 'magit-dispatch
   )
  )


;; Ace window
;; ==========
(use-package ace-window
  :ensure t
  :config
  (setq aw-scope 'frame)
  :general
  ("M-o" 'ace-window)
  )


;; Ivy / Swiper / Counsel
;; ======================
(use-package ivy
  :ensure t
  :init
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  :config
  (ivy-mode t)
  :general
  (
   "C-s" 'swiper
   "C-c C-r" 'ivy-resume
   )
  )


(use-package counsel
  :ensure t
  :config
  (counsel-mode t)
  :general
  ("M-x" 'counsel-M-x
   "C-x C-f" 'counsel-find-file)
  (minibuffer-local-map
   "C-r" 'counsel-minibuffer-history
   )
  )
(use-package swiper
  :ensure t
  :general
  ("C-s" 'swiper)
  )

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
  :after evil
  :ensure t
  :config
  (evil-collection-init)
  )
(use-package evil-magit
  :ensure t)


;; Dired-related
;; =============
(use-package dired-hacks-utils
  :ensure t)
(use-package dired-open
  :ensure t)
(use-package dired-subtree
  :ensure t
  :general
  ('normal dired-mode-map 
           "TAB" 'dired-subtree-cycle
           )
  )
(use-package dired-filter
  :ensure t
  :general
  ('normal dired-mode-map
           "f" 'dired-filter-mode)
  )
(use-package dired-rainbow
  :ensure t
  :config
  (progn
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
    (dired-rainbow-define-chmod executable-unix "#38c172" "-.*x.*")
    )) 


;; Yasnippet
;; =========
(use-package yasnippet
  :ensure t
  :init
  (setq yas-snippet-dirs
        `(,(expand-file-name "personal/snippets" user-emacs-directory))
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


;; Eyebrowse
;; =========
(use-package eyebrowse
  :ensure t
  :init
  (setq eyebrowse-new-workspace t)
  (setq eyebrowse-wrap-around t)
  :config
  (eyebrowse-mode t))


;; Helpful
;; =======
(use-package helpful
  :ensure t
  :general
  (
   "C-h f" 'helpful-callable
   "C-h k" 'helpful-key
   "C-h v" 'helpful-variable)
  )

;; Which-key
(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;; PDF-tools
(use-package pdf-tools
  :ensure t
  :init
  (with-eval-after-load 'evil
    (evil-set-initial-state 'pdf-view-mode 'emacs))
  :config
  (setq pdf-view-use-scaling t) 
  (pdf-loader-install)
  :bind
  (:map pdf-view-mode-map
        ("k" . pdf-view-previous-line-or-previous-page)
        ("j" . pdf-view-next-line-or-next-page))
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
  (setq zenburn-scale-org-headlines t)
  (load-theme 'zenburn t)
  )
(use-package smart-mode-line
  :ensure t
  :init
  (setq rm-blacklist
        '(" counsel" " ivy" " WK" " ARev" " Undo-Tree" " ElDoc"
          " BufFace" " Ind" " OVP" " Wrap" " Abbrev"))
  (setq sml/name-width 30)
  :config
  (sml/setup))
(use-package smart-mode-line-powerline-theme
  :disabled
  :ensure t)

;; Keybindings
;; ===========
;; Custom keybindings
(general-def
  "C-x k" 'kill-this-buffer)

(general-create-definer my-leader-def
  :prefix "SPC")

;; ** Global Keybindings
(my-leader-def
  :keymaps 'normal
  "a" 'org-agenda
  "b" 'ivy-switch-buffer
  "c" 'org-capture
  "d" 'counsel-dired
  "e" 'counsel-find-file
  "g" 'magit-status
  "k" 'kill-buffer-and-window
  "o" 'ace-window
  "s" 'eshell
  "v" 'org-brain-visualize
  "1" 'eyebrowse-switch-to-window-config-1
  "2" 'eyebrowse-switch-to-window-config-2
  "3" 'eyebrowse-switch-to-window-config-3
  "/" 'swiper
  "." 'org-pomodoro
 )


(general-create-definer my-local-leader-def
  :prefix "SPC m")

;; ** Mode Keybindings
(my-local-leader-def
  :states 'normal
  :keymaps 'org-mode-map
  "p" 'org-insert-link
  "r" 'org-refile
  "s" 'org-narrow-to-subtree
  "y" 'org-store-link
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
