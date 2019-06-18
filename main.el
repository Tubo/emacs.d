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
;; New shells shall spawn in side windows
(add-to-list 'display-buffer-alist
             '("*eshell" (display-buffer-in-side-window) (side . bottom)))

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
  :general
  ("C-x o" 'ace-window)
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
  (evil-collection-init '(help helpful magit custom diff-mode doc-view ediff elisp-mode elisp-refs epa eshell eww ibuffer imenu-list info package-menu popup term which-key))
  )


;; Dired-related
;; =============
(use-package dired
  :general
  ('normal dired-mode-map
           "q" 'quit-window
           "j" 'dired-next-line
           "k" 'dired-previous-line
           [mouse-2] 'dired-mouse-find-file-other-window
           [follow-link] 'mouse-face
           ;; Commands to mark or flag certain categories of files
           "#" 'dired-flag-auto-save-files
           "." 'dired-clean-directory
           "~" 'dired-flag-backup-files
           ;; Upper case keys (except !) for operating on the marked files
           "A" 'dired-do-find-regexp
           "C" 'dired-do-copy
           "B" 'dired-do-byte-compile
           "D" 'dired-do-delete
           "H" 'dired-do-hardlink
           "L" 'dired-do-load
           "M" 'dired-do-chmod
           "O" 'dired-do-chown
           "P" 'dired-do-print
           "Q" 'dired-do-find-regexp-and-replace
           "R" 'dired-do-rename
           "S" 'dired-do-symlink
           "T" 'dired-do-touch
           "X" 'dired-do-shell-command
           "Z" 'dired-do-compress
           "c" 'dired-do-compress-to
           "!" 'dired-do-shell-command
           "&" 'dired-do-async-shell-command
           ;; Comparison commands
           "=" 'dired-diff
           ;; move to marked files
           "M-{" 'dired-prev-marked-file
           "M-}" 'dired-next-marked-file
           "%" nil
           "%u" 'dired-upcase
           "%l" 'dired-downcase
           "%d" 'dired-flag-files-regexp
           "%g" 'dired-mark-files-containing-regexp
           "%m" 'dired-mark-files-regexp
           "%r" 'dired-do-rename-regexp
           "%C" 'dired-do-copy-regexp
           "%H" 'dired-do-hardlink-regexp
           "%R" 'dired-do-rename-regexp
           "%S" 'dired-do-symlink-regexp
           "%&" 'dired-flag-garbage-files
           ;; mark
           "*" nil
           "**" 'dired-mark-executables
           "*/" 'dired-mark-directories
           "*@" 'dired-mark-symlinks
           "*%" 'dired-mark-files-regexp
           "*(" 'dired-mark-sexp
           "*." 'dired-mark-extension
           "*O" 'dired-mark-omitted
           "*c" 'dired-change-marks
           "*s" 'dired-mark-subdir-files
           "*m" 'dired-mark
           "*u" 'dired-unmark
           "*?" 'dired-unmark-all-files
           "*!" 'dired-unmark-all-marks
           "U" 'dired-unmark-all-marks
           "* <delete>" 'dired-unmark-backward
           "* C-n" 'dired-next-marked-file
           "* C-p" 'dired-prev-marked-file
           "*t" 'dired-toggle-marks
           ;; Lower keys for commands not operating on all the marked files
           "a" 'dired-find-alternate-file
           "d" 'dired-flag-file-deletion
           "C-m" 'dired-find-file
           "gr" 'revert-buffer
           "I" 'dired-maybe-insert-subdir
           "J" 'dired-goto-file
           "K" 'dired-do-kill-lines
           "r" 'dired-do-redisplay
           "m" 'dired-mark
           "t" 'dired-toggle-marks
           "u" 'dired-unmark            ; also "*u"
           "W" 'browse-url-of-dired-file
           "x" 'dired-do-flagged-delete
           "Y" 'dired-copy-filename-as-kill
           "+" 'dired-create-directory
           ;; open
           "RET" 'dired-find-file
           "gO" 'dired-find-file-other-window
           "go" 'dired-view-file
           ;; sort and search
           "o" 'dired-sort-toggle-or-edit
           ;; moving
           "gp" 'dired-prev-dirline
           "gn" 'dired-next-dirline
           "gu" 'dired-up-directory
           "gd" 'dired-up-directory
           ;; hiding
           "(" 'dired-hide-details-mode
           )
  )
(use-package dired-hacks-utils
  :ensure t)
(use-package dired-open
  :ensure t)
(use-package dired-subtree
  :ensure t
  :general
  ('normal dired-mode-map 
           "i" 'dired-subtree-toggle
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


;; Ranger
(use-package ranger
  :disabled
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
  "k" 'kill-this-buffer
  "o" 'ace-window
  "s" 'eshell
  "1" 'eyebrowse-switch-to-window-config-1
  "2" 'eyebrowse-switch-to-window-config-2
  "3" 'eyebrowse-switch-to-window-config-3
  "/" 'swiper
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
