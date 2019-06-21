(use-package org
  :config
  (add-hook 'org-mode-hook (lambda () (visual-line-mode 1)))
  (add-hook 'org-mode-hook (lambda () (org-variable-pitch-minor-mode 1)))
  (add-hook 'org-mode-hook (lambda () (abbrev-mode 1)))
  (org-clock-persistence-insinuate)
  (setq org-refile-targets '((org-agenda-files :maxlevel . 2)))
  (setq org-pretty-entities t)
  (setq org-use-sub-suerpscripts "{}")
  (setq org-log-done 'time)
  :custom
  (org-startup-indented t)
  (org-clock-persist 'history)  
  (org-agenda-todo-list-sublevels nil)
  )


(use-package worf
  :ensure t
  )

(use-package ox-pandoc
  :disabled
  :ensure t
  :custom
  (org-pandoc-menu-entry
   '((120 "to docx and open." org-pandoc-export-to-docx-and-open)
     (88 "to docx." org-pandoc-export-to-docx)))
  )

(use-package ox-hugo
  :ensure t
  :after ox)

(use-package org-variable-pitch
  :ensure t)

(use-package org
  :if (eq system-type 'darwin)
  :config
  (setq org-default-notes-file "~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org/inbox.org")
  :custom-face
  (org-beamer-tag ((t (:box (:line-width -1 :color grey40)))))
  (org-block ((t (:inherit shadow :height 0.8 :family "Menlo"))))
  (org-block-begin-line ((t (:inherit org-meta-line :height 1 :family "Monaco"))))
  (org-checkbox ((t (:background "#5F5F5F" :foreground "#FFFFEF" :box (:line-width 1 :style released-button) :family "Monaco"))))
  (org-code ((t (:inherit shadow :height 0.8 :family "Monaco"))))
  (org-date ((t (:foreground "#8CD0D3" :underline t :height 0.8 :family "Monaco"))))
  (org-document-info ((t (:foreground "gold"))))
  (org-document-info-keyword ((t (:inherit shadow))))
  (org-document-title ((t (:foreground "#8CD0D3" :height 1.4 :family "Palatino"))))
  (org-indent ((t (:family "Monaco" :inherit org-hide))))
  (org-level-1 ((t (:foreground "#DFAF8F" :height 1.4 :family "Palatino"))))
  (org-level-2 ((t (:foreground "#BFEBBF" :height 1.3 :family "Palatino"))))
  (org-level-3 ((t (:foreground "#7CB8BB" :height 1.2 :family "Palatino"))))
  (org-meta-line ((t (:inherit font-lock-comment-face :weight extra-light :height 0.8 :family "Menlo"))))
  (org-priority ((t (:foreground "gold" :family "Monaco"))))
  (org-special-keyword ((t (:inherit org-meta-line))))
  (org-table ((t (:foreground "#9FC59F" :family "Monaco"))))
  (org-tag ((t (:foreground "dark gray" :weight ultra-light :height 0.8 :family "Monaco"))))
  (org-todo ((t (:foreground "gold" :weight bold))))
  (variable-pitch ((t (:height 1.3 :family "Avenir Next"))))
  )

(use-package org
  :if (eq system-type 'gnu/linux)
  :custom-face
  (org-beamer-tag ((t (:box (:line-width -1 :color grey40)))))
  (org-block ((t (:inherit shadow :height 0.9 :family "Source Code Variable"))))
  (org-checkbox ((t (:background "#5F5F5F" :foreground "#FFFFEF" :box (:line-width 1 :style released-button) :family "Source Code Variable"))))
  (org-code ((t (:inherit shadow :height 0.8 :family "Source Code Variable"))))
  (org-date ((t (:foreground "#8CD0D3" :underline t :height 0.8 :family "Source Code Variable"))))
  (org-document-info ((t (:foreground "gold"))))
  (org-document-info-keyword ((t (:inherit shadow))))
  (org-document-title ((t (:foreground "#8CD0D3" :height 1.4 :family "Noto Sans"))))
  (org-indent ((t (:family "Source Code Pro" :inherit org-hide))))
  (org-level-1 ((t (:foreground "#DFAF8F" :height 1.4 :family "ETBembo" :weight bold))))
  (org-level-2 ((t (:foreground "#BFEBBF" :height 1.3 :family "ETBembo" :weight bold))))
  (org-level-3 ((t (:foreground "#7CB8BB" :height 1.2 :family "ETBembo" :weight bold))))
  (org-meta-line ((t (:inherit font-lock-comment-face :height 0.9 :family "Source Code Variable"))))
  (org-priority ((t (:foreground "gold" :family "Source Code Variable"))))
  (org-special-keyword ((t (:inherit org-meta-line))))
  (org-table ((t (:foreground "#9FC59F" :family "Source Code Variable"))))
  (org-tag ((t (:foreground "dark gray" :weight ultra-light :height 0.8 :family "Source Code Variable"))))
  (org-todo ((t (:foreground "gold" :weight bold))))
  (variable-pitch ((t (:family "ETBembo" :height 1.2))))
  )


(use-package org
  :if (eq system-type 'windows-nt)
  :custom-face
  (org-beamer-tag ((t (:box (:line-width -1 :color grey40)))))
  (org-block ((t (:inherit shadow :height 0.9 :family "Source Code Variable"))))
  (org-checkbox ((t (:background "#5F5F5F" :foreground "#FFFFEF" :box (:line-width 1 :style released-button) :family "Source Code Variable"))))
  (org-code ((t (:inherit shadow :height 0.8 :family "Source Code Variable"))))
  (org-date ((t (:foreground "#8CD0D3" :underline t :height 0.8 :family "Source Code Variable"))))
  (org-document-info ((t (:foreground "gold"))))
  (org-document-info-keyword ((t (:inherit shadow))))
  (org-document-title ((t (:foreground "#8CD0D3" :height 1.4 :family "Arial"))))
  (org-indent ((t (:family "Source Code Pro" :inherit org-hide))))
  (org-level-1 ((t (:foreground "#DFAF8F" :height 1.4 :family "DejaVu Serif" :weight bold))))
  (org-level-2 ((t (:foreground "#BFEBBF" :height 1.3 :family "DejaVu Serif" :weight bold))))
  (org-level-3 ((t (:foreground "#7CB8BB" :height 1.2 :family "DejaVu Serif" :weight bold))))
  (org-meta-line ((t (:inherit font-lock-comment-face :height 0.9 :family "Source Code Variable"))))
  (org-priority ((t (:foreground "gold" :family "Source Code Variable"))))
  (org-special-keyword ((t (:inherit org-meta-line))))
  (org-table ((t (:foreground "#9FC59F" :family "Source Code Variable"))))
  (org-tag ((t (:foreground "dark gray" :weight ultra-light :height 0.8 :family "Source Code Variable"))))
  (org-todo ((t (:foreground "gold" :weight bold))))
  (variable-pitch ((t (:family "DejaVu Sans"))))
  )
