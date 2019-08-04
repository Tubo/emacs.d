(use-package org
  :config
  (add-hook 'org-mode-hook (lambda () (visual-line-mode 1)))
  (add-hook 'org-mode-hook (lambda () (org-variable-pitch-minor-mode 1)))
  (add-hook 'org-mode-hook (lambda () (abbrev-mode 1)))
  (setq org-clock-persist 'history)
  (org-clock-persistence-insinuate)
  (setq org-refile-targets '((org-agenda-files :maxlevel . 1) (nil :maxlevel . 2)))
  (setq org-pretty-entities t)
  (setq org-use-sub-superscripts "{}")
  (setq org-log-done 'time)
  (setq org-agenda-files (quote ("~/Dropbox/org/gtd.org")))
  :custom
  (org-startup-indented t)
  (org-clock-persist 'history)  
  (org-agenda-todo-list-sublevels nil)
  (org-tags-column -100)
  :custom-face
  (org-level-1 ((t (:foreground "#DFAF8F" :height 1.4))))
  (org-level-2 ((t (:foreground "#BFEBBF" :height 1.2))))
  (org-level-3 ((t (:foreground "#7CB8BB" :height 1.0))))
  (org-tag ((t (:height 0.7))))
  )


(use-package worf
  :ensure t
  )


(use-package anki-editor
  :ensure t
  :custom
  (anki-editor-create-decks t)
  )


(use-package org-pomodoro
  :ensure t)


(use-package org-variable-pitch
  :ensure t)


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


(use-package ox-cv
  :load-path "~/.emacs.d/personal/packages/"
  :after ox
  )

;; MacOS settings
(use-package org
  :if (eq system-type 'darwin)
  :config
  (setq org-default-notes-file "~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org/inbox.org")
  :custom
  (org-variable-pitch-fixed-font "Andale Mono")
  :custom-face
  (variable-pitch ((t (:height 1.2 :family "Avenir Next"))))
  )

;; Linux settings
(use-package org
  :if (eq system-type 'gnu/linux)
  :custom
  (org-variable-pitch-fixed-font "Course Code Variable")
  :custom-face
  (variable-pitch ((t (:height 1.2 :family "Noto Sans"))))
  )


;; Windows settings
(use-package org
  :if (eq system-type 'windows-nt)
  :custom
  (org-variable-pitch-fixed-font "Course Code Variable")
  :custom-face
  (variable-pitch ((t (:height 1.2 :family "DejaVu Sans"))))
  )
