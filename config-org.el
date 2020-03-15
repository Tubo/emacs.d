(use-package org
  :config
  (add-hook 'org-mode-hook (lambda () (visual-line-mode 1)))
  (add-hook 'org-mode-hook (lambda () (org-variable-pitch-minor-mode 1)))
  (add-hook 'org-mode-hook (lambda () (abbrev-mode 1)))
  (yas-reload-all)
  (add-hook 'org-mode-hook (lambda () (yas-minor-mode 1)))
  (setq org-clock-persist 'history)
  (org-clock-persistence-insinuate)
  (setq org-refile-targets '(;; (org-agenda-files :maxlevel . 1)
                             (nil :maxlevel . 2)))
  (setq org-pretty-entities t)
  (setq org-use-sub-superscripts "{}")
  (setq org-log-done 'time)
  (setq org-agenda-files (quote ("~/Dropbox/org/gtd.org")))
  (setq org-export-backends '(ascii))

  :custom
  (org-startup-indented t)
  (org-startup-with-inline-images t)
  (org-image-actual-width nil)
  (org-edit-src-content-indentation 0)
  (org-clock-persist 'history)  
  (org-agenda-todo-list-sublevels nil)
  (org-tags-column -100)

  :custom-face
  (org-level-1 ((t (:foreground "#DFAF8F"))))
  (org-level-2 ((t (:foreground "#BFEBBF"))))
  (org-level-3 ((t (:foreground "#7CB8BB"))))
  (org-drawer ((t (:height 0.8 :foreground "LightSkyBlue"))))
  (org-special-keyword ((t (:height 0.8))))
  (org-tag ((t (:height 0.7))))
  )

(use-package worf
  :disabled
  :ensure t
  )

(use-package evil-org
  :ensure t
  :after org
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
            (lambda ()
              (evil-org-set-key-theme)))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)
  )

(use-package anki-editor
  :ensure t
  :custom
  (anki-editor-create-decks t)
  )


(use-package org-pomodoro
  :disabled
  :ensure t)


(use-package org-variable-pitch
  :ensure t)

(use-package org-drill
  :disabled
  :ensure
  :requires ox
  )

(use-package org-brain
  :ensure t
  :after org
  :init
  (require 'org-capture)
  (setq org-brain-path "~/Dropbox/med_notes")
  ;; For Evil users
  (with-eval-after-load 'evil
    (evil-set-initial-state 'org-brain-visualize-mode 'emacs))
  :config
  (setq org-id-track-globally t)
  (setq org-id-locations-file "~/.emacs.d/.org-id-locations")
  (push '("b" "Brain" plain (function org-brain-goto-end)
          "* %i%?" :empty-lines 1)
        org-capture-templates)
  (setq org-brain-visualize-default-choices 'all)
  (setq org-brain-title-max-length 12)
  ;; Use headline entries only
  (setq org-brain-include-file-entries nil
        org-brain-headline-entry-name-format-string "%2$s"
        org-brain-file-entries-use-title nil)
  (setq my/default-org-brain-file "mindmap"
        org-brain-file-from-input-function
        (lambda (x) (if (cdr x) (car x) my/default-org-brain-file)))
  )

(use-package org-noter
  :ensure t
  :config
  (add-hook 'org-noter-insert-heading-hook #'org-id-get-create) 
  )


;; Org export backends
;; ====================
(use-package ox-pandoc
  :ensure t
  :custom
  (org-pandoc-menu-entry
   '((120 "to docx and open." org-pandoc-export-to-docx-and-open)
     (88 "to docx." org-pandoc-export-to-docx)
     (?k "to markdown." org-pandoc-export-to-markdown)
     (?K "to markdown and open." org-pandoc-export-to-markdown-and-open)
     )))


(use-package ox-hugo
  :disabled
  :ensure t
  :after ox)


(use-package ox-cv
  :load-path "~/.emacs.d/personal/packages/"
  :after ox
  )

;; OS-specific settings
;; ====================
;; MacOS settings
(use-package org
  :if (eq system-type 'darwin)
  :config
  :custom
  (org-variable-pitch-fixed-font "Andale Mono")
  :custom-face
  (variable-pitch ((t (:height 1.2 :family "Avenir Next"))))
  )

;; Linux settings
(use-package org
  :if (eq system-type 'gnu/linux)
  :custom
  (org-variable-pitch-fixed-font "Sourse Code Variable")
  :custom-face
  (variable-pitch ((t (:height 1.2 :family "Noto Sans"))))
  )


;; Windows settings
(use-package org
  :if (eq system-type 'windows-nt)
  :custom
  (org-variable-pitch-fixed-font "Sourse Code Variable")
  :custom-face
  (variable-pitch ((t (:height 1.2 :family "DejaVu Sans"))))
  )
