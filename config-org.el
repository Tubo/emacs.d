(use-package org
  :straight org-plus-contrib
  :hook ((org-mode . (lambda () (visual-line-mode 1)))
         (org-mode . (lambda () (abbrev-mode 1)))
         (org-mode . (lambda () (yas-minor-mode 1)))
         (org-mode . prettify-symbols-mode))
  :config
  (require 'org-tempo)
  (yas-reload-all)
  (setq prettify-symbols-unprettify-at-point 'right-edge)
  (org-clock-persistence-insinuate)

  :custom
  (org-agenda-files (quote ("~/Dropbox/org/gtd.org")))
  (org-refile-targets '((nil :maxlevel . 2)))
  (org-clock-persist 'history)
  (org-use-sub-superscripts "{}")
  (org-log-done 'time)
  (org-fontify-done-headline nil)
  (org-export-backends '(ascii html latex org))
  (org-pretty-entities t)
  (org-startup-indented t)
  (org-startup-with-inline-images t)
  (org-image-actual-width '(500))
  (org-edit-src-content-indentation 0)
  (org-src-tab-acts-natively t)
  (org-clock-persist 'history)
  (org-agenda-todo-list-sublevels nil)
  (org-tags-column -80)
  (org-odd-levels-only nil)
  (org-ellipsis " ⤵")
  (org-hide-leading-stars t)
  (org-enforce-todo-dependencies t)
  ;; Babel
  (org-confirm-babel-evaluate nil)
  (org-src-fontify-natively t)
  (org-src-tab-acts-natively t)

  :general
  ("C-c a" 'org-agenda
   "C-c l" 'org-store-link)
  (:keymaps 'org-mode-map
            "C-;" 'mark-word))





(use-package org-roam
  :hook
  (after-init . org-roam-mode)
  :custom
  (org-roam-buffer "*Notes Collection*")
  (org-roam-buffer-position 'right)
  (org-roam-completion-system 'ivy)
  (org-roam-directory "~/Dropbox/org/notes/")
  (org-roam-tag-sources '(prop last-directory))
  (org-roam-link-title-format "%s")
  (org-roam-capture-templates '(("d" "default" plain (function org-roam--capture-get-point)
                                 "%?"
                                 :file-name "%<%Y%m%d%H%M%S>-${slug}"
                                 :head "#+title: ${title}\n#+roam_alias:\n#+roam_tags:\n\n"
                                 :immediate-finish
                                 :unnarrowed t)
                                ("r" "reference" plain (function org-roam--capture-get-point)
                                 "%?"
                                 :file-name "refs/%<%Y%m%d%H%M%S>-${slug}"
                                 :head "#+title: ${title}\n#+roam_alias:\n#+roam_tags:\n#+roam_key:\n\n"
                                 :immediate-finish
                                 :unnarrowed t)
                                ("m" "medical" plain (function org-roam--capture-get-point)
                                 "%?"
                                 :file-name "med/%<%Y%m%d%H%M%S>-${slug}"
                                 :head "#+title: ${title}\n#+roam_alias:\n#+roam_tags:\n\n"
                                 :immediate-finish
                                 :unnarrowed t)
                                ("x" "dx / ddx" plain (function org-roam--capture-get-point)
                                 (file "~/Dropbox/org/templates/diagnosis.org")
                                 :file-name "med/dx/${slug}"
                                 :head "#+title: ${title}\n#+roam_alias:\n#+roam_tags:\n\n"
                                 :immediate-finish
                                 :unnarrowed t)))
  :custom-face
  (org-roam-link ((t (:family "Optima" :foreground "LightSkyBlue" :weight bold))))
  (org-roam-link-current ((t (:family "Optima" :foreground "LawnGreen" :foreground-distant "cyan"))))
  (org-roam-link-invalid ((t (:family "Optima" :foreground "DarkRed"))))
  :general
  ("C-c f" 'org-roam-find-file
   "C-c i" 'org-roam-insert
   "C-c I" 'org-roam-insert-immediate)
  ("C-c n r" 'org-roam
   "C-c r" 'org-roam))

(use-package org-journal
  :bind
  ("C-c n j" . org-journal-new-entry)
  :custom
  (org-journal-carryover-delete-empty-journal 'always)
  (org-journal-enable-agenda-integration nil)
  (org-journal-date-prefix "#+title: ")
  (org-journal-file-format "%Y-%m-%d.org")
  (org-journal-dir (concat org-roam-directory "dailies/"))
  (org-journal-date-format "%A, %d %B %Y"))


(use-package worf
  :hook (org-mode . worf-mode))

(use-package org-variable-pitch
  :hook (org-mode . org-variable-pitch-minor-mode)
  :custom
  ;; This sets the monospace font used in org-mode
  (org-variable-pitch-fixed-font "Hack Nerd Font")
  ;; Use monospace for stars
  (org-variable-pitch-fontify-headline-prefix nil)
  (org-variable-pitch-fixed-faces '(org-drawer
                                    org-block
                                    org-block-begin-line
                                    org-block-end-line
                                    org-code
                                    org-document-info-keyword
                                    org-done
                                    org-formula
                                    org-indent
                                    org-meta-line
                                    org-special-keyword
                                    org-table
                                    org-todo
                                    org-verbatim
                                    org-date)))

(use-package org-superstar
  ;; Modern replacement for org-bullets
  :hook (org-mode . (lambda () (org-superstar-mode 1)))
  :custom
  (org-superstar-headline-bullets-list '("•"))
  (org-superstar-prettify-item-bullets nil))

(use-package org-fc
  :straight (org-fc
             :type git :host github :repo "l3kn/org-fc"
             :files (:defaults "awk" "demo.org"))
  :config
  (require 'org-fc-hydra)
  :custom
  (org-fc-directories `(,org-roam-directory)))

(use-package anki-editor
  :after org
  :hook
  (org-mode . anki-editor-mode)
  (org-mode . hi-lock-mode)
  :custom
  (anki-editor-create-decks t)
  :config
  (load-file "~/.emacs.d/config/packages/utils.el")
  :general
  (:keymaps 'org-mode-map
            "<f5>" 'my/org-add-cloze
            "<f6>" 'my/formatted-copy
            "C-," 'my/anki-cloze-dwim
            "C-<" #'(lambda () (interactive) (my/anki-cloze-dwim -1))
            "C-c ," 'my/anki-del-cloze-region-or-subtree
            "C-c k" 'my/anki-del-cloze-at-point))

(use-package org-ref
  :config
  (defun my/org-ref-open-pdf-at-point ()
    "Open the pdf for bibtex key under point if it exists."
    (interactive)
    (let* ((results (org-ref-get-bibtex-key-and-file))
           (key (car results))
           (pdf-file (car (bibtex-completion-find-pdf key))))
      (if (file-exists-p pdf-file)
          (org-open-file pdf-file)
        (message "No PDF found for %s" key))))
  :custom
  (reftex-default-bibliography '("~/Dropbox/org/bib/My Library.bib"))
  (org-ref-default-bibliography '("~/Dropbox/org/bib/My Library.bib"))
  (bibtex-completion-pdf-field "File")
  (bibtex-completion-bibliography org-ref-default-bibliography)
  (org-ref-open-pdf-function 'my/org-ref-open-pdf-at-point)
  (org-ref-completion-library 'org-ref-ivy-cite))

(use-package org-pomodoro)

(use-package org-drill
  :disabled
  :requires 'ox)

(use-package org-noter
  :disabled
  :hook
  (org-noter-insert-heading . org-id-get-create))

(use-package org-download
  :disabled
  :demand
  :hook (dired-mode . org-download-enable)
  :custom
  (org-download-display-inline-images nil)
  (org-download-method 'directory)
  (org-download-image-html-width 500)
  (org-download-screenshot-file (expand-file-name "screenshot.jpg" temporary-file-directory))
  (org-download-screenshot-method "screencapture -t 'jpg' -i %s"))





;; Org export backends
;; ====================
(use-package ox-pandoc
  :custom
  (org-pandoc-menu-entry
   '((120 "to docx and open." org-pandoc-export-to-docx-and-open)
     (88 "to docx." org-pandoc-export-to-docx)
     (?k "to markdown." org-pandoc-export-to-markdown)
     (?K "to markdown and open." org-pandoc-export-to-markdown-and-open))))

(use-package ox-hugo
  :disabled
  :after ox)

(use-package ox-cv
  :disabled
  :load-path "~/.emacs.d/config/packages/"
  :after ox)
