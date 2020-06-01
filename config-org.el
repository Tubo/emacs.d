(use-package org
  :pin org
  :ensure org-plus-contrib
  ;; :requires 'org-tempo
  :config
  (add-hook 'org-mode-hook (lambda () (visual-line-mode 1)))
  (add-hook 'org-mode-hook (lambda () (org-variable-pitch-minor-mode 1)))
  (add-hook 'org-mode-hook (lambda () (abbrev-mode 1)))
  (yas-reload-all)
  (add-hook 'org-mode-hook (lambda () (yas-minor-mode 1)))
  (org-clock-persistence-insinuate)
  (setq-default prettify-symbols-alist '(("#+BEGIN_SRC" . "†")
                                         ("#+END_SRC" . "†")
                                         ("#+begin_src" . "†")
                                         ("#+end_src" . "†")
                                         ("#+RESULTS:" . ">")))
  (setq prettify-symbols-unprettify-at-point 'right-edge)
  (add-hook 'org-mode-hook 'prettify-symbols-mode)

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

  ;; Babel
  (org-confirm-babel-evaluate nil)
  (org-src-fontify-natively t)
  (org-src-tab-acts-natively t)


  :custom-face
  (org-drawer ((t (:height 0.8 :foreground "LightSkyBlue" :family "Monaco"))))
  (org-special-keyword ((t (:height 0.8))))
  (org-tag ((t (:height 0.7))))
  (org-done ((t (:foreground "LightGreen"))))
  (org-ellipsis ((t (:underline nil :height 0.5))))

  :general
  ("C-c l" 'org-store-link
   "C-c c" 'org-capture))





(use-package worf
  :hook (org-mode . worf-mode))


(use-package org-variable-pitch
  :hook (org-mode . org-variable-pitch-minor-mode))


(use-package anki-editor
  ;; :quelpa (anki-editor :fetcher github :repo "tubo/anki-editor")
  ;; :quelpa (anki-editor :fetcher file :path "~/Projects/anki-editor")
  :after org
  :load-path "~/Projects/anki-editor/"
  :hook (org-mode . anki-editor-mode)
  :custom
  (anki-editor-create-decks t)
  :config
  (defun formatted-copy ()
    "Export region to HTML, and copy it to the clipboard."
    (interactive)
    (save-window-excursion
      (let* ((buf (org-export-to-buffer 'html "*Formatted Copy*" nil t t t '(:H 1)))
             (html (with-current-buffer buf (buffer-string))))
        (with-current-buffer buf
          (shell-command-on-region
           (point-min)
           (point-max)
           "textutil -stdin -format html -convert rtf -stdout | pbcopy"))
        (kill-buffer buf))))

  (defun anki-editor-cloze-region-auto-incr (&optional arg)
    "Cloze region without hint and increase card number."
    (interactive)
    (anki-editor-cloze-region my-anki-editor-cloze-number "")
    (setq my-anki-editor-cloze-number (1+ my-anki-editor-cloze-number))
    (forward-sexp))

  (defun anki-editor-cloze-region-dont-incr (&optional arg)
    "Cloze region without hint using the previous card number."
    (interactive)
    (anki-editor-cloze-region (1- my-anki-editor-cloze-number) "")
    (forward-sexp))

  (defun anki-editor-reset-cloze-number (&optional arg)
    "Reset cloze number to ARG or 1"
    (interactive)
    (setq my-anki-editor-cloze-number (or arg 1)))

  (defun anki-editor-push-tree ()
    "Push all notes under a tree."
    (interactive)
    (anki-editor-push-notes '(4))
    (anki-editor-reset-cloze-number))

  (anki-editor-reset-cloze-number)

  :general
  (:keymaps 'org-mode-map
            "<f5>" 'anki-editor-cloze-region-auto-incr
            "<f6>" 'anki-editor-cloze-region-dont-incr
            "<f7>" 'anki-editor-reset-cloze-number))

(use-package org-pomodoro
  :disabled)

(use-package org-drill
  :disabled
  :requires 'ox)

(use-package org-noter
  :disabled
  :config
  (add-hook 'org-noter-insert-heading-hook #'org-id-get-create))

(use-package org-bullets
  :disabled
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("✸" "◆" "◉" "○" "▶")))

(use-package org-download
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
  :load-path "~/.emacs.d/personal/packages/"
  :after ox)





;; OS-specific settings
;; ====================
(use-package org
  ;; MacOS settings
  :if (eq system-type 'darwin)
  :config
  :custom
  (org-variable-pitch-fixed-font "Andale Mono")
  :custom-face
  (variable-pitch ((t (:height 1.2 :family "Avenir Next")))))

(use-package org
  ;; Linux settings
  :if (eq system-type 'gnu/linux)
  :custom
  (org-variable-pitch-fixed-font "Sourse Code Variable")
  :custom-face
  (variable-pitch ((t (:height 1.2 :family "Noto Sans")))))

(use-package org
  ;; Windows settings
  :if (eq system-type 'windows-nt)
  :custom
  (org-variable-pitch-fixed-font "Sourse Code Variable")
  :custom-face
  (variable-pitch ((t (:height 1.2 :family "DejaVu Sans")))))
