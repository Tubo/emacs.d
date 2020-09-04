

;; Autocompletion

(use-package company
  :ghook 'prog-mode-hook 'LaTeX-mode 'latex-mode
  :config
  (defvar completion-at-point-functions-saved nil)

  (defun my/yas-expand-next-field-complete ()
    (interactive)
    (if yas-minor-mode
        (let ((old-point (point))
              (old-tick (buffer-chars-modified-tick)))
          (yas-expand)
          (when (and (eq old-point (point))
                     (eq old-tick (buffer-chars-modified-tick)))
            (ignore-errors (yas-next-field))
            (when (and (eq old-point (point))
                       (eq old-tick (buffer-chars-modified-tick)))
              (company-complete-common))))
      (company-complete-common)))

  (setq company-backends (remove 'company-ropemacs company-backends))

  ;; Usage based completion sorting
  (defun my/company-elisp-setup ()
    (set (make-local-variable 'company-backends)
         '((company-capf :with company-dabbrev-code))))
  (use-package company-statistics
    :hook ((emacs-lisp-mode lisp-interaction-mode) . my/company-elisp-setup)
    :config (company-statistics-mode))

  :custom
  (company-idle-delay 0.3)
  (company-tooltip-limit 10)
  (company-minimum-prefix-length 2)
  (tab-always-indent 'complete)
  :general
  (:keymaps 'company-active-map
            "TAB" 'my/yas-expand-next-field-complete))

(use-package company-tabnine
  :after company
  :custom
  (company-tabnine-max-num-results 9)
  (company-idle-delay 0)
  (company--show-numbers t)
  :bind
  (("M-q" . company-other-backend))
  :hook
  (lsp-after-open . (lambda ()
                      (setq company-tabnine-max-num-results 3)
                      (add-to-list 'company-transformers 'company//sort-by-tabnine t)
                      (add-to-list 'company-backends '(company-lsp :with company-tabnine :separate))))
  (kill-emacs . company-tabnine-kill-process)
  :config
  ;; Enable TabNine on default
  (add-to-list 'company-backends #'company-tabnine)

  ;; Integrate company-tabnine with lsp-mode
  (defun company//sort-by-tabnine (candidates)
    (if (or (functionp company-backend)
            (not (and (listp company-backend) (memq 'company-tabnine company-backend))))
        candidates
      (let ((candidates-table (make-hash-table :test #'equal))
            candidates-lsp
            candidates-tabnine)
        (dolist (candidate candidates)
          (if (eq (get-text-property 0 'company-backend candidate)
                  'company-tabnine)
              (unless (gethash candidate candidates-table)
                (push candidate candidates-tabnine))
            (push candidate candidates-lsp)
            (puthash candidate t candidates-table)))
        (setq candidates-lsp (nreverse candidates-lsp))
        (setq candidates-tabnine (nreverse candidates-tabnine))
        (nconc (seq-take candidates-tabnine 3)
               (seq-take candidates-lsp 6))))))

(use-package company-box
  :diminish
  :after company
  :functions (my-company-box--make-line
              my-company-box-icons--elisp)
  :commands (company-box--get-color
             company-box--resolve-colors
             company-box--add-icon
             company-box--apply-color
             company-box--make-line
             company-box-icons--elisp)
  :hook (company-mode . company-box-mode)
  :custom
  (company-box-backends-colors nil)
  (company-box-show-single-candidate t)
  (company-box-max-candidates 50)
  (company-box-doc-delay 0.3)
  :config
  ;; Support `company-common'
  (defun my-company-box--make-line (candidate)
    (-let* (((candidate annotation len-c len-a backend) candidate)
            (color (company-box--get-color backend))
            ((c-color a-color i-color s-color) (company-box--resolve-colors color))
            (icon-string (and company-box--with-icons-p (company-box--add-icon candidate)))
            (candidate-string (concat (propertize (or company-common "") 'face 'company-tooltip-common)
                                      (substring (propertize candidate 'face 'company-box-candidate) (length company-common) nil)))
            (align-string (when annotation
                            (concat " " (and company-tooltip-align-annotations
                                             (propertize " " 'display `(space :align-to (- right-fringe ,(or len-a 0) 1)))))))
            (space company-box--space)
            (icon-p company-box-enable-icon)
            (annotation-string (and annotation (propertize annotation 'face 'company-box-annotation)))
            (line (concat (unless (or (and (= space 2) icon-p) (= space 0))
                            (propertize " " 'display `(space :width ,(if (or (= space 1) (not icon-p)) 1 0.75))))
                          (company-box--apply-color icon-string i-color)
                          (company-box--apply-color candidate-string c-color)
                          align-string
                          (company-box--apply-color annotation-string a-color)))
            (len (length line)))
      (add-text-properties 0 len (list 'company-box--len (+ len-c len-a)
                                       'company-box--color s-color)
                           line)
      line))
  (advice-add #'company-box--make-line :override #'my-company-box--make-line)

  ;; Prettify icons
  (defun my-company-box-icons--elisp (candidate)
    (when (derived-mode-p 'emacs-lisp-mode)
      (let ((sym (intern candidate)))
        (cond ((fboundp sym) 'Function)
              ((featurep sym) 'Module)
              ((facep sym) 'Color)
              ((boundp sym) 'Variable)
              ((symbolp sym) 'Text)
              (t . nil)))))
  (advice-add #'company-box-icons--elisp :override #'my-company-box-icons--elisp)

  (declare-function all-the-icons-faicon 'all-the-icons)
  (declare-function all-the-icons-material 'all-the-icons)
  (declare-function all-the-icons-octicon 'all-the-icons)
  (setq company-box-icons-all-the-icons
        `((Unknown . ,(all-the-icons-material "find_in_page" :height 0.85 :v-adjust -0.2))
          (Text . ,(all-the-icons-faicon "text-width" :height 0.8 :v-adjust -0.05))
          (Method . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.05 :face 'all-the-icons-purple))
          (Function . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.05 :face 'all-the-icons-purple))
          (Constructor . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.05 :face 'all-the-icons-purple))
          (Field . ,(all-the-icons-octicon "tag" :height 0.8 :v-adjust 0 :face 'all-the-icons-lblue))
          (Variable . ,(all-the-icons-octicon "tag" :height 0.8 :v-adjust 0 :face 'all-the-icons-lblue))
          (Class . ,(all-the-icons-material "settings_input_component" :height 0.85 :v-adjust -0.2 :face 'all-the-icons-orange))
          (Interface . ,(all-the-icons-material "share" :height 0.85 :v-adjust -0.2 :face 'all-the-icons-lblue))
          (Module . ,(all-the-icons-material "view_module" :height 0.85 :v-adjust -0.2 :face 'all-the-icons-lblue))
          (Property . ,(all-the-icons-faicon "wrench" :height 0.8 :v-adjust -0.05))
          (Unit . ,(all-the-icons-material "settings_system_daydream" :height 0.85 :v-adjust -0.2))
          (Value . ,(all-the-icons-material "format_align_right" :height 0.85 :v-adjust -0.2 :face 'all-the-icons-lblue))
          (Enum . ,(all-the-icons-material "storage" :height 0.85 :v-adjust -0.2 :face 'all-the-icons-orange))
          (Keyword . ,(all-the-icons-material "filter_center_focus" :height 0.85 :v-adjust -0.2))
          (Snippet . ,(all-the-icons-material "format_align_center" :height 0.85 :v-adjust -0.2))
          (Color . ,(all-the-icons-material "palette" :height 0.85 :v-adjust -0.2))
          (File . ,(all-the-icons-faicon "file-o" :height 0.85 :v-adjust -0.05))
          (Reference . ,(all-the-icons-material "collections_bookmark" :height 0.85 :v-adjust -0.2))
          (Folder . ,(all-the-icons-faicon "folder-open" :height 0.85 :v-adjust -0.05))
          (EnumMember . ,(all-the-icons-material "format_align_right" :height 0.85 :v-adjust -0.2 :face 'all-the-icons-lblue))
          (Constant . ,(all-the-icons-faicon "square-o" :height 0.85 :v-adjust -0.05))
          (Struct . ,(all-the-icons-material "settings_input_component" :height 0.85 :v-adjust -0.2 :face 'all-the-icons-orange))
          (Event . ,(all-the-icons-faicon "bolt" :height 0.8 :v-adjust -0.05 :face 'all-the-icons-orange))
          (Operator . ,(all-the-icons-material "control_point" :height 0.85 :v-adjust -0.2))
          (TypeParameter . ,(all-the-icons-faicon "arrows" :height 0.8 :v-adjust -0.05))
          (Template . ,(all-the-icons-material "format_align_center" :height 0.85 :v-adjust -0.2)))
        company-box-icons-alist 'company-box-icons-all-the-icons))


;; LSP

(use-package lsp-mode
  :hook ((python-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp
  :custom
  (lsp-keymap-prefix "s-z"))

(use-package lsp-ui
  :commands lsp-ui-mode
  :general
  (:keymaps 'lsp-ui-mode-map
            [remap xref-find-definitions] 'lsp-ui-peek-find-definitions
            [remap xref-find-references] 'lsp-ui-peek-find-references))
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package dap-mode
  :hook (dap-stopped . (lambda (arg) (call-interactively #'dap-hydra)))
  :config
  (use-package dap-python
    :straight nil))




;; Emacs Lisp

(use-package lispy
  :hook
  (emacs-lisp-mode . lispy-mode )
  :config
  (defun my/conditionally-enable-lispy ()
    (when (eq this-command 'eval-expression)
      (lispy-mode 1)))
  (add-hook 'minibuffer-setup-hook 'my/conditionally-enable-lispy)
  :general
  (:keymaps 'lispy-mode-map
            "\"" 'lispy-doublequote))

(use-package elisp-refs)

(use-package elisp-slime-nav
  :hook ((emacs-lisp-mode ielm-mode) . elisp-slime-nav-mode))

(use-package highlight-defined
  :hook (emacs-lisp-mode . highlight-defined-mode))

(use-package rainbow-delimiters
  :hook (emacs-lisp-mode . rainbow-delimiters-mode))

(use-package eldoc
  :hook (emacs-lisp-mode . eldoc-mode)
  :hook (lisp-interaction-mode-hook . eldoc-mode))

(use-package page-break-lines
  ;; Display ^L (C-q C-l) page breaks as tidy horizontal lines
  :config
  (global-page-break-lines-mode))

(use-package eval-sexp-fu
  :init
  (setq eval-sexp-fu-flash-duration 0.4)
  :config
  (turn-on-eval-sexp-fu-flash-mode)
  :general
  (:keymaps 'emacs-lisp-mode-map
            "C-c C-c" 'eval-sexp-fu-eval-sexp-inner-list
            "C-c C-e" 'eval-sexp-fu-eval-sexp-inner-sexp)
  (:keymaps 'lisp-interaction-mode-map
            "C-c C-c" 'eval-sexp-fu-eval-sexp-inner-list
            "C-c C-e" 'eval-sexp-fu-eval-sexp-inner-sexp))

(use-package eros
  :hook (emacs-lisp-mode . eros-mode )
  :hook (lisp-interaction-mode . eros-mode))

(use-package ielm
  :config
  (add-hook 'inferior-emacs-lisp-mode-hook
            (lambda ()
              (turn-on-eldoc-mode))))

(use-package ipretty
  :config (ipretty-mode t))




;; Python
(use-package pyvenv
  :config
  :ghook 'python-mode)




;; Haskell

(use-package haskell-mode
  :custom
  (haskell-stylish-on-save t))

(use-package dante
  :after haskell-mode
  :hook
  (haskell-mode . dante-mode)
  (haskell-mode . flycheck-mode)
  :config
  ;; (add-to-list dante-methods-alist '(stack-ghci "stack ghci") t)
  )




;; Elm

(use-package elm-mode
  :disabled
  :init
  (setq elm-tags-on-save t))




;; Json

(use-package json-mode
  :mode "\\.json\\'")

;; Yaml
(use-package yaml-mode
  :mode "\\.yml\\'"
  :general
  (:keymaps 'yaml-mode-map
            "C-m" 'newline-and-indent))


