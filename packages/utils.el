;;; Anki

(defun my/anki-add-note (model deck fields)
  (anki-editor--anki-connect-invoke-result
   "addNote" `(("note" . (("deckName" . ,deck)
                          ("modelName" . ,model)
                          ("fields" . ,fields))))))

(defun my/anki-add-cloze (deck text &optional context extra)
  (let ((fields `(("Text" . ,text)
                  ("Context" . ,(or context ""))
                  ("Extra" . ,(or extra "")))))
    (my/anki-add-note "Cloze" deck fields)))

(defun my/anki-update-note (id fields)
  (anki-editor--anki-connect-invoke-result
   "updateNoteFields" `(("note" . (("id" . ,id)
                                   ("fields" . ,fields))))))

(defun my/anki-update-cloze (id text &optional context extra)
  (let ((fields `(("Text" . ,text)
                  ("Context" . ,(or context ""))
                  ("Extra" . ,(or extra "")))))
    (my/anki-update-note id fields)))

(defun my/org-add-cloze ()
  "Personal useful function."
  (interactive)
  (let ((context (read-string "Context: " (car minibuffer-history) '(minibuffer-history . 0))))
    (save-window-excursion
      (save-restriction
        (org-narrow-to-subtree)
        (let* ((deck (org-entry-get-with-inheritance "ANKI_DECK"))
               (id (org-entry-get (point) "ANKI_CARD_ID"))
               (buf (org-export-to-buffer 'html "*Formatted Copy*" nil nil t t))
               (html (with-current-buffer buf (buffer-string))))
          (kill-buffer buf)
          (if id
              (my/anki-update-cloze (string-to-number id) html context)
            (setq id (my/anki-add-cloze "Tubo's Anatomy::Upper limb" html context))
            (org-set-property "ANKI_CARD_ID" (number-to-string id))))))))


;;; Anki GUI control

(defun my/anki-query-browse (query)
  (anki-editor--anki-connect-invoke-result
   "guiBrowse" `(("query" . ,query))))

(defun my/anki-current-card ()
  (anki-editor--anki-connect-invoke-result
   "guiCurrentCard" '()))

(defun my/formatted-copy ()
  "Export region to HTML, and copy it to the clipboard."
  (interactive)
  (save-window-excursion
    (save-restriction
      (unless (use-region-p)
        (org-back-to-heading)
        (org-narrow-to-subtree))
      (let* ((buf (org-export-to-buffer 'html "*Formatted Copy*" nil nil t t '(:H 0)))
             (html (with-current-buffer buf (buffer-string))))
        (with-current-buffer buf
          (shell-command-on-region
           (point-min)
           (point-max)
           "textutil -stdin -format html -convert rtf -stdout | pbcopy"
           ;; "pandoc -f html -t rtf | pbcopy -Prefer rtf"
           ))
        (kill-buffer buf))))
  (message "Content copied as html."))


;;; Add cloze for text

(defun my/find-largest-cloze ()
  "Find the largest number of cloze in a subtree."
  (save-excursion
    (save-restriction
      (let (current
            (largest 0))
        (org-back-to-heading)
        (org-narrow-to-subtree)
        (while (re-search-forward "{{c" nil t)
          (setq current (thing-at-point 'number))
          (if (> current largest)
              (setq largest current)))
        largest))))


(defun my/anki-cloze (begin end arg)
  "Cloze region from BEGIN to END with number ARG."
  (let ((region (buffer-substring begin end)))
    (save-excursion
      (delete-region begin end)
      (insert (with-output-to-string
                (princ (format "{{c%d::%s" (or arg 1) region))
                (princ "}}"))))))

(defun my/anki-cloze-dwim (arg)
  "Cloze region without hint and set cloze number by ARG."
  (interactive "p")
  (let ((largest (my/find-largest-cloze)))
    (unless (use-region-p)
      (unless (cl-search (string (preceding-char)) "\n ({[")
        (backward-word))
      (mark-word))
    (cond ((= arg 1)
           (my/anki-cloze (region-beginning) (region-end) (1+ largest)))
          ((> arg 1)
           (my/anki-cloze (region-beginning) (region-end) arg))
          ((= arg -1)
           (my/anki-cloze (region-beginning) (region-end) largest))))
  (forward-sexp)
  (forward-to-word 1))

(defun my/anki-del-cloze-at-point ()
  (interactive)
  (when (string= (string (following-char)) "{")
    (forward-char 2))
  (search-backward "{{")
  (zap-to-char 2 (string-to-char ":"))
  (search-forward "}}")
  (delete-backward-char 2))

(defun my/anki-del-cloze-region-or-subtree ()
  (interactive)
  (save-excursion
    (save-restriction
      (if (use-region-p)
          (narrow-to-region (region-beginning) (region-end))
        (org-narrow-to-subtree))
      (goto-char (point-min))
      (while (search-forward-regexp "{{c.*?}}" nil t)
        (my/anki-del-cloze-at-point)))))

(defun my/anki-reorder-cloze-number ()
  (interactive)
  (save-excursion
    (save-restriction
      (org-back-to-heading)
      (org-narrow-to-subtree)
      (let (point cur (prev 0) (count 0))
        (while (re-search-forward "{{c\\([0-9]*\\)" nil t)
          (setq cur (string-to-number (match-string 1)))
          (replace-match (number-to-string
                          (if (= prev cur)
                              count
                            (setq count (1+ count)))) nil nil nil 1)
          (setq prev cur))))))
