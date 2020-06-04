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
    (let* ((buf (org-export-to-buffer 'html "*Formatted Copy*" nil t t t '(:H 1)))
           (html (with-current-buffer buf (buffer-string))))
      (with-current-buffer buf
        (shell-command-on-region
         (point-min)
         (point-max)
         "textutil -stdin -format html -convert rtf -stdout | pbcopy"))
      (kill-buffer buf))))


;;; Add cloze for text

(defun my/find-largest-cloze ()
  (save-excursion
    (let (current
          (largest 0))
      (org-back-to-heading)
      (while (re-search-forward "{{c" nil t)
        (setq current (thing-at-point 'number))
        (if (> current largest)
            (setq largest current)))
      largest)))

(defun anki-editor-cloze-region-auto-incr (arg)
  "Cloze region without hint and increase card number."
  (interactive "p")
  (let ((largest (my/find-largest-cloze)))
    (unless (region-active-p)
      (unless (cl-search (string (preceding-char)) " (") (backward-word))
      (mark-word))
    (cond ((= arg 1)
           (anki-editor-cloze-region (1+ largest) ""))
          ((> arg 1)
           (anki-editor-cloze-region arg ""))
          ((= arg -1)
           (anki-editor-cloze-region largest ""))))
  (forward-sexp))
