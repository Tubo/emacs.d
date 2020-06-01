(defun my/anki-build-card (model deck fields)
  (anki-editor--anki-connect-invoke-result "guiAddCards" `(("note" . (("deckName" . ,deck)
                                                                      ("modelName" . ,model)
                                                                      ("fields" . ,fields))))))

(defun my/anki-build-cloze (deck text &optional context extra)
  (let ((fields `(("Text" . ,text)
                  ("Context" . ,(or context ""))
                  ("Extra" . ,(or extra "")))))
    (my/anki-build-card "Cloze" deck fields)))


(defun my-org-add-cloze ()
  (interactive)
  (let* ((heading (substring-no-properties (org-get-heading t t t t)))
         (buf (org-export-to-buffer 'html "*Formatted Copy*" nil nil t t))
         (html (with-current-buffer buf (buffer-string))))
    (kill-buffer buf)
    (my/anki-build-cloze "Tubo's Anatomy" html heading)))
