(require 'request)

(defvar my/anki--ext-plist
  '(
    :export-options (visible-only)
    :section-numbers nil
    :headline-numbering nil
    :headline-offset 1
    :headline-levels 1
    :with-todo-keywords nil
    :with-toc nil
    :with-tasks ("DONE" "ANKI"))
  "Options for html export")

(defun my/anki--connect-action (action &optional params version)
  (let (a)
    (when version
      (push `(version . ,version) a))
    (when params
      (push `(params . ,params) a))
    (push `(action . ,action) a)))

(defun my/anki--connect-invoke-queue ()
  (let (action-queue)
    (lambda (&optional action params handler)
      (if action
          (push (cons (my/anki--connect-action action params) handler) action-queue)
        (when action-queue
          (apply #'my/anki--connect-invoke-multi (nreverse action-queue))
          (setq action-queue nil))))))

(defmacro my/anki--connect-invoke-result (&rest args)
  "Invoke AnkiConnect with ARGS, return the result from response or raise an error."
  `(let-alist (my/anki--connect-invoke ,@args)
     (when .error (error .error))
     .result))

(defun my/anki--connect-invoke-multi (&rest actions)
  (-zip-with (lambda (result handler)
               (when-let ((_ (listp result))
                          (err (alist-get 'error result)))
                 (error err))
               (and handler (funcall handler result)))
             (my/anki--connect-invoke-result
              "multi" `((actions . ,(mapcar #'car actions))))
             (mapcar #'cdr actions)))

(defun my/anki--connect-invoke (action &optional params)
  "Invoke AnkiConnect with ACTION and PARAMS."
  (let ((request-body (json-encode (my/anki--connect-action action params 6)))
        (request-backend 'curl)
        (json-array-type 'list)
        reply err)

    (let ((response (request "http://127.0.0.1:8765"
                      :type "POST"
                      :parser 'json-read
                      :data request-body
                      :success (cl-function (lambda (&key data &allow-other-keys)
                                              (setq reply data)))
                      :error (cl-function (lambda (&key _ &key error-thrown &allow-other-keys)
                                            (setq err (string-trim (cdr error-thrown)))))
                      :sync t)))

      ;; HACK: With sync set to t, `request' waits for curl process to
      ;; exit, then response data becomes available, but callbacks
      ;; might not be called right away but at a later time, that's
      ;; why here we manually invoke callbacks to receive the result.
      (unless (request-response-done-p response)
        (request--curl-callback (get-buffer-process (request-response--buffer response)) "finished\n")))

    (when err (error "Error communicating with AnkiConnect using cURL: %s" err))
    (or reply (error "Got empty reply from AnkiConnect"))))

(defun my/anki--connect-map-note (note)
  "Convert NOTE to the form that AnkiConnect accepts."
  (let-alist note
    (list (cons "id" .note-id)
          (cons "deckName" .deck)
          (cons "modelName" .note-type)
          (cons "fields" .fields)
          ;; Convert tags to a vector since empty list is identical to nil
          ;; which will become None in Python, but AnkiConnect requires it
          ;; to be type of list.
          (cons "tags" (vconcat .tags)))))

(defun my/anki--add-note (model deck fields)
  (my/anki--connect-invoke-result
   "addNote" `(("note" . (("deckName" . ,deck)
                          ("modelName" . ,model)
                          ("fields" . ,fields))))))

(defun my/anki--add-cloze (deck text &optional context extra)
  (let ((fields `(("Text" . ,text)
                  ("Context" . ,(or context ""))
                  ("Extra" . ,(or extra "")))))
    (my/anki--add-note "Tubo's Cloze" deck fields)))

(defun my/anki--update-note (id fields)
  (my/anki--connect-invoke-result
   "updateNoteFields" `(("note" . (("id" . ,id)
                                   ("fields" . ,fields))))))

(defun my/anki--update-cloze (id text &optional context extra)
  (let ((fields `(("Text" . ,text)
                  ("Context" . ,(or context ""))
                  ("Extra" . ,(or extra "")))))
    (my/anki--update-note id fields)))

(defun my/anki-parent-heading ()
  "Return the heading of the parent, nil if top level."
  (save-excursion
    (org-up-heading-safe)
    (org-entry-get (point) "ITEM")))

(defun my/anki-add-cloze ()
  "Push the current heading or region to Anki."
  (interactive)
  (let* ((context-property (org-entry-get-with-inheritance "ANKI_CLOZE_CONTEXT"))
         (context-parent (my/anki-parent-heading))
         (context (or context-property
                      (read-string "Context: " context-parent '(minibuffer-history . 0)))))
    (save-excursion
      (save-restriction
        (org-narrow-to-subtree)
        (my/anki-reorder-cloze-number)
        (let* ((deck (org-entry-get-with-inheritance "ANKI_DECK"))
               (id (org-entry-get (point) "ANKI_NOTE_ID"))
               (body-end (save-excursion
                           (org-next-visible-heading 1)
                           (point)))
               (content (buffer-substring (point-min) body-end))
               (html (org-export-string-as content 'html t my/anki--ext-plist)))
          (if id
              (my/anki--update-cloze (string-to-number id) html context)
            (setq id (my/anki--add-cloze deck html context))
            (org-set-property "ANKI_NOTE_ID" (number-to-string id)))))
      (recenter 10 t))))



;;; Anki GUI control

(defun my/anki--gui-query-browse (query)
  (my/anki--connect-invoke-result
   "guiBrowse" `(("query" . ,query))))

(defun my/anki--gui-current-card ()
  (my/anki--connect-invoke-result
   "guiCurrentCard" '()))

(defun my/anki--gui-current-card-id ()
  (alist-get 'cardId (my/anki--gui-current-card)))

(defun my/anki-sync-note-id ()
  "Updates the current note with the current card on Anki GUI."
  (interactive)
  (let ((id (my/anki--gui-current-card-id)))
    (org-set-property "ANKI_NOTE_ID" (number-to-string id))))

(defun my/anki-goto-current-note ()
  (interactive)
  (let ((id (my/anki--gui-current-card-id)))
    (push-mark)
    (goto-char (point-min))
    (search-forward (number-to-string id) nil t 1) ))


(defun my/formatted-copy (args)
  "Export region or subtree to HTML, and then copy it to the clipboard.
If ARGS is non-nil, export subtree as visible only (slower) without clozes."
  (interactive "P")
  (save-window-excursion
    (save-restriction
      (unless (use-region-p)
        (org-back-to-heading)
        (org-narrow-to-subtree))
      (my/anki-reorder-cloze-number)
      (if args                        ; to export a non-clozed version
          (let ((buf (org-export-to-buffer 'html "*Formatted Copy*" nil nil t t
                                           '(:headline-levels 1 :headline-offset 2))))
            (with-current-buffer buf
              (mark-whole-buffer)
              (my/anki-del-cloze-region-or-subtree)
              (shell-command-on-region (point-min) (point-max)
                                       "textutil -stdin -format html -convert rtf -stdout | pbcopy")))
        (save-excursion
          (let* ((content (if (use-region-p)
                              (buffer-substring (region-beginning) (region-end))
                            (org-next-visible-heading 1)
                            (buffer-substring (point-min) (point))))
                 (output (org-export-string-as content 'html t my/anki--ext-plist)))
            (shell-command
             (format "echo \"%s\" | textutil -stdin -format html -convert rtf -stdout | pbcopy" output)))))))
  (message "Content copied as html."))

;;; Add cloze for text

(defun my/anki--get-largest-cloze-id ()
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


(defun my/anki--cloze-region (begin end arg)
  "Cloze region from BEGIN to END with number ARG."
  (let ((region (buffer-substring begin end)))
    (save-excursion
      (delete-region begin end)
      (insert (with-output-to-string
                (princ (format "{{c%d::%s" (or arg 1) region))
                (princ "}}"))))))

(defun my/anki-cloze-dwim (arg)
  "Cloze region without hint and set cloze number by ARG.
If within a cloze already, then recloze the point with the old ID."
  (interactive "p")
  (save-restriction
    (org-narrow-to-subtree)
    (let* ((largest (my/anki--get-largest-cloze-id))
           (within-clozep (my/anki--within-clozep))
           (cloze-id (my/anki--current-cloze-id)))

      (when within-clozep
        ;; Delete the old cloze if the point is within a cloze already.
        (my/anki-del-cloze-at-point))

      (unless (use-region-p)
        (unless (string-match "[^a-z0-9A-Z]" (string (preceding-char)))
          (forward-to-word 1))
        (mark-word))

      (my/anki--cloze-region (region-beginning) (region-end)
                             (cond (cloze-id cloze-id) ; use the current-cloze if able
                                   ((= arg 1) (1+ largest))
                                   ((> arg 1) arg)
                                   ((= arg -1) largest)))))
  (forward-sexp)
  (backward-to-word 1))

(defun my/anki--within-clozep ()
  "Return t if the point is currently within a cloze marking."
  (save-excursion
    (save-restriction
      (org-narrow-to-subtree)
      (let* ((init (point))
             (open (search-backward "{{c" nil t 1))
             (close (search-forward "}}" nil t 1)))
        (and (and open close)           ; both braces must be present
             (< open init)
             (< init close))))))

(defun my/anki--current-cloze-id ()
  "Return the id if the point is within a cloze, otherwise nil."
  (save-excursion
    (when (my/anki--within-clozep)
      (search-backward "{{c" nil t)
      (search-forward "{{c" nil t)
      (thing-at-point 'number))))

(defun my/anki-del-cloze-at-point ()
  (interactive)
  (save-excursion
    (when (string= (string (following-char)) "{")
      (forward-char 2))
    (search-backward "{{")
    (zap-to-char 2 (string-to-char ":"))
    (search-forward "}}")
    (delete-backward-char 2)))

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
      (let (point cur (prevs '()) (count 0))
        (while (re-search-forward "{{c\\([0-9]*\\)" nil t)
          (setq cur (string-to-number (match-string 1)))
          (unless (member cur prevs) (push cur prevs))
          (replace-match (number-to-string
                          (- (length prevs) (cl-position cur prevs))) nil nil nil 1)
          (setq prev cur))))))
