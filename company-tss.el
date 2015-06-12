;;; Many things still left to be desired.
;;;
;;; TODO
;;; Caching according to lookup. Some lookup is too slow, we should use the
;;; "type" string returned from "quickInfo" to create a hash that have necessary
;;; info cached (great for built-in global, maybe we should have them preloaded
;;; into environment anyway). Company mode feels much slower than AC on
;;; document, body stuff...

(require 'tss)

(require 'cl-lib)
(require 'dash)
(require 's)

;;;: Helpers

;;; These two variables serve to boost performance (not strictly necessary)
(defvar-local company-tss-candidates-info-cache (make-hash-table :test #'equal)
  "An info candidates cache(hash) to hold data for this completion.

NOT used yet.")

;;; TODO using Unicode char to make signs look better
(defun company-tss--get-sign (kind)
  "Return a symbolic sign for KIND"
  (let ((kind (tss--stringify-response-element kind)))
    (cond ((member kind '("keyword" "builtin-keyword"))  "w")
          ((string= kind "primitive type")               "p")
          ((string= kind "module")                       "m")
          ((string= kind "interface")                    "i")
          ((string= kind "class")                        "c")
          ((member kind '("var" "property" "parameter")) "v")
          ((tss--function-kind-p kind)                   "f")
          ((string= kind "getter")                       "g")
          ((string= kind "type")                         "t")
          ((string= kind "local var")                    "l")
          ((string= kind "unknown")                      "")
          (t
           (warn "found unknown server response for kind : %s" kind)
           ""))))

;;; TODO too basic, too cumbersome, we need better support from ts-tools
;;; May-28-2015 14:49:44 CST: actually for a start, we can use `typescript-mode'
;;; to get a decent colorization. Unfortunately, `typescript-mode' colorization
;;; is too basic for now.
(defun company-tss--colorize-type (name sign type)
  "Use regexp to colorize TYPE. Return colorized type."
  (if (or (string-empty-p sign)
          (not (member sign '("f" "v"))))
      ""
    (let ((desc type)
          (start 0))
      ;; colorize property
      (when (string-match "(\\([^ \t]+\\))" desc start)
        (add-face-text-property (match-beginning 1)
                                (match-end 1)
                                'font-lock-preprocessor-face
                                nil desc)
        (setq start (match-end 0)))
      ;; colorize candidate
      (when (string-match name desc start)
        (add-face-text-property (match-beginning 0)
                                (match-end 0)
                                'font-lock-function-name-face
                                nil desc)
        (setq start (match-end 0)))
      ;; colorize params or type
      (pcase sign
        ("f"
         ;; colorize params
         (while (and (< start (length desc))
                     (string-match
                      "\\(?:\\([a-zA-Z0-9_]+\\)\\|\)\\): [ \t]*\\([a-zA-Z0-9_]+\\)"
                      desc start))
           (when (match-beginning 1)
             (add-face-text-property (match-beginning 1)
                                     (match-end 1)
                                     'font-lock-variable-name-face
                                     nil desc))
           (when (match-beginning 2)
               (add-face-text-property (match-beginning 2)
                                    (match-end 2)
                                    'font-lock-type-face
                                    nil desc))
           (setq start (match-end 0))))
        ;; colorize variable type
        ("v"
         (when (string-match ": [ \t]*\\([a-zA-Z0-9_]+\\)"
                             desc start)
           (add-face-text-property (match-beginning 1)
                                   (match-end 1)
                                   'font-lock-type-face
                                   nil desc))))
      desc)))

(defun company-tss--format-document (name kind type doc)
  "Format a documentation for `company-doc'."
  (let* ((sign (company-tss--get-sign kind))
         (kind (upcase (tss--stringify-response-element kind)))
         (type (company-tss--colorize-type name sign
                                           (or type "unknown")))
         (doc (or doc ""))
         (typedesc (pcase sign
                     ("w" "")
                     ("f" (concat (propertize "Signature: "
                                              'face 'apropos-symbol)
                                  type "\n\n"))
                     ("v" (concat (propertize "Type: "
                                              'face 'apropos-symbol)
                                  type "\n\n")))))
    (setq name (propertize name 'face 'font-lock-keyword-face))
    (setq kind (propertize kind 'face 'font-lock-preprocessor-face))
    (concat name " is " kind ".\n\n"
            typedesc
            (propertize "Comment: \n" 'face 'info-title-4)
            doc "\n")))

;;;: Prefix
(defun company-tss-get-prefix ()
  (when (company-tss--code-point?)
    ;; As noted in `tss--company-get-member-candates', the exact prefix doesn't
    ;; matter to tss, but the company need the prefix to correctly insert
    ;; candidates.
    (let ((start (-some #'company-tss--get-re-prefix
                        '( ;; member
                          "\\.\\([a-zA-Z0-9_]*\\)"
                          ;; type
                          ": ?\\([a-zA-Z0-9_]*\\)"
                          ;; new
                          "\\<new +\\([a-zA-Z0-9_]*\\)"
                          ;; extends
                          " +extends +\\([a-zA-Z0-9_]*\\)"
                          ;; implements
                          " +implements +\\([a-zA-Z0-9_]*\\)"
                          ;; tag
                          "[^/] *<\\([a-zA-Z0-9_]*\\)"
                          ;; anything
                          ;; TODO what is this?
                          "\\(?:^\\|[^a-zA-Z0-9_.]\\) *\\([a-zA-Z0-9_]+\\)"))))
      (when start
        (buffer-substring-no-properties start (point))))))

(defun company-tss--get-re-prefix (re)
  "Get prefix matching regular expression RE."
  (save-excursion
    (when (re-search-backward (concat re "\\=") nil t)
      (or (match-beginning 1)
          (match-beginning 0)))))

;; TODO the following uses text faces, not very reliable. Use parsing
;; facilities.
(defun company-tss--code-point? ()
  "Check whether current point is a code point, not within
comment or string literals."
  (let ((fc (get-text-property (point) 'face)))
    (not (memq fc '(font-lock-comment-face
                    font-lock-string-face)))))

;;;: Candidates
(defun company-tss-get-candidates (&optional prefix)
  "Retrieve completion candidates for current point.

NOTE: PREFIX is NOT passed to tss, the tss can figure this
out according to file position directly."
  (mapcar (lambda (e)
            (let ((name (tss-utils/assoc-path e 'name))
                  (kind (tss-utils/assoc-path e 'kind)))
              (propertize name
                          :annotation (company-tss--get-sign kind)
                          ;; TODO for unknown reason, `kind' returned from
                          ;; `get-doc' is different from `get-completions',
                          ;; which seems to a more appropriate one, very
                          ;; weird..... So pass this value through `:kind'
                          ;; property.
                          :kind kind)))
          (tss-utils/assoc-path (tss--get-completions) 'entries)))

;;;: Meta
(defun company-tss-get-meta (candidate)
  (let ((ret (get-text-property 0 :meta candidate)))
    (if ret ret
      (company-tss-sync-get-data candidate)
      (company-tss-get-meta candidate))))

;;;: Doc
(defun company-tss-get-doc (candidate)
  (let ((ret (get-text-property 0 :doc candidate)))
    (if ret ret
      (company-tss-sync-get-data candidate)
      (company-tss-get-doc candidate))))

;;; TODO have some idle/async way to fetch info about candidates in the
;;; background
;;; TODO more info like: definition, location, even script snippets, references and etc.
;;;
;;; BUG The following doesn't do well with advanced types like interface, in
;;; fact I think the ts-tools return something too ambiguous....
(defun company-tss-sync-get-data (candidate)
  (tss--active-test)
  (let ((doc (let ((client tss--client)
                   (updated-source (company-tss--get-source-with-candidate candidate)))
               (tss-client/set-buffer client (current-buffer))
               (tss-client/sync-buffer-content client
                                               (cdr (assoc 'source updated-source))
                                               (cdr (assoc 'linecount updated-source)))
               (tss-client/get-doc client
                                   (cdr (assoc 'line updated-source))
                                   (cdr (assoc 'column updated-source))))))
    ;; use text properties to carry info
    (add-text-properties 0 (length candidate)
                         (let ((kind (get-text-property 0 :kind candidate))
                               (type (tss-utils/assoc-path doc 'type))
                               (doc-comment (tss-utils/assoc-path doc 'docComment)))
                           ;; TODO whether we really need format-meta? the
                           ;; following way can also enjoy colorization from
                           ;; format-document.
                           `(:meta ,type
                                   :doc
                                   ,(company-tss--format-document
                                     candidate kind type doc-comment)))
                         candidate)))

(defun company-tss--get-source-with-candidate (candidate)
  "Get new source by changing the current buffer content with CANDIDATE inserted.

Return an assoc list:
    ((line . <line number after candidate inserted>)
     (column . <column number after candidate inserted>)
     (linecount . <total line count after candidate inserted>)
     (source . <updated source>))."
  (let ((curbuf (current-buffer))
        (prefix company-prefix)
        (curpt (point)))
    (with-temp-buffer
      (insert-buffer-substring curbuf)
      (goto-char curpt)
      ;; to insert candidate correctly have `company-prefix' setup.
      (setq company-prefix prefix)
      (company--insert-candidate candidate)
      ;; result
      (list `(line . ,(line-number-at-pos))
            `(column . ,(current-column))
            `(linecount . ,(count-lines (point-min) (point-max)))
            `(source . ,(buffer-string))))))

;;;: Annotation
(defun company-tss-get-annotation (candidate)
  (format " (%s)" (get-text-property 0 :annotation candidate)))

(defun company-tss (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-tss))
    (prefix (and (tss--active?)
                 (company-tss-get-prefix)))
    (candidates (company-tss-get-candidates arg))
    (meta (company-tss-get-meta arg))
    (doc-buffer (company-doc-buffer (company-tss-get-doc arg)))
    ;; TODO better formatting for annotations
    (annotation (company-tss-get-annotation arg))))

(provide 'company-tss)
