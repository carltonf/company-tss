;;; Many things still left to be desired.
;;;
;;; TODO
;;; 
;;; 1. DONE better `tss--sync-server', modify the source sent on-the-fly to
;;; include newly selected candidate, so we can have access to info from
;;; "definition", "references" and etc. If we can have this, we can always use
;;; "completions-brief" to speed completions up greatly.
;;;
;;; 2. Caching according to lookup. Some lookup is too slow, we should use the
;;; "type" string returned from "quickInfo" to create a hash that have necessary
;;; info cached (great for built-in global, maybe we should have them preloaded
;;; into environment anyway). Company mode feels much slower than AC on
;;; document, body stuff...

(require 'cl-lib)

;;; These two variables serve to boost performance (not strictly necessary)
(defvar-local tss--last-company-start-point 1)
(defvar-local tss--last-company-candidates nil)
(defvar-local company-tss-candidates-info-cache (make-hash-table :test #'equal)
  "An info candidates cache(hash) to hold data for this completion")

(defun tss--get-company-member-candidates ()
  (tss--trace "start get company member candidates.")
  (tss--get-company-candidates t))

(defun tss--get-company-candidates-cached? ()
  "Test whether we can reuse results in `tss--last-company-candidates'."
  (when (> tss--last-company-start-point (point-max))
    (setq tss--last-company-start-point (point-max)))
  (let* ((currpt (point))
         (code (buffer-substring-no-properties
                currpt tss--last-company-start-point)))
    (if (and (> currpt tss--last-company-start-point)
             (string-match "\\`[a-zA-Z0-9_]+\\'" code))
        (progn (tss--trace "Last company candidates cached. code[%s]" code)
               t)
      (tss--trace "No last company candidates cached. code[%s]" code)
      nil))
  ;; TODO always return nil, as the cache management is not done (do we really
  ;; need this?)
  nil)

;;; TODO using Unicode char to make it look better
(defun company-tss-get-sign (kind)
  "Return a symbolic sign for KIND"
  (let ((kind (tss--stringify-response-element kind)))
    (cond ((member kind '("keyword" "builtin-keyword"))  "w")
          ((string= kind "primitive type")               "p")
          ((string= kind "module")                       "m")
          ((string= kind "interface")                    "i")
          ((string= kind "class")                        "c")
          ((member kind '("var" "property" "parameter")) "v")
          ((tss--function-kind-p kind)                   "f")
          ((string= kind "unknown")                      "")
          (t
           (tss--warn "found unknown server response for kind : %s" kind)
           ""))))

;;; TODO needed?
(defcustom tss-company-summary-truncate-length 128
  "Length for truncation of candidate summary of auto-complete.el."
  :type 'integer
  :group 'tss)

(defun tss--get-company-summary (sum)
  (when (stringp sum)
    (let ((str (replace-regexp-in-string "\r?\n" " " sum)))
      (truncate-string-to-width
       str tss-company-summary-truncate-length 0 nil "..."))))

;;; TODO too basic, too cumbersome, we need better support from ts-tools
(defun company-tss--colorize-type (name sign type)
  "Use regexp to colorize TYPE. Return colorized type."
  (if (string-empty-p sign)
      ""
    (let ((desc type)
          (start 0))
      ;; colorize property
      (string-match "(\\([^ \t]+\\))" desc start)
      (add-face-text-property (match-beginning 1)
                              (match-end 1)
                              'font-lock-preprocessor-face
                              nil desc)
      (setq start (match-end 0))
      ;; colorize candidate
      (string-match name desc start)
      (add-face-text-property (match-beginning 0)
                              (match-end 0)
                              'font-lock-function-name-face
                              nil desc)
      (setq start (match-end 0))
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
           (add-face-text-property (match-beginning 2)
                                   (match-end 2)
                                   'font-lock-type-face
                                   nil desc)
           (setq start (match-end 0))))
        ;; colorize variable type
        ("v"
         (string-match ": [ \t]*\\([a-zA-Z0-9_]+\\)"
                       desc start)
         (add-face-text-property (match-beginning 1)
                                 (match-end 1)
                                 'font-lock-type-face
                                 nil desc)))
      desc)))

(defun company-tss-format-document (name kind type doc)
  "Format a documentation for `company-doc', note the naming of
the arguments are from `ts-tools', a very unfortunate and
misleading names."
  (let* ((sign (company-tss-get-sign kind))
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

(defun company-tss-get-candidates (&optional prefix)
  "Retrieve completion candidates for current point.

NOTE: 1. the prefix is NOT passed to tss-server, the ts-tools can
figure this out according to file position directly. 

2. MEMBERP is NOT used any more, retrieving candidates will
always use completions-brief. Documents are retrieved later.

see https://github.com/clausreinke/typescript-tools for details
about command line building."
  (yaxception:$
    (yaxception:try
      (if (tss--get-company-candidates-cached?)
          tss--last-company-candidates
        (setq tss--last-company-start-point (point))
        (setq tss--last-company-candidates
              (when (tss--sync-server)
                (let* ((posarg (tss--get-position-argument))
                       ;; TODO what `memberarg' used for? Don't find this
                       ;; argument in ts-tools doc
                       ;; (memberarg (cond (memberp "true")
                       ;;                  (t       "false")))
                       (fpath (expand-file-name (buffer-file-name)))
                       (cmdstr (format "completions-brief %s %s"
                                       posarg fpath))
                       ;; ret format: isMemberCompletion,
                       ;; isNewIdentifierLocation, and entries array, there can
                       ;; also be a prefix string.
                       (ret (tss--get-server-response cmdstr :waitsec 2))
                       (entries (when (listp ret)
                                  (cdr (assoc 'entries ret)))))
                  (mapcar (lambda (e)
                            (let ((name (cdr (assoc 'name e)))
                                  (kind (cdr (assoc 'kind e))))
                              (propertize name
                                          :annotation (company-tss-get-sign kind)
                                          ;; :meta (tss--get-company-summary type)
                                          )))
                          entries))))))
    (yaxception:catch 'error e
      (tss--show-message "%s" (yaxception:get-text e))
      (tss--error "failed get ac candidates : %s\n%s"
                  (yaxception:get-text e)
                  (yaxception:get-stack-trace-string e))
      (setq tss--last-company-candidates nil))))

;; 1. `tss--active-code-prefix' uses text faces to make sure the current
;; point is in code (not string or comment region)
(defun company-tss-get-prefix ()
  (let ((curpt (point)))
    (save-excursion
      ;; `tss--get-active-code-prefix' only returns starting position for prefix,
      ;; bad naming... but as noted in `tss--company-get-member-candates', the
      ;; exact prefix doesn't matter, tss-server can handle it all.
      (buffer-substring-no-properties
       (tss--get-active-code-prefix "\\.\\([a-zA-Z0-9_]*\\)")
       curpt))))

;;; TODO have some idle/async way to fetch info about candidates in the
;;; background
(defun company-tss-sync-get-data (candidate)
  (let* ((curbuf (current-buffer))
         (curpt (point))
         (posarg nil)                   ;to be updated in the updated source
         (prefix company-prefix)
         (updated-source (with-temp-buffer
                           (insert-buffer curbuf)
                           (goto-char curpt)
                           (let ((company-prefix prefix))
                             ;; to handle prefix well, we need have company-prefix setup
                             ;; TODO maybe we should handle prefix manually?
                             (company--insert-candidate candidate))
                           (setq posarg (tss--get-position-argument))
                           (buffer-string)))
         (fpath  (buffer-file-name))
         (cmdstr (format "quickInfo %s %s" posarg fpath))
         (info (when (tss--sync-server :updated-source updated-source)
                 (tss--get-server-response cmdstr :waitsec 2))))
    (add-text-properties 0 (length candidate)
                         (let ((kind (cdr (assoc 'kind info)))
                               (rawdesc (cdr (assoc 'type info)))
                               (doc-comment (cdr (assoc 'docComment info))))
                           `(:meta
                             ,rawdesc
                             :doc
                             ,(company-tss-format-document
                               candidate kind rawdesc doc-comment)))
                         candidate)))

(defun company-tss-get-meta (candidate)
  (let ((ret (get-text-property 0 :meta candidate)))
    (if ret ret
      (company-tss-sync-get-data candidate)
      (company-tss-get-meta candidate))))

(defun company-tss-get-doc (candidate)
  (let ((ret (get-text-property 0 :doc candidate)))
    (if ret ret
      (company-tss-sync-get-data candidate)
      (company-tss-get-doc candidate))))

(defun company-tss-get-annotation (candidate)
  (format " (%s)" (get-text-property 0 :annotation candidate)))

(defun company-tss-member (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-tss-member))
    (prefix (company-tss-get-prefix))
    ;; arg is the prefix
    (candidates (company-tss-get-candidates arg))
    ;; arg is the current selected candidate
    (meta (company-tss-get-meta arg))
    (doc-buffer (company-doc-buffer (company-tss-get-doc arg)))
    ;; TODO solve the formatting issue for annotations
    (annotation (company-tss-get-annotation arg)))) 

(global-set-key (kbd "C-z t")
                (lambda () (interactive)
                  (company-begin-backend #'company-tss-member)))
