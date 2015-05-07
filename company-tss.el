;;; Many things still left to be desired.
;;;
;;; TODO
;;; 
;;; 1. better `tss--sync-server', modify the source sent on-the-fly to include
;;; newly selected candidate, so we can have access to info from "definition",
;;; "references" and etc. If we can have this, we can always use
;;; "completions-brief" to speed completions up greatly. Try to reuse eldoc part
;;; also
;;;
;;; 2. Caching according to lookup. Some lookup is too slow, we should use the
;;; "type" string returned from "quickInfo" to create a hash that have necessary
;;; info cached (great for built-in global, maybe we should have them preloaded
;;; into environment anyway). Company mode feels much slower than AC on
;;; document, body stuff...

(require 'cl-lib)

;;; These two variables serve to boost performance (not strictly necessary)
(defvar tss--last-company-start-point 1)
(make-variable-buffer-local 'tss--last-company-start-point)
(defvar tss--last-company-candidates nil)
(make-variable-buffer-local 'tss--last-company-candidates)

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

(defun tss--get-company-symbol (kind)
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

(defun tss--get-company-document (name kind type doc)
  (let* ((sym (intern (tss--get-company-symbol kind)))
         (kind (upcase (tss--stringify-response-element kind)))
         (type (or type "unknown"))
         (doc (or doc ""))
         (typedesc (case sym
                     (w "")
                     (f (concat (propertize "Signature: "
                                            'face 'apropos-symbol)
                                type "\n\n"))
                     (t (concat (propertize "Type: "
                                            'face 'apropos-symbol)
                                type "\n\n")))))
    (setq name (propertize name 'face 'font-lock-keyword-face))
    (setq kind (propertize kind 'face 'font-lock-type-face))
    (concat name " is " kind ".\n\n" typedesc
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
                                          :annotation (tss--get-company-symbol kind)
                                          ;; :meta (tss--get-company-summary type)
                                          ;; :doc
                                          ;; (tss--get-company-document name kind type doc)
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

(defun company-tss-sync-get-data (candidate)
  (let ((curbuf (current-buffer))
        (curpt (point))
        (curpath (buffer-file-name (current-buffer))))
    (with-temp-buffer
      (insert-buffer curbuf)
      (goto-char curpt)
      (company--insert-candidate candidate)
      (when (let ((major-mode 'typescript-mode)) ;bypass `tss--active-p'
              (tss--sync-server :path curpath
                                ;; be explicit
                                :buff (current-buffer)))
        (let* ((posarg (tss--get-position-argument))
               (fpath curpath)
               (cmdstr (format "quickInfo %s %s"
                               posarg fpath))
               (info (tss--get-server-response cmdstr :waitsec 2)))
          (pp-to-string info))))))

(defun company-tss-get-meta (candidate)
  (get-text-property 0 :meta candidate))

(defun company-tss-get-doc (candidate)
  (company-tss-sync-get-data candidate))

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
    (annotation (company-tss-get-annotation arg))
    ))

