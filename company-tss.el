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

(require 'tss)

(require 'cl-lib)
(require 'dash)
(require 's)

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

;;; TODO needed?
(defcustom company-tss--meta-truncate-length 64
  "Length for truncation of company meta."
  :type 'integer
  :group 'company-tss)

(defun company-tss--format-meta (meta)
  (when (stringp meta)
    (let ((str (replace-regexp-in-string "\r?\n" " " meta)))
      (truncate-string-to-width
       str company-tss--meta-truncate-length 0 nil "..."))))

;;; TODO too basic, too cumbersome, we need better support from ts-tools
;;; May-28-2015 14:49:44 CST: actually for a start, we can use `typescript-mode'
;;; to get a decent colorization.
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
  "Format a documentation for `company-doc', note the naming of
the arguments are from `ts-tools', a very unfortunate and
misleading names."
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

(defun company-tss-get-candidates (&optional prefix)
  "Retrieve completion candidates for current point.

NOTE: 1. the prefix is NOT passed to tss-server, the ts-tools can
figure this out according to file position directly.

2. MEMBERP is NOT used any more, retrieving candidates will
always use completions-brief. Documents are retrieved later.

see https://github.com/clausreinke/typescript-tools for details
about command line building."
  (when (tss--sync-server)
    (let* ((posarg (tss--get-position-argument))
           ;; TODO what `memberarg' used for? Don't find this
           ;; argument in ts-tools doc. As I believe, this represents
           ;; an deprecated interface in ts-tools.
           ;;
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
                              :annotation (company-tss--get-sign kind)
                              :kind kind)))
              entries))))

(defun company-tss--get-active-code-prefix (re)
  "Retrieve prefix only in active code region."
  (save-excursion
    (when (and (tss--active-code-point-p)
               (re-search-backward (concat re "\\=") nil t))
      (or (match-beginning 1)
          (match-beginning 0)))))

;; 1. `tss--active-code-prefix' uses text faces to make sure the current
;; point is in code (not string or comment region)
(defun company-tss-get-prefix ()
  (let ((curpt (point))
        ;; `tss--get-active-code-prefix' only returns starting position for prefix,
        ;; bad naming... but as noted in `tss--company-get-member-candates', the
        ;; exact prefix doesn't matter, tss-server can handle it all.
        (start (-some #'company-tss--get-active-code-prefix
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
      (buffer-substring-no-properties start curpt))))

;;; TODO have some idle/async way to fetch info about candidates in the
;;; background
;;; TODO more info like: definition, location, even script snippets, references and etc.
;;;
;;; BUG The following doesn't do well with advanced types like interface, in
;;; fact I think the ts-tools return something too ambiguous....
(defun company-tss-sync-get-data (candidate)
  (let* ((curbuf (current-buffer))
         (curpt (point))
         (prefix company-prefix)
         ;; to be updated in the updated source
         (linecount 0)
         (posarg nil)
         (updated-source (with-temp-buffer
                           (insert-buffer-substring curbuf)
                           (goto-char curpt)
                           (let ((company-prefix prefix))
                             ;; to handle prefix well, we need have company-prefix setup
                             ;; TODO maybe we should handle prefix manually?
                             (company--insert-candidate candidate))
                           (setq posarg (tss--get-position-argument)
                                 linecount (count-lines (point-min) (point-max)))
                           (buffer-string)))
         (fpath  (buffer-file-name))
         (cmdstr (format "quickInfo %s %s" posarg fpath))
         (info (when (tss--sync-server :source updated-source
                                       :linecount linecount)
                 (tss--get-server-response cmdstr :waitsec 2))))
    (add-text-properties 0 (length candidate)
                         (let ((kind
                                ;; TODO due to some limits of ts-tools, kind
                                ;; returned from this way is different from the
                                ;; "completions-brief", very weird...., use
                                ;; :annotation property instead as a workaround
                                ;;
                                ;; (cdr (assoc 'kind info))
                                (get-text-property 0 :kind candidate))
                               (rawdesc (cdr (assoc 'type info)))
                               (doc-comment (cdr (assoc 'docComment info))))
                           ;; TODO whether we really need format-meta? the
                           ;; following way can also enjoy colorization from
                           ;; format-document.
                           `(:meta ,rawdesc
                             :doc
                             ,(company-tss--format-document
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

(defun company-tss (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-tss-member))
    (prefix (and (tss--active-p)
                 (company-tss-get-prefix)))
    (candidates (company-tss-get-candidates arg))
    (meta (company-tss-get-meta arg))
    (doc-buffer (company-doc-buffer (company-tss-get-doc arg)))
    ;; TODO better formatting for annotations
    (annotation (company-tss-get-annotation arg))))

(provide 'company-tss)
