;;; Some code in tss.el is patched to support my design in "company-tss.el"
(defun* tss--sync-server (&key waitsec path buff
                               updated-source)
  "* Add UPDATED-SOURCE(:string) to support custom buffer content.
The priority order of content to be sent with 'update' command
is (from hi to low): updated-source, buff, (current-buffer)."
  (when (tss--active-p)
    (save-restriction
      (widen)
      (let ((proc (tss--get-process))
            (waiti 0)
            (maxwaiti (* (or waitsec 3) 5))
            (cmdstr (format "update %d %s"
                            (with-current-buffer (or buff (current-buffer))
                              (count-lines (point-min) (point-max)))
                            (expand-file-name (or path (buffer-file-name))))))
        (tss--debug "Start sync server : %s" cmdstr)
        (when (tss--send-string proc cmdstr)
          (setq tss--server-response nil)
          (setq tss--incomplete-server-response "")
          (setq tss--json-response-start-char "")
          (setq tss--json-response-end-char "")
          (tss--send-string proc (or updated-source
                                     (with-current-buffer (or buff (current-buffer))
                                       (buffer-substring))))
          (tss--trace "Start wait sync server.")
          (while (and (< waiti maxwaiti)
                      (not tss--server-response))
            (accept-process-output proc 0.2 nil t)
            (incf waiti))
          (cond ((not (< waiti maxwaiti))
                 (tss--warn "Timeout sync server.")
                 nil)
                (t
                 (tss--trace "Finished sync server.")
                 (eq tss--server-response 'succeed))))))))
