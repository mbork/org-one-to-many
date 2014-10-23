;; org-one-to-many functions

(require 'cl)
;;(require 'org-dp)
(setq export-granularity 2)

;; for testing
;; (setq eval-expression-print-level nil)
;; (setq eval-expression-print-length nil)

; Copy subtrees at level EXPORT-GRANULARITY to their own files
(defun org-one-to-many (&optional level directory)
  "Copy headlines at level LEVEL (or the parameter
  EXPORT-GRANULARITY if LEVEL not specifed) to their own files in
  the directory called DIRECTORY (or named after the current
  buffer)."
  (interactive "p")
  ;; (setq filenames '(nil)) ; (setq filenames nil) won't work - we need FILENAMES to be a cons, not nil!
  (let* ((filenames)
	 (filename (if buffer-file-name (file-name-base) "otm-output"))
	 (directory (or directory filename))
	 (buffer (current-buffer))
	 subfilename beg end)
    (make-directory directory t)
    (with-temp-buffer
      (org-mode)
      (insert-buffer-substring buffer)
      ;; do stuff
      (org-element-map (org-element-parse-buffer 'headline) 'headline
	; check org-map-entries & org-element-at-point!!
	(lambda (elt) (if (= (org-element-property :level elt) export-granularity)
					; add text properties with filenames
			  (put-text-property (org-element-property :begin elt)
					     (org-element-property :end elt)
					     :otm-filename
					     (title-to-filename (org-element-property :raw-value elt)
								filenames))
			)))
					; change the links (see
					; [[mu4e:msgid:87bnpd4ov7.fsf@nicolasgoaziou.fr][Re:
					; How to change a link?]])
      (let (links)
	(org-element-map (org-element-parse-buffer) 'link
	  (lambda (elt)
	    (if (member (org-element-property :type elt) '("custom-id" "fuzzy"))
		(push elt links))))
	(mapc (lambda (link)
		(goto-char (org-element-property :begin link))
		(let ((destfile
		       (save-excursion
			 (org-open-at-point)
			 (get-text-property (point) :otm-filename)))
		      (sourcefile (get-text-property (point) :otm-filename)))
		  (unless (equal destfile sourcefile)
		    (delete-region (org-element-property :begin link)
				   (org-element-property :end link))
		    ;; (pp-eval-expression 'link)
		    (org-element-put-property link
					      :raw-link
					      (concat "file:"
						      (or destfile (concat "split-" filename))
						      ".org"
						      "::"
						      (org-element-property :raw-link link)))
		    (insert-and-inherit (org-element-interpret-data link)))))
	      links))
					; split the file
      (goto-char (point-min))
      (while
	  (setq beg (next-single-property-change (point) :otm-filename))
	(setq end (next-single-char-property-change beg :otm-filename))
	(setq subfilename (concat (get-text-property beg :otm-filename) ".org"))
	(write-region beg end (concat directory "/" subfilename))
	(delete-region beg end)
	(goto-char beg)
	(save-excursion (insert "[[file:" subfilename "]]\n")))
					; write the big file
      (write-region (point-min)
		    (point-max)
		    (concat directory "/split-" filename ".org")
		    nil
		    'no-message))))

; Generate filenames from titles (=arbitrary strings)
;; (defvar filenames '()
;;   "List of used-up filenames, to ensure injectivity of the
;;   mapping TITLE -> (TITLE-TO-FILENAME TITLE)")

(defun title-to-filename (title filenames)
  "Convert TITLE to a valid filename, by removing all non-letters and
  changing all spaces to hyphens.  Then check whether FILENAME is in
  FILENAMES, and if yes, append some number to it so that it becomes
  unique.  Finally, add the generated filename to FILENAMES."
  (let* ((filename (replace-regexp-in-string "[ \t\n\r]" "-" title))    ; thanks to
	 (filename (replace-regexp-in-string "[^a-zA-Z-]" "" filename)) ; help-gnu-emacs
	 (filename (replace-regexp-in-string "--+" "-" filename))       ; posters
	 (filename (replace-regexp-in-string "-$" "" filename))
	 (filename (downcase filename)))
    (if (member filename filenames)
	(let ((count 1) new-filename)
	  (while (progn
		   (incf count)
		   (setq new-filename (concat filename "-" (number-to-string count)))
		   (member new-filename filenames)))
	  (setq filenames (push new-filename filenames))
	  new-filename)
      (setq filenames (push filename filenames))
      filename)))
