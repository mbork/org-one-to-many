;; org-one-to-many functions

(require 'cl)

(defvar default-split-tag "split"
  "Default value of SPLIT-AT in `org-one-to-many', used when no
parameter is supplied.")

;; Copy selected subtrees to their own files
(defun org-one-to-many (&optional split-at directory bullets)
  "Copy selected headlines to their own files in the directory
called DIRECTORY (or named after the current buffer).  Selection
happens by means of SPLIT-AT.  It can be a string - then the
selected headlines are the ones with that tag.  It can be
a positive number or a list containing a positive number - then
the selected headlines are the ones at that level.  It can be
a negative number - then the selected headlines will be the ones
at level \(- SPLIT-AT), but the links to them will be bullets
instead of headlines (just as if BULLETS were non-nil).  It can
be a cons cell - then the selected headlines will be the ones
whose level is between (car SPLIT-AT) and (cdr SPLIT-AT)
inclusive.

If BULLETS is non-nil, put the links to the split headlines into
a plain list bullets instead of headings."
  (interactive "P")
  (let* ((filename (if buffer-file-name (file-name-base) "otm-output"))
	 (directory (or directory filename))
	 (buffer (current-buffer))
	 (bullets (or bullets (< (prefix-numeric-value split-at) 0)))
	 (split-at (cond ((numberp split-at)
			  (abs split-at))
			 ((and (consp split-at)
			       (null (cdr split-at)))
			  (abs (prefix-numeric-value split-at)))
			 ((null split-at)
			  default-split-tag)
			 (t
			  split-at)))
	 (split-p (cond ((stringp split-at)
			 (lambda (elt)
			   (member-ignore-case split-at (org-element-property :tags elt))))
			((numberp split-at)
			 (lambda (elt)
			   (= (org-element-property :level elt) split-at)))
			((and (consp split-at)
			      (numberp (car split-at))
			      (numberp (cdr split-at)))
			 (lambda (elt)
			   (and (>= (org-element-property :level elt)
				    (car split-at))
				(<= (org-element-property :level elt)
				    (cdr split-at)))))
			(t
			 (error "This shouldn't happen."))))
	 (filenames (list (concat "split-" filename ".org")))
	 otm/filenames subfilename beg end headline-title)
    (make-directory directory t)
    (with-temp-file (concat directory "/split-" filename ".org")
      (org-mode)
      (insert-buffer-substring buffer)
      ;; do stuff
      (org-element-map (org-element-parse-buffer 'headline) 'headline
	; TODO: check org-map-entries & org-element-at-point!!
	(lambda (elt) (if (funcall split-p elt)
					; add text properties with filenames
			  (put-text-property (org-element-property :begin elt)
					     (org-element-property :end elt)
					     :otm-filename
					     (otm/title-to-filename
					      (org-element-property :raw-value elt))))))
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
	(push subfilename filenames)
	;; Write the part between beg and end to the external file,
	;; promoting it to level 1 first
	(let ((headline (buffer-substring-no-properties beg end)))
	  (with-temp-file (concat directory "/" subfilename)
	    (insert headline)
	    (goto-char (point-min))
	    (org-mode)
	    (dotimes (l (1- (org-element-property :level (org-element-at-point))))
	      (org-promote-subtree)))
	  ;; delete the previous contents, insert a link
	  (goto-char beg)
	  (skip-chars-forward "* ")
	  (when bullets
	    (delete-region (line-beginning-position) (point))
	    (insert "- "))
	  (setq headline-title (org-element-property :title (org-element-at-point)))
	  (delete-region (point) end)
	  (save-excursion (insert (org-make-link-string (concat "file:" subfilename) headline-title) "\n")))))
    (nreverse filenames)))

; Generate filenames from titles (=arbitrary strings)
;; (defvar otm/filenames ()
;;   "List of used-up filenames, to ensure injectivity of the
;;   mapping TITLE -> (OTM/TITLE-TO-FILENAME TITLE)")

(defun otm/title-to-filename (title)
  "Convert TITLE to a valid filename, by removing all non-letters and
  changing all spaces to hyphens.  Then check whether FILENAME is in
  OTM/FILENAMES, and if yes, append some number to it so that it becomes
  unique.  Finally, add the generated filename to OTM/FILENAMES."
  (let* ((filename (replace-regexp-in-string "[ \t\n\r]" "-" title))    ; thanks to
	 (filename (replace-regexp-in-string "[^a-zA-Z-]" "" filename)) ; help-gnu-emacs
	 (filename (replace-regexp-in-string "--+" "-" filename))       ; posters
	 (filename (replace-regexp-in-string "-$" "" filename))
	 (filename (downcase filename)))
    (if (member filename otm/filenames)
	(let ((count 1) new-filename)
	  (while (progn
		   (incf count)
		   (setq new-filename (concat filename "-" (number-to-string count)))
		   (member new-filename otm/filenames)))
	  (push new-filename otm/filenames)
	  new-filename)
      (push filename otm/filenames)
      filename)))

(provide 'org-one-to-many)

