;; org-one-to-many functions

(require 'cl)
(setq export-granularity 2)

; for testing
(setq eval-expression-print-level nil)
(setq eval-expression-print-length nil)

; Walk through the headlines of level EXPORT-GRANULARITY
(progn
  (set-buffer "otm-test.org")
  (let ((headlines
	 (org-element-map (org-element-parse-buffer) '(headline)
	   (lambda (oelt) (if (= (org-element-property :level oelt) export-granularity)
			      oelt)))))
					; now HEADLINES contains a
					; list of headlines of level
					; EXPORT-GRANULARITY
    (set-buffer (get-buffer-create "tmp"))
    (pp-display-expression headlines "tmp")))

; Generate filenames from titles (=arbitrary strings)
(defvar filenames '()
  "List of used-up filenames, to ensure injectivity of the
  mapping TITLE -> (TITLE-TO-FILENAME TITLE)")

(defun title-to-filename (title)
  "Convert TITLE to a valid filename, by removing all non-letters and
  changing all spaces to hyphens.  Then check whether FILENAME is in
  FILENAMES, and if yes, append some number to it so that it becomes
  unique.  Finally, add the generated filename to FILENAMES."
  (let* ((filename (replace-regexp-in-string "[ \t\n\r]" "-" title))    ; thanks to
	 (filename (replace-regexp-in-string "[^a-zA-Z-]" "" filename)) ; help-gnu-emacs
	 (filename (downcase filename)))                                ; posters
    (if (member filename filenames)
	(let ((count 1) new-filename)
	  (while (progn 
		   (incf count)
		   (setq new-filename (concat filename "-" (number-to-string count)))
		   (member new-filename filenames)))
	  (nconc filenames (list new-filename))
	  new-filename)
      (setq filenames (nconc filenames (list filename)))
      filename)))
