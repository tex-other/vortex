;; Copyright (c) 1985-1992 The Regents of the University of California.
;; All rights reserved.
;;
;; Permission is hereby granted, without written agreement and without
;; license or royalty fees, to use, copy, modify, and distribute this
;; software and its documentation for any purpose, provided that the
;; above copyright notice and the following two paragraphs appear in
;; all copies of this software.
;; 
;; IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY FOR
;; DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES, INCLUDING LOST PROFITS, ARISING OUT
;; OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF THE UNIVERSITY OF
;; CALIFORNIA HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;
;; THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY WARRANTIES,
;; INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
;; AND FITNESS FOR A PARTICULAR PURPOSE.  THE SOFTWARE PROVIDED HEREUNDER IS
;; ON AN "AS IS" BASIS, AND THE UNIVERSITY OF CALIFORNIA HAS NO OBLIGATION TO
;; PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.
;;
;; RCS Header
;; $Header: /home/yew/yew4/vortex/newdist/emacs/RCS/TeX-cite.el,v 1.14 1993/09/16 02:32:14 munson Exp $
;;
;; This file, "TeX-cite.el", is a GNU Emacs interface to BibTeX databases
;; for plain TeX and LaTeX documents.  It is a subsystem of TeX-mode.
;; It allows users to automatically cite references found in BibTeX databases
;; without leaving their TeX or LaTeX document.
;;

(require 'TeX-mode)

;;;;;;;;;;;;;;
;;;Global Data	      
;;;;;;;;;;;;;;

;; Buffer Names

(defconst tex-cite-tmp "--- TeX Bibliography Scratch Buffer ---")
(defconst tex-cite-window "--- TeX Bibliography Lookup Help ---")
(defvar tex-master-buffer nil "Buffer for document master.")
(defconst tex-cite-query-buffer-name "TeX-Cite-Query")

;; File Names

(defvar tex-cite-wildcard nil "List of files matching *.bib in tex-bib-path")
(defconst tex-cite-ext ".bib")
(defvar tex-cite-document-directory nil "Directory containing document file")

;; Citation Options

(defvar tex-cite-note-on nil "A flag t if note field required.")
(defvar tex-cite-cite-on nil "A flag t if cite, nil if nocite.")
(defconst tex-cite-cite-str "\\cite")
(defconst tex-cite-nocite-str "\\nocite")

;; State of Multifile Search

(defvar tex-cite-mfs-last-regexp nil
  "Last regular expression used by multifile search")
(defvar tex-cite-mfs-last-file-list nil
  "Last file list used by multifile search")
(defvar tex-cite-mfs-front nil "Front of file list")
(defvar tex-cite-mfs-tail nil "Back of file list.  Car of this list is the
current file")
(defvar tex-cite-mfs-last-back nil "Last value of BACK used by mfs")

;; State of Bibliography Search

(defvar tex-cite-search-last-query nil "Last query passed to tex-cite-search")

;; State of Queries

(defvar tex-cite-last-query nil)

;; Data for Generation and Parsing of Queries

(defvar tex-cite-query-field-labels nil
  "A list of the strings which act as labels for the fields of the
query template")

(defvar tex-cite-query-fields-alist nil
  "An association list which maintains the relationship between the
labels of the template fields and the normal BibTeX fields the
label stands for.")

;; Data for User Prompts during Lookups

(defconst tex-cite-bibmsg (concat ".bib filename (default *.bib in "
				 tex-bib-env "): "))

(defvar tex-cite-query-lookup-response
  '(((?\r ?y) "RET" yes)
    ((?\e) "ESC" yes-more)
    ((?  ?n) "SPC" next)
    ((?\177 ?p) "DEL" previous)
    ((?s) "s" show)
    ((?F) "F" new-file)
    ((?K) "K" new-key-from-top)
    ((?k) "k" new-key)
    ((?u) "u" note-switch)
    ((?m) "m" cite-switch)
    ((?\^r) "C-r" redit)
    ((?\^c) "C-c=exit" quit)
    ((??) "?=help" help))
  "Data for lookup prompt")

(defvar tex-cite-query-failure-response
  '(((?F) "F" new-file)
    ((?K) "K" new-key-from-top)
    ((?e) "e" new-entry)
    ((?\^r) "C-r" redit)
    ((?\^c) "C-c=exit" quit)
    ((??) "?=help" help))
  "Data for prompt upon failure of search")

(defvar tex-cite-lookup-window-config nil
  "Last window configuration used in tex-cite-lookup")

(defvar tex-cite-lookup-show nil "Flag is non-nil if user wants show-window")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Interactive Automatic Citation functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun tex-cite-cite (&optional note)
  "Make a citation entry.  If C-u is given as a prefix, prompt for
the optional note field."
  (interactive "P")
  (message "Looking up .bib files for a citation...")
  (tex-cite-entry-merge note t)
  (message "Looking up .bib files for a citation...done"))

(defun tex-cite-nocite ()
  "Make a pseudo-citation entry."
  (interactive)
  (message "Looking up .bib files for a pseudo citation...")
  (tex-cite-entry-merge nil nil)
  (message "Looking up .bib files for a pseudo citation...done"))

(defun tex-cite-forms-toggle ()
  (interactive)
  (cond
   (tex-cite-use-full-query
    (setq tex-cite-use-full-query nil)
    (message "Disabling Full Query Lookup ... (enabling regular expression)"))
   (t
    (setq tex-cite-use-full-query t)
    (message "Enabling Full Query Lookup ... (disabling regular expression)"))
   ))

(defun tex-cite-clear-file-list ()
  (interactive)
  (setq tex-cite-wildcard nil)
  (message "Clearing List of .bib files ... done")
  )

(defun tex-cite-case-toggle ()
  (interactive)
  (cond (tex-cite-case-fold-search
	 (setq tex-cite-case-fold-search nil)
	 (message "Setting citation search to be Case Sensitive ... done"))
	(t
	 (setq tex-cite-case-fold-search t)
	 (message "Setting citation search to be Case Insensitive ... done")))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Citation Insertion Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun tex-cite-entry-merge (note-on cite-on)
  (let ((tex-cite-note-on note-on)
	(tex-cite-cite-on cite-on)
	(result (tex-cite-lookup))
	(continue t)
	cite-string entry-id)
    (while (and result continue)
      (setq cite-string 
	    (if tex-cite-cite-on tex-cite-cite-str tex-cite-nocite-str))
      (setq entry-id (car result))
      (setq continue (nth 1 result))
      (if tex-cite-note-on
	(tex-cite-with-note cite-string entry-id)
	(tex-cite-without-note cite-string entry-id))
      (tex-cite-biblist)
      (if continue
	(setq result (tex-cite-lookup t))))))

(defun tex-cite-with-note (cite-string entry-id)
  (let* ((str (read-string "Citation note field: "))
	 (note (if (string-equal str "")
		 ""
		 (concat "[" str "]"))))
    (insert ?~ cite-string note ?{ entry-id ?})))

(defun tex-cite-without-note (cite-string entry-id)
  (let ((regexp (concat "\\" cite-string "\\s *{\\([^}]*\\)}")))
    (if (save-excursion (and (search-backward "\\" nil t)
			     (looking-at regexp)))
	(let ((entries (vortex-parse-comma-list (vortex-match-string 1)))
	      (insert-point (match-end 1))
	      )
	  (cond
	   ((and (> (point) (match-end 0))
		 (save-excursion
		   (re-search-backward "\\w\\|\n[ \t]*\n" (match-end 0) t)))
	    (insert "~" cite-string "{" entry-id "}"))
	   ((vortex-memq entry-id entries)
	    (message "Entry {%s} already cited in this list." entry-id)
	    (beep)
	    (sit-for 2))
	   (t
	    (save-excursion
	      (goto-char insert-point)
	      (insert "," entry-id)))
	   ))
      (insert "~" cite-string "{" entry-id "}")
      )))

(defun tex-cite-biblist ()
  "Add the .bib file FN to the \\bibliography list.  Create one if necessary."
  (let* ((fn-pre (substring (car tex-cite-mfs-tail) 0 -4))
	 (fn-nd-pre (file-name-nondirectory fn-pre))
	 (fd (file-name-directory fn-pre))
	 (pre (if (and fd
		       (vortex-memq 
			 (substring fd 0 -1) ; get rid of trailing "/"
			 tex-bib-path))
		fn-nd-pre
		fn-pre))
	 eoe)
    (save-excursion
      (set-buffer tex-master-buffer)
      (goto-char (point-max))
      (if (re-search-backward "\\\\bibliography *{.*}" nil t)
	(if (search-forward pre (setq eoe (match-end 0)) t)
	  nil
	  (progn
	    (goto-char (1- eoe))
	    (insert ?, pre)))
        (re-search-backward "\\\\end *$\\|\\\\bye *$\\|\\\\end{document}\\|\\\\input.*-.*$" nil t)
	(insert "\n\\bibliography{" pre "}\n")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; File Naming and Location functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun tex-cite-master ()
  (let ((master-file (tex-check-master-file)))
    (setq tex-master-buffer (get-file-buffer master-file))
    (if tex-master-buffer
	nil
      (save-excursion
	(find-file master-file)
	(setq tex-master-buffer (current-buffer))))))

(defun tex-cite-fn-ext (fn suffix)
  "Attach SUFFIX to the base of FN, if FN has no extension or has extension 
`\.bib'.  Otherwise (i.e FN has other extension) FN itself is returned."
  (let ((base (string-match "\\.bib" fn))
	(ext t))
    (if base
      nil
      (if (string-match "\\." fn)		; Already has extension
	(setq ext nil)))
    (if ext
      (concat (substring fn 0 base) suffix)
      fn)))
		
(defun tex-cite-list-wildcard (ext path)
  "Return a list of files with extension EXT in PATH (list of directories)."
  (let ((tmp (get-buffer-create tex-cite-tmp))
	(args "/bin/ls")
	(lst nil))
    (save-excursion
      (set-buffer tmp)
      (erase-buffer)
      (while path
	(setq args (concat args " " (car path) "/*" ext))
	(setq path (cdr path)))
      (message "Looking for *.bib in path %s..." tex-bib-env)
      (call-process-region 1 (point-max) shell-file-name t t nil "-c" args)
      (message "Looking for *.bib in path %s...done" tex-bib-env)
      (goto-char 1)
      (if (looking-at "No match\.")
	nil
	(while (not (eobp))
	  (if (looking-at "^ *$")
	    nil
	    (set-mark (point))
	    (end-of-line)
	    (setq lst (cons (buffer-substring (mark) (point))
			    lst)))
	  (next-line 1)
	  (beginning-of-line))
	(reverse lst)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Multifile Regular Expression Search functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun tex-cite-multifile-search
  (&optional back nowrap top new-regexp new-file-list)
  "Search through a list of files for a regular expression.  If TOP is non-nil,
search from beginning of file-list.  If NOWRAP is non-nil, do not wrap around
end of file list when searching.  If BACK is non-nil, search backwards.
NEW-REGEXP is a single regular expression
to be searched for.  NEW-FILE-LIST is the list of files to be searched.
If NEW-REGEXP and NEW-FILE-LIST are nil, use the last values provided.
It is assumed that TOP and BACK will never both be non-nil"
  (let ((regexp (if new-regexp
		    (setq tex-cite-mfs-last-regexp new-regexp)
		  tex-cite-mfs-last-regexp))
	(first-time nil)
	)
    (if (or top new-file-list)
	(progn
	  (tex-cite-mfs-initialize-data new-file-list)
	  (setq first-time t)))
    (setq default-directory tex-cite-document-directory)
    (set-buffer (find-file-noselect (car tex-cite-mfs-tail)))
    (message "Searching %s ..." (car tex-cite-mfs-tail))
    (catch 'found
      (while t
	(cond ((let ((case-fold-search tex-cite-case-fold-search))
		 (tex-cite-mfs-search-buffer regexp back first-time))
	       (throw 'found (current-buffer)))
	      ((if back
		   (tex-cite-mfs-roll-backward nowrap)
		 (tex-cite-mfs-roll-forward nowrap))
	       (setq default-directory tex-cite-document-directory)
	       (set-buffer (find-file-noselect (car tex-cite-mfs-tail)))
	       (message "Searching %s ..." (car tex-cite-mfs-tail))
	       (setq first-time t))
	      (t (throw 'found nil)))))
    ))

(defun tex-cite-mfs-initialize-data (file-list)
  (if file-list
      (setq tex-cite-mfs-last-file-list file-list)
    (setq file-list tex-cite-mfs-last-file-list))
  (setq tex-cite-mfs-last-back nil
	tex-cite-mfs-front nil
	tex-cite-mfs-tail file-list))

(defun tex-cite-mfs-search-buffer (regexp back first-time)
  (cond (first-time (if back (goto-char (point-max))
		      (goto-char (point-min))))
	((not (eq back tex-cite-mfs-last-back))
	 (if back (goto-char (match-beginning 0))
	   (goto-char (match-end 0))))
	)
  (setq tex-cite-mfs-last-back back)
  (if back
      (re-search-backward regexp nil t)
    (re-search-forward regexp nil t)))

(defun tex-cite-mfs-roll-forward (nowrap)
  (cond ((and (null (cdr tex-cite-mfs-tail)) nowrap) nil)
	((cdr tex-cite-mfs-tail)
	 (setq tex-cite-mfs-front
	       (cons (car tex-cite-mfs-tail) tex-cite-mfs-front))
	 (setq tex-cite-mfs-tail (cdr tex-cite-mfs-tail))
	 t)
	(t (setq tex-cite-mfs-front nil)
	   (setq tex-cite-mfs-tail tex-cite-mfs-last-file-list)
	   (message "Wrapping around...")(sit-for 1)
	   t)
	))

(defun tex-cite-mfs-roll-backward (nowrap)
  (cond ((and (null tex-cite-mfs-front) nowrap) nil)
	(tex-cite-mfs-front
	 (setq tex-cite-mfs-tail
	       (cons (car tex-cite-mfs-front) tex-cite-mfs-tail))
	 (setq tex-cite-mfs-front (cdr tex-cite-mfs-front))
	 t)
	(t (setq tex-cite-mfs-front (reverse tex-cite-mfs-last-file-list))
	   (setq tex-cite-mfs-tail (list (car tex-cite-mfs-front)))
	   (setq tex-cite-mfs-front (cdr tex-cite-mfs-front))
	   (message "Wrapping around...")(sit-for 1)
	   t)
	))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Bibliography-Oriented Search Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun tex-cite-search (&optional back top new-query file-list)
  (let* ((query (if new-query
		    (setq tex-cite-search-last-query new-query)
		  new-query tex-cite-search-last-query))
	 (first-regexp (if tex-cite-use-full-query (car (car query))
			 query))
	 (nowrap top)
	 (buffer (tex-cite-multifile-search
		  back nowrap top first-regexp file-list))
	 (continue t)
	 (return-value nil)
	 )
    (if buffer
	(progn
	  (set-buffer buffer)
	  (while
	      (and (not (setq return-value (tex-cite-query-check-entry query)))
		   (setq buffer (tex-cite-multifile-search back nowrap)))
	    (set-buffer buffer)))
      )
    return-value))

(defun tex-cite-query-check-entry (query)
  (save-excursion
    (let* ((case-fold-search t)
	   (entry-data (save-excursion (bibtex-search-entry)))
	   (result t)
	   )
      (if (or (null entry-data) (> (point) (nth 1 entry-data)))
	  nil
	(while (and query result tex-cite-use-full-query)
	  (setq result (tex-cite-query-check-regexp (car query) entry-data))
	  (setq query (cdr query)))
	(if result
	    (list (nth 2 (nth 3 entry-data)) (current-buffer) entry-data))
	))))

(defun tex-cite-query-check-regexp (regexp-data entry-data)
  (let ((entry-start (car entry-data))
	(entry-end (nth 1 entry-data))
	(regexp (car regexp-data))
	(label-regexp (cdr regexp-data))
	)
    (cond
     ((string-equal label-regexp "ID")
      (string-match regexp (nth 2 (nth 3 entry-data))))
     (t
      (goto-char entry-start)
      (catch 'done
	(while (re-search-forward regexp entry-end t)
	  (save-excursion
	    (let ((field-data (bibtex-current-field-data)))
	      (if (and
		   (or (string-equal label-regexp "ANYFIELD")
		       (eq 0 (string-match
			      label-regexp (nth 2 (nth 2 field-data)))))
		   (let ((case-fold-search tex-cite-case-fold-search))
		     (string-match regexp (nth 2 (nth 3 field-data)))))
		  (throw 'done t)))
	    ))))
     )))
      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Initialization Functions for Full Queries
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun tex-cite-query-init ()
  (setq tex-cite-query-field-labels
	(mapcar 'tex-cite-make-query-label tex-cite-query-fields))
  (setq tex-cite-query-fields-alist
	(tex-cite-make-query-field-alist 
	 tex-cite-query-field-labels tex-cite-query-fields)))

(defun tex-cite-make-query-label (field-list)
  "FIELD-LIST is a list of strings.  This function returns the same strings
concatenated together with slashes between them.  That is,
(`AUTHOR' `EDITOR') => `AUTHOR/EDITOR'"
  (mapconcat 'eval field-list "/"))

(defun tex-cite-make-query-field-alist (labels field-lists)
  "Recursively generates an association list between the elements of
of the two lists that are its arguments.  It assumes that the lists are of
the same length, though if the first one is short, it's not a problem.
Has been modified to tranform the elements of the second list into
regular expressions matching each string in the list."
  (if (null labels) nil
    (cons (cons (car labels)
		(mapconcat 'eval  (car field-lists) "\\|"))
	  (tex-cite-make-query-field-alist (cdr labels)
					  (cdr field-lists)))))
;;;  Permanent hack to insure initialization
(tex-cite-query-init)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions for Input of Fancy Queries
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun tex-cite-get-query (file-list)
  "Returns a list of (word-list field-list) pairs.  A field-list is a
list of BibTeX field labels.  The words in the word list will contain
no white space.  The list of pairs is constructed by having the user
fill in a query template.  All of the field labels used are standard
except ID, which stands for the unique identifier, and ANYFIELD, which
is supposed to stand for all fields."
  (if tex-cite-use-full-query
      (tex-cite-get-full-query)
    (tex-cite-get-regexp-query file-list)))

(defun tex-cite-get-full-query ()
  "Returns a list of (word-list field-list) pairs.  A field-list is a
list of BibTeX field labels.  The words in the word list will contain
no white space.  The list of pairs is constructed by having the user
fill in a query template.  All of the field labels used are standard
except ID, which stands for the unique identifier, and ANYFIELD, which
is supposed to stand for all fields."
  (save-window-excursion
    (let ((query-buffer (tex-cite-get-query-buffer)))
      (tex-cite-insert-template query-buffer)
      (tex-cite-edit-query-buffer query-buffer)
      (setq tex-cite-last-query
	    (tex-cite-parse-query-buffer query-buffer))
      (bury-buffer query-buffer)
      tex-cite-last-query))
  )

(defun tex-cite-get-regexp-query (file-list)
    (read-string
     (concat "Lookup regexp in " (if (cdr file-list)
				     "*.bib"
				   (file-name-nondirectory (car file-list)))
	     " (default browsing mode): ")))

(defun tex-cite-get-query-buffer ()
  "Either gets the buffer named by tex-cite-query-buffer-name or creates it.
Returns the buffer and leaves the cursor in the buffer."
  (cond ((get-buffer tex-cite-query-buffer-name))
	(t (set-buffer (get-buffer-create tex-cite-query-buffer-name))
	   (bibtex-mode)
	   (current-buffer))))

(defun tex-cite-insert-template (buffer)
  "Inserts a blank template in BUFFER. Note that what fields are produced
depends on the elements of tex-cite-query-field-labels."
  (switch-to-buffer-other-window buffer)
  (goto-char (point-min))
  (if (or (not tex-cite-last-query)
	   (y-or-n-p "Clear query form? "))
      (progn
	(erase-buffer)
	(insert "%% Type ESC C-c when query is complete.  ")
	(insert (if default-case-fold-search
		    "Using Case-Insensitive search.\n\n"
		  "Using Case-Sensitive search.\n\n"))
	(insert "@QUERY{,")
	(let ((field-list tex-cite-query-field-labels))
	  (while (cdr field-list)
	    (insert "\n")
	    (indent-to bibtex-field-indent)
	    (insert (car field-list)
		    " = "
		    bibtex-field-pair
		    ",")
	    (setq field-list (cdr field-list)))
	  (insert "\n")
	  (indent-to bibtex-field-indent)
	  (insert (car field-list)
		  " = "
		  bibtex-field-pair
		  "\n")
	  (indent-to bibtex-field-indent)
	  (insert "}")
	  ))))

(defun tex-cite-edit-query-buffer (buffer)
  "Lets the user recursively edit the query buffer, politely placing the
cursor at the identifier field."
  (goto-char 1)
  (bibtex-next-field 2)
  (message "Edit template to complete query ... ESC C-c to accept")
  (recursive-edit))

(defun tex-cite-parse-query-buffer (buffer)
  "Parses a completed query template in BUFFER. Will only parse the first
template found there.  Returns a list of (word-list field-list) pairs, which
are described in more detail under tex-cite-get-query."
  (let ((result nil)
	(field-alist tex-cite-query-fields-alist))
    (set-buffer buffer)
    (goto-char (point-min))
    (re-search-forward "@QUERY{\\(.*\\)," nil t)
    (if (/= (match-beginning 1) (match-end 1))
	(setq result
	      (mapcar
	       '(lambda (re) (cons re "ID"))
	       (tex-cite-field-parse (match-beginning 1) (match-end 1)))))
    (while field-alist
      (re-search-forward
       (concat (car (car field-alist)) "\\s *=\\s {\\(.*\\)}") nil t)
      (if (/= (match-beginning 1) (match-end 1))
	  (let
	      ((new-items
		(mapcar
		 '(lambda (re) (cons re (cdr (car field-alist))))
		 (tex-cite-field-parse (match-beginning 1) (match-end 1)))))
	    (setq result (nconc result new-items))))
      (setq field-alist (cdr field-alist)))
    result
    ))

(defun tex-cite-field-parse (start end)
  "START and END are the boundaries of a substring in the current buffer.
This function returns a list of the separate words found in this substring.
The notion of `word' used here differs from that used by Emacs.  The local
definition of `word' means contiguous non-whitespace characters."
  (goto-char start)
  (let ((regexp-start nil)
	(result nil))
    (while (re-search-forward "\\S " end t)
      (cond
       ((eq (char-after (match-beginning 0)) ?\")
	(setq regexp-start (point))
        (if (re-search-forward "[^\\\\]\"" end t)
	    (setq result
		  (append result (list (buffer-substring regexp-start
							 (1- (point))))))
	    (error "Mismatched quotes, please reenter your query")))
       (t
	(setq regexp-start (1- (point)))
	(if (re-search-forward "\\s " end 1)
	    (setq result
		  (append result (list (buffer-substring regexp-start
							 (1- (point))))))
	  (setq result
		(append result (list (buffer-substring regexp-start end))))))
       ))
    result))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   User interface for Citation Lookup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun tex-cite-lookup (&optional round2 abort)
  (tex-cite-master)
  (let ((file-list (if round2 tex-cite-mfs-last-file-list
		     (tex-cite-get-file-list)))
	(home-buffer (current-buffer))
	(query nil)
	(result nil)
	(response nil)
	(return-value nil)
	(entry-id nil)
	(help-window nil)
	(pre-help-config nil)
	(show-window nil)
	(continue t)
	)
    (setq tex-cite-document-directory default-directory)
    (save-window-excursion
      (if round2
	  (progn
	    (set-window-configuration tex-cite-lookup-window-config)
	    (setq result (tex-cite-search)))
	(setq tex-cite-lookup-show nil)
	(setq query (tex-cite-get-query file-list))
	(if (or (null query) (equal "" query))
	    (error "You supplied an empty query."))
	(setq result (tex-cite-search nil t query file-list)))
      (while continue
	(setq entry-id (car result))
	(if tex-cite-lookup-show
	    (progn
	      (cond
	       (show-window
		(select-window show-window)
		(switch-to-buffer (nth 1 result)))
	       (t
		(set-buffer home-buffer)
		(pop-to-buffer (nth 1 result))
		(setq show-window (get-buffer-window (current-buffer)))))
	      (save-excursion 
		(search-backward "@" nil t)
		(recenter 0))))
	(if result
	    (setq response
		  (vortex-user-response (concat "Confirm {" entry-id "}? ")
					tex-cite-query-lookup-response))
	  (setq response
		(vortex-user-response "Your query not found "
				      tex-cite-query-failure-response)))
	(if help-window
	    (progn
	      (set-window-configuration pre-help-config)
	      (setq help-window nil)))
	(cond
	 ((eq response 'yes)
	  (setq return-value (list entry-id nil))
	  (setq continue nil))
	 ((eq response 'yes-more)
	  (set-buffer (nth 1 result))
	  (goto-char (nth 1 (nth 2 result)))
	  (setq return-value (list entry-id t))
	  (setq continue nil))
	 ((eq response 'next)
	  (goto-char (nth 1 (nth 2 result)))
	  (setq result (tex-cite-search nil)))
	 ((eq response 'previous)
	  (goto-char (car (nth 2 result)))
	  (setq result (tex-cite-search t)))
	 ((eq response 'show)
	  (setq tex-cite-lookup-show t))
	 ((eq response 'new-file)
	  (setq result (tex-cite-search
			nil t nil (setq file-list (tex-cite-get-file-list)))))
	 ((eq response 'new-key-from-top)
	  (setq result (tex-cite-search
			nil t (setq query (tex-cite-get-query file-list)))))
	 ((eq response 'new-key)
	  (setq result (tex-cite-search
			nil nil (setq query (tex-cite-get-query file-list)))))
	 ((eq response 'new-entry)
	  (setq result (tex-cite-new-entry)))
	 ((eq response 'note-switch)
	  (setq tex-cite-note-on (not tex-cite-note-on))
	  (if tex-cite-note-on
	      (message "Citation note field prompting enabled.")
	    (message "Citation note field prompting disabled."))
	  (sit-for 1))
	 ((eq response 'cite-switch)
	  (setq tex-cite-cite-on (not tex-cite-cite-on))
	  (if tex-cite-cite-on
	      (message "Citation mode enabled.")
	    (message "Pseudo citation mode enabled."))
	  (sit-for 1))
	 ((eq response 'redit)
	  (message "Entering recursive edit...(return to bibliography lookup by ESC C-c)")
	  (save-excursion (save-window-excursion (recursive-edit))))
	 ((eq response 'quit)
	  (if abort
	      (progn
		(ding)
		(message "%squit (returning to previous level)" sysmsg)
		(sit-for 1)
		(throw abort t))
	    (setq return-value nil)
	    (setq continue nil)))
	 ((eq response 'help)
	  (setq help-window t)
	  (setq pre-help-config (current-window-configuration))
	  (if result
	      (tex-cite-lookup-help)
	    (tex-cite-option-help)))
	 )))           ;end cond, while, save-window-excursion
    (setq tex-cite-lookup-window-config (current-window-configuration))
    return-value
    ))

(defun tex-cite-get-file-list ()
  (let ((fn (read-string tex-cite-bibmsg)))
    (cond ((string-equal fn "")
	   (or tex-cite-wildcard
	       (setq tex-cite-wildcard
		     (tex-cite-list-wildcard tex-cite-ext tex-bib-path))))
	  ((setq fn (vortex-file-exists-p (tex-cite-fn-ext fn tex-cite-ext)
				       tex-bib-path))
	   (list fn))
	  (t nil))))

(defun tex-cite-new-entry (&optional abort)
  (let* ((func (read-command "Creating new entry of type: "))
	 (key (read-string "Entry name: "))
	 (fn (tex-cite-fn-ext (read-string tex-cite-bibmsg) tex-cite-ext))
	 (tmp (vortex-file-exists-p fn tex-bib-path))
	 (entry nil))
    (if tmp
      (setq fn tmp))
    (save-window-excursion
      (find-file-other-window fn)
      (goto-char (point-max))
      (funcall func)
      (insert key)
      (bibtex-next-field 1)
      (message "Fill in rest of empty fields, type ESC C-c to return.")
      (recursive-edit)
      (goto-char (point-max))
      (setq entry (bibtex-search-entry))
      (goto-char 1))
    (message "New entry {%s} created." key)
    (sit-for 1)
    (if (not abort)
	(list key (get-buffer fn) entry)
      (setq =entry= key)
      (setq =key= key)
      (throw abort key))))

(defun tex-cite-option-help ()
  (pop-to-buffer tex-cite-window)
  (erase-buffer)
  (insert-string
"        F -- Give up current .bib file, prompt for an alternate filename.
        K -- Give up current search key, prompt for an alternate query and
	     start searching from beginning of buffer.
        e -- Create a new .bib entry.
      C-r -- Enter recursive edit.  Use ESC C-c to return.
      C-c -- Quit, go back to previous level, if there is one.
        ? -- This help message.")
  (goto-char 1)
  (other-window 1))

(defun tex-cite-lookup-help ()
  (pop-to-buffer tex-cite-window)
  (erase-buffer)
  (insert-string
"    RET/y -- Confirm and exit.
      ESC -- Confirm and continue searching for another matching reference.
    SPC/n -- Go to next entry containing this key, if any.
    DEL/p -- Go to previous entry containing this key, if any.
        s -- Show the entry in other window.
        F -- Give up current .bib file, prompt for an alternate filename.
        K -- Give up current search key, prompt for an alternate query and
	     start searching from beginning of buffer.
        k -- Give up current search key, prompt for an alternate query and
	     start searching from current position.
        u -- Toggle citation note field prompting.
        m -- Toggle cite/nocite mode.
      C-r -- Enter recursive edit.  Use ESC C-c to return.
      C-c -- Quit, go back to previous level, if there is one.
        ? -- This help message.")
  (goto-char 1)
  (other-window 1))

