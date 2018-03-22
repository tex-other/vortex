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
;; $Header: /home/yew/yew4/vortex/newdist/emacs/RCS/BibTeX-ops.el,v 1.14 1993/09/16 02:32:14 munson Exp $
;;
;; This file, bibtex-ops.el, is a subsystem of BibTeX-mode.
;; It contains functions for entry and field operations
;; such as scroll, copy, delete, etc.
;; It gets autoloaded whenever a function defined in this file is invoked.
;;

(require 'BibTeX-mode)

;; ======================
;; Basic Entry operations
;; ======================

;(defun bibtex-locate-end-of-entry ()	; Not used by anybody, just for ref
;  (if (re-search-forward bibtex-entry-match nil t)
;    (if (re-search-backward bibtex-entry-close-regexp nil t)
;      (forward-char 1)
;      (error "Entry close symbol not found."))
;    (beginning-of-line)
;    (forward-list 1))
;  (if (eobp) (point) (1+ (point))))
         
(defun bibtex-goto-context (att)
  (goto-char (car (nth 3 att))))

(defun bibtex-previous-entry (&optional n &optional att-only)
  "Moves point to Nth previous entry label, if any, and return its attribute
list.  If the optional ATT-ONLY is non-nil, return the list only (point remains
unchanged)."
  (interactive "P")
  (let ((case-fold-search t))
    (setq n (or n 1))
    (cond
     ((> n 0)
      (let* ((ce (save-excursion (bibtex-search-entry)))
	     (pe (if ce
		     (save-excursion
		       (goto-char (car ce))
		       (bibtex-search-entry)))))
	(while (and (> n 1) pe)
	  (setq n (1- n))
	  (setq pe
		(save-excursion
		  (goto-char (car pe))
		  (bibtex-search-entry))))
	(if pe
	    (progn
	      (if att-only nil (bibtex-goto-context pe))
	      pe)
	  (if att-only nil (error "Failed to find previous entry.")))))
     ((< n 0) (bibtex-next-entry (- n) att-only))
     (t (bibtex-current-entry att-only))))) ; n=0

(defun bibtex-current-entry (&optional att-only)
  "Moves point to current entry label, if any, and return its attribute list.
If the optional ATT-ONLY is non-nil, return the list only (point remains
unchanged)."
  (interactive "P")
  (let* ((case-fold-search t)
	 (ce (save-excursion (bibtex-search-entry))))
    (if ce
	(progn
	  (if att-only nil (bibtex-goto-context ce))
	  ce)
      (if att-only nil (error "Failed to find current entry.")))))
	 
(defun bibtex-next-entry (&optional n &optional att-only)
  "Moves point to Nth next entry label, if any, and return its attribute list.
If the optional ATT-ONLY is non-nil, return the list only (point remains
unchanged)."
  (interactive "P")
  (let ((case-fold-search t))
    (setq n (or n 1))
    (cond
     ((> n 0)
      (let ((ne (save-excursion (bibtex-search-entry t))))
	(while (and (> n 1) ne)
	  (setq n (1- n))
	  (setq ne 
		(save-excursion
		  (bibtex-goto-context ne)
		  (bibtex-search-entry t))))
	(if ne
	    (progn
	      (if att-only nil (bibtex-goto-context ne))
	      ne)
	  (if att-only nil (error "Failed to find next entry.")))))
     ((< n 0) (bibtex-previous-entry (- n) att-only))
     (t (bibtex-current-entry att-only))))) ; n=0


;; =========================
;; Advanced Entry operations
;; =========================

(defun bibtex-dup-previous-entry (&optional n &optional quiet)
  "Duplicate Nth previous entry, if any, above current entry.
Move point to the new entry label.
If the optional QUIET is non-nil, do this quietly."
  (interactive "P")
    (setq n (or n 1))
    (if quiet nil (message "Duplicating previous entry #%d..." n))
    (let* ((case-fold-search t)
	   (pe (bibtex-previous-entry n t)))
      (if pe
	  (let ((eb (nth 0 pe))
		(ee (nth 1 pe)))
	    (goto-char eb)
	    (insert-string (buffer-substring eb ee))
	    (bibtex-current-entry)
	    (if quiet nil (message "Duplicating previous entry #%d...done" n)))
	(if quiet nil (error "Duplicating previous entry #%d...abort (entry not found)" n)))))

(defun bibtex-dup-current-entry (&optional quiet)
  "Copies current entry, if any, below current entry, and  
moves point to the new entry label.
If the optional QUIET is non-nil, does this quietly."
  (interactive "P")
    (if quiet nil (message "Duplicating current entry..."))
    (let* ((case-fold-search t)
	   (ce (bibtex-current-entry t)))
      (if ce
	  (let ((eb (nth 0 ce))
		(ee (nth 1 ce)))
	    (goto-char ee)
	    (insert-string (buffer-substring eb ee))
	    (bibtex-current-entry)
	    (if quiet nil (message "Duplicating current entry...done")))
	(if quiet nil (error "Duplicating current entry...abort (entry not found)")))))

(defun bibtex-dup-next-entry (&optional n &optional quiet)
  "Duplicate Nth next entry, if any, below current entry.
Move point to the new entry label.
If the optional QUIET is non-nil, do this quietly."
  (interactive "P")
  (setq n (or n 1))
  (if quiet nil (message "Duplicating next entry #%d..." n))
  (let* ((case-fold-search t)
	 (ne (bibtex-next-entry n t)))
    (if ne
	(let ((eb (nth 0 ne))
	      (ee (nth 1 ne)))
	  (goto-char eb)
	  (insert-string (buffer-substring eb ee))
	  (bibtex-current-entry)
	  (if quiet nil (message "Duplicating next entry #%d...done" n)))
      (if quiet nil (error "Duplicating next entry #%d...abort (entry not found)" n)))))

(defun bibtex-kill-previous-entry (&optional n &optional quiet)
  "Kill Nth previous entry, if any.  Point remains unchanged.
If the optional QUIET is non-nil, do this quietly."
  (interactive "P")
  (setq n (or n 1))
  (if quiet nil (message "Killing previous entry #%d..." n))
  (let* ((case-fold-search t)
	 (pe (bibtex-previous-entry n t)))
    (if pe
	(let ((eb (nth 0 pe))
	      (ee (nth 1 pe)))
	  (kill-region eb ee)
	  (if quiet nil (message "Killing previous entry #%d...done" n)))
      (if quiet nil (error "Killing previous entry #%d...abort (entry not found)" n)))))

(defun bibtex-kill-current-entry (&optional quiet)
  "Kill current entry, if any.  Point is moved to next entry label, if any;
else point is moved to previous entry label, if any.
If the optional QUIET is non-nil, do this quietly."
  (interactive "P")
    (if quiet nil (message "Killing current entry..."))
    (let* ((case-fold-search t)
	   (ce (bibtex-current-entry t)))
      (if ce
	  (let ((eb (nth 0 ce))
		(ee (nth 1 ce))
		(ne (bibtex-next-entry 1 t)))
	    (kill-region eb ee)
	    (if ne
		(bibtex-next-entry)
	      (let ((pe (bibtex-current-entry t)))
		(if pe
		    (bibtex-current-entry))))
	    (if quiet nil (message "Killing current entry...done")))
	(if quiet nil (error "Killing current entry...abort (entry not found)")))))

(defun bibtex-kill-next-entry (&optional n &optional quiet)
  "Kill Nth next entry, if any.  Point remains unchanged.
If the optional QUIET is non-nil, do this quietly."
  (interactive "P")
  (setq n (or n 1))
  (if quiet nil (message "Killing next entry #%d..." n))
  (let* ((case-fold-search t)
	 (ne (bibtex-next-entry n t)))
    (if ne
	(let ((eb (nth 0 ne))
	      (ee (nth 1 ne)))
	  (kill-region eb ee)
	  (if quiet nil (message "Killing next entry #%d...done" n)))
      (if quiet nil (error "Killing next entry #%d...abort (entry not found)" n)))))
      
(defun bibtex-rename-current-entry (new)
  "Rename current entry to one of type NEW."
  (interactive "aRename current entry: ")
  (let* ((case-fold-search t)
	 (type1 (nth 2 (nth 2 (bibtex-current-entry t))))
	 (type2 (upcase (symbol-name new))))
    (funcall new)
    (message "Renaming current entry from %s to %s..." type1 type2)
    (save-excursion
      (let* ((ce (bibtex-current-entry))
	     (ee (nth 1 ce))
	     (boundary (point)))
	(goto-char ee)
	(bibtex-current-field)
	(while (< boundary (point))
	  (bibtex-text-previous-entry 1 t)
	  (bibtex-previous-field)
	  (sit-for 0))
	(if (= boundary (point))
	  (bibtex-text-previous-entry 1 t))
	(bibtex-kill-previous-entry 1 t)))
      (message "Renaming current entry from %s to %s...done" type1 type2)))


;; ======================
;; Basic Field Operations
;; ======================

;; The next three functions are the user-accessible functions for moving
;; from field to field

(defun bibtex-current-field ()
  "Moves point to current field text, if any, and return its attribute list."
  (interactive)
  (let ((cf (bibtex-current-field-data)))
    (if cf
	(progn
	  (goto-char (car (nth 3 cf)))
	  cf)
      (error "Failed to find current field."))))

(defun bibtex-next-field (&optional n)
  "Move point to Nth next field text, if any, and return its attribute list."
  (interactive "P")
  (if (null n) (setq n 1))
  (cond
   ((< n 0) (bibtex-previous-field (- n)))
   ((= n 0) (bibtex-current-field))
   (t
    (let ((nf (bibtex-next-field-data n)))
      (if nf
	  (progn
	    (goto-char (car (nth 3 nf)))
	    nf)
	(error "Failed to find next field."))))))

(defun bibtex-previous-field (&optional n)
  "Move point to Nth previous field text, if any, 
and return its attribute list."
  (interactive "P")
    (if (null n) (setq n 1))
    (cond
     ((< n 0) (bibtex-next-field (- n)))
     ((= n 0) (bibtex-current-field))
     (t
      (let ((pf (bibtex-previous-field-data n)))
	(if pf
	    (progn
	      (goto-char (car (nth 3 pf)))
	      pf)
	  (error "Failed to find previous field."))))))

(defconst bibtex-id-regexp "[a-z][]a-z0-9.:;?!`[---/*@$&~_^\\+|<>]*"
  "Matches bibtex entry and field names and abbreviations.  Based on pages 12
and 143 of the LaTeX book")

(defconst bibtex-key-regexp "[]a-z0-9.:;?!`'()[-/*@]*"
  "Matches BibTeX keys and the empty string.  Based on pages 12 and 73
of the LaTeX book.")

(defconst bibtex-entry-name-regexp
  (concat "[@%]\\s *\\(" bibtex-id-regexp "\\)\\s *[{(]")
  "Matches the entry name and intial paren or brace.")

(defconst bibtex-field-name-regexp
  (concat "[ \t\n,{(]\\(" bibtex-id-regexp "\\)\\s *=")
  "Matches the field name and equal sign.")

(defconst bibtex-field-regexp
  (concat bibtex-entry-name-regexp "\\|" bibtex-field-name-regexp))

(defun bibtex-next-field-data (number)
  (save-excursion
    (let* ((cf (bibtex-current-field-data))
	   (case-fold-search t)
	   (string-p (if cf (string-match "@string" (nth 2 (nth 2 cf)))))
	   md)
      (if (and cf string-p)
	  (goto-char (nth 1 (nth 2 cf))))
      (while (and (> number 0)
		  (re-search-forward bibtex-field-regexp nil t))
	(setq md (match-data))
	(if (or (string-match bibtex-field-match (vortex-match-string 0))
		string-p)
	    (setq number (1- number))))
      (if (eq number 0)
	  (bibtex-field-value md)
	nil))
    ))

(defun bibtex-previous-field-data (number)
  (let ((cf (bibtex-current-field-data))
	(case-fold-search t)
	md)
    (if (< (nth 1 cf) (point))
	;; Point is between two entries, so current-field is also
	;; previous field
	(if (= number 1)
	    cf
	  (goto-char (car cf))
	  (bibtex-previous-field-data (1- number)))
      (save-excursion
	(goto-char (car cf))
	(while (and (> number 0)
		    (re-search-backward bibtex-field-regexp nil t))
	  (setq md (match-data))
	  (forward-char)		;repositions point for @string case
	  (if (or (string-match bibtex-field-match (vortex-match-string 0))
		  (string-match
		   "@STRING"
		   (nth 2 (nth 2 (bibtex-current-field-data)))))
	      (setq number (1- number))))
	(if (eq number 0)
	    (bibtex-field-value md)
	  nil)
	))))

;; This function returns a list containing the following elements:
;;	Field start position
;;	Field end position
;;	Field label descriptor (start end text)
;;	Field text descriptor (start end text) where text does not include
;;		any delimiters
;; as in (FS FE (LS LE LT) (TS TE TT))
(defun bibtex-current-field-data ()
  (save-excursion
    (let ((case-fold-search t)
	  (bol (save-excursion (beginning-of-line) (point)))
	  (pos (point))
	  md1 md2 string-p
	  )
      ;; find preceding field header and save the match data
      ;; STRING-P is t if the preceding field header is an abbreviation
      (if (re-search-backward bibtex-field-regexp nil t)
	  (progn
	    (setq md1 (match-data)
		  string-p (looking-at "@\\s *string"))
	    (goto-char (match-end 0)))
	(goto-char (point-min))
	)
      ;; find the next field header in the buffer.
      ;; for entries, the original point was either inside this field header
      ;; or was in the text area of the preceding field header
      (if (re-search-forward bibtex-field-regexp nil t)
	  (setq md2 (match-data))
	)
      (cond
       ;; Case 1: Either a buffer with no fields or the point lay
       ;; before the first field in the buffer.
       ((and (null md1) (or (null md2) (< pos (match-beginning 0))))
	nil)
       ;; Case 2: Point was in or after the last field in the buffer
       ((null md2) (bibtex-field-value md1))
       ;; Case 3: Point was in the field label.  If this is the one
       ;; label that appears in an @string, we consider the current field
       ;; to be the one with the preceding header.
       ((>= pos (match-beginning 0))
	(if string-p
	    (bibtex-field-value md1)
	  (bibtex-field-value md2)))
       ;; Case 4: Point was before the second field header but on the
       ;; same line and there is only whitespace between point and
       ;; this header.  It's a weird rule but it yields a good user
       ;; interface.
       ((not (save-excursion
	       (goto-char bol)
	       (re-search-forward "[^ \t]" (match-beginning 0) t)))
	(bibtex-field-value md2))
       ;; Case 5: In general, this case applies when point is in
       ;; the text area of a field.
       (t (bibtex-field-value md1))
       ))))

(defun bibtex-field-value (header-match-data)
  (store-match-data header-match-data)
  (let* ((bofm (match-beginning 0))	;beginning of first match
	 (eofm (match-end 0))		;end of first match
	 (first-char (buffer-substring bofm (1+ bofm)))
	 (entry (string-equal "@" first-char))
	 (group (string-equal "%" first-char))
	 (bof (if (or entry group) bofm (1+ bofm)))
	 (bon (match-beginning (if (or entry group) 0 2)))
	 (real-bon (match-beginning (if (or entry group) 1 2)))
	 (eon (match-end (if (or entry group) 1 2)))
	 (name (buffer-substring real-bon eon))
	 eof bot eot fail
	 )
    (goto-char eofm)
    (cond
     ((and entry (string-match "string" name))
      (setq name (concat "@" name)
	    eof (save-excursion
		  (search-forward "=" nil t)
		  (match-beginning 0)))
      (if (re-search-forward bibtex-id-regexp eof t)
	  (setq bot (match-beginning 0)
		eot (match-end 0))
	(setq bot eofm
	      eot eofm)))
     (entry
      (setq name (concat "@" name))
      (re-search-forward
       (concat "\\(" bibtex-key-regexp "\\)\\s *,") nil t)
      (setq eof (match-end 0)
	    bot (match-beginning 1)
	    eot (match-end 1)))
     (group
      (setq name "%GROUP")
      (re-search-forward
       (concat "\\(" bibtex-key-regexp "\\)\\s *,") nil t)
      (setq eof (match-end 0)
	    bot (match-beginning 1)
	    eot (match-end 1)))
     ((re-search-forward "[\"{,}]" nil t)
      (let* ((stop-char (match-beginning 0))
	     (next-char (match-end 0))
	     (char (buffer-substring stop-char next-char))
	     )
	(cond
	 ((string-match "[\"{]" char)
	  (goto-char stop-char)
	  (forward-sexp)
	  (setq eot (1- (point)))
	  (re-search-forward "[,}]" nil t)
	  (setq eof (if (string-equal 
			 (vortex-match-string 0)
			 "}")
			(1+ eot)
		      (match-end 0))
		bot next-char))
	 ((re-search-backward (concat "[ \t\n=]" bibtex-id-regexp) eofm t)
	  (setq eof (if (string-equal "," char)
			next-char
		      (1- stop-char))
		bot (1+ (match-beginning 0))
		eot (match-end 0)))
	 (t
	  (if (eq 1 (- next-char eofm))
	      (progn
		(backward-char)
		(insert " ")))
	  (setq eof (if (string-equal "," char)
			next-char
		      (1+ eofm))
		bot (1+ eofm)
		eot bot))
	 )))
     (t (setq fail t))
     )
    (if fail
	nil 
      (list bof eof (list bon eon name)
	    (list bot eot (buffer-substring bot eot))))
    ))

;; =========================
;; Advanced Field Operations
;; =========================

(defun bibtex-erase-delimiters (&optional quiet)
  "Erase the embracing field delimiters of current field, if any."
  (interactive)
    (if quiet nil (message "Erasing current field delimiters..."))
    (let* ((case-fold-search t)
	   (cf (bibtex-current-field-data))
	   (fb (car (nth 3 cf))))
      (goto-char (1- fb))
      (if (looking-at "[{\"]")
	  (let ((back (point)))
	    (forward-sexp 1)
	    (delete-char -1)		; Delete [\"}]
	    (goto-char back)
	    (delete-char 1)
	    (if quiet nil (message "Erasing current field delimiters...done")))
	(if quiet nil (error "Erasing current field delimiters...abort (delimiters missing)")))))

(defun bibtex-erase-field (&optional quiet)
  "Erase current field, if any.  Move point to next field, if any;
else move point to previous field, if any.
If the optional QUIET is non-nil, do this quietly."
  (interactive)
    (if quiet nil (message "Erasing current field..."))
    (let* ((case-fold-search t)
	   (cf (bibtex-current-field-data)))
      (if cf
	  (let ((fb (nth 0 cf))
		(fe (nth 1 cf))
		(pf (bibtex-previous-field-data 1)))
	    (if pf
		(if (= (nth 1 pf) fe)	; RIGHT field of @STRING
		    (setq fb (1- fb))))
	    (kill-region fb fe)
	    (let ((nf (bibtex-next-field-data 1)))
	      (if nf
		  (bibtex-goto-context nf)
		(let ((new-cf (bibtex-current-field)))
		  (if new-cf
		      (bibtex-goto-context new-cf)))))
	    (if quiet nil (message "Erasing current field...done")))
	(if quiet nil (error "Erasing current field...abort (field missing)")))))

(defun bibtex-erase-text (&optional quiet)
  "Erase the text of current field.  Point remains unchanged.
If the optional QUIET is non-nil, do this quietly."
  (interactive)
  (if quiet nil (message "Erasing current text..."))
  (let* ((case-fold-search t)
	 (cf (bibtex-current-field)))
	(if cf
	    (let* ((xt (nth 3 cf))
		   (xb (car xt))
		   (xe (nth 1 xt)))
	      (kill-region xb xe)
	      (if quiet nil (message "Erasing current text...done")))
	  (if quiet nil (message "Erasing current text...abort (text missing)")))))

(defun bibtex-text-previous-entry (&optional n &optional quiet)
  "Copy field text from Nth previous entry, if any, before point.
If the optional QUIET is non-nil, do this quietly."
  (interactive "P")
  (setq n (or n 1))
  (if quiet nil (message "Copying text from previous entry #%d..." n))
  (let* ((case-fold-search t)
	 (cf (bibtex-current-field-data)))
    (if cf
      (let ((pe (bibtex-previous-entry n t)))
        (if pe
	  (let* ((tp (nth 2 cf))
		 (tb (car tp))
		 (te (nth 1 tp))
		 (type (nth 2 tp)))
	    (cond
	      ((string-equal type bibtex-abbrev)          ; abbreviation
	       (if quiet
		 nil
		 (error "Copying text from previous entry #%d...abort (can't copy abbreviation)" n)))
	      ((save-excursion
                 (goto-char tb)
		 (looking-at bibtex-entry-match))         ; entry label
	       (insert-string (nth 2 (nth 3 pe)))
	       (if quiet
		 nil
                 (message "Copying text from previous entry #%d...done (entry label)" n)))
	      (t                                      ; regualr field text
	       (bibtex-get-text n pe type "previous" quiet))))
	  (if quiet
	    nil
	    (error "Copying text from previous entry #%d...abort (entry not found)" n))))
      (if quiet nil (error "Copying text from previous entry #%d...abort (field missing)" n)))))
  
(defun bibtex-text-next-entry (&optional n &optional quiet)
  "Copy field text from Nth next entry, if any, before point.
If the optional QUIET is non-nil, do this quietly."
  (interactive "P")
  (setq n (or n 1))
  (if quiet nil (message "Copying text from next entry #%d..." n))
  (let* ((case-fold-search t)
	 (cf (bibtex-current-field-data)))
    (if cf
	(let ((ne (bibtex-next-entry n t)))
	  (if ne
	      (let* ((tp (nth 2 cf))
		     (tb (car tp))
		     (te (nth 1 tp))
		     (type (nth 2 tp)))
		(cond
		 ((string-equal type bibtex-abbrev) ; abbreviation
		  (if quiet
		      nil
		    (error "Copying text from next entry #%d...abort (can't copy abbreviation)" n)))
		 ((save-excursion
		    (goto-char tb)
		    (looking-at bibtex-entry-match)) ; entry label
		  (insert-string (nth 2 (nth 3 ne)))
		  (if quiet
		      nil
		    (message "Copying text from next entry #%d...done (entry label)" n)))
		 (t			; regular field text
		  (bibtex-get-text n ne type "next" quiet))))
	    (if quiet
		nil
	      (error "Copying text from next entry #%d...abort (entry not found)" n))))
      (if quiet
	  nil
	(error "Copying text from next entry #%d...abort (missing field)" n)))))

(defun bibtex-get-text (n entry type p-or-n &optional quiet)
  (let* ((eb (car entry))
	 (ee (nth 1 entry))
	 (regexp (bibtex-get-spec type bibtex-field-types bibtex-field-specs))
	 (source
	  (save-excursion
	    (goto-char eb)
	    (if (or (re-search-forward type ee t)
		    (if regexp (re-search-forward regexp ee t)))
		(bibtex-current-field))))
	 label)
    (if (and source
	     (progn
	       (setq label (nth 2 (nth 2 source)))
	       (or (string-equal label type)
		   (y-or-n-p (concat type " not found, copy " label " instead? ")))))
	(progn
	  (insert (nth 2 (nth 3 source)))
	  (if quiet (message "") (message "Copying text from %s entry #%d...done (%s)." p-or-n n label)))
      (if quiet
	  (message "")
	(ding)
	(message "Copying text from %s entry #%d...abort (field %s not in entry)" p-or-n n type)))))

(defun bibtex-zap-name ()
  (interactive)
  (let ((case-fold-search t)
	begin name)
    (save-excursion
      (search-backward "@")
      (forward-word 1)
      (setq begin (1+ (point)))
      (re-search-forward ":\\|,\\|$")
      (setq name (buffer-substring begin (if (eolp) (point) (1- (point))))))
    (if (= (preceding-char) ? )
	(insert name)
      (insert ?  name))
    (capitalize-word -1)))
