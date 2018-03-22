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
;; $Header: /home/yew/yew4/vortex/newdist/emacs/RCS/BibTeX-mode.el,v 1.14 1993/09/16 02:32:14 munson Exp $
;;
;; This file, BibTeX-mode.el, is a GNU Emacs frontend for creating and
;; maintaining BibTeX bibliography databases.
;;


(provide 'BibTeX-mode)

;; =================================================
;; Load functions common to TeX-mode and BibTeX-mode
;; =================================================
(require 'VorTeX-common)

;; ============================================================
;; Global variables (can be changed in user's bibtex-mode-hook)
;; ============================================================

(defvar bibtex-abbrev-files nil "List of default abbreviation files")
(defvar bibtex-abbrev-fields '("JOURNAL" "MONTH") 
        "List of fields that always use abbrevations")
(defvar bibtex-entry-use-parens nil 
  "Default embracing delimiter pair is braces")
(defvar bibtex-field-use-quotes nil 
  "Default embracing delimiter for field texts is braces")
(defvar bibtex-field-indent 8 "Indentation for entry fields.")
(defvar bibtex-context nil "Global context to be included in draft")  
(defvar bibtex-softcopy 
  (cond ((and (eq window-system 'x) (eq window-system-version 11))
	 "dvi2x11")
	((and (eq window-system 'x) (eq window-system-version 10))
	 "dvi2x")
	(t "dvitool"))
  "TeX DVI previewer.")    ; System dependent
(defvar bibtex-error-pos 0 "Position of last BibTeX error in *shell*")
(defvar bibtex-preview-header nil
  "Customized header string to be inserted after the default header.")

;; Handling cases for the SUN window system (default for anything other than X)
(if (not (string-equal window-system "x"))
  (setq bibtex-softcopy "dvitool"))


;; ===========================
;; Constants for Abbreviations
;; ===========================

(defconst bibtex-abbrev "@STRING")
(defconst bibtex-abbrev-match "@\\s *STRING\\s *[{(]")
(defconst bibtex-abbrev-match-fg "@\\s *STRING\\s *[{(]\\|%\\s *GROUP\\s *[{(]")
(defconst bibtex-abbrev-empty "@\\s *STRING\\s *[{(]\\s *=")


;; =====================
;; Constants for Entries
;; =====================

(defvar bibtex-entry-open ?{ "Default open symbol for entries")
(defvar bibtex-entry-close ?} "Default close symbol for entries")
(defconst bibtex-entry-close-regexp "[})]")
(defconst bibtex-entry-no-group
  '("ARTICLE"      "BOOK"          "BOOKLET"    "CONFERENCE"    "INBOOK" 
    "INCOLLECTION" "INPROCEEDINGS" "MANUAL"     "MASTERSTHESIS" "MISC" 
    "PHDTHESIS"    "PROCEEDINGS"   "TECHREPORT" "UNPUBLISHED" "QUERY"))
(defconst bibtex-entry-no-group-match
  (vortex-mkstring "@\\s *" "\\s *[{(]" bibtex-entry-no-group))
(defconst bibtex-entry-types
  (append (mapcar '(lambda (en) (concat "@" en)) bibtex-entry-no-group)
	  (list "%GROUP")))
(defconst bibtex-entry-match
  (concat bibtex-entry-no-group-match "\\|%\\s *GROUP\\s *[{(]"))
(defconst bibtex-entry-empty
  (vortex-mkstring "@\\s *" "\\s *[{(]\\s *," bibtex-entry-no-group))
(defconst bibtex-entry-specs
  '(("@ARTICLE"
     nil
     nil
     ("AUTHOR" "TITLE" "JOURNAL" "YEAR")
     ("VOLUME" "NUMBER" "PAGES" "MONTH" "NOTE")
     )
    ("@BOOK"
     ("AUTHOR" "EDITOR")
     nil
     ("TITLE" "PUBLISHER" "YEAR")
     ("VOLUME" "SERIES" "ADDRESS" "EDITION" "MONTH" "NOTE")
     )
    ("@BOOKLET"
     nil
     nil
     ("TITLE")
     ("AUTHOR" "HOWPUBLISHED" "ADDRESS" "MONTH" "YEAR" "NOTE")
     )
    ("@CONFERENCE"
     nil
     nil
     ("AUTHOR" "TITLE" "BOOKTITLE" "YEAR")
     ("EDITOR" "PAGES" "ORGANIZATION" "PUBLISHER" "ADDRESS" "MONTH" "NOTE")
     )
    ("@INBOOK"
     ("AUTHOR" "EDITOR")
     ("CHAPTER" "PAGES")
     ("TITLE" "PUBLISHER" "YEAR")
     ("VOLUME" "SERIES" "ADDRESS" "EDITION" "MONTH" "NOTE")
     )
    ("@INCOLLECTION"
     nil
     nil
     ("AUTHOR" "TITLE" "BOOKTITLE" "PUBLISHER" "YEAR")
     ("EDITOR" "CHAPTER" "PAGES" "ADDRESS" "MONTH" "NOTE")
     )
    ("@INPROCEEDINGS"
     nil
     nil
     ("AUTHOR" "TITLE" "BOOKTITLE" "YEAR")
     ("EDITOR" "PAGES" "ORGANIZATION" "PUBLISHER" "ADDRESS" "MONTH" "NOTE")
     )
    ("@MANUAL"
     nil
     nil
     ("TITLE")
     ("AUTHOR" "ORGANIZATION" "ADDRESS" "EDITION" "MONTH" "YEAR" "NOTE")
     )
    ("@MASTERSTHESIS"
     nil
     nil
     ("AUTHOR" "TITLE" "SCHOOL" "YEAR")
     ("ADDRESS" "MONTH" "NOTE")
     )
    ("@MISC"
     nil
     nil
     nil
     ("AUTHOR" "TITLE" "HOWPUBLISHED" "MONTH" "YEAR" "NOTE")
     )
    ("@PHDTHESIS"
     nil
     nil
     ("AUTHOR" "TITLE" "SCHOOL" "YEAR")
     ("ADDRESS" "MONTH" "NOTE")
     )
    ("@PROCEEDINGS"
     nil
     nil
     ("TITLE" "YEAR")
     ("EDITOR" "PUBLISHER" "ORGANIZATION" "ADDRESS" "MONTH" "NOTE")
     )
    ("@TECHREPORT"
     nil
     nil
     ("AUTHOR" "TITLE" "INSTITUTION" "YEAR")
     ("TYPE" "NUMBER" "ADDRESS" "MONTH" "NOTE")
     )
    ("@UNPUBLISHED"
     nil
     nil
     ("AUTHOR" "TITLE" "NOTE")
     ("MONTH" "YEAR"))))


;; ====================
;; Constants for Fields
;; ====================

(defvar bibtex-field-pair "{}" "Default embracing symbol for field texts is braces")
(defconst bibtex-field-types
  '("ADDRESS" "ANNOTE"    "AUTHOR"       "BOOKTITLE"   "CHAPTER" 
    "EDITION" "EDITOR"    "HOWPUBLISHED" "INSTITUTION" "JOURNAL"
    "KEY"     "MONTH"     "NOTE"         "NUMBER"      "ORGANIZATION"  
    "PAGES"   "PUBLISHER" "SCHOOL"       "SERIES"      "TITLE" 
    "TYPE"    "VOLUME"    "YEAR"         "NOTEFILES"   "ANYFIELD"
    ))
(defconst bibtex-field-match
  (concat (vortex-mkstring "\\b" "\\s *=" bibtex-field-types) "\\|"
	  bibtex-entry-match "\\|" bibtex-abbrev-match))
(defconst bibtex-field-empty
  (concat " *= *[\"{] *[}\"] *, *$\\| *= *[\"{] *[}\"] *$\\| *= *[ ,}] *$\\| *= *\" *\" *}"
	  "\\|" bibtex-entry-empty "\\|" bibtex-abbrev-empty))
(defconst bibtex-field-specs
  '("ADDRESS *="
    "ANNOTE *="
    "AUTHOR *=\\|EDITOR *="
    "BOOKTITLE *=\\|JOURNAL *="
    "CHAPTER *="
    "EDITION *="
    "EDITOR *=\\|AUTHOR *="
    "HOWPUBLISHED *="
    "INSTITUTION *=\\|ORGANIZATION *=\\|SCHOOL *="
    "JOURNAL *=\\|BOOKTITLE *="
    "KEY *="
    "MONTH *="
    "NOTE *="
    "NOTEFILES *="
    "NUMBER *="
    "ORGANIZATION *=\\|INSTITUTION *=\\|SCHOOL *="
    "PAGES *="
    "PUBLISHER *="
    "SCHOOL *=\\|INSTITUTION *=\\|ORGANIZATION *="
    "SERIES *="
    "TITLE *=\\|BOOKTITLE *="
    "TYPE *="
    "VOLUME *="
    "YEAR *="
    "ANYFIELD *="))
(defvar bibtex-extra-fields nil "Fields which the user wishes to have appear
in all entries")


;; ===========
;; BibTeX Mode
;; ===========

(defvar bibtex-mode-syntax-table nil "Syntax table in use in BibTeX-mode buffers.")
(defvar bibtex-mode-abbrev-table nil "Abbrev table in use in BibTeX-mode buffers.")
(defvar bibtex-mode-customization nil "Flag t when BibTeX mode is customized.")

(defun bibtex-mode (&optional quiet)
  "Major mode for editing BibTeX database entries."
  (interactive)
  (kill-all-local-variables)
  (if (not bibtex-mode-map) (bibtex-mode-keymap-init))
  (use-local-map bibtex-mode-map)
  (setq major-mode 'bibtex-mode)
  (setq mode-name "BibTeX")
  (setq paragraph-start "^ *$\\|^")
  (setq paragraph-separate "^ *$\\|^")
  (define-abbrev-table 'bibtex-mode-abbrev-table nil)
  (setq local-abbrev-table bibtex-mode-abbrev-table)
  (make-local-variable 'bibtex-require-local-eval)
  (setq bibtex-require-local-eval nil)
  (if bibtex-mode-syntax-table
      (set-syntax-table bibtex-mode-syntax-table)
    (setq bibtex-mode-syntax-table (make-syntax-table))
    (set-syntax-table bibtex-mode-syntax-table)
;; Syntax Entry Modifications
    (modify-syntax-entry ?{ "(}")
    (modify-syntax-entry ?} "){")
    (modify-syntax-entry ?[ "(]")
    (modify-syntax-entry ?] ")[")
;; Global Initialization
    (let ((init (vortex-file-exists-p "BibTeX-init.el" load-path)))
      (if init
	(load "BibTeX-init")
	(setq init (vortex-file-exists-p "BibTeX-init.elc" load-path))
	(if init
	  (load "BibTeX-init"))))
;; Copyright Message
    (or quiet (bibtex-copyright)))
;; User-specific Hook
  (run-hooks 'bibtex-mode-hook)
  (if bibtex-mode-customization
    nil
;; Alternative Delimiters
    (setq bibtex-mode-customization t)
    (if bibtex-entry-use-parens
      (setq bibtex-entry-open ?\(
	    bibtex-entry-close ?\)))
    (if bibtex-field-use-quotes
      (setq bibtex-field-pair "\"\""))))

;; ============
;; Key Bindings
;; ============

(defvar bibtex-mode-map nil "Keymap used in BibTeX mode.")
(defvar bibtex-control-c-map nil)

(defvar bibtex-keymap-data
  '((nil
     (bibtex-mode-map
      ("\C-c" bibtex-control-c-map)
      ("\^c\^h" bibtex-mode-help) ("\e\n" bibtex-newline-indent)    
      ("\n" bibtex-newline-indent-label) ("\^c\^a " bibtex-abbrev-enable)
      ("\^c\^a\177" bibtex-abbrev-disable) ("\^c\n " bibtex-autofill-enable)
      ("\^c\n\177" bibtex-autofill-disable)))
    ("BibTeX-misc"
     (bibtex-mode-map
      ("\^c\eb" bibtex-cleanup-buffer) ("\^c\er" bibtex-cleanup-region)
      ("\^c\ee" bibtex-cleanup-entry) ("\^c\nb" bibtex-sort-buffer)
      ("\^c\nr" bibtex-sort-region) ("\^c\^\\d" bibtex-make-draft)
      ("\^c\^@" bibtex-goto-error)))
    ("BibTeX-ops"
     (bibtex-mode-map
      ("\^cp" bibtex-previous-entry) ("\^cc" bibtex-current-entry)
      ("\^cn" bibtex-next-entry) ("\^c\^dp" bibtex-dup-previous-entry)
      ("\^c\^dc" bibtex-dup-current-entry) ("\^c\^dn" bibtex-dup-next-entry)
      ("\^c\^kp" bibtex-kill-previous-entry)
      ("\^c\^kc" bibtex-kill-current-entry) ("\^c\^kn" bibtex-kill-next-entry)
      ("\^c\^r" bibtex-rename-current-entry) ("\^c\^p" bibtex-previous-field)
      ("\^c\^c" bibtex-current-field) ("\^c\^n" bibtex-next-field)
      ("\^c\^ed" bibtex-erase-delimiters) ("\^c\^ef" bibtex-erase-field)
      ("\^c\^et" bibtex-erase-text) ("\^c\^tp" bibtex-text-previous-entry)
      ("\^c\^tn" bibtex-text-next-entry) ("\^c\^z" bibtex-zap-name)
      (nil bibtex-current-field-data)))
    ("BibTeX-abv"
     (bibtex-mode-map
      ("\^c\^l" bibtex-load-abbrev) ("\^c\^s" bibtex-save-abbrev)
      ("\^c\^ig" bibtex-insert-gabbrev) ("\^c\^if" bibtex-insert-fabbrev)
      ("\^c\^\\g" bibtex-make-gabbrev)))
    ("BibTeX-notes"
     (bibtex-mode-map
      ("\^c\^v" bibtex-view-notefiles)))))

(defun bibtex-mode-keymap-init ()
  (mapcar 'define-prefix-command
	  '(bibtex-mode-map bibtex-control-c-map))
  (vortex-keymap-util bibtex-keymap-data)
  (setq bibtex-mode-map (symbol-function 'bibtex-mode-map))
  )


;; ============
;; Indentation
;; ============

(defun bibtex-newline-indent ()
  (interactive)
  (newline)
  (indent-to bibtex-field-indent))

(defun bibtex-newline-indent-label ()
  (interactive)
  (let ((case-fold-search t))
    (newline)
    (indent-to (save-excursion
		 (goto-char (car (nth 3 (bibtex-current-field-data))))
		 (current-column)))
    ))

(defun bibtex-do-auto-fill ()
  "This is a modification of the code available in simple.el.
The reason to modify the default is for indentations of field text to
work properly in auto-fill minor mode."
  (let ((opoint (point)))
    (save-excursion
      (move-to-column (1+ fill-column))
      (skip-chars-backward "^ \t\n")
      (if (bolp)
	  (re-search-forward "[ \t]" opoint t))
      ;; If there is a space on the line before fill-point,
      ;; and nonspaces precede it, break the line there.
      (if (save-excursion
	    (skip-chars-backward " \t")
	    (not (bolp)))
	(progn
	  (indent-new-comment-line)
	  (indent-to (save-excursion
		       (goto-char (car (nth 3 (bibtex-current-field))))
		       (current-column))))))))

(defun bibtex-auto-fill-mode (arg)
  "Toggle auto-fill mode.
With arg, turn auto-fill mode on iff arg is positive.
In auto-fill mode, inserting a space at a column beyond  fill-column
automatically breaks the line at a previous space."
  (interactive "P")
  (setq auto-fill-hook
	(and
	  (if (null arg) (not auto-fill-hook)
	    (> (prefix-numeric-value arg) 0))
	  'bibtex-do-auto-fill)))

(defun bibtex-autofill-enable ()
  (interactive)
  (bibtex-auto-fill-mode 1)
  (message "Auto-fill minor mode enabled (fill column is %d)." fill-column))

(defun bibtex-autofill-disable ()
  (interactive)
  (bibtex-auto-fill-mode 0)
  (message "Auto-fill minor mode disabled."))

(defun bibtex-abbrev-enable ()
  (interactive)
  (abbrev-mode 1)
  (message "Abbrev minor mode enabled."))

(defun bibtex-abbrev-disable ()
  (interactive)
  (abbrev-mode 0)
  (message "Abbrev minor mode disabled."))

;; =============
;; Miscellaneous
;; =============

(defun bibtex-mode-version ()
  (interactive)
  (message "BibTeX-mode V1.12 (1/26/92)."))

(defun bibtex-copyright ()
  (if (and (boundp 'tex-bib-to-bibtex) tex-bib-to-bibtex)
    nil
    (message "%s C-c C-h for help.  Copyright (C) Regents UC." (bibtex-mode-version))))
		    

;; ========================
;; BibTeX-mode Help Message
;; ========================

(defun bibtex-mode-help ()
  (interactive)
  (pop-to-buffer "--- BibTeX-mode Help ---")
  (if (= (buffer-size) 0)
    (progn
      (insert-string
"                          ----- Catogeries -----
Entry Operations	    Built-in Entry Types       Field Operations
Abbreviation Mechanism      Cleanup Facility	       Sorting Facilty
Draft Making & Debugging    Mode Operations	       Useful Variables

                        ----- Entry Operations -----
M-x<type>  invoke new entry of <type>
C-c p	   bibtex-previous-entry        C-c c      bibtex-current-entry
C-c n      bibtex-next-entry            C-c C-r    bibtex-rename-current-entry
C-c C-d p  bibtex-dup-previous-entry    C-c C-d c  bibtex-dup-current-entry  
C-c C-d n  bibtex-dup-next-entry        C-c C-k p  bibtex-kill-previous-entry
C-c C-k c  bibtex-kill-current-entry    C-c C-k n  bibtex-kill-next-entry

                     ----- Built-in Entry Types -----
	@article          @book           @booklet        @conference
	@inbook           @incollection   @inproceedings  @manual
	@masterthesis     @misc           @phdthesis      @proceeding
	@techreport       @unpublished

                       ----- Field Operations -----
C-c C-p    bibtex-previous-field         C-c C-c    bibtex-current-field
C-c C-n    bibtex-next-field             C-c C-e d  bibtex-erase-delimiters
C-c C-e f  bibtex-erase-field            C-c C-e t  bibtex-erase-text
C-c C-t p  bibtex-text-previous-entry 	 C-c C-t n  bibtex-text-next-entry
LFD        bibtex-newline-indent         ESC LFD    bibtex-newline-indent-label
 
                   ------ Abbreviation Mechanism -----
M-x@group   invoke group abbreviation   M-x%group         same as M-x@group
M-x@string  invoke field abbreviation	M-x@abbreviation  same as M-x@string
C-c C-s	    bibtex-save-abbrev		C-c C-l		  bibtex-load-abbrev
C-c C-i g   bibtex-insert-gabbrev  	C-c C-i f         bibtex-insert-fabbrev
C-c C-\\ g  bibtex-make-gabbrev    

	               ----- Cleanup Facility -----
C-c ESC b     bibtex-cleanup-buffer	C-c ESC r    bibtex-cleanup-region
C-c ESC e     bibtex-cleanup-entry

	               ----- Sorting Facility -----
C-c LFD b     bibtex-sort-buffer	C-c LFD r    bibtex-sort-region

	           ----- Draft Making & Debugging -----
C-c C-\\ d    bibtex-make-draft		C-c C-@	     bibtex-goto-error

                     ----- Annotations Facility -----
C-c C-v       bibtex-view-notefiles

	               ----- Mode Operations -----
C-c C-h	     bibtex-mode-help	       unbound      bibtex-mode-version
C-c C-a SPC  bibtex-abbrev-enable      C-c C-a DEL  bibtex-abbrev-disable
C-c LFD SPC  bibtex-autofill-enable    C-c LFD DEL  bibtex-autofill-disable

	              ----- Useful Variables -----
bibtex-abbrev-files	   bibtex-abbrev-fields		bibtex-context
bibtex-entry-use-parens    bibtex-field-use-quotes	bibtex-field-indent
bibtex-extra-fields        bibtex-softcopy		vortex-default-printer
vortex-printer-data	   vortex-default-print-command\n")))
  (goto-char (point-min))
  (set-buffer-modified-p nil)
  (setq buffer-read-only t)
  (other-window 1))


;; ================
;; Entry Invocation
;; ================

(defun bibtex-put-entry (spec)
  "Put out a new entry of TYPE where (TYPE XORF ORF REQF OPTF) is SPEC and
XORF is the list of XOR field labels, nil if none;
ORF is the list of OR field labels, nil if none;
REQF is the list of other required field labels, nil if none;
OPTF is the list of optional field labels, nil if none."
  (let ((type (car spec))
	(xorf (nth 1 spec))
	(orf  (nth 2 spec))
	(reqf (nth 3 spec))
	(optf (nth 4 spec))
	(case-fold-search t))
    (if (or xorf orf reqf optf)
      (let ((ce (bibtex-search-entry t)))	; Searching for the next entry
	(if ce
	  (progn
	    (goto-char (nth 0 ce))
	    (newline))
	  (goto-char (point-max))
	  (delete-blank-lines)
	  (newline))
	(insert type bibtex-entry-open ",\n")
	(let ((pos (- (point) 2))
	      (put-fl '(lambda (fl)
			 (mapcar '(lambda (x)
				    (indent-to bibtex-field-indent)
				    (insert x " = ")
				    (if (vortex-memq x bibtex-abbrev-fields)
				      nil
				      (insert bibtex-field-pair))
				    (insert ",\n"))
				 fl))))
	  (if (or xorf orf reqf)
	    (progn
	      (insert "=============================== REQUIRED FIELDS ===============================\n")))
	  (if xorf
	    (progn
	      (indent-to bibtex-field-indent)
	      (insert "-------------- Exclusive OR fields: specify exactly one --------------\n")
	      (funcall put-fl xorf)))
	  (if orf
	    (progn
	      (indent-to bibtex-field-indent)
	      (insert "-------------- Inclusive OR fields: specify one or both --------------\n")
	      (funcall put-fl orf)))
	  (if (and (or xorf orf) reqf)
	    (progn
	      (indent-to bibtex-field-indent)
	      (insert "------------- Rest of required fields: specify every one -------------\n")))
	  (if reqf
	    (funcall put-fl reqf))
	  (if optf
	    (progn
	      (insert "=============================== OPTIONAL FIELDS ===============================\n")
	      (funcall put-fl optf)))
	  ;; Inserts fields present in all entries (i.e. NOTEFILES)
	  (if bibtex-extra-fields
	      (funcall put-fl bibtex-extra-fields))
	  (previous-line 1)		      ; get rid of trailing comma
	  (end-of-line)
	  (delete-char -1)
	  (next-line 1)
	  (insert bibtex-entry-close ?\n)
	  (goto-char pos)))
      (error "Entry %s doesn't have any field labels.  Abort." type))))

(defun bibtex-invoke-entry (type &optional spec-only)
  "Put out a new bib entry of type TYPE.
If the optional SPEC-ONLY is non-nil, return the specification list only."
  (if spec-only nil (message "Invoking new entry of type %s..." type))
  (let ((spec (bibtex-get-spec type bibtex-entry-types bibtex-entry-specs)))
    (if spec
      (if spec-only
        spec
        (bibtex-put-entry spec)
        (message "Invoking new entry of type %s...done" type))
      (if spec-only
	nil
        (error "Invoking new entry of type %s...abort (undefined entry type)" type)))))

(defun bibtex-get-spec (key nl al)
  (if nl
    (if (equal key (car nl))
      (car al)
      (bibtex-get-spec key (cdr nl) (cdr al)))))

(defun @article (&optional spec-only)
  "Put out a new bib entry of type ARTICLE.
An article from a journal or magazine.
If the optional SPEC-ONLY is non-nil, return the specification list only."
  (interactive)
  (bibtex-invoke-entry "@ARTICLE" spec-only))

(defun @book (&optional spec-only)
  "Put out a new bib entry of type BOOK.
A book with explicit publisher.
If the optional SPEC-ONLY is non-nil, return the specification list only."
  (interactive)
  (bibtex-invoke-entry "@BOOK" spec-only))

(defun @booklet (&optional spec-only)
  "Put out a new bib entry of type BOOKLET.
A work that is printed and bound, but without a named publisher or
sponsoring institution.
If the optional SPEC-ONLY is non-nil, return the specification list only."
  (interactive)
  (bibtex-invoke-entry "@BOOKLET" spec-only))

(defun @conference (&optional spec-only)
  "Put out a new bib entry of type CONFERENCE.
Same as @inproceedings, included for Scribe compatibility.
If the optional SPEC-ONLY is non-nil, return the specification list only."
  (interactive)
  (bibtex-invoke-entry "@CONFERENCE" spec-only))

(defun @inbook (&optional spec-only)
  "Put out a new bib entry of type INBOOK.
A part of a book, which may be a chapter and/or a range of pages.
If the optional SPEC-ONLY is non-nil, return the specification list only."
  (interactive)
  (bibtex-invoke-entry "@INBOOK" spec-only))

(defun @incollection (&optional spec-only)
  "Put out a new bib entry of type INCOLLECTION.
A part of a book having its own title.
If the optional SPEC-ONLY is non-nil, return the specification list only."
  (interactive)
  (bibtex-invoke-entry "@INCOLLECTION" spec-only))

(defun @inproceedings (&optional spec-only)
  "Put out a new bib entry of type INPROCEEDINGS.
An article in a conference proceedings.
If the optional SPEC-ONLY is non-nil, return the specification list only."
  (interactive)
  (bibtex-invoke-entry "@INPROCEEDINGS" spec-only))

(defun @manual (&optional spec-only)
  "Put out a new bib entry of type MANUAL.
Technical documentation.
If the optional SPEC-ONLY is non-nil, return the specification list only."
  (interactive)
  (bibtex-invoke-entry "@MANUAL" spec-only))

(defun @mastersthesis (&optional spec-only)
  "Put out a new bib entry of type MASTERSTHESIS.
A Master's thesis.
If the optional SPEC-ONLY is non-nil, return the specification list only."
  (interactive)
  (bibtex-invoke-entry "@MASTERSTHESIS" spec-only))

(defun @misc (&optional spec-only)
  "Put out a new bib entry of type MISC.
Use this type when nothing else fits.
If the optional SPEC-ONLY is non-nil, return the specification list only."
  (interactive)
  (bibtex-invoke-entry "@MISC" spec-only))

(defun @phdthesis (&optional spec-only)
  "Put out a new bib entry of type PHDTHESIS.
A Ph.D. thesis.
If the optional SPEC-ONLY is non-nil, return the specification list only."
  (interactive)
  (bibtex-invoke-entry "@PHDTHESIS" spec-only))

(defun @proceedings (&optional spec-only)
  "Put out a new bib entry of type PROCEEDINGS.
The proceedings of a conference.
If the optional SPEC-ONLY is non-nil, return the specification list only."
  (interactive)
  (bibtex-invoke-entry "@PROCEEDINGS" spec-only))

(defun @techreport (&optional spec-only)
  "Put out a new bib entry of type TECHREPORT.
A report published by a school or other institution,
usually numbered within a series.
If the optional SPEC-ONLY is non-nil, return the specification list only."
  (interactive)
  (bibtex-invoke-entry "@TECHREPORT" spec-only))

(defun @unpublished (&optional spec-only)
  "Put out a new bib entry of type UNPUBLISHED.
A document having an author and title, but not formally published.
If the optional SPEC-ONLY is non-nil, return the specification list only."
  (interactive)
  (bibtex-invoke-entry "@UNPUBLISHED" spec-only))


;; =============
;; Abbreviations
;; =============

(defun %group (entry)
  "Put out a new group abbreviation of type ENTRY.
A group abbreviation is a collection of fields that serve as a special
type of abbreviation.  Regular (field) abbreviation can be used in
group abbrev but not another group abbrev.
Regular entry and field operations apply to group abbrev."
  (interactive "aGroup abbrev of type: ")
  (message "Invoking new group abbreviation...")
  (let* ((type (upcase (symbol-name entry)))
	 (spec (bibtex-get-spec type bibtex-entry-types bibtex-entry-specs)))
    (if spec
      (progn
        (bibtex-put-entry spec)
	(save-excursion
	  (beginning-of-line)
	  (kill-word 1)
	  (insert "%GROUP"))
	(message "Invoking new group abbreviation...done"))
      (error "Invoking new group abbreviation...abort (undefined entry type %s)" type))))

(defun @group (entry)
  "Alias of %group."
  (interactive "aGroup abbrev of type: ")
  (%group entry))

(defun @abbreviation ()
  "Put out an abbreviation skeleton."
  (interactive)
  (beginning-of-line)
  (open-line 1)
  (insert-string "@STRING{ = \"\"}")
  (search-backward " ="))

(defun @string ()
  "An alias of @abbreviation."
  (interactive)
  (@abbreviation))

;; ====================
;; Supporting Functions
;; ====================

(defun bibtex-search-entry (&optional next)
  "Return attributes of an entry as (ES EE (TS TE TYPE) (LS LE LABEL)).
If NEXT is non-nil, search forward, else backward.  Return nil if fails."
  (let ((direction (if next 're-search-forward 're-search-backward))
	(eol (save-excursion (end-of-line) (point)))
	bor eor)
    (beginning-of-line)
    (re-search-forward bibtex-entry-match eol t)
    (if (funcall direction bibtex-entry-match nil t)
      (let ((es (progn
		  (beginning-of-line)
		  (if (bobp) (point) (1- (point)))))
	    (ee (save-excursion
		  (forward-list 1)
		  (if (eobp) (point) (1+ (point)))))
	    (tp (progn
		  (setq bor (match-beginning 0))
		  (goto-char (match-end 0))
		  (re-search-backward "\\w")
		  (setq eor (1+ (point)))
		  (list bor eor (buffer-substring bor eor))))
	    (lb (progn
		  (re-search-forward "[{(]\\s *\\(\\S *\\)\\s *,")
		  (setq bor (match-beginning 1))
		  (setq eor (match-end 1))
		  (list bor eor (buffer-substring bor eor)))))
	(list es ee tp lb)))))


(defconst bibtex-env "TEXBIB")
(defconst bibtex-path (vortex-mkpath bibtex-env))

