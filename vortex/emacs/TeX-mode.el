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
;; $Header: /home/yew/yew4/vortex/newdist/emacs/RCS/TeX-mode.el,v 1.14 1993/09/16 02:32:14 munson Exp $
;;
;; This file contains TeX-mode.el, a GNU Emacs interface to TeX.
;;
;; Other subsystems (all autoloaded) are
;;     TeX-match.el
;;     TeX-custom.el
;;     TeX-misc.el
;;     TeX-spell.el
;;     TeX-bib.el
;;     TeX-index.el
;;     TeX-cite.el
;;     TeX-insp.el

(provide 'TeX-mode)

;; =================================================
;; Load functions common to TeX-mode and BibTeX-mode
;; =================================================
(require 'VorTeX-common)
(require 'backquote)

;; =========================================================
;; Global variables (values may be changed in tex-mode-hook)
;; =========================================================

;; TeX-spell.el

(defvar tex-speller       "/usr/bin/spell")
(defvar tex-spellin       "/usr/bin/spellin")
(defvar tex-spellout      "/usr/bin/spellout")
(defvar tex-dictionary    "/usr/dict/words")
(defvar tex-hsl-global "/usr/dict/hlista" "Global hashed spelling list.")
(defvar tex-hsl-default nil "User-defined default hashed spelling list.")
(defvar tex-usl-default nil "User-defined default unhashed spelling list.")
(defvar tex-look       "/usr/bin/look")
(defvar tex-egrep      "/usr/bin/egrep")
(defvar tex-detex      "/usr/sww/bin/detex -i")
(defvar tex-delatex    "/usr/sww/bin/delatex")

;; TeX-bib.el

(defvar tex-bib-month-regexp
    (vortex-mkstring nil nil '("jan" "feb" "mar" "apr" "may" "jun"
			       "jul" "aug" "sep" "oct" "nov" "dec"))
  "List of abbreviations for months pre-defined in BibTeX")

;; TeX-cite.el

(defvar tex-cite-case-fold-search default-case-fold-search)
(defvar tex-cite-use-full-query nil)
(defvar tex-cite-query-fields
  '(("ANYFIELD")
    ("AUTHOR" "EDITOR")
    ("TITLE")
    ("JOURNAL" "BOOKTITLE")
    ("INSTITUTION" "ORGANIZATION" "SCHOOL")
    ("YEAR")
    ))

;; TeX-custom.el

(defvar tex-delimiters-auto nil "List of custom-made automatic matching delimiters")
(defvar tex-delimiters-semi nil "List of custom-made semi-automatic matching delimiters")
(defvar tex-latex-envs nil "List of custom-made LaTeX env matching delimiters")

;; TeX-index.el

(defvar tex-index-level-on nil
  "True if index level specification is required.")
(defvar tex-index-actual-on nil
  "True if there needs to be an actual field.")
(defvar tex-index-encap-on nil
  "True if page numbers are to be encapsulated.")
(defvar tex-index-keyptrn-on nil
  "True if \[key, ptrn\] needs to be saved.")
(defvar tex-index-command-prefix "\\index{")
(defvar tex-index-command-suffix "}")
(defvar tex-index-level "!")
(defvar tex-index-actual "@")
(defvar tex-index-encap "|")
(defvar tex-index-processor "makeindex")

;; ===========
;; TeX-insp.el
;; ===========

(defvar tex-insp-use-dvi nil)
(defvar tex-insp-dvi2x-flags "=700x200")

;; TeX-match.el

(defvar tex-boundary-check-on t "Do or not do boundary checking")
(defvar tex-latex-indentation 2 "Default LaTeX environment indentation")

;;;
;;; TeX-misc.el
;;;

;; DOCUMENT-TYPE:
;; 
;; Every file has a document type.  It is determined either by a 
;; "Document Type: " comment on the first line or by querying the user.

;; Local configuration details can be set by altering the formatter 
;; specifications, which are set by the following two lists (one for X and
;; one for sunview).  Each element of these lists contain:
;;   0) Document type in all lower case
;;   1) name of corresponding formatter (executable file name)
;;   2) string to appear in "Document Type:" line of each file (cannot
;;      contain embedded spaces)
;;   3) prompt string to be used when querying the user for document type.
;;      Keep this short so that all of the strings will fit in the
;;      minibuffer.
;;   4) list of response characters with which user can choose this
;;      document type.
;;   5) value for tex-displayer (usually "retex -f <formatter>")
;;      this is not the previewer but rather the routine which automatically
;;      reformats and redisplays the dvi file
;;   6) the spelling filter (detex, olddetex, or delatex)
;;   7) value for tex-preamble (nil for LaTeX derivatives)
;;   8) value for tex-postamble
;;
(defconst tex-formatter-specs-for-x
    (` (("latex" "latex" "LaTeX" "RET/L(aTeX)" (?\r ?l ?L) "retex -f latex"
		 (, tex-delatex) nil "\\end{document}\n")
	("tex" "tex" "TeX" "T(eX)" (?t ?T) "retex"
	       (, tex-detex) "" "\\bye\n")
	("amstex" "amstex" "AmSTeX" "A(msTeX)" (?a ?A) "retex -f amstex"
		  (, tex-detex) "\\document\n" "\\enddocument\n")
	("slitex" "slitex" "SliTeX" "S(liTeX)" (?s ?S) "retex -f slitex"
		  (, tex-delatex) nil "\\end{document}\n")
	)))
(defconst tex-formatter-specs-for-sunview
    (` (("latex" "latex" "LaTeX" "RET/L(aTeX)" (?\r ?l ?L) "latexdvi"
		 (, tex-delatex) nil "\\end{document}\n")
	("tex" "tex" "TeX" "T(eX)" (?t ?T) "texdvi"
	       (, tex-detex) "" "\\bye\n")
	("amstex" "amstex" "AmSTeX" "A(msTeX)" (?a ?A) "amstexdvi"
		  (, tex-detex) "\\document\n" "\\enddocument\n")
	("slitex" "slitex" "SliTeX" "S(liTeX)" (?s ?S) "slitexdvi"
		  (, tex-delatex) nil "\\end{document}\n")
	)))

;; The values stored in TEX-FORMATTER-SPECS are used to determine various
;; formatter dependent information.  There are macros defined below to
;; access the individual fields.

(defvar tex-formatter-specs
  (cond ((eq window-system 'x) tex-formatter-specs-for-x)
	((eq window-system 'sunview) tex-formatter-specs-for-sunview)
	(t tex-formatter-specs-for-sunview))
  "Summary of information that differs between formatters.")

(defvar tex-dvisend "dvisend"
  "Processor which talks to DVI previewer; X only.")
(defvar tex-softcopy
  (cond ((and (eq window-system 'x) (eq window-system-version 11))
	 "dvi2x11")
	((eq window-system 'x) "dvi2x")
	(t "dvitool"))
  "TeX DVI previewer.")

;; ================
;; Global Constants
;; ================

(defconst tex-bib-env "BIBINPUTS")
(defconst tex-bib-path (vortex-mkpath tex-bib-env))
(defconst tex-inputs-env "TEXINPUTS")
(defconst tex-inputs-path (vortex-mkpath tex-inputs-env))
(defconst tex-input "\\\\input\\|\\\\blackandwhite\\|\\\\colorslides")


;; ==================
;; Major mode for TeX
;; ==================

(defvar tex-mode-syntax-table nil "Syntax table in use in TeX mode buffers.")
(defvar tex-mode-abbrev-table nil "Abbrev table in use in TeX mode buffers.")
(defvar tex-mode-customization nil "Flag t when TeX mode is customized.")
(defvar tex-mode-initialization nil "Flag t after tex-mode initialized")

(defun tex-mode (&optional quiet)
  "Major mode for editing TeX documents."
  (interactive)
  (kill-all-local-variables)
  (if (not tex-mode-initialization)
      (tex-mode-init))
  (use-local-map tex-mode-map)
  (setq major-mode 'tex-mode)
  (setq mode-name "TeX")
  ;; User-specific Hook
  (run-hooks 'tex-mode-hook)
  (if (not tex-mode-customization)
      (progn
	(tex-mode-customize)
	(setq tex-mode-customization t)))
  (tex-local-startup)
  ;; Copyright Message
  (if (not tex-mode-initialization)
      (progn
	(tex-mode-copyright)
	(setq tex-mode-initialization t)))
  )
  

(defun tex-mode-init ()
  (tex-mode-keymap-init)
  (define-abbrev-table 'tex-mode-abbrev-table nil)
  (setq tex-mode-syntax-table (make-syntax-table))
  ;; Syntax Entry Modifications
  (modify-syntax-entry ?{ "(}")
  (modify-syntax-entry ?} "){")
  (modify-syntax-entry ?[ "(]")
  (modify-syntax-entry ?] ")[")
  ;; Global Initialization
  (let ((init (vortex-file-exists-p "TeX-init.el" load-path)))
    (if init
	(load "TeX-init")
      (setq init (vortex-file-exists-p "TeX-init.elc" load-path))
      (if init
	  (load "TeX-init"))))
  (if (not vortex-default-printer)
      (let ((env (getenv "PRINTER")))
	(if env
	    (setq vortex-default-printer env)
	  (setq tex-printer-query t))
	))
  )

(defun tex-mode-customize ()
  (if tex-delimiters-auto (tex-mkgrp-auto))
  (if tex-delimiters-semi (tex-mkgrp-semi))
  (if tex-latex-envs (tex-mkgrp-env))
  )


;; =====================
;; TeX-mode Help Message
;; =====================

(defun tex-mode-help ()
  (interactive)
  (pop-to-buffer "--- TeX-mode Help ---")
  (if (= (buffer-size) 0)
    (insert-string
"                            --- Categories ---
Format/Display		  Preview/Print			Debugging Facilities
Spelling Checking	  Bibliography Processing	Indexing
Basic Matching		  Zone Matching			Word Matching  
Automatic Matching	  LaTeX Environments		Customized Matching
Mode & Global Operations  Useful Variables

			   --- Format/Display ---
C-c C-f	d	tex-format-document	C-c C-d	d	tex-display-document
C-c C-f b	tex-format-buffer	C-c C-d b	tex-display-buffer
C-c C-f r	tex-format-region	C-c C-d r	tex-display-region

			   --- Preview/Print ---
C-c C-v SPC	tex-view-all		C-c C-v DEL	tex-view-partial
C-c C-x SPC	tex-x-view-all		C-c C-x DEL	tex-x-view-partial
C-c C-p	SPC	tex-print-all		C-c C-p DEL	tex-print-partial

		        --- Debugging Facilities ---
C-c C-c 	tex-comment-region	C-c C-u		tex-uncomment-region
C-c C-@		tex-goto-error

                         --- Spelling Checking ---
C-c C-s d	tex-spell-document	C-c C-s b	tex-spell-buffer
C-c C-s p	tex-spell-paragraph	C-c C-s r	tex-spell-region
C-c C-s w	tex-spell-word		C-c C-s SPC	tex-spell-complete

		      --- Bibliography Processing ---
C-c C-b c	tex-cite-cite		C-c C-b n	tex-cite-nocite
C-c C-b f       tex-cite-forms-toggle   C-c C-b C       tex-cite-case-toggle
C-c C-b x       tex-cite-clear-file-list
C-c C-b i	tex-insp-inspect        C-c C-b R       tex-insp-reread-file
C-c C-b d	tex-bib-document	C-c C-b b	tex-bib-buffer
C-c C-b SPC	tex-bib-recover		C-c C-b s	tex-bib-save
C-c C-b p       tex-bib-preprocess      C-c C-b u       tex-bib-unify

                             --- Indexing ---
C-c C-i m          tex-index-make	C-c C-i s 	   tex-index-save
\[C-u\] C-c C-i d    tex-index-document	\[C-u\] C-c C-i b    tex-index-buffer
C-c C-i r    tex-index-region		C-c C-i w    tex-index-word
C-c C-i A    tex-index-authors		C-c C-i c    tex-index-chmod
C-c C-i l    tex-index-level-toggle	C-c C-i a    tex-index-actual-toggle
C-c C-i e    tex-index-encap-toggle	C-c C-i k    tex-index-keyptrn-toggle

                           --- Basic Matching ---       
C-c \(		tex-bounce-backward	   C-c \)    tex-bounce-forward
C-c C-t ESC	tex-toggle-boundary-check

       		           --- Zone Matching ---
C-c SPC		tex-zone-open		C-c DEL		tex-zone-close
C-c C-z		tex-zone-inspect
C-c ESC-'	tex-zone-single-quote	C-c ESC-\"	tex-zone-double-quote
C-c ESC-$	tex-zone-math		C-c ESC-d	tex-zone-display-math
C-c ESC-c	tex-zone-centerline	C-c ESC-h	tex-zone-hbox
C-c ESC-v	tex-zone-vbox		C-c ESC-b	tex-zone-bf
C-c ESC-i	tex-zone-it		C-c ESC-r	tex-zone-rm
C-c ESC-s	tex-zone-sl		C-c ESC-t	tex-zone-tt
C-c ESC-e	tex-zone-em
			
 		      --- Word Matching \(Backward\) --- 
C-c '		tex-word-single-quote	C-c \"		tex-word-double-quote
C-c $		tex-word-math		C-c d		tex-word-display-math
C-c c		tex-word-centerline	C-c h		tex-word-hbox
C-c v		tex-word-vbox		C-c b		tex-word-bf
C-c i		tex-word-it		C-c r		tex-word-rm
C-c s		tex-word-sl		C-c t		tex-word-tt
C-c e		tex-word-em

 		      --- Word Matching \(Forward\) --- 
C-c-4 '	 tex-word-forward-single-quote	C-c-4 \" tex-word-forward-double-quote
C-c-4 $  	 tex-word-forward-math	C-c-4 d  tex-word-forward-display-math
C-c-4 c	   tex-word-forward-centerline	C-c-4 h	 	 tex-word-forward-hbox
C-c-4 v		 tex-word-forward-vbox	C-c-4 b		   tex-word-forward-bf
C-c-4 i		   tex-word-forward-it	C-c-4 r		   tex-word-forward-rm
C-c-4 s		   tex-word-forward-sl	C-c-4 t		   tex-word-forward-tt
C-c-4 e		   tex-word-forward-em

  		        --- Automatic Matching  --- 
$		tex-dollar		\"		tex-quote
C-c C-t $	tex-toggle-dollar	C-c C-t \"	tex-toggle-quote

                        --- LaTeX Environments ---
C-c C-l SPC	tex-latex-open		C-c C-l DEL	tex-latex-close
C-c C-l LFD	tex-latex-skip		C-c C-l a	tex-latex-array
C-c C-l c	tex-latex-center	C-c C-l D	tex-latex-document
C-c C-l d	tex-latex-description	C-c C-l e	tex-latex-enumerate
C-c C-l f	tex-latex-figure	C-c C-l i	tex-latex-itemize
C-c C-l l	tex-latex-flushleft	C-c C-l p	tex-latex-picture
C-c C-l Q	tex-latex-quotation	C-c C-l q	tex-latex-quote
C-c C-l r	tex-latex-flushright	C-c C-l TAB	tex-latex-tabbing
C-c C-l T	tex-latex-table		C-c C-l t	tex-latex-tabular
C-c C-l v	tex-latex-verbatim

  		       --- Custimized Matching  --- 
C-c C-\\ a	tex-make-auto		C-c C-\\ s	tex-make-semi
C-c C-\\ e	tex-make-env

	              --- Mode & Global Operations ---
C-c C-h		tex-mode-help		unbound	       tex-mode-version
C-c C-a SPC	tex-abbrev-enable	C-c C-a DEL    tex-abbrev-disable
C-c LFD SPC	tex-autofill-enable	C-c LFD DEL    tex-autofill-disable
C-c C-\\ SPC	tex-make-preamble	C-c C-\\ DEL   tex-make-postamble
C-c 0		tex-set-master-file	C-c 1	       tex-set-document-type
C-c C-e		tex-execute		LFD	       tex-newline-indent

			 --- Useful Variables ---
tex-delatex				tex-detex
tex-delimiters-auto			tex-delimiters-semi
tex-latex-envs				tex-latex-indentation
vortex-default-print-command		tex-softcopy
vortex-default-printer			vortex-printer-data
tex-cite-use-full-query                 tex-cite-case-fold-search
tex-insp-use-dvi"))
  (goto-char (point-min))
  (set-buffer-modified-p nil)
  (setq buffer-read-only t)
  (other-window 1))


;; =========================
;; Buffer-specific variables
;; =========================

(defun tex-local-startup ()
  (setq local-abbrev-table tex-mode-abbrev-table)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start "^ *$\\|^\\|\\\\begin{.*} *$\\|\\\\end{.*} *$\\|^%$")
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'tex-insp-symbolic-point)
  (setq tex-insp-symbolic-point nil)
  (make-local-variable 'tex-insp-actual-point)
  (setq tex-insp-actual-point nil)
  (make-local-variable 'comment-start)
  (setq comment-start "%")
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "[^\\]\\(\\\\\\\\\\)*%+ *")
  (make-local-variable 'tex-zone-marker-stack)
  (setq tex-zone-marker-stack nil)
  (make-local-variable 'tex-zone-marker-count)
  (setq tex-zone-marker-count 0)
  (make-local-variable 'tex-dollar-list)
  (setq tex-dollar-list nil)
  (make-local-variable 'tex-match-dollar-on)
  (setq tex-match-dollar-on t)
  (make-local-variable 'tex-quote-list)
  (setq tex-dollar-list nil)
  (make-local-variable 'tex-match-quote-on)
  (setq tex-match-quote-on t)
  (make-local-variable 'tex-par-start)
  (setq tex-par-start (set-marker (make-marker) 1))
  (make-local-variable 'tex-par-end)
  (setq tex-par-end (set-marker (make-marker) 1))
  )


;; =============================
;; Automatic Matching Delimiters
;; =============================

(defvar tex-symbol-marker nil)
(defvar tex-symbol-marker-pos 0)
(defconst tex-par "^ *$" "Regexp for paragraph separation")
(defconst tex-bslash-sign ?\\)
(defconst tex-dollar-sign ?$)
(defconst tex-dollar-string "\\$")
(defconst tex-dollar-regexp "^\\$\\|[^\\]\\$")
(defconst tex-quote-sign ?\")
(defconst tex-quote-string "\"")
(defconst tex-quote-regexp "^\"\\|[^\\]\"")

(defun tex-bouncing-point (m)
  (save-excursion
    (if (pos-visible-in-window-p)
      (sit-for 1)
      (let* ((pos1 (point))
	     (pos2 (+ pos1 m))
	     (sym (buffer-substring pos1 pos2))
	     (msg1 (progn (beginning-of-line) (buffer-substring (point) pos1)))
	     (msg2 (progn (end-of-line) (buffer-substring pos2 (point)))))
        (message "%s`%s'%s" msg1 sym msg2)))))

(defun tex-locate-delimiter (pos sym symlst)
  (let ((marker nil)
	(marker-pos 0)
	(pair t)
	(head nil))
    (catch 'loop
      (while symlst
	(setq marker (car symlst))
	(setq marker-pos (1- (marker-position marker)))
	(if (and (/= pos marker-pos) (= (char-after marker-pos) sym))
	  (if (> pos marker-pos)
	    (progn
	      (setq tex-symbol-marker-pos marker-pos)
	      (setq tex-symbol-marker marker) 
	      (setq head (cons marker head))
	      (setq pair (not pair)))
	    (if pair (setq tex-symbol-marker nil))
	    (throw 'loop (append (reverse head)
				 (cons (set-marker (make-marker) (1+ pos)) 
				       symlst)))))
	(setq symlst (cdr symlst)))
      (if pair (setq tex-symbol-marker nil))
      (reverse (cons (set-marker (make-marker) (1+ pos)) head)))))

(defun tex-auto-dollar ()
  "Verify if the current paragraph is the same as last.
If so, do nothing, otherwise reset tex-par-begin and tex-par-end and
reconstruct the symbol-list."
  (let ((start (save-excursion
	         (if (re-search-backward tex-par nil t)
		   (point)
		   1)))
        (end (save-excursion
	       (if (re-search-forward tex-par nil t)
		 (1+ (point))
		 (1+ (point-max)))))
	(init nil))
    (if (/= (marker-position tex-par-start) start)
      (progn
	(set-marker tex-par-start start)
	(setq init t)))
    (if (/= (marker-position tex-par-end) end)
      (progn
	(set-marker tex-par-end end)
	(setq init t)))
    (if init
      (save-excursion
	(setq tex-dollar-list nil)
	(goto-char start)
	(while (re-search-forward tex-dollar-regexp end t)
	  (setq tex-dollar-list
	    (append tex-dollar-list
	      (list (set-marker (make-marker)
				(if (= (following-char) tex-dollar-sign)
				  (progn
				    (forward-char 1)
				    (point))
				  (point)))))))))))

(defun tex-dollar ()
  (interactive)
  (if tex-match-dollar-on
    (let ((pc (preceding-char))
	  (pos (point))
	  (pt (point))
	  (single t))
      (tex-auto-dollar)
      (if (= pc tex-bslash-sign)
	(insert tex-dollar-sign)
	(if (= pc tex-dollar-sign)
	  (progn
	    (setq single nil)
	    (if (and (> pos 2) (= (char-after (- pos 2)) tex-dollar-sign))
	      (setq pt (1- pos))	; Doesn't echo 3rd $, if $$ already
	      (backward-char 1) 
	      (insert tex-dollar-sign)
	      (goto-char (1+ pos))))
	  (insert tex-dollar-sign))
	(setq tex-dollar-list (tex-locate-delimiter pt tex-dollar-sign tex-dollar-list))
	(if tex-symbol-marker
	  (save-excursion
	    (goto-char tex-symbol-marker-pos)
	    (if (and (= (preceding-char) tex-dollar-sign)
		     (/= (char-after (- (point) 2)) tex-dollar-sign))
	      (progn
		(backward-char 1)
		(if single
		  (save-excursion
		    (goto-char pos)
		    (insert tex-dollar-sign))))		; $$foo$`$'
	      (if (not single)
		(progn
		  (insert tex-dollar-sign)		; `$'$foo$$
		  (backward-char 1))))
	    (tex-bouncing-point (if single 1 2))))))
    (insert tex-dollar-sign)
    (tex-toggle-dollar t)))

(defun tex-toggle-dollar (&optional quiet)
  (interactive)
  (if (or tex-match-dollar-on quiet)
    (progn
      (setq tex-match-dollar-on nil)
      (setq tex-dollar-list nil)
      (set-marker tex-par-start 1)
      (set-marker tex-par-end 1)
      (define-key tex-mode-map "\$" 'self-insert-command)
      (if quiet nil (message "Automatic \$ matching disabled.")))
    (setq tex-match-dollar-on t)
    (define-key tex-mode-map "\$" 'tex-dollar)
    (message "Automatic \$ matching enabled.")))

(defun tex-auto-delimiter (sym-lst sym-regexp)
  "Verify if the current paragraph is the same as last.
If so, do nothing, otherwise reset tex-par-begin and tex-par-end and
reconstruct the SYM-LST."
  (let ((start (save-excursion
	         (if (re-search-backward tex-par nil t)
		   (point)
		   1)))
        (end (save-excursion
	       (if (re-search-forward tex-par nil t)
		 (1+ (point))
		 (1+ (point-max)))))
	(init nil))
    (if (= (marker-position tex-par-start) start)
      nil
      (set-marker tex-par-start start)
      (setq init t))
    (if (= (marker-position tex-par-end) end)
      nil
      (set-marker tex-par-end end)
      (setq init t))
    (if init
      (save-excursion
	(eval (list 'setq sym-lst nil))
	(goto-char start)
	(while (re-search-forward sym-regexp end t)
	  (eval (list 'setq sym-lst
		  (list 'append sym-lst '(list (set-marker (make-marker) (point)))))))))))
	
(defun tex-delimiter (match-p toggle sym-lst sym-regexp sym-sign)
  (if (eval match-p)
    (let ((pc (preceding-char))
	  (pos (point)))
      (tex-auto-delimiter sym-lst sym-regexp)
      (insert sym-sign)
      (if (= pc tex-bslash-sign)
	nil
	(eval (list 'setq sym-lst (list 'tex-locate-delimiter 'pos sym-sign sym-lst)))
	(if tex-symbol-marker
	  (save-excursion
	    (goto-char tex-symbol-marker-pos)
	    (tex-bouncing-point 1)))))
    (insert sym-sign)
    (funcall toggle t)))

(defun tex-toggle-delimiter (sym-on sym-lst sym-string sym-func &optional quiet)
  (if (or (eval sym-on) quiet)
    (progn
      (eval (list 'setq sym-on nil))
      (eval (list 'setq sym-lst nil))
      (set-marker tex-par-start 1)
      (set-marker tex-par-end 1)
      (define-key tex-mode-map sym-string 'self-insert-command)
      (if quiet nil (message "Automatic %s matching disabled." sym-string)))
    (eval (list 'setq sym-on t))
    (define-key tex-mode-map sym-string sym-func)
    (message "Automatic %s matching enabled." sym-string)))

(defun tex-quote ()
  (interactive)
  (tex-delimiter 'tex-match-quote-on 
		 'tex-toggle-quote 
		 'tex-quote-list 
		 tex-quote-regexp 
		 tex-quote-sign))

(defun tex-toggle-quote (&optional quiet)
  (interactive)
  (tex-toggle-delimiter 'tex-match-quote-on 'tex-quote-list tex-quote-string 'tex-quote quiet))


;; ===============
;; Indent and Fill
;; ===============

(defun tex-newline-indent ()
  (interactive)
  (let ((ind (current-indentation)))
    (newline)
    (indent-to ind)))

(defconst tex-latex-nofill-pattern " *\\begin{.*}$"
  "LaTeX env pattern that does not need to be filled.")

(defun fill-paragraph (arg)
  "Fill paragraph at or after point.
Prefix arg means justify as well.
This function is a modification of the one under the same name in fill.el."
  (interactive "P")
  (save-excursion
    (forward-paragraph)
    (or (bolp) (newline 1))
    (let ((end (point)))
      (backward-paragraph)
      (if (looking-at tex-latex-nofill-pattern)
	(end-of-line))	;; patch this up so that it works with LaTeX envs
      (fill-region-as-paragraph (point) end arg))))

(defun tex-do-auto-fill ()
  "This is a modification of the code under the same name, but without
the tex- prefix, available in simple.el.
The reason to modify the default is for indentations of \item's to
work properly in auto-fill minor mode."
  (let ((fill-point
	 (let ((opoint (point)))
	   (save-excursion
	     (move-to-column (1+ fill-column))
	     (skip-chars-backward "^ \t\n")
	     (if (bolp)
		 (re-search-forward "[ \t]" opoint t))
	     (skip-chars-backward " \t")
	     (point))))
	(ind (current-indentation)))
    ;; If there is a space on the line before fill-point,
    ;; and nonspaces precede it, break the line there.
    (if (save-excursion
	  (goto-char fill-point)
	  (not (bolp)))
	;; If point is at the fill-point, do not `save-excursion'.
	;; Otherwise, if a comment prefix or fill-prefix is inserted,
	;; point will end up before it rather than after it.
	(if (save-excursion
	      (skip-chars-backward " \t")
	      (= (point) fill-point))
	    (progn
	      (indent-new-comment-line)
	      (indent-to ind))
	    (save-excursion
	      (goto-char fill-point)
	      (indent-new-comment-line)
	      (indent-to ind))))))

(defun tex-auto-fill-mode (arg)
  "Toggle auto-fill mode.
With arg, turn auto-fill mode on iff arg is positive.
In auto-fill mode, inserting a space at a column beyond fill-column
automatically breaks the line at a previous space.
This code is a copy of the function available in simple.el."
  (interactive "P")
  (setq auto-fill-hook
	(and
	  (if (null arg) (not auto-fill-hook)
	    (> (prefix-numeric-value arg) 0))
	  'tex-do-auto-fill)))

(defun tex-autofill-enable ()
  (interactive)
  (tex-auto-fill-mode 1)
  (message "Auto-fill minor mode enabled (fill column is %d)." fill-column))

(defun tex-autofill-disable ()
  (interactive)
  (tex-auto-fill-mode 0)
  (message "Auto-fill minor mode disabled."))

(defun tex-abbrev-enable ()
  (interactive)
  (abbrev-mode 1)
  (message "Abbrev minor mode enabled."))

(defun tex-abbrev-disable ()
  (interactive)
  (abbrev-mode 0)
  (message "Abbrev minor mode disabled."))

;;;;;;;;;;;;;;;;;;;;;;
;;;  Key Bindings Data
;;;;;;;;;;;;;;;;;;;;;;

;;
;; Dense Keymaps  (Sparse ones will be created automatically by define-key)
;;

(defvar tex-mode-map nil "Main keymap used in TeX-mode")
(defvar tex-control-c-map nil
  "Secondary keymap used for most TeX-mode operations")
(defvar tex-spell-map nil
  "Tertiary keymap used for TeX-mode spelling commands")
(defvar tex-bib-map nil
  "Tertiary keymap used for TeX-mode bibliographic commands")
(defvar tex-index-map nil
  "Tertiary keymap used for TeX-mode index commands")
(defvar tex-zone-map nil
  "Tertiary keymap used for TeX-mode zone matching commands")
(defvar tex-forward-map nil
  "Tertiary keymap used for TeX-mode forward matching commands")
(defvar tex-latex-map nil
  "Tertiary keymap used for TeX-mode LaTeX environment commands")

(defconst tex-keymap-data
  '((nil
     (tex-mode-map
      ("\C-c" tex-control-c-map)
      ("\t" self-insert-command) ("\n" tex-newline-indent)
      ("\$" tex-dollar) ("\"" tex-quote))
     (tex-control-c-map
      ("\^a " tex-abbrev-enable) ("\^a\177" tex-abbrev-disable)
      ("\n " tex-autofill-enable) ("\n\177" tex-autofill-disable)
      ("\^t\$" tex-toggle-dollar) ("\^t\"" tex-toggle-quote)
      ("\^h"   tex-mode-help)
      ("\C-s" tex-spell-map) ("\C-b" tex-bib-map)
      ("\C-i" tex-index-map) ("\e" tex-zone-map)
      ("4" tex-forward-map) ("\C-l" tex-latex-map)))
    ("TeX-match"
     (tex-control-c-map
      ("(" tex-bounce-backward) (")" tex-bounce-forward)
      ("\^t\e" tex-toggle-boundary-check) (" " tex-zone-open)
      ("\177" tex-zone-close) ("\^z" tex-zone-inspect)
      ("$" tex-word-math) ("d" tex-word-display-math)
      ("'" tex-word-single-quote) ("\"" tex-word-double-quote)
      ("c" tex-word-centerline) ("h" tex-word-hbox)
      ("v" tex-word-vbox) ("b" tex-word-bf)
      ("i" tex-word-it) ("r" tex-word-rm)
      ("s" tex-word-sl) ("t" tex-word-tt)
      ("e" tex-word-em)
      (nil tex-word))
     (tex-zone-map
      ("$" tex-zone-math) ("d" tex-zone-display-math)
      ("'" tex-zone-single-quote) ("\"" tex-zone-double-quote)
      ("c" tex-zone-centerline) ("h" tex-zone-hbox)
      ("v" tex-zone-vbox) ("b" tex-zone-bf)
      ("i" tex-zone-it) ("r" tex-zone-rm)
      ("s" tex-zone-sl) ("t" tex-zone-tt)
      ("e" tex-zone-em))
     (tex-forward-map
      ("$" tex-word-forward-math) ("d" tex-word-forward-display-math)
      ("'" tex-word-forward-single-quote)
      ("\"" tex-word-forward-double-quote)
      ("c" tex-word-forward-centerline) ("h" tex-word-forward-hbox)
      ("v" tex-word-forward-vbox) ("b" tex-word-forward-bf)
      ("i" tex-word-forward-it) ("r" tex-word-forward-rm)
      ("s" tex-word-forward-sl) ("t" tex-word-forward-tt)
      ("e" tex-word-forward-em))
     (tex-latex-map
      (" " tex-latex-open) ("\177" tex-latex-close)
      ("\n" tex-latex-skip) ("a" tex-latex-array)
      ("c" tex-latex-center) ("D" tex-latex-document)
      ("d" tex-latex-description) ("e" tex-latex-enumerate)
      ("f" tex-latex-figure) ("i" tex-latex-itemize)
      ("l" tex-latex-flushleft) ("p" tex-latex-picture)
      ("Q" tex-latex-quotation) ("q" tex-latex-quote)
      ("r" tex-latex-flushright) ("\t" tex-latex-tabbing)
      ("T" tex-latex-table) ("t" tex-latex-tabular)
      ("v" tex-latex-verbatim)))
    ("TeX-custom"
     (tex-control-c-map
      ("\^\\a" tex-make-auto) ("\^\\s" tex-make-semi)
      ("\^\\e" tex-make-env)
      (nil tex-mkgrp-auto) (nil tex-mkgrp-semi)
      (nil tex-mkgrp-env)))
    ("TeX-misc"
     (tex-control-c-map
      ("\^\\ " tex-make-preamble) ("\^\\\177" tex-make-postamble)
      ("\^fd" tex-format-document) ("\^fb" tex-format-buffer)
      ("\^fr" tex-format-region) ("\^dd" tex-display-document)
      ("\^db" tex-display-buffer) ("\^dr" tex-display-region)
      ("\^v " tex-view-all) ("\^v\177" tex-view-partial)
      ("\^p " tex-print-all) ("\^p\177" tex-print-partial)
      ("\^x " tex-x-view-all) ("\^x\177" tex-x-view-partial)
      ("\^e" tex-execute) ("0" tex-set-master-file)
      ("1" tex-set-document-type) ("\^@" tex-goto-error)
      ("\^c" tex-comment-region) ("\^u" tex-uncomment-region)
      (nil tex-shell-init) (nil tex-get-include-files)
      (nil tex-confirm) (nil tex-check-input-arg)
      (nil tex-get-input-files) (nil tex-check-master-file)
      (nil tex-visit-master-file)
      (nil tex-check-document-type)))
    ("TeX-spell"
     (tex-spell-map
      (" " tex-spell-complete) ("w" tex-spell-word)
      ("r" tex-spell-region) ("p" tex-spell-paragraph)
      ("b" tex-spell-buffer) ("d" tex-spell-document)))
    ("TeX-bib"
     (tex-bib-map
      ("d" tex-bib-document) ("b" tex-bib-buffer)
      (" " tex-bib-recover) ("s" tex-bib-save)
      ("p" tex-bib-preprocess) ("u" tex-bib-unify)))
    ("TeX-cite"
     (tex-bib-map
      ("c" tex-cite-cite)  ("n" tex-cite-nocite)
      ("f" tex-cite-forms-toggle) ("C" tex-cite-case-toggle)
      ("x" tex-cite-clear-file-list) (nil tex-cite-lookup)))
    ("TeX-insp"
     (tex-bib-map
      ("i" tex-insp-inspect) ("R" tex-insp-reread-file)))
    ("TeX-index"
     (tex-index-map
      ("m" tex-index-make) ("d" tex-index-document)
      ("b" tex-index-buffer) ("r" tex-index-region)
      ("w" tex-index-word) ("s" tex-index-save)
      ("A" tex-index-authors) ("c" tex-index-chmod)
      ("l" tex-index-level-toggle) ("a" tex-index-actual-toggle)
      ("e" tex-index-encap-toggle)
      ("k" tex-index-keyptrn-toggle)))))

(defun tex-mode-keymap-init ()
  (mapcar 'define-prefix-command
	  '(tex-control-c-map tex-spell-map tex-bib-map tex-index-map
			      tex-zone-map tex-forward-map tex-latex-map
			      tex-mode-map))
  (vortex-keymap-util tex-keymap-data)
  (setq tex-mode-map (symbol-function 'tex-mode-map))
  )

;;;
;;; Access macros for formatter dependent values
;;;

(defmacro tex-formatter-specs (doc-type)
  (` (assoc (downcase (, doc-type)) tex-formatter-specs)))

(defmacro tex-formatter (doc-type)
  (` (nth 1 (tex-formatter-specs (, doc-type)))))

(defmacro tex-doc-type (doc-type)
  (` (nth 2 (tex-formatter-specs (, doc-type)))))

(defmacro tex-doc-type-prompt (doc-type)
  (` (nth 3 (tex-formatter-specs (, doc-type)))))

(defmacro tex-doc-type-response (doc-type)
  (` (nth 4 (tex-formatter-specs (, doc-type)))))

(defmacro tex-displayer (doc-type)
  (` (nth 5 (tex-formatter-specs (, doc-type)))))

(defmacro tex-spell-filter (doc-type)
  (` (nth 6 (tex-formatter-specs (, doc-type)))))

(defmacro tex-preamble (doc-type)
  (` (nth 7 (tex-formatter-specs (, doc-type)))))

(defmacro tex-postamble (doc-type)
  (` (nth 8 (tex-formatter-specs (, doc-type)))))

;; =============
;; Miscellaneous
;; =============

(defun tex-mode-version ()
  (interactive)
  (message "TeX-mode version 1.12 (1/26/92)."))

(defun tex-mode-copyright ()
  (message "%s  C-c C-h for help.  Copyright (C) Regents UC." (tex-mode-version)))
