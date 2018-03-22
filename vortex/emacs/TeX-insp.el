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
;; $Header: /home/yew/yew4/vortex/newdist/emacs/RCS/TeX-insp.el,v 1.14 1993/09/16 02:32:14 munson Exp $
;;
;; This file, "TeX-insp.el", is a GNU Emacs interface to BibTeX databases
;; for plain TeX and LaTeX documents.  It is a subsystem of TeX-mode.
;; It allows users to view the actual references which are referred to
;; by citations in their documents.
;;

(require 'TeX-mode)

(defvar tex-insp-old-master "" "Last document master filename.")
(defvar tex-insp-old-dir "" "Last document directory.")
(defvar tex-bib-fn-list nil "List of buffer names of involving files")
(defvar tex-bib-rec-list nil "List of recovering information")
(defvar tex-insp-page-alist nil)
(defvar tex-insp-base-ext "++")
(defvar tex-insp-context
  (progn
    (if (not (boundp 'bibtex-context))
	(run-hooks 'bibtex-mode-hook))
    (if (not (boundp 'bibtex-context))
	(setq bibtex-abbrev-files ""))
    bibtex-context)
  )

(defvar tex-insp-dvi2x-process nil)
(defconst tex-insp-dvi2x-buffer "TeX-insp-dvi2x-output")
(defvar tex-insp-dvi2x-host nil)
(defvar tex-insp-dvi2x-file nil)
(setq tex-insp-dvi2x-flags
      (concat "-P" tex-insp-dvi2x-flags))

(defun tex-insp-inspect ()
  "Display in the other window the corresponding content of
the closest citation in the preceding text.
Returns an error if no citations are found in the preceding text.
Thus, if tex-insp-inspect is started at the end of buffer, each citation
can be inspected in a reverse order."
  (interactive)
  (let* ((master (tex-check-master-file))
	 (base (substring master 0 (string-match "\.tex" master)))
	 (bbl-file (concat base "\.bbl"))
	 )
    (if (not (file-exists-p bbl-file))
	(error "Biblography file not created yet."))
    (save-excursion
      (find-file-noselect bbl-file))
    ;; assumes that tex-insp-prior-citation calls error if no citation found
    (let* ((prior-citation (tex-insp-prior-citation))
	   (entry-id
	    (if (eq (car prior-citation) 'symbolic)
		(nth 1 prior-citation)
	      (tex-insp-actual-to-symbolic (nth 1 prior-citation) base))))
      (cond ((null entry-id)
	     (error "Apparent citation not found in .bbl file."))
	    (tex-insp-use-dvi
	     (tex-insp-inspect-dvi base entry-id))
	    (t
	     (tex-insp-display-bbl bbl-file entry-id)))
      )))

(defun tex-insp-display-bbl (bbl-file entry-id)
  (find-file-other-window bbl-file)
  (goto-char 1)
  (search-forward entry-id nil t)
  (tex-insp-adjust-window)
  (other-window 1))

(defun tex-insp-actual-to-symbolic (key base)
  (nth 1 (assoc key (tex-insp-get-reclist base))))

(defun tex-insp-adjust-window ()
  (recenter 0)
  (let* ((end (save-excursion (forward-paragraph 1) (point)))
	 (no (+ (count-lines (point) end) 2)))
    (enlarge-window (- no (window-height)))))

(defun tex-insp-get-reclist (ref-file)
  (let ((ref-file (concat base "\.ref"))
	(master-file (tex-check-master-file)))
    (if (and (string-equal tex-insp-old-master master-file)
	     (string-equal tex-insp-old-dir default-directory)
	     tex-bib-fn-list tex-bib-rec-list)
	nil
      (setq tex-insp-old-master master-file)
      (setq tex-insp-old-dir default-directory)
      (if (not (file-exists-p ref-file))
	  (error "Bibliography cross reference file %s doesn't exist."
		 ref-file))
      (save-excursion
	(let ((buf (set-buffer (find-file-noselect ref-file))))
	  (goto-char 1)
	  (setq tex-bib-fn-list (read buf))
	  (setq tex-bib-rec-list (read buf)))))
    (tex-insp-match-lst (buffer-name))))
      
(defun tex-insp-match-lst (fn)
  (let ((fns tex-bib-fn-list)
	(lst tex-bib-rec-list))
    (catch 'found
      (while fns
	(if (string-match fn (car fns))
	    (throw 'found (car lst)))
	(setq fns (cdr fns))
	(setq lst (cdr lst)))
      )))

;;;; =========================
;;;; Citation Search Functions
;;;; =========================

(defconst tex-insp-cite-regexp
  "\\(\\\\\\(no\\)?cite\\)\\|\\(\\[\\)"
  "Matches \cite, \nocite, and [.")

(defconst tex-insp-symbolic-regexp
  "\\\\\\(no\\)?cite\\s *\\(\\[.*\\]\\)?\\s *{\\([^}]*\\)}"
  "Matches any symbolic citation.  Group 3 matches the entry identifiers.")

(defconst tex-insp-actual-regexp "\\[\\(.*\\)\\]"
  "Matches any bracketed text.  Group 1 matches the string between brackets.")

;;; TEX-INSP-PRIOR-CITATION returns a list specifying the citation, 
;;; symbolic or actual, which precedes point.  ORIG-POINT is not part
;;; of the interface.  The returned list has a first element that is
;;; either  'symbolic or 'actual and its second element is the citation
;;; string. 

(defun tex-insp-prior-citation (&optional orig-point)
  (if (not orig-point)
      (setq orig-point (point)))
  (if (re-search-backward tex-insp-cite-regexp nil t)
      (cond
       ((match-beginning 1)
	(tex-insp-check-symbolic (match-beginning 1) orig-point))
       ((match-beginning 3)
	(tex-insp-check-actual (match-beginning 3) orig-point)))
    (goto-char orig-point)
    (error "No citations found in preceding text.")))

;;; TEX-INSP-CHECK-SYMBOLIC determines if the symbolic citation command
;;; found by tex-insp-prior-citation actually contains a citation that
;;; precedes ORIG-POINT.  CITE-BEGIN is the point where the \ was found.
;;; The function returns a list whose first element is 'symbolic and 
;;; second element is the entry id.

(defun tex-insp-check-symbolic (cite-begin orig-point)
  (goto-char cite-begin)
  (tex-insp-check-support orig-point tex-insp-symbolic-regexp 3 'symbolic))

;;; TEX-INSP-CHECK-ACTUAL checks whether the left bracket found by
;;; tex-insp-prior-citation is an actual citation.  If the bracket is
;;; part of a \cite, tex-insp-check-symbolic is called.  Otherwise, if
;;; the citation is actually prior to ORIG-POINT, a list of the form
;;; ('actual citation-string) is returned.

(defun tex-insp-check-actual (cite-begin orig-point)
  (goto-char cite-begin)
  (save-excursion (re-search-backward "\\\\cite\\s *" nil t))
  (if (eq (match-end 0) cite-begin)
      (tex-insp-check-symbolic (match-beginning 0) orig-point)
    (tex-insp-check-support orig-point tex-insp-actual-regexp 1 'actual)))

;;; TEX-INSP-CHECK-SUPPORT is called by the preceding check routines.  It
;;; determines whether the current citation being examined contains any
;;; citations that precede the original point.  If so, it returns a list
;;; containing TYPE and that string.  TYPE is either 'actual or 'symbolic.
;;; Otherwise, it calls tex-insp-prior-citation.  ORIG-POINT is the point
;;; from which the search originally began.  REGEXP is the regular
;;; expression which matches the entire citation string.  MATCH-NUM is the
;;; number of the group in REGEXP which matches the citations without
;;; delimiters.  This function assumes that commas are used to separate
;;; individual citations.

(defun tex-insp-check-support (orig-point regexp match-num type)
  (if (looking-at regexp)
      (if (or (< orig-point (match-beginning match-num))
	      (string-equal "" (vortex-match-string match-num)))
	  (tex-insp-prior-citation orig-point)
	(let ((cite-list
	       (reverse (vortex-parse-comma-list
			 (vortex-match-string match-num))))
	      (cite-end (match-end 0)))
	  (if (catch 'found
		(while cite-list
		  (save-excursion
		    (re-search-forward
		     (concat "[{,[]\\(" (car cite-list) "\\)[],}]")
		     cite-end t))
		  (if (>= orig-point (match-end 1))
		      (throw 'found (car cite-list))
		    (setq cite-list (cdr cite-list)))))
	      (progn
		(goto-char (match-beginning 1))
		(list type (car cite-list)))
	    (tex-insp-prior-citation orig-point))
	  ))
    (tex-insp-prior-citation orig-point)))


;;;; ==============
;;;; DVI Inspection
;;;; ==============

(defun tex-insp-reread-file ()
  "Makes the dvi2x process used for reference inspection return to the
file of references"
  (interactive)
  (start-process "dvisend" nil tex-dvisend tex-insp-dvi2x-host
		 "-m ChangeDocument" tex-insp-dvi2x-file)
  (setq tex-insp-page-alist nil))

(defun tex-insp-make-page-alist (inspbbl)
  (let ((new-alist nil)
	(pageno 1))
    (set-buffer (find-file-noselect inspbbl))
    (beginning-of-buffer)
    (while
	(re-search-forward "\\\\bibitem\\(\\[.*\\]\\)?{\\(.*\\)}" nil t)
      (setq new-alist
	    (cons
	     (list (buffer-substring (match-beginning 2) (match-end 2))
		   (setq pageno (1+ pageno)))
	     new-alist))
      )
    (setq tex-insp-page-alist new-alist)))
    
(defun tex-insp-newbbl (bbl inspbbl)
  (find-file-noselect bbl)
  (set-buffer (find-file-noselect inspbbl))
  (erase-buffer)
  (insert-buffer (get-file-buffer bbl))
  (beginning-of-buffer)
  (replace-string "\\bibitem" "\\pagebreak\n\\bibitem")
  (clear-visited-file-modtime)
  (save-buffer)
  )

(defun tex-insp-newtex (insptex)
  (set-buffer (find-file-noselect insptex))
  (erase-buffer)
  (insert "\\documentstyle{article}\n"
	  "\\oddsidemargin = -.75in\n"
	  "\\topmargin = -1in\n"
	  (if tex-insp-context (concat "\\input " tex-insp-context "\n"))
	  "\\begin{document}\n"
	  "\\bibliography{}\n"
	  "\\end{document}\n")
  (clear-visited-file-modtime)
  (save-buffer)
  )

(defun tex-insp-run-latex (inspbase)
  (message "Reformatting references ...")
  (let ((latex-process (start-process "latex" nil "latex" inspbase)))
    (while (eq (process-status latex-process) 'run)
      (sit-for 1)))
  (message "Reformatting references ... done")
  )

(defun tex-insp-find-dvi2x-port ()
  (set-buffer tex-insp-dvi2x-buffer)
  (goto-char (point-max))
  (re-search-backward "^.*:.*$")
  (setq tex-insp-dvi2x-host (vortex-match-string 0))
  )

(defun tex-insp-run-dvi2x (dvi-file reread)
  (cond ((or (null tex-insp-dvi2x-process)
	     (eq 'exit (process-status tex-insp-dvi2x-process)))
	 (message "Restarting dvi2x ...")
	 (vortex-init-process-buffer tex-insp-dvi2x-buffer)
	 (setq tex-insp-dvi2x-process
	       (start-process "tex-insp-dvi2x" tex-insp-dvi2x-buffer
			      tex-softcopy tex-insp-dvi2x-flags "=700x200" dvi-file))
	 (set-buffer tex-insp-dvi2x-buffer)
	 (while (eq (buffer-size) 0)
	   (message "Place dvi2x window ...")(sit-for 1))
	 (tex-insp-find-dvi2x-port)
	 (message ""))
	((eq (process-status tex-insp-dvi2x-process) 'run)
	 (cond ((not (string-equal tex-insp-dvi2x-file dvi-file))
		(let ((process
		       (start-process
			"dvisend" nil tex-dvisend
			tex-insp-dvi2x-host "-m ChangeDocument" dvi-file)))
		  (while (eq 'run (process-status process))
		    (sit-for 1))))
	       (reread
		(start-process "dvisend" nil tex-dvisend
			       tex-insp-dvi2x-host "-m RereadDocument"))
	       ))
	(t
	 (if (get-buffer tex-insp-dvi2x-buffer)
	     (kill-buffer tex-insp-dvi2x-buffer))
	 (tex-insp-run-dvi2x dvi-file))
	)
  (setq tex-insp-dvi2x-file dvi-file)
  )

(defun tex-insp-inspect-dvi (base entry-id)
  (if (not (eq window-system 'x))
      (error "Must be using X window system to use dvi inspection."))
  (let ((orig-buffer (current-buffer))
	(bbl (concat base ".bbl"))
	(inspbase (concat base tex-insp-base-ext))
	(reread-dvi nil)
	)
    (let ((inspbbl (concat inspbase ".bbl"))
	  (insptex (concat inspbase ".tex"))
	  (inspdvi (concat inspbase ".dvi"))
	  )
      (if (file-newer-than-file-p bbl inspbbl)
	  (progn
	    (tex-insp-newbbl bbl inspbbl)
	    (tex-insp-make-page-alist inspbbl)))
      (if (or (null tex-insp-page-alist)
	      (not (string-equal tex-insp-dvi2x-file inspdvi)))
	  (tex-insp-make-page-alist inspbbl))
      (if (file-newer-than-file-p inspbbl insptex)
	  (tex-insp-newtex insptex))
      (if (file-newer-than-file-p insptex inspdvi)
	  (progn
	    (tex-insp-run-latex inspbase)
	    (setq reread-dvi t)))
      (tex-insp-run-dvi2x inspdvi reread-dvi)
      (let ((page (nth 1 (assoc entry-id tex-insp-page-alist))))
	(start-process "dvisend" tex-insp-dvi2x-buffer tex-dvisend
		       tex-insp-dvi2x-host
		       "-m" "GotoPage" (int-to-string page))
	)
      )))

