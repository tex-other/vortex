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
;; $Header: /home/yew/yew4/vortex/newdist/emacs/RCS/BibTeX-notes.el,v 1.14 1993/09/16 02:32:14 munson Exp $
;;
;; This file, BibTeX-notes.el, is an experimental subcomponent of GNU
;; Emacs BibTeX-mode.  It contains functions that assist in the viewing
;; and maintenance of annotations to BibTeX reference entries.
;;

(require 'BibTeX-mode)

(defconst bibtex-notes-env "BTXMODENOTEFILES")
(defconst bibtex-notes-path (vortex-mkpath bibtex-notes-env))

(defconst bibtex-notes-prompt-data
  '(((?y ? ) "SPC/y" yes)
    ((?n ?\177) "DEL/n" no)
    ((?a ?\n) "RET/a" asknot)
    ((?d ?\e) "ESC/d" done)
    ((?\^r) "C-r" recurse)
    ))

(defun bibtex-view-notefiles (&optional n)
  "Displays the contents of files listed in the `NOTEFILES' field of the
current entry.  If N is nil, then the user is prompted with the name of 
each readable file in the field.  He can choose to display or skip the file,
enter recursive edit, enter `asknot' sub-mode, or return to editing the
BibTeX window.  `Asknot' sub-mode displays and enters a recursive editing 
session for each readable file remaining in the list."
  (interactive "P")
  (let ((case-fold-search t))
    (if n
	(let ((file (nth (1- n) (bibtex-notefiles-list))))
	  (if (not (vortex-file-readable-p file bibtex-notes-path))
	      (error "\"%s\" not readable" file)
	    (save-excursion 
	      (save-window-excursion
		(if (get-buffer file)
		    (display-buffer file t)
		  (find-file-other-window file))
		(message "Entering recursive edit...(return to current state ESC C-c)")
		(recursive-edit)))))			    
      (let* ((file-list (bibtex-notefiles-list))
	     (files file-list)
	     (cur-file nil)
	     (bib-window (get-buffer-window (current-buffer)))
	     (notes-window nil)
	     (response nil))
	(save-excursion
	  (save-window-excursion
	    (catch 'finish 
	      (while t
		(setq cur-file
		      (vortex-file-readable-p (car files) bibtex-notes-path))
		(cond
		 ((not cur-file)
		  (message "Could not find readable copy of \"%s\" ... (strike any key to continue)" (car files))
		  (read-char)
		  (if (null (setq files (cdr files))) (setq files file-list))
		  (setq cur-file (car files)))
		 (cur-file
		  (setq response (vortex-user-response
				  (concat "Display " cur-file "? ")
				  bibtex-notes-prompt-data))
		  (cond 
		   ((eq response 'yes) 
		    (setq notes-window (bibtex-display-notefile 
					cur-file bib-window notes-window))
		    (if (null (setq files (cdr files))) (setq files file-list))
		    (setq cur-file (car files)))
		   ((eq response 'no)
		    (if (null (setq files (cdr files))) (setq files file-list))
		    (setq cur-file (car files)))
		   ((eq response 'asknot)
		    (bibtex-display-edit-notefiles files bib-window notes-window)
		    (setq files file-list))
		   ((and (eq response 'recurse) notes-window)
		    (select-window notes-window)
		    (message "Entering recursive edit...(return to current state ESC C-c)")
		    (save-excursion (save-window-excursion (recursive-edit)))
		    (select-window bib-window))
		   ((and (eq response 'recurse) (not notes-window))
		    (message "Entering recursive edit...(return to current state ESC C-c)")
		    (save-excursion (save-window-excursion (recursive-edit))))
		   ((eq response 'done)
		    (message "")	;clear minibuffer
		    (throw 'finish t))
		   ))))
	      ))))
      )))

(defun bibtex-notefiles-list ()
  "Returns a list of file names found in the NOTEFILES field of the
current entry"
  (save-excursion
    (let ((current-entry (bibtex-search-entry nil)))
      (goto-char (car current-entry))
      (if (re-search-forward "[\t\n ,]NOTEFILES\\s *="
			     (nth 1 current-entry) nil)
	  (mapcar 'bibtex-remove-spaces
		  (vortex-parse-comma-list
		   (nth 2 (nth 3 (bibtex-current-field-data))))))
      )))

(defun bibtex-remove-spaces (string)
  "Returns STRING with leading and trailing white space removed."
  (let* ((start (string-match "[^ \t\n]" string))
	 (end (string-match "[ \t\n]" string start))
	 (temp (string-match "[^ \t\n]" string end)))
    (while (and end temp)
      (setq end (string-match "[ \t\n]" string temp))
      (setq temp (string-match "[^ \t\n]" string end)))
    (substring string start end)
    ))

(defun bibtex-readable-files (file-list)
  "Returns a list containing all files in FILE-LIST which are readable.
The current directory and all directories on BIBTEX-PATH (global) are
checked while trying to find the file."
  (if (null file-list) nil
    (let ((first-file (vortex-file-readable-p (car file-list) bibtex-path)))
      (if first-file
	  (cons first-file (bibtex-readable-files (cdr file-list)))
	(bibtex-readable-files (cdr file-list))))
    ))

(defun bibtex-display-notefile (file bib-window notes-window)
  "Displays FILE in NOTES-WINDOW, then moves cursor to BIB-WINDOW.
If NOTES-WINDOW is nil, FILE is displays in some other window (which
is created, if necessary). Returns notes-window."
  (cond (notes-window
	  (select-window notes-window)
	  (if (get-buffer file)
	      (switch-to-buffer file)
	    (find-file file))
	  (select-window bib-window))
	(t
	  (find-file-other-window file)
	  (setq notes-window (get-buffer-window (current-buffer)))
	  (select-window bib-window)))
  notes-window)

(defun bibtex-display-edit-notefiles (files bib-window notes-window)
  "Displays each file in the list FILES.  Once a file has been displayed
a recursive editing session is begun.  The user switches to the next file
by exiting the recursive editing session. (This approach to moving from
file to file is taken to avoid having to support another prompt/reply
function)."
  (while files
    (setq notes-window 
	  (bibtex-display-notefile (car files) bib-window notes-window))
    (save-excursion (save-window-excursion 
		      (select-window notes-window)
		      (message "ESC C-c will display the next file.")
		      (recursive-edit)))
    (setq files (cdr files))))

