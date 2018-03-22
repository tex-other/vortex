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
;; $Header: /home/yew/yew4/vortex/newdist/emacs/RCS/TeX-misc.el,v 1.14 1993/09/16 02:32:14 munson Exp $
;;
;; This file, "TeX-misc.el", is a subsystem of "TeX-mode.el".
;; It contains miscellaneous functions such as document type checking,
;; TeX/LaTeX/SliTeX/AmSTeX execution, error positioning, region commenting,
;; ..., etc.
;;

(require 'TeX-mode)

(defconst tex-ext-regexp "\\.tex")
(defconst tex-dot-regexp "\\.")
(defconst tex-ext ".tex")
(defconst tex-dvi-ext ".dvi")
(defconst tex-pre "+")
(defconst tex-post "-")
(defconst tex-tmp "#")
(defconst tex-partial "%")
(defvar tex-x-machine "localhost:0" "Machine:Device pair")

;;; ==================================
;;; Document type Checking
;;; ==================================

;; DOCUMENT TYPE is an attribute of a buffer/file.  It is recorded in a 
;; comment on the first line of the buffer.  Legal document types are 
;; determined by the values in TEX-FORMATTER-SPECS.  The document type
;; recorded in the comment must match (when downcased) the car of one of
;; the elements of TEX-FORMATTER-SPECS.  The user can directly edit the
;; comment or use TEX-SET-DOCUMENT-TYPE to change the value.

(defun tex-set-document-type ()
  "Sets the document type for the current buffer (i.e. LaTeX, TeX, etc.)."
  (interactive)
  (let* ((current-type (tex-find-doc-type-comment))
	 (new-type (tex-get-doc-type)))
    (if current-type
	(save-excursion
	  (goto-char (match-beginning 1))
	  (delete-region (match-beginning 1) (match-end 1))
	  (insert new-type))
      (save-excursion
	(goto-char 1)
	(insert "% Document Type: " new-type "\n")))
    new-type))

(defun tex-check-document-type ()
  (let ((current-type (tex-find-doc-type-comment)))
    (if (and current-type
	     (equal current-type (tex-verify-doc-type current-type)))
	current-type
      (tex-set-document-type))))

;;; TEX-FIND-DOC-TYPE-COMMENT searches the buffer for a document type comment.
;;; It returns the type as a string.  The location of the string in the
;;; buffer is held in the 1th element of match-data.
(defun tex-find-doc-type-comment ()
  (let ((case-fold-search t))
    (save-excursion
      (goto-char 1)
      (if (re-search-forward "% *Document Type: *\\(\\w+\\)" nil t)
	  (vortex-match-string 1)))))

(defconst tex-doc-type-response-data
    (mapcar '(lambda (form-spec)
	      (list (nth 4 form-spec)
		    (nth 3 form-spec)
		    (nth 2 form-spec)))
	    tex-formatter-specs))

(defun tex-get-doc-type ()	  
  (vortex-user-response "Document Type? " tex-doc-type-response-data))

;;; returns nil if no matching formatter data is found
(defun tex-verify-doc-type (type)
  (nth 2 (assoc (downcase type) tex-formatter-specs)))

;;; ==================================
;;; Master File Checking
;;; ==================================

;; The MASTER FILE is an attribute of a buffer/file.  It is recorded in a
;; comment on the second line of the buffer.  The master file must
;; generally be an existing readable file.  These routines allow
;; violation of this rule, but many TeX-mode routines will break if the
;; violation is not corrected.  The user can directly edit the comment or
;; use TEX-SET-MASTER-FILE to change the value.

(defun tex-set-master-file ()
  (interactive)
  (let* ((current-master (tex-find-master-comment))
	 (new-master (tex-get-master-file current-master)))
    (if current-master
	(save-excursion
	  (goto-char (match-beginning 1))
	  (delete-region (match-beginning 1) (match-end 1))
	  (insert new-master))
      (save-excursion
	(goto-char 1)
	(if (re-search-forward
	     "Document Type: .*" (save-excursion (end-of-line) (point)) t)
	    (forward-line))
	(insert "% Master File: " new-master "\n")))
    new-master))
	
;; TEX-FIND-MASTER-COMMENT searches the buffer for a master file comment.
;; It returns the file as a string.  The location of the string in the
;; buffer is held in the 1th element of match-data.
(defun tex-find-master-comment ()
  (let ((case-fold-search t))
    (save-excursion
      (goto-char 1)
      (if (re-search-forward "%\\s *Master File:\\s *\\(\\S +\\)\\s " nil t)
	  (vortex-match-string 1)))))

(defun tex-check-master-file ()
  "Check the master file comment and return the name of the master file."
  (let ((current-master (tex-find-master-comment)))
    (if (and current-master (tex-verify-master-file current-master))
	current-master
      (tex-set-master-file))))

(defun tex-visit-master-file ()
  "Check and visit the document master file in the other window."
  (let ((master (tex-check-master-file)))
    (if (not (string-equal master (file-name-nondirectory (buffer-file-name))))
	(find-file-other-window master))
    master
    ))
  
(defun tex-get-master-file (current-master)
  (let ((temp-mf
	 (or current-master (file-name-nondirectory (buffer-file-name))))
	(return-mf nil))
    (while (not return-mf)
      (setq return-mf
	    (tex-verify-master-file (read-string "Master File: " temp-mf))))
    return-mf))

(defun tex-verify-master-file (filename)
  (cond ((file-exists-p filename) filename)
	((y-or-n-p (concat "Master File: " filename
			   " [Not found].  Use it anyway? "))
	 filename)
	(t nil)))

;; ==========================
;; TeX/LaTeX/SliTeX execution
;; ==========================

(defun tex-make-global (start end sign amble)
  (let* ((fn (file-name-nondirectory (buffer-file-name)))
	 (gbl (concat (substring fn 0 (string-match tex-dot-regexp fn)) sign tex-ext)))
    (message "Making document %s to \"%s\"..." amble gbl)
    (copy-to-buffer gbl start end)
    (kill-region start end)
    (insert "\n\\input " gbl ?\n)
    (save-excursion
      (set-buffer gbl)
      (write-file gbl))
    (message "Making document %s to \"%s\"...done" amble gbl)))

(defun tex-make-preamble (start end)
  (interactive "r")
  (tex-make-global start end tex-pre "preamble"))
  
(defun tex-make-postamble (start end)
  (interactive "r")
  (tex-make-global start end tex-post "postamble"))
  
(defun tex-shell-init (fn)
  (let ((proc (get-buffer-process "*shell*"))
	(dir (substring (or (file-name-directory fn) default-directory) 0 -1))
	cdir)
    (if (and proc (eq (process-status proc) 'run))
	(save-excursion
	  (set-buffer "*shell*")
	  (goto-char (point-max))
					;;	(stop-shell-subjob)
					;;	(insert-string "kill %")
	  (if (save-excursion
		(beginning-of-line)
		(looking-at "^? *$"))
	      (progn
		(insert ?x)
		(shell-send-input)
		(sleep-for 1)))
	  (kill-shell-input)
	  (insert-string "pwd")
	  (shell-send-input)
	  (sleep-for 1)
	  (save-excursion
	    (if (search-backward "/" nil t)
		nil
	      (previous-line 1))
	    (beginning-of-line)
	    (setq cdir (buffer-substring (point) (progn (end-of-line) (point)))))
	  (if (string-equal dir cdir)
	      nil
	    (insert-string "cd " dir)
	    (shell-send-input)
	    (sleep-for 1)))
      (save-excursion (shell))
      (define-key shell-mode-map "\^c\^@" 'tex-goto-error)
      (set-buffer "*shell*")
      (message "Waiting for shell prompt...")
      (while (= (buffer-size) 0) (sleep-for 1))
      (message "Waiting for shell prompt...done"))))
  
(defun tex-execute (cmd fn &optional suffix &optional ow)
  "Run CMD with argument FN in the inferior shell process.
If SUFFIX is non-nil, it is to be attached to the end of \"CMD FN\".
If called interactively, prompt for CMD and FN.
If OW is non-nil, position the cursor to the original window."
  (interactive "sCommand (including switches): \nfFilename: ")
;  (if (and (not suffix) 
;	   (buffer-modified-p)
;	   (y-or-n-p "Confirm saving current buffer? "))
;    (write-file fn))
  (save-some-buffers t)
  (let ((file (file-name-nondirectory fn)))
    (if suffix
	(setq cmd (concat cmd
			  " "
			  (substring file 0 (string-match tex-dot-regexp file))
			  (or suffix "")))
      (setq cmd (concat cmd " " fn)))
    (message "Sending `%s' to shell..." cmd)
    (tex-shell-init fn)
    (pop-to-buffer "*shell*")
    (goto-char (point-max))
    (recenter 0)
    (insert cmd)
    (setq tex-error-pos (point))
    (setq tex-error-start-pos tex-error-pos)
    (shell-send-input)
    (message "")			; Get rid of last message
    (if ow (other-window 1))))

(defun tex-execute-tmp (start end pgm doc-type &optional reg)
  (let* ((master-file (tex-check-master-file))
	 (fnnd (file-name-nondirectory (buffer-file-name)))
	 (fbase (substring fnnd 0 (string-match tex-dot-regexp fnnd)))
	 (tmp (concat fbase tex-tmp tex-ext))
	 (region (if reg
		     (if (< start end)
			 (buffer-substring start end) 
		       "")
		   (concat "\\input " fbase "\n"))))
    (if (or reg (not (string-equal fnnd master-file)))
	(let* ((mbase (substring master-file 
				 0 
				 (string-match "\\." master-file)))
	       (pre (concat mbase tex-pre tex-ext))
	       (post (concat mbase tex-post tex-ext)))
	  (delete-other-windows)
	  (split-window-vertically nil)
	  (split-window-horizontally nil)
	  (other-window 1)
	  (find-file tmp)
	  (tex-mode t)
	  (erase-buffer)
	  (insert "% Document Type: " doc-type
		  "\n% Master File: " tmp
		  "\n% Temporary file originated from: " fnnd "\n")
	  ;; if preamble file exists, use it
	  ;; if TeX or AmSTeX, else must be LaTeX or SliTeX
	  (cond ((file-exists-p pre) (insert "\\input " pre))
		((tex-preamble doc-type)
		 (insert (tex-preamble doc-type)))
		(t (insert (tex-get-preamble master-file fbase doc-type))))
	  ;; SliTeX disables body
	  (if (not (string-equal (downcase doc-type) "slitex"))
	      (insert "\n" region))
	  (if (bolp) (newline) (newline 2))
	  (if (file-exists-p post)
	      (insert "\\input " post ?\n)
	    (insert (tex-postamble doc-type)))
	  (if (and (re-search-forward "\\\\cite\\|\\\\nocite" nil t)
		   (y-or-n-p "Preprocess symbolic citations? "))
	      (progn
		(tex-bib-buffer)
		(tex-bib-save t))
	    (write-file tmp))
	  (sit-for 0)
	  (tex-execute pgm tmp))
      (tex-execute pgm master-file))))

(defun tex-get-preamble (master base doc-type)
  (save-excursion
    (find-file master)
    (goto-char 1)
    (if (re-search-forward "\\\\begin{document}" nil t)
	(cond
	  ((string-equal (downcase doc-type) "latex")
	   (concat (buffer-substring 1 (match-end 0)) "\n"))
	  ((string-equal (downcase doc-type) "slitex")
	   (concat (buffer-substring 1 (match-end 0)) "\n"
		   "\\blackandwhite{" base "}\n")))
      (error "Can't find preamble in %s document rooted at \"%s\"...abort"
	     doc-type master))))

(defun tex-format-document ()
  (interactive)
  (let ((doc-type (tex-check-document-type)))
    (tex-execute (tex-formatter doc-type) (tex-visit-master-file))))

(defun tex-format-buffer ()
  (interactive)
  (let ((doc-type (tex-check-document-type)))
    (tex-execute-tmp 1 (point-max) (tex-formatter doc-type) doc-type)))

(defun tex-format-region (start end)
  (interactive "r")
  (let ((doc-type (tex-check-document-type)))
    (tex-execute-tmp start end (tex-formatter doc-type) doc-type t)))

(defun tex-display-document (&optional machine)
  (interactive
   (if (and current-prefix-arg
	    (string-equal window-system "x"))
       (list (setq tex-x-machine
		   (read-string "Hostname:Device = " tex-x-machine)))
     (list "")))
  (let ((doc-type (tex-check-document-type)))
    (tex-execute (concat (tex-displayer doc-type) " " machine)
		 (tex-visit-master-file))))

(defun tex-display-buffer (&optional machine)
  (interactive (if (and current-prefix-arg
			(string-equal window-system "x"))
		   (list (setq tex-x-machine
			       (read-string "Hostname:Device = " tex-x-machine)))
		 (list "")))
  (let ((doc-type (tex-check-document-type)))
    (tex-execute-tmp 1 (point-max)
		     (concat (tex-displayer doc-type) " " machine)
		     doc-type t)))

(defun tex-display-region (start end &optional machine)
  (interactive "r")
  (if (and current-prefix-arg
	   (string-equal window-system "x"))
      (progn
	(setq tex-x-machine
	      (read-string "Hostname:Device = " tex-x-machine))
	(setq machine tex-x-mahcine))
    (setq machine ""))
  (let ((doc-type (tex-check-document-type)))
    (tex-execute-tmp start end
		     (concat (tex-displayer doc-type) " " machine)
		     doc-type t)))

(defun tex-view-dvi (&optional pages &optional machine)
  "Invoke a DVI previewer or send a reread command to an existing one
depending on MACHINE being non-nil.  The string bound to MACHINE will
be the HOSTNAME:DEVICE attribute under which the previewer is running.
If PAGES is non-nil, preview partial DVI file by invoking a DVI extractor
before previewing.  In this case, the new DVI file will be foo%.dvi
instead of foo.dvi.  Queries will be issued if DVI file is older or missing."
  (let* ((master-file (tex-visit-master-file))
	 (fnnd (file-name-nondirectory master-file))
	 (fnb (substring fnnd 0 (string-match tex-dot-regexp fnnd)))
	 (dvi (concat fnb tex-dvi-ext))
	 (tmp (concat fnb tex-partial tex-dvi-ext))
	 proc arg1 arg2)
    (cond
      ((and pages machine)		;; partial under dvisend
       (setq proc (concat vortex-extractor " " pages " " dvi " " tmp "; "
			  tex-dvisend " " machine " -c " tmp 
			  " &"))
       (setq arg1 "")
       (setq arg2 nil))
      (pages				;; partial in general
       (setq proc (concat vortex-extractor " " pages " " dvi " " tmp "; "
			  tex-softcopy " " tmp
			  " &"))
       (setq arg1 "")
       (setq arg2 nil))
      (machine				;; all under dvisend
       (setq proc (concat tex-dvisend " " machine " -c " dvi 
			  " &"))
       (setq arg1 "")
       (setq arg2 nil))
      (t				;; all in general
       (setq proc tex-softcopy)
       (setq arg1 fnb)
       (setq arg2 (concat tex-dvi-ext "&"))))
    (if (file-exists-p dvi)
	(if (file-newer-than-file-p dvi master-file)
	    (tex-execute proc arg1 arg2 t)
	  (if (y-or-n-p (concat "File " dvi " is older than the source, preview anyway? "))
	      (tex-execute proc arg1 arg2 t)
	    (if (y-or-n-p (concat "File " dvi " is older than the source, reformat then preview? "))
		(tex-display-document)
	      (ding)
	      (message "File %s not previewed." dvi))))
      (if (y-or-n-p (concat "File " dvi " not found, format the source then preview? "))
	  (tex-display-document)
	(ding)
	(message "File %s not found and therefore not previewed." dvi)))))

(defun tex-view-all ()
  "Preview the DVI file associated with the current master file
by invoking a DVI previewer.
This is for use with the DVI previewer bound to tex-softcopy."
  (interactive)
  (tex-view-dvi))

(defun tex-view-partial (pages)
  "Select and preview certain pages from the DVI file associated with
the current master file by invoking a DVI previewer.
This is for use with the DVI previewer bound to tex-softcopy.
PAGES must be given as one or more page ranges."
  (interactive "sSelect DVI pages: ")
  (tex-view-dvi pages))

(defun tex-x-view-all (&optional machine)
  "Preview the DVI file associated with the current master file
by passing a message to an existing DVI previewer.
This is for use with dvi2x and dvisend by Steven Procter under X.
With prefix argument, the HOSTNAME:DEVICE attribute will be queried.
Without prefix argument, tex-x-machine is assumed
whose default is localhost:0."
  (interactive (if current-prefix-arg
		   (list (setq tex-x-machine
			       (read-string "Hostname:Device = " tex-x-machine)))
		 (list tex-x-machine)))
  (if (and (eq window-system 'x)
	   (string-match "dvi2x" tex-softcopy)
	   (string-equal tex-dvisend "dvisend"))
      (tex-view-dvi nil machine)
    (ding)
    (message "Wrong window system or wrong DVI previewer.")))

(defun tex-x-view-partial (pages &optional machine)
  "Select and preview certain pages from the DVI file associated with
the current master file by passing a message to an existing DVI previewer.
This is for use with dvi2x and dvisend by Steven Procter under X.
PAGES must be given as one or more page ranges.
With prefix argument, the HOSTNAME:DEVICE attribute will be queried.
Without prefix argument, tex-x-machine is assumed
whose default is localhost:0."
  (interactive (list (read-string "Select DVI pages: ")
		     (if current-prefix-arg
			 (setq tex-x-machine
			       (read-string "Machine:Device = " tex-x-machine))
		       tex-x-machine)))
  (if (and (eq window-system 'x)
	   (string-match "dvi2x" tex-softcopy)
	   (string-equal tex-dvisend "dvisend"))
      (tex-view-dvi pages machine)
    (ding)
    (message "Wrong window system or wrong DVI previewer.")))

(defun tex-print-all (&optional prefix-arg)
  (interactive "P")
  (let* ((command-args
	  (if prefix-arg (read-string "Printer command args: ")))
	 (master-file (tex-visit-master-file))
	 (printer
	  (if vortex-printer-query
	      (read-string (concat "Which printer? " vortex-printer-list ": ")
			   vortex-default-printer)
	    vortex-default-printer))
	 (printer-data (assoc printer vortex-printer-data))
	 (print-command
	  (concat (if printer-data
		      (cdr printer-data)
		    (concat vortex-default-print-command printer))
		  " " command-args))
	 )
    (tex-execute print-command master-file
		 (concat tex-dvi-ext
			 (if vortex-printer-queue
			     (concat "; " vortex-printer-queue printer) ""))
		 t)))

(defun tex-print-partial (prefix-arg pages)
  (interactive "P\nsSelect DVI pages: ")
  (let* ((command-args
	  (if prefix-arg (read-string "Printer command args: ")))
	 (printer
	  (if vortex-printer-query
	      (read-string (concat "Which printer? " vortex-printer-list ": ")
			   vortex-default-printer)
	    vortex-default-printer))
	 (printer-data (assoc printer vortex-printer-data))
	 (print-command
	  (concat (if printer-data
		      (cdr printer-data)
		    (concat vortex-default-print-command printer))
		  " " command-args))
	 (fnnd (file-name-nondirectory (buffer-file-name)))
	 (fnb (substring fnnd 0 (string-match tex-dot-regexp fnnd)))
	 (dvi (concat fnb tex-dvi-ext))
	 (tmp (concat fnb tex-partial tex-dvi-ext)))
    (tex-execute (concat vortex-extractor " " pages " " dvi " " tmp "; "
			 print-command
			 " " tmp
			 (if vortex-printer-queue
			     (concat "; " vortex-printer-queue printer)
			   ""))
		 "" 
		 nil 
		 t)))


;; =========
;; Debugging
;; =========

(defvar tex-error-pos nil "Position of last TeX error in *shell*")

(defun tex-goto-error ()
  "Position point to next TeX/AmSTeX/LaTeX/BibTeX error."
  (interactive)
  (if (and (boundp 'tex-bib-to-bibtex) tex-bib-to-bibtex)
      (bibtex-goto-error)
    (let* ((ori (buffer-file-name))
	   (win2 (/ (window-height) 2))
	   (doc-type (tex-check-document-type))
	   (msg (concat "Positioning to next " (or doc-type "TeX") " error..."))
	   match line string fn)
      (set-buffer "*shell*")
      (define-key shell-mode-map "\^c\^@" 'tex-goto-error)
      (if (> (buffer-size) 0)
	  (progn
	    (pop-to-buffer "*shell*")
	    (goto-char (point-max))
	    (if (save-excursion
		  (if tex-error-pos
		      (goto-char tex-error-pos)
		    (if (re-search-backward "^This is TeX" nil t)
			(setq tex-error-pos (point))
		      (error "Don't know how to locate next error...abort")))
		  (re-search-forward	; TeX or LaTeX error messages
		   "l\\.\\([1-9][0-9]*\\) [\.]*\\(.*\\)$\\|line \\([1-9][0-9]*\\) [\.]*\\(.*\\)$" nil t))
		(let (pos)
		  (message msg)
		  (goto-char (1+ (match-beginning 0)))
		  (setq match (if (= (following-char) ?.) 1 3))
		  (setq line
			(string-to-int (buffer-substring (match-beginning match)
							 (match-end match))))
		  (setq match (1+ match))
		  (setq string 
			(buffer-substring (match-beginning match)
					  (setq pos (match-end match))))
		  (re-search-backward "^!" nil t)
		  (recenter 0)
		  (setq fn (tex-get-error-filename))
		  (setq tex-error-pos pos)
		  (goto-char (point-max))
		  (find-file-other-window fn)
		  (goto-line line)
		  (search-forward string (save-excursion (end-of-line) (point)) t)
		  (recenter win2)
		  (message "%sdone" msg))
	      (message "%sno more" msg)))
	(message " %snot found" msg)))))

(defvar tex-error-start-pos 1)

(defun tex-scan-parens (&optional bound)
  (let ((n 0))
    (save-excursion
      (catch 'found
	(while t
	  (if (re-search-forward "(\\|)" bound t)
	      (if (= (preceding-char) ?\))
		  (progn
		    (setq n (1- n))
		    (cond ((= n 0) (throw 'found (1- (point))))
			  ((< n 0) (throw 'found nil))))
		(setq n (1+ n)))
	    (throw 'found nil)))))))

(defun tex-get-error-filename ()
  (let ((pos (point))
	fn)
    (save-excursion
      (catch 'done
	(while t
	  (if (re-search-backward "(\\([^)^ ^\n][^)^ ^\n]*\\)"
				  tex-error-start-pos t)
	      (progn
		(setq fn (vortex-match-string 1))
		(if (file-exists-p fn)
		    (if (tex-scan-parens pos)
			nil
		      (throw 'done fn))))
	    (error "Can't find any files name...(tex-get-error-filename)")))))))
	  
(defconst tex-comment-string "%")
(defconst tex-comment-char ?%)

(defun tex-comment-region (start end &optional n)
  "Comment out all lines in region between START and END by inserting
a TeX comment sign in front of each line.  With positive prefix argument N,
insert that many %'s.  Otherwise, insert just one."
  (interactive "r\nP")
  (message "Commenting region...")
  (let* ((n (if n (max n 1) 1))
	 (comment-string (make-string n tex-comment-char)))
    (save-excursion
      (goto-char end)
      (beginning-of-line)
      (while (> (point) start)
	(forward-line -1)
	(insert comment-string)
	(beginning-of-line))))
  (message "Commenting region...done"))

(defun tex-uncomment-region (start end &optional n)
  "Uncomment all lines in region between START and END by deleting
the leading % from each line, if any.  With prefix argument N being positive,
delete that many %'s, if any."
  (interactive "r\nP")
  (message "Uncommenting region...")
  (let* ((n (if n (max n 1) 1))
	 (comment-string (make-string n tex-comment-char)))
    (save-excursion
      (goto-char start)
      (while (< (point) end)
	(beginning-of-line)
	(if (looking-at (concat " *" comment-string))
	    (progn
	      (zap-to-char 1 tex-comment-char); Delete leading blanks
	      (delete-char n)))		; Delete the symbol
	(next-line 1))))
  (message "Uncommenting region...done"))


;; =============
;; Include Files 
;; =============

(defvar tex-include-files nil "List of files included.")

(defun tex-get-include-files (&optional quiet)
  (let ((doc-type (tex-check-document-type)))
    (if (not quiet)
	(message "Looking for associated %s files..." doc-type))
    (save-excursion
      (let (only incl lst)
	(if (string-equal doc-type
			  (nth 2 (assoc "tex" tex-formatter-specs)))
	    (setq tex-include-files (list (buffer-file-name)))
	  (setq only (tex-includeonly doc-type quiet))
	  (setq incl (tex-include doc-type quiet))
	  (setq tex-include-files
		(cons (buffer-file-name)
		      (if only
			  (tex-latex-include only incl)
			(mapcar '(lambda (x) 
				  (if (and x (string-match "\\." x))
				      x
				    (concat x tex-ext)))
				incl)))))))
    (if (not quiet)
	(message "Looking for associated %s files...done" doc-type))))

(defun tex-includeonly (doc-type quiet)
  (if (not quiet)  
    (message "Looking for associated %s files (\\includeonly)..." doc-type))
  (goto-char 1)
  (if (search-forward
       "\\includeonly" 
       (save-excursion
	 (re-search-forward "\\\\begin\\|\\\\input\\|\\\\include\\W" nil t)
	 (point))
       t)
    (if (re-search-backward
	 "^%\\|[^\\]%" (save-excursion (beginning-of-line) (1- (point))) t)
	nil                                    ; Do nothing, commented out
      (let* ((lst nil)
	     (pos (progn (re-search-forward "\\w") (1- (point))))
	     (bound (save-excursion (up-list 1) (1+ (point))))
	     fn)
	(while (and (< pos bound) (re-search-forward ",\\|}" bound t))
	  (setq fn (buffer-substring pos (1- (point))))
	  (setq lst (cons (substring fn 0 (string-match tex-ext-regexp fn)) lst))
	  (re-search-forward "\\w")
	  (setq pos (1- (point))))
	(if (not quiet)
	  (message "Looking for associated %s files (\\includeonly)...done"
		   doc-type))
	(reverse lst)))))

(defun tex-include (doc-type quiet)
  (goto-char 1)
  (let ((lst nil)
	fn pos)
    (if (not quiet)
      (message "Looking for associated %s files (\\include)..." doc-type))
    (while (re-search-forward "\\\\include *{ *\\w\\|\\\\bibliography *{ *\\w" nil t)
      (if (save-excursion    				; Commented out?
	    (re-search-backward "^%\\|[^\\]%" 
				(save-excursion 
				  (beginning-of-line) 
				  (1- (point))) 
				t))
	nil
	(if (save-excursion
	      (skip-chars-backward "^\\")
	      (looking-at "include"))
	  (progn
	    (setq fn (buffer-substring (1- (point))
				       (progn
					 (re-search-forward " \\|}\\|,")
					 (1- (point)))))
	    (setq fn (substring fn 0 (string-match tex-ext-regexp fn))))
	  (setq fn (file-name-nondirectory (buffer-file-name)))
	  (setq fn (substring fn 0 (string-match "\\." fn)))
	  (setq fn (concat fn ".bbl")))
	(setq lst (cons fn lst))))
    (if (not quiet)
      (message "Looking for associated %s files (\\include)...done" doc-type))
    (reverse lst)))

(defun tex-latex-include (only inc)
  (if only
    (let ((fn (car only)))
      (if (vortex-memq fn inc)
	(cons (concat fn tex-ext) (tex-latex-include (cdr only) inc))
	(tex-latex-include (cdr only) inc)))))


;; ===========
;; Input Files
;; ===========

(defun tex-check-input-arg (pos msg)
  (switch-to-buffer (current-buffer))
  (if (y-or-n-p (concat msg ", ignore? "))
    nil
    (message "Entering recursive edit, use ESC C-c to return...")
    (goto-char pos)
    (save-excursion (recursive-edit))
    (goto-char (- (point) 7))
    nil))

(defun tex-get-input-files (msg fnnd)
  (goto-char 1)
  (save-excursion
    (let ((lst nil) bslash fn)
      (if (save-excursion (re-search-forward tex-input nil t))
	(progn
	  (while (re-search-forward tex-input nil t)
	    (setq bslash (match-beginning 0))
	    (if (save-excursion
		  (re-search-backward "^%\\|[^\\]%" 	; Commented out?
				      (save-excursion 
					(beginning-of-line) 
					(1- (point))) 
				      t))
	      nil
	      (if (setq fn (tex-scan-input))
		(setq lst (cons fn lst))
		(message "%s, doing \"%s\"..." msg fnnd))))
	  (reverse lst))))))

(defun tex-scan-input ()
  (let ((eol (save-excursion (end-of-line) (point)))
	eoi pos char arg fn)
    (setq eoi (point))
    (re-search-forward "[^ \t{]")
    (setq pos (1- (point)))
    (re-search-forward "\\.\\| \\|\t\\|}\\|$")
    (setq char (preceding-char))
    (if (> (point) eol)
      (tex-check-input-arg eoi "\\input format error")
      (setq arg
	(if (= char ?\.)
	  (progn
	    (re-search-forward " \\|^t\\|}\\|$")
	    (setq char (preceding-char))
	    (buffer-substring 
	      pos
	      (if (or (= char ? ) (= char ?\t) (= char ?}))
		(1- (point))
		(point))))
	  (concat (buffer-substring
		    pos 
		    (if (or (= char ? ) (= char ?\t) (= char ?})) 
		      (1- (point)) 
		      (point)))
		  ".tex")))
      (if (and arg (setq fn (vortex-file-exists-p arg tex-inputs-path))) 
	fn))))
;;	(find-file fn)
;;	(tex-check-input-arg eoi (concat "File \"" arg "\" nonexistent"))))))


(defun tex-confirm (pmt &optional spc &optional del &optional ret &optional esc)
  "Request for confirmation on PMT.
SPC or `y' means yes, DEL or `n' means no, RET or `a' sets the flag ASKNOT
and will not ask any more, ESC or `d' means done (perhaps prematurely), and
LFD or `r' enters recursive edit (ESC C-c returns to current state).
The caller must have the variable `asknot' bound in a let expression and
the symbol `finish' bound in a catch clause whose body is a while statement."
  (let* ((spc (if spc spc ?y))
	 (del (if del del ?n))
	 (ret (if ret ret ?a))
	 (esc (if esc esc ?d))
	 (pmt (concat pmt
		      "?  ["
		      "SPC/" (char-to-string spc) "  "
		      "DEL/" (char-to-string del) "  "
		      "RET/" (char-to-string ret) "  "
		      "ESC/" (char-to-string esc) "  "
		      "C-r]")))
    (catch 'right
      (while t
	(message pmt)
	(setq ans (read-char))
	(cond ((or (= ans ? ) (= ans spc))
	       (throw 'right t))
	      ((or (= ans ?\177) (= ans del))
	       (throw 'right nil))
	      ((or (= ans ?\r) (= ans ret))
	       (setq asknot t)
	       (throw 'right t))
	      ((or (= ans ?\e) (= ans esc))
	       (if (y-or-n-p "Done (perhaps prematurely), are you sure? ")
	         (throw 'finish nil)))
	      ((= ans ?\^r)
	       (message "Entering recursive edit...(return to current state ESC C-c)")
	       (save-excursion
	         (recursive-edit)))
	      (t (ding) 
		 (message "%s...WHAT?" pmt)
		 (sit-for 1)))))))
  
