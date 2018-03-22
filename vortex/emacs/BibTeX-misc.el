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
;; $Header: /home/yew/yew4/vortex/newdist/emacs/RCS/BibTeX-misc.el,v 1.14 1993/09/16 02:32:14 munson Exp $
;;
;; This file, bibtex-misc.el, is a subsystem of BibTeX-mode.
;; It contains facilities for cleaning up .bib files, as well as
;; It gets autoloaded whenever a function defined in this file is invoked.
;;

(autoload 'sort-subr "sort")
(require 'BibTeX-mode)

;; =======
;; Cleanup
;; =======

(defun bibtex-cleanup-buffer ()
  (interactive)
  (let ((case-fold-search t))
    (bibtex-cleanup
     1 (set-marker (make-marker) (point-max)) "current buffer")))

(defun bibtex-cleanup-region (start end)
  (interactive "r")
  (let ((start (save-excursion
		 (goto-char start)
		 (car (or (bibtex-current-entry t) (bibtex-current-field-data)))))
	(bound (set-marker (make-marker)
			   (save-excursion
			     (goto-char end)
			     (nth 1 (or (bibtex-current-entry t)
					(bibtex-current-field-data))))))
	(case-fold-search t))
    (bibtex-cleanup start bound "region")))

(defun bibtex-cleanup-entry (&optional n)
  (interactive "P")
  (let ((start (car (bibtex-previous-entry (if n n 0) t)))
	(bound (set-marker (make-marker) (nth 1 (bibtex-current-entry t))))
	(case-fold-search))
    (bibtex-cleanup start bound "entries")))

(defun bibtex-cleanup (start bound msg)
  "Clean up the bib file or part of it."
  (message "Cleaning up %s..." msg)
  (save-excursion
    (bibtex-correct-empty-fields start bound)
    (bibtex-delete-banners start bound)
    (bibtex-delete-trailing-commas start bound)
    (bibtex-abbrev-to-top start bound)
    (bibtex-insert-title))
  (message "Cleaning up %s...done" msg))

(defun bibtex-abbrev-to-top (start bound &optional quiet)
  (let ((end (marker-position bound))
        (base (save-excursion
		(goto-char 1)
	        (if (re-search-forward bibtex-entry-no-group-match nil t)
		      (progn
			(beginning-of-line)
			(1- (point)))
		      (point-max))))
	abbrev boa eoa astring)
    (if quiet nil (message "Pulling all abbreviations to top..."))
    (goto-char start)
    (while (re-search-forward bibtex-abbrev-match-fg end t)
      (if (= (char-after (match-beginning 0)) ?@)
        (setq abbrev (bibtex-current-field))
        (setq abbrev (bibtex-current-entry)))
      (setq boa (car abbrev))
      (if (> boa base)	; Pulling it up to the beginning
	(save-excursion
	  (setq eoa (nth 1 abbrev))
	  (setq astring (buffer-substring boa eoa))
	  (delete-region boa eoa)
	  (goto-char base)
	  (insert astring)
	  (setq base (point)))))
    (if quiet nil (message "Pulling all abbreviations to top...done"))))

(defun bibtex-delete-banners (start bound &optional quiet)
  "Delete the banner lines in all newly invoked entries.
If the optional QUIET is non-nil, do this quietly.
If START and BOUND are non-nil, do this within that region."
  (if quiet nil (message "Deleting banners in newly invoked entries..."))
  (let ((end (marker-position bound)))
    (goto-char start)
    (while (search-forward "=========" end t)
      (beginning-of-line)
      (kill-line 1))
    (while (search-backward "---------" start t)
      (beginning-of-line)
      (kill-line 1))
    (if quiet nil (message "Deleting banners in newly invoked entries...done"))))
	   
(defun bibtex-delete-trailing-commas (start bound &optional quiet)
  "Delete the trailing comma of every the entry.
If the optional QUIET is non-nil, does this quietly.
If START and BOUND are non-nil, does this within that region."
  (if quiet nil (message "Deleting trailing comma of every entry..."))
  (let ((end (marker-position bound)))
    (goto-char start)
    (while (re-search-forward bibtex-entry-no-group-match end t)
      (re-search-backward "[({]")     ; backup to left of {
      (let ((begin (point)))
	(forward-list 1)
	(let ((wpos (save-excursion (re-search-backward "\\w" begin t) (point))))
	  (if (re-search-backward "," begin t)
	    (if (< wpos (point)) (delete-char 1))))))
    (if quiet nil (message "Deleting trailing comma of every entry...done"))))
    
(defun bibtex-correct-empty-fields (start bound &optional quiet)
  "Check and correct empty fields within region START..BOUND."
  (let ((end (marker-position bound)))
    (goto-char start)
    (if quiet nil (message "Correcting empty fields..."))
    (while (re-search-forward bibtex-field-empty end t)
      (if quiet nil (message "Correcting empty fields..."))
      (let* ((pos (save-excursion (beginning-of-line) (point)))
	     (cf (bibtex-current-field))
	     (tp (nth 2 cf))
	     (tb (car tp))
	     (type (nth 2 tp)))
	(cond
	  ((< (point) pos)            			; Unknown field
	   (goto-char pos)
	   (kill-line 1))
	  ((save-excursion                                ; abbrev label
	     (goto-char tb)
	     (looking-at bibtex-abbrev-empty))
	   (bibtex-previous-field)
	   (bibtex-required-text "Required abbreviation label: "))
	  ((string-equal type bibtex-abbrev)                 ; abbrev text
	   (bibtex-required-text "Required abbreviation text: "))
	  ((save-excursion                                ; entry label
	     (goto-char tb)
	     (looking-at bibtex-entry-match))
	   (bibtex-required-text (concat type " -- required entry label: ")))
	  (t                                              ; regular field
	   (let* ((ce (bibtex-current-entry t))
		  (eb (car ce))
		  (ee (nth 1 ce))
		  (et (nth 2 (nth 2 ce)))
		  (gabbrev (string-equal et "%GROUP")) 
		  (spec (if (not gabbrev)
			  (bibtex-get-spec et bibtex-entry-types bibtex-entry-specs)))
		  (fl nil))
	     (if (or spec gabbrev)
	       (cond
		 ((or gabbrev (vortex-memq type (nth 4 spec)) ; optional field
		      (vortex-memq type bibtex-extra-fields))
		  (beginning-of-line)
		  (kill-line 1))
		 ((vortex-memq type (nth 3 spec))             ; required field
		  (bibtex-required-text (concat type " -- required field text: ")))
		 ((vortex-memq type (setq fl (nth 1 spec)))       ; xor field
		  (bibtex-xor-text type fl eb ee))
		 ((vortex-memq type (setq fl (nth 2 spec)))       ; or field
		  (bibtex-or-text type fl eb ee)))
	       (if (y-or-n-p                                   ; unknown entry
		     (concat et " -- unknown entry type, kill it? "))
		   (bibtex-kill-current-entry t)
		   (message "Entry not killed.  Proceeding..."))))))))
    (if quiet nil (message "Correcting empty fields...done"))))
		
(defun bibtex-required-text (msg)
  (let ((text (read-string msg)))
    (if (string-equal text "")
      (progn
	(ding)
	(message (concat msg "Can't accept null string.  Try again..."))
	(sit-for 1)
	(bibtex-required-text msg))
      (insert-string text)
      (message "Text inserted.")
      (sit-for 0))))
		
(defun bibtex-optional-text (msg)
  (let ((text (read-string msg)))
    (if (string-equal text "")
      (if (y-or-n-p "Killing current field, are you sure? ")
	(progn
	  (beginning-of-line)
	  (kill-line 1))
	(bibtex-optional-text msg))
      (insert-string text)
      (message "Text inserted.")
      (sit-for 0))))

(defun bibtex-xor-text (type xorf begin end)
  (let ((xors (vortex-mkstring "" " *=" xorf))
	(filled (concat
		  (vortex-mkstring "" " *= *[\"{]\\w.*[\"}]" xorf)
		  (vortex-mkstring "" " *= *\\w.*$" xorf))))
    (if (save-excursion
	  (or (re-search-backward filled begin t)
	      (re-search-forward filled end t)))
      (progn                               ; Other field has been filled
	(beginning-of-line)
	(kill-line 1))
      (if (save-excursion
	    (re-search-forward xors end t))    ; There are unfilled fields
	(bibtex-optional-text (concat type " -- first XOR field to be filled: (RET if not) "))
	(bibtex-required-text (concat type " -- the only XOR field remaining: "))))))
    
(defun bibtex-or-text (type orf begin end)
  (let ((ors (vortex-mkstring "" " *=" orf)))
    (if (or (save-excursion
	      (beginning-of-line)
	      (re-search-backward ors begin t))
	    (save-excursion (re-search-forward ors end t)))
      (bibtex-optional-text (concat type " -- OR field to be filled: (RET if not) "))
      (bibtex-required-text (concat type " -- the only OR field remaining: ")))))

(defun bibtex-insert-title ()
  (goto-char (point-min))
  (let ((cfn (buffer-file-name))
	(time (current-time-string)))
    (if (re-search-forward "File.*last modified on" nil t)
      (progn
	(beginning-of-line)
	(kill-line 1)))
    (insert-string "File " cfn " last modified on " time ".\n")))


;; ============
;; Draft Making
;; ============

(defun bibtex-make-draft ()
  "Creates, previews, and prints a draft version of the .bib file."
  (interactive)
  (let* ((=cbn= (buffer-name))
	 (=cfn= (buffer-file-name))
	 (=pre0= (substring =cbn= 0 (string-match "\\.bib" =cbn=)))
	 (=pre= (concat =pre0= "+"))
	 (=aux= (concat =pre= ".aux"))
	 (=bbl= (concat =pre= ".bbl"))
	 (=tex= (concat =pre= ".tex"))
	 (=dvi= (concat =pre= ".dvi"))
	 (=blg= (concat =pre= ".blg"))
	 (=log= (concat =pre= ".log"))
	 (case-fold-search t))
    (message "Making a draft bibliography...")
    (if (buffer-modified-p)
      (progn
	(if (y-or-n-p "Confirm cleanup? ")
	  (bibtex-cleanup-buffer))
	(if (y-or-n-p (concat "Confirm writing \"" =cbn= "\"? "))
	  (write-file =cfn=))))
    (get-buffer-create =aux=)
    (bibtex-mkaux)
    (bibtex-bibtex)
    (get-buffer-create =tex=)
    (bibtex-mktex)
    (bibtex-tex)
    (if (y-or-n-p "Confirm previewing the draft? ") (bibtex-get-softcopy))
    (if (y-or-n-p "Confirm printing the draft? ") (bibtex-get-hardcopy))
    (save-excursion
      (set-buffer "*shell*")
      (goto-char (point-max))
      (if (y-or-n-p "Confirm saving the draft? ")
	(progn
	  (insert "\\rm " =aux= "* " =bbl= "* " =blg= " " =log=)
	  (shell-send-input)
	  (message "Making a draft bibliography...done (draft saved in %s & %s)" =tex= =dvi=))
        (insert "\\rm " =pre= "\\.*")
        (shell-send-input)
        (message "Making a draft bibliography...done (temporary files deleted)")))))

(defun bibtex-shell-init (fn)
  (let ((proc (get-buffer-process "*shell*"))
	(dir (substring (or (file-name-directory fn) default-directory) 0 -1))
	cdir)
    (if (and proc (eq (process-status proc) 'run))
      (save-excursion
	(set-buffer "*shell*")
	(goto-char (point-max))
	(stop-shell-subjob)
	(sleep-for 1)
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
      (define-key shell-mode-map "\^c\^@" 'bibtex-goto-error)
      (set-buffer "*shell*")
      (message "Waiting for shell prompt...")
      (while (= (buffer-size) 0) (sleep-for 2))
      (message "Waiting for shell prompt...done"))))

(defun bibtex-shell (first cmd &optional msgs)
  (if (file-exists-p =cfn=)
    (save-window-excursion
      (if first 
	(progn
	  (message "Sending `%s' to shell..." cmd)
	  (bibtex-shell-init =cfn=)))
      (pop-to-buffer "*shell*")
      (goto-char (point-max))
      (recenter 0)
      (insert cmd)
      (setq bibtex-error-pos (point))
      (shell-send-input)
      (if msgs
	(let ((abort nil))
	  (if (y-or-n-p (concat msgs "[Wait till finish if `y'] "))
	    nil
	    (stop-shell-subjob)
	    (insert-string "kill %")
	    (shell-send-input)
	    (sleep-for 1)
	    (insert-string "\\rm " =pre= "*")
	    (shell-send-input)
	    (setq abort t))
	  (if (and first
		   (save-excursion
		      (goto-char bibtex-error-pos)
		      (re-search-forward "^ --line\\|^ : Warning:\\|---line" nil t)))
	    (progn
	      (setq bibtex-bibs nil)
	      (if abort
		(error "Making a draft bibliography...abort (use C-c C-@ to locate BibTeX errors)")
		(if (y-or-n-p "BibTeX errors found, are you sure you want to continue? ")
		  nil
		  (error "Making a draft bibliography...abort (use C-c C-@ to locate BibTeX errors)"))))
	    (if abort
	      (error "Making a draft bibliography...abort (temporary files deleted)")))
	  (message "Making a draft bibliography...continuing"))
	(message "Type any key to continue...")
	(read-char)))
    (error "Making a draft bibliography...abort (nonexistent \"%s\")" =cfn=)))
 
(defun bibtex-bibtex ()
  (bibtex-shell t (concat "bibtex " =pre=) "Continue formatting the draft? "))

(defun bibtex-tex ()
  (bibtex-shell nil (concat "tex " =pre=) "Preview, print, or save the draft? "))

(defun bibtex-get-softcopy ()
  (bibtex-shell nil (concat bibtex-softcopy " " =dvi= "&")))

(defun bibtex-get-hardcopy ()
  (let* ((printer
	  (if vortex-printer-query
	      (read-string (concat "Which printer? " vortex-printer-list ": ") 
			   vortex-default-printer)
	    vortex-default-printer))
	 (printer-data (assoc printer vortex-printer-data))
	 (print-command (if printer-data
			    (cdr printer-data)
			  (concat vortex-default-print-command printer)))
	 )
      (bibtex-shell nil (concat print-command " " =dvi= "; "
				vortex-printer-queue printer))
      ))

(defun bibtex-mkaux ()
  "Creates .aux file to be BibTeXed."
  (save-excursion
    (set-buffer =aux=)
    (erase-buffer))
  (goto-char 1)
  (message "Creating \"%s\" (for BibTeX)..." =aux=)
  (save-window-excursion
    (let ((label ""))
      (while (re-search-forward bibtex-entry-no-group-match nil t)
	(re-search-forward "\\(\\w.*\\), *$" nil t)
	(setq label (vortex-match-string 1))
        (set-buffer =aux=)
	(insert-string "\\citation{" label "}\n")
        (set-buffer =cbn=)))
    (goto-char 1)                     ; Still in bib
    (set-buffer =aux=)
    (goto-char 1)
    (if (save-excursion (re-search-forward "\\\\citation{\\w.*}" nil t))
      (insert-string "\\relax\n")
      (error "%s: empty field or wrong format...abort." =aux=)))  ; abort here
  (save-window-excursion
    (let ((style (read-string "Style (RET=plain, 1=unsrt, 2=alpha, 3=abbrv, else=your-own-style): ")))
      (cond
	((string-equal style "") (setq style "plain"))
	((string-equal style "1") (setq style "unsrt"))
	((string-equal style "2") (setq style "alpha"))
	((string-equal style "3") (setq style "abbrv")))
      (message "Creating \"%s\" (for BibTeX)..." =aux=)
      (set-buffer =aux=)
      (goto-char (point-max))
      (insert "\\bibdata{" 
	      (bibtex-get-fn-string bibtex-abbrev-files)
	      =pre0= "}\n\\bibstyle{" style "}\n")
      (write-region 1 (point-max) =aux= nil 'no-message)))
  (message "Creating \"%s\" (for BibTeX)...done" =aux=))

(defun bibtex-get-fn-string (lst)
  (let ((string "") fn)
    (while lst
      (setq fn (car lst))
      (setq string (concat string (bibtex-get-base fn "\\.bib") ","))
      (setq lst (cdr lst)))
    string))

(defun bibtex-get-base (fn suffix)
  (substring fn 0 (string-match suffix fn)))

(defun bibtex-mktex ()
  "Creates a .tex file as the bibliography."
  (if (file-exists-p =bbl=)
    nil
    (error "%s: file doesn't exist...abort" =bbl=))       ; abort here
  (message "Making \"%s\" as your bibliography..." =tex=)
  (save-window-excursion
    (set-buffer =tex=)
    (erase-buffer)
    (insert-file-contents =bbl=)
    (goto-char (point-max))
    (search-backward "\\end")
    (previous-line 1)
    (end-of-line)
    (delete-region (point) (point-max))
    (goto-char (point-min))
    (if (search-forward "\\bibitem" nil t)
      nil
      (error "%s: bad format...sborted." =bbl=))            ; abort here
    (beginning-of-line)
    (delete-region (point-min) (point))
    (if (re-search-forward "\\\\bibitem\\[\\w*\\]" nil t)
      (progn                                 ; Alpha style
	(beginning-of-line)
	(while (search-forward "\\bibitem" nil t)
	  (search-forward "[")
	  (replace-match "{")
	  (search-forward "]")
	  (replace-match "}")
	  (search-forward "}")
	  (insert-string "{")
	  (if (search-forward "\\bibitem" nil t)
	    (previous-line 1)
	    (goto-char (point-max)))
	  (re-search-backward "\\w")
	  (end-of-line)
	  (insert-string "}"))
	(goto-char (point-min)))
      (beginning-of-line)                    ; Other styles
      (let ((n 0))
	(while (search-forward "\\bibitem" nil t)
	  (insert-string "{" (setq n (+ n 1)) "}")
	  (search-forward "}")
	  (insert-string "{")
	  (if (search-forward "\\bibitem" nil t)
	    (previous-line 1)
	    (goto-char (point-max)))
	  (re-search-backward "\\w")
	  (end-of-line)
	  (insert-string "}"))))
    (goto-char (point-max))
    (insert-string "\\bye\n")
    (goto-char 1)
    (if bibtex-context (insert "\\input " bibtex-context ?\n))
    (insert-string "
\\let\\em\\it
\\def\\newblock{\\hskip .11em plus .33em minus -.07em}
\\def\\bibitem#1#2#3{
  {\\bigskip  \\advance\\leftskip by 1in
   \\item{\\hbox to 1.25in{\\hss\$\\lbrace#2\\rbrace\$}
   \\quad\\hbox to .6in{\\hss[#1]}}
   #3\\par}}
\\def\\etalchar#1{$^{#1}$}
\\font\\big=cmbx10 scaled\\magstep3
\\nopagenumbers
\\footline={{\\bf Time: }{\\sl " (current-time-string) "}\\hfil
           {\\bf File: }``{\\tt " =cfn= "}''\\hfil
           {\\bf Page: } \\folio}
\\centerline{\\big Draft Bibliography}\n\\vskip .15truein\n\n")
    (if bibtex-preview-header
      (insert bibtex-preview-header))
    (write-region 1 (point-max) =tex= nil 'no-message))
  (message "Making \"%s\" as your bibliography...done" =tex=))


;; =================
;; Error Positioning
;; =================

(defvar bibtex-bibs nil "List of \.bib files")

(defun bibtex-goto-error ()
  "Positioning to next BibTeX error/warning."
  (interactive)
  (let (key bor eor pos
	(fn nil)
	(case-fold-search t))
    (define-key shell-mode-map "\^c\^@" 'bibtex-goto-error)
    (if (save-excursion
	  (set-buffer "*shell*")
	  (bibtex-get-bibs)
	  (goto-char bibtex-error-pos)
	  (re-search-forward   				; BibTeX errors
	    "^ --line\\|^ : Warning:\\|---line" nil t))
      (progn
	(message "Positioning to next BibTeX error/warning...")
	(pop-to-buffer "*shell*")
	(goto-char (match-beginning 0))			; Position to match
	(beginning-of-line)
	(cond 
	  ((looking-at " --line") 
	   (previous-line 1)
	   (beginning-of-line)
	   (setq pos (point))
	   (re-search-forward "[1-9][0-9]*" nil t)
	   (setq bor (match-beginning 0))
	   (setq eor (match-end 0))
	   (setq key (string-to-int (buffer-substring bor eor)))
	   (re-search-forward "file \\([^ ]+\.bib\\)" 
			      (save-excursion (end-of-line) (point))
			      t)
	   (setq fn (vortex-file-exists-p 
		      (vortex-match-string 1)
		      bibtex-path)))
	  ((looking-at " : Warning:")
	   (beginning-of-line)
	   (setq pos (point))
	   (re-search-forward "in \\(.*\\)$" nil t)
	   (setq bor (match-beginning 1))
	   (setq eor (match-end 1))
	   (setq key (buffer-substring bor eor)))
	  (t					; ---line
	   (beginning-of-line)
	   (setq pos (point))
	   (re-search-forward "[1-9][0-9]*" nil t)
	   (setq bor (match-beginning 0))
	   (setq eor (match-end 0))
	   (setq key (string-to-int (buffer-substring bor eor)))
	   (re-search-forward "file \\([^ ]+\.bib\\)" 
			      (save-excursion (end-of-line) (point))
			      t)
	   (setq fn (vortex-file-exists-p 
		      (vortex-match-string 1)
		      bibtex-path))))
	(setq bibtex-error-pos (point))
	(goto-char pos)
	(recenter 0)
	(if fn
	  (progn
	    (find-file-other-window fn)
	    (goto-line key))
	  (bibtex-find-key key bibtex-bibs)
	  (search-backward key nil t))
	(recenter (/ (window-height) 2))
	(message "Positioning to next BibTeX error/warning...done"))
      (let ((win (get-buffer-window "*shell*")))
	(if win (delete-window win))
	(if (and (boundp 'tex-bib-to-bibtex-p) tex-bib-to-bibtex-p)
	  (exit-recursive-edit)))
      (message "Positioning to next BibTeX error/warning...not found"))))

(defun bibtex-get-bibs ()
  (if bibtex-bibs
    nil
    (save-excursion
      (goto-char bibtex-error-pos)
      (while (re-search-forward "^\\([^ ]*\.bib\\) is" nil t)
	(setq bibtex-bibs (cons (vortex-file-exists-p 
			          (vortex-match-string 1)
			          bibtex-path)
			  bibtex-bibs)))))
  bibtex-bibs)

(defun bibtex-find-key (key lst)
  (catch 'found
    (while lst
      (set-buffer (find-file-noselect (car lst)))
      (goto-char 1)
      (if (re-search-forward (concat "[({] *" key " *,") nil t)
	(throw 'found (find-file-other-window (car lst)))
	(setq lst (cdr lst))))
    (error "Pattern `%s' not found in any \.bib files..." key)))
    

;; ===============
;; Sorting entries
;; ===============

(defun bibtex-sort-buffer ()
  (interactive)
  (let ((case-fold-search t))
    (bibtex-sort 1 (set-marker (make-marker) (point-max)) "current buffer")))

(defun bibtex-sort-region (start end)
  (interactive "r")
  (let ((start (save-excursion
		 (goto-char start)
		 (car (or (bibtex-current-entry t) (bibtex-current-field-data)))))
	(bound (set-marker (make-marker)
			   (save-excursion
			     (goto-char end)
			     (nth 1 (or (bibtex-current-entry t)
					(bibtex-current-field-data))))))
	(case-fold-search t))
    (bibtex-sort start bound "region")))

(defun bibtex-sort (start bound msg)
  "Sort the bib file or part of it."
  (message "Sorting entries in %s..." msg)
  (save-excursion
    (goto-char start)
    (sort-subr nil 'next-rec 'end-rec 'start-key))
  (message "Sorting entries in %s...done" msg))

(defun next-rec ()
  (let ((end (marker-position bound)))
    (if (re-search-forward "^@" end t)
      (backward-char 1)
      (goto-char end))))

(defun end-rec ()
  (re-search-forward "^$" (marker-position bound)))

;; Make case-insensitive; pick up ( as well as {.
;; Ashwin Ram, 1/16/89.
(defun start-key ()
  (if (re-search-forward "\\s(" (marker-position bound) t)
      (let ((xx (point)))
	(search-forward ",")
	(backward-char 1)
	(downcase (buffer-substring xx (point))))))

