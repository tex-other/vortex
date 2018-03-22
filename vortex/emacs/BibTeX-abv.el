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
;; $Header: /home/yew/yew4/vortex/newdist/emacs/RCS/BibTeX-abv.el,v 1.14 1993/09/16 02:32:14 munson Exp $
;;
;; This file, bibtex-abv.el, is a subsystem of BibTeX-mode.
;; It deals with field & group abbreviation compiling, loading, browsing, etc.
;; It gets autoloaded whenever a function defined in this file is invoked.
;;

(require 'BibTeX-mode)

;; =============
;; Abbreviations
;; =============

(defconst bibtex-bib-ext ".bib")
(defconst bibtex-abbrev-ext ".abv")
(defvar bibtex-gabbrev-evaled nil 
  "Flag which is set t if default group abbrev has been evaluated")
(defvar bibtex-fabbrev-evaled nil 
  "Flag which is set t if default field abbrev has been evaluated")
(defvar bibtex-gabbrev nil "List of group abbrev names")
(defvar bibtex-fabbrev nil "List of field abbrev names")
(defvar bibtex-abbrev-internal nil "List of files that have been read")

;; ===========
;; Prompt Data
;; ===========

(defconst bibtex-browse-prompt
  '(((?\r ?y) "RET" yes)
    ((?\e) "ESC" yes-abbrev)
    ((?  ?n) "SPC" next)
    ((?\177 ?p) "DEL" previous)
    ((?s) "s" show)
    ((?g) "g" goto)
    ((?f) "f" new-file)
    ((?\^r) "C-r" redit)
    ((??) "?=help" help))
  "Data for abbreviation browsing prompt")

(defun bibtex-make-gabbrev (start end name)
  (interactive "r\nsGroup abbrev name: ")
  (let ((case-fold-search t))
    (if (string-equal name "")
	(error "Can't accept null name...try again")
      (message "Defining group abbreviation `%s'..." name)
      (let* (grp
	     (fnnd (file-name-nondirectory (buffer-file-name)))
	     (abvbuf (get-buffer-create 
		      (file-name-nondirectory 
		       (concat default-directory 
			       (bibtex-fn-w-ext fnnd bibtex-abbrev-ext))))))
	(goto-char start)
	(beginning-of-line)
	(setq start (point))
	(goto-char end)
	(end-of-line)
	(setq grp (buffer-substring start (point)))
	(bibtex-current-entry)
	(previous-line 1)
	(insert "\n%GROUP(")
	(save-excursion
	  (insert name ",\n" grp "\n)\n")
	  (bibtex-scan-gabbrev fnnd abvbuf))
	(kill-buffer abvbuf))
      (message "Defining group abbreviation `%s'...done" name))))

(defun bibtex-insert-gabbrev (sym)
  (interactive "SGroup abbrev name (default browsing mode): ")
  (let ((name (symbol-name sym))
	(fn (buffer-file-name))
	(case-fold-search t)
	cont)
    (message "Inserting group abbreviation...")
    (if (string-equal name "") 
      (progn
        (bibtex-eval-gabbrev fn)
	(let ((pair (bibtex-browse-gabbrev)))
	  (if pair
	    (progn
	      (setq name (car pair))
	      (setq cont (nth 1 pair)))
	    (setq cont nil))))
      (setq cont (get sym 'group)))
    (if cont
      (progn
	(if (bolp)
	  nil
	  (next-line 1)
	  (beginning-of-line))
	(insert cont)
	(message "Inserting group abbreviation `%s'...done" name))
      (bibtex-read-file sym name))))

(defun bibtex-browse-gabbrev ()
  (let ((=prev= nil)
	(=curr= bibtex-gabbrev)
	(=buff= (get-buffer-create "--- BibTeX Abbreviation ---"))
	=name=	=cont=)
    (catch 'exit
      (while =curr=
	(setq =name= (car =curr=))
	(setq =cont= (get (intern =name=) 'group))
	(setq =prev= (cons =name= =prev=))
	(setq =curr= (cdr =curr=))
	(if (get-buffer-window =buff=) (bibtex-show-abbrev))
	(bibtex-browse-cmd)))))
	
(defun bibtex-browse-cmd (&optional fld)
  (let ((cmd 
	 (vortex-user-response (concat "Confirm `" =name= "' ")
			       bibtex-browse-prompt))
	win)
    (catch 'done
      (while t
	(cond
	 ((eq cmd 'yes)
	  (if (setq win (get-buffer-window =buff=))
	      (delete-window win))
	  (throw 'exit (list =name= =cont= t)))
	 ((eq cmd 'yes-abbrev)
	  (if (setq win (get-buffer-window =buff=))
	      (delete-window win))
	  (throw 'exit (list =name= =cont= nil)))
	 ((eq cmd 'next)
	  (if =curr=
	      nil
	    (if (y-or-n-p "Can't find next abbreviation, exit? ")
		nil
	      (message "Wrapping around...")
	      (sit-for 1)
	      (setq =curr= (reverse =prev=))
	      (setq =prev= nil)))
	  (throw 'done t))
	 ((eq cmd 'previous)
	  (if (setq =prev= (cdr =prev=))
	      (progn
		(setq =curr= (cons (car =prev=) (cons =name= =curr=)))
		(setq =prev= (cdr =prev=)))
	    (if (y-or-n-p "Can't find previous abbreviation, exit? ")
		nil
	      (message "Wrapping around...")
	      (sit-for 1)
	      (setq =prev= (reverse (cons =name= =curr=)))
	      (setq =curr= (list (car =prev=)))
	      (setq =prev= (cdr =prev=))))
	  (throw 'done nil))
	 ((eq cmd 'show)
	  (bibtex-show-abbrev)
	  (setq cmd (vortex-user-response (concat "Confirm `" =name= "' ")
					  bibtex-browse-prompt)))
	 ((eq cmd 'goto)
	  (let ((key (read-string "Goto key: "))
		grp)
	    (setq =curr= (if fld bibtex-fabbrev bibtex-gabbrev))
	    (while =curr=
	      (setq grp (car =curr=))
	      (if (string-lessp key grp)
		  (throw 'done t)
		(setq =prev= (cons grp =prev=))
		(setq =curr= (cdr =curr=))))
	    (setq =curr= (list (car =prev=)))
	    (setq =prev= (cdr =prev=))
	    (throw 'done t)))
	 ((eq cmd 'new-file)
	  (throw 'done (setq =curr= nil)))
	 ((eq cmd 'redit)
	  (message "Entering recursive edit...use ESC C-c to return")
	  (recursive-edit)
	  (setq cmd (vortex-user-response (concat "Confirm `" =name= "' ")
					  bibtex-browse-prompt)))
	 ((eq cmd 'help)
	  (save-window-excursion
	    (bibtex-browse-help)
	    (setq cmd (vortex-user-response (concat "Confirm `" =name= "' ")
					    bibtex-browse-prompt))))
	 )))))

(defun bibtex-show-abbrev ()
   (pop-to-buffer =buff=)
   (erase-buffer)
   (insert "Abbreviation: " =name= "\n\n" =cont=)
   (goto-char 1)
   (other-window 1))
  
(defun bibtex-browse-help ()
  (pop-to-buffer "--- BibTeX Abbreviation Help ---")
  (if (> (buffer-size) 0)
    (goto-char (point-min))
    (insert-string
"RET or `y' -- Confirm and exit, the abbreviated text is inserted.
ESC -- Confirm and exit, only the abbreviation is inserted (FIELD ABBREV ONLY).
SPC -- Ignore current abbreviation.  Advance to the next.
DEL -- Ignore current abbreviation.  Go back to the previous.
`s' -- Show the content of current abbreviation in the other window.
`g' -- Go to the abbreviation whose name is greater than the specified key.
`f' -- Not in current list of abbreviations.  Read more from a new file.
C-r -- Enrter recursive edit.  Return to browsing by ESC C-c.
`?' -- This help message.")
    (goto-char (point-min)))
  (other-window 1))

(defun bibtex-read-file (sym name &optional field)
  (let ((msg "Load file (default path TEXBIB): ")
	fn tmp)
    (if (y-or-n-p (concat (if field "Field" "Group")
                          " abbreviations"
                          (if (string-equal name "") "" (concat " `" name "'"))
			  " not found, read from another file? "))
      (progn
	(catch 'ok
	  (while t
	    (setq fn (vortex-file-exists-p (setq tmp (read-string msg))	bibtex-path))
	    (if fn
	      (if (vortex-memq fn bibtex-abbrev-internal)
		(progn
		  (ding)
		  (if (y-or-n-p (concat "Abbreviations in \"" 
					tmp 
					"\" have been read, try another file? "))
		    nil
		    (throw 'ok t)))
		(throw 'ok t))
	      (ding)
	      (message "%s%s...not found, try again" msg tmp)
	      (sit-for 2))))
	(bibtex-load-abbrev fn (if field "field" "group"))
	(if field
	  (bibtex-insert-fabbrev))
	  (bibtex-insert-gabbrev sym))
      (if field
	(message "Browsing field abbreviations...abort")
	(message "Inserting group abbreviation...abort")))))

(defun bibtex-eval-gabbrev (fn)
  (if bibtex-gabbrev-evaled
    nil
    (if bibtex-abbrev-files
      (let ((lst bibtex-abbrev-files))
	(while lst
	  (bibtex-load-abbrev (car lst) "group")
	  (setq lst (cdr lst)))
	(setq bibtex-gabbrev-evaled t))))
  (if (or bibtex-require-local-eval (vortex-memq fn bibtex-abbrev-internal))
    nil
    (bibtex-load-abbrev fn "group")
    (setq bibtex-require-local-eval t)))

(defun bibtex-insert-fabbrev ()
  (interactive)
  (let ((fn (buffer-file-name))
	(case-fold-search t)
	pair name cont)
    (message "Browsing field abbreviations...")
    (bibtex-eval-fabbrev fn)
    (setq pair (bibtex-browse-fabbrev))
    (if pair
      (progn
	(setq name (car pair))
	(setq cont (nth 1 pair))
	(if (nth 2 pair)
	  (progn
            (insert cont)
	    (message "Browsing field abbreviations...done (`%s' text inserted)" name))
	  (insert name)
	  (bibtex-erase-delimiters t)
	  (message "Browsing field abbreviations...done (`%s' inserted)" name)))
      (bibtex-read-file nil "" t))))

(defun bibtex-browse-fabbrev ()
  (let ((=prev= nil)
	(=curr= bibtex-fabbrev)
	(=buff= (get-buffer-create "--- BibTeX Abbreviation ---"))
	=name=	=cont=)
    (catch 'exit
      (while =curr=
	(setq =name= (car =curr=))
	(setq =cont= (get (intern =name=) 'field))
	(setq =prev= (cons =name= =prev=))
	(setq =curr= (cdr =curr=))
	(if (get-buffer-window =buff=) (bibtex-show-abbrev))
	(bibtex-browse-cmd t)))))

(defun bibtex-eval-fabbrev (fn)
  (if bibtex-fabbrev-evaled
    nil
    (if bibtex-abbrev-files
      (let ((lst bibtex-abbrev-files))
	(while lst
	  (bibtex-load-abbrev (car lst) "field")
	  (setq lst (cdr lst)))
	(setq bibtex-fabbrev-evaled t))))
  (if (or bibtex-require-local-eval (vortex-memq fn bibtex-abbrev-internal))
    nil
    (bibtex-load-abbrev fn "field"))
  (setq bibtex-require-local-eval t))

(defun bibtex-fn-w-ext (fn suffix)
  "Attach SUFFIX to the base of FN, if FN has no extension or has
extension `\.bib' or `\.abv'.  Otherwise (i.e FN has other extension)
FN itself is returned."
  (let ((base (string-match "\\.bib" fn))
	(ext-set t))
    (if base
      nil
      (if (setq base (string-match "\\.abv" fn))
	nil
	(if (string-match "\\." fn)		; Already has extension
	  (setq ext-set nil))))
    (if ext-set
      (concat (substring fn 0 base) suffix)
      fn)))

(defun bibtex-load-abbrev (fn &optional type)
  "Load abbreviations from file FN.
Load both types of abbrev is TYPE is nil; otherwise load only TYPE."
  (interactive "FLoad abbreviations, file name base: ")
  (let ((abv (bibtex-fn-w-ext (expand-file-name fn) bibtex-abbrev-ext))
	(case-fold-search t)
	bib)
    (if (vortex-file-exists-p abv bibtex-path)
      (bibtex-read-abbrev abv type)
      (message "\"%s\" (compiled abbreviations) not found" (file-name-nondirectory abv))
      (sit-for 2)
      (setq bib (bibtex-fn-w-ext abv bibtex-bib-ext))
      (if (vortex-file-exists-p bib bibtex-path)
        (bibtex-save-abbrev bib abv t)))))

(defun bibtex-read-abbrev (abv type)
  "Read a .abv file.  Each entry in this file is
	TYPE (i.e. either field or group)
	NAME
        TEXT.
If TYPE is nil, read both types, else read only the specified type."
  (let ((abvbuf (find-file-noselect abv))
	(fnnd (file-name-nondirectory abv))
	(abv (or type ""))
	(key (if type (concat "^" type) "\\w")))
    (save-excursion
      (message "Loading %s abbreviations from file \"%s\"..." abv fnnd)
      (setq bibtex-abbrev-internal (cons abv bibtex-abbrev-internal))
      (set-buffer abvbuf)
      (goto-char 1)
      (while (re-search-forward key nil t)
	(beginning-of-line)
	(bibtex-scan-abbrev fnnd abvbuf))
      (kill-buffer abvbuf)
      (message "Loading %s abbreviations from file \"%s\"...done" abv fnnd))))

(defun bibtex-scan-abbrev (fnnd abvbuf)
  (let ((field (eq (read abvbuf) 'field))
	(name (read abvbuf)))
    (message "Loading abbreviations from file \"%s\", doing `%s'..." fnnd name)
    (if field
      (bibtex-add-fabbrev name)
      (bibtex-add-gabbrev name))
    (eval (read abvbuf))))

(defun bibtex-save-abbrev (&optional fn &optional abv &optional check)
  "Compile abbreviations, pull them up to the beginning of the file,
convert them into Emacs Lisp objects, and save them in the a \.abv file.
If the optional CHECK is non-nil, ask the user if it has already been
compiled."
  (interactive)
  (message "Compiling abbreviations...")
  (let* ((fn (or fn (buffer-file-name)))
	 (fnnd (file-name-nondirectory fn))
	 (abv (or abv (concat default-directory 
			      (bibtex-fn-w-ext fnnd bibtex-abbrev-ext))))
	 (case-fold-search t))
    (save-excursion
      (set-buffer (find-file-noselect fn))
      (if (save-excursion 
	    (goto-char 1)	       ;; Compile the whole file
	    (re-search-forward bibtex-abbrev-match-fg nil t))
	(if (or check
		(not bibtex-require-local-eval)
		(y-or-n-p "Abbreviations already compiled, overwrite? "))
	  (save-excursion
	    (goto-char 1)
	    (let* ((base (if (re-search-forward bibtex-entry-no-group-match nil t)
			   (progn
			     (beginning-of-line)
			     (1- (point)))
			   (point-max)))
		   (abvbuf (get-buffer-create (file-name-nondirectory abv)))
		   abbrev boa eoa astring)
	      (message "Compiling abbreviations...(doing \"%s\")" fnnd)
	      (save-excursion
		(set-buffer abvbuf)
		(erase-buffer))
	      (goto-char 1)
	      (while (re-search-forward bibtex-abbrev-match-fg nil t)
		(if (= (char-after (match-beginning 0)) ?@)
		  (setq abbrev (bibtex-scan-fabbrev fnnd abvbuf))
		  (setq abbrev (bibtex-scan-gabbrev fnnd abvbuf)))
		(setq boa (car abbrev))
		(if (> boa base)	; Pulling it up to the beginning
		  (save-excursion
		  (setq eoa (nth 1 abbrev))
		  (setq astring (buffer-substring boa eoa))
		  (delete-region boa eoa)
		    (goto-char base)
		    (insert astring) 
		    (setq base (point)))))
	      (save-excursion
		(set-buffer abvbuf)
		(write-region 1 (point-max) abv nil 'no-message)) ; Save .abv
	      (kill-buffer abvbuf)
	      (message "Compiling abbreviations...done (saved in %s)" abv))
	    (if check nil (setq bibtex-require-local-eval t)))
	  (message "Compiling abbreviations...quit"))
	(if (file-exists-p abv)
	  (call-process shell-file-name nil nil nil "-c" (concat "\\rm " abv)))
	(message "Compiling abbreviations...done (no abbreviations compiled)")))))
  
(defun bibtex-scan-fabbrev (fnnd abvbuf)
  (let* ((abbrev (bibtex-current-field))
	 (name (nth 2 (nth 3 abbrev)))
	 (end (nth 1 abbrev))
	 (text (nth 2 (nth 3 (bibtex-next-field)))))
    (message "Compiling abbreviations in file \"%s\", doing `%s'..." fnnd name)
    (goto-char end)
    (save-excursion
      (set-buffer abvbuf)
      (bibtex-add-fabbrev name)
      (put (intern name) 'field text)
      (insert "field\n\"" name "\"\n" "(put '" name " 'field \"" text "\")\n\n"))
    abbrev))
	
(defun bibtex-add-fabbrev (name)
  (let ((head nil)
	(tail bibtex-fabbrev)
	field)
    (catch 'ok
      (while tail
	(setq field (car tail))
	(cond
	  ((string-equal name field)
	   (throw 'ok t))
	  ((string-lessp name field)
	   (throw 'ok (setq tail (cons name tail))))
	  (t
	   (setq head (cons field head))
	   (setq tail (cdr tail)))))
      (setq tail (cons name nil)))
    (setq bibtex-fabbrev (append (reverse head) tail))))

(defun bibtex-scan-gabbrev (fnnd abvbuf)
  (let* ((abbrev (bibtex-current-entry t))
	 (bog (+ (nth 1 (nth 3 abbrev)) 2))
	 (eog (- (nth 1 abbrev) 2))
	 (name (nth 2 (nth 3 abbrev)))
	 (string (buffer-substring bog eog)))
    (message "Compiling abbreviations in file \"%s\", doing `%s'..." fnnd name)
    (goto-char eog)
    (save-excursion
      (set-buffer abvbuf)
      (bibtex-add-gabbrev name)
      (insert "group\n\"" name "\"\n")
      (save-excursion (insert "(put '" name " 'group \n" "\"" string "\n\")"))
      (eval (read abvbuf))
      (insert "\n\n"))
    abbrev))
      
(defun bibtex-add-gabbrev (name)
  (let ((head nil)
	(tail bibtex-gabbrev)
	grp)
    (catch 'ok
      (while tail
	(setq grp (car tail))
	(cond
	  ((string-equal name grp)
	   (throw 'ok t))
	  ((string-lessp name grp)
	   (throw 'ok (setq tail (cons name tail))))
	  (t
	   (setq head (cons grp head))
	   (setq tail (cdr tail)))))
      (setq tail (cons name nil)))
    (setq bibtex-gabbrev (append (reverse head) tail))))
