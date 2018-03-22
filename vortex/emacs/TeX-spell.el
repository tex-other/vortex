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
;; $Header: /home/yew/yew4/vortex/newdist/emacs/RCS/TeX-spell.el,v 1.14 1993/09/16 02:32:14 munson Exp $
;;
;; This file, "TeX-spell.el", is a subsystem of "TeX-mode.el".
;;

(require 'TeX-mode)

(defconst tex-spell-dict-buff "--- TeX Dictionary ---")
(defconst tex-spell-help-buff "--- TeX Spelling Help ---")
(defconst tex-spell-error-buff "--- TeX Spelling Errors ---")
(defconst tex-spell-hsl-suffix ".hsl"
  "Default filename extension for hashed spelling list.")
(defconst tex-spell-usl-suffix ".usl"
  "Default filename extension for unhashed spelling list.")

(defun tex-spell-document (&optional prefix)
  "Check spelling of every file included in current document.
With prefix argument, check spelling using a customized hashed spelling list,
if any, without query.  A customized hashed spelling list is either current
document master filename base concatenated with \".hsl\", or the file
bound to the variable tex-hsl-default."
  (interactive "P")
  (tex-visit-master-file)
  (let ((doc-type (tex-check-document-type)))
    (tex-get-include-files)
    (let ((lst tex-include-files)
	  (asknot nil)
	  (=slok= nil)
	  msg fn fnnd)
      (catch 'finish
	(while lst
	  (save-excursion
	    (set-buffer (find-file-noselect (setq fn (car lst))))
	    (setq msg (concat "Checking " doc-type " spelling for entire document"))
	    (setq fnnd (file-name-nondirectory fn))
	    (setq lst (append (tex-get-input-files msg fnnd) (cdr lst)))
	    (if (or asknot (tex-confirm (concat "Check spelling for " fnnd)))
		(progn
		  (find-file fnnd)
		  (message "%s, doing %s..." msg fnnd)
		  (sit-for 1)
		  (tex-spell-buffer prefix))))))
      (message "%s...done" msg))))

(defun tex-spell-buffer (&optional prefix)
  "Check spelling of every word in the buffer.
With prefix argument, check spelling using a customized hashed spelling list,
if any, without query.  A customized hashed spelling list is either current
document master filename base concatenated with \".hsl\", or the file
bound to the variable tex-hsl-default."
  (interactive "P")
  (tex-spell-region prefix 1 (point-max) (buffer-name)))

(defun tex-spell-paragraph (&optional prefix)
  "Check spelling of every word in the current paragraph.
With prefix argument, check spelling using a customized hashed spelling list,
if any, without query.  A customized hashed spelling list is either current
document master filename base concatenated with \".hsl\", or the file
bound to the variable tex-hsl-default."
  (interactive "P")
  (tex-spell-region prefix
		    (save-excursion (backward-paragraph) (point))
		    (save-excursion (forward-paragraph) (point))
		    "paragraph"))

(defun tex-spell-region (&optional prefix start end &optional description)
  "Like tex-buffer-spell but applies only to region.
From program, applies from START to END.
With prefix argument, check spelling using a customized hashed spelling list,
if any, without query.  A customized hashed spelling list is either current
document master filename base concatenated with \".hsl\", or the file
bound to the variable tex-hsl-default."
  (interactive "P\nr")
  (let* ((sm (set-marker (make-marker) start))
	(em (set-marker (make-marker) end))
	(doc-type (tex-check-document-type))
	(filter-string (tex-spell-filter doc-type))
	(filter-file (substring filter-string
				0 (string-match " " filter-string)))
	)
    (if (file-exists-p filter-file)
      nil
      (error "Spelling filter \"%s\" does not exist"
	     filter-string))
    (save-window-excursion
      (let ((=err= (get-buffer-create tex-spell-error-buff))
	    (=cmd= (get-buffer-create "*Shell Command Output*"))
	    (=buff= (current-buffer))
	    (=fn= (buffer-file-name))
	    (=msgs= (concat "Checking " doc-type " spelling for " (or description "region")))
	    (=again= nil)
	    speller)
	(catch 'all-done
	  (while t
	    (setq start (marker-position sm))
	    (setq end (marker-position em))
	    (setq tex-spell-small-window nil)
	    (save-excursion
	      (set-buffer =err=)
	      (widen)
	      (erase-buffer))
	    (setq speller (tex-spell-get-speller prefix))
	    (if (= ?\n (char-after (1- end)))
	      (progn
		(call-process-region start end shell-file-name nil =err= nil "-c" filter-string)
		(save-excursion
		  (set-buffer =err=)
		  (call-process-region 1 (point-max) shell-file-name t =err= nil "-c" speller)))
	      (let ((filter filter-string)) ; will be nil in =err= if not assgined
		(save-excursion
		  (set-buffer =err=)
		  (insert-buffer-substring =buff= start end)
		  (insert ?\n)
		  (call-process-region 1 (point-max) shell-file-name t =err= nil "-c" filter)
		  (call-process-region 1 (point-max) shell-file-name t =err= nil "-c" speller))))
	    (tex-spell-examine prefix)))
	(kill-buffer =err=)
	(kill-buffer =cmd=)))))
       
(defun tex-spell-examine (prefix)
  (let ((case-fold-search t)
	(case-replace t)
	(down t)
	(first t)
	(master (tex-check-master-file))
	word msgs reg-word new-word selected-word
	cmd next-cmd last-pos old-pos)
    (if (save-excursion
	  (set-buffer =err=)
	  (goto-char 1)
	  (> (buffer-size) 0))
      (progn
	(while (save-excursion
		 (if tex-spell-small-window
		   (progn
		     (delete-window)
		     (other-window -1)
		     (enlarge-window tex-slow-win-lines)
		     (setq tex-spell-small-window nil)))
		 (set-buffer =err=)
		 (not (or (= (buffer-size) 0)
			  (and down 
			       (eobp)
			       (if (tex-spell-exit master "next")
				 t
				 (previous-line 1)
				 nil))
			  (and (not down)
			       first
			       (tex-spell-exit master "previous")))))
	  (save-excursion
	    (set-buffer =err=)
	    (setq word (buffer-substring (point) (save-excursion (end-of-line) (point)))))
	  (tex-spell-get-cmd))
;;	(if (and (buffer-modified-p) (y-or-n-p (concat "Save file " =fn= "? ")))
;;	  (write-file =fn=))
	(if (y-or-n-p (concat =msgs= ", try again? "))
	  (setq =again= t)
	  (message "%s...done" =msgs=)
	  (throw 'all-done t)))
      (message "%s...done (no errors found)" =msgs=)
      (throw 'all-done t))))

(defun tex-spell-get-cmd ()
  (setq msgs (concat "Erroneous `" word "'  [SPC  DEL  n  p  r  R  w  C-r  ?=help]"))
  (setq reg-word (concat "\\b" (regexp-quote word) "\\b"))
  (setq last-pos (goto-char start))
  (setq next-cmd t)
  (setq selected-word nil)
  (catch 'get-cmd
    (while t
      (if next-cmd
	(progn
	  (message msgs)
	  (setq cmd (read-char))))
	(setq next-cmd t)
      (cond
	((= cmd ? )            ; Ignore and goto next error
	 (save-excursion
	   (set-buffer =err=)
	   (next-line 1)
	   (setq first nil)
	   (setq down t))
	 (throw 'get-cmd t))
	((= cmd ?\177)         ; Ignore and goto previous
	 (save-excursion
	   (set-buffer =err=)
	   (if (bobp)
	     (setq first t)
	     (previous-line 1))
	   (setq down nil))
	 (throw 'get-cmd t))
	((= cmd ?n)                        ; Next instance
	 (setq old-pos (point))
	 (goto-char last-pos)
	 (if (tex-spell-search reg-word end)
	   (progn
	     (setq last-pos (point))
	     (goto-char (match-beginning 0)))
	   (goto-char old-pos)
	   (message "Wrapping around...")
	   (sit-for 1)
	   (goto-char start)            ; Wrap-around
	   (tex-spell-search reg-word end)      ; Must be there
	   (setq last-pos (point))
	   (goto-char (match-beginning 0))))
	((= cmd ?p)                      	; Previous instance
	 (if (tex-spell-search reg-word start t)	; Backward
	   (save-excursion
	     (goto-char (match-end 0))
	     (setq last-pos (point)))
	   (message "Wrapping around...")
	   (sit-for 1)
	   (goto-char end)            ; Wrap-around
	   (tex-spell-search reg-word start t)    ; Must be there
	   (save-excursion
	     (goto-char (match-end 0))
	     (setq last-pos (point)))))
	((= cmd ?r)                      	 ; Replace with word
	 (setq new-word (read-string
			  (concat "Replacing `" word "' by: ")
			  word))
	 (save-excursion
	   (catch 'abort
	     (tex-spell-replace 'abort end reg-word word new-word t)
	     (save-excursion
	       (set-buffer =err=)
	       (delete-region (point) (progn (next-line 1) (point)))
	       (if (bobp) (setq down t)))
	     (throw 'get-cmd t))))
	((= cmd ?R)                         ; Replace w/o word
	 (setq new-word (read-string 
			  (concat "Replacing `" word "' by: ")
			  selected-word))
	 (save-excursion
	   (catch 'abort
	     (tex-spell-replace 'abort end reg-word word new-word t)
	     (save-excursion
	       (set-buffer =err=)
	       (delete-region (point) (progn (next-line 1) (point)))
	       (if (bobp) (setq down t)))
	     (throw 'get-cmd t))))
	((= cmd ?w)		  	; Dictionary lookup
	 (catch 'abort
	   (tex-spell-word word 'abort)))
	((= cmd ?\^r)
	 (message "Entering recursive edit...(return to spelling checking by ESC C-c)")
	 (let ((win (get-buffer-window tex-spell-dict-buff)))
	   (if win
	     (let ((pos (point)) left)
	       (pop-to-buffer tex-spell-dict-buff)
	       (recursive-edit)
	       (if (eq win (get-buffer-window (buffer-name)))
		 (progn
		   (save-excursion
		     (forward-word 1)
		     (backward-word 1)
		     (setq left (point)))
		   (backward-word 1)
		   (insert ?[)
		   (setq left (point))
		   (forward-word 1)
		   (setq selected-word (buffer-substring left (point)))
		   (insert ?])
		   (pop-to-buffer =buff=)
		   (setq next-cmd nil)
		   (message msgs)
		   (setq cmd (read-char))
		   (pop-to-buffer tex-spell-dict-buff)
		   (delete-window))))
	     (recursive-edit))))
	((= cmd ??)
	 (save-window-excursion
	   (tex-spell-help)
	   (setq next-cmd nil)
	   (message msgs)
	   (setq cmd (read-char))))
	(t
	 (ding)
	 (message "%s...WHAT?" msgs)
	 (sit-for 1))))))

(defun tex-spell-help ()
  (pop-to-buffer tex-spell-help-buff)
  (if (> (buffer-size) 0)
    nil
    (insert-string
"SPC -- Ignore current erroneous word, try next error, if any.
DEL -- Ignore current erroneous word and try the previous, if any. 
  n -- Go to next instance of the word in buffer, wrap around if necessary.
  p -- Go to previous instance of the word in buffer, wrap around if necessary.
  r -- Replace all instances of the word below dot.
       A repetition of current erroneous word appears at replacement prompt.
  R -- Replace all instances of the word below dot.
       If a word is selected in --- TeX Dictionary ---, it is repeated at 
       prompt; otherwise nothing is repeated.
  w -- Dictionary lookup for words containing the specified substring.
       Result displayed in the other window called --- TeX Dictionary ---.
C-r -- Enter recursive edit.  Return to spelling checking by ESC C-c.
  ? -- This help message.
C-g -- Abort to top level."))
  (enlarge-window (- 15 (window-height)))
  (goto-char 1)
  (other-window 1))
      
(defun tex-spell-exit (master scroll)
  (let ((=gbl= (get-buffer-create "--- TeX Global Spelling List ---"))
	(msg (concat "No more " scroll " entry, exit?  [SPC=(y)es  DEL=(n)o  RET=(s)ave]"))
	cmd)
    (catch 'done
      (while t
	(message msg)
	(setq cmd (read-char))
        (cond ((or (= cmd ? ) (= cmd ?y))
	       (throw 'done t))
	      ((or (= cmd ?\177) (= cmd ?n))
	       (throw 'done nil))
	      ((or (= cmd ?\r) (= cmd ?s))
	       (tex-spell-save-hsl master)
	       (throw 'done t))
	      (t
	       (setq cmd nil)
	       (ding)
	       (message "%s...WHAT?" msg)
	       (sit-for 1)))))))

(defun tex-spell-check-hsl (fn fnnd msgs)
  (save-excursion
    (message "%s, verifying spelling list %s..." msgs fnnd)
    (set-buffer =cmd=)
    (erase-buffer)
    (call-process shell-file-name nil t nil "-c" (concat "echo \"am\" | spellout " fn))
    (if (= (buffer-size) 0)
      t
      (message "Spelling list is corrupted, type any key to continue" fnnd)
      (read-char)
      (message "Removing corrupted spelling list %s..." fn)
      (shell-command (concat "\\rm -f" fn))
      (message "Removing corrupted spelling list %s...done" fn)
      (sit-for 1)
      (message "%s..." msgs)
      nil)))

(defun tex-spell-get-speller (prefix)
   "Make a hashed spelling list."
   (let* ((master-file (tex-check-master-file))
	  (hl (tex-spell-ext master-file tex-spell-hsl-suffix))
	  (hlnd (file-name-nondirectory hl))
          (ul (tex-spell-ext master-file tex-spell-usl-suffix))
	  (ulnd (file-name-nondirectory ul))
	  (slokp (boundp '=slok=))
	  (default ""))
     (if (or =again=
	     (and slokp =slok=)
	     (and (or (file-exists-p hl)
		      (and tex-hsl-default
			   (file-exists-p
			     (setq hl (expand-file-name tex-hsl-default)))
			   (setq hlnd tex-hsl-default)))
		  (file-newer-than-file-p hl ul)
		  (or prefix
		      (y-or-n-p (concat "Use " hlnd " as hashed spelling list? ")))
		      (tex-spell-check-hsl hl hlnd =msgs=)))
       (progn
	 (and slokp (setq =slok= t))
	 (setq =again= t)
	 (message "%s, using %s as spelling list..." =msgs= (file-name-nondirectory hl))
         (concat tex-speller " -d " hl))
       (if (and (or (file-exists-p ul)
		    (and tex-usl-default
			 (file-exists-p
			   (setq ul (expand-file-name tex-usl-default)))
			 (setq ulnd tex-usl-default)))
		(or prefix
		    (y-or-n-p (concat "Use " ulnd " as unhashed spelling list? "))))
	 (progn
	   (and slokp (setq =slok= t))
	   (setq =again= t)
	   (message "%s, creating %s from %s..." =msgs= hlnd ulnd)
	   (if (and tex-usl-default (file-exists-p tex-usl-default))
	     (setq default tex-usl-default))
	   (call-process shell-file-name nil =cmd= nil "-cf"
	     (concat "cat " ul " " default "|" tex-spellout " " tex-hsl-global
		     "|" tex-spellin " " tex-hsl-global ">" hl))
	   (message "%s, using %s as spelling list..." =msgs= hlnd)
	   (concat tex-speller " -d " hl))
	 (message "%s..." =msgs=)
	 (if (and tex-hsl-global (file-exists-p tex-hsl-global))
	   (concat tex-speller " -d " tex-hsl-global)
	   tex-speller)))))

(defun tex-spell-ext (fn suffix)
  (let* ((fnnd (file-name-nondirectory fn))
	 (dir (file-name-directory fn))
	 (pos (string-match "\\." fnnd)))
    (if (and pos (> pos 0))
      (concat (or dir "") (substring fnnd 0 pos) suffix)
      (concat fn suffix))))

(defun tex-spell-check-words ()
  (if (save-excursion
	(set-buffer =err=)
	(goto-char 1)
	(> (buffer-size) 0))
    (let ((msgs "Save words in spelling list.  [RET=(g)lobal  SPC=(l)ocal  DEL=(e)xamine]")
	  chr)
      (save-excursion
	(set-buffer =gbl=)
	(erase-buffer))
      (catch 'right
	(while t
	  (message msgs)
	  (setq chr (read-char))
	  (cond
	    ((or (= chr ?\177) (= chr ?e))
	     (tex-spell-process-uw)
	     (throw 'right t))
	    ((or (= chr ? ) (= chr ?l))
	     (throw 'right t))
	    ((or (= chr ?\r) (= chr ?g))
	     (save-excursion
	       (set-buffer =err=)
	       (copy-to-buffer =gbl= 1 (point-max))
	       (erase-buffer))
	     (throw 'right t))
	    (t
	     (ding)
	     (message "%s...WHAT?" msgs)
	     (sit-for 1))))))
    (message "No uncorrected words left.")
    (sit-for 1)
    (throw 'done t)))

(defun tex-spell-process-uw ()
  (let (word msgs cmd next-cmd)
    (save-excursion
      (set-buffer =err=)
      (goto-char 1)
      (catch 'checked
	(while (> (buffer-size) 0)
	  (beginning-of-line)
	  (if (eobp)
	    (if (y-or-n-p "No more next word, done? ")
	      (throw 'checked t)
	      (previous-line 1)))
	  (setq word (buffer-substring
		       (point) (save-excursion (end-of-line) (point))))
	  (if (string-equal word "")
	    (kill-line 1))
	  (tex-spell-cw-cmd))
	(if (save-excursion
	      (set-buffer =gbl=)
	      (= (buffer-size) 0))
	  (progn
	    (message "No uncorrected words left.")
	    (sit-for 1)
	    (throw 'done t)))))))
    
(defun tex-spell-cw-cmd ()
  (setq msgs (concat word "?  [SPC=(n)ext  DEL=(p)rev  RET=(g)lobal  LFD=(r)emove  ESC=(d)one]"))
  (catch 'get-cmd
    (while t
      (message msgs)
      (setq next-cmd t)
      (setq cmd (read-char))
      (cond
	((or (= cmd ? ) (= cmd ?n))		; Ignore and goto next error
	 (next-line 1))
	((or (= cmd ?\177) (= cmd ?p))		; Ignore and goto previous
	 (if (bobp)
	   (if (y-or-n-p "No more previous word, done? ")
	     (throw 'checked t))
	   (previous-line 1)))
	((or (= cmd ?\r) (= cmd ?g))		; move to the global list
	 (kill-line 1)
	 (if (eobp)
	   (previous-line 1))
	 (save-excursion
	   (set-buffer =gbl=)
	   (yank)))
	((or (= cmd ?\n) (= cmd ?r))		; remove 
	 (kill-line 1)
	 (if (eobp)
	   (previous-line 1)))
	((or (= cmd ?\e) (= cmd ?d))
	 (throw 'checked t))
	(t
	 (setq next-cmd nil)
	 (ding)
	 (message "%s...WHAT?" msgs)
	 (sit-for 1)))
      (if next-cmd
	(throw 'get-cmd t)))))

(defun tex-spell-save-hsl (master)
  (tex-spell-check-words)
  (if (save-excursion
	(set-buffer =gbl=)
	(> (buffer-size) 0))
    (tex-spell-save-global))
  (tex-spell-save-local))

(defun tex-spell-save-global ()
  (if tex-usl-default 
    (let* ((ulnd (file-name-nondirectory tex-usl-default))
	   (tmp (concat "/tmp/" ulnd))
	   (tmp= (concat tmp "=")))
      (save-excursion
        (set-buffer =gbl=)
	(if (file-exists-p tex-usl-default)
	  (progn
	    (message "Extending default unhashed spelling list %s..." tex-usl-default)
	    (write-region 1 (point-max) tmp= nil 'no-message)
	    (call-process shell-file-name nil =cmd= nil "-c"
	      (concat "cat " tmp= " " tex-usl-default " | sort -u > " tmp
		      "; rm " tmp= "; mv " tmp " " tex-usl-default))
	    (message "Extending default unhashed spelling list %s...done" tex-usl-default)
	    (sit-for 1))
	  (message "Creating default unhashed spelling list %s..." tex-usl-default)
	  (write-region 1 (point-max) tex-usl-default nil 'no-message)
	  (message "Creating default unhashed spelling list %s...done" tex-usl-default)
	  (sit-for 1))))
    (ding)
    (message "Default unhashed spelling list `tex-usl-default' not set.")
    (sit-for 1)))

(defun tex-spell-save-local ()
  (let* ((hl (tex-spell-ext master tex-spell-hsl-suffix))
	 (hlnd (file-name-nondirectory hl))
	 (hl-p (file-exists-p hl))
	 (ul (tex-spell-ext master tex-spell-usl-suffix))
	 (ulnd (file-name-nondirectory ul))
	 (ul-p (file-exists-p ul))
	 (tmp (concat "/tmp/" ulnd))
	 (tmp= (concat tmp "="))
	 (newer (file-newer-than-file-p hl ul))
	 (hl-g (and hl-p
		    (or =again=
		        (tex-spell-check-hsl hl hlnd "Saving spelling list"))))
	 (default ""))
    (if ul-p
      (progn
	(message "Extending unhashed spelling list %s..." ulnd)
	(write-region 1 (point-max) tmp= nil 'no-message)
	(call-process shell-file-name nil =cmd= nil "-c"
	  (concat "cat " tmp= " " ul " | sort -u > " tmp
		  "; rm " tmp= "; mv " tmp " ."))
	(message "Extending unhashed spelling list %s...done" ulnd))
      (if (> (buffer-size) 0)
	(progn
	  (message "Creating unhashed spelling list %s..." ulnd)
	  (write-region 1 (point-max) ul nil 'no-message)
	  (message "Creating unhashed spelling list %s...done" ulnd))))
    (if (and newer hl-g)
     (progn
       (message "Extending hashed spelling list %s..." hlnd)
       (call-process-region 1 (point-max) shell-file-name nil =cmd= nil "-c"
	 (concat "cat |" tex-spellout " " hl "|" tex-spellin " " hl
		   ">" "/tmp/" hlnd "; mv /tmp/" hlnd " ."))
       (and (boundp '=slok=) (setq =slok= t))
       (message "Extending hashed spelling list %s...done" hlnd)
       (sit-for 1))
     (message "Creating hashed spelling list %s..." hlnd)
     (if (and tex-usl-default (file-exists-p tex-usl-default))
       (setq default tex-usl-default))
     (call-process shell-file-name nil =cmd= nil "-c"
       (concat "cat " ul " " default "|" tex-spellout " " tex-hsl-global
	       "|" tex-spellin " " tex-hsl-global ">" hl))
       (and (boundp '=slok=) (setq =slok= t))
     (message "Creating hashed spelling list %s...done" hlnd)
     (sit-for 1))))

(defun tex-spell-word (&optional pre &optional abort)
  "Check the spelling of a word.  If PRE is non-nil, use it as default.
If pre is a non-string and is set by prefix argument, then use the
preceding word as default."
  (interactive "P")
  (let ((old (current-buffer))
	(tmp (get-buffer-create tex-spell-dict-buff))
	(offset 0)
	(dict nil)
	key m i fix prefix suffix msg bow)
    (catch 'loop
      (while t
        (message "Lookup string as prefix, infix, or suffix? [RET/p  i  s]")
	(setq fix (read-char))
	(cond
	  ((or (= fix ?\r) (= fix ?p))
	   (setq prefix "")
	   (setq suffix "")
	   (setq fix "prefix")
	   (throw 'loop t))
	  ((= fix ?i)
	   (setq dict t)
	   (setq prefix "")
	   (setq suffix "")
	   (setq fix "infix")
	   (throw 'loop t))
	  ((= fix ?s)
	   (setq dict t)
	   (setq prefix "^.*")
	   (setq suffix "$")
	   (setq fix "suffix")
	   (throw 'loop t))
	  (t
	   (ding)
           (message "Lookup string as prefix, infix, or suffix  [RET/p  i  s], WHAT?")
	   (sit-for 1)))))
    (if (and pre (not (stringp pre)))
      (save-excursion
	(backward-word 1)
        (setq bow (point))
	(forward-word 1)
	(setq pre (buffer-substring bow (point)))))
    (setq key (read-string (concat "String as " fix ": ") pre))
    (if (string-equal key "")
      (if abort
	(progn
	  (ding)
	  (message "Don't know how to lookup a null string.")
	  (throw abort nil))
        (error "Don't know how to lookup a null string."))
      (setq msg (concat "Looking up `" key "' as " fix " in \"" tex-dictionary "\"..."))
      (setq key (concat prefix key suffix)))
    (message msg)
    (set-buffer tmp)
    (erase-buffer)
    (if dict
      (call-process tex-egrep nil t nil key tex-dictionary)
      (call-process tex-look nil t nil key))
    (if (= (buffer-size) 0)
      (progn
	(ding)
        (message "%sfailed" msg)
	(sit-for 1))
      (goto-char 1)
      (while (not (eobp))
	(end-of-line)
	(setq offset (max offset (+ (current-column) 5)))
	(next-line 1))
      (setq m (/ (window-width) offset))
      (goto-char 1)
      (while (not (eobp))
	(setq i 1)
	(while (and (< i m) (not (eobp)))
	  (end-of-line)
	  (delete-char 1)
	  (indent-to (1+ (* i offset)))
	  (setq i (1+ i)))
	(next-line 1))
      (goto-char 1)
      (pop-to-buffer tmp)
      (message "%sdone" msg))
    (pop-to-buffer old)))

(defun tex-spell-replace (cont bound from-regexp from-string to-string query-flag)
  (let ((nocasify (not (and case-fold-search 
			    case-replace
			    (string-equal from-string (downcase from-string)))))
	(literal nil)
	(keep-going t))
    (push-mark)
    (push-mark)
    (while (and keep-going
		(not (eobp))
		(progn
		  (set-mark (point))
		  (tex-spell-search from-regexp bound)))
      (undo-boundary)
      (if (not query-flag)
	  (replace-match to-string nocasify literal)
	(let (done replaced)
	  (while (not done)
	    (message "Query replacing `%s' with `%s' (^C if to quit prematurely)..." from-string to-string)
	    (setq char (read-char))
	    (cond
	      ((= char ?\^c)
	       (ding)
	       (message "Quitting query replacing...(return to previous level)")
	       (sit-for 1)
	       (throw cont nil))
	      ((= char ?\e)
	       (setq keep-going nil)
	       (setq done t))
	      ((= char ?^)
	       (goto-char (mark))
	       (setq replaced t))
	      ((= char ?\ )
	       (or replaced (replace-match to-string nocasify literal))
	       (setq done t))
	      ((= char ?\.)
	       (or replaced (replace-match to-string nocasify literal))
	       (setq keep-going nil)
	       (setq done t))
	      ((and (not replaced) (= char ?\,))
	       (replace-match to-string nocasify literal)
	       (setq replaced t))
	      ((= char ?!)
	       (or replaced (replace-match to-string nocasify literal))
	       (setq done t query-flag nil))
	      ((= char ?\177)
	       (setq done t))
	      ((= char ?\^r)
	       (store-match-data
		 (prog1 (match-data)
			(save-excursion (recursive-edit)))))
	      ((= char ?\^w)
	       (delete-region (match-beginning 0) (match-end 0))
	       (store-match-data
		 (prog1 (match-data)
		        (save-excursion (recursive-edit))))
	       (setq done t))
	      (t
	       (setq keep-going nil)
	       (setq unread-command-char char)
	       (setq done t)))))))
    keep-going))

(defvar tex-slow-speed 2400)
(defvar tex-slow-win-lines 1)
(defvar tex-spell-small-window nil "Flag designating small window")
(defvar tex-spell-large-window-begin 0)
(defvar tex-spell-large-window-end 0)

(defun tex-spell-search (regexp bound &optional backward)
  (let ((search-fun (if backward 're-search-backward 're-search-forward))
	(slow-terminal-mode (<= (baud-rate) tex-slow-speed))
	(window-min-height (min window-min-height (1+ tex-slow-win-lines)))
	(found-dot nil))	; to restore dot from a small window
    (if (funcall search-fun regexp bound t)
      (progn
        (setq found-dot (point))
        (if (not tex-spell-small-window)
	  (progn
	    (setq tex-spell-large-window-begin 
	      (save-excursion 
		(move-to-window-line 0)
		(beginning-of-line)
		(point)))
	    (setq tex-spell-large-window-end
	      (save-excursion 
		(move-to-window-line -1)
		(end-of-line)
		(point)))))
        (if (and slow-terminal-mode
		 (not (or tex-spell-small-window (pos-visible-in-window-p))))
	  (progn
	    (setq tex-spell-small-window t)
	    (move-to-window-line 0)
	    (split-window nil (- (window-height) (1+ tex-slow-win-lines)))
	    (other-window 1)))
	(goto-char found-dot)
	(if (and tex-spell-small-window
		 (>= found-dot tex-spell-large-window-begin)
		 (<= found-dot tex-spell-large-window-end))
	  (progn
	    (setq tex-spell-small-window nil)
	    (delete-window)
	    (other-window -1)
	    (enlarge-window tex-slow-win-lines)
	    (goto-char found-dot)))
 	t))))

(defun tex-spell-complete ()
  "Complete the current word by doing a dictionary lookup using
the word as prefix.  Prompt the user if more than one instance is found.
When the user confirms a match, it replaces the original prefix."
  (interactive)
  (if (file-exists-p tex-look)
    nil
    (error "Spelling filter \"%s\" does not exist.  Rebind tex-look." tex-look))
  (let ((=err= (get-buffer-create tex-spell-error-buff))
        bow eow word)
    (save-excursion
      (if (looking-at "\\w")
	(forward-word 1)
	(skip-chars-backward "[ \t\n,.!?:;')}]"))
      (setq eow (point))
      (backward-word 1)		; beginning of word
      (setq bow (point)))
    (if (setq word (tex-spell-match (buffer-substring bow eow)))
      (save-excursion
	(delete-region bow eow)
	(goto-char bow)
	(insert word)))
    (if (looking-at "\\w")
      (forward-word 1))
    (kill-buffer =err=)))

(defun tex-spell-match (key)
  (let ((asknot nil)
	(wd nil)
	(col 1)
	(nol 0))
    (message "Completing %s..." key)
    (save-excursion
      (set-buffer =err=)
      (widen)
      (erase-buffer)
      (call-process tex-look nil t nil key)
      (goto-char 1)
      (setq nol (count-lines 1 (point-max)))
      (if (> (buffer-size) 0)
	(catch 'finish
	  (while t
	    (setq wd (buffer-substring (point)
				       (save-excursion (end-of-line) (point))))
	    (if (tex-confirm (concat "Confirm (" col ":" nol ") `" wd "'") ?n ?p ?y)
	      (if asknot
		(throw 'finish wd)
		(next-line 1)
		(beginning-of-line)
		(if (= col nol)
		  (progn
		    (message "Wrapping around...")
		    (sit-for 1)
		    (goto-char 1)
		    (setq col 1))
		  (setq col (1+ col))))
	      (if (= col 1)
		(progn
		  (message "Wrapping around...")
		  (sit-for 1)
		  (goto-char (point-max))
		  (setq col nol)
		  (if (looking-at "^$")
		    (previous-line 1)))
	        (previous-line 1)
		(setq col (1- col)))
	      (beginning-of-line))))
	(ding)
	(message "No match.")
	nil))))
