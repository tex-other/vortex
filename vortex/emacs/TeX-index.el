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
;; $Header: /home/yew/yew4/vortex/newdist/emacs/RCS/TeX-index.el,v 1.14 1993/09/16 02:32:14 munson Exp $
;;
;; This file, "TeX-index.el", is a subsystem of "TeX-mode.el".
;; It contains miscellaneous functions for indexing TeX documents.
;;

(require 'TeX-mode)

(defconst tex-index-buff "--- TeX Indexing Help ---")
(defconst tex-index-char ?!)
(defconst tex-index-limit 30 "Max REGEXP string length in minibuffer display.")
(defconst tex-index-entries "\\\\index\\|\\\\cite\\|\\\\nocite")
(defconst tex-index-pattern (concat tex-index-level "\\|" tex-index-entries))
(defvar tex-index-keyfile nil)


;;======
;; Mode
;;======

(defun tex-index-chmod ()
  "Change mode permanently.  A mode is a 3-bit code prefix-actual-encap-keyptrn."
  (interactive)
  (let* ((mode (read-string "Change mode (level-actual-encap-keyptrn, 4-bit binary): "))
	 (m1 (substring mode 0 1))
	 (m2 (substring mode 1 2))
	 (m3 (substring mode 2 3))
	 (m4 (substring mode 3 4)))
    (if m1
      (setq tex-index-level-on (/= (string-to-char m1) ?0)))
    (if m2
      (setq tex-index-actual-on (/= (string-to-char m2) ?0)))
    (if m3
      (setq tex-index-encap-on (/= (string-to-char m3) ?0)))
    (if m4
      (setq tex-index-keyptrn-on (/= (string-to-char m4) ?0)))
    (if (interactive-p)
      (message "Index mode (prefix-actual-encap-keyptrn) is now %s" mode))))

(defun tex-index-level-toggle ()
  "Toggle the flag tex-index-level-on."
  (interactive)
  (if tex-index-level-on
    (progn
      (setq tex-index-level-on nil)
      (message "Index level specification disabled."))
    (setq tex-index-level-on t)
    (message "Index level specification enabled.")))

(defun tex-index-actual-toggle ()
  "Toggle the flag tex-index-actual-on."
  (interactive)
  (if tex-index-actual-on
    (progn
      (setq tex-index-actual-on nil)
      (message "Index actual field disabled."))
    (setq tex-index-actual-on t)
    (message "Index actual field enabled.")))

(defun tex-index-encap-toggle ()
  "Toggle the flag tex-index-encap-on."
  (interactive)
  (if tex-index-encap-on
    (progn
      (setq tex-index-encap-on nil)
      (message "Page number encapsulator disabled."))
    (setq tex-index-encap-on t)
    (message "Page number encapsulator enabled.")))
  
(defun tex-index-keyptrn-toggle ()
  "Toggle the flag tex-index-keyptrn-on."
  (interactive)
  (if tex-index-keyptrn-on
    (progn
      (setq tex-index-keyptrn-on nil)
      (message "Saving \[key, ptrn\] pair disabled."))
    (setq tex-index-keyptrn-on t)
    (message "Saving \[key, ptrn\] pair enabled.")))


;;===========================
;; Mode-dependent Operations
;;===========================

(defun tex-index-get-level ()
  "Get the index level.  If the prefix is not terminated by tex-index-level,
one is attached."
  (if tex-index-level-on
    (let* ((pre (read-string (concat "Index prefix (max 2 levels, "
				     tex-index-level
				     " as delimiter, RET if none): ")))
	   (l (length pre)))
      (if (or (= l 0)
	      (string-equal (substring pre (1- l) l) tex-index-level))
	pre
	(concat pre tex-index-level)))
    ""))

(defun tex-index-get-actual ()
  "Get index actual field."
  (if tex-index-actual-on
    (let ((str (read-string "Index actual field: ")))
      (if (string-equal str "")
	""
	(concat tex-index-actual str)))
    ""))

(defun tex-index-get-encap ()
  "Get the index page number encapsulator."
  (if tex-index-encap-on
    (let ((msg "Page encapsulator: RET=none, b=Bf, i=It, s=Sl, u=Ul, c=see, o=other.")
	  var ans)
      (catch 'done
	(while t
	  (message msg)
	  (setq var (read-char))
	  (setq ans
		(cond ((= var ?\r) "")
		      ((= var ?b) (concat tex-index-encap "Bf"))
		      ((= var ?i) (concat tex-index-encap "It"))
		      ((= var ?s) (concat tex-index-encap "Sl"))
		      ((= var ?u) (concat tex-index-encap "Ul"))
		      ((= var ?c) (concat tex-index-encap "see"))
		      ((= var ?o) 
		       (let ((str (read-string "Encapsulator string: ")))
			 (if (string-equal str "")
			   ""
			   (concat tex-index-encap str))))
		      (t (progn
			   (setq cont nil)
			   (ding)
			   (message "%s...WHAT?" msg)
			   (sit-for 1)
			   nil))))
	  (if ans
	    (throw 'done ans)))))
    ""))


;;==============
;; Saving Pairs
;;==============

(defun tex-index-save (key &optional regexp)
  "Save a \[KEY, PATTERN\] pair in a specified file."
  (interactive "sSave index key: ")
  (if (or (interactive-p) tex-index-keyptrn-on)
    (progn
      (if regexp
	nil
	(setq regexp (read-string "Save regexp: " key)))
      (if (string-equal regexp "")
	(error "Can't take empty string as index search pattern.  Abort."))
      (let* ((kf (read-string "Index key file: "
			     (or tex-index-keyfile default-directory)))
	     (buf (find-file-noselect kf))
	     (kfnd (file-name-nondirectory kf)))
	(if (save-excursion
	      (set-buffer buf)
	      (= (buffer-size) 0))
	  (progn
	    (kill-buffer buf)
	    (setq buf (get-buffer kfnd))
	    (if buf
	      (setq tex-index-keyfile kfnd)
	      (setq buf (find-file-noselect kf))
	      (setq tex-index-keyfile kf)))
	  (setq tex-index-keyfile kf))
	(tex-index-sort key regexp buf kfnd)))))
    
(defun tex-index-sort (key regexp buf fn)
  "Sort and insert the \[KEY, REGEXP\] pair in file FN bound to buffer BUF." 
  (save-excursion
    (set-buffer buf)
    (goto-char 1)
    (let* ((l (length regexp))
	   (pair (concat "[" key ", "
			 (if (< l tex-index-limit)
			   regexp
			   (concat (substring regexp 0 tex-index-limit) "..."))
			 "]"))
	   pos key0 regexp0)
      (message "Saving %s in \"%s\"..." pair fn)
      (catch 'sorted
	(while t
	  (setq pos (point))
	  (if (setq key0 (tex-index-read buf))
	    (if (setq regexp0 (tex-index-read buf))  ; skip the matching regexp
	      (cond ((string-lessp key key0)
		     (goto-char pos)
		     (tex-index-save-pair key regexp)
		     (throw 'sorted t))
		    ((string-equal key key0)
		     (if (string-equal regexp regexp0)
		       nil
		       (setq regexp0
			     (if (< l tex-index-limit)
			       regexp0
			       (concat (substring regexp0 0 tex-index-limit) 
				       "...")))
		       (if (y-or-n-p (concat "Overwrite [" key0 ", " regexp0 "]? "))
			 (progn
			   (delete-region pos (point))
			   (tex-index-save-pair key regexp)
			   (throw 'sorted t))
			 (goto-char pos)
			 (tex-index-save-pair key regexp)
			 (throw 'sorted t)))))
	      (error "Key %s doesn't have a matching regexp in \"%s\"...abort" key fn))
            (tex-index-save-pair key regexp)
	    (throw 'sorted t))))
      (message "Saving %s in \"%s\"...done" pair fn))))

(defun tex-index-save-pair (key regexp)
  (insert "\n\"" key "\"\n\"" regexp "\"\n")
  (let ((pt (point)))
    (previous-line 1)
    (beginning-of-line)
    (catch 'ok
      (while t
	(if (search-forward "\\" pt t)
	  (if (looking-at "[0-9|()`'bBwWsS]")
	    (insert ?\\))
	  (throw 'ok t))))))


;;=========================================
;; Basic Insert Function (used everywhere)	
;;=========================================

(defun tex-index-insert (key &optional regexp)
  (if (string-equal key "")
  (error "Can't take empty string as index key.  Abort."))
  (insert tex-index-command-prefix
	  (tex-index-get-level)
	  key
	  (tex-index-get-actual)
	  (tex-index-get-encap)
	  tex-index-command-suffix)
  (tex-index-save key regexp))
    
(defun tex-index-goto-right ()
  (if (looking-at "[ \t\n]")
    nil
    (skip-chars-forward "^ ^\t^\n^\\")))


;;====================
;; Low-level Indexing
;;====================

(defun tex-index-word (&optional n)
  "Copy the previous word in \\index{...}.  With positive prefix
argument N, do it for previous N words.  If N is negative, it is
converted to 1 implicitly.  The user will be prompted to enter index
prefix (cf. tex-index-level)."
  (interactive "P")
  (let* ((n (if n (max n 1) 1))
	 (start (save-excursion
		  (backward-word n)
		  (point)))
	 (key (buffer-substring start (point))))
    (tex-index-insert key)))
	
(defun tex-index-region (start end)
  "Copy the content of region in \\index{...}.
The user will be prompted to enter index prefix (cf. tex-index-level)."
  (interactive "r")
  (if (= (point) end)
    (tex-index-insert (buffer-substring start end))
    (save-excursion
      (goto-char end)
      (tex-index-insert (buffer-substring start end)))))


;;================
;; Filed Indexing
;;================

(defun tex-index-read (buf)
  "Read and return the next Lisp expression in BUF, skipping blank lines until
end-of-buffer and assuming BUF has already been set already.
Return nil if nothing to read.  This avoids the unpleasant
abort on \"End of file during parsing\" in regular \(read buf\)."
  (catch 'stop
    (while t
      (if (or (eobp)
	      (not (looking-at " *$")))
	(throw 'stop t)
	(next-line 1)
	(beginning-of-line))))
  (if (eobp)
    nil
    (read buf)))

(defun tex-index-file (kf &optional lst)
  "Index each \[key, pattern\] pair saved in file KF.
If the optional LST is non-nil, process each pair for the entire
document, else process only the current buffer."
  (let* ((func (if lst 'tex-index-dsingle 'tex-index-bsingle))
	 (buf (find-file-noselect kf))
	 (kfnd (file-name-nondirectory kf))
	 (msg (concat "Indexing "
		      (if lst
			"entire document"
			"current buffer")
		      " based on key file \"" kfnd "\"..."))
	 key regexp)
    (message msg)
    (if (save-excursion
	  (set-buffer buf)
	  (= (buffer-size) 0))
      (progn
	(kill-buffer buf)
	(setq buf (get-buffer kfnd))
        (if (and buf
		 (save-excusrion
		   (set-buffer buf)
		   (> (buffer-size) 0)))
	  (setq tex-index-keyfile kfnd)
	  (error "%sabort (file is empty)" msg)))
      (setq tex-index-keyfile kf))
    (catch 'done
      (save-excursion
	(set-buffer buf)
	(goto-char 1))
      (while t
	(save-excursion
	  (set-buffer buf)
	  (if (not (setq key (tex-index-read buf)))
	    (throw 'done t))
	  (if (not (setq regexp (tex-index-read buf)))
	    (throw 'done nil)))
	(funcall func key regexp lst)))
    (message "%sdone" msg)))


;;=====================
;; High-level Indexing
;;=====================

(defun tex-index-document (&optional kf &optional key)
  "Process single or multiple \[key, pattern\] pairs for the entire document.
With C-u as prefix, a file of \[key, pattern\] pairs is expected.
Otherwise, prompt for just a single pair."
  (interactive (if current-prefix-arg
		   (list (read-string
			  "Index key file: "
			  (or tex-index-keyfile default-directory)))
		 (list nil (read-string "Index key: "))))
  (if kf
      (progn
	(tex-check-master-file)		; in TeX-misc.el
	(tex-get-include-files t)	; in TeX-misc.el
	(tex-index-file kf tex-include-files))
    (if (string-equal key "")
	(error "Can't take empty string as index key.  Abort."))
    (let ((regexp (read-string "Regexp: " key)))
      (if (string-equal regexp "")
	  (error "Can't take empty string as index search pattern.  Abort."))
      (tex-check-master-file)		; in TeX-misc.el
      (tex-get-include-files t)		; in TeX-misc.el
      (tex-index-dsingle key regexp tex-include-files))))
	      
(defun tex-index-dsingle (key regexp lst)
  "Enter \\index{KEY} at each instance of REGEXP for each file in LST, subject
to confirmation from the user."
  (let ((asknot nil)
	(quiet nil)
	(finish 'finish)
	(msg (concat "Indexing `" key "' for document"))
	kf kfnd ans start)
    (catch 'finish
      (while lst
	(save-excursion
	  (set-buffer (find-file-noselect (setq kf (car lst))))
	  (setq kfnd (file-name-nondirectory kf))
	  (setq lst (append (tex-get-input-files msg kfnd) (cdr lst)))
	  (if (or asknot
		  (tex-confirm (concat "Indexing `" key "' for " kfnd)))
	    (progn
	      (find-file kfnd)
	      (message "%s, doing %s..." msg kfnd)
	      (save-excursion
		(goto-char 1)
		(if (tex-index-search regexp)
		  (progn
		    (setq start (match-beginning 0))
		    (tex-index-goto-right)
		    (tex-index-get-cmd)
		    (message "%s, doing %s...done" msg kfnd))
		  (message "%s, doing %s...failed (pattern not found)" msg kfnd))))))))
    (message "%s...done" msg)))

(defun tex-index-buffer (&optional kf &optional key)
  "Process single or multiple \[key, pattern\] pairs for the current buffer.
With C-u as prefix, a file of \[key, pattern\] pairs is expected.
Otherwise, prompt for just a single pair."
  (interactive (if current-prefix-arg
	         (list (read-string "Index key file: " 
		         (or tex-index-keyfile default-directory)))
		 (list nil (read-string "Index key: "))))
  (if kf
    (tex-index-file kf)
    (if (string-equal key "")
      (error "Can't take empty string as index key.  Abort."))
    (let ((regexp (read-string "Regexp: " key)))
      (if (string-equal regexp "")
	(error "Can't take empty string as index search pattern.  Abort."))
      (tex-index-bsingle key regexp))))
	 
(defun tex-index-bsingle (key regexp &optional lst)
  "Enter \\index{KEY} at each instance of REGEXP for the current buffer.
LST should always be nil \(it's included for the convenience of the caller."
  (save-excursion
    (goto-char 1)
    (let ((asknot nil)
	  (quiet nil)
	  (finish 'done)
	  start)
      (if (tex-index-search regexp)
	(progn
	  (setq start (match-beginning 0))
	  (tex-index-goto-right)
	  (tex-index-get-cmd)
	  (message "Indexing `%s' for current buffer...done" key))
        (message "Indexing `%s' for current buffer...failed (pattern not found)" key)))))


;;===============================
;; Command Handling (big switch)
;;===============================

(defun tex-index-get-cmd ()
  "Give a menu of options and take appropriate action upon receving the
answer."
  (let* ((msg (concat "Insert \\index{" key "}? [SPC=y  DEL=n  LFD=p  ? for more options]"))
	 (KEY key)
	 (local KEY)
	 (QUIET quiet)
	 (next (if QUIET nil t))
	 (cmd (if QUIET ?y nil))
	 (nmsg (concat "Can't find next instance to index `" key "', done? "))
	 (pmsg (concat "Can't find previous instance to index `" key "', done? ")))
    (catch 'done
      (while t
	(if (and next (not QUIET))
	  (progn
	    (message msg)
	    (setq local KEY)
	    (setq cmd (read-char)))
	  (setq next t))
	(let ((tex-index-actual-on (if QUIET nil))
	      (tex-index-level-on (if QUIET nil))
	      (tex-index-keyptrn-on (if QUIET nil)))
	  (cond
	    ((or (= cmd ? ) (= cmd ?y))
	     (tex-index-insert local regexp)
	     (if (tex-index-scroll-down QUIET)
	       (throw 'done t)))
	    ((or (= cmd ?\177) (= cmd ?n))
	     (if (tex-index-scroll-down QUIET)
	       (throw 'done t)))
	    ((or (= cmd ?\n) (= cmd ?p))
	     (if (tex-index-scroll-up)
	       (throw 'done t)))
	    ((or (= cmd ?\r) (= cmd ?m))
	     (tex-index-chmod-temporary))
	    ((= cmd ?M)
	     (tex-index-chmod))
	    ((= cmd ?k)
	     (setq local (read-string "New index key (single instance): "))
	     (message "Insert \\index{%s}? (SPC/y, DEL/n, LFD/p, ? for more options)" local)
	     (setq cmd (read-char))
	     (setq next nil))
	    ((= cmd ?\^k)
	     (setq KEY (read-string "New index key (remaining buffer): "))
	     (message "Insert \\index{%s}? (SPC/y, DEL/n, LFD/p, ? for more options)" KEY)
	     (setq cmd (read-char))
	     (setq next nil))
	    ((= cmd ?K)
	     (setq KEY (setq key (read-string "New index key (remaining document): ")))
	     (message "Insert \\index{%s}? (SPC/y, DEL/n, LFD/p, ? for more options)" key)
	     (setq cmd (read-char))
	     (setq next nil))
	    ((= cmd ?@)
	     (if (y-or-n-p (concat "Quietly insert \\index{" local "} for buffer, are you sure? "))
	       (progn
		 (setq KEY local)
		 (setq cmd ?y)
		 (setq QUIET t))))
	    ((= cmd ?!)
	     (if (y-or-n-p (concat "Quietly insert \\index{" local "} for document, are you sure? "))
	       (progn
		 (setq key local)
		 (setq cmd ?y)
		 (setq asknot (setq quiet (setq QUIET t))))))
	    ((= cmd ?\e)
	     (throw 'done t))
	    ((= cmd ?\^c)
	     (throw finish t))
	    ((= cmd ?\^r)
	     (message "Entering recursive edit...(return by ESC C-c)")
	     (recursive-edit))
	    ((= cmd ??)
	     (save-window-excursion
	       (tex-index-help)
	       (setq next nil)
	       (message msg)
	       (setq cmd (read-char))))
	    (t
	     (ding)
	     (message "%s...WHAT?" msg)
	     (sit-for 1))))))))

(defun tex-index-chmod-temporary ()
  "Confirm insertion with single instance mode change."
  (let* ((mode (read-string "Confirm with mode (prefix-actual-keyptrn, 3-bit binary): "))
	 (m1 (substring mode 0 1))
	 (m2 (substring mode 1 2))
	 (m3 (substring mode 2 3))
	 (tex-index-actual-on (if m1 
				 (/= (string-to-char m1) ?0) 
				 tex-index-actual-on))
	 (tex-index-level-on (if m2 
				(/= (string-to-char m2) ?0)
				tex-index-level-on))
	 (tex-index-keyptrn-on (if m3
				 (/= (string-to-char m3) ?0)
				 tex-index-keyptrn-on)))
    (tex-index-insert local regexp))
  (if (tex-index-scroll-down QUIET)
    (throw 'done t)))


;;========================
;; Scrolling and Skipping
;;========================

(defun tex-index-scroll-down (&optional quiet)
  "Search forward for the next instance of REGEXP.
Prompt for exit confirmation, if not found.  Wrap around if denied."
   (if (tex-index-search regexp)
     (progn
       (setq start (match-beginning 0))
       (tex-index-goto-right)
       nil)
     (if (or quiet (y-or-n-p nmsg))
       t
       (message "Wrapping around...")
       (sit-for 1)
       (goto-char 1)
       (tex-index-scroll-down quiet))))

(defun tex-index-scroll-up ()
  "Search backward for the previous instance of REGEXP.
Prompt for exit confirmation, if not found.  Wrap around if denied."
  (if (save-excursion
	(goto-char start)
	(tex-index-search regexp t))
    (progn
      (setq start (match-beginning 0))
      (goto-char (match-end 0))
      (tex-index-goto-right)
      nil)
    (if (y-or-n-p pmsg)
      t
      (message "Wrapping around...")
      (sit-for 1)
      (setq start (point-max))
      (tex-index-scroll-up))))

(defun tex-index-search (regexp &optional backward)
  "Locate the next instance of REGEXP.  Ignore one that is in the argument
list of \\index{...}.  if the optional BACKWARD is non-nil, search backward."
  (let ((func (if backward 're-search-backward 're-search-forward))
	(here (point))
	(case-fold-search t)	
	pos bol)
    (catch 'found
      (while t
	(if (funcall func regexp nil t)
	  (setq pos (point))
	  (goto-char here)
	  (throw 'found nil))
	(setq bol (save-excursion
		    (beginning-of-line)
		    (point)))
	(if (not (re-search-backward tex-index-entries bol t))
	  (throw 'found t))
	(forward-word 1)
	(forward-sexp 1)
	(if (< (point) pos)
	  (progn
	    (goto-char pos)
	    (throw 'found t)))
      	(goto-char pos)))))


;;===================
;; Menu Help Message	    
;;===================

(defun tex-index-help ()
  (pop-to-buffer tex-index-buff)
  (if (> (buffer-size) 0)
    nil
    (insert
"SPC/y -- Confirm index entry insertion and advance to next instance, if any.
DEL/n -- Ignore current instance and advance to the next, if any.
LFD/p -- Ignore current instance and go back to the previous, if any. 
RET/m -- Confirm with mode change (actual-prefix-keyptrn as a 3-bit binary).
    M -- Permanent change mode (actual-prefix-keyptrn as a 3-bit binary).
    k -- Change single instance of index key.
  C-k -- Change index key for remaining instances in buffer.
    K -- Change index key for remaining instances in document.
    @ -- Quietly insert \\index{key} for remaining buffer.
    ! -- Quietly insert \\index{key} for remaining document.
  ESC -- Quit working on current buffer.  Try next file in document, if any.
  C-c -- Quit working on current key.  Try next key in key file, if any.
  C-r -- Enter recursive edit.  Return by ESC C-c.
    ? -- This help message."))
  (enlarge-window (- 15 (window-height)))
  (goto-char 1)
  (other-window 1))


;;========================
;; Interface to makeindex
;;========================

(defun tex-index-make ()
  "Make index for the TeX/AmSTeX/LaTeX document rooted at current file."
  (interactive)
  (tex-check-master-file)
  (sit-for 1)      ; so that the master file will be displayed immediately
  (let* ((buff (file-name-nondirectory (buffer-file-name)))
	 (base (substring buff 0 (string-match "\\." buff)))
	 (idx (concat base ".idx"))
	 (doc-type (tex-check-document-type))
	 (type (cond
		 ((string-equal doc-type "LaTeX") "")
		 ((string-equal doc-type "TeX") "t")
		 ((string-equal doc-type "AmSTeX") "a")
		 (t (error "Don't know how to make index for document of type %s" doc-type))))
	 (flag (progn
		 (message "%s index formatting option: e=entire doc, s=separate index, else=nothing" doc-type)
		 (let ((ch (read-char)))
		   (cond ((= ch ?e) (concat "-e" type))
			 ((= ch ?s) (concat "-s" type))
			 (t (if (string-equal type "")
			      ""
			      (concat "-" type))))))))
    (if (file-exists-p idx)
      (tex-execute (concat tex-index-processor " " flag) base)
      (ding)
      (message "\"%s\" doesn't exist...run formatter first." idx))))


;;==============================================
;; Indexing Author Names in a Bibliography FIle
;;==============================================

(defun tex-index-authors ()
  "Process each author's name and insert \\index{AUTHOR} in the specified
bibliography file BBL.  Prompt each author's name appearing in the \\bibitem
entry \(but ignoring any one that's in \\index{...}\) for confirmation.
The prompted name will be last name first, followed by a comma, and then
the other parts of the name.  Names like ``Michael Van De Vanter'' will be
regarded as ``Vanter, Michael Van De'' which is of course wrong.
However, this can be modified before the final confirmation is made
\(i.e. typing RET\)."
  (interactive)
  (let* ((master-file (tex-check-master-file))
	 (fn (or master-file (buffer-file-name)))
	 (bn (concat (substring fn 0 (string-match "\\." fn)) ".bbl"))
	 (bbl (read-string "Indexing author names in bibliography file " bn))
	 (asknot nil))
    (if (file-exists-p bbl)
      nil
      (ding)
      (message "File \"%s\" not found...try again" bbl)
      (sit-for 1)
      (tex-index-authors))
    (save-window-excursion
      (if (string-equal (buffer-file-name) bbl)
	nil
	(find-file-other-window bbl)
	(tex-mode t))
      (goto-char 1)
      (catch 'finish
	(while (re-search-forward "\\\\bibitem" nil t)
	  (next-line 1)
	  (beginning-of-line)
	  (if (looking-at "{\\\\it\\| *{")
	    nil				;; looking at a title, not author
	    (if (or asknot
		    (tex-confirm "Ok to process current line"))
	      (tex-index-author-name)))))
      (message "Indexing author names...done"))))
    
(defun tex-index-author-name ()
  (save-excursion
    (let ((pos1 (point))		;; beginning of an author field
	  pos2				;; end of author field
	  (=done= nil)
	  (=index= nil)
	  name name1)
      (catch 'loop1
	(while t
	  (goto-char pos1)
	  (catch 'ok			;; search for last name
	    (while t
	      (tex-index-author-stop)
	      (setq pos2 (point))
	      (backward-sexp 1)
	      (setq name (tex-index-remove-lfd
			   (buffer-substring (point) pos2)))
	      (skip-chars-backward " ,~\n")
	      (setq name1 (tex-index-remove-lfd 
			    (buffer-substring pos1 (point))))
	      (if (or (string-equal name1 " ") (string-equal name1 ""))
		(goto-char pos2)
		(throw 'ok t))))
	  (setq name (tex-index-author-read (concat name ", " name1)))
	  (goto-char pos2)
	  (if =index=
	    (progn
	      (forward-word 1)
	      (forward-sexp 1)
	      (delete-region pos2 (point))))
	  (tex-index-insert name)
	  (if (> (current-column) 72)
	    (save-excursion
	      (goto-char pos1)
	      (insert ?\n)))
	  (cond
	    ((looking-at "[ ,] *and")
	     (forward-word 1))
	    ((looking-at "[ ,] *editor")
	     (setq =done= t)))
	  (if =done= (throw 'loop1 t))
	  (skip-chars-forward " ,\.~\n")
	  (setq pos1 (point)))))))

(defun tex-index-author-stop ()
  (if (re-search-forward
        ",\\|\\.$\\|\\band\\b\\|\\bet al\\.\\|\\bet~al\\.\\|\\\\index" nil t)
    (let ((pt (point)))
      (goto-char (- pt 3))
      (if (looking-at "^\\w\.\\| \\w\.")  ;; single letter, must be initial
	(progn
	  (goto-char pt)
	  (delete-char 1)		;; get rid of LFD
	  (tex-index-author-stop))
	(goto-char pt)
	(cond
	  ((string-equal (buffer-substring (- pt 3) pt) "and")	;; 2 authors
	   (backward-word 1))
	  ((let ((st5 (buffer-substring (- pt 5) pt)))		;; et al.
	     (or (string-equal st5 "et al")
		 (string-equal st5 "et~al")))
	   (setq =done= t)
	   (backward-word 2))
	  ((string-equal (buffer-substring (- pt 6) pt) "\\index")
	   (backward-word 1)
	   (backward-char 1)))
	(skip-chars-backward " ,\.~\n")
	(if (not =done=) (setq =done= (= (following-char) ?.)))
	(setq =index= (looking-at "\\\\index"))))
    (ding)
    (if (y-or-n-p (concat "Line "
			  (count-lines 1 pos1)
			  " contains invalid author field, continue? "))
      (throw 'loop1 nil)
      (error "Abort."))))

(defun tex-index-remove-lfd (in)
  (let ((p "")
	(out "")
	c)
    (while (not (string-equal in ""))
      (setq c (substring in 0 1))
      (if (string-equal c "\n")
	(setq c " "))
      (if (and (string-equal p " ") (string-equal c " "))
	nil
	(setq out (concat out c))
	(setq p c))
      (setq in (substring in 1)))
    out))

(defun tex-index-author-read (author)
  (let (name)
    (catch 'loop2
      (while t
	(setq name (read-string "Author name: " author))
	(if (string-equal name "")
	  (progn
	    (ding)
	    (message "Can't take an empty author name...try again"))
	  (throw 'loop2 name))))))

