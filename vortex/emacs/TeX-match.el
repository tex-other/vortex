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
;; $Header: /home/yew/yew4/vortex/newdist/emacs/RCS/TeX-match.el,v 1.14 1993/09/16 02:32:14 munson Exp $
;; This file, "tex-match.el", is a subsystem of "tex-mode.el".
;; It contains assorted functions for semi-automatic (zone/word) delimiter 
;; matching and LaTeX environment delimiter matching.
;;

;; ========
;; Bouncing
;; ========

(defun tex-bounce-backward ()
  "Bounce backward to check the open paren."
  (interactive)
  (save-excursion
    (backward-list 1)
    (tex-bouncing-point 1)))

(defun tex-bounce-forward ()
  "Bounce forward to check the close paren."
  (interactive)
  (save-excursion
    (forward-list 1)
    (backward-char)
    (tex-bouncing-point 1)))


;; =============
;; Zone Matching
;; =============

(defun tex-toggle-boundary-check ()
  "Toggle the flag tex-boundary-check-on"
  (interactive)
  (if tex-boundary-check-on
    (progn
      (setq tex-boundary-check-on nil)
      (message "Boundary checking in delemiter matching disabled."))
    (setq tex-boundary-check-on t)
    (message "Boundary checking in delemiter matching enabled.")))


;; =============
;; Zone Matching
;; =============

(defconst tex-blank ? )
(defconst tex-tab ?\t )
(defconst tex-space " \\|\t")

(defun tex-zone-open ()
  "Open a TeX zone."
  (interactive)
  (setq tex-zone-marker-stack
    (cons (set-marker (make-marker) (point)) tex-zone-marker-stack))
  (setq tex-zone-marker-count (1+ tex-zone-marker-count))
  (message "TeX zone #%d opened." tex-zone-marker-count))

(defun tex-zone-close ()
  "Close a TeX zone explicitly."
  (interactive)
  (if tex-zone-marker-stack
    (let ((mark (car tex-zone-marker-stack)))
      (message "TeX zone #%d closed." tex-zone-marker-count)
      (setq tex-zone-marker-stack (cdr tex-zone-marker-stack))
      (setq tex-zone-marker-count (1- tex-zone-marker-count))
      (if (markerp mark)
	(marker-position mark)
	(error "TeX zone marker stack contains non-markers...aborted (I'm confused)")))
    (error "TeX zone marker stack is empty")))

(defun tex-zone-inspect (&optional n)
  "Pek the Nth TeX zone marker."
  (interactive "p")
  (if tex-zone-marker-stack
    (let* ((n (if (and n (> n 0)) n 1))
	   m mark)
      (if (> n tex-zone-marker-count)
	(error "TeX zone marker stack is only %d level deep" tex-zone-marker-count))
      (setq mark (nth (1- n) tex-zone-marker-stack))
      (if (markerp mark)
	(progn
	  (message "TeX zone #%d (total %d)" (- tex-zone-marker-count (1- n)) tex-zone-marker-count)
	  (save-excursion 
	    (tex-bouncing-point (goto-char (marker-position mark)))))
	(error "TeX zone marker stack contains non-markers...aborted (I'm confused)")))
    (error "TeX zone marker stack is empty")))

(defun tex-zone-start ()
  (if (or (bolp) (= (preceding-char) tex-blank) (= (preceding-char) tex-tab))
    nil 
    (if (looking-at tex-space)
      (progn
        (re-search-forward "[^ \t]")
        (backward-char 1))
      (if tex-boundary-check-on 
	(let* ((lw (save-excursion
		     (re-search-backward " \\|\t\\|^")
		     (if (bolp)
		       (point)
		       (1+ (point)))))
	       (rw (save-excursion
		     (re-search-forward " \\|\t\\|$")
		     (if (or (= (preceding-char) tex-blank) (= (preceding-char) tex-tab))
		       (1- (point))
		       (point))))
	       (word (concat (buffer-substring lw (point)) "|" (buffer-substring (point) rw)))
	       char)
	  (message "Confirm position `%s' (l, r, else=yes)" word)
	  (setq char (read-char))
	  (cond
	    ((= char ?l) (goto-char lw))
	    ((= char ?r)
	     (goto-char rw)
	     (re-search-forward "[^ \t]")
	     (backward-char 1))
	    (t t)))))))

(defun tex-zone-end ()
  (if (or (eolp) (looking-at tex-space))
    (progn
      (re-search-backward "[^ \t]")
      (forward-char 1))
    (if tex-boundary-check-on
      (let* ((lw (save-excursion
		   (re-search-backward " \\|\t\\|^")
		   (if (bolp)
		     (point)
		     (1+ (point)))))
	     (rw (save-excursion
		   (re-search-forward " \\|\t\\|$")
		   (if (or (= (preceding-char) tex-blank) (= (preceding-char) tex-tab))
		     (1- (point))
		     (point))))
	     (word (concat (buffer-substring lw (point)) "|" (buffer-substring (point) rw)))
	     char)
	(message "Confirm position `%s' (l, r, else=yes)" word)
	(setq char (read-char))
	(cond
	  ((= char ?r) (goto-char rw))
	  ((= char ?l)
	   (goto-char lw)	 	
	   (re-search-backward "[^ \t]")
	   (forward-char 1))
	  (t t))))))

(defun tex-zone (l-sym r-sym)
  "Puts L-SYM and R-SYM around topmost zone marker and point."
  (let ((start (tex-zone-close))
	(end (point))
	(swap nil)
	(ll (length l-sym))
	(rl (length r-sym))
	(lr 0)
	(rr 0))
    (if (> start end)
      (progn
	(setq swap start)
	(setq start end)
	(setq end swap)))
    (goto-char start)
    (tex-zone-start)
    (setq lr (point))
    (insert l-sym)
    (goto-char (+ ll end))
    (tex-zone-end)
    (insert r-sym)
    (setq rr (point))
    (if swap
      (progn
        (goto-char lr)
        (save-excursion
	  (goto-char rr)
	  (tex-bouncing-point rl)))
      (save-excursion
	(goto-char lr)
	(tex-bouncing-point ll)))))

(defun tex-zone-math ()
  "Embrace the innermost TeX zone with a pair of $'s.  
Position confirmation is required if either end touches any non-blank symbol."
  (interactive)
  (tex-zone "$" "$"))

(defun tex-zone-display-math ()
  "Embrace the innermost TeX zone with a pair of $$'s.  
Position confirmation is required if either end touches any non-blank symbol."
  (interactive)
  (tex-zone "$$" "$$"))

(defun tex-zone-single-quote ()
  "Embrace the innermost TeX zone with a left single quote (`) 
and a right single quote (').
Position confirmation is required if either end touches any non-blank symbol."
  (interactive)
  (tex-zone "`" "'"))

(defun tex-zone-double-quote ()
  "Embrace the innermost TeX zone with a left double quote (``) 
and a right double quote ('').
Position confirmation is required if either end touches any non-blank symbol."
  (interactive)
  (tex-zone "``" "''"))

(defun tex-zone-centerline ()
  "Embrace the innermost TeX zone by \\centerline{...} with ... being the text 
between zone marker and point.
Position confirmation is required if either end touches any non-blank symbol."
  (interactive)
  (tex-zone "\\centerline{" "}"))

(defun tex-zone-hbox ()
  "Embrace the innermost TeX zone by \\hbox{...} with ... being the text 
between zone marker and point.
Position confirmation is required if either end touches any non-blank symbol."
  (interactive)
  (tex-zone "\\hbox{" "}"))

(defun tex-zone-vbox ()
  "Embrace the innermost TeX zone by \\vbox{...} with ... being the text 
between zone marker and point.
Position confirmation is required if either end touches any non-blank symbol."
  (interactive)
  (tex-zone "\\vbox{" "}"))

(defun tex-zone-bf ()
  "Embrace the innermost TeX zone by {\\bf ...} with ... being the text 
between zone marker and point.
Position confirmation is required if either end touches any non-blank symbol."
  (interactive)
  (tex-zone "{\\bf " "}"))

(defun tex-zone-it ()
  "Embrace the innermost TeX zone by {\\it ...\\/} with ... being the text 
between zone marker and point.
Position confirmation is required if either end touches any non-blank symbol."
  (interactive)
  (tex-zone "{\\it " "\\/}"))

(defun tex-zone-rm ()
  "Embrace the innermost TeX zone by {\\rm ...} with ... being the text 
between zone marker and point.
Position confirmation is required if either end touches any non-blank symbol."
  (interactive)
  (tex-zone "{\\rm " "}"))

(defun tex-zone-sl ()
  "Embrace the innermost TeX zone by {\\sl ...\\/} with ... being the text 
between zone marker and point.
Position confirmation is required if either end touches any non-blank symbol."
  (interactive)
  (tex-zone "{\\sl " "\\/}"))

(defun tex-zone-tt ()
  "Embrace the innermost TeX zone by {\\tt ...} with ... being the text 
between zone marker and point.
Position confirmation is required if either end touches any non-blank symbol."
  (interactive)
  (tex-zone "{\\tt " "}"))


(defun tex-zone-em ()
  "Embrace the innermost TeX zone by {\\em ...} with ... being the text 
between zone marker and point.
Position confirmation is required if either end touches any non-blank symbol."
  (interactive)
  (tex-zone "{\\em " "}"))

;; =============
;; Word Matching
;; =============

(defun tex-word (l-sym r-sym &optional n)
  "Puts L-SYm and R-SYM around next N (default 1) words; previous N words
if N is negative."
  (let ((n (if n n 1))
	(ll (length l-sym))
	(rl (length r-sym))
	(lr 0)
	(rr 0))
    (if (> n 0)
      (progn
        (tex-zone-start)
	(setq lr (point))
	(insert l-sym)
	(forward-word n)
	(if (looking-at " \\|$\\|\t")
	  nil
	  (re-search-forward " \\|$\\|\t")
	  (if (or (= (preceding-char) tex-blank) (= (preceding-char) tex-tab))
	    (backward-char 1)))
	(insert r-sym)
	(setq rr (point))
	(goto-char lr)
	(save-excursion
	  (goto-char rr)
	  (tex-bouncing-point rl)))
      (tex-zone-end)
      (insert-string r-sym)
      (setq rr (+ ll (point)))
      (forward-word n)   ;; forward instead of backward because n is negative
      (if (or (bolp) (= (preceding-char) tex-blank) (= (preceding-char) tex-tab))
	nil
	(re-search-backward " \\|^\\|\t")
	(if (or (= (following-char) tex-blank) (= (following-char) tex-tab))
	  (forward-char 1)))
      (setq lr (point))
      (insert-string l-sym)
      (goto-char rr)
      (save-excursion
	(goto-char lr)
	(tex-bouncing-point ll)))))


;; ========================
;; Word Matching (Backward)
;; ========================

(defun tex-word-math (&optional n)
  "Embrace the previous word with a pair of $'s.
With positive prefix argument N, embrace previous N words;
or with negative prefix argument N, embrace next N words.
Position confirmation is required if either end touches any non-blank symbol."
  (interactive "P")
  (tex-word "$" "$" (if n (- n) -1)))

(defun tex-word-display-math (&optional n)
  "Embrace the previous word with a pair of $$'s.
With positive prefix argument N, embrace previous N words;
or with negative prefix argument N, embrace next N words.
Position confirmation is required if either end touches any non-blank symbol."
  (interactive "P")
  (tex-word "$$" "$$" (if n (- n) -1)))

(defun tex-word-single-quote (&optional n)
  "Embrace the previous word with a pair of left and right single quotes (|`...'|).
With positive prefix argument N, embrace previous N words;
or with negative prefix argument N, embrace next N words.
Position confirmation is required if either end touches any non-blank symbol."
  (interactive "P")
  (tex-word "`" "'" (if n (- n) -1)))

(defun tex-word-double-quote (&optional n)
  "Embrace the previous word with a pair of left and right double quotes (|``...''|).
With positive prefix argument N, embrace previous N words;
or with negative prefix argument N, embrace next N words.
Position confirmation is required if either end touches any non-blank symbol."
  (interactive "P")
  (tex-word "``" "''" (if n (- n) -1)))

(defun tex-word-centerline (&optional n)
  "Embrace the previous word by \\centerline{...} with ... being the word.
With positive prefix argument N, embrace previous N words;
or with negative prefix argument N, embrace next N words.
Position confirmation is required if either end touches any non-blank symbol."
  (interactive "P")
  (tex-word "\\centerline{" "}" (if n (- n) -1)))

(defun tex-word-hbox (&optional n)
  "Embrace the previous word by \\hbox{...} with ... being the word.
With positive prefix argument N, embrace previous N words;
or with negative prefix argument N, embrace next N words.
Position confirmation is required if either end touches any non-blank symbol."
  (interactive "P")
  (tex-word "\\hbox{" "}" (if n (- n) -1)))

(defun tex-word-vbox (&optional n)
  "Embrace the previous word by \\vbox{...} with ... being the word.
With positive prefix argument N, embrace previous N words;
or with negative prefix argument N, embrace next N words.
Position confirmation is required if either end touches any non-blank symbol."
  (interactive "P")
  (tex-word "\\vbox{" "}" (if n (- n) -1)))

(defun tex-word-bf (&optional n)
  "Embrace the previous word by {\\bf ...} with ... being the word.
With positive prefix argument N, embrace previous N words;
or with negative prefix argument N, embrace next N words.
Position confirmation is required if either end touches any non-blank symbol."
  (interactive "P")
  (tex-word "{\\bf " "}" (if n (- n) -1)))

(defun tex-word-it (&optional n)
  "Embrace the previous word by {\\it ...\\/} with ... being the word.
With positive prefix argument N, embrace previous N words;
or with negative prefix argument N, embrace next N words.
Position confirmation is required if either end touches any non-blank symbol."
  (interactive "P")
  (tex-word "{\\it " "\\/}" (if n (- n) -1)))

(defun tex-word-rm (&optional n)
  "Embrace the previous word by {\\rm ...} with ... being the word.
With positive prefix argument N, embrace previous N words;
or with negative prefix argument N, embrace next N words.
Position confirmation is required if either end touches any non-blank symbol."
  (interactive "P")
  (tex-word "{\\rm " "}" (if n (- n) -1)))

(defun tex-word-sl (&optional n)
  "Embrace the previous word by {\\sl ...\\/} with ... being the word.
With positive prefix argument N, embrace previous N words;
or with negative prefix argument N, embrace next N words.
Position confirmation is required if either end touches any non-blank symbol."
  (interactive "P")
  (tex-word "{\\sl " "\\/}" (if n (- n) -1)))

(defun tex-word-tt (&optional n)
  "Embrace the previous word by {\\tt ...} with ... being the word.
With positive prefix argument N, embrace previous N words;
or with negative prefix argument N, embrace next N words.
Position confirmation is required if either end touches any non-blank symbol."
  (interactive "P")
  (tex-word "{\\tt " "}" (if n (- n) -1)))

(defun tex-word-em (&optional n)
  "Embrace the previous word by {\\em ...} with ... being the word.
With positive prefix argument N, embrace previous N words;
or with negative prefix argument N, embrace next N words.
Position confirmation is required if either end touches any non-blank symbol."
  (interactive "P")
  (tex-word "{\\em " "}" (if n (- n) -1)))


;; =======================
;; Word Matching (Forward)
;; =======================

(defun tex-word-forward-math (&optional n)
  "Embrace the previous word with a pair of $'s.  
With positive prefix argument N, embrace next N words;
or with negative prefix argument N, embrace previous N words.
Position confirmation is required if either end touches any non-blank symbol."
  (interactive "P")
  (tex-word "$" "$" n))

(defun tex-word-forward-display-math (&optional n)
  "Embrace the next word with a pair of $$'s.  
With positive prefix argument N, embrace next N words;
or with negative prefix argument N, embrace previous N words.
Position confirmation is required if either end touches any non-blank symbol."
  (interactive "P")
  (tex-word "$$" "$$" n))

(defun tex-word-forward-single-quote (&optional n)
  "Embrace the next word with a pair of left and right single quotes (|`...'|).
With positive prefix argument N, embrace next N words;
or with negative prefix argument N, embrace previous N words.
Position confirmation is required if either end touches any non-blank symbol."
  (interactive "P")
  (tex-word "`" "'" n))

(defun tex-word-forward-double-quote (&optional n)
  "Embrace the next word with a pair of left and right double quotes (|``...''|).
With positive prefix argument N, embrace next N words;
or with negative prefix argument N, embrace previous N words.
Position confirmation is required if either end touches any non-blank symbol."
  (interactive "P")
  (tex-word "``" "''" n))

(defun tex-word-forward-centerline (&optional n)
  "Embrace the next word by \\centerline{...} with ... being the word.
With positive prefix argument N, embrace next N words;
or with negative prefix argument N, embrace previous N words.
Position confirmation is required if either end touches any non-blank symbol."
  (interactive "P")
  (tex-word "\\centerline{" "}" n))

(defun tex-word-forward-hbox (&optional n)
  "Embrace the next word by \\hbox{...} with ... being the word.
With positive prefix argument N, embrace next N words;
or with negative prefix argument N, embrace previous N words.
Position confirmation is required if either end touches any non-blank symbol."
  (interactive "P")
  (tex-word "\\hbox{" "}" n))

(defun tex-word-forward-vbox (&optional n)
  "Embrace the next word by \\vbox{...} with ... being the word.
With positive prefix argument N, embrace next N words;
or with negative prefix argument N, embrace previous N words.
Position confirmation is required if either end touches any non-blank symbol."
  (interactive "P")
  (tex-word "\\vbox{" "}" n))

(defun tex-word-forward-bf (&optional n)
  "Embrace the next word by {\\bf ...} with ... being the word.
With positive prefix argument N, embrace next N words;
or with negative prefix argument N, embrace previous N words.
Position confirmation is required if either end touches any non-blank symbol."
  (interactive "P")
  (tex-word "{\\bf " "}" n))

(defun tex-word-forward-it (&optional n)
  "Embrace the next word by {\\it ...\\/} with ... being the word.
With positive prefix argument N, embrace next N words;
or with negative prefix argument N, embrace previous N words.
Position confirmation is required if either end touches any non-blank symbol."
  (interactive "P")
  (tex-word "{\\it " "\\/}" n))

(defun tex-word-forward-rm (&optional n)
  "Embrace the next word by {\\rm ...} with ... being the word.
With positive prefix argument N, embrace next N words;
or with negative prefix argument N, embrace previous N words.
Position confirmation is required if either end touches any non-blank symbol."
  (interactive "P")
  (tex-word "{\\rm " "}" n))

(defun tex-word-forward-sl (&optional n)
  "Embrace the next word by {\\sl ...\\/} with ... being the word.
With positive prefix argument N, embrace next N words;
or with negative prefix argument N, embrace previous N words.
Position confirmation is required if either end touches any non-blank symbol."
  (interactive "P")
  (tex-word "{\\sl " "\\/}" n))

(defun tex-word-forward-tt (&optional n)
  "Embrace the next word by {\\tt ...} with ... being the word.
With positive prefix argument N, embrace next N words;
or with negative prefix argument N, embrace previous N words.
Position confirmation is required if either end touches any non-blank symbol."
  (interactive "P")
  (tex-word "{\\tt " "}" n))

(defun tex-word-forward-em (&optional n)
  "Embrace the next word by {\\em ...} with ... being the word.
With positive prefix argument N, embrace next N words;
or with negative prefix argument N, embrace previous N words.
Position confirmation is required if either end touches any non-blank symbol."
  (interactive "P")
  (tex-word "{\\em " "}" n))


;; ==================
;; LaTeX Environments
;; ==================

(defun tex-latex-open (env arg)
  "Open a new LaTeX environment.  The user will be prompted for ENV and ARG."
  (interactive "sLaTeX \\begin{env}, specify env (RET if none): \nsArguments (RET if none): ")
  (let ((col (current-column))
	(empty-p (string-equal env "")))
    (if empty-p
      (insert "\\begin")
      (insert "\\begin{" env "}")
      (if (string-equal arg "")
	nil
	(insert arg)))
    (newline-and-indent)
    (if empty-p
      (insert "\\end")
      (insert "\\end{" env "}"))
    (beginning-of-line)
    (open-line 1)
    (indent-to (+ col tex-latex-indentation))))

(defun tex-latex-close ()
  "Close LaTeX environment."
  (interactive)
  (let (pos col env)
    (save-excursion
      (goto-char (point-min))
      (tex-replace-string "\\begin" "(*****")
      (goto-char (point-min))
      (tex-replace-string "\\end" ")***"))
    (insert-string ")")
    (save-excursion
      (setq pos (point))
      (backward-sexp 1)
      (if (> pos (point))
	(progn
	  (setq pos (point))
	  (setq col (current-column))
	  (if (search-forward "*****" nil t)
	    (if (looking-at " *{")
	      (progn
		(search-forward "{")
		(let ((begin (1- (point))))
		  (search-forward "}")
		  (setq env (buffer-substring begin (point)))))
	      (setq env ""))
	    (setq col -1)))
	(setq col -1)))
    (delete-backward-char 1)
    (if (> col -1)
      (progn
	(beginning-of-line)
	(if (looking-at "^\\W*$")
	  nil
	  (end-of-line)
	  (newline))
	(indent-to col)
	(insert-string "\\end" env)
	(let ((ind (current-indentation)))
	  (newline)
	  (indent-to ind))))
    (save-excursion
      (goto-char (point-min))
      (tex-replace-string "(*****" "\\begin")
      (goto-char (point-min))
      (tex-replace-string ")***" "\\end"))
    (if (> col -1)
      (save-excursion
	(goto-char pos)
	(if (pos-visible-in-window-p)
	  (sit-for 1)
	  (progn
	    (beginning-of-line)
	    (re-search-forward "^.*$")
	    (message "%s" (buffer-substring (match-beginning 0) (match-end 0))))))
      (error "\\begin...\\end pair failed to match!"))))

(defun tex-replace-string (old new)
  "Replace OLD by NEW, both strings, quietly (i.e. with no minibuffer messages)."
  (while (search-forward old nil t)
    (delete-region (match-beginning 0) (match-end 0))
    (insert-string new)))

(defun tex-latex-skip ()
  "Skip the next line (presumably \\end{env}) and indent properly."
  (interactive)
  (next-line 1)
  (end-of-line)
  (tex-newline-indent))

(defun tex-latex-putenv (n env &optional arg)
  (let* ((col (current-column))
	 (n (+ (if n (if (< n 0) 0 n) tex-latex-indentation) col))
	 (env (concat "{" env "}"))
	 (arg (if arg (read-string (concat "Arguments to environment " env ": ")) "")))
    (insert "\\begin" env arg "\n")
    (indent-to col)
    (insert "\\end" env)
    (beginning-of-line)
    (open-line 1)
    (indent-to n)))

(defun tex-latex-array (&optional n)
  (interactive "P")
  (tex-latex-putenv n "array" t))

(defun tex-latex-center (&optional n)
  (interactive "P")
  (tex-latex-putenv n "center"))

(defun tex-latex-document (&optional n)
  (interactive "P")
  (tex-latex-putenv n "document"))

(defun tex-latex-description (&optional n)
  (interactive "P")
  (tex-latex-putenv n "description"))

(defun tex-latex-enumerate (&optional n)
  (interactive "P")
  (tex-latex-putenv n "enumerate"))

(defun tex-latex-figure (&optional n)
  (interactive "P")
  (tex-latex-putenv n "figure"))

(defun tex-latex-flushleft (&optional n)
  (interactive "P")
  (tex-latex-putenv n "flushleft"))

(defun tex-latex-flushright (&optional n)
  (interactive "P")
  (tex-latex-putenv n "flushright"))

(defun tex-latex-itemize (&optional n)
  (interactive "P")
  (tex-latex-putenv n "itemize"))

(defun tex-latex-picture (&optional n)
  (interactive "P")
  (tex-latex-putenv n "picture" t))

(defun tex-latex-quotation (&optional n)
  (interactive "P")
  (tex-latex-putenv n "quotation"))

(defun tex-latex-quote (&optional n)
  (interactive "P")
  (tex-latex-putenv n "quote"))

(defun tex-latex-tabbing (&optional n)
  (interactive "P")
  (tex-latex-putenv n "tabbing" t))

(defun tex-latex-table (&optional n)
  (interactive "P")
  (tex-latex-putenv n "table"))

(defun tex-latex-tabular (&optional n)
  (interactive "P")
  (tex-latex-putenv n "tabular" t))

(defun tex-latex-verbatim (&optional n)
  (interactive "P")
  (tex-latex-putenv n "verbatim"))

	       

