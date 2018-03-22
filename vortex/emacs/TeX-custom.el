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
;; $Header: /home/yew/yew4/vortex/newdist/emacs/RCS/TeX-custom.el,v 1.14 1993/09/16 02:32:14 munson Exp $
;;
;; This file, "TeX-custom.el", is a subsystem of "TeX-mode.el".
;; It is a facility to define custom-made semi-automatic (zone/word)
;; matching delimiters as well as automatic ones as defined in "TeX-match.el"
;; and "TeX-mode.el" respectively.  It also contains functions to define
;; new LaTeX environment delimiter pairs similar to those defined 
;; in "TeX-match.el".
;;

(require 'TeX-mode)

;; ==============================================
;; Customizing Semi-automatic Matching Delimiters
;; ==============================================

(defvar tex-semi-done nil)
(defconst tex-zone-prefix "\^c\e")
(defconst tex-word-prefix "\^c")
(defconst tex-word-forward-prefix (concat tex-word-prefix "4"))

(defun tex-mkgrp-semi ()
  (if tex-semi-done
    nil
    (let ((semi tex-delimiters-semi)
	  (n 1)
	  args)
      (while semi
	(setq args (car semi))
	(if (= (length args) 4)
          (apply 'tex-make-semi (append args (cons n '(t))))
	  (ding)
	  (message "WARNING: Incorrect format found in entry #%d of tex-delimiters-semi" n)
	  (sit-for 3))
	(setq n (1+ n))
	(setq semi (cdr semi)))
      (setq tex-semi-done t))))

(defun tex-make-semi (l-sym r-sym name letter &optional n &optional quiet)
  "Register a new set of semi-automatic delimiter pairs whose open delimiter
is L-SYM and right delimiter is R-SYM under the name NAME.
The functions tex-zone-NAME, tex-word-NAME, and tex-word-forward-NAME will
be generated and be respectively bound to C-c ESC-LETTER, C-c LETTER, and
C-c-4 LETTER."
  (interactive "sOpen delimiter: \nsClose delimiter: \nsName: \nsLetter (to be incorporated in key bindings): ")
  (let ((l-sym (tex-check-empty l-sym "open delimiter" "tex-delimiters-semi" n))
	(r-sym (tex-check-empty r-sym "close delimiter" "tex-delimiters-semi" n))
	(name (tex-check-empty name "name" "tex-delimiters-semi" n))
	(letter (tex-check-empty letter "letter" "tex-delimiters-semi" n)))
    (if (and l-sym r-sym name letter)
      (progn
	(tex-mkzone (concat "tex-zone-" name) (concat tex-zone-prefix letter) quiet)
	(tex-mkword (concat "tex-word-" name) (concat tex-word-prefix letter) quiet)
	(tex-mkword-forward (concat "tex-word-forward-" name) 
			    (concat tex-word-forward-prefix letter) quiet)
	(if quiet
	  nil
	  (message "%s...%s registered as a semi-automatic matching pair." l-sym r-sym))))))
	
(defun tex-mkzone (name key quiet)
  (let ((symbol (intern name))
	func)
    (if (or quiet
	    (and (or (not (fboundp symbol))
                     (y-or-n-p (concat "Function " name " already exists, overwrite? ")))
		 (or (not (setq func (key-binding key)))
	              (y-or-n-p (concat key " already bound to " (symbol-name func) ", overwrite? ")))))
      (eval (list 'progn
              (list 'defun symbol nil
		      '(interactive)
		      (list 'tex-zone l-sym r-sym))
	      (list 'define-key 'tex-mode-map	key 'symbol))))))

(defun tex-mkword (name key quiet)
  (let ((symbol (intern name))
	func)
    (if (or quiet
	    (and (or (not (fboundp symbol))
                     (y-or-n-p (concat "Function " name " already exists, overwrite? ")))
		 (or (not (setq func (key-binding key)))
	              (y-or-n-p (concat key " already bound to " (symbol-name func) ", overwrite? ")))))
      (eval (list 'progn
	      (list 'defun symbol '(n) 
		      '(interactive "P") 
		      (list 'tex-word l-sym r-sym '(if n (- n) -1)))
	      (list 'define-key 'tex-mode-map	key 'symbol))))))

(defun tex-mkword-forward (name key quiet)
  (let ((symbol (intern name))
	func)
    (if (or quiet
	    (and (or (not (fboundp symbol))
                     (y-or-n-p (concat "Function " name " already exists, overwrite? ")))
		 (or (not (setq func (key-binding key)))
	             (y-or-n-p (concat key " already bound to " (symbol-name func) ", overwrite? ")))))
      (eval (list 'progn
	      (list 'defun symbol '(n) 
		      '(interactive "P") 
		      (list 'tex-word l-sym r-sym 'n))
	      (list 'define-key 'tex-mode-map	key 'symbol))))))
      

;; =========================================
;; Customizing Automatic Matching Delimiters
;; =========================================

(defvar tex-auto-done nil)
(defconst tex-toggle-prefix "\^c\^t")

(defun tex-mkgrp-auto ()
  (if tex-auto-done
    nil
    (let ((auto tex-delimiters-auto)
	  (n 1)
	  args)
      (while auto
	(setq args (car auto))
	(if (= (length args) 2)
	  (apply 'tex-make-auto (append args (cons n '(t))))
	  (ding)
	  (message "WARNING: Incorrect format found in entry #%d of tex-delimiters-auto" n)
	  (sit-for 3))
	(setq n (1+ n))
	(setq auto (cdr auto)))
      (setq tex-auto-done t))))
	
(defun tex-make-auto (sym name &optional n &optional quiet)
  "Register a new automatic matching delimiter pair whose open and close
delimiters are identical (SYM).  The functions tex-NAME and tex-toggle-NAME
will be generated and be bound to the symbol itself and C-c C-t SYM 
respectively."
  (interactive "sDelimiter: \nsName: ")
  (let* ((sym (tex-check-empty sym "delimiter" "tex-delimiter-auto" n))
	 (name (tex-check-empty name "name" "tex-delimiter-auto" n))
	 (fn (concat "tex-" name))
	 (func (intern fn))
	 sym-func)
    (if (and sym name)
      (progn
	(setq sym (substring sym 0 1))
	(if (or quiet
		(eq (setq sym-func (key-binding sym)) 'self-insert-command)
		(and (not (eq fn sym-func))
		     (y-or-n-p (concat "Symbol `" sym 
				       "' already bound to function "
				       (symbol-name sym-func) ", overwrite? "))))
	  (let ((match-on (intern (concat "tex-match-" name "-on")))
		(lst (intern (concat "tex-" name "-list")))
		(toggle (intern (concat "tex-toggle-" name))))
	    (eval (list 'progn
		    (list 'make-local-variable 'match-on)
		    (list 'setq match-on t)
		    (list 'make-local-variable 'lst)
		    (list 'setq lst nil)
		    (list 'defun func nil
		      '(interactive)
		      (list 'tex-delimiter
			    (list 'quote match-on)
			    (list 'quote toggle)
			    (list 'quote lst)
			    (concat "^" sym "\\|[^\\]" sym)
			    (string-to-char sym)))
		    (list 'define-key 'tex-mode-map sym 'func)
		    (list 'defun toggle '(&optional quiet)
		      '(interactive)
		      (list 'tex-toggle-delimiter
			    (list 'quote match-on)
			    (list 'quote lst)
			    sym
			    (list 'quote func)
			    'quiet))
		    (list 'define-key 'tex-mode-map (concat tex-toggle-prefix sym) 'toggle)))
	    (if quiet
	      nil
	      (message "%s...%s registered as an automatic matching pair." sym sym))))))))
     

;; ========================================
;; Customizing LaTeX environment delimiters
;; ========================================

(defvar tex-latex-done nil)
(defconst tex-latex-prefix "\^c\^l")

(defun tex-mkgrp-env ()
  "Register every element of tex-latex-envs by calling tex-make-env quietly."
  (if tex-latex-done
    nil
    (let ((envs tex-latex-envs)
	  (n 1)
	  args)
      (while envs
	(setq args (car envs))
	(if (= (length args) 3)
	  (apply 'tex-make-env (append args (cons n '(t))))
	  (ding)
	  (message "WARNING: Incorrect format found in entry #%d of tex-latex-envs" n)
	  (sit-for 3))
	(setq n (1+ n))
	(setq envs (cdr envs)))
      (setq tex-latex-done t))))

(defun tex-make-env (env letter &optional argp &optional n &optional quiet)
  "Register a new LaTeX environment delimiter pair under the name ENV.
A new function tex-latex-ENV will be generated and be bound to C-c C-l LETTER.
ARGP should be t (TRUE) if optional LaTeX environment argument is required,
nil otherwise.  N the current count of the (ENV LETTER ARGP) tuple in
tex-latex-envs.  No message is given if QUIET is true."
  (interactive "sName of LaTeX environment: \nsLetter (to be incorporated in key binding): ")
  (let* ((env (tex-check-empty env "environment name" "tex-latex-envs" n))
	 (letter (tex-check-empty letter "letter" "tex-latex-envs" n))
	 (fn (concat "tex-latex-" env))
	 (funsym (intern fn))
	 (key (concat tex-latex-prefix letter))
	 func)
    (if (and env letter)
      (progn
	(if quiet nil (setq argp (y-or-n-p "Any arguments? ")))
	(if (or quiet
		(and (or (not (fboundp funsym))
			 (y-or-n-p (concat "Function " fn " already exists, overwrite? ")))
		     (or (not (setq func (key-binding key)))
			 (y-or-n-p (concat key " already bound to " (symbol-name func) ", overwrite? ")))))
	  (progn
	    (eval (list 'progn
		    (list 'defun funsym 
			    '(&optional n)
			    '(interactive "P")
			    (list 'tex-latex-putenv 'n env argp))
		    (list 'define-key 'tex-mode-map key 'funsym)))
	    (if quiet
	      nil
	      (message "\\begin{%s}...\\end{%s} registered as a delimiter pair..."env env)))
	  (message "%s not bound to %s" key fn))))))


;; ==============
;; Help Functions
;; ==============

(defun tex-check-empty (string name &optional lst &optional n)
  (if (> (length string) 0)
    string
    (ding)
    (if n
      (message "WARNING: Null %s found in entry #%d of %s...ignored" name n lst)
      (message "WARNING: %s shouldn't be null..." name))
    (sit-for 3)
    nil))
