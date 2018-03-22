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
;;;
;;;  RCS Header
;;;  $Header: /home/yew/yew4/vortex/newdist/emacs/RCS/VorTeX-common.el,v 1.14 1993/09/16 02:32:14 munson Exp $
;;;
;; This file contains VorTeX-common.el, a collection of routines
;; used by both GNU Emacs TeX-Mode and BibTeX-mode.
;;

(provide 'VorTeX-common)

;; ============
;; Printer Data
;; ============

(defvar vortex-default-print-command "lpr -d -P"
  "Print command for use with unknown printer.")
(defvar vortex-printer-query t "Nil if no need to query for printer name.")
(defvar vortex-printer-data
  '(("cx" . "dps -Pcx")
    ("dp" . "lpr -d -Pdp")
    ("gp" . "dps -Pgp")
    ("ps" . "dps -Pps"))
  "Association list of available printers and their corresponding print
commands for .dvi files")
(defvar vortex-printer-list
  (concat "(" (mapconcat 'car vortex-printer-data ", ") ")")
  "Printer list for prompting")
(defvar vortex-default-printer
  (let ((env-printer (getenv "PRINTER")))
    (if env-printer env-printer "gp"))
  "Default printer")
(defvar vortex-default-print-command "lpr -d -P"
  "Print command for use with unknown printer.")
(defvar vortex-printer-query t "Nil if no need to query for printer name.")
(defvar vortex-printer-queue "lpq -P" "Default printer")
(defvar vortex-extractor "dviselect"
  "Program which extracts pages from DVI file")



;;  This was designed to provide a generic, data driven prompt-response
;;  mechanism.  Note that this function will ONLY accept responses
;;  in RESPONSE-DATA, so you probably want to provide a `quit' response.
(defun vortex-user-response (head response-data)
  "Gets the user's response to a request.  HEAD is a string which is 
usually used to request a response.  RESPONSE-DATA is a list of three-element
lists.  The first element is a list of acceptable characters.  The
second element is a string to be included in the user prompt.  The third
element is a symbol which will be the return value if the user enters one
one of the characters in the first element."
  (let ((prompt 
	 (concat head "["
		 (mapconcat '(lambda (x) (nth 1 x)) response-data " ") "]"))
	(result nil)
	)
    (message prompt)
    (while (not (setq result
		      (vortex-match-response (read-char) response-data)))
      (ding)
      (message "%s...WHAT?" prompt)
      (sit-for 1)
      (message prompt))
    result
    ))

;; This is an analogue to ASSOC that is appropriate to the list of 
;; three-element lists used by vortex-user-response.  It uses tail recursion.
(defun vortex-match-response (char response-data)
  (if (null response-data) nil
    (if (memq char (car (car response-data)))
	(nth 2 (car response-data))
      (vortex-match-response char (cdr response-data)))
    ))

(defun vortex-mkpath (env)
  "Make a path, a list of strings, out of ENV, an environment variable."
  (let ((env (getenv env))
	(path nil)
	to)
    (while env
      (setq to (string-match ":" env))
      (if to
	(progn
	  (setq path (cons (substring env 0 to) path))
	  (setq env (substring env (1+ to))))
	(setq path (cons env path))
	(setq env nil)))
    (reverse path)))

(defun vortex-mkstring (pre post l)
  (mapconcat
   '(lambda (elt) (concat pre elt post)) l "\\|"))

(defun vortex-file-exists-p (file &optional path)
  "Check if FILE is in default-directory, if not, check if it's in PATH.
Return the full path name of FILE is found, else return nil."
  (vortex-file-predicate file 'file-exists-p path))

(defun vortex-file-readable-p (file &optional path)
  "Check if FILE is in default-directory and is readable.  Otherwise,
looks for a readable file of the same name in each directory in PATH."
  (vortex-file-predicate file 'file-readable-p path))

(defun vortex-file-predicate (file file-predicate path)
  (let ((path (cons default-directory path))
	fn)
    (if (funcall file-predicate file)
      file
      (catch 'done
	(while path
	  (setq fn (concat (car path) "/" file))
	  (if (funcall file-predicate fn)
	    (throw 'done fn)
	    (setq path (cdr path))))))))

;; vortex-memq is an analogue to memq that uses string-equal instead of eq
;; as the equality test.  It returns t if KEY is string-equal to an element
;; of L.
(defun vortex-memq (key l)
  (if l
    (or (string-equal key (car l))
	(vortex-memq key (cdr l)))))
;;
;; VORTEX-KEYMAP-UTIL takes MODE-DATA and uses it to set up keymaps.
;; MODE-DATA is a list of file-data (see comments for
;; vortex-set-file-bindings).
;;
(defun vortex-keymap-util (mode-data)
  (mapcar 'vortex-set-file-bindings mode-data))

;;
;; VORTEX-SET-FILE-BINDINGS is used to set all key-bindings and
;; autoload functions for a file.  The car of FILE-DATA is the name
;; of the file.  The remaining elements of FILE-DATA are per-keymap
;; specifications of key-bindings (see comments for vortex-set-bindings).
;;
(defun vortex-set-file-bindings (file-data)
  (let ((file (car file-data))
	(map-data-list (cdr file-data)))
    (mapcar '(lambda (map-data) (vortex-set-bindings file map-data))
	    map-data-list)
    ))

;;
;; VORTEX-SET-BINDINGS sets key-bindings for FILE.  The car of MAP-DATA
;; specifies the keymap for the bindings.  The cdr of MAP-DATA is a list of
;; pairs in which the car is a key-sequence and the cadr is the binding
;; (usually a function).  If FILE is null, no autoload is done.  If
;; any key-sequence is null, its function has no binding but is
;; autoloaded (yes, this case is hokey, but it was easy to implement)
;;
(defun vortex-set-bindings (file map-data)
  (let ((keymap (symbol-function (car map-data)))
	(key-data-list (cdr map-data)))
    (mapcar
     '(lambda (key-data)
	(let ((key-sequence (car key-data))
	      (key-binding (nth 1 key-data)))
	  (if key-sequence (define-key keymap key-sequence key-binding))
	  (if file (autoload key-binding file))))
     key-data-list
     )))

(defun vortex-parse-comma-list (fl)
  "Parse a string containing comma-separated file names into a list of
file names (each of which is a string).  If a file name includes 
a comma, that comma must be 'escaped' with a double backslash (\\),
to be ignored as a separator."
  (let ((md (match-data)))
    (prog1
	(if (string-match "[^\\]," fl)
	    (let ((after-comma (match-end 0)))
	      (cons (substring fl 0 (1- after-comma))
		    (vortex-parse-comma-list (substring fl after-comma))))
	  (list fl))
      (store-match-data md))))

(defun vortex-init-process-buffer (buffer-name)
  (let (process)
    (while (setq process (get-buffer-process buffer-name))
      (kill-process process)))
  (save-excursion
    (set-buffer (get-buffer-create buffer-name))
    (erase-buffer)))

;;;
;;; VORTEX-MATCH-STRING takes NUMBER and returns
;;; (buffer-substring (match-beginning NUMBER) (match-end NUMBER))
;;;

(defun vortex-match-string (number)
  (buffer-substring (match-beginning number) (match-end number)))

;;;
;;; This is a workaround for an emacs bug that made define-prefix-command
;;; not work in emacs version prior to 18.54.
;;;

(progn
  (string-match "\\([0-9]+\\)\\.\\([0-9]+\\)" emacs-version)
  (let ((first-num (string-to-int
		    (substring emacs-version (match-beginning 1)
			       (match-end 1))))
	(second-num (string-to-int
		    (substring emacs-version (match-beginning 2)
			       (match-end 2))))
	)
    (if (or (< first-num 18)
	    (and (= first-num 18) (< second-num 54)))
	(defun define-prefix-command (symbol)
	  (fset symbol (make-keymap)))
      )))

(defun vortex-mapcan (func list)
  (if (null list)
      nil
    (nconc (funcall func (car list))
	   (vortex-mapcan func (cdr list)))))

