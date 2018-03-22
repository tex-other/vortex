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
;; $Header: /home/yew/yew4/vortex/newdist/emacs/RCS/TeX-bib.el,v 1.14 1993/09/16 02:32:14 munson Exp $
;;
;; This file, "TeX-bib.el", is a GNU Emacs interface to BibTeX databases
;; for plain TeX and LaTeX documents.  It is a subsystem of TeX-mode.
;;

(require 'TeX-mode)

(autoload 'bibtex-goto-error  "BibTeX-misc")
(setq max-lisp-eval-depth 1000)
(defconst tex-bib-regexp-all "\\\\cite{\\|\\\\nocite{\\|\\\\input\\|\\\\cite\\[.*\\]{")
(defconst tex-bib-regexp-cite "\\\\cite{\\|\\\\nocite{\\|\\\\cite\\[.*\\]{")
(defvar tex-bib-cite-list nil 
  "List of (CITE-FLAG (ENT1 ENT2 ... ENTn) START END) to be replaced by act")
(defvar tex-bib-cite-tags nil
  "List of (ENTRY (Fn STARTn ENDn) ... (F1 START1 END1)), used in correcting errors.")
(defvar tex-bib-err-list nil "List of \\cite errors")
(defvar tex-bib-fn-list nil "List of buffer names of involving files")
(defvar tex-bib-rec-list nil "List of recovering information")
(defvar tex-bib-ref-list nil "List of symbolic/actual cross references")
(defvar tex-bib-to-bibtex nil "Flag set t if bibtex-mode is loaded here.")

(defvar tex-bib-abbrev
  (progn
    (if (not (boundp 'bibtex-abbrev-files))
	(run-hooks 'bibtex-mode-hook))
    (if (not (boundp 'bibtex-abbrev-files))
	(setq bibtex-abbrev-files nil))
    bibtex-abbrev-files)
  "List of abbreviation files to be included in any \\bibliography commands")



;; ===============
;; Preprocessing
;; ===============

(defun tex-bib-document ()
  "Make a bibliography for the TeX/LaTeX document rooted at the master."
  (interactive)
  (tex-check-master-file)
  (tex-bib-buffer))

(defun tex-bib-buffer ()
  "Make a bibliography for the TeX/LaTeX document rooted at current file."
  (interactive)
  (let* ((=buff= (file-name-nondirectory (buffer-file-name)))
	 (=base= (substring =buff= 0 (string-match "\.tex" =buff=))) 
	 (=aux= (concat =base= ".aux"))
	 (=bbl= (concat =base= ".bbl"))
	 (=ref= (concat =base= ".ref"))
	 (=blg= (concat =base= ".blg"))
	 (regexp tex-bib-regexp-all)
	 =bibmsg= cont
	 (doc-type (tex-check-document-type)))
    (get-buffer-create =aux=)
    (get-buffer-create =ref=)
    (save-excursion
      (setq =bibmsg= (concat "Making bibliography for " doc-type
			     " document rooted at \"" =buff= "\"..."))
      (message =bibmsg=)
      (sit-for 1)
      (tex-bib-recover =buff= =base=))
    (tex-get-include-files)
    (message 
     "%s%s"
     =bibmsg=
     (catch '=done=
       (while t
	 (tex-bib-mkaux regexp)
	 (get-buffer-create =bbl=)
	 (setq normal nil)
	 (catch 'abort
	   (tex-bib-bibtex 'abort)
	   (setq normal t))
	 (if (and (save-excursion
		    (set-buffer "*shell*")
		    (goto-char tex-error-pos)
		    (re-search-forward "^ --line\\|^ : Warning:\\|---line" nil t))
		  (y-or-n-p "Errors detected in .bib files, correct them? "))
	     (save-excursion
	       (if (boundp 'bibtex-error-pos)
		   nil
		 (setq tex-bib-to-bibtex t)
		 (load "BibTeX-mode"))
	       (message "Use C-c C-@ to go to next .bib error.")
	       (setq bibtex-bibs nil)
	       (setq bibtex-error-pos tex-error-pos)
	       (recursive-edit)
	       (setq tex-bib-to-bibtex nil)
	       (message "Returning to bibliography making...")
	       (sit-for 1)))
	 (if (and tex-bib-err-list
		  (y-or-n-p "Citation errors detected, correct them? "))
	     (progn
	       (tex-bib-correct-error)
	       (setq regexp tex-bib-regexp-cite)
	       (setq tex-include-files tex-bib-fn-list))
	   (if normal
	       (save-excursion
		 (message "%scontinuing" =bibmsg=)
		 (sit-for 1)
		 (cond
		  ((string-equal
		    doc-type
		    (nth 2 (assoc "tex" tex-formatter-specs))) ; TeX
		   (tex-bib-mkbbl-tex))
		  (t			; LaTeX/SliTeX
		   (tex-bib-mkbbl-latex)))
		 (tex-bib-sym2act)
		 (tex-bib-write-ref)
		 (tex-bib-input-bbl)
		 (save-excursion
		   (set-buffer "*shell*")
		   (goto-char (point-max))
		   (insert-string "\\rm " =aux= "* " =blg= "*")
		   (shell-send-input))
		 (tex-bib-save)
		 (throw '=done= "done"))
	     (throw '=done= "abort (on request)")))))
     (kill-buffer =aux=)
     (kill-buffer =ref=)
     (kill-buffer =bbl=))))

(defun tex-bib-recover (&optional buff base)
  "Recover symbolic references from the current actual refernces."
  (interactive)
  (message "Recovering symbolic references...")
  (let* ((buff (if buff buff (buffer-name)))
	 (base (if base base (substring buff 0 (string-match "\.tex" buff))))
	 (bbl (concat base ".bbl"))
	 (ref (concat base ".ref"))
	 (refbuf (get-buffer-create ref)))
    (save-excursion
      (goto-char (point-max))
      (if (re-search-backward (concat "\\\\input *" bbl "\\|\\\\input *{" bbl) nil t)
	(progn
	  (beginning-of-line)
          (kill-line 1)
	  (goto-char (point-max))
	  (if (re-search-backward "\\\\bibliography *{" nil t)
	    (progn
	      (re-search-backward "^\\|\\w")
	      (if (bolp) nil (forward-char 1))
	      (zap-to-char 1 ?\\)
	      (if (bolp) nil (insert ? ))))
	  (goto-char (point-max))
	  (if (search-backward "\\bibliographystyle" nil t)
	    (progn
	      (re-search-backward "^\\|\\w")
	      (if (bolp) nil (forward-char 1))
	      (zap-to-char 1 ?\\)
	      (if (bolp) nil (insert ? ))))
	  (if (file-exists-p ref)
	    (progn
	      (set-buffer refbuf)
	      (erase-buffer)
	      (insert-file-contents ref)
	      (if (= (buffer-size) 0)
		(error "Recovering symbolic references...abort (missing recovery information.)")
		(goto-char 1)
		(setq tex-bib-fn-list (read refbuf))
		(setq tex-bib-rec-list (read refbuf)))
	      (tex-bib-act2sym)
	      (message "Recovering symbolic references...done"))
	    (message "Recovering symbolic references...no .ref file")))
        (message "Recovering symbolic references...no .bbl file referenced"))
      (if (interactive-p)
	(kill-buffer refbuf)))))
		
(defun tex-bib-act2sym ()
  (message "Recovering symbolic references...")
  (let (fn nd lst pair act exist)
    (while tex-bib-fn-list
      (save-excursion
	(setq exist t)
	(setq fn (car tex-bib-fn-list))
	(setq nd (file-name-nondirectory fn))
	(setq lst (car tex-bib-rec-list))
        (message "Recovering symbolic references---doing \"%s\"..." nd)
	(if (file-exists-p fn)
	  (set-buffer (find-file-noselect fn))
	  ; Do this because different hosts have differnt directory part
	  (if (file-exists-p nd)
	    (set-buffer (find-file-noselect nd))
	    (setq exist nil)))
	(if exist
	  (progn
	    (goto-char 1)
	    (while lst
	      (setq pair (car lst))
	      (setq act (car pair))
	      (message "Recovering symbolic references---doing %s in \"%s\"..." act nd)
	      (if (search-forward act nil t)
		(progn
		  (delete-region (match-beginning 0) (match-end 0))
		  (insert-string (nth 1 pair))))
	      (setq lst (cdr lst)))
	    (message 
	      "Recovering symbolic references---doing \"%s\"...done" nd))
	  (ding)
	  (message
	    "Recovering symbolic references---doing \"%s\"...file not found" nd)
	  (sit-for 1))
	(setq tex-bib-fn-list (cdr tex-bib-fn-list))
	(setq tex-bib-rec-list (cdr tex-bib-rec-list)))))
  (message "Recovering symbolic references...done"))

(defun tex-bib-sym2act ()
  (message "Substituting actuals for symbolics...")
  (let ((fn-list tex-bib-fn-list)
	sym-list fn nd)
    (setq tex-bib-rec-list nil)
    (while fn-list
      (setq sym-list (car tex-bib-cite-list))
      (if sym-list
	(save-excursion
	  (setq fn (car fn-list))
	  (setq nd (file-name-nondirectory fn))
	  (tex-bib-sym2act-replace)))
      (setq tex-bib-cite-list (cdr tex-bib-cite-list))
      (setq fn-list (cdr fn-list))))
  (setq tex-bib-rec-list (reverse tex-bib-rec-list))
  (message "Substituting actuals for symbolics...done"))

(defun tex-bib-sym2act-replace ()
  (let ((lst nil)
	item cite note entries begin end syment symref act)
    (message "Substituting actuals for symbolics---doing \"%s\"..." nd)
    (set-buffer (find-file-noselect fn))
    (goto-char 1)
    (while sym-list
      (setq item (car sym-list))
      (if item
	(progn
	  (setq cite (car item))
	  (setq note (nth 1 item))
	  (setq entries (nth 2 item))
	  (setq begin (nth 3 item))
	  (setq end (nth 4 item))
	  (goto-char begin)
	  (if cite
	    (progn				; \\cite{...}
	      (delete-region begin end)
	      (insert "~[")
	      (setq symref "")
	      (while entries
		(setq syment (car entries))
		(message "Substituting actuals for symbolics---doing {%s} in \"%s\"..." syment nd)
		(if (setq act (tex-bib-match-sym syment tex-bib-ref-list))
		  (progn
		    (insert act ",")
		    (setq symref (concat symref "," syment)))
		  (ding)		; Theoretically, this won't happen
		  (message "WARNING: %s not in .bbl file...ignored" syment)
		  (sit-for 2))
		(setq entries (cdr entries)))
	      (delete-char -1)
	      (if note
		(insert ", " note))
	      (insert ?])
	      (setq lst (cons (list (buffer-substring (1+ begin) (point))
				    (concat "\\cite"
					    (if note
					       (concat "[" note "]"))
					    "{"
					    (substring symref 1)
				            "}"))
			      lst)))
	    (setq symref (buffer-substring begin end))		; \\nocite{...}
	    (tex-bib-comment-out begin end)
	    (setq lst (cons (list (concat "%" symref) symref) lst)))))
      (setq sym-list (cdr sym-list)))
    (if lst (setq tex-bib-rec-list (cons lst tex-bib-rec-list)))
    (message "Substituting actuals for symbolics---doing \"%s\"...done" nd)))

(defun tex-bib-match-sym (sym lst)
  (let (pair)
    (catch 'here
      (while lst
	(setq pair (car lst))
	(if (string-equal sym (nth 1 pair))
	  (throw 'here (car pair))
	  (setq lst (cdr lst)))))))

(defun tex-bib-mkaux (regexp)
  "Make .aux file to be BibTeXed."
  (message "Making \"%s\" for BibTeX..." =aux=)
  (save-excursion
    (set-buffer =aux=)
    (widen)
    (erase-buffer))
  (setq tex-bib-cite-list nil)
  (setq tex-bib-cite-tags nil)
  (setq tex-bib-fn-list nil)
  (let (=fnlst= =fn= =fnnd=)
    (setq =fnlst= tex-include-files)
    (while =fnlst=
      (setq =fn= (car =fnlst=))
      (setq =fnlst= (cdr =fnlst=))
      (setq =fnnd= (file-name-nondirectory =fn=))
      (if (string-match "\.bbl$" =fnnd=)
	nil
	(save-excursion
	  (set-buffer (find-file-noselect =fn=))
	  (message "Making %s for BibTeX---doing \"%s\"..." =aux= =fnnd=)
	  (tex-bib-mkaux-scan regexp)
	  (message "Making %s for BibTeX---doing \"%s\"...done" =aux= =fnnd=)))))
  (save-excursion
    (tex-bib-mkaux-data)
    (tex-bib-mkaux-style))
  (tex-bib-mkaux-cleanup)
  (setq tex-bib-cite-list (reverse tex-bib-cite-list))
  (setq tex-bib-fn-list (reverse tex-bib-fn-list))
  (message "Making \"%s\" for BibTeX...done" =aux=))

(defun tex-bib-mkaux-scan (regexp)
  (let ((=cites= nil) (=inputs= nil) bslash =char=)
    (goto-char 1)
    (save-excursion
      (while (re-search-forward regexp nil t)
	(setq bslash (match-beginning 0))
;	(message "bslash = %d" bslash) (sit-for 2)
	(if (save-excursion
	      (re-search-backward "^%\\|[^\\]%" 	; Commented out?
				  (save-excursion 
				    (beginning-of-line) 
				    (1- (point))) 
				  t))
	  nil
	  (save-excursion
	    (setq =char= (char-after (1+ bslash)))
	    (if (= =char= ?i)
	      (tex-bib-scan-input)		; \input
	      (tex-bib-scan-citation))))))	; \cite or \nocite
    (if =inputs= (setq =fnlst= (append (reverse =inputs=) =fnlst=)))
    (if =cites=
      (progn
;;; *** Error ***
;;; If the bibliographystyle is prior to any cites, this causes an
;;; error, because the data is tex-bib-cite-list is dirtied by 
;;; tex-bib-mkaux-style.
	(tex-bib-mkaux-data)
	(tex-bib-mkaux-style)
        (setq tex-bib-cite-list (cons =cites= tex-bib-cite-list))
	(setq tex-bib-fn-list (cons =fn= tex-bib-fn-list))))))

(defun tex-bib-scan-input ()
  (let ((eol (save-excursion (end-of-line) (point)))
	(arg nil)
	act eoi pos char)
    (setq eoi (point))
    (re-search-forward "[^ \t{]")
    (setq pos (1- (point)))
    (re-search-forward "\\.\\| \\|\t\\|}\\|$")
    (setq char (preceding-char))
    (if (> (point) eol)
      (tex-check-input-arg eoi "\\input format error")
      (cond
        ((save-excursion		; \input foo.bbl, the bibliography
	   (goto-char pos)
	   (looking-at =bbl=))
	 nil)
	((= (preceding-char) ?.)
	 (re-search-forward " \\|\t\\|}\\|$")
	 (setq char (preceding-char))
	 (setq arg (buffer-substring pos 
				     (if (or (= char ? ) (= char ?\t) (= char ?}))
				       (1- (point))
				       (point)))))
	(t
	 (setq arg (concat (buffer-substring pos 
					     (if (or (= char ? ) (= char ?\t) (= char ?}))
					       (1- (point))
					       (point)))
			   ".tex"))))
      (if (and arg (setq act (vortex-file-exists-p arg tex-bib-path)))
	(setq =inputs= (cons act =inputs=))))))
;;	(tex-check-input-arg eoi (concat "File \"" arg "\" nonexistent"))))))

(defun tex-bib-scan-citation ()
  "Each element of lst is (CITE NOTE (R1 R2 ... Rn) BEGIN END)
where CITE is t if it comes from \\cite, nil if from \\nocite,
NOTE is the optional note field such as \[pp.1-5\],
Ri is the ith entry in \\cite{R1,R2,...,Rn},
BEGIN is the position of \\ and END the is position after }"
  (let (note =start= =end= =pos1= entries)
    (catch 'ok
      (setq =start=
	(save-excursion
	  (goto-char bslash)
	  (if (re-search-backward "[^ ~]" nil t)
	    (1+ (point))
	    (point))))
      (setq =end=
	(save-excursion
	  (search-forward "}")
	  (point)))
      (setq note
	(if (save-excursion
	      (re-search-backward "\\[ *\\(.*\\) *\\]" =start= t))
	  (vortex-match-string 1)
	  nil))
      (setq entries (tex-bib-cite-args))
      (setq =cites= (cons (list (= =char= ?c) note entries =start= =end=) =cites=)))))

(defun tex-bib-cite-args ()
  (let ((lst nil) string pos2)
    (catch 'abort
      (while (re-search-forward "\\w" =end= t)
        (setq =pos1= (1- (point)))
	(re-search-forward " *,\\| *}\\|[^,] +[^,}]" =end= t)
	(if (or (= (preceding-char) ?,) (= (preceding-char) ?}))
	  (progn
	    (setq pos2 (save-excursion (re-search-backward "[^,} ]") (1+ (point))))
	    (setq string (buffer-substring =pos1= pos2))
	    (tex-bib-mktags string =pos1= pos2)
	    (setq item (concat "\\citation{" string "}"))
	    (save-excursion
	      (set-buffer =aux=)
	      (goto-char (point-max))
	      (if (save-excursion (search-forward item nil t))
		nil                ; Don't duplicate if item already exists
		(insert-string item "\n")))
	    (setq lst (cons string lst)))
	  (tex-bib-cite-error lst)))     ; Illegal white space
      (reverse lst))))

(defun tex-bib-cite-error (lst)
  (let ((only1 nil)
	pos err)
    (if (re-search-forward "," =end= t)
      (setq pos (1- (point)))
      (setq pos (1- =end=))
      (setq only1 (if lst t)))
    (setq tex-bib-err-list (list (setq err (buffer-substring =pos1= pos))))
    (tex-bib-mktags err =pos1= pos)
    (ding)
    (message "{%s} in \"%s\" contains illegal white space..." err =fnnd=)
    (sit-for 1)
    (tex-bib-correct-error)
    (search-backward "\\")
    (if (and (= (preceding-char) ?%) only1)
      (throw 'ok (setq =cites= nil))
      (goto-char =pos1=)
      (setq =end= (save-excursion (search-forward "}" nil t) (point))))))

(defun tex-bib-mktags (string start end)
  "Register current entry and file name in tex-bib-cite-tags.
Each element of tex-bib-cite-tags is (ENTRY (Fn start end) ... (F1 start end))."
  (let ((prev nil) tag entry)
    (catch 'done
      (while tex-bib-cite-tags
	(setq tag (car tex-bib-cite-tags))
	(if (string-equal string (setq entry (car tag)))
	  (throw 'done
		 (setq tex-bib-cite-tags 
		       (append (reverse prev) 
			       (cons (cons entry (cons (list =fn= start end) (cdr tag)))
				     (cdr tex-bib-cite-tags))))))
	  (setq prev (cons tag prev))
	  (setq tex-bib-cite-tags (cdr tex-bib-cite-tags)))
      (setq tex-bib-cite-tags (reverse (cons (list string (list =fn= start end)) prev))))))

(defun tex-bib-comment-out (start end)
  (save-excursion
    (goto-char end)
    (if (eolp) nil (insert ?\n))
    (goto-char start)
    (insert ?%)))

(defun tex-bib-mkaux-data ()
  (goto-char (point-max))		; Still in file =fn=
  (if (re-search-backward "\\\\bibliography *{ *\\(.*\\) *}" nil t)
      (let ((file-string
	     (tex-bib-check-abbrev
	      (vortex-match-string 1)))
	    (bmark (make-marker))
	    (epos (match-end 0))
	    (bol (save-excursion (beginning-of-line) (point)))
	    )
;	(message "Begin: %d    End: %d" (match-beginning 0) epos)
;	(sit-for 5)
	(set-marker bmark (match-beginning 0))
	(if (search-backward "%" bol t)
	    (set-marker bmark (match-beginning 0)))
	(delete-region (marker-position bmark) epos)
	(goto-char (marker-position bmark))
	(if (not (bolp)) (newline))
	(insert "% \\bibliography{" file-string "}")
	(set-marker bmark nil)
	(save-excursion
	  (set-buffer =aux=)
	  (goto-char (point-max))
	  (if (bolp) nil (newline))
	  (if (save-excursion
		(re-search-backward "\\\\bibdata{\\(.*\\)}" nil t))
	      (progn
		(push-mark (match-beginning 0) t)
		(delete-region (match-beginning 0) (match-end 0)))
	    (push-mark nil t))
	  (goto-char (mark))
	  (insert (concat "\\bibdata{" file-string "}\n"))
	  (pop-mark)
	  ))))

(defun tex-bib-check-abbrev (file-string)
  (mapconcat '(lambda (fn) (substring fn 0 (string-match "\.bib" fn)))
	     (tex-bib-add-missing-strings
	      (vortex-parse-comma-list file-string)
	      tex-bib-abbrev)
	     ",")
  )

(defun tex-bib-add-missing-strings (main-list new-strings)
  (cond ((null new-strings) main-list)
	((vortex-memq (car new-strings) main-list)
	 (tex-bib-add-missing-strings main-list (cdr new-strings)))
	(t
	 (cons (car new-strings)
	       (tex-bib-add-missing-strings main-list (cdr new-strings))))
	))

(defun tex-bib-mkaux-style ()
  (let (string)
    (goto-char (point-max))            ; Still in file =fn=
    (if (re-search-backward "\\\\bibliographystyle *{\\(.*\\)}" nil t)
      (progn
	(setq string (vortex-match-string 1))
	(beginning-of-line)
	(if (looking-at ".*% *\\\\bibliographystyle")
	  nil
	  (if (looking-at "^ *\\\\bibliographystyle")
	    nil
	    (search-forward "\\bibliographystyle")
	    (search-backward "\\")
	    (newline))
	  (insert-string "% "))
	(save-excursion
	  (set-buffer =aux=)
	  (goto-char (point-max))
	  (if (bolp) nil (newline))
	  (if (search-backward "\\bibstyle{" nil t)
	    (if (search-forward string nil t)
	      nil
	      (message "Style %s in file \"%s\" inconsistent...ignored" string =fnnd=)
	      (sit-for 1))
	    (insert "\\bibstyle{" string "}\n")))))))

(defun tex-bib-mkaux-cleanup ()
  (save-excursion
    (set-buffer =aux=)
    (if (= (buffer-size) 0) 
      (throw '=done= "abort (no citations)")))   ; =done= in tex-bib-buffer
  (save-window-excursion
    (set-buffer =aux=)
    (goto-char 1)
    (insert-string "\\relax\n")
    (tex-bib-mkaux-cleanup-data)
    (tex-bib-mkaux-cleanup-style)
    (write-region 1 (point-max) =aux= nil 'no-message)))

(defun tex-bib-mkaux-cleanup-data ()
  (goto-char (point-max))		; In =aux=
  (if (bolp) nil (newline))
  (if (re-search-backward "\\\\bibdata{.*}" nil t)
    nil
    (set-buffer =buff=)
    (goto-char (point-max))
    (if (bolp) nil (newline))
    (let ((bibs (read-string "Base names of .bib files (as f1,f2,...): " =base=)))
      (if (string-equal bibs "") (setq bibs =base=))
      (insert "% \\bibliography{" tex-bib-abbrev bibs "}\n")
      (set-buffer =aux=)
      (insert "\\bibdata{" tex-bib-abbrev bibs "}\n"))))

(defun tex-bib-mkaux-cleanup-style ()
  (goto-char (point-max))		; In =aux=
  (if (re-search-backward "\\\\bibstyle{.*}" nil t)
    nil
    (set-buffer =buff=)
    (goto-char (point-max))
    (if (bolp) nil (newline))
    (let ((style (read-string "Style [RET=plain  1=unsrt  2=alpha  3=abbrv  else]: ")))
      (cond
	((string-equal style "") (setq style "plain"))
	((string-equal style "1") (setq style "unsrt"))
	((string-equal style "2") (setq style "alpha"))
	((string-equal style "3") (setq style "abbrv")))
      (if (bolp)
	(insert-string "% \\bibliographystyle{" style "}\n")
	(insert-string "\n% \\bibliographystyle{" style "}\n"))
      (set-buffer =aux=)
      (insert-string "\\bibstyle{" style "}\n"))))

(defun tex-bib-bibtex (abort)
  "Execute \"bibtex pre\" at shell, where pre is the prefix of FN."
  (let ((normal t))
    (save-window-excursion
      (message "Sending `bibtex %s' to shell..." =base=)
      (tex-shell-init =buff=)		; Defined in tex-misc.el
      (pop-to-buffer "*shell*")
      (goto-char (point-max))
      (recenter 0)
      (insert-string "bibtex " =base=)
      (setq tex-error-pos (point))		; defined in tex-misc.el
      (shell-send-input)
      (if (y-or-n-p "Continue act/sym substitution? [Wait till finish if `y'] ")
	nil
	(stop-shell-subjob)
	(sleep-for 1)
	(insert-string "kill %")
	(shell-send-input)
	(sleep-for 1)
	(insert "\\rm " =aux= "* " =blg= "*")
	(shell-send-input)
	(setq normal nil))
      (goto-char tex-error-pos)
      (setq tex-bib-err-list nil)
      (while (re-search-forward "database entry for \"\\(\\w.*\\)\"" nil t)
	(setq tex-bib-err-list
	  (cons (vortex-match-string 1)
		tex-bib-err-list)))
      (goto-char (point-max))
      (if (and normal tex-bib-err-list)
	(progn
	  (insert "\\rm " =aux= "* " =blg= "*")
	  (shell-send-input)))
      (if normal nil (throw abort t)))))

(defun tex-bib-get-tag (entry)
  (let ((lst tex-bib-cite-tags) tag)
    (catch 'done
      (while lst
	(setq tag (car lst))
	(if (string-equal entry (car tag))
	  (throw 'done (cdr tag))
	  (setq lst (cdr lst)))))))

(defun tex-bib-correct-error ()
  (message "Correcting citation errors...")
  (let ((=prev= nil)
	(=tags= nil)
        =err= =entry= =start= =end= lst tag epos fn nd)
    (while tex-bib-err-list
      (setq =err= (car tex-bib-err-list))
      (setq tex-bib-err-list (cdr tex-bib-err-list))
      (setq =prev= (cons =err= =prev=))
      (setq =entry= (tex-bib-err-cmd))
      (setq =tags= (cons (cons =entry= (tex-bib-get-tag =err=)) =tags=)))
    (tex-bib-err-sort =tags=)
    (save-excursion
      (while =tags=
        (setq tag (car =tags=))
	(setq lst (cdr tag))
	(set-buffer (find-file-noselect (setq fn (car tag))))
	(setq nd (file-name-nondirectory fn))
        (message "Correcting citation errors---doing \"%s\"..." nd)
	(while lst
	  (setq epos (car lst))
	  (setq =entry= (car epos))
	  (setq =start= (nth 1 epos))
	  (setq =end= (nth 2 epos))
	  (if =entry= 
	    (if (and (integerp =entry=) (< =entry= 0))
	      (if (= =entry= -1)
		(tex-bib-err-comment-out)
		(tex-bib-err-delete))
	      (goto-char =start=)
	      (delete-region =start= =end=)
	      (insert =entry=)))
	  (setq lst (cdr lst)))
	(setq =tags= (cdr =tags=))
        (message "Correcting citation errors---doing \"%s\"...done" nd))))
  (message "Correcting citation errors...done"))

(defun tex-bib-err-sort (tags)
  "Each element in TAGS is (ENTRY (Fn START END) ....).
Sort them with fn being the key."
  (setq =tags= nil)
  (let (tag entry fl fn f1 f2)
    (while tags
      (setq tag (car tags))
      (setq entry (car tag))
      (setq fl (cdr tag))
      (while fl
	(setq f1 (car fl))
	(if (setq f2 (tex-bib-err-tags (setq fn (car f1))))
	  (setq =tags= (cons (cons fn (tex-bib-err-order entry (cdr f1) f2)) =tags=))
	  (setq =tags= (cons (list fn (cons entry (cdr f1))) =tags=)))
	(setq fl (cdr fl)))
      (setq tags (cdr tags)))))

(defun tex-bib-err-order (entry ele lst)
  (let ((front nil))
    (catch 'ok
      (while lst
	(setq tag (car lst))
	(if (> (car ele) (nth 1 tag))
	  (throw 'ok (append (reverse front) (cons (cons entry ele) lst)))
	  (setq front (cons tag front))
	  (setq lst (cdr lst))))
      (reverse (cons (cons entry ele) front)))))

(defun tex-bib-err-tags (fn)
  (let ((front nil) tag)
    (catch 'ok
      (while =tags=
	(if (string-equal fn (car (setq tag (car =tags=))))
	  (throw 'ok (progn (setq =tags= (append front (cdr =tags=))) (cdr tag)))
	  (setq front (cons tag front))
	  (setq =tags= (cdr =tags=))))
      (setq =tags= front)
      nil)))

(defun tex-bib-err-comment-out ()
  (let ((string (buffer-substring =start= =end=))
	cite)
    (goto-char =start=)
    (delete-region =start= =end=)
    (if (looking-at " *,")
      (progn
	(search-forward ",")
	(delete-region =start= (point))))
    (re-search-backward "\\w\\|{")
    (forward-char 1)
    (if (looking-at " *,")
      (progn
	(search-forward ",")
	(delete-region (point) =start=)))
    (search-backward "\\")
    (setq =start= (point))
    (forward-word 1)
    (setq =end= (1+ (point)))
    (if (looking-at "{\\W*}")
	(progn
	  (forward-char 1)
	  (insert string)
	  (goto-char =start=)
	  (insert "[?]")
	  (if (bolp)
	    (insert "%")
	    (insert "\n%"))
	  (search-forward "}")
	  (if (looking-at " *$")
	      nil
	    (insert ?\n)))
      (next-line 1)
      (beginning-of-line)
      (open-line 1)
      (setq cite (buffer-substring =start= =end=))
      (insert ?% cite string "}"))))

(defun tex-bib-err-delete ()
  (delete-region =start= =end=) 		; Delete
  (goto-char =start=)
  (if (looking-at " *,")
    (progn
      (search-forward ",")
      (delete-region =start= (point))))
  (re-search-backward "\\w\\|{")
  (forward-char 1)
  (if (looking-at " *,")
    (progn
      (search-forward ",")
      (delete-region (point) =start=))))

(defun tex-bib-err-cmd ()
  (let (cmd msg)
    (setq msg (concat "Correcting {" =err= "} [SPC  DEL  r  l  c  d  ?=help]"))
    (message msg)
    (setq cmd (read-char))
    (catch 'done
      (while t
	(cond
	  ((= cmd ? )
	   (if tex-bib-err-list
	     nil
	     (if (y-or-n-p "No more next error, done? ")
	       nil
	       (message "Wrapping around...")
	       (sit-for 1)
	       (setq tex-bib-err-list (reverse =prev=))
	       (setq =prev= nil)))
	   (throw 'done nil))
	  ((= cmd ?\177)
	   (if (setq =prev= (cdr =prev=))
	     (progn
	       (setq tex-bib-err-list (cons (car =prev=) (cons =err= tex-bib-err-list)))
	       (setq =prev= (cdr =prev=)))
	     (if (y-or-n-p "No more previous error, done? ")
	       nil
	       (message "Wrapping around...")
	       (sit-for 1)
	       (setq =prev= (reverse (cons =err= tex-bib-err-list)))
	       (setq tex-bib-err-list (list (car =prev=)))
	       (setq =prev= (cdr =prev=))))
	   (throw 'done nil))
	  ((= cmd ?r)
	   (setq  =prev= (cdr =prev=))
	   (throw 'done (read-string (concat "Replacing " =err= " by: ") =err=)))
	  ((= cmd ?l)
	   (setq  =prev= (cdr =prev=))
	   (catch 'abort
	     (throw 'done (car (tex-cite-lookup nil 'abort))))
	   (message msg)
	   (setq cmd (read-char)))
	  ((= cmd ?c)
	   (setq  =prev= (cdr =prev=))
	   (throw 'done -1))
	  ((= cmd ?d)
	   (setq  =prev= (cdr =prev=))
	   (throw 'done -2))
	  ((= cmd ??)
	   (save-window-excursion
	     (tex-bib-error-help)
	     (message msg)
	     (setq cmd (read-char))))
	  (t
	   (ding)
	   (message "%s...WHAT?" msg)
	   (sit-for 1)
	   (message msg)
	   (setq cmd (read-char))))))))
  
(defun tex-bib-error-help ()
  (pop-to-buffer "--- TeX Citation Correction Help ---")
  (if (> (buffer-size) 0)
    (goto-char 1)
    (insert-string
"      SPC -- Show the next citation error.
      DEL -- Show the previous citation error.
        r -- Replace.
        l -- Lookup in .bib files.
        c -- Comment out the entry.
        d -- Delete the entry.
        ? -- This help message.")
    (goto-char 1))
  (other-window 1))

(defun tex-bib-mkbbl-tex ()
  "Creates a .tex file as the bibliography."
  (message "Making \"%s\" as reference file..." =bbl=)
  (save-excursion
    (setq tex-bib-ref-list nil)
    (set-buffer =bbl=)
    (erase-buffer)
    (insert-file-contents =bbl=)
    (goto-char (point-max))             ; Delete stuff from \end
    (search-backward "\\end")
    (previous-line 1)
    (end-of-line)
    (delete-region (point) (point-max))
    (goto-char 1)
    (if (search-forward "\\bibitem" nil t)
      (beginning-of-line)
      (error "Making \"%s\" as reference file...abort (.bbl file is empty)" =bbl=))
    (delete-region 1 (point))     ; Delete stuff before 1st \bibitem
    (if (looking-at "\\\\bibitem\\[.*\\]")
      (let (act)                                      ; Alpha style
	(while (re-search-forward "\\\\bibitem\\[\\(.*\\)\\]{\\(.*\\)}" nil t)
	  (setq act (tex-bib-fix-etal
		     (vortex-match-string 1)))
	  (setq tex-bib-ref-list 
	    (cons (list act (vortex-match-string 2)) tex-bib-ref-list))
	  (insert-string "{")                         ; Bracify 3rd argument
	  (if (search-forward "\\bibitem" nil t)
	    (previous-line 1)
	    (goto-char (point-max)))
	  (re-search-backward "\\w")
	  (end-of-line)
	  (insert-string "}")))
      (let ((n 0) act)                                 ; Other styles
	(while (search-forward "\\bibitem" nil t)
	  (setq act (concat (setq n (1+ n))))
	  (insert ?[ act ?])
	  (re-search-forward "{\\(.*\\)}")
	  (setq tex-bib-ref-list 
	    (cons (list act (vortex-match-string 1))
		  tex-bib-ref-list))
	  (insert-string "{")
	  (if (search-forward "\\bibitem" nil t)
	    (previous-line 1)
	    (goto-char (point-max)))
	  (re-search-backward "\\w")
	  (end-of-line)
	  (insert-string "}"))))
    (goto-char 1)
    (insert-string 
"\\let\\em\\it
\\def\\etalchar#1{$^{#1}$}
\\def\\newblock{\\hskip .11em plus .33em minus -.07em}
\\def\\bibitem[#1]#2#3{{\\bigskip \\item{\\hbox to .6in{\\hss[#1]}}#3\\par}}
\n")
    (write-region 1 (point-max) =bbl= nil 'no-message))
  (message "Making \"%s\" as reference file...done" =bbl=))

(defun tex-bib-fix-etal (citation)
  (let ((md (match-data)))
    (prog1
	(if (string-match "{\\\\etalchar{\\(.\\)}}" citation)
	    (concat
	     (substring citation 0 (match-beginning 0))
	     "{$^{"
	     (substring citation (match-beginning 1) (match-end 1))
	     "}$}"
	     (substring citation (match-end 0)))
	  citation)
      (store-match-data md))))

(defun tex-bib-mkbbl-latex ()
  (message "Making %s as reference file..." =bbl=)
  (save-excursion
    (setq tex-bib-ref-list nil)
    (set-buffer =bbl=)
    (erase-buffer)
    (insert-file-contents =bbl=)
    (goto-char 1)
    (if (search-forward "\\bibitem" nil t)
      (beginning-of-line)
      (error "Making \"%s\" as reference file...abort (.bbl file is empty)" =bbl=))
    (save-excursion
      (if (looking-at "^\\\\bibitem\\[.*\\]")
	  ; Alpha style
	  (while (re-search-forward "\\\\bibitem\\[\\(.*\\)\\]{\\(.*\\)}"
				    nil t)
	    (let ((act (tex-bib-fix-etal (vortex-match-string 1)))
		  (sym (vortex-match-string 2)))
	      (setq tex-bib-ref-list (cons (list act sym) tex-bib-ref-list))))
	(let ((n 0))                             ; Other styles
	  (while (re-search-forward "\\\\bibitem{\\(.*\\)}" nil t)
	    (setq tex-bib-ref-list
	      (cons (list (concat (setq n (1+ n)))
			  (vortex-match-string 1))
		    tex-bib-ref-list)))))))
  (message "Making %s as reference file...done" =bbl=))
      
(defun tex-bib-write-ref ()
  (save-excursion
    (set-buffer =ref=)
    (erase-buffer)
    (insert (prin1-to-string tex-bib-fn-list) ?\n)
    (insert (prin1-to-string tex-bib-rec-list) ?\n)
    (write-region 1 (point-max) =ref= nil 'no-message)))

(defun tex-bib-input-bbl ()
  (save-excursion
    (goto-char (point-max))
    (if (re-search-backward "\\\\end\\|\\\\bye\\|\\\\end{document}\\|\\\\input.*-.*$" nil t)
      (insert "\\input " =bbl= "\n")
      (insert "\n\\input " =bbl= "\n" tex-postamble))))

(defun tex-bib-save (&optional quiet)
  (interactive)
  (let ((lst tex-bib-fn-list)
	(asknot nil)
	fn msg char)
    (if lst
      (progn
        (if quiet nil (message "Saving all files..."))
	(if (vortex-memq (buffer-file-name) lst)
	   nil
	  (setq lst (cons (buffer-file-name) lst)))
	(catch 'finish
	  (while lst
	    (setq fn (car lst))
	    (if (or asknot
		    (tex-confirm (concat "Save file " (file-name-nondirectory fn))))
	      (save-excursion
		(set-buffer (find-file-noselect fn))
		(if (buffer-modified-p) (write-file fn))))
	    (setq lst (cdr lst))))
	(if quiet nil (message "Saving all files...done")))
      (if quiet nil (message "No files changed due to bibliography making")))))

;;;;
;;;; New preprocessing section
;;;; Originally installed by EVM (5/8/90)
;;;;

(defvar tex-bib-bibtex-buffer-name "TeX-bib-BibTeX")
(defvar tex-bib-scan-regexp
  (concat "\\("
	  (mapconcat '(lambda (x) x)
		     '("\\\\cite" "\\\\nocite" "\\\\input\\|\\\\include"
		       "\\\\verb\\*?"
		       "\\\\begin{verbatim\\*?}" "\\\\bibliography")
		     "\\)\\|\\(")
	  "\\)"))

(defvar tex-bib-aux-regexp
  (concat
   "\\\\\\(citation\\|bibdata\\|bibstyle\\|bibcite\\)"
   "{.*}\\({.*}\\)?\n"))

(defun tex-bib-preprocess ()
  "Emulate running latex-bibtex-latex on the current master document."
  (interactive)
  (tex-visit-master-file)
  (let* ((buffer-name (file-name-nondirectory (buffer-file-name)))
	 (file-base
	  (substring buffer-name 0 (string-match "\.tex" buffer-name)))
	 )
    (message "Making .aux file for \"%s\""
	     buffer-name)
    (tex-bib-write-aux buffer-name file-base (tex-bib-scan-file file-base))
    (message "Running BibTeX for \"%s\"" buffer-name)
    (tex-bib-run-bibtex file-base)
    (message
     "Placing cross-references in .aux file for \"%s\""
     buffer-name)
    (tex-bib-update-aux file-base)
    (message
     "Bibliography preprocessing for \"%s\" ... done."
     buffer-name)
    ))

(defun tex-bib-write-aux (buffer-name file-base scan-data)
  (let ((aux-buffer (find-file-noselect (concat file-base ".aux")))
	(bibstyle (or (car scan-data)
		      (tex-bib-get-bibstyle buffer-name)))
	(bibdata (or (nth 1 scan-data)
		     (tex-bib-get-bibdata buffer-name file-base)))
	)
    (save-excursion
      (set-buffer aux-buffer)
      (tex-bib-clean-aux)
      (goto-char (point-max))
      (mapcar
       '(lambda (cite-data)
	  (insert "\\citation" "{" (nth 5 cite-data) "}\n"))
       (nth 2 scan-data))
      (insert "\\bibstyle{" (car bibstyle) "}\n")
      (insert "\\bibdata{" (tex-bib-check-abbrev (car bibdata)) "}\n")
      (set-buffer-modified-p t)
      (save-buffer))
    (kill-buffer aux-buffer)
    ))

(defun tex-bib-clean-aux ()
  "Removes all bibliography commands from the current buffer, which
must be a .aux file."
  (goto-char (point-min))
  (while (re-search-forward tex-bib-aux-regexp nil t)
    (delete-region (match-beginning 0) (match-end 0))
    ))

(defun tex-bib-get-bibdata (buffer-name file-base)
  (let ((bibs (read-string "Base names of .bib files (as f1,f2,...): "
			   file-base)))
    (if (string-equal bibs "") (setq bibs file-base))
    (save-excursion
      (if (y-or-n-p
	   (concat "Should this \\bibliography command be placed in "
		   buffer-name "?"))
	  (save-excursion
	    (goto-char (point-max))
	    (if (not (bolp)) (newline))
	    (let ((begin (point)))
	      (insert "\\bibliography{" (tex-bib-check-abbrev bibs) "}\n")
	      (list bibs begin (1- (point)) (current-buffer))))
	(list bibs nil nil nil)))
    ))

(defun tex-bib-get-bibstyle (buffer-name)
  (let ((style (read-string
		"Style [RET=plain  1=unsrt  2=alpha  3=abbrv  else]: ")))
    (cond
     ((string-equal style "") (setq style "plain"))
     ((string-equal style "1") (setq style "unsrt"))
     ((string-equal style "2") (setq style "alpha"))
     ((string-equal style "3") (setq style "abbrv")))
    (save-excursion
      (if (y-or-n-p
	   (concat "Should this \\bibliographystyle be placed in "
		   buffer-name "?"))
	  (save-excursion
	    (goto-char (point-max))
	    (if (not (bolp)) (newline))
	    (let ((begin (point)))
	      (insert "\\bibliographystyle{" style "}\n")
	      (list style begin (1- (point)) (current-buffer))))
	(list style nil nil nil)))
    ))

;;; -----------------------
;;; File scanning functions
;;; -----------------------

;; TEX-BIB-SCAN-FILE searches the tree of TeX files rooted at the current
;; buffer for bibliographic commands.  It returns a complete summary of 
;; all such commands.  The summary is a list of three elements: bibstyle,
;; bibdata, and cite-list.  Its argument is the filename base for the current
;; buffer.
;;   BIBSTYLE and BIBDATA are lists describing the \bibliographystyle and
;; \bibliography commands.  They are nil if their command was not found.
;; The components of these lists are:
;;      -- the argument to the command
;;      -- the position of the beginning of the command
;;      -- the position of the end of the command
;;      -- the buffer in which the command was found
;;   CITE-LIST is a list describing each citation found in the files.
;; Each element corresponds to a single \cite or \nocite command and the
;; elements are in the order which LaTeX would produce.  Each element is a
;; list containing the following components:
;;      -- a flag which is t for a \cite and nil for a \nocite
;;      -- the optional note argument to \cite, including the brackets,
;;         or nil, if not found
;;      -- a list containing each separate key named in the citation
;;      -- the position of the beginning of the command
;;      -- the position of the end of the command
;;      -- the argument to the command
;;      -- the buffer in which the command was found
;; Thus, \cite[a]{b,c} results in something like
;;   (t "[a]" ("b" "c") 100 115 "b,c" <buffer foo.tex>)

(defun tex-bib-scan-file (file-base)
  (let ((buffer (current-buffer))
	bibstyle bibdata cite-list)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward tex-bib-scan-regexp nil t)
	(let ((md (match-data)))
	  (cond
	   ((save-excursion
	      (prog1
		  (re-search-backward
		   "[^\\]%"
		   (save-excursion (beginning-of-line) (1- (point)))
		   t)
		(store-match-data md)))
	    nil)
	   ((match-beginning 1)		;\cite
	    (setq cite-list
		  (nconc
		   cite-list
		   (list (tex-bib-cite-data (match-beginning 1) buffer)))))
	   ((match-beginning 2)		;\nocite
	    (setq cite-list
		  (nconc
		   cite-list
		   (list (tex-bib-nocite-data (match-beginning 2) buffer)))))
	   ((match-beginning 3)		;\input or \include
	    (let ((child-file-name (tex-bib-get-input-arg)))
	      (if (not (string-match "\\." child-file-name))
		  (setq child-file-name (concat child-file-name ".tex")))
	      (let ((child-file (vortex-file-readable-p child-file-name)))
		(if child-file
		    (save-excursion
		      (set-buffer (find-file-noselect child-file))
		      (let ((retval (tex-bib-scan-file file-base)))
			(setq bibstyle (or (car retval) bibstyle))
			(setq bibdata (or (nth 1 retval) bibdata))
			(setq cite-list (nconc cite-list (nth 2 retval)))))))))
	   ((match-beginning 4)		;\verb
	    (forward-char)
	    (search-forward (char-to-string (preceding-char))))
	   ((match-beginning 5)		;\begin{verbatim\*?}
	    (re-search-forward "\\\\end{verbatim\\*?}" nil t))
	   ((match-beginning 6)		;\bibliography or \bibliographystyle
	    (let ((begin (match-beginning 6)))
	      (if (looking-at "style")
		  (progn
		    (goto-char (match-end 0))
		    (let ((arg (tex-bib-get-braced-arg)))
		      (setq bibstyle (list arg begin (point) buffer))))
		(let ((arg (tex-bib-get-braced-arg))
		      (bbl-file (vortex-file-readable-p
				 (concat file-base ".bbl"))))
		  (setq bibdata (list arg begin (point) buffer))
		  (if bbl-file
		      (save-excursion
			(set-buffer (find-file-noselect bbl-file))
			(setq cite-list
			      (nconc cite-list
				     (nth 2 (tex-bib-scan-file file-base))))
			(kill-buffer (current-buffer))))
		  ))))
	   ))))
    (list bibstyle bibdata cite-list)
    ))

(defun tex-bib-get-braced-arg ()
  (if (looking-at "[ \t]*\n?[ \t]*{\\([^}]+\\)}")
      (progn
	(goto-char (match-end 0))
	(buffer-substring (match-beginning 1) (match-end 1)))))

(defun tex-bib-get-input-arg ()
  (let ((braced-arg (tex-bib-get-braced-arg)))
    (cond
     (braced-arg braced-arg)
     ((looking-at "[ \t]*\n?[ \t]*\\(\\S +\\)")
      (goto-char (match-end 1))
      (buffer-substring (match-beginning 1) (match-end 1))))))

(defun tex-bib-cite-data (begin buffer)
  (if (looking-at
       "\\([ \t]*\n?[ \t]*\\(\\[[^]]*\\]\\)\\)?[ \t]*\n?[ \t]*{\\([^}]+\\)}")
      (let ((note (if (match-beginning 2)
		      (buffer-substring (match-beginning 2) (match-end 2))))
	    (cite-string
	     (tex-bib-strip-comments
	      (if (match-beginning 3)
		  (buffer-substring (match-beginning 3) (match-end 3))))))
	(list t note
	      (vortex-parse-comma-list cite-string)
	      begin (point) cite-string buffer))))

(defun tex-bib-strip-comments (string)
  (let ((comment-regexp "%.*\n")
	(start 0)
	(result ""))
    (while (string-match comment-regexp string start)
      (setq result
	    (concat result
		    (substring string start (match-beginning 0))))
      (setq start (match-end 0))
      )
    (setq result (concat result (substring string start)))
    result))
    
(defun tex-bib-nocite-data (begin buffer)
  (let ((cite-string (tex-bib-get-braced-arg)))
    (list nil nil
	  (vortex-parse-comma-list cite-string)
	  begin (point) cite-string buffer)))

;;;
;;; Running BibTeX
;;; ==============
;;; 
;;; These functions run bibtex and check for errors in its output
;;;

(defun tex-bib-run-bibtex (file-base)
  (vortex-init-process-buffer tex-bib-bibtex-buffer-name)
  (let ((process
	 (start-process "bibtex" tex-bib-bibtex-buffer-name
			"bibtex" file-base)))
    (while (eq 'run (process-status process))
      (sit-for 1))
    (cond
     ((eq (process-status process) 'exit)
      (tex-bib-bibtex-errors tex-bib-bibtex-buffer-name process))
     (t
      (pop-to-buffer tex-bib-bibtex-buffer-name)
      (error "BibTeX stopped running unexpectedly ... Abort!")))
    ))

(defun tex-bib-bibtex-errors (buffer-name bt-process)
  "Throws an error if bibtex had any problems.  Should do 
some real error checking, but doesn't."
  (if (not (eq (process-exit-status bt-process) 0))
      (let ((bt-window (display-buffer buffer-name t)))
	(if (y-or-n-p "BibTeX found some problems.  continue anyway?")
	    (delete-window bt-window)
	  (error "Aborting ...")))))

;;;
;;; Updating the .aux file
;;; ======================
;;;
;;; These functions read the .bbl file produced by bibtex and 
;;; modify the .aux file to show the citation cross-references,
;;; emulating the second run of LaTeX in the normal sequence of 
;;; actions.
;;;

(defun tex-bib-update-aux (file-base)
  (let ((bbl-buffer (find-file-noselect (concat file-base ".bbl")))
	(aux-buffer (find-file-noselect (concat file-base ".aux")))
	(count 0)
	)
    (set-buffer aux-buffer)
    (goto-char (point-max)) 
    (set-buffer bbl-buffer)
    (goto-char (point-min))
    (while (re-search-forward
	     "\\\\bibitem\\(\\[\\(.*\\)\\]\\)?{\\([^}]*\\)}" nil t)
      (setq count (1+ count))
      (let ((key (vortex-match-string 3))
	    (cite-string (if (match-beginning 2) (tex-bib-fix-etal
						  (vortex-match-string 2))))
	    )
	(set-buffer aux-buffer)
	(insert "\\bibcite{" key "}{"
		       (or cite-string (int-to-string count)) "}\n"))
      (set-buffer bbl-buffer)
      )
    (kill-buffer bbl-buffer)
    (set-buffer aux-buffer)
    (set-buffer-modified-p t)
    (save-buffer)
    (kill-buffer aux-buffer)
    ))

;;; -----------------------------
;;; Bibliography Unification Code
;;; -----------------------------


(defconst tex-bib-scratch-buffer-name "TeX-bib Scratch")

(defun tex-bib-unify ()
  "Compose a one-file BibTeX database for this document"
  (interactive)
  (message "Bib Unification: scanning document for citations ...")
  (save-window-excursion
    (save-excursion
      (tex-visit-master-file)
      (let* ((scratch-buffer
	      (generate-new-buffer tex-bib-scratch-buffer-name))
	     (buffer-name (file-name-nondirectory (buffer-file-name)))
	     (file-base
	      (substring buffer-name 0 (string-match "\.tex" buffer-name)))
	     (biblio-data (tex-bib-scan-file file-base))
	     (uid-list (tex-bib-extract-uids biblio-data))
	     (file-list (tex-bib-extract-files biblio-data))
	     )
	(unwind-protect
	     (progn
	       (message "Bib Unification: collecting references ...")
	       (tex-bib-extract-references uid-list file-list scratch-buffer)
	       (message "Bib Unification: collecting abbreviations ...")
	       (tex-bib-extract-abbreviations file-list scratch-buffer)
	       (tex-bib-write-scratch scratch-buffer)
	       (message "Bib Unification: done.")
	       )
	  (set-buffer-modified-p nil)
	  (kill-buffer scratch-buffer))
	))))

(defun tex-bib-extract-uids (biblio-data)
  (let* ((raw-cite-list (nth 2 biblio-data))
	 (simple-cite-list
	  (vortex-mapcan '(lambda (cite) (nth 2 cite)) raw-cite-list))
	 (final-cite-list nil)p
	 )
    (while simple-cite-list
      (if (not (vortex-memq (car simple-cite-list) final-cite-list))
	  (setq final-cite-list (cons (car simple-cite-list) final-cite-list))
	)
      (setq simple-cite-list (cdr simple-cite-list)))
    final-cite-list))

(defun tex-bib-extract-files (biblio-data)
  (vortex-parse-comma-list (car (nth 1 biblio-data))))

(defun tex-bib-extract-references (uid-list file-list scratch-buffer)
  (while file-list
    (let* ((raw-file-name (concat (car file-list) ".bib"))
	   (file-name (vortex-file-readable-p raw-file-name tex-bib-path))
	   )
      (if (not file-name)
	  (error (concat "Could not find file \"" (car file-list) "\".")))
      (save-excursion
	(let ((bib-buffer (set-buffer (find-file-noselect file-name))))
	  (setq uid-list
		(delq nil (mapcar '(lambda (uid)
				    (tex-bib-search-and-copy-reference
				     uid bib-buffer scratch-buffer))
				  uid-list))))
      (setq file-list (cdr file-list))
      )))
  (if uid-list
      (progn
	(beep)
	(if (not (y-or-n-p "Not all references found.  Continue anyway?"))
	    (error "Bibliography composition ... aborted."))))
  )

(defun tex-bib-search-and-copy-reference (uid bib-buffer scratch-buffer)
  (goto-char 1)
  (if (catch 'found
	(while (search-forward uid nil t)
	  (let ((entry-data (bibtex-current-entry t)))
	    (if (string-match uid (nth 2 (nth 3 entry-data)))
		(progn
		  (set-buffer scratch-buffer)
		  (insert-buffer-substring bib-buffer (car entry-data)
					   (nth 1 entry-data))
		  (insert "\n")
		  (set-buffer bib-buffer)
		  (throw 'found t))))))
      nil
    uid))

(defun tex-bib-write-scratch (scratch-buffer)
  (let ((file-name (read-file-name "File for storing references: ")))
    (while (not (file-writable-p file-name))
      (beep)
      (setq file-name
	    (read-file-name
	     "File not writable. Alternate file? " nil file-name)))
    (set-buffer scratch-buffer)
    (write-file file-name)))

(defun tex-bib-extract-abbreviations (file-list scratch-buffer)
  (let ((abbrev-list (tex-bib-abbrev-list scratch-buffer)))
    (while file-list
      (let* ((raw-file-name (concat (car file-list) ".bib"))
	     (file-name (vortex-file-readable-p raw-file-name tex-bib-path))
	     (bib-buffer (find-file-noselect file-name)))
	(setq abbrev-list
	      (delq nil (mapcar '(lambda (abbrev)
				  (tex-bib-search-and-copy-abbreviation
				   abbrev bib-buffer scratch-buffer))
				abbrev-list))))
      (setq file-list (cdr file-list)))
    (if (and abbrev-list
	     (not (beep))
	     (not (y-or-n-p "Not all abbreviations found.  Continue anyway?")))
	(error "Bibliography composition ... aborted."))
    ))

(defun tex-bib-search-and-copy-abbreviation (abbrev bib-buffer scratch-buffer)
  (save-excursion
    (set-buffer bib-buffer)
    (goto-char (point-min))
    (let ((case-fold-search t))
      (if (re-search-forward
	   (concat "@string\\s *\\([{(]\\)\\s *" abbrev "\\s *=") nil t)
	  (let* ((string-start (match-beginning 0))
		 (left-brace (match-beginning 1))
		 (right-brace (save-excursion
				(goto-char left-brace)
				(forward-sexp)
				(point)))
		 )
	    (set-buffer scratch-buffer)
	    (insert-buffer-substring bib-buffer string-start right-brace)
	    (insert "\n")
	    (set-buffer bib-buffer)
	    nil)
	abbrev))))

(defun tex-bib-abbrev-list (scratch-buffer)
  (set-buffer scratch-buffer)
  (let ((abbrev-list nil)
	(current-entry nil)
	(current-field nil)
	(case-fold-search t)
	)
    ;; make certain that first entry won't be skipped
    (goto-char (point-min))
    (newline)
    (goto-char (point-min))
    (while (setq current-entry (bibtex-next-entry 1 t))
      (goto-char (nth 1 (nth 3 current-entry)))
      (catch 'past-end
	(while (setq current-field (bibtex-next-field-data 1))
	  (if (> (car current-field) (nth 1 current-entry))
	      (throw 'past-end nil))
	  (let ((field-text (downcase (nth 2 (nth 3 current-field)))))
	    (goto-char (car (nth 3 current-field)))
	    (if (and (not (re-search-backward "[\"{]" (car current-field) t))
		     (not (string-match tex-bib-month-regexp field-text))
		     (not (vortex-memq field-text abbrev-list)))
		(setq abbrev-list (cons field-text abbrev-list)))))
	))
    (goto-char (point-min))
    abbrev-list))

