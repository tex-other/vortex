Copyright (c) 1985-1992 The Regents of the University of California.
All rights reserved.

Permission is hereby granted, without written agreement and without
license or royalty fees, to use, copy, modify, and distribute this
software and its documentation for any purpose, provided that the
above copyright notice and the following two paragraphs appear in
all copies of this software.

IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY FOR
DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES, INCLUDING LOST PROFITS, ARISING OUT
OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF THE UNIVERSITY OF
CALIFORNIA HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY WARRANTIES,
INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
AND FITNESS FOR A PARTICULAR PURPOSE.  THE SOFTWARE PROVIDED HEREUNDER IS
ON AN "AS IS" BASIS, AND THE UNIVERSITY OF CALIFORNIA HAS NO OBLIGATION TO
PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.

README.EMACS

The software in this directory is a suite of GNU Emacs Lisp programs
to improve your TeX environment.  It was created as part of VorTeX, a
research project at the University of California, Berkeley which,
under the supervision of Prof. Michael Harrison, developed an
interactive environment for TeX using the X10 window system.

The VorTeX project is no longer active and these GNU Emacs modes are
no longer being actively supported.  Because of that, the modes have
been released for unrestricted distribution within the limits
described above.  While several of us who have worked on this code
have strong sentimental attachment to it, we have NO plans to provide
any support.  We welcome others to the task of improving this powerful
environment for TeX documents and BibTeX databases.

This software is quite useful, but has numerous bugs and non-bug
problems:

-- The spelling routines in TeX-mode do not correctly check spelling
on a file the second time.  That is, once you've checked the file
once, the routines ask if you want to recheck the spelling.  They fail
because of a problem with the "hash file".

-- The routines in BibTeX-mode which are supposed to spot errors
reported by BibTeX don't work because the error strings for BibTeX
have changed since the code was written.  Also, they assume that the
BibTeX process will return a non-zero value if there are any errors,
which is not the case anymore.

-- The user interface to many of the longer tasks is quite modal and
the key bindings when in one of these modes can be confusing.

-- The code makes extensive use of dynamic binding of lisp variables,
which makes it pretty hard to figure out what is going on.  So, if you
try to modify the code, be patient --- it can take a while to figure
out how things are done.  Newer code avoids using dynamic binding.

-- The "automatic citation" feature in TeX-mode (C-c C-b c) should be
generalized to create a BibTeX database browser.

As a final note, the "bibliography preprocessing" feature in TeX-mode
(C-c C-b p) is actually extremely useful.  In fact, the integration of
TeX-mode and BibTeX-mode is a key reason to prefer these modes over
the standard GNU modes and AUCTeX.

INSTALLATION INSTRUCTIONS

You may have to modify some of the variable bindings in order to use
these programs.  Anything you need to modify should be found in the
first parts of TeX-mode.el and BibTeX-mode.el.

To install these routines, just copy all files matching [BTV]*.el{,c}
into your public Emacs Lisp directory (we use
/usr/sww/lib/emacs/lisp).  Then, individual users can modify their
.emacs files according to the directions in the documentation to have
these files loaded automatically.  Alternatively, you can change
site-init.el in your Emacs build process so that our TeX-mode and
BibTeX-mode get used rather than the more spartan ones distributed
with GNU Emacs.

We have two major systems in this release: TeX-mode and BibTeX-mode, both are
of version 1.12 (Jan. 1992).  The main documentation is for version 1.5 and
is therefore a bit obsolete, but it should suffice for most of what you
want to do.  There is also a short document describing three new
features.  We would be particularly interested in feedback on the
usefulness of these new features.  They are described in
./doc/newfeatures.tex, which is a LaTeX document using the CM fonts.
We are discussing the possibility of producing a new version of the
manuals. 


I. The first system is TeX-mode which is functionally divided into
   the following files:
  
  (1) The file "TeX-mode.el" defines `tex-mode' which sets up mode features
      such as syntax table modifications, key bindings, local variables, etc.
      and autoloads the other TeX-mode files.  Code for dollar sign ($) and
      double quote (") balancing is also defined in this file.
  
  (2) The file "TeX-match.el" contains an assorted set of functions for
      the various delimiter matching schemes available in TeX-mode.
  
  (3) The file "TeX-custom.el" contains facilities to define new
      delimiters under the same paradigm as provided by "TeX-match.el".
  
  (4) The file "TeX-misc.el" contains miscellaneous TeX supporting functions 
      such as TeX/AmSTeX/LaTeX/SliTeX execution, error positioning, ..., etc.
  
  (5) The file "TeX-spell.el" is a spelling checking facility.
  
  (6) The file "TeX-bib.el" performs actual/symbolic citation substitution.
      In earlier versions of TeX-mode, this file also contained reference
      inspection and automatic citation routines.
 
  (7) The file "TeX-index.el" provides facilities to enter \index entries
      and to transform the |.idx| file generated by the formatter to
      the actual index, the |.ind| file.  The program `makeindex'
      also comes with this distribution, as it is essential to the
      transformation.  Install makeindex in /usr/local.

  (8) The file "TeX-cite.el" contains an automatic citation facility
      which supports two styles of reference database queries.

  (9) The file "TeX-insp.el" contains the reference inspection
      facility.

   The document ./doc/tex-mode.tex describes how to use each user-level
   function with instructive examples.  It describes version 1.5 of the system.
   The file is in TeX 2.0 and uses the CM fonts.

   Version 1.11 has some different key bindings and a couple of new
   functions.  Please refer to C-c C-h for the most current command set.
   The most substantial changes are the addition of a new query method
   for automatic citation and reference inspection using X window system
   previewers.  These two new features are described in ./doc/newfeatures.tex.
   A supplementary document (spell.ascii) describing a new feature
   of the spelling subsystem is also included in ./doc. Version 1.12
   is only different in small ways.


II. The second system is BibTeX-mode which is an interface
    to BibTeX databases.  With the mode autoloaded, you would be able
    to edit your .bib files or preview them easily.

  (1) The file "BibTeX-mode.el" defines basic attributes to be used
      in the mode.

  (2) The file "BibTeX-ops.el" contains supporting functions for scrolling,
      copying, deleting, etc. entries and fields.

  (3) The file "BibTeX-abv.el" deals with compiling, loading, browsing,
      and interpolation of abbreviations.  It supports not only the
      field abbreviation paradigm as defined by BibTeX, but an extended
      group abbreviation scheme as well.
 
  (4) The file "BibTeX-misc.el" defines a draft making/previewing facility and
      a mechanism to position to the next error found by BibTeX.

  (5) The file "BibTeX-notes.el" defines an annotation facility which will
      allow you to attach files of annotations to the entries in your
      reference database.

   The document ./doc/bibtex-mode.tex describes how to use each user-level
   function with instructive examples.  It is for version 1.5 of the system.
   The file is in TeX 2.0 and uses the CM fonts.

   The only substantive changes between v1.5 and v1.11 are the addition
   of the annotation facility and the new form of bibliography
   preprocessing.  Otherwise, the only significant differences
   are bug fixes and a few key binding changes.  Always refer to C-c C-h
   for the most up-to-date command set.  The annotation facility
   is described in ./doc/newfeatures.tex. V1.11 also features much faster
   movement between fields.  v1.12 has only minor changes.


						Pehong Chen
						Michael Harrison
						Ethan Munson
