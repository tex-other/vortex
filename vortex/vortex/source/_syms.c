/* 
 * Copyright (c) 1987 The Regents of the University of California.
 * All rights reserved.
 *
 * Permission is hereby granted, without written agreement and without
 * license or royalty fees, to use, copy, modify, and distribute this
 * software and its documentation for any purpose, provided that the
 * above copyright notice and the following two paragraphs appear in
 * all copies of this software.
 * 
 * IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY FOR
 * DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES, INCLUDING LOST PROFITS, ARISING OUT
 * OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF THE UNIVERSITY OF
 * CALIFORNIA HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY WARRANTIES,
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
 * AND FITNESS FOR A PARTICULAR PURPOSE.  THE SOFTWARE PROVIDED HEREUNDER IS
 * ON AN "AS IS" BASIS, AND THE UNIVERSITY OF CALIFORNIA HAS NO OBLIGATION TO
 * PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.
 *
 * The above licensing information supersedes all licensing information
 * below.
 */

/*
 *  VorTeX -- Visually ORiented TeX
 *
 *  Peehong Chen, John Coker, Jeff McCarrell, Steve Procter
 *  for Prof. Michael Harrison of the Computer Science Division
 *  University of California, Berkeley
 *
 *  This file was created automatically from the builtin
 *  symbols extracted from the C source files that make up
 *  VorTeX.  This file is expendable; it will be re-created
 *  every time VorTeX is linked.  Some part of the system should
 *  call the functions builtin_values() and builtin_symbols()
 *  created here during early initialization.
 *
 *  See the exsym/mksym utilities for a description of the
 *  method by which the builtin symbol database is created
 *  and scanned to produce this file, which is linked into
 *  VorTeX to define the basic symbols (builtin functions
 *  and standard variables).
 *
 *  Created: Thu Jul 14 21:55:34 1988
 */

#include "value.h"
#include "symtab.h"

extern unsigned long bdata_doXfetch, bdata_doXgetbdwidth,
	bdata_doXgetdisp, bdata_doXgetfocus, bdata_doXgetfont,
	bdata_doXgetgeom, bdata_doXlower, bdata_doXraise,
	bdata_doXsetbdwidth, bdata_doXsetfocus, bdata_doXsetfont,
	bdata_doXsetgeom, bdata_doXstore, bdata_doabort, bdata_doabs,
	bdata_doaccess, bdata_doadd, bdata_doallocate, bdata_doand,
	bdata_doappend, bdata_doappendstring, bdata_doarray,
	bdata_doarrayp, bdata_doatomp, bdata_doautoalloc,
	bdata_dobackquote, bdata_dobacktrace, bdata_dobeep,
	bdata_dobopen, bdata_doboundp, bdata_dobreakloop,
	bdata_dobufferp, bdata_dobuffile, bdata_dobuflen,
	bdata_dobufmodified, bdata_dobufname, bdata_dobufreadonly,
	bdata_dobufstart, bdata_dobuftype, bdata_docar, bdata_docat,
	bdata_docatch, bdata_docdr, bdata_docerror, bdata_dochdir,
	bdata_dochmod, bdata_doclose, bdata_doclosedoc,
	bdata_docompletelist, bdata_doconcat, bdata_doconcata,
	bdata_docond, bdata_doconfirm, bdata_docons, bdata_docont,
	bdata_docopen, bdata_docopy, bdata_docopychars,
	bdata_docopyregion, bdata_docurprfwin, bdata_docurrentdoc,
	bdata_docursrcwin, bdata_docurwindow, bdata_dodeactivatewin,
	bdata_dodebug, bdata_dodeclarevarloc, bdata_dodefun,
	bdata_dodelbackward, bdata_dodelforward, bdata_dodelregion,
	bdata_dodigit, bdata_dodirs, bdata_dodtprp,
	bdata_dodumpbindings, bdata_dodumpbuffer, bdata_doeq,
	bdata_doequal, bdata_doerasebuf, bdata_doerror, bdata_doeval,
	bdata_doevalexpr, bdata_doevenp, bdata_doexec, bdata_doexit,
	bdata_doexitvortex, bdata_doexplode, bdata_doexplodec,
	bdata_doexploden, bdata_doextcommand, bdata_dofakedoc,
	bdata_dofillarray, bdata_dofindfile, bdata_dofindmatch,
	bdata_dofindregexp, bdata_doflock, bdata_doflush,
	bdata_dofollowchar, bdata_dofopen, bdata_doforallbufs,
	bdata_doforallvars, bdata_doforallwins, bdata_doformat,
	bdata_doformatdoc, bdata_dofuncall, bdata_dofunctionp,
	bdata_dogc, bdata_dogenprefix, bdata_dogensym,
	bdata_dogetbinding, bdata_dogetenv, bdata_dogethostname,
	bdata_dogethostnameonly, bdata_dogetintborder, bdata_dogetplist,
	bdata_dogetprefix, bdata_dogetprop, bdata_doglobalmap,
	bdata_dogo, bdata_dogreatereqp, bdata_dogreaterp, bdata_doif,
	bdata_doindex, bdata_doinsertchar, bdata_doinsertfile,
	bdata_doinsertstring, bdata_dointeractive, bdata_dokill,
	bdata_dokillformat, bdata_dokillproof, bdata_dokillwindow,
	bdata_dolambda, bdata_dolength, bdata_dolesseqp, bdata_dolessp,
	bdata_dolet, bdata_dolexpr, bdata_dolist, bdata_dolistbufs,
	bdata_dolistp, bdata_doload, bdata_doloaddoc, bdata_dolocalmap,
	bdata_dolocatorpos, bdata_doloop, bdata_domacro,
	bdata_domakeconn, bdata_domakedoc, bdata_domakekeymap,
	bdata_domakelocalvar, bdata_domakesparse, bdata_domapc,
	bdata_domapcar, bdata_domarkpos, bdata_domax, bdata_domemberp,
	bdata_domemeqp, bdata_domenu, bdata_domessage,
	bdata_dometadigit, bdata_dometaprefix, bdata_domin,
	bdata_dominbcomplete, bdata_dominbhelp, bdata_dominbinput,
	bdata_dominbprint, bdata_dominbreturn, bdata_domod,
	bdata_donewline, bdata_donextline, bdata_donextregexp,
	bdata_donlambda, bdata_donot, bdata_donotify, bdata_donth,
	bdata_donthcdr, bdata_donullp, bdata_donumberp, bdata_dooddp,
	bdata_door, bdata_dopatom, bdata_dopipe, bdata_dopointpos,
	bdata_dopopbuffer, bdata_dopopd, bdata_dopopopmesg, bdata_dopp,
	bdata_doprecchar, bdata_doprevline, bdata_doprfgotopage,
	bdata_doprfmoveabs, bdata_doprfmoverel, bdata_doprfmoveto,
	bdata_doprfnextpage, bdata_doprfselect, bdata_doprfselregion,
	bdata_doprinc, bdata_doprint, bdata_doprog, bdata_doproofdoc,
	bdata_dopushd, bdata_doputprop, bdata_doquote,
	bdata_doquotedinsert, bdata_doratom, bdata_doread,
	bdata_doreadc, bdata_dorecenter, bdata_doremob,
	bdata_doremovebuf, bdata_doremovelocals, bdata_doremprop,
	bdata_doreset, bdata_doreturn, bdata_doreverse, bdata_dorplaca,
	bdata_dorplacd, bdata_doscrollback, bdata_doscrollfwd,
	bdata_dosearchbackwd, bdata_dosearchbkregexp,
	bdata_dosearchforwd, bdata_doseek, bdata_doselfinsert,
	bdata_doselmore, bdata_doset, bdata_dosetbinding,
	bdata_dosetenv, bdata_dosetintborder, bdata_dosetmark,
	bdata_dosetplist, bdata_dosetpoint, bdata_dosetprefix,
	bdata_dosetq, bdata_dosetwindowmark, bdata_dosetwindowpoint,
	bdata_dosoftlim, bdata_dosopen, bdata_dosplitwindow,
	bdata_dosrchfwdregex, bdata_dostartformat, bdata_dostartproof,
	bdata_dostat, bdata_dostatus, bdata_dostore, bdata_dostridx,
	bdata_dostridxr, bdata_dostringp, bdata_dostrlen,
	bdata_dostrmapc, bdata_dosubstr, bdata_doswitchbuf,
	bdata_doswitchwindow, bdata_dosymbolp, bdata_dosyntax,
	bdata_doterpri, bdata_dothrow, bdata_dotime, bdata_dotimecmp,
	bdata_doumask, bdata_douseglobal, bdata_douselocal,
	bdata_douwprotect, bdata_dovolume, bdata_dowait, bdata_dowhile,
	bdata_dowindowmark, bdata_dowindowp, bdata_dowindowpoint,
	bdata_dowritefile, bdata_dowritenamed, bdata_doyesorno,
	bdata_dozerop, bdata_findcharback, bdata_findcharfwd;

struct value bvl_doXfetch, bvl_doXgetbdwidth, bvl_doXgetdisp,
	bvl_doXgetfocus, bvl_doXgetfont, bvl_doXgetgeom, bvl_doXlower,
	bvl_doXraise, bvl_doXsetbdwidth, bvl_doXsetfocus,
	bvl_doXsetfont, bvl_doXsetgeom, bvl_doXstore, bvl_doabort,
	bvl_doabs, bvl_doaccess, bvl_doadd, bvl_doallocate, bvl_doand,
	bvl_doappend, bvl_doappendstring, bvl_doarray, bvl_doarrayp,
	bvl_doatomp, bvl_doautoalloc, bvl_dobackquote, bvl_dobacktrace,
	bvl_dobeep, bvl_dobopen, bvl_doboundp, bvl_dobreakloop,
	bvl_dobufferp, bvl_dobuffile, bvl_dobuflen, bvl_dobufmodified,
	bvl_dobufname, bvl_dobufreadonly, bvl_dobufstart, bvl_dobuftype,
	bvl_docar, bvl_docat, bvl_docatch, bvl_docdr, bvl_docerror,
	bvl_dochdir, bvl_dochmod, bvl_doclose, bvl_doclosedoc,
	bvl_docompletelist, bvl_doconcat, bvl_doconcata, bvl_docond,
	bvl_doconfirm, bvl_docons, bvl_docont, bvl_docopen, bvl_docopy,
	bvl_docopychars, bvl_docopyregion, bvl_docurprfwin,
	bvl_docurrentdoc, bvl_docursrcwin, bvl_docurwindow,
	bvl_dodeactivatewin, bvl_dodebug, bvl_dodeclarevarloc,
	bvl_dodefun, bvl_dodelbackward, bvl_dodelforward,
	bvl_dodelregion, bvl_dodigit, bvl_dodirs, bvl_dodtprp,
	bvl_dodumpbindings, bvl_dodumpbuffer, bvl_doeq, bvl_doequal,
	bvl_doerasebuf, bvl_doerror, bvl_doeval, bvl_doevalexpr,
	bvl_doevenp, bvl_doexec, bvl_doexit, bvl_doexitvortex,
	bvl_doexplode, bvl_doexplodec, bvl_doexploden, bvl_doextcommand,
	bvl_dofakedoc, bvl_dofillarray, bvl_dofindfile, bvl_dofindmatch,
	bvl_dofindregexp, bvl_doflock, bvl_doflush, bvl_dofollowchar,
	bvl_dofopen, bvl_doforallbufs, bvl_doforallvars,
	bvl_doforallwins, bvl_doformat, bvl_doformatdoc, bvl_dofuncall,
	bvl_dofunctionp, bvl_dogc, bvl_dogenprefix, bvl_dogensym,
	bvl_dogetbinding, bvl_dogetenv, bvl_dogethostname,
	bvl_dogethostnameonly, bvl_dogetintborder, bvl_dogetplist,
	bvl_dogetprefix, bvl_dogetprop, bvl_doglobalmap, bvl_dogo,
	bvl_dogreatereqp, bvl_dogreaterp, bvl_doif, bvl_doindex,
	bvl_doinsertchar, bvl_doinsertfile, bvl_doinsertstring,
	bvl_dointeractive, bvl_dokill, bvl_dokillformat,
	bvl_dokillproof, bvl_dokillwindow, bvl_dolambda, bvl_dolength,
	bvl_dolesseqp, bvl_dolessp, bvl_dolet, bvl_dolexpr, bvl_dolist,
	bvl_dolistbufs, bvl_dolistp, bvl_doload, bvl_doloaddoc,
	bvl_dolocalmap, bvl_dolocatorpos, bvl_doloop, bvl_domacro,
	bvl_domakeconn, bvl_domakedoc, bvl_domakekeymap,
	bvl_domakelocalvar, bvl_domakesparse, bvl_domapc, bvl_domapcar,
	bvl_domarkpos, bvl_domax, bvl_domemberp, bvl_domemeqp,
	bvl_domenu, bvl_domessage, bvl_dometadigit, bvl_dometaprefix,
	bvl_domin, bvl_dominbcomplete, bvl_dominbhelp, bvl_dominbinput,
	bvl_dominbprint, bvl_dominbreturn, bvl_domod, bvl_donewline,
	bvl_donextline, bvl_donextregexp, bvl_donlambda, bvl_donot,
	bvl_donotify, bvl_donth, bvl_donthcdr, bvl_donullp,
	bvl_donumberp, bvl_dooddp, bvl_door, bvl_dopatom, bvl_dopipe,
	bvl_dopointpos, bvl_dopopbuffer, bvl_dopopd, bvl_dopopopmesg,
	bvl_dopp, bvl_doprecchar, bvl_doprevline, bvl_doprfgotopage,
	bvl_doprfmoveabs, bvl_doprfmoverel, bvl_doprfmoveto,
	bvl_doprfnextpage, bvl_doprfselect, bvl_doprfselregion,
	bvl_doprinc, bvl_doprint, bvl_doprog, bvl_doproofdoc,
	bvl_dopushd, bvl_doputprop, bvl_doquote, bvl_doquotedinsert,
	bvl_doratom, bvl_doread, bvl_doreadc, bvl_dorecenter,
	bvl_doremob, bvl_doremovebuf, bvl_doremovelocals, bvl_doremprop,
	bvl_doreset, bvl_doreturn, bvl_doreverse, bvl_dorplaca,
	bvl_dorplacd, bvl_doscrollback, bvl_doscrollfwd,
	bvl_dosearchbackwd, bvl_dosearchbkregexp, bvl_dosearchforwd,
	bvl_doseek, bvl_doselfinsert, bvl_doselmore, bvl_doset,
	bvl_dosetbinding, bvl_dosetenv, bvl_dosetintborder,
	bvl_dosetmark, bvl_dosetplist, bvl_dosetpoint, bvl_dosetprefix,
	bvl_dosetq, bvl_dosetwindowmark, bvl_dosetwindowpoint,
	bvl_dosoftlim, bvl_dosopen, bvl_dosplitwindow,
	bvl_dosrchfwdregex, bvl_dostartformat, bvl_dostartproof,
	bvl_dostat, bvl_dostatus, bvl_dostore, bvl_dostridx,
	bvl_dostridxr, bvl_dostringp, bvl_dostrlen, bvl_dostrmapc,
	bvl_dosubstr, bvl_doswitchbuf, bvl_doswitchwindow,
	bvl_dosymbolp, bvl_dosyntax, bvl_doterpri, bvl_dothrow,
	bvl_dotime, bvl_dotimecmp, bvl_doumask, bvl_douseglobal,
	bvl_douselocal, bvl_douwprotect, bvl_dovolume, bvl_dowait,
	bvl_dowhile, bvl_dowindowmark, bvl_dowindowp, bvl_dowindowpoint,
	bvl_dowritefile, bvl_dowritenamed, bvl_doyesorno, bvl_dozerop,
	bvl_findcharback, bvl_findcharfwd;

extern struct string *ABORTCATCH, *ABORTONINT_NAME, *AGAINMSG,
	*ARGLIST_NAME, *ARGZERO_NAME, *AUTOACTIVE_NAME, *AUTORAISE_NAME,
	*AUTOWARP_NAME, *BACKTRACE_BUF, *BINDINGS, *BQUOTESTR, *BREAKABORT,
	*BREAKONINT_NAME, *BREAKVAR, *CONNPORT_NAME, *CURSOR_INVERSE,
	*CURSOR_OUTLINE, *CURSOR_UNDERLINE, *DEFMODEFMTVAL, *DEFMODESTRVAL,
	*DEFPROMPT, *DOCSTRVAR_NAME, *ECHOPREFIX_NAME, *ERRORCATCH,
	*FORMATHOST_NAME, *FORMATPORT_NAME, *FORMATPROG_NAME,
	*GLOBALMODE_NAME, *GSLEADER, *HELLOSTR, *HELPBUFFER_NAME, *HOME_NAME,
	*INTERACTSTR, *KEYMAP, *LAMBDASTR, *LEXPRSTR, *LISTBUF_NAME,
	*LOADPATH_NAME, *LOCALMODE_NAME, *MACROSTR, *MINBMODEFMTVAL,
	*MINBMODESTRVAL, *MINIBUF_NAME, *MODEFMT_NAME, *NIL_NAME,
	*NLAMBDASTR, *NO_STR, *OVERWRITE_NAME, *PROGGOSTR, *PROGRETSTR,
	*PROMPTSTR, *PROOFHOST_NAME, *PROOFPORT_NAME, *PROOFPROG_NAME,
	*PROOF_CREATE, *PROOF_DESTROY, *QUOTESTR, *SCRATCH_NAME, *STATLEADER,
	*STDIN_NAME, *STDOUT_NAME, *TABWIDTH, *TIMELEADER, *TOPLEVCATCH,
	*TRUNCATE, *T_NAME, *UNDEFNAME, *WRITEUNMOD_NAME, *YES_OR_NO_FUNC,
	*YES_STR;

extern struct value bvl_domod, bvl_doadd, bvl_doadd, bvl_doadd,
	bvl_doadd, bvl_dolessp, bvl_dolesseqp, bvl_dogreaterp,
	bvl_dogreatereqp, bvl_doabort, bvl_doabs, bvl_dometaprefix,
	bvl_doaccess, bvl_doadd, bvl_doallocate, bvl_doand,
	bvl_doappend, bvl_doappendstring, bvl_doarray, bvl_doarrayp,
	bvl_doatomp, bvl_doautoalloc, bvl_dobackquote, bvl_dobacktrace,
	bvl_dobeep, bvl_dobopen, bvl_doboundp, bvl_dobreakloop,
	bvl_dobuffile, bvl_dobuflen, bvl_dobufmodified, bvl_dobufname,
	bvl_dobufreadonly, bvl_dobufstart, bvl_dobuftype, bvl_dobufferp,
	bvl_docar, bvl_docat, bvl_docatch, bvl_dochdir, bvl_docdr,
	bvl_docerror, bvl_dochmod, bvl_doclose, bvl_doclosedoc,
	bvl_docompletelist, bvl_doconcat, bvl_doconcata, bvl_docond,
	bvl_doconfirm, bvl_docons, bvl_docont, bvl_docopen, bvl_docopy,
	bvl_docopychars, bvl_docopyregion, bvl_docurrentdoc,
	bvl_docurprfwin, bvl_docursrcwin, bvl_docurwindow,
	bvl_dodeactivatewin, bvl_dodebug, bvl_dodeclarevarloc,
	bvl_dodefun, bvl_dodelbackward, bvl_dodelforward,
	bvl_dodelregion, bvl_dodigit, bvl_dodirs, bvl_dofindmatch,
	bvl_dofindregexp, bvl_donextregexp, bvl_dodtprp,
	bvl_dodumpbindings, bvl_dodumpbuffer, bvl_doeq, bvl_doequal,
	bvl_doerasebuf, bvl_doerror, bvl_doeval, bvl_doevalexpr,
	bvl_doevenp, bvl_doexec, bvl_doexit, bvl_doexitvortex,
	bvl_doexplode, bvl_doexplodec, bvl_doexploden, bvl_doextcommand,
	bvl_dofakedoc, bvl_dofillarray, bvl_findcharback,
	bvl_findcharfwd, bvl_dofindfile, bvl_doflock, bvl_doflush,
	bvl_dofollowchar, bvl_dofopen, bvl_doforallbufs,
	bvl_doforallvars, bvl_doforallwins, bvl_doformat,
	bvl_doformatdoc, bvl_dofuncall, bvl_dofunctionp, bvl_dogc,
	bvl_dogenprefix, bvl_dogensym, bvl_dogetbinding,
	bvl_dogetprefix, bvl_dogetintborder, bvl_dogetenv,
	bvl_dogethostname, bvl_dogethostnameonly, bvl_dogetplist,
	bvl_dogetprop, bvl_doglobalmap, bvl_dogo, bvl_dogreatereqp,
	bvl_dogreaterp, bvl_doif, bvl_doindex, bvl_doinsertchar,
	bvl_doinsertfile, bvl_doinsertstring, bvl_dointeractive,
	bvl_dokill, bvl_doremovebuf, bvl_dokillformat, bvl_dokillproof,
	bvl_dokillwindow, bvl_dolambda, bvl_dolength, bvl_dolesseqp,
	bvl_dolessp, bvl_dolet, bvl_dolexpr, bvl_dolist, bvl_dolistbufs,
	bvl_dolistp, bvl_doload, bvl_doloaddoc, bvl_dolocalmap,
	bvl_dolocatorpos, bvl_doloop, bvl_domacro, bvl_domakeconn,
	bvl_domakedoc, bvl_domakekeymap, bvl_domakelocalvar,
	bvl_domakesparse, bvl_domapc, bvl_domapcar, bvl_domarkpos,
	bvl_doconcata, bvl_domax, bvl_domemberp, bvl_domemeqp,
	bvl_domenu, bvl_domessage, bvl_dometadigit, bvl_domin,
	bvl_dominbcomplete, bvl_dominbhelp, bvl_dominbinput,
	bvl_dominbprint, bvl_dominbreturn, bvl_doadd, bvl_domod,
	bvl_donewline, bvl_donextline, bvl_donlambda, bvl_donot,
	bvl_donotify, bvl_donth, bvl_donthcdr, bvl_donullp,
	bvl_donumberp, bvl_dooddp, bvl_door, bvl_dopatom, bvl_dopipe,
	bvl_dopointpos, bvl_dopopbuffer, bvl_dopopd, bvl_dopopopmesg,
	bvl_dopp, bvl_doprecchar, bvl_doprevline, bvl_doprinc,
	bvl_doprint, bvl_doprog, bvl_doproofdoc, bvl_doprfgotopage,
	bvl_doprfmoveabs, bvl_doprfmoverel, bvl_doprfmoveto,
	bvl_doprfnextpage, bvl_doprfselect, bvl_doselmore,
	bvl_doprfselregion, bvl_dopushd, bvl_doputprop, bvl_doquote,
	bvl_doquotedinsert, bvl_doadd, bvl_doratom, bvl_doread,
	bvl_doreadc, bvl_dorecenter, bvl_doremob, bvl_doremovelocals,
	bvl_doremprop, bvl_doreset, bvl_doreturn, bvl_doreverse,
	bvl_dorplaca, bvl_dorplacd, bvl_doscrollback, bvl_doscrollfwd,
	bvl_dosearchbackwd, bvl_dosearchbkregexp, bvl_dosearchforwd,
	bvl_dosrchfwdregex, bvl_doseek, bvl_doselfinsert, bvl_doset,
	bvl_dosetbinding, bvl_dosetprefix, bvl_dosetintborder,
	bvl_dosetmark, bvl_dosetpoint, bvl_dosetwindowmark,
	bvl_dosetwindowpoint, bvl_dosetenv, bvl_dosetplist, bvl_dosetq,
	bvl_dosoftlim, bvl_dosopen, bvl_dosplitwindow,
	bvl_dostartformat, bvl_dostartproof, bvl_dostat, bvl_dostatus,
	bvl_dostore, bvl_dostridx, bvl_dostridxr, bvl_dostringp,
	bvl_dostrlen, bvl_dostrmapc, bvl_dosubstr, bvl_doswitchbuf,
	bvl_doswitchwindow, bvl_dosymbolp, bvl_doeval, bvl_dosyntax,
	bvl_doterpri, bvl_dothrow, bvl_dotime, bvl_dotimecmp, bvl_doadd,
	bvl_doumask, bvl_douwprotect, bvl_douseglobal, bvl_douselocal,
	bvl_dowait, bvl_dowhile, bvl_dowindowmark, bvl_dowindowpoint,
	bvl_dowindowp, bvl_dowritefile, bvl_dowritenamed, bvl_dovolume,
	bvl_doXfetch, bvl_doXgetbdwidth, bvl_doXgetdisp,
	bvl_doXgetfocus, bvl_doXgetfont, bvl_doXgetgeom, bvl_doXlower,
	bvl_doXraise, bvl_doXsetbdwidth, bvl_doXsetfocus,
	bvl_doXsetfont, bvl_doXsetgeom, bvl_doXstore, bvl_doyesorno,
	bvl_dozerop;

builtin_strings()
{
	struct string	*save_string();
	ABORTCATCH = save_string("abort", 5);
	ABORTCATCH->st_perm = 1;
	ABORTONINT_NAME = save_string("abort-on-interrupt", 18);
	ABORTONINT_NAME->st_perm = 1;
	AGAINMSG = save_string("Please answer ``yes'' or ``no'': ", 33);
	AGAINMSG->st_perm = 1;
	ARGLIST_NAME = save_string("program-args", 12);
	ARGLIST_NAME->st_perm = 1;
	ARGZERO_NAME = save_string("program-name", 12);
	ARGZERO_NAME->st_perm = 1;
	AUTOACTIVE_NAME = save_string("auto-activate-windows", 21);
	AUTOACTIVE_NAME->st_perm = 1;
	AUTORAISE_NAME = save_string("auto-raise-windows", 18);
	AUTORAISE_NAME->st_perm = 1;
	AUTOWARP_NAME = save_string("auto-warp-mouse", 15);
	AUTOWARP_NAME->st_perm = 1;
	BACKTRACE_BUF = save_string("*backtrace*", 11);
	BACKTRACE_BUF->st_perm = 1;
	BINDINGS = save_string("*bindings*", 10);
	BINDINGS->st_perm = 1;
	BQUOTESTR = save_string("backquote", 9);
	BQUOTESTR->st_perm = 1;
	BREAKABORT = save_string("break-on-abort", 14);
	BREAKABORT->st_perm = 1;
	BREAKONINT_NAME = save_string("break-on-interrupt", 18);
	BREAKONINT_NAME->st_perm = 1;
	BREAKVAR = save_string("break-on-error", 14);
	BREAKVAR->st_perm = 1;
	CONNPORT_NAME = save_string("connection-port", 15);
	CONNPORT_NAME->st_perm = 1;
	CURSOR_INVERSE = save_string("cursor-inverse", 14);
	CURSOR_INVERSE->st_perm = 1;
	CURSOR_OUTLINE = save_string("cursor-outline", 14);
	CURSOR_OUTLINE->st_perm = 1;
	CURSOR_UNDERLINE = save_string("cursor-underline", 16);
	CURSOR_UNDERLINE->st_perm = 1;
	DEFMODEFMTVAL = save_string("VorTeX Buffer %b%* \"%f\"  %[[%w:%m]%] %p  %M% ", 45);
	DEFMODEFMTVAL->st_perm = 1;
	DEFMODESTRVAL = save_string("fundamental", 11);
	DEFMODESTRVAL->st_perm = 1;
	DEFPROMPT = save_string(": ", 2);
	DEFPROMPT->st_perm = 1;
	DOCSTRVAR_NAME = save_string("docstr-last-match", 17);
	DOCSTRVAR_NAME->st_perm = 1;
	ECHOPREFIX_NAME = save_string("echo-prefix-sequence", 20);
	ECHOPREFIX_NAME->st_perm = 1;
	ERRORCATCH = save_string("error", 5);
	ERRORCATCH->st_perm = 1;
	FORMATHOST_NAME = save_string("formatter-host", 14);
	FORMATHOST_NAME->st_perm = 1;
	FORMATPORT_NAME = save_string("formatter-port", 14);
	FORMATPORT_NAME->st_perm = 1;
	FORMATPROG_NAME = save_string("formatter-program", 17);
	FORMATPROG_NAME->st_perm = 1;
	GLOBALMODE_NAME = save_string("global-mode-string", 18);
	GLOBALMODE_NAME->st_perm = 1;
	GSLEADER = save_string("%gsym", 5);
	GSLEADER->st_perm = 1;
	HELLOSTR = save_string("Welcome to VorTeX lisp, version 0.\n", 35);
	HELLOSTR->st_perm = 1;
	HELPBUFFER_NAME = save_string("*completions*", 13);
	HELPBUFFER_NAME->st_perm = 1;
	HOME_NAME = save_string("HOME", 4);
	HOME_NAME->st_perm = 1;
	INTERACTSTR = save_string("interactive", 11);
	INTERACTSTR->st_perm = 1;
	KEYMAP = save_string("%keymap", 7);
	KEYMAP->st_perm = 1;
	LAMBDASTR = save_string("lambda", 6);
	LAMBDASTR->st_perm = 1;
	LEXPRSTR = save_string("lexpr", 5);
	LEXPRSTR->st_perm = 1;
	LISTBUF_NAME = save_string("*buffer-list*", 13);
	LISTBUF_NAME->st_perm = 1;
	LOADPATH_NAME = save_string("load-path", 9);
	LOADPATH_NAME->st_perm = 1;
	LOCALMODE_NAME = save_string("mode-string", 11);
	LOCALMODE_NAME->st_perm = 1;
	MACROSTR = save_string("macro", 5);
	MACROSTR->st_perm = 1;
	MINBMODEFMTVAL = save_string("VorTeX Input  %[[%w:%m]%] %p  %M% ", 34);
	MINBMODEFMTVAL->st_perm = 1;
	MINBMODESTRVAL = save_string("minibuffer", 10);
	MINBMODESTRVAL->st_perm = 1;
	MINIBUF_NAME = save_string("*minibuffer*", 12);
	MINIBUF_NAME->st_perm = 1;
	MODEFMT_NAME = save_string("mode-line-format", 16);
	MODEFMT_NAME->st_perm = 1;
	NIL_NAME = save_string("nil", 3);
	NIL_NAME->st_perm = 1;
	NLAMBDASTR = save_string("nlambda", 7);
	NLAMBDASTR->st_perm = 1;
	NO_STR = save_string("no", 2);
	NO_STR->st_perm = 1;
	OVERWRITE_NAME = save_string("confirm-overwrite-changed-file", 30);
	OVERWRITE_NAME->st_perm = 1;
	PROGGOSTR = save_string("*prog-go*", 9);
	PROGGOSTR->st_perm = 1;
	PROGRETSTR = save_string("*prog-return*", 13);
	PROGRETSTR->st_perm = 1;
	PROMPTSTR = save_string("-> ", 3);
	PROMPTSTR->st_perm = 1;
	PROOFHOST_NAME = save_string("proof-editor-host", 17);
	PROOFHOST_NAME->st_perm = 1;
	PROOFPORT_NAME = save_string("proof-editor-port", 17);
	PROOFPORT_NAME->st_perm = 1;
	PROOFPROG_NAME = save_string("proof-editor-program", 20);
	PROOFPROG_NAME->st_perm = 1;
	PROOF_CREATE = save_string("proof-buffer-create-hook", 24);
	PROOF_CREATE->st_perm = 1;
	PROOF_DESTROY = save_string("proof-buffer-destroy-hook", 25);
	PROOF_DESTROY->st_perm = 1;
	QUOTESTR = save_string("quote", 5);
	QUOTESTR->st_perm = 1;
	SCRATCH_NAME = save_string("*scratch*", 9);
	SCRATCH_NAME->st_perm = 1;
	STATLEADER = save_string("%stat", 5);
	STATLEADER->st_perm = 1;
	STDIN_NAME = save_string("stdin", 5);
	STDIN_NAME->st_perm = 1;
	STDOUT_NAME = save_string("stdout", 6);
	STDOUT_NAME->st_perm = 1;
	TABWIDTH = save_string("tab-width", 9);
	TABWIDTH->st_perm = 1;
	TIMELEADER = save_string("%time", 5);
	TIMELEADER->st_perm = 1;
	TOPLEVCATCH = save_string("top-level", 9);
	TOPLEVCATCH->st_perm = 1;
	TRUNCATE = save_string("truncate-long-lines", 19);
	TRUNCATE->st_perm = 1;
	T_NAME = save_string("t", 1);
	T_NAME->st_perm = 1;
	UNDEFNAME = save_string("undef", 5);
	UNDEFNAME->st_perm = 1;
	WRITEUNMOD_NAME = save_string("write-unmodified-files", 22);
	WRITEUNMOD_NAME->st_perm = 1;
	YES_OR_NO_FUNC = save_string("yes-or-no", 9);
	YES_OR_NO_FUNC->st_perm = 1;
	YES_STR = save_string("yes", 3);
	YES_STR->st_perm = 1;

	return (0);
}

builtin_values()
{
	bvl_doXfetch.vl_type = 5;
	bvl_doXfetch.vl_data = bdata_doXfetch;
	bvl_doXgetbdwidth.vl_type = 5;
	bvl_doXgetbdwidth.vl_data = bdata_doXgetbdwidth;
	bvl_doXgetdisp.vl_type = 5;
	bvl_doXgetdisp.vl_data = bdata_doXgetdisp;
	bvl_doXgetfocus.vl_type = 5;
	bvl_doXgetfocus.vl_data = bdata_doXgetfocus;
	bvl_doXgetfont.vl_type = 5;
	bvl_doXgetfont.vl_data = bdata_doXgetfont;
	bvl_doXgetgeom.vl_type = 5;
	bvl_doXgetgeom.vl_data = bdata_doXgetgeom;
	bvl_doXlower.vl_type = 5;
	bvl_doXlower.vl_data = bdata_doXlower;
	bvl_doXraise.vl_type = 5;
	bvl_doXraise.vl_data = bdata_doXraise;
	bvl_doXsetbdwidth.vl_type = 5;
	bvl_doXsetbdwidth.vl_data = bdata_doXsetbdwidth;
	bvl_doXsetfocus.vl_type = 5;
	bvl_doXsetfocus.vl_data = bdata_doXsetfocus;
	bvl_doXsetfont.vl_type = 5;
	bvl_doXsetfont.vl_data = bdata_doXsetfont;
	bvl_doXsetgeom.vl_type = 5;
	bvl_doXsetgeom.vl_data = bdata_doXsetgeom;
	bvl_doXstore.vl_type = 5;
	bvl_doXstore.vl_data = bdata_doXstore;
	bvl_doabort.vl_type = 5;
	bvl_doabort.vl_data = bdata_doabort;
	bvl_doabs.vl_type = 5;
	bvl_doabs.vl_data = bdata_doabs;
	bvl_doaccess.vl_type = 5;
	bvl_doaccess.vl_data = bdata_doaccess;
	bvl_doadd.vl_type = 5;
	bvl_doadd.vl_data = bdata_doadd;
	bvl_doallocate.vl_type = 5;
	bvl_doallocate.vl_data = bdata_doallocate;
	bvl_doand.vl_type = 5;
	bvl_doand.vl_data = bdata_doand;
	bvl_doappend.vl_type = 5;
	bvl_doappend.vl_data = bdata_doappend;
	bvl_doappendstring.vl_type = 5;
	bvl_doappendstring.vl_data = bdata_doappendstring;
	bvl_doarray.vl_type = 5;
	bvl_doarray.vl_data = bdata_doarray;
	bvl_doarrayp.vl_type = 5;
	bvl_doarrayp.vl_data = bdata_doarrayp;
	bvl_doatomp.vl_type = 5;
	bvl_doatomp.vl_data = bdata_doatomp;
	bvl_doautoalloc.vl_type = 5;
	bvl_doautoalloc.vl_data = bdata_doautoalloc;
	bvl_dobackquote.vl_type = 5;
	bvl_dobackquote.vl_data = bdata_dobackquote;
	bvl_dobacktrace.vl_type = 5;
	bvl_dobacktrace.vl_data = bdata_dobacktrace;
	bvl_dobeep.vl_type = 5;
	bvl_dobeep.vl_data = bdata_dobeep;
	bvl_dobopen.vl_type = 5;
	bvl_dobopen.vl_data = bdata_dobopen;
	bvl_doboundp.vl_type = 5;
	bvl_doboundp.vl_data = bdata_doboundp;
	bvl_dobreakloop.vl_type = 5;
	bvl_dobreakloop.vl_data = bdata_dobreakloop;
	bvl_dobufferp.vl_type = 5;
	bvl_dobufferp.vl_data = bdata_dobufferp;
	bvl_dobuffile.vl_type = 5;
	bvl_dobuffile.vl_data = bdata_dobuffile;
	bvl_dobuflen.vl_type = 5;
	bvl_dobuflen.vl_data = bdata_dobuflen;
	bvl_dobufmodified.vl_type = 5;
	bvl_dobufmodified.vl_data = bdata_dobufmodified;
	bvl_dobufname.vl_type = 5;
	bvl_dobufname.vl_data = bdata_dobufname;
	bvl_dobufreadonly.vl_type = 5;
	bvl_dobufreadonly.vl_data = bdata_dobufreadonly;
	bvl_dobufstart.vl_type = 5;
	bvl_dobufstart.vl_data = bdata_dobufstart;
	bvl_dobuftype.vl_type = 5;
	bvl_dobuftype.vl_data = bdata_dobuftype;
	bvl_docar.vl_type = 5;
	bvl_docar.vl_data = bdata_docar;
	bvl_docat.vl_type = 5;
	bvl_docat.vl_data = bdata_docat;
	bvl_docatch.vl_type = 5;
	bvl_docatch.vl_data = bdata_docatch;
	bvl_docdr.vl_type = 5;
	bvl_docdr.vl_data = bdata_docdr;
	bvl_docerror.vl_type = 5;
	bvl_docerror.vl_data = bdata_docerror;
	bvl_dochdir.vl_type = 5;
	bvl_dochdir.vl_data = bdata_dochdir;
	bvl_dochmod.vl_type = 5;
	bvl_dochmod.vl_data = bdata_dochmod;
	bvl_doclose.vl_type = 5;
	bvl_doclose.vl_data = bdata_doclose;
	bvl_doclosedoc.vl_type = 5;
	bvl_doclosedoc.vl_data = bdata_doclosedoc;
	bvl_docompletelist.vl_type = 5;
	bvl_docompletelist.vl_data = bdata_docompletelist;
	bvl_doconcat.vl_type = 5;
	bvl_doconcat.vl_data = bdata_doconcat;
	bvl_doconcata.vl_type = 5;
	bvl_doconcata.vl_data = bdata_doconcata;
	bvl_docond.vl_type = 5;
	bvl_docond.vl_data = bdata_docond;
	bvl_doconfirm.vl_type = 5;
	bvl_doconfirm.vl_data = bdata_doconfirm;
	bvl_docons.vl_type = 5;
	bvl_docons.vl_data = bdata_docons;
	bvl_docont.vl_type = 5;
	bvl_docont.vl_data = bdata_docont;
	bvl_docopen.vl_type = 5;
	bvl_docopen.vl_data = bdata_docopen;
	bvl_docopy.vl_type = 5;
	bvl_docopy.vl_data = bdata_docopy;
	bvl_docopychars.vl_type = 5;
	bvl_docopychars.vl_data = bdata_docopychars;
	bvl_docopyregion.vl_type = 5;
	bvl_docopyregion.vl_data = bdata_docopyregion;
	bvl_docurprfwin.vl_type = 5;
	bvl_docurprfwin.vl_data = bdata_docurprfwin;
	bvl_docurrentdoc.vl_type = 5;
	bvl_docurrentdoc.vl_data = bdata_docurrentdoc;
	bvl_docursrcwin.vl_type = 5;
	bvl_docursrcwin.vl_data = bdata_docursrcwin;
	bvl_docurwindow.vl_type = 5;
	bvl_docurwindow.vl_data = bdata_docurwindow;
	bvl_dodeactivatewin.vl_type = 5;
	bvl_dodeactivatewin.vl_data = bdata_dodeactivatewin;
	bvl_dodebug.vl_type = 5;
	bvl_dodebug.vl_data = bdata_dodebug;
	bvl_dodeclarevarloc.vl_type = 5;
	bvl_dodeclarevarloc.vl_data = bdata_dodeclarevarloc;
	bvl_dodefun.vl_type = 5;
	bvl_dodefun.vl_data = bdata_dodefun;
	bvl_dodelbackward.vl_type = 5;
	bvl_dodelbackward.vl_data = bdata_dodelbackward;
	bvl_dodelforward.vl_type = 5;
	bvl_dodelforward.vl_data = bdata_dodelforward;
	bvl_dodelregion.vl_type = 5;
	bvl_dodelregion.vl_data = bdata_dodelregion;
	bvl_dodigit.vl_type = 5;
	bvl_dodigit.vl_data = bdata_dodigit;
	bvl_dodirs.vl_type = 5;
	bvl_dodirs.vl_data = bdata_dodirs;
	bvl_dodtprp.vl_type = 5;
	bvl_dodtprp.vl_data = bdata_dodtprp;
	bvl_dodumpbindings.vl_type = 5;
	bvl_dodumpbindings.vl_data = bdata_dodumpbindings;
	bvl_dodumpbuffer.vl_type = 5;
	bvl_dodumpbuffer.vl_data = bdata_dodumpbuffer;
	bvl_doeq.vl_type = 5;
	bvl_doeq.vl_data = bdata_doeq;
	bvl_doequal.vl_type = 5;
	bvl_doequal.vl_data = bdata_doequal;
	bvl_doerasebuf.vl_type = 5;
	bvl_doerasebuf.vl_data = bdata_doerasebuf;
	bvl_doerror.vl_type = 5;
	bvl_doerror.vl_data = bdata_doerror;
	bvl_doeval.vl_type = 5;
	bvl_doeval.vl_data = bdata_doeval;
	bvl_doevalexpr.vl_type = 5;
	bvl_doevalexpr.vl_data = bdata_doevalexpr;
	bvl_doevenp.vl_type = 5;
	bvl_doevenp.vl_data = bdata_doevenp;
	bvl_doexec.vl_type = 5;
	bvl_doexec.vl_data = bdata_doexec;
	bvl_doexit.vl_type = 5;
	bvl_doexit.vl_data = bdata_doexit;
	bvl_doexitvortex.vl_type = 5;
	bvl_doexitvortex.vl_data = bdata_doexitvortex;
	bvl_doexplode.vl_type = 5;
	bvl_doexplode.vl_data = bdata_doexplode;
	bvl_doexplodec.vl_type = 5;
	bvl_doexplodec.vl_data = bdata_doexplodec;
	bvl_doexploden.vl_type = 5;
	bvl_doexploden.vl_data = bdata_doexploden;
	bvl_doextcommand.vl_type = 5;
	bvl_doextcommand.vl_data = bdata_doextcommand;
	bvl_dofakedoc.vl_type = 5;
	bvl_dofakedoc.vl_data = bdata_dofakedoc;
	bvl_dofillarray.vl_type = 5;
	bvl_dofillarray.vl_data = bdata_dofillarray;
	bvl_dofindfile.vl_type = 5;
	bvl_dofindfile.vl_data = bdata_dofindfile;
	bvl_dofindmatch.vl_type = 5;
	bvl_dofindmatch.vl_data = bdata_dofindmatch;
	bvl_dofindregexp.vl_type = 5;
	bvl_dofindregexp.vl_data = bdata_dofindregexp;
	bvl_doflock.vl_type = 5;
	bvl_doflock.vl_data = bdata_doflock;
	bvl_doflush.vl_type = 5;
	bvl_doflush.vl_data = bdata_doflush;
	bvl_dofollowchar.vl_type = 5;
	bvl_dofollowchar.vl_data = bdata_dofollowchar;
	bvl_dofopen.vl_type = 5;
	bvl_dofopen.vl_data = bdata_dofopen;
	bvl_doforallbufs.vl_type = 5;
	bvl_doforallbufs.vl_data = bdata_doforallbufs;
	bvl_doforallvars.vl_type = 5;
	bvl_doforallvars.vl_data = bdata_doforallvars;
	bvl_doforallwins.vl_type = 5;
	bvl_doforallwins.vl_data = bdata_doforallwins;
	bvl_doformat.vl_type = 5;
	bvl_doformat.vl_data = bdata_doformat;
	bvl_doformatdoc.vl_type = 5;
	bvl_doformatdoc.vl_data = bdata_doformatdoc;
	bvl_dofuncall.vl_type = 5;
	bvl_dofuncall.vl_data = bdata_dofuncall;
	bvl_dofunctionp.vl_type = 5;
	bvl_dofunctionp.vl_data = bdata_dofunctionp;
	bvl_dogc.vl_type = 5;
	bvl_dogc.vl_data = bdata_dogc;
	bvl_dogenprefix.vl_type = 5;
	bvl_dogenprefix.vl_data = bdata_dogenprefix;
	bvl_dogensym.vl_type = 5;
	bvl_dogensym.vl_data = bdata_dogensym;
	bvl_dogetbinding.vl_type = 5;
	bvl_dogetbinding.vl_data = bdata_dogetbinding;
	bvl_dogetenv.vl_type = 5;
	bvl_dogetenv.vl_data = bdata_dogetenv;
	bvl_dogethostname.vl_type = 5;
	bvl_dogethostname.vl_data = bdata_dogethostname;
	bvl_dogethostnameonly.vl_type = 5;
	bvl_dogethostnameonly.vl_data = bdata_dogethostnameonly;
	bvl_dogetintborder.vl_type = 5;
	bvl_dogetintborder.vl_data = bdata_dogetintborder;
	bvl_dogetplist.vl_type = 5;
	bvl_dogetplist.vl_data = bdata_dogetplist;
	bvl_dogetprefix.vl_type = 5;
	bvl_dogetprefix.vl_data = bdata_dogetprefix;
	bvl_dogetprop.vl_type = 5;
	bvl_dogetprop.vl_data = bdata_dogetprop;
	bvl_doglobalmap.vl_type = 5;
	bvl_doglobalmap.vl_data = bdata_doglobalmap;
	bvl_dogo.vl_type = 5;
	bvl_dogo.vl_data = bdata_dogo;
	bvl_dogreatereqp.vl_type = 5;
	bvl_dogreatereqp.vl_data = bdata_dogreatereqp;
	bvl_dogreaterp.vl_type = 5;
	bvl_dogreaterp.vl_data = bdata_dogreaterp;
	bvl_doif.vl_type = 5;
	bvl_doif.vl_data = bdata_doif;
	bvl_doindex.vl_type = 5;
	bvl_doindex.vl_data = bdata_doindex;
	bvl_doinsertchar.vl_type = 5;
	bvl_doinsertchar.vl_data = bdata_doinsertchar;
	bvl_doinsertfile.vl_type = 5;
	bvl_doinsertfile.vl_data = bdata_doinsertfile;
	bvl_doinsertstring.vl_type = 5;
	bvl_doinsertstring.vl_data = bdata_doinsertstring;
	bvl_dointeractive.vl_type = 5;
	bvl_dointeractive.vl_data = bdata_dointeractive;
	bvl_dokill.vl_type = 5;
	bvl_dokill.vl_data = bdata_dokill;
	bvl_dokillformat.vl_type = 5;
	bvl_dokillformat.vl_data = bdata_dokillformat;
	bvl_dokillproof.vl_type = 5;
	bvl_dokillproof.vl_data = bdata_dokillproof;
	bvl_dokillwindow.vl_type = 5;
	bvl_dokillwindow.vl_data = bdata_dokillwindow;
	bvl_dolambda.vl_type = 5;
	bvl_dolambda.vl_data = bdata_dolambda;
	bvl_dolength.vl_type = 5;
	bvl_dolength.vl_data = bdata_dolength;
	bvl_dolesseqp.vl_type = 5;
	bvl_dolesseqp.vl_data = bdata_dolesseqp;
	bvl_dolessp.vl_type = 5;
	bvl_dolessp.vl_data = bdata_dolessp;
	bvl_dolet.vl_type = 5;
	bvl_dolet.vl_data = bdata_dolet;
	bvl_dolexpr.vl_type = 5;
	bvl_dolexpr.vl_data = bdata_dolexpr;
	bvl_dolist.vl_type = 5;
	bvl_dolist.vl_data = bdata_dolist;
	bvl_dolistbufs.vl_type = 5;
	bvl_dolistbufs.vl_data = bdata_dolistbufs;
	bvl_dolistp.vl_type = 5;
	bvl_dolistp.vl_data = bdata_dolistp;
	bvl_doload.vl_type = 5;
	bvl_doload.vl_data = bdata_doload;
	bvl_doloaddoc.vl_type = 5;
	bvl_doloaddoc.vl_data = bdata_doloaddoc;
	bvl_dolocalmap.vl_type = 5;
	bvl_dolocalmap.vl_data = bdata_dolocalmap;
	bvl_dolocatorpos.vl_type = 5;
	bvl_dolocatorpos.vl_data = bdata_dolocatorpos;
	bvl_doloop.vl_type = 5;
	bvl_doloop.vl_data = bdata_doloop;
	bvl_domacro.vl_type = 5;
	bvl_domacro.vl_data = bdata_domacro;
	bvl_domakeconn.vl_type = 5;
	bvl_domakeconn.vl_data = bdata_domakeconn;
	bvl_domakedoc.vl_type = 5;
	bvl_domakedoc.vl_data = bdata_domakedoc;
	bvl_domakekeymap.vl_type = 5;
	bvl_domakekeymap.vl_data = bdata_domakekeymap;
	bvl_domakelocalvar.vl_type = 5;
	bvl_domakelocalvar.vl_data = bdata_domakelocalvar;
	bvl_domakesparse.vl_type = 5;
	bvl_domakesparse.vl_data = bdata_domakesparse;
	bvl_domapc.vl_type = 5;
	bvl_domapc.vl_data = bdata_domapc;
	bvl_domapcar.vl_type = 5;
	bvl_domapcar.vl_data = bdata_domapcar;
	bvl_domarkpos.vl_type = 5;
	bvl_domarkpos.vl_data = bdata_domarkpos;
	bvl_domax.vl_type = 5;
	bvl_domax.vl_data = bdata_domax;
	bvl_domemberp.vl_type = 5;
	bvl_domemberp.vl_data = bdata_domemberp;
	bvl_domemeqp.vl_type = 5;
	bvl_domemeqp.vl_data = bdata_domemeqp;
	bvl_domenu.vl_type = 5;
	bvl_domenu.vl_data = bdata_domenu;
	bvl_domessage.vl_type = 5;
	bvl_domessage.vl_data = bdata_domessage;
	bvl_dometadigit.vl_type = 5;
	bvl_dometadigit.vl_data = bdata_dometadigit;
	bvl_dometaprefix.vl_type = 5;
	bvl_dometaprefix.vl_data = bdata_dometaprefix;
	bvl_domin.vl_type = 5;
	bvl_domin.vl_data = bdata_domin;
	bvl_dominbcomplete.vl_type = 5;
	bvl_dominbcomplete.vl_data = bdata_dominbcomplete;
	bvl_dominbhelp.vl_type = 5;
	bvl_dominbhelp.vl_data = bdata_dominbhelp;
	bvl_dominbinput.vl_type = 5;
	bvl_dominbinput.vl_data = bdata_dominbinput;
	bvl_dominbprint.vl_type = 5;
	bvl_dominbprint.vl_data = bdata_dominbprint;
	bvl_dominbreturn.vl_type = 5;
	bvl_dominbreturn.vl_data = bdata_dominbreturn;
	bvl_domod.vl_type = 5;
	bvl_domod.vl_data = bdata_domod;
	bvl_donewline.vl_type = 5;
	bvl_donewline.vl_data = bdata_donewline;
	bvl_donextline.vl_type = 5;
	bvl_donextline.vl_data = bdata_donextline;
	bvl_donextregexp.vl_type = 5;
	bvl_donextregexp.vl_data = bdata_donextregexp;
	bvl_donlambda.vl_type = 5;
	bvl_donlambda.vl_data = bdata_donlambda;
	bvl_donot.vl_type = 5;
	bvl_donot.vl_data = bdata_donot;
	bvl_donotify.vl_type = 5;
	bvl_donotify.vl_data = bdata_donotify;
	bvl_donth.vl_type = 5;
	bvl_donth.vl_data = bdata_donth;
	bvl_donthcdr.vl_type = 5;
	bvl_donthcdr.vl_data = bdata_donthcdr;
	bvl_donullp.vl_type = 5;
	bvl_donullp.vl_data = bdata_donullp;
	bvl_donumberp.vl_type = 5;
	bvl_donumberp.vl_data = bdata_donumberp;
	bvl_dooddp.vl_type = 5;
	bvl_dooddp.vl_data = bdata_dooddp;
	bvl_door.vl_type = 5;
	bvl_door.vl_data = bdata_door;
	bvl_dopatom.vl_type = 5;
	bvl_dopatom.vl_data = bdata_dopatom;
	bvl_dopipe.vl_type = 5;
	bvl_dopipe.vl_data = bdata_dopipe;
	bvl_dopointpos.vl_type = 5;
	bvl_dopointpos.vl_data = bdata_dopointpos;
	bvl_dopopbuffer.vl_type = 5;
	bvl_dopopbuffer.vl_data = bdata_dopopbuffer;
	bvl_dopopd.vl_type = 5;
	bvl_dopopd.vl_data = bdata_dopopd;
	bvl_dopopopmesg.vl_type = 5;
	bvl_dopopopmesg.vl_data = bdata_dopopopmesg;
	bvl_dopp.vl_type = 5;
	bvl_dopp.vl_data = bdata_dopp;
	bvl_doprecchar.vl_type = 5;
	bvl_doprecchar.vl_data = bdata_doprecchar;
	bvl_doprevline.vl_type = 5;
	bvl_doprevline.vl_data = bdata_doprevline;
	bvl_doprfgotopage.vl_type = 5;
	bvl_doprfgotopage.vl_data = bdata_doprfgotopage;
	bvl_doprfmoveabs.vl_type = 5;
	bvl_doprfmoveabs.vl_data = bdata_doprfmoveabs;
	bvl_doprfmoverel.vl_type = 5;
	bvl_doprfmoverel.vl_data = bdata_doprfmoverel;
	bvl_doprfmoveto.vl_type = 5;
	bvl_doprfmoveto.vl_data = bdata_doprfmoveto;
	bvl_doprfnextpage.vl_type = 5;
	bvl_doprfnextpage.vl_data = bdata_doprfnextpage;
	bvl_doprfselect.vl_type = 5;
	bvl_doprfselect.vl_data = bdata_doprfselect;
	bvl_doprfselregion.vl_type = 5;
	bvl_doprfselregion.vl_data = bdata_doprfselregion;
	bvl_doprinc.vl_type = 5;
	bvl_doprinc.vl_data = bdata_doprinc;
	bvl_doprint.vl_type = 5;
	bvl_doprint.vl_data = bdata_doprint;
	bvl_doprog.vl_type = 5;
	bvl_doprog.vl_data = bdata_doprog;
	bvl_doproofdoc.vl_type = 5;
	bvl_doproofdoc.vl_data = bdata_doproofdoc;
	bvl_dopushd.vl_type = 5;
	bvl_dopushd.vl_data = bdata_dopushd;
	bvl_doputprop.vl_type = 5;
	bvl_doputprop.vl_data = bdata_doputprop;
	bvl_doquote.vl_type = 5;
	bvl_doquote.vl_data = bdata_doquote;
	bvl_doquotedinsert.vl_type = 5;
	bvl_doquotedinsert.vl_data = bdata_doquotedinsert;
	bvl_doratom.vl_type = 5;
	bvl_doratom.vl_data = bdata_doratom;
	bvl_doread.vl_type = 5;
	bvl_doread.vl_data = bdata_doread;
	bvl_doreadc.vl_type = 5;
	bvl_doreadc.vl_data = bdata_doreadc;
	bvl_dorecenter.vl_type = 5;
	bvl_dorecenter.vl_data = bdata_dorecenter;
	bvl_doremob.vl_type = 5;
	bvl_doremob.vl_data = bdata_doremob;
	bvl_doremovebuf.vl_type = 5;
	bvl_doremovebuf.vl_data = bdata_doremovebuf;
	bvl_doremovelocals.vl_type = 5;
	bvl_doremovelocals.vl_data = bdata_doremovelocals;
	bvl_doremprop.vl_type = 5;
	bvl_doremprop.vl_data = bdata_doremprop;
	bvl_doreset.vl_type = 5;
	bvl_doreset.vl_data = bdata_doreset;
	bvl_doreturn.vl_type = 5;
	bvl_doreturn.vl_data = bdata_doreturn;
	bvl_doreverse.vl_type = 5;
	bvl_doreverse.vl_data = bdata_doreverse;
	bvl_dorplaca.vl_type = 5;
	bvl_dorplaca.vl_data = bdata_dorplaca;
	bvl_dorplacd.vl_type = 5;
	bvl_dorplacd.vl_data = bdata_dorplacd;
	bvl_doscrollback.vl_type = 5;
	bvl_doscrollback.vl_data = bdata_doscrollback;
	bvl_doscrollfwd.vl_type = 5;
	bvl_doscrollfwd.vl_data = bdata_doscrollfwd;
	bvl_dosearchbackwd.vl_type = 5;
	bvl_dosearchbackwd.vl_data = bdata_dosearchbackwd;
	bvl_dosearchbkregexp.vl_type = 5;
	bvl_dosearchbkregexp.vl_data = bdata_dosearchbkregexp;
	bvl_dosearchforwd.vl_type = 5;
	bvl_dosearchforwd.vl_data = bdata_dosearchforwd;
	bvl_doseek.vl_type = 5;
	bvl_doseek.vl_data = bdata_doseek;
	bvl_doselfinsert.vl_type = 5;
	bvl_doselfinsert.vl_data = bdata_doselfinsert;
	bvl_doselmore.vl_type = 5;
	bvl_doselmore.vl_data = bdata_doselmore;
	bvl_doset.vl_type = 5;
	bvl_doset.vl_data = bdata_doset;
	bvl_dosetbinding.vl_type = 5;
	bvl_dosetbinding.vl_data = bdata_dosetbinding;
	bvl_dosetenv.vl_type = 5;
	bvl_dosetenv.vl_data = bdata_dosetenv;
	bvl_dosetintborder.vl_type = 5;
	bvl_dosetintborder.vl_data = bdata_dosetintborder;
	bvl_dosetmark.vl_type = 5;
	bvl_dosetmark.vl_data = bdata_dosetmark;
	bvl_dosetplist.vl_type = 5;
	bvl_dosetplist.vl_data = bdata_dosetplist;
	bvl_dosetpoint.vl_type = 5;
	bvl_dosetpoint.vl_data = bdata_dosetpoint;
	bvl_dosetprefix.vl_type = 5;
	bvl_dosetprefix.vl_data = bdata_dosetprefix;
	bvl_dosetq.vl_type = 5;
	bvl_dosetq.vl_data = bdata_dosetq;
	bvl_dosetwindowmark.vl_type = 5;
	bvl_dosetwindowmark.vl_data = bdata_dosetwindowmark;
	bvl_dosetwindowpoint.vl_type = 5;
	bvl_dosetwindowpoint.vl_data = bdata_dosetwindowpoint;
	bvl_dosoftlim.vl_type = 5;
	bvl_dosoftlim.vl_data = bdata_dosoftlim;
	bvl_dosopen.vl_type = 5;
	bvl_dosopen.vl_data = bdata_dosopen;
	bvl_dosplitwindow.vl_type = 5;
	bvl_dosplitwindow.vl_data = bdata_dosplitwindow;
	bvl_dosrchfwdregex.vl_type = 5;
	bvl_dosrchfwdregex.vl_data = bdata_dosrchfwdregex;
	bvl_dostartformat.vl_type = 5;
	bvl_dostartformat.vl_data = bdata_dostartformat;
	bvl_dostartproof.vl_type = 5;
	bvl_dostartproof.vl_data = bdata_dostartproof;
	bvl_dostat.vl_type = 5;
	bvl_dostat.vl_data = bdata_dostat;
	bvl_dostatus.vl_type = 5;
	bvl_dostatus.vl_data = bdata_dostatus;
	bvl_dostore.vl_type = 5;
	bvl_dostore.vl_data = bdata_dostore;
	bvl_dostridx.vl_type = 5;
	bvl_dostridx.vl_data = bdata_dostridx;
	bvl_dostridxr.vl_type = 5;
	bvl_dostridxr.vl_data = bdata_dostridxr;
	bvl_dostringp.vl_type = 5;
	bvl_dostringp.vl_data = bdata_dostringp;
	bvl_dostrlen.vl_type = 5;
	bvl_dostrlen.vl_data = bdata_dostrlen;
	bvl_dostrmapc.vl_type = 5;
	bvl_dostrmapc.vl_data = bdata_dostrmapc;
	bvl_dosubstr.vl_type = 5;
	bvl_dosubstr.vl_data = bdata_dosubstr;
	bvl_doswitchbuf.vl_type = 5;
	bvl_doswitchbuf.vl_data = bdata_doswitchbuf;
	bvl_doswitchwindow.vl_type = 5;
	bvl_doswitchwindow.vl_data = bdata_doswitchwindow;
	bvl_dosymbolp.vl_type = 5;
	bvl_dosymbolp.vl_data = bdata_dosymbolp;
	bvl_dosyntax.vl_type = 5;
	bvl_dosyntax.vl_data = bdata_dosyntax;
	bvl_doterpri.vl_type = 5;
	bvl_doterpri.vl_data = bdata_doterpri;
	bvl_dothrow.vl_type = 5;
	bvl_dothrow.vl_data = bdata_dothrow;
	bvl_dotime.vl_type = 5;
	bvl_dotime.vl_data = bdata_dotime;
	bvl_dotimecmp.vl_type = 5;
	bvl_dotimecmp.vl_data = bdata_dotimecmp;
	bvl_doumask.vl_type = 5;
	bvl_doumask.vl_data = bdata_doumask;
	bvl_douseglobal.vl_type = 5;
	bvl_douseglobal.vl_data = bdata_douseglobal;
	bvl_douselocal.vl_type = 5;
	bvl_douselocal.vl_data = bdata_douselocal;
	bvl_douwprotect.vl_type = 5;
	bvl_douwprotect.vl_data = bdata_douwprotect;
	bvl_dovolume.vl_type = 5;
	bvl_dovolume.vl_data = bdata_dovolume;
	bvl_dowait.vl_type = 5;
	bvl_dowait.vl_data = bdata_dowait;
	bvl_dowhile.vl_type = 5;
	bvl_dowhile.vl_data = bdata_dowhile;
	bvl_dowindowmark.vl_type = 5;
	bvl_dowindowmark.vl_data = bdata_dowindowmark;
	bvl_dowindowp.vl_type = 5;
	bvl_dowindowp.vl_data = bdata_dowindowp;
	bvl_dowindowpoint.vl_type = 5;
	bvl_dowindowpoint.vl_data = bdata_dowindowpoint;
	bvl_dowritefile.vl_type = 5;
	bvl_dowritefile.vl_data = bdata_dowritefile;
	bvl_dowritenamed.vl_type = 5;
	bvl_dowritenamed.vl_data = bdata_dowritenamed;
	bvl_doyesorno.vl_type = 5;
	bvl_doyesorno.vl_data = bdata_doyesorno;
	bvl_dozerop.vl_type = 5;
	bvl_dozerop.vl_data = bdata_dozerop;
	bvl_findcharback.vl_type = 5;
	bvl_findcharback.vl_data = bdata_findcharback;
	bvl_findcharfwd.vl_type = 5;
	bvl_findcharfwd.vl_data = bdata_findcharfwd;

	return (0);
}

builtin_symbols()
{
	struct value	fix_init_value();
	struct string	*str;
	str = save_string("%", 1);
	setglobal(str, fix_init_value(bvl_domod), 0);
	str = save_string("*", 1);
	setglobal(str, fix_init_value(bvl_doadd), 0);
	str = save_string("+", 1);
	setglobal(str, fix_init_value(bvl_doadd), 0);
	str = save_string("-", 1);
	setglobal(str, fix_init_value(bvl_doadd), 0);
	str = save_string("/", 1);
	setglobal(str, fix_init_value(bvl_doadd), 0);
	str = save_string("<", 1);
	setglobal(str, fix_init_value(bvl_dolessp), 0);
	str = save_string("<=", 2);
	setglobal(str, fix_init_value(bvl_dolesseqp), 0);
	str = save_string(">", 1);
	setglobal(str, fix_init_value(bvl_dogreaterp), 0);
	str = save_string(">=", 2);
	setglobal(str, fix_init_value(bvl_dogreatereqp), 0);
	str = save_string("abort-function", 14);
	setglobal(str, fix_init_value(bvl_doabort), 0);
	str = save_string("abs", 3);
	setglobal(str, fix_init_value(bvl_doabs), 0);
	str = save_string("accept-meta-as-prefix", 21);
	setglobal(str, fix_init_value(bvl_dometaprefix), 0);
	str = save_string("access", 6);
	setglobal(str, fix_init_value(bvl_doaccess), 0);
	str = save_string("add", 3);
	setglobal(str, fix_init_value(bvl_doadd), 0);
	str = save_string("allocate", 8);
	setglobal(str, fix_init_value(bvl_doallocate), 0);
	str = save_string("and", 3);
	setglobal(str, fix_init_value(bvl_doand), 0);
	str = save_string("append", 6);
	setglobal(str, fix_init_value(bvl_doappend), 0);
	str = save_string("append-string", 13);
	setglobal(str, fix_init_value(bvl_doappendstring), 0);
	str = save_string("array", 5);
	setglobal(str, fix_init_value(bvl_doarray), 0);
	str = save_string("arrayp", 6);
	setglobal(str, fix_init_value(bvl_doarrayp), 0);
	str = save_string("atomp", 5);
	setglobal(str, fix_init_value(bvl_doatomp), 0);
	str = save_string("auto-allocate", 13);
	setglobal(str, fix_init_value(bvl_doautoalloc), 0);
	str = save_string("backquote", 9);
	setglobal(str, fix_init_value(bvl_dobackquote), 0);
	str = save_string("backtrace", 9);
	setglobal(str, fix_init_value(bvl_dobacktrace), 0);
	str = save_string("beep", 4);
	setglobal(str, fix_init_value(bvl_dobeep), 0);
	str = save_string("bopen", 5);
	setglobal(str, fix_init_value(bvl_dobopen), 0);
	str = save_string("boundp", 6);
	setglobal(str, fix_init_value(bvl_doboundp), 0);
	str = save_string("break-loop", 10);
	setglobal(str, fix_init_value(bvl_dobreakloop), 0);
	str = save_string("buffer-file-name", 16);
	setglobal(str, fix_init_value(bvl_dobuffile), 0);
	str = save_string("buffer-length", 13);
	setglobal(str, fix_init_value(bvl_dobuflen), 0);
	str = save_string("buffer-modified", 15);
	setglobal(str, fix_init_value(bvl_dobufmodified), 0);
	str = save_string("buffer-name", 11);
	setglobal(str, fix_init_value(bvl_dobufname), 0);
	str = save_string("buffer-read-only", 16);
	setglobal(str, fix_init_value(bvl_dobufreadonly), 0);
	str = save_string("buffer-start", 12);
	setglobal(str, fix_init_value(bvl_dobufstart), 0);
	str = save_string("buffer-type", 11);
	setglobal(str, fix_init_value(bvl_dobuftype), 0);
	str = save_string("bufferp", 7);
	setglobal(str, fix_init_value(bvl_dobufferp), 0);
	str = save_string("car", 3);
	setglobal(str, fix_init_value(bvl_docar), 0);
	str = save_string("cat", 3);
	setglobal(str, fix_init_value(bvl_docat), 0);
	str = save_string("catch", 5);
	setglobal(str, fix_init_value(bvl_docatch), 0);
	str = save_string("cd", 2);
	setglobal(str, fix_init_value(bvl_dochdir), 0);
	str = save_string("cdr", 3);
	setglobal(str, fix_init_value(bvl_docdr), 0);
	str = save_string("cerror", 6);
	setglobal(str, fix_init_value(bvl_docerror), 0);
	str = save_string("chmod", 5);
	setglobal(str, fix_init_value(bvl_dochmod), 0);
	str = save_string("close", 5);
	setglobal(str, fix_init_value(bvl_doclose), 0);
	str = save_string("close-document", 14);
	setglobal(str, fix_init_value(bvl_doclosedoc), 0);
	str = save_string("complete-from-list", 18);
	setglobal(str, fix_init_value(bvl_docompletelist), 0);
	str = save_string("concat", 6);
	setglobal(str, fix_init_value(bvl_doconcat), 0);
	str = save_string("concata", 7);
	setglobal(str, fix_init_value(bvl_doconcata), 0);
	str = save_string("cond", 4);
	setglobal(str, fix_init_value(bvl_docond), 0);
	str = save_string("confirm", 7);
	setglobal(str, fix_init_value(bvl_doconfirm), 0);
	str = save_string("cons", 4);
	setglobal(str, fix_init_value(bvl_docons), 0);
	str = save_string("cont", 4);
	setglobal(str, fix_init_value(bvl_docont), 0);
	str = save_string("copen", 5);
	setglobal(str, fix_init_value(bvl_docopen), 0);
	str = save_string("copy", 4);
	setglobal(str, fix_init_value(bvl_docopy), 0);
	str = save_string("copy-chars", 10);
	setglobal(str, fix_init_value(bvl_docopychars), 0);
	str = save_string("copy-region", 11);
	setglobal(str, fix_init_value(bvl_docopyregion), 0);
	str = save_string("current-document", 16);
	setglobal(str, fix_init_value(bvl_docurrentdoc), 0);
	str = save_string("current-proof-window", 20);
	setglobal(str, fix_init_value(bvl_docurprfwin), 0);
	str = save_string("current-source-window", 21);
	setglobal(str, fix_init_value(bvl_docursrcwin), 0);
	str = save_string("current-window", 14);
	setglobal(str, fix_init_value(bvl_docurwindow), 0);
	str = save_string("deactivate-window", 17);
	setglobal(str, fix_init_value(bvl_dodeactivatewin), 0);
	str = save_string("debug", 5);
	setglobal(str, fix_init_value(bvl_dodebug), 0);
	str = save_string("declare-variable-local", 22);
	setglobal(str, fix_init_value(bvl_dodeclarevarloc), 0);
	str = save_string("defun", 5);
	setglobal(str, fix_init_value(bvl_dodefun), 0);
	str = save_string("delete-backward-character", 25);
	setglobal(str, fix_init_value(bvl_dodelbackward), 0);
	str = save_string("delete-forward-character", 24);
	setglobal(str, fix_init_value(bvl_dodelforward), 0);
	str = save_string("delete-region", 13);
	setglobal(str, fix_init_value(bvl_dodelregion), 0);
	str = save_string("digit-command", 13);
	setglobal(str, fix_init_value(bvl_dodigit), 0);
	str = save_string("dirs", 4);
	setglobal(str, fix_init_value(bvl_dodirs), 0);
	str = save_string("docstr-find-match", 17);
	setglobal(str, fix_init_value(bvl_dofindmatch), 0);
	str = save_string("docstr-find-regexp", 18);
	setglobal(str, fix_init_value(bvl_dofindregexp), 0);
	str = save_string("docstr-next-regexp", 18);
	setglobal(str, fix_init_value(bvl_donextregexp), 0);
	str = save_string("dtprp", 5);
	setglobal(str, fix_init_value(bvl_dodtprp), 0);
	str = save_string("dump-bindings", 13);
	setglobal(str, fix_init_value(bvl_dodumpbindings), 0);
	str = save_string("dump-buffer", 11);
	setglobal(str, fix_init_value(bvl_dodumpbuffer), 0);
	str = save_string("eq", 2);
	setglobal(str, fix_init_value(bvl_doeq), 0);
	str = save_string("equal", 5);
	setglobal(str, fix_init_value(bvl_doequal), 0);
	str = save_string("erase-buffer", 12);
	setglobal(str, fix_init_value(bvl_doerasebuf), 0);
	str = save_string("error", 5);
	setglobal(str, fix_init_value(bvl_doerror), 0);
	str = save_string("eval", 4);
	setglobal(str, fix_init_value(bvl_doeval), 0);
	str = save_string("eval-expression", 15);
	setglobal(str, fix_init_value(bvl_doevalexpr), 0);
	str = save_string("evenp", 5);
	setglobal(str, fix_init_value(bvl_doevenp), 0);
	str = save_string("exec", 4);
	setglobal(str, fix_init_value(bvl_doexec), 0);
	str = save_string("exit", 4);
	setglobal(str, fix_init_value(bvl_doexit), 0);
	str = save_string("exit-vortex", 11);
	setglobal(str, fix_init_value(bvl_doexitvortex), 0);
	str = save_string("explode", 7);
	setglobal(str, fix_init_value(bvl_doexplode), 0);
	str = save_string("explodec", 8);
	setglobal(str, fix_init_value(bvl_doexplodec), 0);
	str = save_string("exploden", 8);
	setglobal(str, fix_init_value(bvl_doexploden), 0);
	str = save_string("extended-command", 16);
	setglobal(str, fix_init_value(bvl_doextcommand), 0);
	str = save_string("fake-document", 13);
	setglobal(str, fix_init_value(bvl_dofakedoc), 0);
	str = save_string("fillarray", 9);
	setglobal(str, fix_init_value(bvl_dofillarray), 0);
	str = save_string("find-character-backward", 23);
	setglobal(str, fix_init_value(bvl_findcharback), 0);
	str = save_string("find-character-forward", 22);
	setglobal(str, fix_init_value(bvl_findcharfwd), 0);
	str = save_string("find-file", 9);
	setglobal(str, fix_init_value(bvl_dofindfile), 0);
	str = save_string("flock", 5);
	setglobal(str, fix_init_value(bvl_doflock), 0);
	str = save_string("flush", 5);
	setglobal(str, fix_init_value(bvl_doflush), 0);
	str = save_string("following-character", 19);
	setglobal(str, fix_init_value(bvl_dofollowchar), 0);
	str = save_string("fopen", 5);
	setglobal(str, fix_init_value(bvl_dofopen), 0);
	str = save_string("forall-buffers", 14);
	setglobal(str, fix_init_value(bvl_doforallbufs), 0);
	str = save_string("forall-variables", 16);
	setglobal(str, fix_init_value(bvl_doforallvars), 0);
	str = save_string("forall-windows", 14);
	setglobal(str, fix_init_value(bvl_doforallwins), 0);
	str = save_string("format", 6);
	setglobal(str, fix_init_value(bvl_doformat), 0);
	str = save_string("format-document", 15);
	setglobal(str, fix_init_value(bvl_doformatdoc), 0);
	str = save_string("funcall", 7);
	setglobal(str, fix_init_value(bvl_dofuncall), 0);
	str = save_string("functionp", 9);
	setglobal(str, fix_init_value(bvl_dofunctionp), 0);
	str = save_string("gc", 2);
	setglobal(str, fix_init_value(bvl_dogc), 0);
	str = save_string("generic-prefix", 14);
	setglobal(str, fix_init_value(bvl_dogenprefix), 0);
	str = save_string("gensym", 6);
	setglobal(str, fix_init_value(bvl_dogensym), 0);
	str = save_string("get-binding", 11);
	setglobal(str, fix_init_value(bvl_dogetbinding), 0);
	str = save_string("get-global-prefix", 17);
	setglobal(str, fix_init_value(bvl_dogetprefix), 0);
	str = save_string("get-internal-border", 19);
	setglobal(str, fix_init_value(bvl_dogetintborder), 0);
	str = save_string("getenv", 6);
	setglobal(str, fix_init_value(bvl_dogetenv), 0);
	str = save_string("gethostname", 11);
	setglobal(str, fix_init_value(bvl_dogethostname), 0);
	str = save_string("gethostnameonly", 15);
	setglobal(str, fix_init_value(bvl_dogethostnameonly), 0);
	str = save_string("getplist", 8);
	setglobal(str, fix_init_value(bvl_dogetplist), 0);
	str = save_string("getprop", 7);
	setglobal(str, fix_init_value(bvl_dogetprop), 0);
	str = save_string("global-map", 10);
	setglobal(str, fix_init_value(bvl_doglobalmap), 0);
	str = save_string("go", 2);
	setglobal(str, fix_init_value(bvl_dogo), 0);
	str = save_string("greatereqp", 10);
	setglobal(str, fix_init_value(bvl_dogreatereqp), 0);
	str = save_string("greaterp", 8);
	setglobal(str, fix_init_value(bvl_dogreaterp), 0);
	str = save_string("if", 2);
	setglobal(str, fix_init_value(bvl_doif), 0);
	str = save_string("index", 5);
	setglobal(str, fix_init_value(bvl_doindex), 0);
	str = save_string("insert-char", 11);
	setglobal(str, fix_init_value(bvl_doinsertchar), 0);
	str = save_string("insert-file", 11);
	setglobal(str, fix_init_value(bvl_doinsertfile), 0);
	str = save_string("insert-string", 13);
	setglobal(str, fix_init_value(bvl_doinsertstring), 0);
	str = save_string("interactive", 11);
	setglobal(str, fix_init_value(bvl_dointeractive), 0);
	str = save_string("kill", 4);
	setglobal(str, fix_init_value(bvl_dokill), 0);
	str = save_string("kill-buffer", 11);
	setglobal(str, fix_init_value(bvl_doremovebuf), 0);
	str = save_string("kill-formatter", 14);
	setglobal(str, fix_init_value(bvl_dokillformat), 0);
	str = save_string("kill-proof-editor", 17);
	setglobal(str, fix_init_value(bvl_dokillproof), 0);
	str = save_string("kill-window", 11);
	setglobal(str, fix_init_value(bvl_dokillwindow), 0);
	str = save_string("lambda", 6);
	setglobal(str, fix_init_value(bvl_dolambda), 0);
	str = save_string("length", 6);
	setglobal(str, fix_init_value(bvl_dolength), 0);
	str = save_string("lesseqp", 7);
	setglobal(str, fix_init_value(bvl_dolesseqp), 0);
	str = save_string("lessp", 5);
	setglobal(str, fix_init_value(bvl_dolessp), 0);
	str = save_string("let", 3);
	setglobal(str, fix_init_value(bvl_dolet), 0);
	str = save_string("lexpr", 5);
	setglobal(str, fix_init_value(bvl_dolexpr), 0);
	str = save_string("list", 4);
	setglobal(str, fix_init_value(bvl_dolist), 0);
	str = save_string("list-buffers", 12);
	setglobal(str, fix_init_value(bvl_dolistbufs), 0);
	str = save_string("listp", 5);
	setglobal(str, fix_init_value(bvl_dolistp), 0);
	str = save_string("load", 4);
	setglobal(str, fix_init_value(bvl_doload), 0);
	str = save_string("load-docstr-file", 16);
	setglobal(str, fix_init_value(bvl_doloaddoc), 0);
	str = save_string("local-map", 9);
	setglobal(str, fix_init_value(bvl_dolocalmap), 0);
	str = save_string("locator-position", 16);
	setglobal(str, fix_init_value(bvl_dolocatorpos), 0);
	str = save_string("loop", 4);
	setglobal(str, fix_init_value(bvl_doloop), 0);
	str = save_string("macro", 5);
	setglobal(str, fix_init_value(bvl_domacro), 0);
	str = save_string("make-connection", 15);
	setglobal(str, fix_init_value(bvl_domakeconn), 0);
	str = save_string("make-document", 13);
	setglobal(str, fix_init_value(bvl_domakedoc), 0);
	str = save_string("make-keymap", 11);
	setglobal(str, fix_init_value(bvl_domakekeymap), 0);
	str = save_string("make-local-variable", 19);
	setglobal(str, fix_init_value(bvl_domakelocalvar), 0);
	str = save_string("make-sparse-keymap", 18);
	setglobal(str, fix_init_value(bvl_domakesparse), 0);
	str = save_string("mapc", 4);
	setglobal(str, fix_init_value(bvl_domapc), 0);
	str = save_string("mapcar", 6);
	setglobal(str, fix_init_value(bvl_domapcar), 0);
	str = save_string("mark", 4);
	setglobal(str, fix_init_value(bvl_domarkpos), 0);
	str = save_string("mash", 4);
	setglobal(str, fix_init_value(bvl_doconcata), 0);
	str = save_string("max", 3);
	setglobal(str, fix_init_value(bvl_domax), 0);
	str = save_string("memberp", 7);
	setglobal(str, fix_init_value(bvl_domemberp), 0);
	str = save_string("memeqp", 6);
	setglobal(str, fix_init_value(bvl_domemeqp), 0);
	str = save_string("menu", 4);
	setglobal(str, fix_init_value(bvl_domenu), 0);
	str = save_string("message", 7);
	setglobal(str, fix_init_value(bvl_domessage), 0);
	str = save_string("meta-digit-command", 18);
	setglobal(str, fix_init_value(bvl_dometadigit), 0);
	str = save_string("min", 3);
	setglobal(str, fix_init_value(bvl_domin), 0);
	str = save_string("minibuf-complete", 16);
	setglobal(str, fix_init_value(bvl_dominbcomplete), 0);
	str = save_string("minibuf-help", 12);
	setglobal(str, fix_init_value(bvl_dominbhelp), 0);
	str = save_string("minibuf-input", 13);
	setglobal(str, fix_init_value(bvl_dominbinput), 0);
	str = save_string("minibuf-print", 13);
	setglobal(str, fix_init_value(bvl_dominbprint), 0);
	str = save_string("minibuf-return", 14);
	setglobal(str, fix_init_value(bvl_dominbreturn), 0);
	str = save_string("minus", 5);
	setglobal(str, fix_init_value(bvl_doadd), 0);
	str = save_string("mod", 3);
	setglobal(str, fix_init_value(bvl_domod), 0);
	str = save_string("newline", 7);
	setglobal(str, fix_init_value(bvl_donewline), 0);
	str = save_string("next-line", 9);
	setglobal(str, fix_init_value(bvl_donextline), 0);
	str = save_string("nlambda", 7);
	setglobal(str, fix_init_value(bvl_donlambda), 0);
	str = save_string("not", 3);
	setglobal(str, fix_init_value(bvl_donot), 0);
	str = save_string("notify", 6);
	setglobal(str, fix_init_value(bvl_donotify), 0);
	str = save_string("nth", 3);
	setglobal(str, fix_init_value(bvl_donth), 0);
	str = save_string("nthcdr", 6);
	setglobal(str, fix_init_value(bvl_donthcdr), 0);
	str = save_string("nullp", 5);
	setglobal(str, fix_init_value(bvl_donullp), 0);
	str = save_string("numberp", 7);
	setglobal(str, fix_init_value(bvl_donumberp), 0);
	str = save_string("oddp", 4);
	setglobal(str, fix_init_value(bvl_dooddp), 0);
	str = save_string("or", 2);
	setglobal(str, fix_init_value(bvl_door), 0);
	str = save_string("patom", 5);
	setglobal(str, fix_init_value(bvl_dopatom), 0);
	str = save_string("pipe", 4);
	setglobal(str, fix_init_value(bvl_dopipe), 0);
	str = save_string("point", 5);
	setglobal(str, fix_init_value(bvl_dopointpos), 0);
	str = save_string("pop-to-buffer", 13);
	setglobal(str, fix_init_value(bvl_dopopbuffer), 0);
	str = save_string("popd", 4);
	setglobal(str, fix_init_value(bvl_dopopd), 0);
	str = save_string("popup-message", 13);
	setglobal(str, fix_init_value(bvl_dopopopmesg), 0);
	str = save_string("pp", 2);
	setglobal(str, fix_init_value(bvl_dopp), 0);
	str = save_string("preceding-character", 19);
	setglobal(str, fix_init_value(bvl_doprecchar), 0);
	str = save_string("previous-line", 13);
	setglobal(str, fix_init_value(bvl_doprevline), 0);
	str = save_string("princ", 5);
	setglobal(str, fix_init_value(bvl_doprinc), 0);
	str = save_string("print", 5);
	setglobal(str, fix_init_value(bvl_doprint), 0);
	str = save_string("prog", 4);
	setglobal(str, fix_init_value(bvl_doprog), 0);
	str = save_string("proof-document", 14);
	setglobal(str, fix_init_value(bvl_doproofdoc), 0);
	str = save_string("proof-goto-page", 15);
	setglobal(str, fix_init_value(bvl_doprfgotopage), 0);
	str = save_string("proof-move-absolute", 19);
	setglobal(str, fix_init_value(bvl_doprfmoveabs), 0);
	str = save_string("proof-move-relative", 19);
	setglobal(str, fix_init_value(bvl_doprfmoverel), 0);
	str = save_string("proof-moveto", 12);
	setglobal(str, fix_init_value(bvl_doprfmoveto), 0);
	str = save_string("proof-next-page", 15);
	setglobal(str, fix_init_value(bvl_doprfnextpage), 0);
	str = save_string("proof-select", 12);
	setglobal(str, fix_init_value(bvl_doprfselect), 0);
	str = save_string("proof-select-more", 17);
	setglobal(str, fix_init_value(bvl_doselmore), 0);
	str = save_string("proof-selected-region", 21);
	setglobal(str, fix_init_value(bvl_doprfselregion), 0);
	str = save_string("pushd", 5);
	setglobal(str, fix_init_value(bvl_dopushd), 0);
	str = save_string("putprop", 7);
	setglobal(str, fix_init_value(bvl_doputprop), 0);
	str = save_string("quote", 5);
	setglobal(str, fix_init_value(bvl_doquote), 0);
	str = save_string("quoted-insert", 13);
	setglobal(str, fix_init_value(bvl_doquotedinsert), 0);
	str = save_string("quotent", 7);
	setglobal(str, fix_init_value(bvl_doadd), 0);
	str = save_string("ratom", 5);
	setglobal(str, fix_init_value(bvl_doratom), 0);
	str = save_string("read", 4);
	setglobal(str, fix_init_value(bvl_doread), 0);
	str = save_string("readc", 5);
	setglobal(str, fix_init_value(bvl_doreadc), 0);
	str = save_string("recenter", 8);
	setglobal(str, fix_init_value(bvl_dorecenter), 0);
	str = save_string("remob", 5);
	setglobal(str, fix_init_value(bvl_doremob), 0);
	str = save_string("remove-local-variables", 22);
	setglobal(str, fix_init_value(bvl_doremovelocals), 0);
	str = save_string("remprop", 7);
	setglobal(str, fix_init_value(bvl_doremprop), 0);
	str = save_string("reset", 5);
	setglobal(str, fix_init_value(bvl_doreset), 0);
	str = save_string("return", 6);
	setglobal(str, fix_init_value(bvl_doreturn), 0);
	str = save_string("reverse", 7);
	setglobal(str, fix_init_value(bvl_doreverse), 0);
	str = save_string("rplaca", 6);
	setglobal(str, fix_init_value(bvl_dorplaca), 0);
	str = save_string("rplacd", 6);
	setglobal(str, fix_init_value(bvl_dorplacd), 0);
	str = save_string("scroll-backward", 15);
	setglobal(str, fix_init_value(bvl_doscrollback), 0);
	str = save_string("scroll-forward", 14);
	setglobal(str, fix_init_value(bvl_doscrollfwd), 0);
	str = save_string("search-backward", 15);
	setglobal(str, fix_init_value(bvl_dosearchbackwd), 0);
	str = save_string("search-backward-regexp", 22);
	setglobal(str, fix_init_value(bvl_dosearchbkregexp), 0);
	str = save_string("search-forward", 14);
	setglobal(str, fix_init_value(bvl_dosearchforwd), 0);
	str = save_string("search-forward-regexp", 21);
	setglobal(str, fix_init_value(bvl_dosrchfwdregex), 0);
	str = save_string("seek", 4);
	setglobal(str, fix_init_value(bvl_doseek), 0);
	str = save_string("self-insert", 11);
	setglobal(str, fix_init_value(bvl_doselfinsert), 0);
	str = save_string("set", 3);
	setglobal(str, fix_init_value(bvl_doset), 0);
	str = save_string("set-binding", 11);
	setglobal(str, fix_init_value(bvl_dosetbinding), 0);
	str = save_string("set-global-prefix", 17);
	setglobal(str, fix_init_value(bvl_dosetprefix), 0);
	str = save_string("set-internal-border", 19);
	setglobal(str, fix_init_value(bvl_dosetintborder), 0);
	str = save_string("set-mark", 8);
	setglobal(str, fix_init_value(bvl_dosetmark), 0);
	str = save_string("set-point", 9);
	setglobal(str, fix_init_value(bvl_dosetpoint), 0);
	str = save_string("set-window-mark", 15);
	setglobal(str, fix_init_value(bvl_dosetwindowmark), 0);
	str = save_string("set-window-point", 16);
	setglobal(str, fix_init_value(bvl_dosetwindowpoint), 0);
	str = save_string("setenv", 6);
	setglobal(str, fix_init_value(bvl_dosetenv), 0);
	str = save_string("setplist", 8);
	setglobal(str, fix_init_value(bvl_dosetplist), 0);
	str = save_string("setq", 4);
	setglobal(str, fix_init_value(bvl_dosetq), 0);
	str = save_string("soft-limit", 10);
	setglobal(str, fix_init_value(bvl_dosoftlim), 0);
	str = save_string("sopen", 5);
	setglobal(str, fix_init_value(bvl_dosopen), 0);
	str = save_string("split-window", 12);
	setglobal(str, fix_init_value(bvl_dosplitwindow), 0);
	str = save_string("start-formatter", 15);
	setglobal(str, fix_init_value(bvl_dostartformat), 0);
	str = save_string("start-proof-editor", 18);
	setglobal(str, fix_init_value(bvl_dostartproof), 0);
	str = save_string("stat", 4);
	setglobal(str, fix_init_value(bvl_dostat), 0);
	str = save_string("status", 6);
	setglobal(str, fix_init_value(bvl_dostatus), 0);
	str = save_string("store", 5);
	setglobal(str, fix_init_value(bvl_dostore), 0);
	str = save_string("stridx", 6);
	setglobal(str, fix_init_value(bvl_dostridx), 0);
	str = save_string("stridxr", 7);
	setglobal(str, fix_init_value(bvl_dostridxr), 0);
	str = save_string("stringp", 7);
	setglobal(str, fix_init_value(bvl_dostringp), 0);
	str = save_string("strlen", 6);
	setglobal(str, fix_init_value(bvl_dostrlen), 0);
	str = save_string("strmapc", 7);
	setglobal(str, fix_init_value(bvl_dostrmapc), 0);
	str = save_string("substr", 6);
	setglobal(str, fix_init_value(bvl_dosubstr), 0);
	str = save_string("switch-to-buffer", 16);
	setglobal(str, fix_init_value(bvl_doswitchbuf), 0);
	str = save_string("switch-to-window", 16);
	setglobal(str, fix_init_value(bvl_doswitchwindow), 0);
	str = save_string("symbolp", 7);
	setglobal(str, fix_init_value(bvl_dosymbolp), 0);
	str = save_string("symeval", 7);
	setglobal(str, fix_init_value(bvl_doeval), 0);
	str = save_string("syntax", 6);
	setglobal(str, fix_init_value(bvl_dosyntax), 0);
	str = save_string("terpri", 6);
	setglobal(str, fix_init_value(bvl_doterpri), 0);
	str = save_string("throw", 5);
	setglobal(str, fix_init_value(bvl_dothrow), 0);
	str = save_string("time", 4);
	setglobal(str, fix_init_value(bvl_dotime), 0);
	str = save_string("timecmp", 7);
	setglobal(str, fix_init_value(bvl_dotimecmp), 0);
	str = save_string("times", 5);
	setglobal(str, fix_init_value(bvl_doadd), 0);
	str = save_string("umask", 5);
	setglobal(str, fix_init_value(bvl_doumask), 0);
	str = save_string("unwind-protect", 14);
	setglobal(str, fix_init_value(bvl_douwprotect), 0);
	str = save_string("use-global-map", 14);
	setglobal(str, fix_init_value(bvl_douseglobal), 0);
	str = save_string("use-local-map", 13);
	setglobal(str, fix_init_value(bvl_douselocal), 0);
	str = save_string("wait", 4);
	setglobal(str, fix_init_value(bvl_dowait), 0);
	str = save_string("while", 5);
	setglobal(str, fix_init_value(bvl_dowhile), 0);
	str = save_string("window-mark", 11);
	setglobal(str, fix_init_value(bvl_dowindowmark), 0);
	str = save_string("window-point", 12);
	setglobal(str, fix_init_value(bvl_dowindowpoint), 0);
	str = save_string("windowp", 7);
	setglobal(str, fix_init_value(bvl_dowindowp), 0);
	str = save_string("write-file", 10);
	setglobal(str, fix_init_value(bvl_dowritefile), 0);
	str = save_string("write-named-file", 16);
	setglobal(str, fix_init_value(bvl_dowritenamed), 0);
	str = save_string("x-bell-volume", 13);
	setglobal(str, fix_init_value(bvl_dovolume), 0);
	str = save_string("x-fetch-buffer", 14);
	setglobal(str, fix_init_value(bvl_doXfetch), 0);
	str = save_string("x-get-border-width", 18);
	setglobal(str, fix_init_value(bvl_doXgetbdwidth), 0);
	str = save_string("x-get-display", 13);
	setglobal(str, fix_init_value(bvl_doXgetdisp), 0);
	str = save_string("x-get-focus", 11);
	setglobal(str, fix_init_value(bvl_doXgetfocus), 0);
	str = save_string("x-get-font", 10);
	setglobal(str, fix_init_value(bvl_doXgetfont), 0);
	str = save_string("x-get-geometry", 14);
	setglobal(str, fix_init_value(bvl_doXgetgeom), 0);
	str = save_string("x-lower-window", 14);
	setglobal(str, fix_init_value(bvl_doXlower), 0);
	str = save_string("x-raise-window", 14);
	setglobal(str, fix_init_value(bvl_doXraise), 0);
	str = save_string("x-set-border-width", 18);
	setglobal(str, fix_init_value(bvl_doXsetbdwidth), 0);
	str = save_string("x-set-focus", 11);
	setglobal(str, fix_init_value(bvl_doXsetfocus), 0);
	str = save_string("x-set-font", 10);
	setglobal(str, fix_init_value(bvl_doXsetfont), 0);
	str = save_string("x-set-geometry", 14);
	setglobal(str, fix_init_value(bvl_doXsetgeom), 0);
	str = save_string("x-store-buffer", 14);
	setglobal(str, fix_init_value(bvl_doXstore), 0);
	str = save_string("yes-or-no", 9);
	setglobal(str, fix_init_value(bvl_doyesorno), 0);
	str = save_string("zerop", 5);
	setglobal(str, fix_init_value(bvl_dozerop), 0);

	return (0);
}
