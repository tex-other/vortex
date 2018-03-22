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
 *  RCS Info: $Header: fmtcode.h,v 0.1 87/04/30 20:53:04 john Locked $
 *
 *  VorTeX - Visually Oriented TeX
 *  A source-based WYSIWYG editor for beautiful documents
 *
 *  This file is part of the source editor/user interface written
 *  by John Coker for the VorTeX project under the direction of
 *  Prof. Michael A. Harrison of the University of California at
 *  Berkeley.
 *
 *  Copyright (c) 1987 John L. Coker
 *  University of California, Berkeley
 *  john@renoir.Berkeley.EDU
 *  vortex@renoir.Berkeley.EDU
 *
 *  This file intact and any code derived from it are copyright
 *  John Coker and the Regents of the University of California.
 *  All rights reserved by the author.  See the copyright notice
 *  distributed with this software for a complete description of
 *  the conditions under which it is made available.
 *
 *  fmtcode.h - code to implement varargs parsing
 */
 
/*
 *  This is gross, but we want to avoid having to repeat this ugly
 *  code for each variable argument print function.  The code below
 *  implements the extended printf(3) described in "format.h" for
 *  a varargs function in which it should be included.  The file
 *  "fmtdecl.h" should already have been incorporated to declare
 *  and initialize the necessary variables.
 *
 *  This code writes the formatted result into a buffer called msgbuf,
 *  with the length put into msglen.  Note that msgbuf will always be
 *  null-terminated, and msglen doesn't include that '\0' character.
 */

	/* start variable arguments and get format */
	if (!_started)
		STARTVA();
	_format = GETVARG(char *);
	if (_format == NULL)
		_format = "";
	_fp = _format;
	_fend = _format + strlen(_format);

	/* loop through format string handling characters */
	while (_fp < _fend && mp < mend) {
		if (*_fp != '%') {
			*mp++ = *_fp++;
			continue;
		}

		/* scan field specification */
		_type = scanfield(&_fp, _fend - _fp, &_adj, &_min, &_max);
		if (_type < 0 || _type > 0177) {
			/* format reading error, output % sign */
			*mp++ = '%';
			continue;
		}

		/* get width values if necessary */
		if (_min == WID_GETARG) {
			_min = GETVARG(int);
			if (_min < 0)
				_min = 0;
		}
		if (_max == WID_GETARG) {
			_max = GETVARG(int);
			if (_max < 0)
				_max = 0;
		}

		/* tack this field onto output string */
		switch (_type) {
		case FMT_DECIMAL:
			_ival = GETVARG(int);
			_cptr = itoa(_ival, 10);
			mp += prtfield(_cptr, strlen(_cptr),
				       mp, mend - mp, _adj, _min, _max);
			break;
		case FMT_UNSIGNED:
			_uval = GETVARG(unsigned int);
			_cptr = utoa(_uval, 10);
			mp += prtfield(_cptr, strlen(_cptr),
				       mp, mend - mp, _adj, _min, _max);
			break;
		case FMT_OCTAL:
			_ival = GETVARG(int);
			_cptr = utoa(_ival, 8);
			mp += prtfield(_cptr, strlen(_cptr),
				       mp, mend - mp, _adj, _min, _max);
			break;
		case FMT_HEX:
			_ival = GETVARG(int);
			_cptr = utoa(_ival, 16);
			mp += prtfield(_cptr, strlen(_cptr),
				       mp, mend - mp, _adj, _min, _max);
			break;
		case FMT_BINARY:
			_ival = GETVARG(int);
			_cptr = utoa(_ival, 2);
			mp += prtfield(_cptr, strlen(_cptr),
				       mp, mend - mp, _adj, _min, _max);
			break;
		case FMT_FLOAT:
			_fval = GETVARG(double);
			_cptr = ftoa(_fval);
			mp += prtfield(_cptr, strlen(_cptr),
				       mp, mend - mp, _adj, _min, _max);
			break;
		case FMT_EXPT:
			_fval = GETVARG(double);
			_cptr = etoa(_fval);
			mp += prtfield(_cptr, strlen(_cptr),
				       mp, mend - mp, _adj, _min, _max);
			break;
		case FMT_FSHORT:
			_fval = GETVARG(double);
			_cptr = gtoa(_fval);
			mp += prtfield(_cptr, strlen(_cptr),
				       mp, mend - mp, _adj, _min, _max);
			break;
		case FMT_STRING:
			_cptr = GETVARG(char *);
			if (_cptr == NULL)
				_cptr = NULLPTR;
			mp += prtfield(_cptr, strlen(_cptr),
				       mp, mend - mp, _adj, _min, _max);
			break;
		case FMT_CHAR:
			_ival = GETVARG(int);
			_uchr = _ival & 0377;
			mp += prtfield(&_uchr, 1,
				       mp, mend - mp, _adj, _min, _max);
			break;
		case FMT_PNTCHAR:
			_ival = GETVARG(int);
			_cptr = pchar(_ival);
			mp += prtfield(_cptr, strlen(_cptr),
				       mp, mend - mp, _adj, _min, _max);
			break;
		case FMT_LSTRING:
			_sval = GETVARG(struct string *);
			if (_sval == NULL) {
				mp += prtfield(NULLPTR, strlen(NULLPTR),
					       mp, mend - mp,
					       _adj, _min, _max);
			} else {
				mp += prtfield(_sval->st_buffer,
					       _sval->st_length,
					       mp, mend - mp,
					       _adj, _min, _max);
			}
			break;
		case FMT_SYMBOL:
			_sval = GETVARG(struct string *);
			if (_sval == NULL)
				_cptr = NULLPTR;
			else
				_cptr = psymbol(_sval);
			mp += prtfield(_cptr, strlen(_cptr),
				       mp, mend - mp, _adj, _min, _max);
			break;
		case FMT_VALUE:
			_vval = GETVARG(struct value);
			_cptr = psexpr(_vval);
			mp += prtfield(_cptr, strlen(_cptr),
				       mp, mend - mp, _adj, _min, _max);
			break;
		case FMT_ROMAN:
			_ival = GETVARG(int);
			_cptr = itor(_ival);
			mp += prtfield(_cptr, strlen(_cptr),
				       mp, mend - mp, _adj, _min, _max);
			break;
		case FMT_KEYCODE:
			_ival = GETVARG(int);
			_cptr = pkeycode(_ival);
			mp += prtfield(_cptr, strlen(_cptr),
				       mp, mend - mp, _adj, _min, _max);
			break;
		case FMT_KEYSEQ:
			_sval = GETVARG(struct string *);
			if (_sval == NULL)
				_cptr = NULLPTR;
			else
				_cptr = pkeyseq(_sval);
			mp += prtfield(_cptr, strlen(_cptr),
				       mp, mend - mp, _adj, _min, _max);
			break;
		default:	/* some regular _type */
			*mp++ = _type;
			break;
		}
	}
	ENDVA();
	*mp = '\0';
	msglen = mp - msgbuf;
