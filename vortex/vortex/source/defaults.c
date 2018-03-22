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
 *  RCS Info: $Header: defaults.c,v 0.1 87/05/01 11:31:06 john Locked $
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
 *  defaults.c - routine to extract appropriate X defaults
 */
static char _ID[] = "@(#)defaults.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include <ctype.h>
#include "vse.h"
#include "vlisp.h"
#include "window.h"

/*
 *  The default colors for normal and reversed video.  The user may
 *  specify colors explicitly or implicitly with ReverseVideo: on
 *  for a black-and-white display.
 */
static char	WHITE[] = "white";
static char	BLACK[] = "black";

#define DEFFGCOLOR	BLACK		/* default foreground color */
#define DEFBGCOLOR	WHITE		/* default background color */
#define DEFBDCOLOR	BLACK		/* default border color */
#define DEFMSCOLOR	BLACK		/* default mouse cursor color */
#define DEFSTCOLOR	BLACK		/* default name stripe color */
#define DEFHLCOLOR	BLACK		/* default high-light color */
#define DEFMFCOLOR	BLACK		/* default menu foreground color */
#define DEFMBCOLOR	WHITE		/* default menu background color */

#define REVFGCOLOR	WHITE		/* reverse foreground color */
#define REVBGCOLOR	BLACK		/* reverse background color */
#define REVBDCOLOR	WHITE		/* reverse border color */
#define REVMSCOLOR	WHITE		/* reverse mouse cursor color */
#define REVSTCOLOR	WHITE		/* reverse name stripe color */
#define REVHLCOLOR	WHITE		/* reverse high-light color */
#define REVMFCOLOR	WHITE		/* reverse menu foreground color */
#define REVMBCOLOR	BLACK		/* reverse menu background color */

/*
 *  The global variables to hold the defaults.  The program defaults
 *  are assigned here at compile time.  Once at run time, the user's
 *  X defaults (from the ~/.Xdefaults file) will be checked to replace
 *  the default values assigned below.
 */
char		*DEFFONTNAME =		"vtsingle";
char		*DEFMINBFONT =		"vtsingle";
char		*DEFICONFONT =		"vtsingle";
char		*DEFSIZEFONT =		"vtdwidth";
char		*DEFMENUFONT =		"vtdwidth";
int		DEFBDWIDTH =		2;
int		DEFINTBORDER =		1;
char		*DEFGEOMETRY =		"=80x24";
char		*PROOFGEOMETRY =	"=80x24";
char		*ALTGEOMETRY =		"=80x54";
char		*MINBGEOMETRY =		"=80x1+10-50";
char		*ICONGEOMETRY =		"=96x40+10+5";

static char	REVERSEVIDEO[]		= "ReverseVideo";
static char	FOREGROUND[]		= "Foreground";
static char	BACKGROUND[]		= "Background";
static char	BORDER[]		= "Border";
static char	MOUSE[]			= "Mouse";
static char	NAMESTRIPE[]		= "NameStripe";
static char	HIGHLIGHT[]		= "HighLight";
static char	MENUFORE[]		= "MenuForeground";
static char	MENUBACK[]		= "MenuBackground";
static char	BORDERWIDTH[]		= "BorderWidth";
static char	INTERNALBORDER[]	= "InternalBorder";
static char	ICONFONT[]		= "IconFont";
static char	MINBFONT[]		= "MinibufFont";
static char	BODYFONT[]		= "BodyFont";
static char	SIZEFONT[]		= "ResizeFont";
static char	MENUFONT[]		= "MenuFont";
static char	GEOMETRY[]		= "Geometry";
static char	PROOFGEOM[]		= "ProofGeometry";
static char	ALTGEOM[]		= "AltGeometry";
static char	MINBGEOM[]		= "MinibufGeometry";
static char	ICONGEOM[]		= "IconGeometry";

extern int	ForePixel;
extern int	BackPixel;
extern int	MousePixel;
extern int	StripePixel;
extern int	HiLightPixel;
extern int	BorderPixel;
extern Pixmap	BackPixmap;
extern Pixmap	BorderPixmap;

extern int	MenuForePixel;
extern int	MenuBackPixel;
extern Pixmap	MenuForePixmap;
extern Pixmap	MenuBackPixmap;

getdefaults()
{
	extern char	*program;
	extern int	bwpixel(), cpixel();
	extern Pixmap	bwpixmap(), cpixmap();
	char		*value;
	char		*fgcolor, *bgcolor, *bdcolor;
	char		*mscolor, *stcolor, *hlcolor;
	char		*mfcolor, *mbcolor;

	/*
	 *  Establish default color values.  We have the compile-
	 *  time defaults, the possibility of reversing the colors
	 *  (only makes sense on a black-and-white display) and
	 *  individual color specifications.
	 */
	if (!Xhavecolor &&
	    (value = XGetDefault(program, REVERSEVIDEO)) != NULL &&
	    (!strcmp(value, "on") || !strcmp(value, "true"))) {
		fgcolor = REVFGCOLOR;
		bgcolor = REVBGCOLOR;
		bdcolor = REVBDCOLOR;
		mscolor = REVMSCOLOR;
		stcolor = REVSTCOLOR;
		hlcolor = REVHLCOLOR;
		mfcolor = REVMFCOLOR;
		mbcolor = REVMBCOLOR;
	} else {
		fgcolor = DEFFGCOLOR;
		bgcolor = DEFBGCOLOR;
		bdcolor = DEFBDCOLOR;
		mscolor = DEFMSCOLOR;
		stcolor = DEFSTCOLOR;
		hlcolor = DEFHLCOLOR;
		mfcolor = DEFMFCOLOR;
		mbcolor = DEFMBCOLOR;
	}

	if (Xhavecolor) {
		if ((value = XGetDefault(program, FOREGROUND)) != NULL)
			fgcolor = value;
		if ((value = XGetDefault(program, BACKGROUND)) != NULL)
			bgcolor = value;
		if ((value = XGetDefault(program, BORDER)) != NULL)
			bdcolor = value;
		if ((value = XGetDefault(program, MOUSE)) != NULL)
			mscolor = value;
		if ((value = XGetDefault(program, NAMESTRIPE)) != NULL)
			stcolor = value;
		if ((value = XGetDefault(program, HIGHLIGHT)) != NULL)
			hlcolor = value;
		if ((value = XGetDefault(program, MENUFORE)) != NULL)
			mfcolor = value;
		if ((value = XGetDefault(program, MENUBACK)) != NULL)
			mbcolor = value;
	}

	/*
	 *  Now get these color values into the global pixel and
	 *  pixmap (tile) variables.  If any of these colors are
	 *  bad, we'll encounter an error here.
	 */
	if (Xhavecolor) {
		ForePixel = cpixel(fgcolor, FOREGROUND);
		BackPixel = cpixel(bgcolor, BACKGROUND);
		BackPixmap = cpixmap(bgcolor, BACKGROUND);
		BorderPixel = cpixel(bdcolor, BORDER);
		BorderPixmap = cpixmap(bdcolor, BORDER);
		MousePixel = cpixel(mscolor, MOUSE);
		StripePixel = cpixel(stcolor, NAMESTRIPE);
		HiLightPixel = cpixel(hlcolor, HIGHLIGHT);
		MenuForePixel = cpixel(mfcolor, MENUFORE);
		MenuForePixmap = cpixmap(mfcolor, MENUFORE);
		MenuBackPixel = cpixel(mbcolor, MENUBACK);
		MenuBackPixmap = cpixmap(mbcolor, MENUBACK);
	} else {
		ForePixel = bwpixel(fgcolor, FOREGROUND);
		BackPixel = bwpixel(bgcolor, BACKGROUND);
		BackPixmap = bwpixmap(bgcolor, BACKGROUND);
		BorderPixel = bwpixel(bdcolor, BORDER);
		BorderPixmap = bwpixmap(bdcolor, BORDER);
		MousePixel = bwpixel(mscolor, MOUSE);
		StripePixel = bwpixel(stcolor, NAMESTRIPE);
		HiLightPixel = bwpixel(hlcolor, HIGHLIGHT);
		MenuForePixel = bwpixel(mfcolor, MENUFORE);
		MenuForePixmap = bwpixmap(mfcolor, MENUFORE);
		MenuBackPixel = bwpixel(mbcolor, MENUBACK);
		MenuBackPixmap = bwpixmap(mbcolor, MENUBACK);
	}

	/*
	 *  Establish default border widths.  BorderWidth is the
	 *  width of the X window border and InternalBorder is the
	 *  gap between the inside of the X window and the buffer.
	 */
	if ((value = XGetDefault(program, BORDERWIDTH)) != NULL) {
		if (!isdigit(*value))
			error("Window border width must be an integer!");
		else
			DEFBDWIDTH = atoi(value);
	}
	if (DEFBDWIDTH < 0)
		DEFBDWIDTH = 0;
	if ((value = XGetDefault(program, INTERNALBORDER)) != NULL) {
		if (!isdigit(*value))
			error("Internal border width must be an integer!");
		else
			DEFINTBORDER = atoi(value);
	}
	if (DEFINTBORDER < 0)
		DEFINTBORDER = 0;

	/*
	 *  Establish the default font name.  We have a compile-time
	 *  default, possibly a passed in value, and the value from
	 *  the user's .Xdefaults file.
	 */
	if ((value = XGetDefault(program, BODYFONT)) != NULL && *value != '\0')
		DEFFONTNAME = DEFMINBFONT = DEFICONFONT = value;
	if ((value = XGetDefault(program, MINBFONT)) != NULL && *value != '\0')
		DEFMINBFONT = value;
	if ((value = XGetDefault(program, ICONFONT)) != NULL && *value != '\0')
		DEFICONFONT = value;
	if ((value = XGetDefault(program, SIZEFONT)) != NULL && *value != '\0')
		DEFSIZEFONT = value;
	if ((value = XGetDefault(program, MENUFONT)) != NULL && *value != '\0')
		DEFMENUFONT = value;

	/*
	 *  Get the default geometry sizes.  We have compile-time
	 *  defaults which may be specified in the user's .Xdefaults
	 *  file.  Note that Geometry and ProofGeometry should
	 *  be the same unless the ProofGeometry is given explicitly.
	 */
	if ((value = XGetDefault(program, GEOMETRY)) != NULL && *value == '=')
		DEFGEOMETRY = PROOFGEOMETRY = value;
	if ((value = XGetDefault(program, PROOFGEOM)) != NULL && *value == '=')
		PROOFGEOMETRY = value;
	if ((value = XGetDefault(program, ALTGEOM)) != NULL && *value == '=')
		ALTGEOMETRY = value;
	if ((value = XGetDefault(program, MINBGEOM)) != NULL && *value == '=')
		MINBGEOMETRY = value;
	if ((value = XGetDefault(program, ICONGEOM)) != NULL && *value == '=')
		ICONGEOMETRY = value;

	return (0);
}

static char	UNKN_MSG[] = "Invalid %s color \"%s\" on this display!";

#define UNKNOWN(c,w)	error(UNKN_MSG, (w), (c))

static int
bwpixel(cname, what)
	char	*cname, *what;
{
	ASSERT(cname != NULL && *cname != '\0');

	if (!strcmp(cname, BLACK))
		return (BlackPixel);
	if (!strcmp(cname, WHITE))
		return (WhitePixel);

	UNKNOWN(cname, what);
	/* NOTREACHED */
}

static Pixmap
bwpixmap(cname, what)
	char	*cname, *what;
{
	ASSERT(cname != NULL && *cname != '\0');

	if (!strcmp(cname, BLACK))
		return (BlackPixmap);
	if (!strcmp(cname, WHITE))
		return (WhitePixmap);

	UNKNOWN(cname, what);
	/* NOTREACHED */
}

static int
cpixel(cname, what)
	char	*cname, *what;
{
	Color	color;

	ASSERT(cname != NULL && *cname != '\0');

	if (XParseColor(cname, &color) == 0)
		UNKNOWN(cname, what);
	if (XGetHardwareColor(&color) == 0)
		error("Can't allocate a color map entry for \"%s\"!", cname);
	return (color.pixel);
}

static Pixmap
cpixmap(cname, what)
	char	*cname, *what;
{
	int	pixel;
	Pixmap	tile;

	pixel = cpixel(cname, what);
	if ((tile = XMakeTile(pixel)) == NULL)
		error("Can't make a Pixmap from a \"%s\" pixel!", cname);
	return (tile);
}
