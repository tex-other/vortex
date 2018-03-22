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
 *  RCS Info: $Header: domenu.c,v 0.1 87/05/01 11:55:09 john Locked $
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
 *  domenu.c - vLisp interface functions to pop-up menus
 */
static char _ID[] = "@(#)domenu.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include <signal.h>
#include "vse.h"
#include "vlisp.h"
#include "function.h"
#include "window.h"

/*
 *  DOCUMENTATION
 *
 *  Name: menu
 *  Call: (menu 'title 'choices [ 'choose ])
 *  Retu: fixnum
 *  Desc: This function brings up a popup menu displaying the given
 *	choices and allows the user to select one with the mouse.
 *	The index of the selected item is returned, or nil if none
 *	was chosen.  The first argument should be a string containing
 *	the title string (which will be centered on the line) or nil
 *	if there should be no title.  The second argument should be a
 *	non-nil list of strings representing the menu choices, the
 *	index in this list (zero-based) is returned identifying the
 *	choice.  If the third argument is present and non-nil it
 *	forces the user to select one of the specified items, it will
 *	not return nil.
 *
 *	The font used to display the menu is controlled by the
 *	\lit{MenuFont} X default.  The colors used for the window
 *	are the same as those used for the current window.  The title
 *	is displayed inverse-video in the name stripe color and the
 *	individual choices are displayed in the body font color.
 *
 *	The upper left corner of the menu will appear at the position
 *	of the mouse cursor, unless that would place part of it off
 *	the screen.  The menu remains until the first mouse button is
 *	pressed, then returns the selection the mouse button was over
 *	or nil if the mouse was on no selection.
 *  Side: A new window appears on the X display and the mouse is
 *	grabbed until the menu is done so nothing else can be done
 *	until the menu is handled.
 *  SeeA: popup-message popup-confirm popup-input
 */

#define ITEMLENGTH	32
#define MENULENGTH	100

DEFUN(domenu, "menu", FUNC_VISUAL, NULL)
{
	extern char	*alloca();
	struct value	arg, elt, ret;
	char		*choices[MENULENGTH+1];
	char		title[32];
	int		choose = FALSE;
	int		got, count;
	register int	n;

	CHECKAC(2, 3);
	arg = EVALARGN(1);
	if (!stringp(arg))
		BADARGN(1, "a string title");
	makecstring(gstring(arg.vl_data), title, sizeof (title));

	arg = EVALARGN(2);
	if (!dtprp(arg))
		BADARGN(2, "a non-nil list");
	count = length(arg);
	for (n = 0; n < count && n < MENULENGTH; n++) {
		elt = nth(n, arg);
		if (!stringp(elt))
			error("%s element of choice list isn't a string!",
			      nthname(n+1));
		choices[n] = alloca(ITEMLENGTH);
		ASSERT(choices[n] != NULL);
		makecstring(gstring(elt.vl_data), choices[n], ITEMLENGTH);
	}
	choices[n] = NULL;

	if (GETACOUNT() > 2) {
		arg = EVALARGN(3);
		if (truep(arg))
			choose = TRUE;
	}

	/* do the pop-up menu now */
	got = popupmenu(title, choices, choose);
	if (got < 0 || got >= count) {
		/* user didn't choose */
		return (v_nil);
	} else {
		/* return choice index */
		ret.vl_type = LISP_FIXNUM;
		sfixnum(ret.vl_data, got);
		return (ret);
	}
}

static FontInfo	*menufont = NULL;
static int	EVENTS = KeyPressed|ButtonPressed;

#include "bitmaps/menu"
#include "bitmaps/menu_mask"

#define TEXTBORDER	4
#define ITEMHEIGHT	(menufont->height + (2 * TEXTBORDER))

extern int	MenuForePixel, MenuBackPixel;
extern Pixmap	MenuForePixmap, MenuBackPixmap;

popupmenu(title, choices, choose)
	char	*title, *choices[];
{
	extern char	*DEFMENUFONT;
	extern Cursor	BaseCursor;
	extern int	abort_char;
	extern int	MousePixel, BorderPixel;
	register int	i, count, wid;
	struct window	*winp = current_window;
	int		x, y, w, h;
	Window		window;
	static Cursor	cursor = NULL;
	XEvent		event;
	XKeyEvent	*key;
	XButtonEvent	*but;
	int		select = -1;
	int		msx, msy, len, tx, ty, n;
	Window		subwin;
	char		*str;

	/* make sure we have a FontInfo to use */
	if (menufont == NULL && (menufont = XOpenFont(DEFMENUFONT)) == NULL)
		menufont = winp->wi_fontinfo;

	/* count the choices and find maximum length */
	wid = XStringWidth(title, menufont, 0, 0);
	for (count = 0; choices[count] != NULL; count++) {
		w = XStringWidth(choices[count], menufont, 0, 0);
		if (w > wid)
			wid = w;
	}

	/* figure out size of pop-up menu window */
	w = wid + 2;
	h = (count + 1) * ITEMHEIGHT;

	/* place the window on top of the current window and grab mouse */
	x = winp->wi_xpos + (winp->wi_xsize - w) / 2;
	y = winp->wi_ypos + (winp->wi_ysize - h) / 2;

	/* make the X window and set things up */
	window = XCreateWindow(RootWindow, x, y, w, h,
			       1, MenuForePixmap, MenuBackPixmap);
	if (window == NULL)
		error("Can't create %dx%d+%d+%d window for menu!", x, y, w, h);
	XStoreName(window, "vortex (menu)");
	XMapWindow(window);

	/* create menu cursor shape */
	if (cursor == NULL) {
		cursor = XCreateCursor(menu_width, menu_height,
				       menu_bits,  menu_mask_bits,
				       menu_x_hot, menu_y_hot,
				       MousePixel, MenuBackPixel, GXcopy);
		if (cursor == NULL)
			cursor = BaseCursor;
	}
	ASSERT(cursor != NULL);

	/* grab the mouse */
	if (XGrabMouse(window, cursor, EVENTS) == NULL)
		error("Can't grab mouse for menu!");

	/* draw the pop-up menu window */
	XClear(window);
	XPixSet(window, 0, 0, w, ITEMHEIGHT, winp->wi_stpixel);
	tx = (w - XStringWidth(title, menufont, 0, 0)) / 2;
	XText(window, tx, TEXTBORDER, title, strlen(title),
	      menufont->id, winp->wi_bgpixel, winp->wi_stpixel);
	for (n = 0; n < count; n++) {
		ty = (n + 1) * ITEMHEIGHT;
		/* separate item from previous one */
		XLine(window, 0, ty, w, ty,
		      1, 1, MenuForePixel, GXcopy, AllPlanes);
		/* draw the text for this item */
		tx = (w - XStringWidth(choices[n], menufont, 0, 0)) / 2;
		XText(window, tx, ty + TEXTBORDER,
		      choices[n], strlen(choices[n]),
		      menufont->id, MenuForePixel, MenuBackPixel);
	}

	/* warp the mouse into the menu */
	XWarpMouse(window, w / 2, ITEMHEIGHT / 2);

	/* read events until user makes a choice */
	for (;;) {
		XNextEvent(&event);
		switch((int)event.type) {
		case KeyPressed:
			key = (XKeyEvent *)&event;
			/* handle input characters */
			str = XLookupMapping(key, &len);
			while (len-- > 0) {
				if (*str == abort_char) {
					XUngrabMouse();
					XDestroyWindow(window);
					int_abort();
					/* NOTREACHED */
				}
				str++;
			}
			XFeep(1);
			break;
		case ButtonPressed:
			/* get position into global variables */
			but = (XButtonEvent *)&event;
			XInterpretLocator(window,
					  &msx, &msy,
					  &subwin, but->location);

			/* is it in our window at all? */
			if (msx < 0 || msy < 0 || msx >= w || msy >= h) {
				if (choose) {
					XFeep(1);
					continue;
				} else {
					select = -1;
					goto done;
				}
			}

			/* which choice does this translate to */
			select = (msy / ITEMHEIGHT) - 1;
			if ((select < 0 || select >= count) && choose) {
				XFeep(1);
				continue;
			} else {
				/* got a valid selection */
				goto done;
			}
		}
	}

done:	/* clean up after the menu */
	XUngrabMouse();
	XDestroyWindow(window);
	XSync(FALSE);

	/* return a reasonable selection index */
	if (select < 0 || select >= count)
		select = -1;
	return (select);
}
