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
 *  RCS Info: $Header: window.c,v 0.1 87/05/01 12:32:52 john Locked $
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
 *  window.c - internal editor window management routines
 */
static char _ID[] = "@(#)window.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include "vse.h"
#include "vlisp.h"
#include "process.h"
#include "buffer.h"
#include "window.h"
#include "parser.h"

Display		*Xdisplay = NULL;	/* X display handle */
char		Xdispname[64];		/* name of display */
int		Xconnected = FALSE;	/* flag inticating active X */
int		Xhavecolor;		/* flag for color display */
int		Xscr_width;		/* width of X display */
int		Xscr_height;		/* height of X display */
FontInfo	*iconfont = NULL;	/* font for icon text */

int	ForePixel;			/* default foreground color */
int	BackPixel;			/* default background color */
int	MousePixel;			/* default mouse color */
int	StripePixel;			/* default name stripe color */
int	HiLightPixel;			/* default high-light color */
int	BorderPixel;			/* default border color */
Pixmap	BackPixmap;			/* default background tile */
Pixmap	BorderPixmap;			/* default border tile */
Pixmap	GrayPixmap;			/* 50% gray pixmap */

int	MenuForePixel;			/* pop-up menu foreground color */
int	MenuBackPixel;			/* background */
Pixmap	MenuForePixmap;			/* foreground pixmap */
Pixmap	MenuBackPixmap;			/* background */

/*
 *  We make a bogus window structure and keep it around, just
 *  in case some routine called from the lisp system accesses
 *  it when we aren't editing interactively.  At compile time,
 *  we make it the only thing in the window list, and also the
 *  current window.
 */
static struct window	fake_window = {
	WIN_BUFFER, 0, 0,	/* wi_type, wi_flags and wi_index */
	NULL, "",		/* wi_xwindow and wi_windname */
	NULL,			/* wi_iconwin */
	NULL, "",		/* wi_fontinfo and wi_fontname */
	{ 0, 0, 0, 0 },		/* wi_whole */
	{ 0, 0, 0, 0 },		/* wi_display */
	{ 0, 0, 0, 0 },		/* wi_icon */
	0, 0,			/* wi_cols and wi_cols */
	NULL,			/* wi_buffer */
	0, 0, 0, 0,		/* wi_bufpos[1-4] */
	NULL, NULL,		/* wi_screen and wi_stripe */
	0, 0,			/* wi_fgpixel and wi_bgpixel */
	0, 0,			/* wi_stpixel and wi_hlpixel */
	NULL, NULL,		/* wi_bgpixmap and wi_bdpixmap */
	0, 0,			/* wi_bdwidth and wi_border */
	NULL,			/* wi_next */
};

struct window	*window_list = &fake_window;
struct window	*current_window = &fake_window;

static char	DISPLAY[] = "unix:0";	/* if nothing else, try this */

#include "bitmaps/gray50%"

#include "bitmaps/arrow_image"
#include "bitmaps/arrow_mask"

Cursor	BaseCursor = NULL;		/* default cursor for all windows */

initwindows(display, fontname, geometry)
	char	*display, *fontname, *geometry;
{
	extern char	*getenv();
	extern int	Xfatal(), Xerror();
	extern FontInfo	*fixedfont();
	extern char	*program;
	extern char	*DEFSTRIPEFONT, *DEFICONFONT;
	struct window	*new;
	WindowInfo	info;
	Bitmap		bitmap;

	/* try to get some reasonable display name */
	if (display == NULL) {
		display = getenv("DISPLAY");
		debug(DXIO, "Trying to open given display \"%s\".", display);
	}
	if (display == NULL) {
		display = DISPLAY;
		debug(DXIO, "Trying to open default display \"%s\".", display);
	}

	/* open initial display */
	if ((Xdisplay = XOpenDisplay(display)) == NULL)
		error("Can't connect to X display server \"%s\"!", display);
	strncpy(Xdispname, display, sizeof (Xdispname) - 1);
	Xdispname[sizeof (Xdispname) - 1] = '\0';

	/* get display size from RootWindow */
	if (XQueryWindow(RootWindow, &info) == 0)
		error("Can't get size of root window; something's fucked!");
	Xscr_width = info.width;
	Xscr_height = info.height;

	/* check if we have colors here */
	Xhavecolor = DisplayCells(Xdisplay) > 2;

	/* make 50% gray pixmap */
	if ((bitmap = XStoreBitmap(g50_width, g50_height, g50_bits)) == NULL)
		error("Can't make X bitmap for 50% gray tile!");
	if ((GrayPixmap = XMakePixmap(bitmap, BlackPixel, WhitePixel)) == NULL)
		error("Can't make X pixmap for 50% gray tile!");
	XFreeBitmap(bitmap);

	/* set up global defaults */
	getdefaults();

	/* open icon text font */
	iconfont = fixedfont(DEFICONFONT);

	/* create default cursor shape */
	BaseCursor = XCreateCursor(arrow_image_width, arrow_image_height,
				   arrow_image_bits,  arrow_mask_bits,
				   arrow_image_x_hot, arrow_image_y_hot,
				   MousePixel, BackPixel, GXcopy);

	/* create initial/current window */
	current_window = window_list = NULL;
	debug(DBUFFER|DWINDOW, "Making initial window's buffer %Y...",
	      current_buffer->bu_name);
	new = makewindow(geometry, fontname, current_buffer);
	switchwindow(new, FALSE, FALSE);

	/* install our own error handlers */
	XIOErrorHandler(Xfatal);
	XErrorHandler(Xerror);

	Xconnected = TRUE;
	havedisplay = TRUE;

	return (0);
}

/*
 *  DOCUMENTATION
 *
 *  Name: split-window
 *  Call: (split-window [ 'geometry ])
 *  Retu: fixnum
 *  Desc: This function creates another editor window under the X window
 *	system which is a duplicate of the current one.  An actual
 *	new X window appears on the screen slightly lower and to the
 *	left of the current one.  If the first argument is given, it
 *	must evaluate to a string containing the window geometry
 *	according to the X paradigm (see \sym{x-set-geometry}).
 *
 *	A window displays a single buffer (and there are other, special
 *	types of windows).  This function is only used to create
 *	buffer windows and so can only be invoked from buffer windows.
 *
 *	This function returns the number unique to this window, which
 *	can be used in calls which affect a specific window.  It is
 *	the ``window handle.''
 *  Side: Another ``window'' under X is made for each editor window,
 *	Each window can be manipulated independently using the window
 *	manager, and other such X utilities.
 *
 *	If the geometry is specified fully (I.e., with both X and
 *	Y positions), the window is just created, otherwise an outline
 *	is ``rubber-banded'' on the display allowing the user to place
 *	the window.  The same procedure is followed as when the initial
 *	window is created.  If no geometry is given explicitly, then
 *	the size (but not position) of the current window is passed.
 *  SeeA: current-window x-set-geometry
 */

DEFUN(dosplitwindow, "split-window", FUNC_VISUAL, "")
{
	extern char	*program;
	struct value	arg, ret;
	struct window	*new;
	struct string	*str;
	char		gbuf[64];

	CHECKAC(0, 1);
	*gbuf = '\0';
	if (GETACOUNT() > 0) {
		arg = EVALARGN(1);
		if (stringp(arg)) {
			str = gstring(arg.vl_data);
			makecstring(str, gbuf, sizeof (gbuf));
		} else if (!nullp(arg)) {
			/* illegal argument */
			BADARGN(1, "a string geometry specification");
		}
 	}

	if (current_window->wi_type != WIN_BUFFER)
		error("Can't split a non-buffer visiting window.");
	ASSERT(current_window->wi_buffer != NULL);

	/* make sure we have a geometry specification */
	if (*gbuf == '\0') {
		/* make geometry from current size */
		sprintf(gbuf, "=%dx%d",
			current_window->wi_cols,
			current_window->wi_rows);
	}

	/* create a new window */
	new = makewindow(gbuf, current_window->wi_fontname,
			 current_window->wi_buffer);

	ret.vl_type = LISP_FIXNUM;
	sfixnum(ret.vl_data, new->wi_index);
	return (ret);
}

/*
 *  DOCUMENTATION
 *
 *  Name: locator-position
 *  Call: (locator-position)
 *  Retu: dotted pair
 *  Desc: This function returns the position (in pixels) from the
 *	upper left corner of the current window at which the mouse
 *	was located when the latest input event occured.
 *
 *	If the latest input event didn't occur in the current window,
 *	nil is returned.  This means that the X, Y positions represented
 *	by the elements of the dotted pair always represent valid
 *	coordinates within the window.
 *
 *	Note that the coordinates are relative to the area of the window
 *	usable by the editors.  This exclues area taken by the internal
 *	border and the window's name stripe.
 *  Side: The source of the returned value may change every time an
 *	input event occurs, so one can't depend on it to remain constant
 *	for more than one keystroke.
 *  SeeA: current-window
 */
int	mouse_Xpos, mouse_Ypos, mouse_window;

DEFUN(dolocatorpos, "locator-position", FUNC_VISUAL, NULL)
{
	struct value	ret, one, two;
	struct window	*win = current_window;

	CHECKAC(0, 0);
	if (current_window->wi_index != mouse_window)
		return (v_nil);

	/* make up the return value */
	one.vl_type = LISP_FIXNUM;
	sfixnum(one.vl_data, mouse_Xpos);
	two.vl_type = LISP_FIXNUM;
	sfixnum(two.vl_data, mouse_Ypos);
	ret = cons(one, two);
	return (ret);
}

static
setposn(event, winp)
	XKeyOrButtonEvent	*event;
	struct window		*winp;
{
	Window	subwin;

	if (event == NULL || winp == NULL) {
		/* no position for this event */
		mouse_window = -1;
		mouse_Xpos = mouse_Ypos = -1;
	} else if ((winp->wi_flags & WIN_MINIBUF) == 0) {
		XInterpretLocator(winp->wi_xwindow,
				  &mouse_Xpos, &mouse_Ypos,
				  &subwin, event->location);
		mouse_Xpos -= winp->wi_left;
		mouse_Ypos -= winp->wi_top;
		if (mouse_Xpos < 0 || mouse_Ypos < 0 ||
		    mouse_Xpos >= winp->wi_width ||
		    mouse_Ypos >= winp->wi_height) {
			/* this is outside the window */
			mouse_window = -1;
			mouse_Xpos = mouse_Ypos = -1;
		} else {
			/* this is a valid mouse coordinate */
			mouse_window = winp->wi_index;
		}
	}

	if (mouse_window >= 0) {
		debug(DXIO, "mouse at %d, %d in window %d (X window %x).",
		      mouse_Xpos, mouse_Ypos, mouse_window, winp->wi_xwindow);
	}
}

/*
 *  DOCUMENTATION
 *
 *  Name: accept-meta-as-prefix
 *  Call: (accept-meta-as-prefix 'prefix)
 *  Retu: length
 *  Desc: This function sets up the internal prefix sequence which
 *	is generated when a key is read with the meta (seventh) bit
 *	set.  The prefix given as the argument will precede the
 *	typed key code with the meta bit stripped off.  The length
 *	of the prefix is returned.
 *
 *	The argument should be a string or a fixnum, specifying the
 *	prefix, or nil, meaning meta characters are disallowed
 *	(the meta bit is simply stripped off).  If a prefix sequence
 *	is given, it should not contain any meta characters because
 *	only one level of meta processing is done on input.
 *
 *	By default, the prefix is set to just \em{escape}, so that
 *	the single key sequence \key{M-c} becomes the two key
 *	sequence \key{ESC c}.
 *  Side: The meta bit must be stripped from keyboard input character
 *	codes because character codes greater than 177 octal are
 *	reserved for mouse events.
 *
 *	Currently the prefix string is internally limited to 64
 *	key strokes in length.
 *  SeeA: escape-map
 */
static unsigned char	meta_prefix[64] = "\033";
static int		meta_length = 1;

DEFUN(dometaprefix, "accept-meta-as-prefix", FLAG_NONE, NULL)
{
	struct value	arg, ret;
	struct string	*str;
	unsigned char	*sp, *send;
	unsigned char	*pp, *pend;
	int		code;

	CHECKAC(1, 1);
	arg = EVALARGN(1);
	switch (arg.vl_type) {
	case LISP_FIXNUM:
		code = gfixnum(arg.vl_data) & 0377;
		meta_prefix[0] = code;
		meta_length = 1;
		break;
	case LISP_STRING:
		str = gstring(arg.vl_data);
		if (str->st_length > 0) {
			send = str->st_buffer + str->st_length;
			pend = meta_prefix + sizeof (meta_prefix);
			sp = str->st_buffer;
			pp = meta_prefix;
			while (sp < send && pp < pend)
				*pp++ = *sp++;
			meta_length = pp - meta_prefix;
		} else {
			/* empty string, no prefix */
			meta_length = 0;
		}	
		break;
	case LISP_NIL:
		meta_length = 0;
		break;
	default:
		BADARGN(1, "a string prefix");
	}
	ret.vl_type = LISP_FIXNUM;
	sfixnum(ret.vl_data, meta_length);
	return (ret);
}

/*
 *  This routine handles all input events pending.  It buffers
 *  keyboard and mouse input into an internal buffer.  The length
 *  of the queue (including the returned character) is returned.
 *  If there was at least one input character, that is returned
 *  in the integer pointed to by the second argument.  If the
 *  first argument is TRUE, we block until an input event occurs
 *  that we can return.
 *
 *  Since a ``single key stroke'' can be mapped to more than one
 *  input code by X, we maintain a queue of input codes and return
 *  the first one from the queue each time.  We also do meta
 *  character processing here (which is gross, but...).
 */
static unsigned char	input_queue[4096],
			*queue_start = input_queue,
			*queue_end = input_queue + NITEMS(input_queue);
static int		queue_length = 0;

#define QUEUE_MINFREE	64	/* always have this many free if possible */

input_event(block, stroke)
	unsigned int	*stroke;
{
	extern int	SWITCH_EVENTS, ICPASS_EVENTS;
	XEvent		event;
	XKeyEvent	*key;
	XButtonEvent	*but;
	XExposeEvent	*expose;
	struct window	*win;
	unsigned char	*str;
	int		len, code;
	Window		*subwin;
	unsigned char	*qp, *qend;
	unsigned long	rmask[FDM_SIZE];

	/* make sure we don't get to close to end of input queue */
	PROTECT();
	if (queue_length <= 0) {
		queue_start = input_queue;
		queue_length = 0;
	}
	if (queue_start > input_queue + QUEUE_MINFREE &&
	    input_queue + queue_length > queue_end + QUEUE_MINFREE) {
		unsigned char	*sp;

		qp = input_queue;
		qend = input_queue + queue_length;
		sp = queue_start;
		while (qp < qend && sp < queue_end)
			*qp++ = *sp++;
		queue_start = input_queue;
	}

next:	/* process all pending X events */
	while (XPending() > 0) {
		/* read the next event in the queue */
		if (XNextEvent(&event) == 0)
			panic("Our X connection went away!");

		/* find the window this event occurred in */
		for (win = window_list; win != NULL; win = win->wi_next) {
			if (win->wi_xwindow == event.window)
				break;
			if (win->wi_iconwin == event.window) {
				if (event.type & ICPASS_EVENTS) {
					/* pass this through to window */
					goto handle;
				} else {
					iconevent(win, &event);
					goto next;
				}
			}
		}
		if (win == NULL) {
			debug(DXIO, "Got event for unknown X window 0x%x!",
			      event.window);
			continue;
		}
		if ((win->wi_flags & WIN_ACTIVE) == 0) {
			/* we must have been de-iconified */
			win->wi_flags |= WIN_ACTIVE;
		}

handle:		/* switch this window to the current one if appropriate */
		if (win != current_window && (event.type & SWITCH_EVENTS) &&
		    (current_window->wi_flags & WIN_FOCUSED) == 0)
			switchwindow(win, FALSE, TRUE);

		switch ((int)event.type) {
		case KeyPressed:
			key = (XKeyEvent *)&event;

			/* get position into global variables */
			setposn(key, win);

			/* handle input characters */
			str = (unsigned char *)XLookupMapping(key, &len);
			qp = queue_start + queue_length;
			while (len > 0 && qp < queue_end) {
				if (*str > CHAR_MAXCODE) {
					unsigned char	*pp, *pend;

					debug(DXIO, "Inserting meta prefix.");
					pp = meta_prefix;
					pend = meta_prefix + meta_length;
					while (pp < pend && qp < queue_end) {
						*qp++ = *pp++;
						queue_length++;
					}
				}
				/* (now) it's just a normal character */
				if (qp < queue_end) {
					code = *str & CHAR_MAXCODE;
					*qp++ = code;
					queue_length++;
					debug(DXIO,
			    "Keyboard input became key stroke '%k' (code %o).",
					      code, code);
					len--;
				}
			}
			if (len > 0) {
				debug(DXIO,
			    "Threw away %d pending keyboard characters.",
				      len);
			}
			break;
		case ButtonPressed:
			but = (XButtonEvent *)&event;

			/* get position into global variables */
			setposn(but, win);

			/* map button into an input code */
			switch (but->detail & 0377) {
			case RightButton:
				code = MOUSE_RIGHT;
				break;
			case MiddleButton:
				code = MOUSE_MIDDLE;
				break;
			case LeftButton:
				code = MOUSE_LEFT;
				break;
			default:
				debug(DXIO, "Got an illegal mouse button!");
				continue;
			}
			if (but->detail & ControlMask)
				code |= MOUSE_CTRL;
			if (but->detail & MetaMask)
				code |= MOUSE_META;
			if (but->detail & (ShiftMask|ShiftLockMask))
				code |= MOUSE_SHIFT;
			/* add the mouse button to input queue */
			qp = queue_start + queue_length;
			if (qp < queue_end) {
				*qp++ = code;
				queue_length++;
				debug(DXIO,
			    "Button event became key stroke '%k' (code %o).",
				      code, code);
			} else {
				/* don't know what to do in this case */
				debug(DXIO, "Threw away a pending mouse key.");
			}
			break;
		case ExposeWindow:
			debug(DXIO, "Got an expose window event.");
			expose = (XExposeEvent *)&event;
			if (expose->width != win->wi_width ||
			    expose->height != win->wi_height)
				resizewin(win, expose->width, expose->height);
			exposewin(win, 0, 0, expose->width, expose->height);
			break;
		case ExposeRegion:
			debug(DXIO, "Got an expose region event.");
			expose = (XExposeEvent *)&event;
			exposewin(win, expose->x, expose->y,
				  expose->width, expose->height);
			break;
		case EnterWindow:
			debug(DXIO, "Got an enter window event.");
			/* made this the current window already */
			break;
		case UnmapWindow:
			debug(DXIO, "Got an unmap window event.");
			if (win->wi_flags & WIN_FOCUSED) {
				XFocusKeyboard(RootWindow);
				win->wi_flags &= ~WIN_FOCUSED;
			}
			win->wi_flags &= ~WIN_ACTIVE;
			break;
		default:
			ierror("Input event type %d not handled!", event.type);
			break;
		}
	}
	UNPROTECT();

	if (block && queue_length <= 0) {
		/* wait for the next X event */
		bzero(rmask, sizeof (rmask));
		FDM_SET(rmask, dpyno(Xdisplay));
		select(NOFILE, rmask, NULL, NULL, NULL);
		goto next;
	}

	if (stroke != NULL && queue_length > 0) {
		/* return a new character to input */
		*stroke = *queue_start++;
		return (queue_length--);
	} else {
		/* return no character, just the queue length */
		return (queue_length);
	}
}

flush_input()
{
	int	gone;

	gone = queue_length;

	queue_length = 0;
	queue_start = input_queue;

	return (gone);
}

scanabort()
{
	extern int	abort_char;
	unsigned char	*ip;

	for (ip = queue_start; ip < queue_end; ip++)
		if (*ip == abort_char) {
			debug(DINPUT,
			      "scanabort: Saw a %C in pending input queue.",
			      (int)*ip);
			/* flush queue through this character */
			queue_start = ip + 1;
			/* call internal abort function */
			int_abort();
		}
	/* return (called via SIGALRM) */
}

/*
 *  DOCUMENTATION
 *
 *  Name: auto-raise-windows
 *  Desc: If this variable is set true, a window will always
 *	raise to the top of the window heirarchy when the mouse
 *	moves into it (as it becomes the current window).
 *  SeeA: x-raise-window current-window
 */
MKSTRING(AUTORAISE_NAME, "auto-raise-windows");

/*
 *  DOCUMENTATION
 *
 *  Name: auto-warp-mouse
 *  Desc: If this variable is set true, when a window is switched
 *	to (for any reason), the mouse will move into it's center
 *	automatically.
 *  SeeA: auto-raise-windows current-window
 */
MKSTRING(AUTOWARP_NAME, "auto-warp-mouse");

switchwindow(win, canwarp, canraise)
	struct window	*win;
{
	Window	subw;
	int	x, y;

	ASSERT(win != NULL);

	/* this is actually an important optimization ... */
	if (current_window == win)
		return (0);

	PROTECT();
	/* activate the window if it isn't */
	if ((win->wi_flags & WIN_ACTIVE) == 0)
		reactivatewin(win);

	/* change borders and warp the mouse, etc. */
	if (current_window != NULL && current_window->wi_bdwidth > 0)
		XChangeBorder(current_window->wi_xwindow, GrayPixmap);
	if (win->wi_bdwidth > 0)
		XChangeBorder(win->wi_xwindow, win->wi_bdpixmap);

	/* this is now our current window */
	current_window = win;
	if (win->wi_type == WIN_BUFFER && win->wi_buffer != NULL) {
		/* make the current buffer the one for this window */
		current_buffer = win->wi_buffer;
	}

	/* move this window to the head of the list */
	if ((win->wi_flags & WIN_MINIBUF) == 0) {
		struct window	*last, *next;

		last = NULL;
		for (next = window_list; next != NULL; next = next->wi_next) {
			if (next == win)
				break;
			last = next;
		}
		ASSERT(next == win);
		next = next->wi_next;

		if (last != NULL) {
			/* it's not already at the head of the list */
			last->wi_next = next;
			win->wi_next = window_list;
			window_list = win;
		}
	}

	/* auto raise the window if appropriate */
	if (canraise && truevar(AUTORAISE_NAME)) {
		if (win->wi_flags & WIN_DORAISE) {
			XRaiseWindow(win->wi_xwindow);
			win->wi_flags &= ~WIN_DORAISE;
		} if (win->wi_flags & WIN_DORAISE) {
			/* raise this window next time around */
			win->wi_flags |= WIN_DORAISE;
		}
	}

	if (canwarp && truevar(AUTOWARP_NAME)) {
		/* check that the mouse isn't already there */
		XQueryMouse(win->wi_xwindow, &x, &y, &subw);
		if (x < 0 || x >= win->wi_xsize ||
		    y < 0 || y >= win->wi_ysize) {
			/* warp mouse into newly selected window */
			warptowindow(win);
		}
	}
	UNPROTECT();

	return (0);
}

warptowindow(winp)
	struct window	*winp;
{
	int	x, y;
	Window	subwin;

	/* warp into the middle of the window */
	XWarpMouse(winp->wi_xwindow,
		   winp->wi_xsize / 2,
		   winp->wi_ysize / 2);

	/* check to see if the mouse is in the window */
	XQueryMouse(RootWindow, &x, &y, &subwin);
	if (subwin != winp->wi_xwindow)
		XRaiseWindow(winp->wi_xwindow);
}

focuswindow(winp)
	struct window	*winp;
{
	struct window	*w;

	/* unset the focus from all X windows */
	XFocusKeyboard(RootWindow);
	for (w = window_list; w != NULL; w = w->wi_next) {
		if ((w->wi_flags & WIN_FOCUSED) != 0)
			w->wi_flags &= ~WIN_FOCUSED;
	}

	/* if we're not to fucus a window, we're done */
	if (winp == NULL)
		return (1);

	/* focus the specified window */
	if (XFocusKeyboard(winp->wi_xwindow) == 0) {
		/* an X error */
		return (-1);
	} else {
		winp->wi_flags |= WIN_FOCUSED;
		return (0);
	}
}

/*
 *  DOCUMENTATION
 *
 *  Name: auto-activate-windows
 *  Desc: This variable controls the action taken when a deactivated
 *	window is accessed.  If this variable is set true, the
 *	window is automatically activated, otherwise it is usually
 *	an error to access a deactivated window.
 *  SeeA: deactivate-window reactivate-window auto-raise-windows
 */
MKSTRING(AUTOACTIVE_NAME, "auto-activate-windows");

struct window *
getwindow(index)
{
	struct window	*win;
	struct value	val;

	for (win = window_list; win != NULL; win = win->wi_next)
		if (win->wi_index == index)
			break;
	if (win == NULL)
		error("No such window number %d.", index);
	if ((win->wi_flags & WIN_ACTIVE) == 0) {
		val = get_variable(AUTOACTIVE_NAME, win->wi_buffer);
		if (truep(val))
			reactivatewin(win);
		else
			error("Window number %d is not active.", index);
	}

	return (win);
}

/* ARGSUSED */
static int
Xfatal(display)
	Display	*display;
{
	panic("An X I/O error occured; lost X display \"%s\"!", Xdispname);
}

/* ARGSUSED */
static int
Xerror(display, event)
	Display		*display;
	XErrorEvent	*event;
{
	extern struct window	*minibuf_window;
	char			*wname, nbuf[64];
	struct window		*win;

	/* find the window in which it occurred */
	for (win = window_list; win != NULL; win = win->wi_next)
		if (win->wi_xwindow == event->window)
			break;
	if (win == NULL) {
		if (event->window == RootWindow) {
			/* this shouldn't happen, but ... */
			wname = "the RootWindow";
		} else {
			sprintf(nbuf, "X window 0x%x",
				(unsigned int)event->window);
			wname = nbuf;
		}
	} else if (win == minibuf_window) {
		wname = "the minibuffer window";
		minibuf_window->wi_xwindow = NULL;
		killwindow(minibuf_window, TRUE);
		minibuf_window = NULL;
	} else {
		sprintf(nbuf, "%sactive window %d",
			(win->wi_flags & WIN_ACTIVE) ? "" : "in",
			win->wi_index);
		wname = nbuf;
	}

	ierror("X error \"%s\", request %d, on %s.",
	       XErrDescrip(event->error_code),
	       event->request_code,
	       wname);
	/* NOTREACHED */
}
