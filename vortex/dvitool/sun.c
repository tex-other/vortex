/* 
 * Copyright (c) 1986-1991 The Regents of the University of California.
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
 */

static char copyright_notice[] = "Copyright (c) 1986 - 1991 Regents of the University of California.\nAll rights reserved.";

#ifndef lint
	static char rcsid[] = "$Source: /home/yew/yew4/vortex/newdist/dvitool/RCS/sun.c,v $  (Berkeley)";
#endif not lint

/*
 * Author: Jeff McCarrell at U.C. Berkeley <jwm@Berkeley.EDU>
 * $Date: 1993/09/16 02:29:01 $
 * $State: Exp $
 */
/*
 * routines for dealing with Sun specific actions.
 */
#include "structs.h"
#include "constants.h"
#include "sun.h"
#include "fdecls.h"
#include "rectangs.h"
#include <sunwindow/cms.h>
#include <sunwindow/win_ioctl.h>
#include <suntool/wmgr.h>

extern tl_data	*tl;
sun_d		sun_data,
		*sunp = &sun_data;
static pixr	*pg_image = 0,
		*msg_image = 0,
		*msg_cursors[MCURS];
int		msg_win_height;

/*
 * this routine is called whenever the user's window changes size.  Here
 * we just squirrel things away. We update in the repaint proc later.
 */
image_resize(image, w, h)
	Canvas		image;
{
	int		horiz_sb_h,
			vert_sb_w;
	extern int	scrollbars;
	extern Rect	last_written;

	/*
	 * note that the Sun canvas code has already subtracted the width
	 * of the scrollbars from the w & h passed in here, so we must
	 * add it back in if necessary.
	 */
	view_r.r_width = win_r.r_width = w;
	view_r.r_height = win_r.r_height = h;

	if (scrollbars) {
		horiz_sb_h = (int) scrollbar_get(sunp->horiz, SCROLL_THICKNESS);
		vert_sb_w = (int) scrollbar_get(sunp->vert, SCROLL_THICKNESS);
		win_r.r_width += vert_sb_w;
		win_r.r_height += horiz_sb_h;
		if ((Scrollbar_setting) scrollbar_get(sunp->horiz,
		  SCROLL_PLACEMENT) == SCROLL_NORTH) {
			win_r.r_top = horiz_sb_h;
		} else {
			win_r.r_top = 0;
		}
		if ((Scrollbar_setting) scrollbar_get(sunp->vert,
		  SCROLL_PLACEMENT) == SCROLL_WEST) {
			win_r.r_left = vert_sb_w;
		} else {
			win_r.r_left = 0;
		}
		last_written.r_left = win_r.r_left;
		last_written.r_top = win_r.r_top;
	}

	/*
	 * adjust left and top so we won't try to paint off the edge of
	 * the document when image_repaint is called.
	 */
	if (dvi->file != (FILE *) 0) {
		if (view_r.r_left + view_r.r_width > pg_image->pr_size.x) {
			view_r.r_left = pg_image->pr_size.x - view_r.r_width;
			if (view_r.r_left < 0) {
				view_r.r_left = 0;
			}
		}
		if (view_r.r_top + view_r.r_height > pg_image->pr_size.x) {
			view_r.r_top = pg_image->pr_size.x - view_r.r_height;
			if (view_r.r_top < 0) {
				view_r.r_top = 0;
			}
		}
	}
	return(0);
}

image_repaint(image, rp_pw, repaint)
	Canvas		image;
	Pixwin		*rp_pw;
	Rectlist	*repaint;
{
	Rect		*r;
	Rectnode	*rn;
	Scrollbar	horiz,
			vert;
	int		width,
			height,
			src_x,
			src_y;
	extern Rect	last_written;

	r = &(repaint->rl_bound);

	pw_lock(rp_pw, r);
	for (rn = repaint->rl_head; rn != (Rectnode *) 0; rn = rn->rn_next) {
		/* transform from their coordinates to mine. */
		r = &(rn->rn_rect);
		if (pg_image != (pixr *) 0) {
			width = (r->r_width < pg_image->pr_size.x) ?
			  r->r_width : pg_image->pr_size.x;
			height = (r->r_height < pg_image->pr_size.y) ?
			  r->r_height : pg_image->pr_size.y;
		} else {
			width = r->r_width;
			height = r->r_height;
		}
		src_x = r->r_left + view_r.r_left;
		src_y = r->r_top + view_r.r_top;

		pw_write(rp_pw,
		  r->r_left, r->r_top,
		  width, height,
		  PIX_SRC,
		  pg_image,
		  src_x, src_y);

		if (width > last_written.r_width) {
			last_written.r_width = width;
		}
		if (height > last_written.r_height) {
			last_written.r_height = height;
		}
	}

	horiz = (Scrollbar) window_get(sunp->image,
		WIN_HORIZONTAL_SCROLLBAR);
	scrollbar_set(horiz,
		SCROLL_VIEW_START,	view_r.r_left,
		SCROLL_VIEW_LENGTH,	view_r.r_width,
		SCROLL_OBJECT_LENGTH,	pg_image_r.r_width,
		0);
	vert = (Scrollbar) window_get(sunp->image,
		WIN_VERTICAL_SCROLLBAR);
	scrollbar_set(vert,
		SCROLL_VIEW_START,	view_r.r_top,
		SCROLL_VIEW_LENGTH,	view_r.r_height,
		SCROLL_OBJECT_LENGTH,	pg_image_r.r_height,
		0);

	scrollbar_paint_clear(horiz);
	scrollbar_paint_clear(vert);
	pw_unlock(rp_pw);
	/*
	 * we set the created flag here because the first repaint request
	 * we see is the first point at which we are sure the window is
	 * really visible to the user.
	 */
	tl->created = 1;
	return(0);
}

/*
 * this routine is called whenever the message subwindow changes size.
 * We get a new pixrect to write into and squirrel away the width of the
 * window for the message writing routines.
 */
msg_resize(message, w, h)
	Canvas		message;
{
	extern int	msg_win_width;

	msg_win_width = w;
	if (msg_image != (pixr *) 0) {
		pr_destroy(msg_image);
	}
	if ((msg_image = mem_create(w, h, 1)) == (pixr *) 0) {
		msg(FATAL, "couldn't get memory for message window!");
	}
}

msg_repaint(msg, rp_pw, repaint)
	Canvas		msg;
	Pixwin		*rp_pw;
	Rectlist	*repaint;
{
	extern char 	msg_text[];
	extern int	msg_visible;

	/* we just repaint from the string we have in memory. */
	if (msg_visible) {
		show_msg(msg_text, 0, 0);
	}
}

make_window(argc, argv)
	int	*argc;
	char	**argv;
{
	Frame			frame;
	Canvas			image,
				message;
	char			*win_emsg = 0,
				*getenv();
	extern struct icon	dvi_icon;
	extern struct cursor	main_cursor;
	extern int		i_win_width,
				i_win_height,
				i_win_x,
				i_win_y,
				i_iconic,
				scrollbars;
	int			input_handler(),
				image_resize(),
				image_repaint(),
				msg_resize(),
				msg_repaint();
	Notify_value		catch_scrolls(),
				interrupt_handler();

	if (getenv("WINDOW_PARENT") == (char *) 0) {
		msg(FATAL, "must be run under suntools");
	}

	pre_make_window();

	/*
	 * It appears that SunView 3.0 has a (sic) bug: the FRAME_CLOSED
	 * enum doesn't look at its arg; it always make a closed tool
	 * regardless of whether it is passed a 0 or a 1.  So we kludge
	 * around it...
	 */
	if (i_iconic) {
		frame = window_create(NULL, FRAME,
			FRAME_ICON,			&dvi_icon,
			FRAME_LABEL,			vers_str(),
			FRAME_ARGC_PTR_ARGV,		argc, argv,
			FRAME_SUBWINDOWS_ADJUSTABLE,	0,
			FRAME_NO_CONFIRM,		1,
			FRAME_CLOSED,			i_iconic,
			WIN_X,				i_win_x,
			WIN_Y,				i_win_y,
			WIN_WIDTH,			i_win_width,
			WIN_HEIGHT,			i_win_height,
			WIN_ERROR_MSG,			win_emsg,
			0);
	} else {
		frame = window_create(NULL, FRAME,
			FRAME_ICON,			&dvi_icon,
			FRAME_LABEL,			vers_str(),
			FRAME_ARGC_PTR_ARGV,		argc, argv,
			FRAME_SUBWINDOWS_ADJUSTABLE,	0,
			FRAME_NO_CONFIRM,		1,
			WIN_X,				i_win_x,
			WIN_Y,				i_win_y,
			WIN_WIDTH,			i_win_width,
			WIN_HEIGHT,			i_win_height,
			WIN_ERROR_MSG,			win_emsg,
			0);
	}
	message = window_create(frame, CANVAS,
		CANVAS_AUTO_SHRINK,		1,
		CANVAS_AUTO_EXPAND,		1,
		CANVAS_FIXED_IMAGE,		0,
		CANVAS_RETAINED,		0,
		CANVAS_REPAINT_PROC,		msg_repaint,
		CANVAS_RESIZE_PROC,		msg_resize,
		WIN_HEIGHT,			msg_win_height,
		WIN_CURSOR,			&main_cursor,
		WIN_ERROR_MSG,			win_emsg,
		0);
	
	image = window_create(frame, CANVAS,
		CANVAS_AUTO_SHRINK,		1,
		CANVAS_AUTO_EXPAND,		1,
		/* so we get repaint events that cover the whole window */
		CANVAS_FIXED_IMAGE,		0,
		CANVAS_RETAINED,		0,
		CANVAS_REPAINT_PROC,		image_repaint,
		CANVAS_RESIZE_PROC,		image_resize,
		WIN_EVENT_PROC,			input_handler,
		WIN_CONSUME_KBD_EVENT,		WIN_ASCII_EVENTS,
		WIN_CURSOR,			&main_cursor,
		WIN_ERROR_MSG,			win_emsg,
		0);
	/*
	 * set up the handler that removes scroll events from the input
	 * stream so the Sun canvas window package doesn't screw things
	 * up.
	 */
	notify_interpose_event_func(image, catch_scrolls, NOTIFY_SAFE);
	/*
	 * set up the handler to catch stop events and set the interrupted
	 * flag.
	 */
	(void) notify_set_signal_func(image, interrupt_handler, 
	  SIGURG, NOTIFY_ASYNC);

	sunp->frame = frame;
	sunp->msg = message;
	sunp->image = image;
	sunp->tool_fd = (int) window_get(frame, WIN_FD, 0);
	sunp->msg_fd = (int) window_get(message, WIN_FD, 0);
	sunp->image_fd = (int) window_get(image, WIN_FD, 0);

	if (scrollbars) {
		make_scrollbars();
	}

	post_make_window();
	return(0);
}

/*
 * check for WIN_STOP events (queued up by the notifier when the user
 * hits the STOP key (which is key L1/setup) and set the interrupted flag.
 */
Notify_value
interrupt_handler(tag, signal, when)
	int			*tag,	/* a handle used by the notifier. */
				signal;	/* the signal received. */
	Notify_signal_mode	when;	/* synch or async notification. */
{
	extern int	interrupted;

	if (signal == SIGURG) {
		interrupted = 1;
		return(NOTIFY_DONE);
	}
	return(NOTIFY_IGNORED);
}

/*
 * Catch scroll events bound for the image's event handler before any
 * processing is done on them so I can do the right processing.  Pass all
 * other events on through.
 */

static Notify_value
catch_scrolls(image, ev, arg, type)
	Canvas			image;
	Event			*ev;
	Notify_arg		arg;
	Notify_event_type	type;
{
	Scroll_motion	motion;
	int		vert,
			scale,
			offset,
			bar_length;
	Rect		*sb_r;

	if (event_id(ev) == SCROLL_REQUEST) {
		sb_r = (Rect *) scrollbar_get(
		  (Scrollbar) arg, SCROLL_RECT);
		/*
		 * compute the horizontal and vertical specific info.
		 */
		if (arg == sunp->vert) {
			bar_length = sb_r->r_height;
			vert = 1;
		} else if (arg == sunp->horiz) {
			bar_length = sb_r->r_width;
			vert = 0;
		} else {
			msg(PLAIN, "unknown scrollbar in SCROLL_REQUEST event!");
			goto done;
		}

		bar_length -= 2 * (int) scrollbar_get(
		  (Scrollbar) arg, SCROLL_PAGE_BUTTON_LENGTH);

		motion = (Scroll_motion) scrollbar_get(
		  (Scrollbar) arg, SCROLL_REQUEST_MOTION);

		switch(motion) {
		case SCROLL_ABSOLUTE:
			offset = (int) scrollbar_get(
			  (Scrollbar) arg, SCROLL_REQUEST_OFFSET)
			  - (int) scrollbar_get((Scrollbar) arg,
			  SCROLL_PAGE_BUTTON_LENGTH);

			scale = (100 * offset) / (bar_length);
			if (scale > 95)
				scale = 100;
			if (scale < 5)
				scale = 0;
			if (vert) {
				scroll_ver_abs(scale);
			} else {
				scroll_hor_abs(scale);
			}
			break;
		case SCROLL_POINT_TO_MIN:
		case SCROLL_MAX_TO_POINT:
		case SCROLL_LINE_FORWARD:
			if (vert) {
				scroll_down();
			} else {
				scroll_right();
			}
			break;
		case SCROLL_POINT_TO_MAX:
		case SCROLL_MIN_TO_POINT:
		case SCROLL_LINE_BACKWARD:
			if (vert) {
				scroll_up();
			} else {
				scroll_left();
			}
			break;
		case SCROLL_PAGE_FORWARD:
			next_page();
			break;
		case SCROLL_PAGE_BACKWARD:
			prev_page();
			break;
		default:
			msg(PLAIN, "unknown motion in SCROLL_REQUEST event");
			break;
		}
done:
		return(NOTIFY_DONE);
	}
	return(notify_next_event_func(image, ev, arg, type));
}

make_scrollbars()
{
	Scrollbar	h,
			v;

	h = scrollbar_create(
		SCROLL_NORMALIZE,	0,
 		SCROLL_ADVANCED_MODE,	1,
		0);
	v = scrollbar_create(
		SCROLL_LINE_HEIGHT,	1,
 		SCROLL_ADVANCED_MODE,	1,
		SCROLL_NORMALIZE,	0,
		0);

	sunp->horiz = h;
	sunp->vert = v;

	window_set(sunp->image,
		WIN_HORIZONTAL_SCROLLBAR, h,
		WIN_VERTICAL_SCROLLBAR,	  v,
		0);
}


pre_make_window() 
{
	struct pixrect		*pr;
	char			*file,
				*emsg,
				fname_buf[1024];
	int			k,
				j;
	extern char		i_icon_file[],
				i_cursor_file[];
	extern struct cursor	main_cursor;
	extern int		i_cursor_xhot,
				i_cursor_yhot,
				msg_draw_y,
				msg_draw_w,
				msg_draw_h;

	/*
	 * set up the various bookkeeping values the message window
	 * needs.  Note that the width of the message window will be
	 * recorded when our resize proc gets called later.
	 */
	if ((sunp->sys_font = pw_pfsysopen()) == (struct pixfont *)0) {
		msg(FATAL, "couldn't open system font!");
	}
	msg_draw_h = sunp->sys_font->pf_defaultsize.y;
	msg_draw_w = sunp->sys_font->pf_defaultsize.x;
	msg_win_height = msg_draw_h + (MSG_WIN_BORDER * 2);
	msg_draw_y = MSG_WIN_BORDER;

	/* set up the message window cursors... */
	for (k = INT_MCUR; k < MCURS; k++) {
		if ((msg_cursors[k] = mem_create(msg_draw_w, msg_draw_h, 1))
		   == (pixr *) 0) {
			msg(FATAL, "couldn't create space for charactes!");
		}
	}

	/* ...and invert them. */
	for (k = INT_MCUR; k < MCURS; k++) {
		switch(k) {
		case INT_MCUR:
			pr = sunp->sys_font->pf_char['i'].pc_pr;
			break;
		case STR_MCUR:
			pr = sunp->sys_font->pf_char['s'].pc_pr;
			break;
		case LSTR_MCUR:
			pr = sunp->sys_font->pf_char['S'].pc_pr;
			break;
		case FNAME_MCUR:
			pr = sunp->sys_font->pf_char['f'].pc_pr;
			break;
		case FUNC_MCUR:
			pr = sunp->sys_font->pf_char['c'].pc_pr;
			break;
		case FONT_MCUR:
			pr = sunp->sys_font->pf_char['F'].pc_pr;
			break;
		case VAR_MCUR:
			pr = sunp->sys_font->pf_char['v'].pc_pr;
			break;
		case REG_MCUR:
			pr = sunp->sys_font->pf_char[' '].pc_pr;
			break;
		case VHELP_MCUR:
			pr = sunp->sys_font->pf_char['V'].pc_pr;
			break;
		case FHELP_MCUR:
			pr = sunp->sys_font->pf_char['C'].pc_pr;
			break;
		case OHELP_MCUR:
			pr = sunp->sys_font->pf_char['O'].pc_pr;
			break;
		default:
			msg(FATAL, "bad msg cursor:init.");
		}

		pr_rop(msg_cursors[k],
		  0, 0,
		  pr->pr_size.x, pr->pr_size.y,
		  PIX_NOT(PIX_SRC),
		  pr,
		  0, 0);
	}

	/*
	 * create a dynamic icon and cursor if the user wants it.
	 */
	if (i_icon_file[0] != '\0') {
		/* get the pixrect from the static icon */
		pr = (struct pixrect *) icon_get(&dvi_icon, ICON_IMAGE);
		/* expand the user-supplied file name */
		if (expand(i_icon_file, fname_buf) < 0) {
			msg(LITERAL, fname_buf);
		} else {
			read_pr_from_file(fname_buf, 64, 64, pr);
			icon_set(&dvi_icon, ICON_IMAGE, pr, 0);
		}
	}
	if (i_cursor_file[0] != '\0') {
		pr = main_cursor.cur_shape;
		if (expand(i_cursor_file, fname_buf) < 0) {
			msg(LITERAL, fname_buf);
		} else {
			read_pr_from_file(fname_buf, 16, 16, pr);
		}
		/*
		 * we don't have to set anything here because pr is
		 * pointing right at the static space where the image is
		 * to be allocated.
		 */
	}
	main_cursor.cur_xhot = (short) i_cursor_xhot;
	main_cursor.cur_yhot = (short) i_cursor_yhot;
}

post_make_window()
{
	extern pixr	*no_font_char;
	extern int	i_icon_x,
			i_icon_y;
	Rect		*icon_rect;

	/* create the menu's */
	init_menus();

	/*
	 * set up the drawing ops.
	 */
	/* turn on pixels */
	sunp->set_op = PIX_NOT(PIX_SRC) | PIX_SRC;

	/* turn off pixels */
	sunp->clear_op = PIX_NOT(PIX_SRC) & PIX_SRC;

	/* write src pixels to dest pixels */
	sunp->write_op = PIX_SRC;

	/* invert dest pixels */
	sunp->invert_op = PIX_NOT(PIX_SRC);

	/*
	 * or src and dest pixels.  Used to draw characters on the load
	 * image so if there is overlap we see both of the character
	 * forms.
	 */
	sunp->or_op = PIX_SRC | PIX_DST;

	/*
	 * pass input generated in the message window to the image window.
	 * What we want is for input generated in the message window to
	 * be passed to the image window to be handled by that input
	 * handler.
	 */
#if 0
	the obvious thing is simply to do:
	window_set(sunp->msg,
		WIN_INPUT_DESIGNEE, sunp->image,
		0);
	but that doesn't work;  this ioctl message appears:
	WIN ioctl number 806C6714: Invalid argument

	this doesn't work either:
	window_set(sunp->msg,
		WIN_INPUT_DESIGNEE, sunp->image_fd,
		0);
	where image_fd is the file descriptor for this window returned
	by:
	window_get(sunp->image, WIN_FD);
	all of the input is sent to the parent of the process rather
	than to the next window, i.e. characters are printed like
	they were going to stdout.

	nor does this work:
	/*
	 * skip the SunView layer and jump to the SunWindow layer
	 * directly.
	 */
	/* first get the input mask */
	win_getinputmask(sunp->msg_fd, &im, &designee);
	designee = sunp->image_fd;
	win_setinputmask(sunp->msg_fd, &im, (struct inputmask *) 0, designee);

	the bottom line is that input to the message subwindow is silently
	ignored.

#endif 0

	/* fill in the char used when we can't find the font */
	no_font_char = mem_create(3, 3, 1);
	pr_rop(no_font_char, 0, 0, 3, 3, sunp->set_op, 0, 0, 0);

	/*
	 * set the x and y of the icon.
	 */
	icon_rect = (Rect *) window_get(sunp->frame, FRAME_CLOSED_RECT);
	icon_rect->r_top = i_icon_y;
	icon_rect->r_left = i_icon_x;
	window_set(sunp->frame, FRAME_CLOSED_RECT, icon_rect, 0);

	/*
 	 * this bit of kludgery is because WIN_CURSOR always returns the same
	 * object, even when the cursor has been changed with window_set().
	 * so, we initialize the stack with a known value so it can be
	 * restored.
	 */
	push_cursor(&main_cursor);
}

main_loop()
{
	window_main_loop(sunp->frame);
}

destroy_window()
{
	window_done(sunp->frame);
}


/*
 * Paint the page's load image (a pixrect) onto the page image.  We
 * assume here that the page image is big enough.
 */

build_page_image(page)
	pg	*page;
{
	extern int	draw_borders,
			show_ld_image,
			border_width;
	int		kk = 3 * border_width;
	pixr		*spr = (pixr *) page->load_i;
	rectang		*pg_r = &(page->load_r);

	/* clear the page image */
	pr_rop(pg_image,
	  0, 0,
	  pg_image->pr_size.x, pg_image->pr_size.y,
	  sunp->clear_op,
	  NULL,
	  0, 0);

	/* write the load image onto the page image */
	pr_rop(pg_image,
	  phys_image_r.r_left + pg_r->r_left,
	  phys_image_r.r_top + pg_r->r_top,
	  pg_r->r_width,
	  pg_r->r_height,
	  sunp->write_op,
	  spr,
	  0, 0);

	if (draw_borders) {
		box_it(
		  phys_image_r.r_left, phys_image_r.r_top,
		  phys_image_r.r_width, phys_image_r.r_height,
		  border_width,
		  BOX_DRAW);
	}

	if (show_ld_image) {
		box_it(
		  phys_image_r.r_left + page->load_r.r_left,
		  phys_image_r.r_top + page->load_r.r_top,
		  page->load_r.r_width,
		  page->load_r.r_height,
		  1,
		  BOX_INVERT);
	}
	return(0);
}

/*
 * draw a box around the rect of width linewidth.
 */
box_it(x, y, w, h, linewidth, op)
{
	int	north = y - linewidth,
		west =  x - linewidth,
		east =  x + w,
		south = y + h,
		width = w + 2 * linewidth,
		height = h + 2 * linewidth;
	pixr	*src_pr;
	
	if (op == BOX_INVERT) {
		op = sunp->invert_op;
	} else {
		op = sunp->set_op;
	}

	src_pr = (pixr *) 0;
	/* the north line */
	pr_rop(pg_image,
	  west, north,
	  width, linewidth,
	  op,
	  src_pr,
	  west, north);

	/* the west line */
	pr_rop(pg_image,
	  west, north,
	  linewidth, height,
	  op,
	  src_pr,
	  west, north);
	  
	/* the east line */
	pr_rop(pg_image,
	  east, north,
	  linewidth, height,
	  op,
	  src_pr,
	  east, north);
	  
	/* the south line */
	pr_rop(pg_image,
	  west, south,
	  width, linewidth,
	  op,
	  src_pr,
	  west, south);
}

/*
 * this routine moves the load image to the right because we have
 * discovered characters that need to be set at a negative hh.  old_hh
 * describes the old minimum value; hh is the new minimum value.  We have
 * hh < min_hh <= 0, so the number of pixels to move to the right is
 * min_hh - hh.
 */
/*
 * an example may help clear this up:  assume we have set the character
 * 'i' at an hh of 0, and then we discover that we must set 'H' at an hh
 * of -6.  There are no bits in the load image pixrect at -6, so we must
 * shift the entire image over by 0 - (-6) pixels so we can paint the 'H'
 * on real estate that exists.
 */
/*
 * This routine has been generalized to adjust the load image in any of
 * four directions.
 */

pg_adjust(page, dir, old_val, val, flags)
	pg	*page;
	int	*flags;
{
	rectang	*rectp = &(page->load_r);
	int	step,
		old_w = rectp->r_width,
		old_h = rectp->r_height,
		new_w,
		new_h,
		new_x,
		new_y;
	pixr	*new_image,
		*old_image = (pixr *) page->load_i;

	/*
	 * we report an error if the new minimum is so far off the page
	 * that the underlying page image cannot hold it.
	 */
	new_x = new_y = 0;
	new_w = old_w;
	new_h = old_h;

	switch(dir) {
	case MV_LEFT:
		step = old_val - val;
		new_w = old_w + step;
		new_x = step;
		if (step > rectp->r_left) {
			if (!(*flags & (1 << MV_LEFT))) {
				msg(PLAIN, "can't set a char at pixel position %d.  enlarge left-margin.",
				  val);
				*flags |= (1 << MV_LEFT);
			}
			return(-1);
		}
		break;
	case MV_RIGHT:
		step = val - old_val;
		new_w = old_w + step;
		if (val > pg_image_r.r_width) {
			if (!(*flags & (1 << MV_RIGHT))) {
				msg(PLAIN, "page isn't wide enough; increase page-width");
				*flags |= (1 << MV_RIGHT);
			}
			return(-1);
		}
		break;
	case MV_UP:
		step = old_val - val;
		new_h = old_h + step;
		new_y = step;
		if (step > rectp->r_top) {
			if (!(*flags & (1 << MV_UP))) {
				msg(PLAIN, "page isn't tall enough; increase top-margin");
				*flags |= (1 << MV_UP);
			}
			return(-1);
		}
		break;
	case MV_DOWN:
		step = val - old_val;
		new_h = old_h + step;
		if (val > pg_image_r.r_height) {
			if (!(*flags & (1 << MV_DOWN))) {
				msg(PLAIN, "page isn't tall enough; increase page-height");
				*flags |= (1 << MV_DOWN);
			}
			return(-1);
		}
		break;
	}

	/*
	 * mark this page as ``don't throw me away'', since we need to
	 * copy its contents onto the new image.
	 */
	page->time_stamp = KEEP_PG_CACHED;
	if (alloc_load_image((char **) &new_image, new_w, new_h) < 0) {
		msg(PLAIN, "couldn't allocate the load image!");
		return(-1);
	}
	page->time_stamp = time(0);

	/* copy the old onto the new */
	pr_rop(new_image,
	  new_x, new_y,
	  old_w, old_h,
	  PIX_SRC,
	  old_image,
	  0, 0);

	free_one_load_image(page);
	page->load_i = (char *) new_image;

	/* adjust the load rect. */
	switch(dir) {
	case MV_LEFT:
		rectp->r_left -= step;
		/* fall through */
	case MV_RIGHT:
		rectp->r_width = new_w;
		break;
	case MV_UP:
		rectp->r_top -= step;
		/* fall through */
	case MV_DOWN:
		rectp->r_height = new_h;
		break;
	}
	return(0);
}


/*
 * Paint the rect which describes a piece of the page image onto the
 * user's window.  I allow this routine to paint the page image so that
 * there is whitespace visible on any side of the page.  i.e., the
 * restriction placed on the user that he may not scroll off of the edge
 * of the page is not enforced by this routine.
 */
/* we just keep the width and height in this rect. */
static Rect	last_written;

do_scroll(in_view, refresh)
	rectang	*in_view;
{
	int		width = view_r.r_width,
			height = view_r.r_height,
			dest_x = view_r.r_left,
			dest_y = view_r.r_top;
	Rect		*view,
			*r,
			view_s,
			lock,
			scratch;
	Rectlist	last_rl,
			view_rl,
			clear_rl;
	Rectnode	*rn;
	Pixwin		*dest = (Pixwin *) window_get(sunp->image, WIN_PIXWIN);
	extern int	view_has_changed;

	/*
	 * convert from the public rectang structure to the private-
	 * to-the-Sun implementation Rect structure.
	 */
	view_s.r_left = in_view->r_left; 
	view_s.r_top = in_view->r_top; 
	view_s.r_width = in_view->r_width; 
	view_s.r_height = in_view->r_height; 
	view = &view_s;

	/*
	 * check to see if the image we are drawing now is smaller than
	 * the one we last drew which would necessitate clearing some
	 * portion of the visible window.  We only have to do this if the
	 * window is the same size as it was last time we drew something
	 * because if it changed size, we've gotten resize and repaint
	 * events and handled it already.
	 */
	if (view->r_width < view_r.r_width || view->r_height < view_r.r_height) {
		if (view->r_width < last_written.r_width || 
		  view->r_height < last_written.r_height) {
			/*
			 * compute the area that we need to clear.
			 */
			scratch.r_left = win_r.r_left;
			scratch.r_top = win_r.r_top;
			scratch.r_width = view->r_width;
			scratch.r_height = view->r_height;
			rl_initwithrect(&scratch, &view_rl);
			rl_initwithrect(&last_written, &last_rl);
			rl_initwithrect(&rect_null, &clear_rl);
		  	
			rl_difference(&last_rl, &view_rl, &clear_rl);
			for (rn = clear_rl.rl_head; rn != (Rectnode *) 0; rn = rn->rn_next) {
				r = &(rn->rn_rect);
				pw_write(dest,
				  r->r_left, r->r_top,
				  r->r_width, r->r_height,
				  sunp->clear_op,
				  NULL,
				  0, 0);
			}
		  }
	} 

	lock = *view;

	lock.r_left = win_r.r_left;
	lock.r_top = win_r.r_top;

	pw_lock(dest, &lock);

	pw_write(dest,
	  win_r.r_left, win_r.r_top,
	  view->r_width, view->r_height,
	  PIX_SRC,
	  pg_image,
	  view->r_left, view->r_top);

	pw_unlock(dest);

	view_has_changed = 0;
	view_r.r_left = view->r_left;
	view_r.r_top = view->r_top;
	last_written.r_width = view->r_width;
	last_written.r_height = view->r_height;

	scrollbar_set(
	  (Scrollbar) window_get(sunp->image, WIN_HORIZONTAL_SCROLLBAR),
	  SCROLL_VIEW_START,	view->r_left,
	  SCROLL_OBJECT_LENGTH,	pg_image_r.r_width,
	  0);
	scrollbar_set(
	  (Scrollbar) window_get(sunp->image, WIN_VERTICAL_SCROLLBAR),
	  SCROLL_VIEW_START,	view->r_top,
	  SCROLL_OBJECT_LENGTH,	pg_image_r.r_height,
	  0);
	scrollbar_paint(sunp->horiz);
	scrollbar_paint(sunp->vert);
	return(0);
}

/*
 * some other routine has changed the view the user has of the current
 * page; this routine sets a flag so the next time do_scroll is called,
 * the window will be re-written from scratch.
 */
static int	view_has_changed = 0;

view_changed()
{
	view_has_changed = 1;
}

/*
 * given a bounding rect and a page, paint the area on the load image onto
 * the page image and ensure that it is visible.
 */
show_pg_rect(page, b, strict, only_backing)
	pg	*page;
	Rect	*b;
{
	int	dest_x,
		dest_y;
	pixr	*src_pr;

	/*
	 * all we do here is map the area on the load image to the page
	 * image and call show_rect.
	 */
	src_pr = (pixr *) page->load_i;
	dest_x = b->r_left + phys_image_r.r_left + page->load_r.r_left;
	dest_y = b->r_top + phys_image_r.r_top + page->load_r.r_top;

	pr_rop(pg_image,
	  dest_x, dest_y,
	  b->r_width, b->r_height,
	  sunp->write_op,
	  src_pr,
	  b->r_left, b->r_top);

	if (only_backing) {
		return(0);
	}
	b->r_left = dest_x;
	b->r_top = dest_y;
	return(show_rect(b, strict));
}


/*
 * given a bounding rect, ensure that the area it represents is visible
 * on the screen unless strict is false.
 */
show_rect(b, strict)
	Rect	*b;
{
	Rect	visible,
		view;
	int	new_x,
		new_y;
		
	Pixwin	*dest = (Pixwin *) window_get(sunp->image, WIN_PIXWIN);


	rect_construct(&visible,
	  view_r.r_left, view_r.r_top,
	  view_r.r_width, view_r.r_height);

	if (rect_includesrect(&visible, b)) {
		if (view_has_changed) {
			do_abs_scroll(view_r.r_left, view_r.r_top, 1);
		} else {
			pw_write(dest,
			  win_r.r_left + b->r_left - view_r.r_left,
			  win_r.r_top + b->r_top - view_r.r_top,
			  b->r_width, b->r_height,
			  PIX_SRC,
			  pg_image,
			  b->r_left, b->r_top);
		}
		return(0);
	}

	if (!strict) {
		return(0);
	}
	
	/* show vertically in the middle of the screen */
	new_y = b->r_top - ((view_r.r_height + b->r_height) / 2);
	/*
	 * scroll horizontally only if the left edge of the selection is
	 * off of the visible page.
	 */
	if (b->r_left > view_r.r_width) {
		new_x = b->r_left - ((view_r.r_width + b->r_width) / 2);
	}
	return(do_abs_scroll(new_x, new_y, 1));
}


/*
 * highlight a character on the load image.  It will later be mapped to
 * the page image (and to the screen) in one operation.
 */

highlight(page, c, op, bound_rect)
	pg	*page;
	s_char	*c;
	Rect	*bound_rect;
{
	int		dest_w,
			dest_h,
			dest_x,
			dest_y,
			src_x,
			src_y,
			draw_mode;
	pixr		*src_pr,
			*dest_pr;
	int		border;
	Rect		c_r;

	/* make sure that this is a displayable character. */
	if (c->c_type != S_SEARCHABLE) {
		return(0);
	}
	src_x = (int) c->flags & S_INVERTED;

	if (op == SHOW_SEL) {
		if (src_x) {
			return(0);
		} else {
			c->flags |= S_INVERTED;
		}
	}

	if (op == UNINVERT_SEL) {
		if (src_x) {
			c->flags &= ~(S_INVERTED);
		} else {
			return(0);
		}
	}

	border = (c->f_info->width < 5) ? 1 : 2;
	dest_x = c->x - border;
	dest_y = c->y - border;
	dest_w = c->f_info->width + 2 * border;
	dest_h = c->f_info->height + 2 * border;
	src_x  = dest_x + phys_image_r.r_left + page->load_r.r_left;
	src_y  = dest_y + phys_image_r.r_top + page->load_r.r_top;

	draw_mode = sunp->invert_op;
	src_pr = pg_image;
	dest_pr = (pixr *) page->load_i;

	/* update the bounding rect. */
	rect_construct(&c_r, dest_x, dest_y, dest_w, dest_h);
	*bound_rect = rect_bounding(&c_r, bound_rect);

	pr_rop(dest_pr,
	  dest_x, dest_y,
	  dest_w, dest_h,
	  draw_mode,
	  src_pr,
	  src_x, src_y);

	return(0);
}

/*
 * uninvert a selection on the ld_image when the pg_image does not
 * contain this ld_image.  This routine must be different from the general
 * case (where the pg_image contains the ld_image) due to the behavior of
 * our uninversion algorithm.  For each character, we simply invert the
 * character, plus the 1 or 2 pixel border around it.  This works fine
 * except when the characters overlap: the first character uninverts the
 * intersecting area and the second character re-inverts it.  The
 * solution to this problem is to create a copy of the region to be
 * uninverted and use that as the source for the inversion.  Note that
 * even if there is overlap, the resulting area will be correctly
 * inverted because the source remains uninverted for all of the
 * characters.  That is also the method used in the usual case, when the
 * load image is the source and the page image is the destination.
 */
uninv_sel(page)
	pg	*page;
{
	s_char	*c;
	pixr	*src,
		*dest;
	int	border,
		dest_w,
		dest_h,
		dest_x,
		dest_y,
		src_x,
		src_y,
		copy_x,
		copy_y;

	dest_w = page->sel_r.r_width;
	dest_h = page->sel_r.r_height;
	copy_x = page->sel_r.r_left;
	copy_y = page->sel_r.r_top;
	dest = (pixr *) page->load_i;

	if ((src = mem_create(dest_w, dest_h, 1)) == (pixr *) 0) {
		msg(FATAL, "couldn't create pixrect for erasing selection!");
	}
	
	/* copy the entire region to our intermediate pixrect. */
	pr_rop(src,
	  0, 0,
	  dest_w, dest_h,
	  sunp->write_op,
	  dest,
	  copy_x, copy_y);
	
	/* now uninvert each individual character. */
	for (c = dvi->sel_start; c <= dvi->sel_end; c++) {
		if (c->c_type != S_SEARCHABLE) {
			continue;
		}
		if (!(c->flags & S_INVERTED)) {
			continue;
		}
		c->flags &= ~(S_INVERTED);
		border = (c->f_info->width < 5) ? 1 : 2;
		dest_x = c->x - border;
		dest_y = c->y - border;
		dest_w = c->f_info->width + 2 * border;
		dest_h = c->f_info->height + 2 * border;
		src_x = c->x - copy_x - border;
		src_y = c->y - copy_y - border;

		pr_rop(dest,
		  dest_x, dest_y,
		  dest_w, dest_h,
		  sunp->invert_op,
		  src,
		  src_x, src_y);
	}
	/* and clean up the pixrect we used. */
	pr_destroy(src);
	return(0);
}
	

/*
 * we make the assumption that the system font has already been opened.
 */
def_font_x()
{
	return(sunp->sys_font->pf_defaultsize.x);
}

def_font_y()
{
	return(sunp->sys_font->pf_defaultsize.y);
}

/*
 * the distance from the top left corner of the default (fixed width)
 * font to its baseline.
 */
def_font_baseline()
{
	return(-(sunp->sys_font->pf_char['A'].pc_home.y));
}

/*
 * make sure that we have a pixrect to draw into.  If the page image has
 * not been allocated yet, make one as big as the user's window.  We
 * assume that the only time this code will ever be called is if the user
 * trys to display some typeout before he has looked at any DVI file.
 */
create_drawing_surface()
{
	int	w = view_r.r_width,
		h = view_r.r_height;

	if (dvi->file != (FILE *) 0) {
		return;
	} 

	if (pg_image != (pixr *) 0) {
		if (w == pg_image->pr_size.x && h == pg_image->pr_size.y) {
			return;
		}
		pr_destroy(pg_image);
	}
	if ((pg_image = mem_create(w, h, 1)) == (pixr *) 0) {
		msg(FATAL, "couldn't aquire the space for a bit map!");
	}

	phys_image_r.r_left = phys_image_r.r_top = 0;
	phys_image_r.r_width = pg_image_r.r_width = w;
	phys_image_r.r_height = pg_image_r.r_height = h;
}

/*
 * write some text onto the page image at the given coordinates.  The
 * assumptions are that the font is fixed width (with the height and
 * width of every character in the font returned by the functions above)
 * and that the coordinates actually point into a memory pixrect.
 */
draw_text(cp, x, y, l)
	char	*cp;
{
	char		save;
	struct pr_prpos	where;

	/* write at most l characters. */
	save = cp[l];
	cp[l] = '\0';

	where.pr = pg_image;
	where.pos.x = x;
	where.pos.y = y + def_font_baseline();
	pf_text(where, sunp->write_op, sunp->sys_font, cp);

	cp[l] = save;
}

static char	fmtbuf[512],
		msg_text[512],
		*msg_last_c = msg_text;
static int	msg_last_x = 0,
		msg_draw_y,
		msg_draw_w,
		msg_draw_h,
		msg_win_width;
static Rect	msg_rect;
extern int	msg_win_cursor;

/*
 * msgs will begin this far from the edge of the window.
 */
#define	MSG_X_BORDER	(4)

/*
 * print a message on the tool namestripe.
 */
show_title(msg, append)
	char	*msg;
{
	if (append) {
		strcpy(fmtbuf, (char *) window_get(sunp->frame,
		  FRAME_LABEL, 0));
		strcat(fmtbuf, msg);
		msg = fmtbuf;
	}
	window_set(sunp->frame, FRAME_LABEL, msg, 0);
}

/*
 * we don't have to deal with the possibility of mouse or scrollbar keys
 * here.  We've just got plain old ascii.
 */

static int	showing_msg_cursor;

show_msg(msg, append, overwrite)
	char	*msg;
{
	register char	*cp = msg,
			msg_c;

	if (cp == (char *) 0) {
		return;
	}
	if (append) {
		if (msg_win_cursor && showing_msg_cursor) {
			/* write over the cursor with the first character. */
			msg_last_x -= msg_draw_w;
		}
		/*
		 * check to see if we should draw this message at the
		 * beginning of the message line.  We only do that if
		 * overwrite is true.  There is room for a bug here
		 * because the length of the string is not necessarily
		 * the length of its printed representation (due to
		 * control characters and other such things) but
		 * presumably, since this option should only be used by
		 * the program (not as a response to input from the user)
		 * there won't be any control characters in the string
		 * and everything will be kosher.
		 */
		if (overwrite && 
		  msg_last_x + strlen(cp) * msg_draw_w > msg_win_width) {
		  	clear_msg_text();
			msg_last_x = MSG_X_BORDER;
			msg_last_c = msg_text;
		}

	} else {
		msg_last_x = MSG_X_BORDER;
		msg_last_c = msg_text;
	}
	showing_msg_cursor = msg_win_cursor;

	rect_construct(&msg_rect, msg_last_x, msg_draw_y, 0, msg_draw_h);

	for (; *cp != '\0'; cp++) {
		*msg_last_c++ = *cp;
		if (iscntrl(*cp)) {
			one_char('^');
			one_char((*cp == '\177') ? '?' : *cp + '\100');
			continue;
		}
		one_char(*cp);
	}

	*msg_last_c = '\0';
	if (msg_win_cursor) {
		one_char(msg_win_cursor + 128);
	}

	/* update the physical window from our mem pixrect here. */
	show_msg_rect(&msg_rect, SHOW_SEL);
}

/*
 * paint this character in the memory pixrect; update msg_last_x, the x
 * at which to paint the next character; and add this character to the
 * bounding box of this message.
 */
one_char(c)
{
	pixr		*src;
	extern int	msg_visible;

	/* clip the message if necessary. */
	if (msg_last_x + msg_draw_w > msg_win_width) {
		return;
	}

	if (c > 127) {
		src = msg_cursors[c - 128];
	} else {
		src = sunp->sys_font->pf_char[c].pc_pr;
	}
	pr_rop(msg_image,
	  msg_last_x, msg_draw_y,
	  msg_draw_w, msg_draw_h,
	  PIX_SRC,
	  src,
	  0, 0);

	msg_visible = 1;
	msg_last_x += msg_draw_w;
	msg_rect.r_width += msg_draw_w;
}


/* erase count characters from the message window. */
msg_del_c(count)
{
	char 	*cp;

	rect_construct(&msg_rect, msg_last_x, msg_draw_y, 0, msg_draw_h);
	for (cp = msg_last_c - 1; count != 0; count--, cp--) {
		if (iscntrl(*cp)) {
			msg_rect.r_width += msg_draw_w;
			msg_rect.r_left -= msg_draw_w;
		}
		msg_rect.r_width += msg_draw_w;
		msg_rect.r_left -= msg_draw_w;
	}
	cp++;

	showing_msg_cursor = msg_win_cursor;

	if (showing_msg_cursor) {
		msg_rect.r_width += msg_draw_w;
		msg_rect.r_left -= msg_draw_w;
	}

	msg_last_c = cp;
	*msg_last_c = '\0';

	show_msg_rect(&msg_rect, CLEAR_SEL);
	msg_last_x = msg_rect.r_left;

	if (showing_msg_cursor) {
		msg_rect.r_width = 0;
		one_char(msg_win_cursor + 128);
		show_msg_rect(&msg_rect, SHOW_SEL);
	}
}

/* ensure that the message window cursor is off. */
msg_cursor_off()
{
	if (msg_win_cursor == NO_MCUR)
		return;

	msg_last_x -= msg_draw_w;
	showing_msg_cursor = 0;

	rect_construct(&msg_rect, msg_last_x, msg_draw_y,
	  msg_draw_w, msg_draw_h);
	show_msg_rect(&msg_rect, CLEAR_SEL);
}


/* clear just the part of the message window that has text in it. */
clear_msg_text()
{
	if (msg_image == (pixr *) 0) {
		return;
	}
	
	rect_construct(&msg_rect, MSG_X_BORDER, msg_draw_y, 
	  msg_last_x - MSG_X_BORDER, msg_draw_h);

	showing_msg_cursor = 0;

	pr_rop(msg_image,
	  msg_rect.r_left, msg_rect.r_top,
	  msg_rect.r_width, msg_rect.r_height,
	  PIX_CLR,
	  NULL,
	  0, 0);
	show_msg_rect(&msg_rect, CLEAR_SEL);
}

show_msg_rect(r, op)
	Rect	*r;
{
	pixr	*src;

	if (op == SHOW_SEL) {
		src = msg_image;
	} else {
		src = (pixr *) 0;
	}

	pw_write(canvas_pixwin(sunp->msg), 
	  r->r_left, r->r_top,
	  r->r_width, r->r_height,
	  PIX_SRC,
	  src,
	  r->r_left, r->r_top);
}


/* clear the area of the page image defined by r */

clear_rect(r)
	Rect	*r;
{
	/* we assume here that the page image has been allocated. */
	pr_rop(pg_image,
	  r->r_left, r->r_top,
	  r->r_width, r->r_height,
	  PIX_SRC,
	  NULL,
	  0, 0);
}


clear_users_window()
{
	Pixwin	*dest = (Pixwin *) window_get(sunp->image, WIN_PIXWIN);

	pw_write(dest,
	  win_r.r_left, win_r.r_top,
	  view_r.r_width, view_r.r_height,
	  sunp->clear_op,
	  NULL,
	  0, 0);

	/* update the view structure and the scrollbars. */
	view_r.r_left = view_r.r_top = 0;
	scrollbar_set (
	  (Scrollbar) window_get(sunp->image, WIN_HORIZONTAL_SCROLLBAR),
	  SCROLL_VIEW_START, 0,
	  0);
	scrollbar_set(
	  (Scrollbar) window_get(sunp->image, WIN_VERTICAL_SCROLLBAR),
	  SCROLL_VIEW_START, 0,
	  0);
	scrollbar_paint(sunp->horiz);
	scrollbar_paint(sunp->vert);		
}

clear_load_image(page)
	pg	*page;
{
	pixr	*pr = (pixr *) page->load_i;

	pr_rop(pr, 0, 0, pr->pr_size.x, pr->pr_size.y,
	  sunp->clear_op, NULL, 0, 0);
}

/*
 * clear the page image, if it exists.
 */
clear_page_image()
{
	if (pg_image != (pixr *) 0) {
		pr_rop(pg_image,
		  0, 0,
		  pg_image->pr_size.x, pg_image->pr_size.y,
		  sunp->clear_op,
		  NULL,
		  0, 0);
	}
}

/*
 * allocate the space for the window system's notion of one load_image
 * that is w by h pixels and return the value in handlep.  takes into
 * account the variable which controls the limit on the number of cached
 * pages.
 * returns 0 and handlep pointing to the space if successful else
 * -1 and *handlep == 0.
 * meant to be called only by alloc_one_page().
 */


alloc_load_image(handlep, w, h)
	char	**handlep;
{
	extern int	page_limit;
	pixr		*pr;

	*handlep = (char *) 0;

	if (page_limit <= dvi->pages->num_cached) {
		free_oldest_load_image();
	}
again:	if ((pr = mem_create(w, h, 1)) != (pixr *) 0) {
		*handlep = (char *) pr;
		return(0);
	}
	if (free_oldest_load_image() == -1) {
		return(-1);
	}
	goto again;
}

/*
 * this routine is meant to be called only by free_one_page().  It
 * exists only because I needed some window system independent way to
 * free up the window system's notion of the load_image.
 */

free_one_load_image(page)
	pg	*page;
{
	pixr	*pr = (pixr *) page->load_i;

	pr_destroy(pr);
	return(0);
}

/*
 * set the size of the page image.
 */
set_page_image_size(w, h)
	int	w,
		h;
{
	extern int	scrollbars;

	if (pg_image != (pixr *) 0) {
		pr_destroy(pg_image);
	}
	if (scrollbars) {
		scrollbar_set(sunp->horiz, 
		  SCROLL_OBJECT_LENGTH, w,
		  0);
		scrollbar_set(sunp->vert,
		  SCROLL_OBJECT_LENGTH, h,
		  0);
	}
again:
	if ((pg_image = mem_create(w, h, 1)) != (pixr *) 0) {
		return(0);
	}
	/* throw away page rects to try to recover */
	if (free_oldest_load_image() == -1) {
		return(-1);
	}
	goto again;
}

/*
 * Get the root file descriptor for the window.  complain and return -1 if
 * we fail.
 */

#include <sys/file.h>

win_rootfd()
{
	char	tmpfname[80];
	int	rootnumber,
		rootfd;

	rootnumber = win_getlink(sunp->tool_fd, WL_PARENT);
	win_numbertoname(rootnumber, tmpfname);
	if ((rootfd = open(tmpfname, O_RDONLY, 0)) < 0) {
		msg(PLAIN, "%s: Can't find root window\n", tl->prog_name);
	}
	return(rootfd);
}

sun_win_func(which_one)
{
	int	rootfd = win_rootfd();

	if (win_rootfd >= 0) {
		switch (which_one) {
		case 1:
			wmgr_close(sunp->tool_fd, rootfd);
			break;
		case 2:
			wmgr_top(sunp->tool_fd, rootfd);
			break;
		case 3:
			wmgr_bottom(sunp->tool_fd, rootfd);
			break;
		}
		close(rootfd);
	}
}

/*
 * DOCUMENTATION
 *
 * Name: close-window
 * Desc: This command ``closes'' \lit{dvitool} to a small iconic shape.
 *	The image that is painted and the position at which the image
 *	is painted are controlled by user-definable variables.
 * SeeA: init-icon-file init-icon-x init-icon-y 
 */
close_window()
{
	sun_win_func(1);
}

/*
 * put our tool at the top of the window display stack.
 */
/*
 * DOCUMENTATION
 *
 * Name: expose-tool
 * Desc: This command behaves exactly the same as the ``expose'' entry
 *	on a the standard tool menu; i.e. it causes a partially covered
 *	\lit{dvitool} to become the topmost tool, and thus unobscured.
 *	If \lit{dvitool} is not covered by some other tool, this command
 *	is a no-op.
 * SeeA: hide-tool toggle-tool
 */
expose_tool()
{
	sun_win_func(2);
}

/*
 * DOCUMENTATION
 *
 * Name: hide-tool
 * Desc: This command behaves exactly the same as the ``Hide'' option
 *	of the standard tool menu; i.e. it causes \lit{dvitool} to be
 *	placed at the bottom of the stack of visible tools.
 * SeeA: expose-tool toggle-tool
 */
hide_tool()
{
	sun_win_func(3);
}


/*
 * DOCUMENTATION
 *
 * Name: toggle-tool
 * Desc: This command puts dvitool at the top of the windows visible
 *	on the screen unless it already is the top window; in that case
 *	it puts it on the bottom of the stack.
 * SeeA: expose-tool hide-tool
 */
toggle_tool()
{
	if (win_getlink(sunp->tool_fd, WL_COVERING) == -1) {
		hide_tool();
	} else {
		expose_tool();
	}
}

/*
 * DOCUMENTATION
 *
 * Name: redraw-tool
 * Desc: This command behaves exactly the same as the ``Redisplay''
 *	option of the standard tool menu; i.e. it causes \lit{dvitool}
 *	to repaint all of its windows.
 */
redraw_tool()
{
	wmgr_refreshwindow(sunp->tool_fd);
}

/* this routine insures that dvitool is open (not iconic). */
open_window()
{
	int	rootfd;

	if ((int) window_get(sunp->frame, FRAME_CLOSED) == 0) {
		return;
	}

	if ((rootfd = win_rootfd()) >= 0) {
		wmgr_open(sunp->tool_fd, rootfd);
		close(rootfd);
	}
}

static short junk[] = {
#	include "icon.h"
};
mpr_static(sv_icon, 64, 64, 1, junk);

/*
 * this routine paints the hourglass image over the icon to show that we
 * are rereading some document.
 */
show_rereading(on)
{
	Icon		fr_icon;
	pixr		*image;
	extern pixr	hour_pr;

	/*
	 * get the icon from the frame, and the pixrect from that.
	 */
	fr_icon = (Icon) window_get(sunp->frame, FRAME_ICON);
	image = (pixr *) icon_get(fr_icon, ICON_IMAGE);

	if (on) {
		pr_rop(&sv_icon,
		  0, 0, 64, 64,
		  PIX_SRC,
		  image,
		  0, 0);

		pr_rop(image,
		  24, 24,
		  16, 16,
		  PIX_SRC,
		  &hour_pr,
		  0, 0);
	} else {
		pr_rop(image,
		  0, 0, 64, 64,
		  PIX_SRC,
		  &sv_icon,
		  0, 0);
	}

	icon_set(fr_icon, ICON_IMAGE, image, 0);

	if (on) 
		window_set(sunp->frame, FRAME_ICON, fr_icon, 0);

	return(0);
}

static struct rect	full_scr_rect,
			normal_screen,
			zoom_scr_rect,
			znormal,
			hzoom_scr_rect,
			hznormal;
static int		is_full = 0,
			is_zoomed = 0,
			is_hzoomed = 0;

/*
 * DOCUMENTATION
 *
 * Name: full-screen
 * Desc: The \em{full-screen} command toggles the size of \lit{dvitool}
 *	between as big as the physical screen will allow and whatever
 *	size it was before \em{full-screen} was invoked.
 * SeeA: zoom-horizontal zoom-vertical
 */
full_screen()
{
	ch_scr_size(&full_scr_rect, &normal_screen, &is_full, 0);
}

/*
 * ``zoom'' dvitool.  This is the same functionality offered by clicking
 * control-middle on the name stripe of the tool:  the tool becomes as
 * tall as the physical screen.  full_screen differs in that it also
 * becomes as wide as the physical screen.
 */

/*
 * DOCUMENTATION
 *
 * Name: zoom-vertical
 * Desc: This command offers the same functionality as clicking 
 *	control middle button on the name stripe, e.g. the tool
 *	becomes a tall as the physical screen while retaining
 *	the current width.  This command is a toggle; running it
 *	again will return \lit{dvitool} to its previous dimensions.
 * SeeA: full-screen zoom-horizontal
 */
zoom_tool()
{
	ch_scr_size(&zoom_scr_rect, &znormal, &is_zoomed, 1);
}

/*
 * DOCUMENTATION
 *
 * Name: zoom-horizontal
 * Desc: Make \lit{dvitool} as wide as the physical screen.  Like
 *	\em{full-screen} and \em{zoom-vertical}, this command is a 
 *	toggle; running it again will return \lit{dvitool} to its 
 *	previous dimensions.
 * SeeA: full-screen zoom-vertical
 */
horiz_zoom()
{
	ch_scr_size(&hzoom_scr_rect, &hznormal, &is_hzoomed, 2);
}

ch_scr_size(big, normal, is_big, type)
	struct rect	*big;
	struct rect	*normal;
	int		*is_big;
{
	struct screen	big_screen;

	if (*is_big) {
		(void) win_getrect(sunp->tool_fd, big);
		wmgr_completechangerect(sunp->tool_fd, normal, big, 0, 0);
	} else {
		/* save the current screen size so we can restore it later */
		(void) win_getrect(sunp->tool_fd, normal);

		/* get the rect of the screen this window runs under */
		win_screenget(sunp->tool_fd, &big_screen);

		/* save off what we need of the rect.*/
		*big = big_screen.scr_rect;
		switch (type) {
		case 0:		/* full screen */
			break;
		case 1:		/* zoom (as tall as possible) */
			big->r_left = normal->r_left;
			big->r_width = normal->r_width;
			break;
		case 2:		/* horizontal zoom (wide) */
			big->r_top = normal->r_top;
			big->r_height = normal->r_height;
			break;
		}

		wmgr_completechangerect(sunp->tool_fd, big,
		  normal, 0, 0);
	}
	*is_big = !*is_big;
}

/*
 * Read the given filename for an array that matches the size needed for
 * the given resolution.  Written by John Coker.
 * Modified for dvitool by jwm.
 */
#define BITSPERSHORT	16

read_pr_from_file(filename, xres, yres, pr)
	char		*filename;
	register int	xres, yres;
	struct pixrect	*pr;
{
	register int	x, y, bit, ch;
	register FILE	*fp;
	int		value;

	fp = fopen(filename, "r");
	if (fp == (FILE *) 0) {
		msg(FATAL | PERROR, "can't open icon/cursor file %s",
		  filename);
	}

	while (isspace(ch = getc(fp)))
		;
	if (feof(fp)) {
		msg(FATAL, "\"%s\": file is empty (other than white space).",
		  filename);
	}
	if (ch == '/')
		ch = getc(fp);
	do
		ch = getc(fp);
	while (ch != '{' && ch != '/' && ch != EOF);
	if (feof(fp)) {
		msg(FATAL,
		    "\"%s\": file should contain an icon/cursor description.",
		    filename);
		return (0);
	}

	/* clear the pixrect and read in individual points */
	pr_rop(pr, 0, 0, xres, yres, PIX_CLR, NULL, 0, 0);
	for (y = 0; y < yres; y++)
		for (x = 0; x < xres; x++) {
			if (x % BITSPERSHORT == 0) {
				if (fscanf(fp, " 0x%x,", &value) != 1) {
					msg(FATAL, 
		    "\"%s\": Not enough elements for given resolution.",
					    filename);
				}
				bit = BITSPERSHORT - 1;
			}
			if (value & (1 << bit--))
				pr_put(pr, x, y, 1);
		}

	fclose(fp);
	return(0);
}
