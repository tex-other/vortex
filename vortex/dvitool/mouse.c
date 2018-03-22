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
	static char rcsid[] = "$Source: /home/yew/yew4/vortex/newdist/dvitool/RCS/mouse.c,v $  (Berkeley)";
#endif not lint

/*
 * Author: Jeff McCarrell at U.C. Berkeley <jwm@Berkeley.EDU>
 * $Date: 1993/09/16 02:29:01 $
 * $State: Exp $
 */

#include "structs.h"
#include "mag.h"
#include "sun.h"
#include <suntool/walkmenu.h>

extern sun_d	*sunp;
extern Event	last_event;

/*
 * the order of these enumerated values should not be arbitrarily changed as a
 * table in keymaps.defs depends on them.  the menu routines return 0 for
 * ``user did not make a choice'' so we need to start at 1.
 */
typedef enum {
		UpCmd = 1, DownCmd, RightCmd, LeftCmd,	/* 4 */
		HomeCmd, BottomCmd,			/* 6 */
		NextPageCmd, PreviousPageCmd,
		NextPagePositionedCmd,
		PreviousPagePositionedCmd,		/* 8*/
		FullScreenCmd,
		QuitCmd,				/* 10 */
		Mag0Cmd, Mag1Cmd, Mag2Cmd,
		Mag3Cmd, Mag4Cmd, Mag5Cmd,
		FileReReadCmd,
		WhichFont,
		Shr1Cmd, Shr2Cmd, Shr3Cmd, Shr4Cmd, Shr5Cmd, /* 23 */
		OverHelpCmd, CmdHelpCmd, VarHelpCmd,
		PrintVarCmd, SetVarCmd,	
		ForSearchCmd, RevSearchCmd,
		FindFileCmd,
		FirstPageCmd, LastPageCmd,
		WinCloseCmd, WinExposeCmd, WinHideCmd,
		RefreshCmd,
		TopLeftCmd, TopRightCmd,
		BotLeftCmd, BotRightCmd,		/* 41 */
		LeftEdgeCmd, RightEdgeCmd,
		TopEdgeCmd, BotEdgeCmd,
		LogPageCmd, PhysPageCmd,
		CharInfoCmd,				/* 48 */
		ListCmdsCmd, ListVarsCmd, cdCmd,
		ZoomCmd, BoundToCmd
} cmd;

/*
 * this table exists so we can equate the enumerated type which represents
 * the ``key'' returned by menu_show to the mag values so we can
 * dynamically create the magnification structures.
 */
typedef struct MagCmds {
	int	mag;
	cmd	m_cmd;
} mag_cmds;

mag_cmds	mag_tbl[] = {
{  0,	Mag0Cmd },
{  1,	Mag1Cmd },
{  2,	Mag2Cmd },
{  3,	Mag3Cmd },
{  4,	Mag4Cmd },
{  5,	Mag5Cmd },
{ -1,	Shr1Cmd },
{ -2,	Shr2Cmd },
{ -3,	Shr3Cmd },
{ -4,	Shr4Cmd },
{ -5,	Shr5Cmd },
{ MAG_SENTINEL },
};


/*
 * lookup the enum value given the magstep.
 */
cmd_from_step(val)
{
	mag_cmds	*p = mag_tbl;

	for (; p->mag != MAG_SENTINEL; p++) {
		if (p->mag == val) {
			return((int) p->m_cmd);
		}
	}
	return(-1);
}


Menu	main_menu,
	scroll_menu,
	edge_menu,
	corner_menu,
	window_menu,
	page_menu,
	file_menu,
	magnify_menu,
	shrink_menu,
	char_menu,
	var_menu,
	help_menu;

init_menus()
{

	help_menu = menu_create(
		MENU_STRING_ITEM, "overview", OverHelpCmd,
		MENU_STRING_ITEM, "commands", CmdHelpCmd,
		MENU_STRING_ITEM, "variables", VarHelpCmd,
		MENU_STRING_ITEM, "list all cmds", ListCmdsCmd,
		MENU_STRING_ITEM, "list all vars", ListVarsCmd,
		MENU_STRING_ITEM, "bound to", BoundToCmd,
		0);

	var_menu = menu_create(
		MENU_STRING_ITEM, "print (esc p)", PrintVarCmd,
		MENU_STRING_ITEM, "set   (esc s)", SetVarCmd,
		0);

	char_menu = menu_create(
		MENU_STRING_ITEM, "search > (^s)", ForSearchCmd,
		MENU_STRING_ITEM, "< search (^r)", RevSearchCmd,
		MENU_STRING_ITEM, "which font", WhichFont,
		MENU_STRING_ITEM, "which char", CharInfoCmd,
		0);
	/*
	 * create the magnify and shrink menus later on the fly.
	 */
	shrink_menu = menu_create(
		0);
	magnify_menu = menu_create(
		0);
	file_menu = menu_create(
		MENU_STRING_ITEM, "find (^X^f)",	FindFileCmd,
		MENU_STRING_ITEM, "reread  (R)",	FileReReadCmd,
		MENU_STRING_ITEM, "change dir",		cdCmd,
		0);
	page_menu = menu_create(
		MENU_STRING_ITEM, "next      (n)", NextPageCmd,
		MENU_STRING_ITEM, "next pos  (N)", NextPagePositionedCmd,
		MENU_STRING_ITEM, "previous  (p)", PreviousPageCmd,
		MENU_STRING_ITEM, "prev pos  (P)", PreviousPagePositionedCmd,
		MENU_STRING_ITEM, "first (esc <)", FirstPageCmd,
		MENU_STRING_ITEM, "last  (esc >)", LastPageCmd,
		MENU_STRING_ITEM, "logical   (g)", LogPageCmd,
		MENU_STRING_ITEM, "physical  (G)", PhysPageCmd,
		0);
	window_menu = menu_create(
		MENU_STRING_ITEM, "close     (c)",	WinCloseCmd,
		MENU_STRING_ITEM, "expose  (^xx)",	WinExposeCmd,
		MENU_STRING_ITEM, "hide    (^xh)", 	WinHideCmd,
		MENU_STRING_ITEM, "fullscreen(f)", 	FullScreenCmd,
		MENU_STRING_ITEM, "zoom      (z)", 	ZoomCmd,
		MENU_STRING_ITEM, "refresh  (^l)", 	RefreshCmd,
		MENU_STRING_ITEM, "exit      (e)", 	QuitCmd,
		0);
	corner_menu = menu_create(
		MENU_STRING_ITEM, "top left         (t)", TopLeftCmd,
		MENU_STRING_ITEM, "top right    (esc t)", TopRightCmd,
		MENU_STRING_ITEM, "bottom left      (b)", BotLeftCmd,
		MENU_STRING_ITEM, "bottom right (esc b)", BotRightCmd,
		0);
	edge_menu = menu_create(
		MENU_STRING_ITEM, "left   (^a)",	LeftEdgeCmd,
		MENU_STRING_ITEM, "right  (^e)",	RightEdgeCmd,
		MENU_STRING_ITEM, "top    (^t)",	TopEdgeCmd,
		MENU_STRING_ITEM, "bottom (^b)",	BotEdgeCmd,
		0);
	scroll_menu = menu_create(
		MENU_STRING_ITEM, "up    (u)", UpCmd,
		MENU_STRING_ITEM, "down  (d)", DownCmd,
		MENU_STRING_ITEM, "right (r)", RightCmd,
		MENU_STRING_ITEM, "left  (l)", LeftCmd,
		0);
#define MAGTEXT	"magnify"
#define SHRTEXT	"shrink"
	main_menu = menu_create(
		MENU_PULLRIGHT_ITEM, "scroll",		scroll_menu,
		MENU_PULLRIGHT_ITEM, "edge",		edge_menu,
		MENU_PULLRIGHT_ITEM, "corners",		corner_menu,
		MENU_PULLRIGHT_ITEM, "window",		window_menu,
		MENU_PULLRIGHT_ITEM, "page",		page_menu,
		MENU_PULLRIGHT_ITEM, "file",		file_menu,
		MENU_PULLRIGHT_ITEM, MAGTEXT,		magnify_menu,
		MENU_PULLRIGHT_ITEM, SHRTEXT,		shrink_menu,
		MENU_PULLRIGHT_ITEM, "char",		char_menu,
		MENU_PULLRIGHT_ITEM, "variable",	var_menu,
		MENU_PULLRIGHT_ITEM, "help",		help_menu,
		0);
}

/*
 * here we change the appearance of the menu to reflect the current
 * number of grow/shrink steps available.
 */
static
show_grow_steps()
{
	extern mags	magsteps[];
	mags		*magp,
			*cur_mag;
	Menu		tmp,
			new_grow,
			new_shrink;
	Menu_item	grow,
			shrink;
	int		cmd_off;

	grow = menu_find(main_menu, MENU_STRING, MAGTEXT, 0);
	shrink = menu_find(main_menu, MENU_STRING, SHRTEXT, 0);

	/*
	 * what we need to do is put all of the magsteps > the current
	 * mag level on the grow menu and all of them < cur mag level on
	 * the shrink menu.
	 */
	for (magp = &magsteps[1]; magp->mag != dvi->user_mag; magp++) {
		if (magp->step == MAG_SENTINEL) {
			return(-1);
		}
	}
	cur_mag = magp;

	new_grow = menu_create(0);
	for (magp = cur_mag + 1; magp->step != MAG_SENTINEL; magp++) {
		if ((cmd_off = cmd_from_step(magp->step)) == -1) {
			return(-1);
		}
		menu_set(new_grow, MENU_STRING_ITEM, magp->str, cmd_off,
		 0);
	}
	
	if ((tmp = (Menu) menu_get(grow, MENU_PULLRIGHT)) != (Menu) 0) {
		menu_destroy(tmp);
	}

	menu_set(grow,
	  MENU_PULLRIGHT, new_grow,
	  MENU_INACTIVE, (magp == cur_mag + 1),
	  0);

	new_shrink = menu_create(0);
	for (magp = cur_mag - 1; magp->step != MAG_SENTINEL; magp--) {
		if ((cmd_off = cmd_from_step(magp->step)) == -1) {
			break;
		}
		menu_set(new_shrink, MENU_STRING_ITEM, magp->str, cmd_off,
		  0);
	}

	if ((tmp = (Menu) menu_get(shrink, MENU_PULLRIGHT)) != (Menu) 0) {
		menu_destroy(tmp);
	}

	menu_set(shrink,
	  MENU_PULLRIGHT, new_shrink,
	  MENU_INACTIVE, (magp == cur_mag - 1),
	  0);

	return(0);
}
	

/*
 * come here to handle functions to be executed by menu operations.
 */
/*
 * DOCUMENTATION
 *
 * Name: mouse-menus
 * Desc: This command invokes the menus.  It may only be executed in
 * 	response to a mouse button, though \em{bind-to-key} will bind
 *	it anywhere.
 */
mouse_exec(argv)
	func_arg	*argv;
{
	caddr_t		s;
	int		j;
	extern func	*menu_map[];

	(void) show_grow_steps();

	/*
	 * menu_show needs the event in window_space, not canvas_space.
	 */
	s = menu_show(main_menu, sunp->image,
	  canvas_window_event(sunp->image, &last_event), 0);
	if (s != 0) {
		j = (int) --s;
		exec_command(menu_map[j]);
	}
}

