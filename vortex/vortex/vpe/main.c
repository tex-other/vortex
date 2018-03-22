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

#include <sys/types.h>
#include <sys/file.h>
#include <sys/signal.h>
#include <ps_comm.h>
#include <tp_comm.h>
#include <X/Xlib.h>
#include <stdio.h>
#include "macros.h"

/*
 *  This code is part of the VorTeX project.  This file was
 *  written by Steven Procter for the VorTeX project under the
 *  direction of Prof. Michael A. Harrison of the University
 *  of California at Berkeley.  This code was written by Steven
 *  Procter.
 *
 *  Copyright (c) 1987 Steven James Procter
 *  University of California, Berkeley
 *  procter@renoir.Berkeley.EDU
 *
 *  This file intact and any code derived from it are copyright
 *  Steven Procter and the Regents of the University of California.
 *  All rights reserved by the author.  See the copyright notice
 *  distributed with this software for a complete description of
 *  the conditions under which it is made available.
 */

/*  Ok, the arguments passed to main from the source editor are (in this
 *  order): <host to connect to> <socket number> <X display name> <window id>
 *  with optional arguments of the form ``-d symbol_name'', where symbol_name
 *  is some known debugging symbol (see debug.h).
 */

#define USAGE               "Usage: vpe Host Socket Display [-d symbol]\n"
#define ARG_HOSTNAME        1
#define ARG_SOCKET_NUMBER   2 
#define ARG_DISPLAY_NAME    3

/*  The 2 communications ports.  */
int         vse_socket;
int         tex_socket;
/*stuff for the selection code*/

int ox,oy;        
int sel_lev;

main (ac, av)
char    *av[];
{
    int         status;
    int         fd;
    int         pipe_closed ();
    Display     *dpy;
    Display     *XOpenDisplay ();
    register    i;
    
    if (ac < 4) {
	fprintf (stderr, USAGE);
	LEAVE(1);
    }

    signal (SIGPIPE, pipe_closed);
    
    /*  Parse the optional arguments.  */
    if (ac > 4) {
	for (i = 4; i < ac; i++) {
	    if (*av[i] == '-') {
		av[i]++;
	    }
	    else {
		fprintf (stderr, "vpe: Unknown argument: \"%s\"\n", av[i]);
		continue;
	    }
	    switch (*av[i]) {
	    case 'd':
		if (*++av[i] != NULL) {
		    printf ("vpe: Setting \"%s\" debugging option.\n", av[i]);
		    SetDebug (av[i]);
		}
		else if (++i <= ac) {
		    printf ("vpe: Setting \"%s\" debugging option.\n", av[i]);
		    SetDebug (av[i]);
		}
		else {
		    fprintf(stderr, "vpe: -d option needs a symbol passed.\n");
		    LEAVE(1);
		}
		break;

	    default:
		fprintf (stderr, "vpe: Unknown option \"%c\".\n", *av[i]);
		break;
	    }
	}
    }
    if ((dpy = XOpenDisplay (av[ARG_DISPLAY_NAME])) == (Display *) NULL) {
	fprintf (stderr, "vpe fatal error: couldn't open display %s.\n",
		 av[ARG_DISPLAY_NAME]);
	LEAVE(1);
    }
    vse_socket = ConnectToSource (av[ARG_HOSTNAME], 
				  atoi (av[ARG_SOCKET_NUMBER]));
    if (vse_socket < 0) {
	fprintf (stderr, 
		 "vpe fatal error: Couldn't connect to the source editor.\n");
	LEAVE(1);
    }
    
    /*  Note that I don't connect to the formatter until the source
     *  editor tells me to.
     */
    tex_socket = -1;

    /*  This is the main loop of the program.  */
    do {
	status = ParseCommand (vse_socket, tex_socket);
    } while (!status);
    printf ("vpe: Ich bin gestorben.\n");
    LEAVE(0);
}

pipe_closed (sig)
{
    char    *a[55];

    fprintf (stderr, "vpe: some pipe connection closed (sig=%d).\n", sig);
    bzero (a, sizeof (a));
    *a[0] = 1;
    LEAVE(1);
}
