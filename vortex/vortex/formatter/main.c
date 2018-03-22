/* 
 * Copyright (c) 1986-1987 The Regents of the University of California.
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

#ifdef VORTEX

/*
 *
 *  VorTeX - Visually Oriented TeX
 *  A Multiple Representation Document Preparation Environment
 *
 *  This file is part of the VorTeX incremental formatter,
 *  adapted from similar code for the proof editor written by Jeff McCarrell
 *
 *  Copyright (C) 1987 by	Pehong Chen	(phc@renoir.berkeley.edu)
 *		       and	Jeff McCarrell	(jwm@renoir.berkeley.edu)
 *
 *  Computer Science Division
 *  University of California, Berkeley
 *
 *  All rights reserved by the author.  See the copyright notice
 *  distributed with this software for a complete description of
 *  the conditions under which it is made available.
 *
 */

#include	<stdio.h>
#include	<sys/types.h>
#include	<ctype.h>
#include	<signal.h>
#include	"gl_comm.h"
#include	"ts_comm.h"
#include	"tex.h"
#include	"scan.h"
#include	"main.h"
#include	"failcodes.h"
#include	"msg.h"

extern int	ts_sock;
extern int	tp_sock;

extern char	program[];
extern char	log_fname[];
extern char	USAGE[];
extern char	docname[];

extern FILE	*log_fp;

char		*rindex();
extern int	show_debug;
extern int	tex_only;	/* for creating undumped version */
extern int	local_only;	/* retrieve data via local file access */
extern int	write_out;	/* write out files */
extern int	sig_state;

extern u_long	*tex_pool;
extern int	tex_max;
extern int	Fid;

extern FILE	*a_open_in();
extern char	*name_of_file;

static int	v_page = 0;
static int	s_page = 0;
int		show_state = FALSE;


#define CHAR_SHIFT		14
#define FILE_SHIFT		7
#define ASCII_SIGN		0x7f

read_tex(hdr, fid)
{
	int	i = 0;
	FILE	*fp;

	if ((fp = a_open_in()) == NIL) {
		msg(STDERR, "TeX file %s not found.", name_of_file);
		exit(-1);
	}

	fseek(fp, 0L, 2);
	tex_max = ftell(fp) + hdr;
	if ((tex_pool = (u_long *) malloc(tex_max*sizeof(u_long))) == NIL) {
		msg(FATAL, "Not enough core...abort.");
	}
	fseek(fp, 0L, 0);
	for (i = hdr; i < tex_max; i++) {	
		tex_pool[i] = htonl(i << CHAR_SHIFT | 
					fid << FILE_SHIFT |
					(ASCII_SIGN & getc(fp)));
		/* msg(STDERR, "data[%d]=%d", i, tex_pool[i]); */
	}
	
	ir_open_file(fid, tex_max*sizeof(u_long), tex_pool);
	fclose(fp);
}

test_local_only (fn)
	char		*fn;
{
	ts_format(1, strlen(fn), fn);
	while (format_continue)
		bg_format();
	if (v_page > 0)
		viewing_page = v_page;
	if (s_page > 0) {
		starting_page = s_page;
		fg_format();
	}
/*
	tex_pool[0] = htonl(IRS_BOF);
	tex_pool[1] = htonl(70L);
	tex_pool[2] = htonl(1L);
	ir_delete(Fid, 3*sizeof(u_long), tex_pool);
	fg_format();
*/
	ts_closedoc(1);
/*
	read_tex(0);
	ir_open_file(0, tex_max*sizeof(u_long), tex_pool);
	read_tex(0);
	ir_open_file(20, tex_max*sizeof(u_long), tex_pool);
	read_tex(3);
	tex_pool[0] = htonl(IRS_BOF);
	tex_pool[1] = htonl(10L);
	tex_pool[2] = htonl(tex_max-3);
	ir_insert(0, tex_max*sizeof(u_long), tex_pool);
	ir_delete(0, 3*sizeof(u_long), tex_pool);
*/
}



startup(hname, port)
	char	*hname,
		*port;
{
	extern int	sig_quit(),
			sig_intr();

	/* setup the log file, if any. */
	if (log_fname[0] != NIL) {
		log_fp = fopen(log_fname, "w");
		if (log_fp == (FILE *) 0) {
			fprintf(stderr, "%s: couldn't open log file \"%s\"\n",
			  program, log_fname);
			log_fname[0] = NIL;
		}
	}

	/* check the formatter <-> source table. */
	(void) commfunc_check(0);
	(void) commfunc_check(1);

	/* setup the interrupt handlers. */
	(void) signal(SIGQUIT, sig_quit);
	(void) signal(SIGINT, sig_intr);
	
	/* make the connection back to vse. */
	if (mktsconn(hname, port) < 0) {
		return(-1);
	}

	return (0);
}


/* die, but first tell the other connections about it. */
expire(code)
{
	if (ts_sock != -1) {
		(void) ts_send(GLC_QUIT, 0, 0, 0, NIL);
	}
	if (tp_sock != -1) {
		(void) tp_send(GLC_QUIT, 0, 0, 0, NIL);
	}

	msg(MDBUG, "formatter going away now.");
	giveup(code);
}

/* die without informing the source, format connections. */
giveup(code)
{
	close_socks();
	/* sleep for a sec to allow things to shutdown. */
	sleep(1);
	exit(code);
}

/*
 * the signal handlers just set some state and return.
 */
sig_quit()
{
	sig_state |= GOT_QUIT;
}

sig_intr()
{
	sig_state |= GOT_INTR;
}


main(argc, argv)
	char		*argv[];
{
	char		*arg;
	int		portno;
	char		*hostname;
	int		arg_count = 0;
#ifdef TIME
		gettimeofday(&time0, &tz);
#endif
	tex_only = FALSE;
	if ((arg = rindex(argv[0], '/')) == NIL) {
		arg = argv[0];
	} else {
		arg++;
	}

	if (argc < 2 || *argv[1] == '\0') {
		msg(STDERR, USAGE, program);
		exit(1);
	}

	while (--argc > 0) {
		arg = *++argv;
		if (*arg == '-') {
			for (++arg; *arg != '\0'; ++arg) {
				switch(*arg) {
				case 'd':
					/* show debug messages */
					show_debug = TRUE;
					break;
				case 'l':
					/* local file access, for debugging */
					local_only = TRUE;
					break;
				case 't':
					/* creating undumped version */
					tex_only = TRUE;
					break;
				case 'o':
					/* creating out files */
					write_out = TRUE;
					break;
				case 'v':
					/* setting local viewing page */
					sscanf(*++argv, "%d", &v_page);
					argc--;
					break;
				case 's':
					/* setting local starting page */
					sscanf(*++argv, "%d", &s_page);
					argc--;
					break;
				case 'm':
					/* setting state file size flag */
					show_state = TRUE;
					break;
				default:
					fprintf(stderr, "%s: unknown flag %c.\n", program, *arg);
					fprintf(stderr, USAGE, program);
				}
			}
		} else {
			if (tex_only || local_only)
				break;

			arg_count++;
			switch (arg_count) {
			case 1:
				hostname = arg;
				break;
			case 2:
				portno = atoi(arg);
				break;
			default:
				fprintf(stderr, "%s: unknown arg %s.\n", program, arg);
				fprintf(stderr, USAGE, program);
				break;
			}
				
		}
	}

	if (tex_only) {
		strcpy(docname, *argv);
		viewing_page = INFINITY;
		fg_format();
		post_format();
		giveup(ES_NORMAL);
	} 

 	if (local_only) {
		test_local_only(*argv);
		giveup(ES_NORMAL);
	} 
		
	if (startup(hostname, portno) < 0) {
		/* give up if we can't initialize. */
		giveup(ES_STARTUP);
	}

#if 0
	/*
	 * temporarily, sleep for a few seconds and then inform the
	 * source editor that we're going away.
	 */
	sleep(20);
	expire(ES_NORMAL);
	/* NOTREACHED */
#endif 0

	/*
	 * Everything is started up OK, so start processing commands.
	 * This call probably won't return.
	 */
	top_level(0, 5);

	/* clean up everything. */
	giveup(ES_NORMAL);
}

#endif VORTEX
