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
	static char rcsid[] = "$Source: /home/yew/yew4/vortex/newdist/dvitool/RCS/reread.c,v $  (Berkeley)";
#endif not lint

/*
 * Author: Jeff McCarrell at U.C. Berkeley <jwm@Berkeley.EDU>
 * $Date: 1993/09/16 02:29:01 $
 * $State: Exp $
 */

#include "structs.h"
#include "constants.h"
#include "fdecls.h"

reread_handler()
{
	char		tmpfname[40],
			new_dvi_name[MAXPATHLEN];
	static char	emsg[] = "couldn't open/read tmp file";
	FILE		*tmp_file;
	int		rootfd,
			rootnumber;

	/*
	 * the first thing we want to do is change the icon to show that
	 * we are rereading.
	 */
	show_rereading(1);

	sprintf(tmpfname, "/tmp/dvitool%d", getpid());
	if ((tmp_file = fopen(tmpfname, "r")) == (FILE *) 0) {
		msg(PERROR | WAIT, emsg, tmpfname);
		goto cleanup;
	}
	if (fgets(new_dvi_name, MAXPATHLEN, tmp_file) == (char *) 0) {
		msg(PERROR | WAIT, emsg, tmpfname);
		fclose(tmp_file);
		goto cleanup;
	}
	/* take the newline out of the file name. */
	new_dvi_name[strlen(new_dvi_name) - 1] = '\0';
	fclose(tmp_file);
	unlink(tmpfname);

	if (dvi->fname == (char *) 0 || 
	  strcmp(dvi->fname, new_dvi_name) != 0) {
		dvi->fname = str_save(new_dvi_name);
		(void) load_dvi_file((pg *) 0, 0, 0,  0);
	} else {
		(void) reread_file();
	}
	/* open the window from iconic. */
	open_window();
cleanup:
	/* restore the icon image. */
	show_rereading(0);
}


/*
 * this flag tells other routines that we are rereading a DVI file, so
 * some values will not be reset.
 */
int	rereading = 0;

/*
 * DOCUMENTATION
 *
 * Name: reread-file
 * Desc: This command reloads and redisplays the current \lit{DVI}
 *	file.  It is most useful when in the \pass{edit--\TeX--preview}
 *	cycle.
 * SeeA: find-file
 */
reread_file()
{
	/*
	 * if we've had errors before on this file, the FILE * will have
	 * been closed, but dvi->fname will still point to a valid name;
	 * we try to use that name as well as the open FILE * for the
	 * reread.
	 */
	if (dvi->fname == (char *) 0) {
		msg(PLAIN, "null filename in reread -- use find-file.");
		return(-1);
	}

	rereading = 1;
	reload_this_file();
	rereading = 0;
	return(0);
}

/*
 * DOCUMENTATION
 *
 * Name: reload-fonts
 * Desc: This function is identical to \em{reread-file} except that
 *	before the file is reread, all of the characters in the
 *	font cache are flushed.  This command is most useful when
 *	some of the pixel images of characters in a \lit{DVI} file
 *	have changed.
 * SeeA: reread-file
 */
reload_fonts()
{
	if (flush_font_cache() != 0) {
		msg(PLAIN, "font cache flush failed!");
		return(-1);
	}
	(void) reread_file();
	return(0);
}

	

#include <sys/file.h>
#include "texdvi.h"

/*
 * these routines create and/or maintain a file which is a stack of pid's
 * of running dvitools.  this stack is used by texdvi to notify the most
 * current dvitool that it is to reread a document.  'most current' is denoted
 * by the pid which is on the last line of the file.
 */

#include <sys/errno.h>
extern int	errno;

static char 	pidfname[40];
static int	pids[MAXPIDS];

/* take our pid off of the stack.  called when exiting. */
remove_pid(ourpid)
	register int ourpid;
{
	register int	count,
			numlines;
	int 		found = -1,
			fclose_fd = 0;
	char 		str[MAXPATHLEN];
	FILE		*ftmp;

	/*
	 * if there aren't enough file descriptors, try again.
	 */
again:
	if ((ftmp = fopen(pidfname, "r+")) == (FILE *) 0) {
		if (errno == EMFILE && fclose_fd < 3) {
			fclose(fclose_fd);
			goto again;
		} else
			return;
	}
	for (count = 0; count < MAXPIDS; count++) {
		if (fgets(str, MAXPATHLEN, ftmp) == (char *) 0)
			break;
		pids[count] = atoi(str);
	}
	fclose(ftmp);
	numlines = count;
	for (count = 0; count < MAXPIDS; count++) {
		if (ourpid == pids[count]) {
			found = count;
			break;
		}
	}
	if (numlines == 1)
		unlink(pidfname);
	else {
		if ((ftmp = fopen(pidfname, "w+")) == (FILE *) 0){
			perror(pidfname);
			exit(1);
		}
		flock(fileno(ftmp), LOCK_EX);
		for (count = 0; count < numlines; count++) {
			if (count != found)
				fprintf(ftmp, "%d\n", pids[count]);
		}
		flock(fileno(ftmp), LOCK_UN);
		fclose(ftmp);
	}
}

install_pid()
{
	register int	count;
	char		str[20];
	FILE		*ftmp;
	extern tl_data	*tl;
	
	sprintf(pidfname, "%s.%d", PIDFILE, getuid());
	if ((ftmp = fopen(pidfname, "r+")) == (FILE *) 0 && errno != ENOENT) {
		perror(pidfname);
		return;
	}
	if (errno == ENOENT) {
		if ((ftmp = fopen(pidfname, "w+")) == (FILE *) 0) {
			return;
		}
	} else {
		for (count = 0; ; count++) {
			if (fgets(str, MAXPATHLEN, ftmp) == (char *) 0)
				break;
		}
	}
	flock(fileno(ftmp), LOCK_EX);
	fseek(ftmp, 0L, 2);
	fprintf(ftmp, "%d\n", tl->pid);
	flock(fileno(ftmp), LOCK_UN);
	fclose(ftmp);
}

/*
 * read a pid from the pid file and return it.  return -1 for no pids in
 * the file, or a non-existant pid file.
 */

get_dvitool_pid()
{
	char buf[40];
	register FILE *fp;
	register int count;	

	sprintf(pidfname, "%s.%d", PIDFILE, getuid());
	if ((fp = fopen(pidfname, "r")) == (FILE *) 0) {
		return(-1);
	}
	for(count = 0; count < MAXPIDS; count++) {
		if (fgets(buf, MAXPATHLEN, fp) == (char *) 0)
			break;
		pids[count] = atoi(buf);
	}
	fclose(fp);

	return((count == 0) ? -1 : pids[count - 1]);
}

/*
 * This routine attempts to use an already running dvitool to preview the
 * first file named on the command line.  This mechanism does not support
 * multiple file names.  
 * Returns: -1 if failure, exit(0) if succeeds.
 */
use_existing_tool()
{
	register int	pid;
	char		buf[80],
			*fn;
	FILE		*fp;
	extern tl_data	*tl;


	/*
	 * the message sent consists of writing a file in /tmp
	 * and sending a signal to the running dvitool.
	 */
	/*
	 * here we check the return status of kill
	 * in case some dvitool didn't remove its
	 * pid from the list when it was terminated.
	 */
	for (;;) {
		if ((pid = get_dvitool_pid()) < 0) {
			return(-1);
		}
		if (kill(pid, 0) < 0) {
			remove_pid(pid);
			continue;
		}
		break;
	}
	/* we've found a valid process id number */
	sprintf(buf, "/tmp/dvitool%d", pid);
	if ((fp = fopen(buf, "w")) == (FILE *) 0) {
		fprintf(stderr, "%s: couldn't open tmpfile %s\n",
		  tl->prog_name, buf);
		fflush(stderr);
		return(-1);
	}

	if ((fn = next_file_name()) == (char *) 0) {
		fprintf(stderr, "must name a dvi file on the command line!");
		fflush(stderr);
		exit(1);
	}

	fputs(fn, fp);
	fputc('\n', fp);
	fclose(fp);
	if (kill(pid, SIGREREAD)) {
		sprintf(buf, "kill failed again! pid %d", pid);
		perror(buf);
		return(-1);
	}
	fprintf(stderr, "using existing dvitool\n");
	fflush(stderr);
	exit(0);
}

