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
 * DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES ARISING OUT
 * OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF THE UNIVERSITY OF
 * CALIFORNIA HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY WARRANTIES,
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
 * AND FITNESS FOR A PARTICULAR PURPOSE.  THE SOFTWARE PROVIDED HEREUNDER IS
 * ON AN "AS IS" BASIS, AND THE UNIVERSITY OF CALIFORNIA HAS NO OBLIGATION TO
 * PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.
 */

/*
 *	$Header: /home/yew/yew4/vortex/newdist/dvitool/RCS/texdvi.c,v 2.2 1993/09/10 02:51:02 munson Exp $
 *
 *	Jeff McCarrell -- UC Berkeley
 *
 *	texdvi: run tex on the file named on the command line
 *	and then display it using dvitool.
 *	if there is no running dvitool, start one or instruct
 *	the running dvitool to display the just-tex'd file.
 *	run tex, latex or slitex on the file depending on argv[0].
 */

#include <stdio.h>
#include <sys/param.h>
#include <sys/file.h>
#include <errno.h>
#include <signal.h>
#include <sys/wait.h>
#include <sys/stat.h>
#include "texdvi.h"

#define FALSE 0
#define TRUE 1

/* from the C library */
extern int errno;
extern char **environ;

char *ParseDVIName();
char *rindex();
char *index();

char *progname;
char dviname[MAXPATHLEN];
char texname[MAXPATHLEN];
int dvipid;

main(argc, argv)
	int argc;
	char *argv[];
{
	char texfname[MAXPATHLEN];
	char tmpfname[MAXPATHLEN];
	char workingdir[MAXPATHLEN];
	char errbuf[BUFSIZ];
	char buf[BUFSIZ];
	char *cp;
 	int child, waittmp, tmp;
	FILE *fp, *fopen();
	union wait status;
	struct stat beforetex, aftertex;
	int newlycreated;

	progname = *argv;

	/*
	 *	figure out whether they want to run tex, latex or slitex
	 *
	 *	assume the name of the processor is of the form:
	 *	pdvi where p is the name of the processor to be run.
	 */
	if ((tmp = strindex(progname, "dvi")) == -1) {
		sprintf(errbuf, "can't determine the name of the processor to run from %s", progname);
		exit(1);
	}
	strncpy(texname, progname, tmp);

	/*
	 * this call must follow the  code which figures out texname
	 * since usage() uses texname.
	 */
	if (argc != 2) {
		usage();
		exit(1);
	}

	dviname[0] = '\0';
	strcat(texfname, *++argv);

#if 0
	if (strncmp(&texfname[strlen(texfname) - (sizeof (TEXEXTENSION) - 1)],
	  TEXEXTENSION, sizeof (TEXEXTENSION) - 1))
		strcat(texfname, TEXEXTENSION);
#endif
	/*
	 * we need to generate the tex filename exactly as TeX
	 * manufactures it so we can compute the .dvi name the same way
	 * TeX will.
	 */
	if (( cp = index(texfname, '.')) == (char *) 0) {
		strcat(texfname, TEXEXTENSION);
	} else {
		strcpy(cp, TEXEXTENSION);
	}

	strcpy(dviname, texfname);
	if (ParseDVIName(dviname, MAXPATHLEN) == NULL) {
		fprintf(stderr, "%s filename too long\n", texfname);
		exit(1);
	}
	if (stat(dviname, &beforetex) == -1 && errno == ENOENT)
		newlycreated = TRUE;

	if ((child = fork()) == -1) {
		sprintf(errbuf, "%s, fork failed!", progname);
		perror(errbuf);
	} else if (child == 0) {
		execlp(texname, texname, texfname, 0, environ);
		perror(texname);
		_exit(127);
	} else {
		signal(SIGINT, SIG_IGN);
		dvipid = getdvitoolpid();
		while ((waittmp = wait(&status)) != child && waittmp != -1)
			;

		if ((stat(dviname, &aftertex) == 0) &&
		  (newlycreated || beforetex.st_mtime != aftertex.st_mtime)) {
			if (waittmp == child && 
			  status.w_T.w_Retcode == 0 ||
			  status.w_T.w_Retcode != 0 &&
			  userconfirm() == TRUE) {
				if (dvipid == 0) {
					createdvitool(&child);
				} else {
					/*
					 * here we check the return status of kill
					 * in case some dvitool didn't remove its
					 * pid from the list when it was terminated.
					 */
					while (dvipid != 0 && kill(dvipid, 0) < 0) {
						removepid(dvipid);
						dvipid = getdvitoolpid();
					}
					if (dvipid == 0)
						createdvitool(&child);
					else {
						sprintf(tmpfname, "/tmp/dvitool%d", dvipid);
						if ((fp = fopen(tmpfname, "w")) == (FILE *) NULL) {
							sprintf(errbuf, "%s: couldn't open tmpfile %s", progname,
							  tmpfname);
							perror(errbuf);
						} else {
							if (!getwd(workingdir)) {
								fputs(dviname, fp);
								fputc('\n', fp);
							} else
								fprintf(fp, "%s/%s\n", workingdir, dviname);
							fclose(fp);
							if (kill(dvipid, SIGREREAD)) {
								sprintf(errbuf, "kill failed again! pid %d:", dvipid);
								perror(errbuf);
							}
							fputs("using existing dvitool\n", stderr);
						}
					}
				}
			}
		}
	}
	exit(0);
}

				
usage()
{
	fprintf(stderr, "usage: %% %s texfile[.tex]\n", progname);
	fprintf(stderr, "runs %s on texfile, then either execs %s on the dvifile\n", texname, DVITOOLNAME);
	fprintf(stderr, "\tor sends a signal to a running %s to re-read the dvifile\n", DVITOOLNAME);
}

userconfirm()
{
	char c;

	fprintf (stderr, "there were errors in the TeX file.\n\
do you still want to preview %s [yn] ? ", dviname);
	fflush(stderr);
	return ((c = ttyin()) == 'y' || c == 'Y' || c == '\r' || c == '\n');
}

createdvitool(child)
	int *child;
{	
 	fputs("starting dvitool\n", stderr);
	if ((*child = fork()) == -1)
		perror("fork!");
	else if (*child != 0)
		exit(0);
	else {
		execlp(DVITOOLNAME, DVITOOLNAME, dviname, 0, environ);
		perror(DVITOOLNAME);
		_exit(127);
	}
}

int pids[MAXPIDS];


getdvitoolpid()
{
	char tmpfname[40];
	char buf[MAXPATHLEN];
	register FILE *fp;
	register int count;	

	sprintf(tmpfname, "%s.%d", PIDFILE, getuid());
	if ((fp = fopen(tmpfname, "r")) == (FILE *) NULL) 
		return(0);
	else {
		for(count = 0; count < MAXPIDS; count++) {
			if (fgets(buf, MAXPATHLEN, fp) == (char *) NULL)
				break;
			pids[count] = atoi(buf);
		}
		fclose(fp);
		return(pids[count - 1]);
	}
}

removepid(ourpid)
	register int ourpid;
{
	register int count, numlines;
	int found = -1;
	char str[100];
	char pidfname[100];
	FILE *fp;

	sprintf(pidfname, "%s.%d", PIDFILE, getuid());
	if ((fp = fopen(pidfname, "r+")) == (FILE *) NULL) {
		fp = (FILE *) NULL;
		return;
	}
	for (count = 0; count < MAXPIDS; count++) {
		if (fgets(str, MAXPATHLEN, fp) == (char *) NULL)
			break;
		pids[count] = atoi(str);
	}
	fclose(fp);
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
		if ((fp = fopen(pidfname, "w+")) == (FILE *) NULL){
			perror(pidfname);
			exit(1);
		}
		flock(fileno(fp), LOCK_EX);
		for (count = 0; count < numlines; count++) {
			if (count != found)
				fprintf(fp, "%d\n", pids[count]);
		}
		flock(fileno(fp), LOCK_UN);
		fclose(fp);
	}
}



/* 	convert a raw file name[s] to the .dvi file name[s] output by TeX
 *	TeX is rather brain-damaged about the way it converts filenames,
 *	it simply finds the first '.' or the end of the string and
 *	concatenates it with DVIEXTENSION.
 *	e.g. chapter.1.tex ==> chapter.dvi
 *	 chapter.2.tex ==> chapter.dvi
 *	if the file name handed this routine is a TeX filename
 *	as denoted by having a TEXEXTENSION, this routine will
 *	create a brain-damaged dvi filename like TeX does.
 *	otherwise, this routine just ensures there is a 
 *	DVIEXTENSION on the file name.
 */

char *
ParseDVIName(nameIn, strLen)
	char *nameIn;
	int strLen;
{
	char dviName[MAXPATHLEN];
	char *noDirFN, *extension;
	
	if ((noDirFN = rindex(nameIn, '/')) == NULL) {
		noDirFN = nameIn;
	} else {
		noDirFN += 1;
	}

	strcpy(dviName, noDirFN);

	if ((extension = rindex(dviName, '.')) != NULL) {
		if (((strcmp(extension, DVIEXTENSION) == 0) || 
		  (strcmp(extension, TEXEXTENSION) == 0))) {
			extension = index(dviName, '.');
			strcpy(extension, DVIEXTENSION);
		}
	} else
		strcat(dviName, DVIEXTENSION);

	if (strlen(dviName) <= strLen - 1) {
		strcpy(nameIn, dviName);
		return(nameIn);
	} else
		return ((char *) NULL);
}

/*
 *	strindex return the position of substr in str, -1 if none
 *	adapted from K&R page 67.
 */

strindex(str, substr)
	char str[], substr[];
{
	register int i, j, k;
	
	for (i = 0; str[i] != '\0'; i++) {
		for (j = i, k = 0; substr[k] != '\0' && str[j] == substr[k]; j++, k++)
			;
		if (substr[k] == '\0')
			return(i);
	}
	return(-1);
}

ttyin()
{
	char buf[BUFSIZ];
	FILE *fopen();
	static FILE *tty = NULL;
	
	if (tty == (FILE *) NULL)
		if ((tty = fopen("/dev/tty", "r")) == (FILE *) NULL) {
			perror("/dev/tty");
			exit(1);
		}
	if (fgets(buf, BUFSIZ, tty) == NULL || buf[0] == 'q')
		exit(0);
	else	/* ordinary line	*/
		return(buf[0]);
}
