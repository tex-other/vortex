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
 *  A Multiple Representation Document Development Environment
 *
 *  This file is part of the VorTeX incremental formatter,
 *
 *  Copyright (C) 1987 by	Ikuo Minakata	(min@renoir.berkeley.edu)
 *  Computer Science Division
 *  University of California, Berkeley
 *
 *  All rights reserved by the author.  See the copyright notice
 *  distributed with this software for a complete description of
 *  the conditions under which it is made available.
 *
 */
#ifndef lint
static char	_rcsid_[] = "$Source:$ for VorTeX Incremental Formatter, Copyright (C) Ikuo Minakata 1987";
static char	_rcsver_[] = "$Revision: $";
static char	_rcsdate_[] = "$Date: $";
#endif !lint

#include	<sys/file.h>
#include	<sys/types.h>
#include	<sys/stat.h>
#include	"tex.h"
#include	"file.h"
#include	"msg.h"
#include	"io.h"
#include	"dvi.h"
#include	"heap.h"
#include	"eq.h"
#include	"eqstack.h"
#include	"cond.h"
#include	"box.h"
#include	"texext.h"
#include	"tfm.h"
#include	"tokenstack.h"
#include	"token.h"
#include	"evalstack.h"
#include	"var.h"
#include	"hash.h"
#include	"main.h"
#include	"macro.h"
#include	"state.h"
#include	"cmds.h"

extern str		str_vtx;
extern str		str_sts;
extern byte_file	sts_file;
extern str		sts_name;
extern short		beg_save_globals;
extern short		end_save_globals;
extern int		show_state;

#define	LOAD		0
#define	SAVE		1

int	max_saved_page = 0;
int	size_globals;

/*
 * save context
 */

save_state()
{
	/* time2 -- beginning of checkpointing;  */
	/* time2 - time1 = processing time excluding checkpointing */

#ifdef TIME
        gettimeofday(&time2, &tz);
        if (total_pages ==0)
		fprintf(stderr,
			"Time to initialize the formatter is %.3f seconds\n",
			(float) ((time2.tv_sec -time1.tv_sec)
		        + (time2.tv_usec - time1.tv_usec) / 1000000.0));
        else {
	        timePer= (float)(time2.tv_sec - time1.tv_sec) +
		    (time2.tv_usec - time1.tv_usec) / 1000000.0;
		  fprintf(stderr,
			  "%.3f seconds to process page %d without checkpointing.\n",
			  timePer, total_pages);
	}
	fflush(stderr);
#endif
	open_state_file(SAVE, total_pages);
	fprintf(stderr, "Saving state of page %d saved in file %s...", total_pages, name_of_file);
	fflush(stderr);

	/* save common global var */
	save_globals();
	/* save eqtb etc */
	save_arrays();
	/* save mem */
	save_mems();
	close_state_file();
/*
	compress_file();
*/
	if (total_pages > max_saved_page)
		max_saved_page = total_pages;
	fprintf(stderr, "done.\n");
}

/*
 * restore context
 */

load_state (no)
	int	no;
{
/*
	uncompress_file(LOAD, no);
*/
	open_state_file(LOAD, no);
	fprintf(stderr, "Loading state of page %d from file %s...\n",
		no, name_of_file);
	fflush(stderr);
	restore_globals();
	restore_arrays();
	restore_mems();
	close_state_file();
/*
	compress_file();
*/
	fprintf(stderr, "done.\n");
}

/*
 *	open state file
 */

open_state_file (md, n)
	int	md;
	int	n;
{

	get_sts_name(md, n);
	if (md == SAVE) {
		if ((sts_file = fopen(name_of_file, "w")) == NULL) {
			msg(STDERR, "can't open state file (%s)", name_of_file);
			exit(-1);
		}
	} else {
		if ((sts_file = fopen(name_of_file, "r")) == NULL) {
			msg(STDERR, "no state file (%s)", name_of_file);
			exit(-1);
		}
	}
	sts_name = b_make_name_string(sts_file);
}

/*
 *	close state file
 */
close_state_file()
{
	b_close(sts_file);
}

get_sts_name (md, n)
	int		md;
	int		n;
{
	char		sts_ext[10];
	char		sts_tmp[FILE_NAME_SIZE + 1];

	if ((md == SAVE) && (access(VORTEX_DIR, F_OK) < 0))
		mkdir(VORTEX_DIR, 0775);
	sprintf(sts_ext, ".%d.%s", n, "sts");
	str_sts = make_string_given(sts_ext);
	if (job_name == 0)
		job_name = str_texput;
	pack_file_name(job_name, str_vtx, str_sts);
}

/*
 *	save context in global variables
 */
save_globals()
{
	unsigned long	bg, ed;

	bg = (unsigned long)&beg_save_globals;
	ed = (unsigned long)&end_save_globals;
	size_globals = ed - bg;
	fwrite(&beg_save_globals, size_globals, 1, sts_file);
	if(show_state)
		fprintf(stderr, "\nsaved global vars: %d bytes", size_globals);
}

/*
 *	restore context in global variables
 */
restore_globals()
{
	int	size;
	unsigned long	bg, ed;

	bg = (unsigned long)&beg_save_globals;
	ed = (unsigned long)&end_save_globals;
	size = ed - bg;
	fread(&beg_save_globals, size, 1, sts_file);
}

/*
 *	save context in separate arrays
 */
save_arrays()
{
	
	/* save eqtb */
	fwrite(&eqtb[0], sizeof(mword)*(EQTB_SIZE+1), 1, sts_file);

	/* save save_stack */
	fwrite(&save_stack[0], sizeof(mword)*(SAVE_SIZE), 1, sts_file);

	/* save hash table */
	fwrite(&hash[0], sizeof(twoh)*(UNDEFINED_CONTROL_SEQUENCE+1), 1, sts_file);

	/* save font_info */
	fwrite(&font_info[0], sizeof(mword)*FONT_MEM_SIZE, 1, sts_file);
}

/*
 *	restore context in separate arrays
 */
restore_arrays()
{
	
	/* restore eqtb */
	fread(&eqtb[0], sizeof(mword)*(EQTB_SIZE+1), 1, sts_file);

	/* restore save_stack */
	fread(&save_stack[0], sizeof(mword)*(SAVE_SIZE), 1, sts_file);

	/* restore hash table */
	fread(&hash[0], sizeof(twoh)*(UNDEFINED_CONTROL_SEQUENCE+1), 1, sts_file);

	/* restore font_info */
	fread(&font_info[0], sizeof(mword)*FONT_MEM_SIZE, 1, sts_file);
}

/*
 *	save some mem structures
 */
save_mems()
{
	/* write all mem[] */
	fwrite(&mem[MEM_MIN], sizeof(mword)*(MEM_MAX-MEM_MIN+1), 1, sts_file);
}

/* 
 *	restore some mem structures
 */
restore_mems()
{
	/* read all mem[] */
	fread(&mem[MEM_MIN], sizeof(mword)*(MEM_MAX-MEM_MIN+1), 1, sts_file);
}

compress_file()
{
	int	pid, wait_result, status;
	char	comp_name[FILE_NAME_SIZE + 1];
	struct	stat	st_buf;

	pid = vfork();
	if (pid == 0) {		/* child process */
		execl("/usr/ucb/compress", "compress", "-f", (char *)name_of_file, 0);
		fprintf(stderr, "unable to invoke compress!");
		exit(1);
	}
	do {
		wait_result = wait(&status);
	} while (wait_result != pid && wait_result != -1);

	if (show_state) {
		sprintf(comp_name, "%s.Z", name_of_file);
		stat(comp_name, &st_buf);
		fprintf(stderr, "\ncompressed file size: %d bytes", st_buf.st_size);
	}
}

uncompress_file(md, n)
	int	md, n;
{
	char	uncomp_name[FILE_NAME_SIZE + 1];
	int	pid, wait_result, status;

	get_sts_name(md, n);
	sprintf(uncomp_name, "%s.%s", name_of_file, "Z");
	if (access(uncomp_name, F_OK) == -1)
		return;
	pid = vfork();
	if (pid == 0) {		/* child process */
		execl("/usr/ucb/uncompress", "uncompress", "-f", uncomp_name, 0);
		fprintf(stderr, "unable to invoke uncompress!");
		exit(1);
	}
	do {
		wait_result = wait(&status);
	} while (wait_result != pid && wait_result != -1);
}

flush_saved_state()
{
	int	i;

	for (i = 0; i <= max_saved_page; i++) {
		get_sts_name(LOAD, i);
		unlink(name_of_file);
		strcat(name_of_file, ".Z");
		unlink(name_of_file);
	}
}

#endif VORTEX
