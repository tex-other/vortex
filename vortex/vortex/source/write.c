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
 *  RCS Info: $Header: write.c,v 0.1 87/05/01 12:33:15 john Locked $
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
 *  write.c - the write-file and write-named-file functions
 */
static char _ID[] = "@(#)write.c for VorTeX, Copyright (c) 1987 John Coker";
 
#include <sys/file.h>
#include "vse.h"
#include "vlisp.h"
#include "buffer.h"
#include "document.h"
#include "gl_comm.h"
#include "ts_comm.h"

/*
 *  DOCUMENTATION
 *
 *  Name: write-unmodified-files
 *  Desc: This variable controls the action taken when the user
 *	attempts to write out an unmodified file-visiting buffer
 *	(with \sym{write-file}).  If this variable is set non-nil,
 *	the file is written even if the buffer isn't modified.
 *	Normally a warning message is printed and the file is not
 *	written.
 *  SeeA: write-file
 */
MKSTRING(WRITEUNMOD_NAME, "write-unmodified-files");

/*
 *  DOCUMENTATION
 *
 *  Name: write-file
 *  Call: (write-file [ 'buffer ])
 *  Retu: size
 *  Desc: This function writes the given buffer out to its
 *	associated file.  Of course, this function can only be
 *	invoked for text file-visiting buffers.  To write a buffer
 *	out to a specified file (not the associated file), see
 *	\sym{write-named-file}.
 *
 *	If the optional second argument is given it should specify
 *	a buffer other than the current one to be written out.
 *
 *	If the buffer has not been modified since the last write
 *	(presumably is not out of symc with the version on disk)
 *	a warning message is printed and the file is not written
 *	out.  However, this is not an error.  The variable
 *	\sym{write-unmodified-files} controls this action.
 *
 *	The number of bytes written (the length of the buffer) is
 *	returned as a fixnum or nil if the buffer was not written
 *	out.
 *  Side: When a buffer is written out, the modification flag is
 *	cleared--the buffer is no longer out of sync with the file
 *	on disk.
 *  SeeA: find-file write-named-file write-unmodified-files
 */

DEFUN(dowritefile, "write-file", FLAG_NONE, "")
{
	struct value	arg, var, ret;
	struct buffer	*bufp = current_buffer;
	struct source	*srcp;
	char		file[1025];
	int		size;

	CHECKAC(0, 1);
	if (GETACOUNT() > 0) {
		arg = EVALARGN(1);
		if (!stringp(arg))
			BADARGN(1, "a string buffer name");
		bufp = buffer_get(gstring(arg.vl_data), TRUE);
	}
	ASSERT(bufp != NULL);

	/* write out the file to the buffer */
	if (bufp->bu_type != BUFF_SOURCE)
		error("Can't write out this kind of buffer!");
	srcp = bufp->bu_sdata;
	ASSERT(srcp != NULL);
	if (srcp->sb_file == NULL) {
		error("Can't write %Y; it isn't a file-visiting buffer.",
		      bufp->bu_name);
	}
	var = get_variable(WRITEUNMOD_NAME, bufp);
	if (!truep(var) && (bufp->bu_flags & BUFF_MODIFIED) == 0) {
		/* be an asshole for a moment ... */
		message("(buffer %Y not modified, file not written)",
			bufp->bu_name);
		return (v_nil);
	}
	makecstring(srcp->sb_file, file, sizeof (file));
	size = write_buffer(bufp, file);

	/* mark buffer as unmodified and needing a new mode line */
	bufp->bu_flags &= ~BUFF_MODIFIED;
	bufp->bu_flags |= BUFF_NEWMODE;

	ret.vl_type = LISP_FIXNUM;
	sfixnum(ret.vl_data, size);
	return (ret);
}

/*
 *  DOCUMENTATION
 *
 *  Name: confirm-overwrite-changed-file
 *  Desc: This variable controls the action taken when a buffer is
 *	to be written out and the file has changed since the last
 *	write.  If set, the user must confirm overwriting the changed
 *	file, otherwise the file is happily overwritten.
 *  SeeA: write-file
 */
MKSTRING(OVERWRITE_NAME, "confirm-overwrite-changed-file");

/*
 *  DOCUMENTATION
 *
 *  Name: write-named-file
 *  Call: (write-named-file 'file [ 'buffer [ 'nochange ] ])
 *  Retu: size
 *  Desc: This function writes the given buffer out to the specified
 *	file.  This function can only be invoked for text buffers.
 *	The specified file name becomes the associated file for the
 *	buffer in question.
 *
 *	If the second argument is given, it specifies the name of
 *	the buffer to write instead of the current buffer.
 *
 *	If the optional third argument is given and evaluates non-nil,
 *	the buffer's associated file is not changed.  This would be
 *	useful for making a copy of the buffer in another file.
 *
 *	The number of bytes written to the file (the buffer size)
 *	is returned.
 *  Side: The modified flag for the buffer is cleared unless the
 *	nochange argument is given non-nil.
 *  SeeA: write-file find-file
 */

DEFUN(dowritenamed, "write-named-file", FLAG_NONE, "fWrite to file: ")
{
	struct value	arg, ret;
	struct buffer	*bufp = current_buffer;
	struct source	*srcp;
	struct string	*file;
	char		pbuf[1025], *path;
	int		size, nochange = FALSE;

	CHECKAC(1, 3);
	arg = EVALARGN(1);
	if (!stringp(arg))
		BADARGN(1, "a string file name");
	file = gstring(arg.vl_data);
	if (GETACOUNT() > 1) {
		arg = EVALARGN(2);
		if (!stringp(arg))
			BADARGN(2, "a string buffer name");
		bufp = buffer_get(gstring(arg.vl_data), TRUE);
	}
	if (GETACOUNT() > 2) {
		arg = EVALARGN(3);
		if (truep(arg))
			nochange = TRUE;
	}

	/* write out the file to the buffer */
	if (bufp->bu_type != BUFF_SOURCE)
		error("Can't write out this kind of buffer!");
	srcp = bufp->bu_sdata;
	ASSERT(srcp != NULL);

	/* get full pathname of file and write it out */
	makecstring(file, pbuf, sizeof (pbuf));
	path = (char *)fixpath(pbuf);
	size = write_buffer(bufp, path);

	/* change buffer file name if appropriate */
	if (!nochange) {
		/* set the attributes now for this new file */
		srcp->sb_file = save_string(path, strlen(path));
		if ((bufp->bu_flags & BUFF_MODIFIED) != 0) {
			bufp->bu_flags &= ~BUFF_MODIFIED;
			bufp->bu_flags |= BUFF_NEWMODE;
		}
	}

	ret.vl_type = LISP_FIXNUM;
	sfixnum(ret.vl_data, size);
	return (ret);
}

#define UMASK		0666	/* modified by user's umask(2) */
#define OUTBUFSIZE	4096	/* must be larger than TBLOCKSIZE */

write_buffer(bufp, file)
	struct buffer	*bufp;
	char		*file;
{
	register struct tblock	*tbp;
	struct source		*srcp;
	register unsigned long	*txp, *tend;
	unsigned char		buf[OUTBUFSIZE], *bend = buf + OUTBUFSIZE;
	register unsigned char	*bp;
	register int		fd, size;

	/* make sure this is a source editor buffer */
	if (bufp->bu_type != BUFF_SOURCE)
		error("Non-source buffer to write as a text file!");
	srcp = bufp->bu_sdata;
	ASSERT(srcp != NULL);

	/* try to open the file */
	if (file == NULL || *file == '\0')
		ierror("Illegal file name to write source buffer!");
	if ((fd = open(file, O_WRONLY|O_CREAT|O_TRUNC, UMASK)) < 0)
		perror("Can't write to source file %s", file);
	lseek(fd, 0L, 0);

	/* write entire buffer in OUTBUFSIZE chunks */
	size = 0;
	bp = buf;
	for (tbp = srcp->sb_text; tbp != NULL; tbp = tbp->tb_next) {
		tend = tbp->tb_text + tbp->tb_length;
		for (txp = tbp->tb_text; txp < tend; txp++) {
			if (bp + 1 >= bend) {
				/* flush the buffer */
				if (write(fd, buf, bp - buf) != bp - buf)
					goto oerror;
				size += (bp - buf);
				bp = buf;
			}
			*bp++ = charof(*txp);
		}
	}
	if (bp > buf) {
		if (write(fd, buf, bp - buf) != bp - buf) {
oerror:			close(fd);
			perror("Write error to source file %s", file);
		}
		size += (bp - buf);
	}
	close(fd);
	message("(wrote %d byte%s to \"%s\")", size, PLURAL(size), file);

	return (size);
}

send_buffer(bufp, fd)
	struct buffer	*bufp;
{
	register struct tblock	*tbp;
	struct source		*srcp;
	register unsigned long	*txp, *tend;
	unsigned long		buf[OUTBUFSIZE], *bend = buf + OUTBUFSIZE;
	register unsigned long	*bp;
	register int		size, c, len;
	register int		fID;
	int			total;

	/* make sure this is a source editor buffer */
	if (bufp->bu_type != BUFF_SOURCE)
		error("Non-source buffer to send to formatter!");
	srcp = bufp->bu_sdata;
	ASSERT(srcp != NULL);

	/* get the file ID to send */
	fID = srcp->sb_fileID;
	debug(DITEX, "Sending buffer %S (TeX ID %d) to formatter.",
	      bufp->bu_name, fID);

	/* send the packet header */
	total = srcp->sb_length * sizeof (buf[0]);
	sendformat(TSC_OPENFILE, fID, total, NULL);

	/* write entire buffer in OUTBUFSIZE chunks */
	size = 0;
	bp = buf;
	for (tbp = srcp->sb_text; tbp != NULL; tbp = tbp->tb_next) {
		tend = tbp->tb_text + tbp->tb_length;
		for (txp = tbp->tb_text; txp < tend; txp++) {
			if (bp + 1 >= bend) {
				/* flush the buffer */
				len = (bp - buf) * sizeof (buf[0]);
				if (write(fd, buf, len) != len)
					goto oerror;
				size += len;
				bp = buf;
			}
			*bp++ = *txp;
		}
	}
	if (bp > buf) {
		len = (bp - buf) * sizeof (buf[0]);
		if (write(fd, buf, len) != len)
oerror:			perror("Write error on socket %d to formatter", fd);
		size += len;
	}

	/* make sure we didn't screw up communications */
	if (size != total) {
		/* should do something intelligent here */
		ierror("We sent %d bytes instead of %d from buffer %S!",
		       size, total, bufp->bu_name);
	}
	message("(sent %d byte%s to formatter)", size, PLURAL(size));

	return (size);
}
