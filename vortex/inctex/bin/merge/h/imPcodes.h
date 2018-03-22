/*
 * Copyright (c) 1987 University of Maryland Department of Computer Science.
 * All rights reserved.  Permission to copy for any purpose is hereby granted
 * so long as this copyright notice remains intact.
 */

/* imPRESS command codes */

#define	imP_SP		128	/* advance one space */
#define imP_SP1		129	/* advance one space plus 1 pixel */

#define imP_Forw	131	/* one pixel forward */
#define imP_Backw	132	/* one pixel backward */
#define imP_MMove	133	/* move in main advance dir. */
#define imP_SMove	134	/* move in secondary advance dir. */
#define imP_SetHAbs	135	/* set absolute H pos */
#define imP_SetHRel	136	/* set relative H pos */
#define imP_SetVAbs	137	/* set absolute V pos */
#define imP_SetVRel	138	/* set relative V pos */

#define imP_Rule	193	/* print a rule */

#define imP_CRLF	197	/* move to begin. of line */

#define imP_DefGlyph	199	/* define a glyph */
#define imP_DelGlyph	200	/* mark a glyph for deletion */

#define imP_SetHVSystem	205	/* set the H/V coordinate system */
#define imP_SetAdvDirs	206	/* set the advance directions */
#define imP_SetFamily	207	/* use this particular family */
#define imP_SetILSpace	208	/* set the interline spacing */
#define imP_SetBOL	209	/* define the beginning of line */
#define imP_SetSP	210	/* define the space between words */
#define imP_CreateFam	211	/* define a family table */

#define imP_Page	213	/* go to (0,0) */

#define imP_EndPage	219	/* print the current page */

#define imP_ForceDel	240	/* force glyph deletion */
#define imP_EOF		255	/* end of document */
