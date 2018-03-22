/* Copyright (c) 1992 Univ. of California, Berkeley, Dept. of Computer Science,
 * Derluen Pan.
 *
 * ...and also...
 * Copyright (c) 1987 University of Maryland Department of Computer Science.
 * All rights reserved.  Permission to copy for any purpose is hereby granted
 * so long as this copyright notice remains intact.
 *
 * Incremental TeX DVI file combination program:
 * IncTeX produces a dvi file, INC/<doc>.dvi, which has the new pages.
 * INC/<doc>.newpages is a text file which gives the new pages which
 * have been produced.  This program combines the new pages with the
 * previous pages in the complete dvi file in <doc>.dvi to produce a
 * updated complete dvi file.
 * The format in <doc>.newpages is:
 * a set of in-sequence new page ranges of 2 forms:
 *  l:r			(range goes from page #l to #r
 *  l,			(range is only 1 page)
 * followed by -1 for end of range list
 * followed by the number of total pages in the new document
 *
 * This program is adapted from Univ of Maryland's dviselect --
 * (DVI page selection program)
 * ..stripped out page selection
 * ..handle repeated font def's
 * ..handle fonts at diff magnification
 */

#include "types.h"
#include "dvi.h"
#include "dviclass.h"
#include "dvicodes.h"
#include "fio.h"
#include <stdio.h>
#include <ctype.h>

#define TRUE 1
#define FALSE 0

char  *ProgName;
extern int   errno;
extern char *optarg;
extern int   optind;

/* Globals */

int Trace = 0;
char *CurFile;
int TweakNum = 0,
    TweakDenom = 0,
    TweakMag = 0;


char	serrbuf[BUFSIZ];	/* buffer for stderr */

/*
 * We will try to keep output lines shorter than MAXCOL characters.
 */
#define MAXCOL	75

/*
 * We use the following structure to keep track of fonts we have seen.
 * The final DVI file lists only the fonts it uses.
 */
struct fontinfo {
	i32	fi_newindex;	/* font number in output file */
	int	fi_reallyused;	/* true => used on a page we copied */
	i32	fi_checksum;	/* the checksum */
	i32	fi_mag;		/* the magnification */
	i32	fi_designsize;	/* the design size */
	short	fi_n1;		/* the name header length */
	short	fi_n2;		/* the name body length */
	char	*fi_name;	/* the name itself */
};

int	SFlag;			/* true => -s, silent operation */

i32	NextOutputFontIndex;	/* generates output indicies */
i32	CurrentFontIndex;	/* current (old) index in input */
i32	OutputFontIndex;	/* current (new) index in ouput */

FILE	*inf;			/* the current input DVI file, one of next 2: */
FILE	*new;			/* the new changed pages */
FILE	*full;			/* the old complete set of pages */
FILE	*pglist;		/* file with list of page changes */
FILE	*outf;			/* the output DVI file */

#define MaxInFiles 3
char *datafiles[MaxInFiles];	/* input file names */
#define PageListFile datafiles[0]
#define NewPageFile datafiles[1]
#define OldCompleteFile datafiles[2]

int	*PageInFile;		/* current page # in dvi file, one of next 2: */
int	PageInNew;		/* this jumps from range to range */
int	PageInFull;		/* this runs continuously */
int	CopyingFull;		/* true if copying from FILE *full */
int	GotOldPostAmble;	/* true if postamble read from FILE *full */
int	TotalPagesToMake;
char	*blatnew = "*";
char	*blatold = "+";

int	RangeStart;
int	RangeEnd;
int	OldEnd;
char	RangeChar;

int	ExpectBOP;		/* true => BOP ok */
int	ExpectEOP;		/* true => EOP ok */

long	StartOfLastPage;	/* The file position just before we started
				   the last page (this is later written to
				   the output file as the previous page
				   pointer). */
long	CurrentPosition;	/* The current position of the file */

int	UseThisPage;		/* true => current page is selected */

i32	InputPageNumber;	/* current absolute page in old DVI file */
int	NumberOfOutputPages;	/* number of pages in new DVI file */

i32	Numerator;		/* numerator from DVI file */
i32	Denominator;		/* denominator from DVI file */
i32	DVIMag;			/* magnification from DVI file */

i32	Count[10];		/* the 10 \count variables */

/* save some string space: we use this a lot */
char	writeerr[] = "error writing DVI file";

char	*malloc(), *realloc(), *sprintf();

/*
 * lint gets rather confused with the current definitions of getc and putc,
 * so we redefine them here (#if lint).  This should really be in the
 * standard I/O library, but I am not about to go change it now!
 */
#ifdef lint
#undef putc
#undef getc
#define putc(c,f) (*(f)->_ptr++ = (unsigned) (c))
#define getc(f)   (*(f)->_ptr++)
#endif


#define HASHSIZE 257

/* hash table to manage font info keyed by filename and index */
static struct hash_entry_s {
	char *file;
	i32 input, output; /* input and output font indices */
	struct fontinfo *fi;
	struct hash_entry_s *chain;
} *hashtable[ HASHSIZE ];

/* hash table to manage font names */
static struct fname_entry_s {
	char *name;
	struct fontinfo *fi;
	struct fname_entry_s *chain;
} *fontnames[ HASHSIZE ];
	
/* hash alg. due to P.J. Weinberger. */
static int
hash(str)
	char *str;
{
	unsigned int r;

	for (r = 0; *str != NULL; ++str) {
		r = (r << 4) + *str;
		if (r > 0x0fffffff) {
			r ^= (r >> 24) & 0xf0;
			r &= 0x0fffffff;
		}
	}
	return r % HASHSIZE;
}

#define NEW(type) ((type *)mymalloc((unsigned)sizeof(type)))

static char *
mymalloc(size)
	unsigned size;
{
	char *new;

	if ((new = malloc(size)) == (char *)NULL)
		error(1, 0, "out of memory");
	return new;
}

/* Return the hash entry for file and index, creating a new one if
 * necessary. */
static struct hash_entry_s *
fontByIndex(file, index, newp)
	char *file;
	i32 index;
	int *newp;
{
	int hv;
	struct hash_entry_s *entry;
	extern long random();

	srandom(index);
	hv = (hash(file)^random()) % HASHSIZE;
	entry = hashtable[hv];
	while (entry) {
		if (entry->file == file && entry->input == index) {
			*newp = 0;
			return entry;
		}
		entry = entry->chain;
	}
	*newp = 1;
	entry = NEW(struct hash_entry_s);
	entry->chain = hashtable[hv];
	hashtable[hv] = entry;
	entry->file = file;
	entry->input = index;
	entry->output = -1;
	entry->fi = (struct fontinfo *)NULL;
	return entry;
}

/* Return the fontinfo struct for font name */
static struct fontinfo *
fontByName(name, newp)
	char *name;
	int *newp;
{
	int hv;
	struct fname_entry_s *entry;
	static int newindices = 0;

	hv = hash(name);
	entry = fontnames[hv];
	while (entry) {
		if (strcmp(entry->name, name) == 0) {
			*newp = 0;
			return entry->fi;
		}
		entry = entry->chain;
	}
	*newp = 1;
	entry = NEW(struct fname_entry_s);
	entry->chain = fontnames[hv];
	fontnames[hv] = entry;
	(void)strcpy(entry->name = mymalloc((unsigned)strlen(name) + 1),
		     name);
	entry->fi = NEW(struct fontinfo);
	entry->fi->fi_newindex = newindices++;
	entry->fi->fi_name = entry->name;
	return entry->fi;
}

static void fontEnumerate(func)
	void (*func)();
{
	int i;
	struct fname_entry_s *entry;

	for (i = 0; i < HASHSIZE; i++) {
		if (entry = fontnames[i])
		    do {
			(*func)(entry->fi);
			entry = entry->chain;
		    } while (entry);
	}
}
			
/*
 * Print a message to stderr, with an optional leading space, and handling
 * long line wraps.
 */
message(space, str, len)
	int space;
	register char *str;
	register int len;
{
	static int beenhere;
	static int col;

	if (!beenhere)
		space = 0, beenhere++;
	if (len == 0)
		len = strlen(str);
	col += len;
	if (space) {
		if (col >= MAXCOL)
			(void) putc('\n', stderr), col = len;
		else
			(void) putc(' ', stderr), col++;
	}
	while (--len >= 0)
		(void) putc(*str++, stderr);
	(void) fflush(stderr);
}
 
/* call this function to decide if the current page in the dvi file
 * should be copied into the output file.  It looks at a flag to see
 * if we are copying from the old complete file or the one with the new
 * page changes.  If the former, we skip pages which are to be replaced
 * by new ones.  There is a current page variable which tells us which
 * logical page we are on in each dvi page.  NumberOfOutputPages is the
 * logical output page, which can be ahead of the page in the old complete
 * file after we have copied in a range of new files.
 */
int CopyThisPage()
{
	if (CopyingFull)	/* NumberOfOutputPages+1...RangeStart-1 */
		if (PageInFull <= NumberOfOutputPages)
		    return(FALSE);	/* skip until we catch up */
		else {
		    if (PageInFull > OldEnd) fprintf(stderr,
		    	"Warning,copies from DVI overrunning @ page %d\n",
			PageInFull);
		    return(TRUE);
		}
	else
		return(TRUE);		/* copy all pages from new file */
}

/*
 * Start a page (process a DVI_BOP).
 */
BeginPage()
{
	register i32 *i;

	if (!ExpectBOP)
		GripeUnexpectedOp("BOP");
	ExpectBOP = 0;
	ExpectEOP++;		/* set the new "expect" state */

	OutputFontIndex = -1;	/* new page requires respecifying font */
	(*PageInFile)++;
	for (i = Count; i < &Count[10]; i++)
		fGetLong(inf, *i);
	(void) GetLong(inf);	/* previous page pointer */

	UseThisPage = CopyThisPage();
	if (!UseThisPage)
		return;

	(void) putc(DVI_BOP, outf);
	for (i = Count; i < &Count[10]; i++)
		PutLong(outf, *i);
	PutLong(outf, StartOfLastPage);
	if (ferror(outf))
		error(1, errno, writeerr);

	StartOfLastPage = CurrentPosition;
	CurrentPosition += 45;	/* we just wrote this much */

	if (!SFlag) {		/* write nice page usage messages */
		char msg[10];

		(void) sprintf(msg, " %d", *PageInFile);
		message(1, msg, 0);
	}
}

/*
 * End a page (process a DVI_EOP).
 */
EndPage()
{
	if (!ExpectEOP)
		GripeUnexpectedOp("EOP");
	ExpectEOP = 0;
	ExpectBOP++;

	if (CopyingFull) {
		if (PageInFull >= OldEnd) {
		    inf = new;		/* switch over to new pages */
		    CopyingFull = FALSE;
		    PageInFile = &PageInNew;
		    PageInNew = RangeStart-1;	/* reset virtual page # */
		    CurFile = NewPageFile;
		    if (!SFlag)
	  		message( 1, blatnew, 1 );
		}
	} else if (PageInNew >= RangeEnd) {
		    if (RangeEnd < 1)
			RangeStart = -1;	/* just copied the last pages */
		    else {
		    	fscanf( pglist, "%d", &RangeStart );
		    	if (RangeStart < 1) 	/* done? */
		    	     fscanf( pglist, "%d", &TotalPagesToMake );
		    }
		    if (RangeStart < 1) {	/* done? */
			RangeEnd = RangeStart;
			OldEnd = TotalPagesToMake;
		    	inf = full;		/* switch over to old pages */
			if (PageInNew >= TotalPagesToMake) {
		    	     CopyingFull = TRUE;
		             CurFile = OldCompleteFile;
		             PageInFile = &PageInFull;
			     FindPostAmble();	/* ...and get postamble info */
			     GotOldPostAmble = TRUE;
		    	     CopyingFull = FALSE;
		    	     CurFile = NewPageFile;
		    	     PageInFile = &PageInNew;
			     inf = new;		/* handle new's postamble */
			} else {		/* else get last pages */
			     RangeStart = TotalPagesToMake+1;
		             CopyingFull = TRUE;
		             PageInFile = &PageInFull;
		             CurFile = OldCompleteFile;
		             if (!SFlag)
	  		         message( 1, blatold, 1 );
			}
		    } else {
		        fscanf( pglist, "%c", &RangeChar );
		        if (RangeChar == ',')
			    RangeEnd = RangeStart;
		        else
			    fscanf( pglist, "%d", &RangeEnd );
			OldEnd = RangeStart-1;
		    
		        inf = full;		/* switch over to old pages */
		        CopyingFull = TRUE;
		        PageInFile = &PageInFull;
		        CurFile = OldCompleteFile;
		        if (!SFlag)
	  		    message( 1, blatold, 1 );
		    }
	}

	if (!UseThisPage)
		return;

	putc(DVI_EOP, outf);
	if (ferror(outf))
		error(1, errno, writeerr);
	CurrentPosition++;
	NumberOfOutputPages++;
}

/*
 * For each of the fonts used in the new DVI file, write out a definition.
 */
void
PostAmbleFontEnumerator(fi)
	struct fontinfo *fi;
{
	if (fi->fi_reallyused) {
		WriteFont(fi);
		if (Trace)
			(void)fprintf(stderr, "font %d, '%s'\n",
				      fi->fi_newindex, fi->fi_name);
	}
}

/*	define maximum fn	*/
#define max(A,B) ( (B>A) ? B : A )

i32	MaxPgHt = 0,	/* max page height */
	MaxPgWd = 0,	/* max page width */
	MaxStak = 0;	/* max stack size */

HandlePostAmble()
{
	register i32 c;

	(void) GetLong(inf);	/* previous page pointer */
	if (GetLong(inf) != Numerator)
		if (!TweakNum) GripeMismatchedValue("numerator");
	if (GetLong(inf) != Denominator)
		if (!TweakDenom) GripeMismatchedValue("denominator");
	if (GetLong(inf) != DVIMag)
		if (!TweakMag) GripeMismatchedValue("\\magfactor");

	c = GetLong(inf); MaxPgHt = max(MaxPgHt,c);	/* tallest pg height */
	c = GetLong(inf); MaxPgWd = max(MaxPgWd,c);	/* widest page width */
	c = GetWord(inf); MaxStak = max(MaxStak,c);	/* DVI stack size */

#ifdef notdef
	(void) GetWord(inf);	/* skip original number of pages */
#endif
	/*
	 * just ignore all the incoming font definitions; we are done with
	 * input file 
	 */

return;
}

/* Prints out cumulative postamble */
PrintPostAmble()
{
	if (Trace) fprintf(stderr, "Writing postamble\n");

	putc(DVI_POST, outf);
	PutLong(outf, StartOfLastPage);
	PutLong(outf, Numerator);
	PutLong(outf, Denominator);
	PutLong(outf, DVIMag);
	PutLong(outf, MaxPgHt);
	PutLong(outf, MaxPgWd);
	PutWord(outf, MaxStak);
	PutWord(outf, NumberOfOutputPages);

	StartOfLastPage = CurrentPosition;	/* point at post */
	CurrentPosition += 29;	/* count all those `put's */

	/*
	 * run through the font table and dump definitions for the
	 * fonts we have used. 
	 */
	fontEnumerate(PostAmbleFontEnumerator);

	putc(DVI_POSTPOST, outf);
	PutLong(outf, StartOfLastPage);	/* actually start of postamble */
	putc(DVI_VERSION, outf);
	putc(DVI_FILLER, outf);
	putc(DVI_FILLER, outf);
	putc(DVI_FILLER, outf);
	putc(DVI_FILLER, outf);
	CurrentPosition += 10;
	while (CurrentPosition & 3)
		putc(DVI_FILLER, outf), CurrentPosition++;
	if (ferror(outf))
		error(1, errno, writeerr);
}

FindPostAmble()
{
	register int c;
	long offset,
	     curr_loc;

	fseek( inf, 0L /* to */, 2 /* end of file */);
	c = fgetc( inf );
	do {
		fseek( inf, -2L /* away from */, 1 /* last loc + 1 */);
		c = fgetc( inf );
	} while (c != DVI_VERSION);
	fseek( inf, -5L /* away from */, 1 /* last+1 */);
	(long) fGetLong( inf, offset );	/* 4 bytes */
	curr_loc = ftell( inf );
	fseek( inf, (offset-curr_loc) /* away from */, 1 /* current */);
	c = fgetc( inf );
	if (c != DVI_POST) {
		fprintf(stderr," Oops, no POST code when expected in %s!\n",
			CurFile);
		return;
	}
	(void) GetLong(inf);	/* previous page pointer */
	c = GetLong(inf);
	if (Trace) fprintf(stderr,"Numerator :%i\n",c);
	c = GetLong(inf);
	if (Trace) fprintf(stderr,"Denominator :%i\n",c);
	c = GetLong(inf);
	if (Trace) fprintf(stderr,"Magnification :%i\n",c);

	c = GetLong(inf); MaxPgHt = max(MaxPgHt,c);	/* tallest pg height */
	if (Trace) fprintf(stderr,"Max Pg Ht :%i\n",c);
	c = GetLong(inf); MaxPgWd = max(MaxPgWd,c);	/* widest page width */
	if (Trace) fprintf(stderr,"Max Pg Wd :%i\n",c);
	c = GetWord(inf); MaxStak = max(MaxStak,c);	/* DVI stack size */
	if (Trace) fprintf(stderr,"Max Stack :%i\n",c);
}

/*
 * Write a font definition to the output file
 */
WriteFont(fi)
	register struct fontinfo *fi;
{
	register int l;
	register char *s;

	if (fi->fi_newindex < 256) {
		putc(DVI_FNTDEF1, outf);
		putc(fi->fi_newindex, outf);
		CurrentPosition += 2;
	} else if (fi->fi_newindex < 65536) {
		putc(DVI_FNTDEF2, outf);
		PutWord(outf, fi->fi_newindex);
		CurrentPosition += 3;
	} else if (fi->fi_newindex < 16777216) {
		putc(DVI_FNTDEF3, outf);
		Put3Byte(outf, fi->fi_newindex);
		CurrentPosition += 4;
	} else {
		putc(DVI_FNTDEF4, outf);
		PutLong(outf, fi->fi_newindex);
		CurrentPosition += 5;
	}
	PutLong(outf, fi->fi_checksum);
	PutLong(outf, fi->fi_mag);
	PutLong(outf, fi->fi_designsize);
	putc(fi->fi_n1, outf);
	putc(fi->fi_n2, outf);
	l = fi->fi_n1 + fi->fi_n2;
	CurrentPosition += 14 + l;
	s = fi->fi_name;
	while (--l >= 0)
		putc(*s, outf), s++;
}

/*
 * Handle the preamble.  Someday we should update the comment field.
 * Print only the first, check that other DVI files' preambles have the same
 * numerator and denominator and magnification.
 */

#define cmpPre( Z, ZNAME ) if (FirstAmble) Z = GetLong(inf); \
   else {	i = GetLong(inf); \
	if (Z != i) {fprintf(stderr, \
	    "\nError, %s in file %s preamble is %d, in previous was %d\n", \
		ZNAME, FileName, i, Z); \
	    (void) fflush(stderr); \
	} \
   };
			
					
HandlePreAmble( FirstAmble, FileName )
int FirstAmble;		/* True if 1st DVI file's preamble */
char *FileName;		/* current file name */
{
	register int n, c;
	i32 i;

	if (GetByte(inf) != Sign8(DVI_PRE))
		GripeMissingOp("PRE");
	if (GetByte(inf) != Sign8(DVI_VERSION))
		GripeMismatchedValue("DVI version number");
	/* now check these values are consistent */
	cmpPre(Numerator,   "scaling numerator");
	cmpPre(Denominator, "scaling denominator");
	cmpPre(DVIMag,      "printing magnification");
	if (!FirstAmble) {		/* skip writing preamble */
		n = UnSign8(GetByte(inf));
		while (--n >= 0)  c = GetByte(inf);
		return;
	};
/* 		Originally it was just this
	Numerator = GetLong(inf);
	Denominator = GetLong(inf);
	DVIMag = GetLong(inf);
*/
	if (TweakNum) Numerator *= 2;
	if (TweakDenom) Denominator *= 2;
	if (TweakMag) DVIMag *= 2;

	putc(DVI_PRE, outf);
	putc(DVI_VERSION, outf);
	PutLong(outf, Numerator);
	PutLong(outf, Denominator);
	PutLong(outf, DVIMag);

	n = UnSign8(GetByte(inf));
	CurrentPosition = 15 + n;	/* well, almost */
	putc(n, outf);
	while (--n >= 0) {
		c = GetByte(inf);
		putc(c, outf);	/* never trust a macro, I always say */
	}
}

/* read pglist file (open already), return true if every page is in NewPageFile.
 */
AllPagesNew()
{
	RangeEnd = 0;
	fscanf( pglist, "%d", &RangeStart );	/*pick up 1st range*/
	if (RangeStart != 1)
		return(FALSE);
	/* now pick up end of ranges and keep going while consecutive */
	while (RangeStart>0 && RangeStart==(RangeEnd+1)) {
	    fscanf( pglist, "%c", &RangeChar );
	    if (RangeChar == ',')
		RangeEnd = RangeStart;
	    else
		fscanf( pglist, "%d", &RangeEnd );
	    fscanf( pglist, "%d", &RangeStart );
	}
	if (RangeStart>0) {
		rewind( pglist );
		return(FALSE);	/* must have seen a page gap */
	}
	/* otherwise see if RangeEnd = Total Pages */
	fscanf( pglist, "%d", &TotalPagesToMake );
	rewind( pglist );	/* reset read position in file */
	return(TotalPagesToMake == RangeEnd);
}

main(argc, argv)
	int argc;
	register char **argv;
{
	register int c;
	char *inname = NULL, *outname = NULL;
	int LastDataFile = 0;
	int i;
	int badnews = 0;

	ProgName = *argv;
	setbuf(stderr, serrbuf);

	while ((c = getopt(argc, argv, "o:stndm")) != EOF) {
		switch (c) {

		case 's':
			SFlag++;	/*silent mode*/
			break;

		case 't':
			Trace++;	/*trace mode*/
			break;

		case 'n':
			TweakNum++;	/*tweak numerator value*/
			break;

		case 'd':
			TweakDenom++;	/*tweak denominator value*/
			break;

		case 'm':
			TweakMag++;	/*tweak magnif value*/
			break;

		case 'o':
			if (outname != NULL)
				goto usage;
			outname = optarg;
			break;

		case '?':
usage:
			fprintf(stderr, "\
Usage: %s [-s] [-t] [-o outfile] pagelistfile newpages oldpages \n",
				ProgName);
			(void) fflush(stderr);
			exit(1);
		}
	}

	LastDataFile = -1;
	if (Trace) fprintf(stderr,"Input Files:\n");
	while (optind < argc) {
	  if (argv[optind] != NULL) {
	    if (LastDataFile >= 2) {
		fprintf(stderr,"Too many input files (more than 3)\n");
		exit(1);
	    } else
	   	datafiles[++LastDataFile] = argv[optind++];
	    if (Trace) fprintf(stderr,"%s\n",datafiles[LastDataFile]);
	  } else
	    optind++;
	}
	if (LastDataFile<2) {
	  	fprintf(stderr,"Number of input files < 3!\n");
		goto usage;
	}

	if (Trace) fprintf(stderr, "Opening %s\n", PageListFile);
	if ((pglist = fopen(PageListFile, "r")) == 0) {
		badnews = 1;
		fprintf(stderr,"Cannot read file %s\n", PageListFile);
		(void) fflush(stderr);
		exit();
	}
	if (AllPagesNew()) {	/* great, just move file and we're done */
		if (rename( NewPageFile, outname ) !=0) {
			/*fprintf(stderr,"Error! Couldn't rename %s to %s\n",
				NewPageFile, outname);*/
			error(1,errno, "couldn't rename %s to %s\n",
				NewPageFile, outname);
		} else
			fprintf(stderr,"%s renamed to %s\n",
				NewPageFile, outname);
		exit(0);
	}
	fscanf( pglist, "%d", &RangeStart );
	if (RangeStart < 1) {
		if (Trace) fprintf(stderr, "No new pages, no work to do\n");
		exit();
	}
	fscanf( pglist, "%c", &RangeChar );
	if (RangeChar == ',')
		RangeEnd = RangeStart;
	else
		fscanf( pglist, "%d", &RangeEnd );
	OldEnd = RangeStart-1;

	StartOfLastPage = -1;
	if (Trace) fprintf(stderr, "Opening %s\n", NewPageFile);
	if ((new = fopen(NewPageFile, "r")) == 0) {
		badnews = 1;
		fprintf(stderr,"\nCannot read file %s\n", NewPageFile);
		(void) fflush(stderr);
		exit();
	}
	if (Trace) fprintf(stderr, "Opening %s\n", OldCompleteFile);
	if ((full = fopen(OldCompleteFile, "r")) == 0) {
		badnews = 1;
		fprintf(stderr,"\nCannot read file %s\n", OldCompleteFile);
		(void) fflush(stderr);
		exit();
	}

	ExpectBOP = 1;

	if (outname == NULL) {
	  	fprintf(stderr, "Warning, dvi output going to stdout!!\n");
		outf = stdout;
	} else if ((outf = fopen(outname, "w")) == 0)
		error(1, errno, "cannot write %s", outname);

	inf = new;
	PageInNew = 0;	/* this jumps from range to range */
	PageInFile = &PageInNew;
	HandlePreAmble( 1 /* first DVI file */, NewPageFile );
	inf = full;
	PageInFull = 0;	/* this runs continuously */
	PageInFile = &PageInFull;
	HandlePreAmble( 0 /* not first DVI file */, OldCompleteFile );

	NumberOfOutputPages = 0;
	CopyingFull = NumberOfOutputPages+1 /* next page */ < RangeStart;
	if (CopyingFull) {
		inf = full;
		PageInFile = &PageInFull;
		CurFile = OldCompleteFile;
		if (!SFlag)
	  	    message( 1, blatold, 1 );
	} else {
		inf = new;
		PageInFile = &PageInNew;
		CurFile = NewPageFile;
		if (!SFlag)
	  	    message( 1, blatnew, 1 );
	}

	GotOldPostAmble = FALSE;

	HandleDVIFile();
	if (!GotOldPostAmble) {
		CopyingFull = TRUE;
		CurFile = OldCompleteFile;
		PageInFile = &PageInFull;
		inf = full;		/* handle old's postamble */
		FindPostAmble();	/* ...and get postamble info */
		GotOldPostAmble = TRUE;
		CopyingFull = FALSE;
		CurFile = NewPageFile;
		PageInFile = &PageInNew;
		inf = new;		/* handle new's postamble */
	}
	HandlePostAmble();

	fclose(inf);

	PrintPostAmble();
	fclose(outf);

	if (!SFlag) {	fprintf(stderr, "\nWrote %d pages, %d bytes\n",
			NumberOfOutputPages, CurrentPosition);
			fprintf(stderr, "Merge finished\n");
	};
	(void) fflush(stderr);
	if (badnews)
		exit(1);
	else
		exit(0);
}


/*
 * Handle a font definition.
 */
#define cmfontinfo(z) if (fi->z != fptr->z) fprintf(stderr, \
	"Font %d z was %d, is %d, file %s\n", index, fi->z, fptr->z, CurFile)

HandleFontDef(index)
	i32 index;
{
	register int i;
	register char *s;
	char buf[ 512 ];
	int newname, newentry;
	struct fontinfo cur, *fi;
	struct hash_entry_s *entry;

	if (NextOutputFontIndex <= index) NextOutputFontIndex = index + 1;

	cur.fi_reallyused = 0;
	cur.fi_checksum = GetLong(inf);
	cur.fi_mag = GetLong(inf);
	cur.fi_designsize = GetLong(inf);
	cur.fi_n1 = UnSign8(GetByte(inf));
	cur.fi_n2 = UnSign8(GetByte(inf));
	i = cur.fi_n1 + cur.fi_n2;
	for (s = buf; --i >= 0;  *s++ = GetByte(inf));
	/* *s = '\0'; */
	sprintf(s,".%d",cur.fi_mag);	/* append magnif to name */

	fi = fontByName(buf, &newname);
	entry = fontByIndex(CurFile, index, &newentry);
	entry->fi = fi;
	entry->output = fi->fi_newindex;
	cur.fi_name = fi->fi_name;

	if (newname) {
		/* new font */
		if (Trace) (void)fprintf(stderr, "new font %s (%d)\n",
					 fi->fi_name, fi->fi_newindex);
		if (!newentry) error(1, 0, "hashtable corrupted");
		cur.fi_newindex = fi->fi_newindex;
		*fi = cur;
	}
	else {
		if (Trace)
			(void)fprintf(stderr,
				      "old font %s (was %d is %d)\n",
				      cur.fi_name, index,
				      fi->fi_newindex);
		if (!newentry) {
			/* font already entered */
			if (cur.fi_checksum != fi->fi_checksum)
				error(1, 0,
				      "font %s checksum changed to %d (was %d)",
				      cur.fi_name, cur.fi_checksum,
				      fi->fi_checksum);
		}
	}
}

/*
 * Handle a \special.
 */
HandleSpecial(c, l, p)
	int c;
	register int l;
	register i32 p;
{
	register int i;

	if (UseThisPage) {
		putc(c, outf);
		switch (l) {

		case DPL_UNS1:
			putc(p, outf);
			CurrentPosition += 2;
			break;

		case DPL_UNS2:
			PutWord(outf, p);
			CurrentPosition += 3;
			break;

		case DPL_UNS3:
			Put3Byte(outf, p);
			CurrentPosition += 4;
			break;

		case DPL_SGN4:
			PutLong(outf, p);
			CurrentPosition += 5;
			break;

		default:
			panic("HandleSpecial l=%d", l);
			/* NOTREACHED */
		}
		CurrentPosition += p;
		while (--p >= 0) {
			i = getc(inf);
			putc(i, outf);
		}
		if (feof(inf))
			error(1, 0, "unexpected EOF in %s",CurFile);
		if (ferror(outf))
			error(1, errno, writeerr);
	} else
		while (--p >= 0)
			(void) getc(inf);
}

ReallyUseFont()
{
	int new;
	struct hash_entry_s *entry;

	entry = fontByIndex(CurFile, CurrentFontIndex, &new);
	if (new)
		error(1, 0, "font %d is not defined in %s", CurrentFontIndex,
		      CurFile);
	else {
		struct fontinfo *fi;

		fi = entry->fi;
		if (Trace)
			(void)fprintf(stderr, "using font (%d) %s\n",
			   fi->fi_newindex, fi->fi_name);
		if (fi->fi_reallyused == 0) {
			fi->fi_reallyused++;
			if (Trace) (void)
			   fprintf(stderr, "font %d written\n",fi->fi_newindex);
			WriteFont(fi);
		}
		OutputFontIndex = entry->output;
		PutFontSelector(entry->output);
	}
}

/*
 * Write a font selection command to the output file
 */
PutFontSelector(index)
	i32 index;
{

	if (index < 64) {
		putc(index + DVI_FNTNUM0, outf);
		CurrentPosition++;
	} else if (index < 256) {
		putc(DVI_FNT1, outf);
		putc(index, outf);
		CurrentPosition += 2;
	} else if (index < 65536) {
		putc(DVI_FNT2, outf);
		PutWord(outf, index);
		CurrentPosition += 3;
	} else if (index < 16777216) {
		putc(DVI_FNT3, outf);
		Put3Byte(outf, index);
		CurrentPosition += 4;
	} else {
		putc(DVI_FNT4, outf);
		PutLong(outf, index);
		CurrentPosition += 5;
	}
}

/*
 * The following table describes the length (in bytes) of each of the DVI
 * commands that we can simply copy, starting with DVI_SET1 (128).
 */
char	oplen[128] = {
	0, 0, 0, 0,		/* DVI_SET1 .. DVI_SET4 */
	9,			/* DVI_SETRULE */
	0, 0, 0, 0,		/* DVI_PUT1 .. DVI_PUT4 */
	9,			/* DVI_PUTRULE */
	1,			/* DVI_NOP */
	0,			/* DVI_BOP */
	0,			/* DVI_EOP */
	1,			/* DVI_PUSH */
	1,			/* DVI_POP */
	2, 3, 4, 5,		/* DVI_RIGHT1 .. DVI_RIGHT4 */
	1,			/* DVI_W0 */
	2, 3, 4, 5,		/* DVI_W1 .. DVI_W4 */
	1,			/* DVI_X0 */
	2, 3, 4, 5,		/* DVI_X1 .. DVI_X4 */
	2, 3, 4, 5,		/* DVI_DOWN1 .. DVI_DOWN4 */
	1,			/* DVI_Y0 */
	2, 3, 4, 5,		/* DVI_Y1 .. DVI_Y4 */
	1,			/* DVI_Z0 */
	2, 3, 4, 5,		/* DVI_Z1 .. DVI_Z4 */
	0,			/* DVI_FNTNUM0 (171) */
	0, 0, 0, 0, 0, 0, 0, 0,	/* 172 .. 179 */
	0, 0, 0, 0, 0, 0, 0, 0,	/* 180 .. 187 */
	0, 0, 0, 0, 0, 0, 0, 0,	/* 188 .. 195 */
	0, 0, 0, 0, 0, 0, 0, 0,	/* 196 .. 203 */
	0, 0, 0, 0, 0, 0, 0, 0,	/* 204 .. 211 */
	0, 0, 0, 0, 0, 0, 0, 0,	/* 212 .. 219 */
	0, 0, 0, 0, 0, 0, 0, 0,	/* 220 .. 227 */
	0, 0, 0, 0, 0, 0, 0,	/* 228 .. 234 */
	0, 0, 0, 0,		/* DVI_FNT1 .. DVI_FNT4 */
	0, 0, 0, 0,		/* DVI_XXX1 .. DVI_XXX4 */
	0, 0, 0, 0,		/* DVI_FNTDEF1 .. DVI_FNTDEF4 */
	0,			/* DVI_PRE */
	0,			/* DVI_POST */
	0,			/* DVI_POSTPOST */
	0, 0, 0, 0, 0, 0,	/* 250 .. 255 */
};

/*
 * Here we read the input DVI file and write relevant pages to the
 * output DVI file. We also keep track of font changes, handle font
 * definitions, and perform some other housekeeping.
 */
HandleDVIFile()
{
	register int c, l;
	register i32 p;
	register int CurrentFontOK = 0;

	/* Only way out is via "return" statement */
	for (;;) {
		c = getc(inf);	/* getc() returns unsigned values */
		if (DVI_IsChar(c)) {
			/*
			 * Copy chars, note font usage, but ignore if
			 * page is not interesting.
			 */
			if (!UseThisPage)
				continue;
			if (!CurrentFontOK) {
				ReallyUseFont();
				CurrentFontOK++;
			}
			putc(c, outf);
			CurrentPosition++;
			continue;
		}
		if (DVI_IsFont(c)) {	/* note font change */
			CurrentFontIndex = c - DVI_FNTNUM0;
			CurrentFontOK = 0;
			continue;
		}
		if ((l = (oplen - 128)[c]) != 0) {	/* simple copy */
			if (!UseThisPage) {
				while (--l > 0)
					(void) getc(inf);
				continue;
			}
			CurrentPosition += l;
			putc(c, outf);
			while (--l > 0) {
				c = getc(inf);
				putc(c, outf);
			}
			if (ferror(outf))
				error(1, errno, writeerr);
			continue;
		}
		if ((l = DVI_OpLen(c)) != 0) {
			/*
			 * Handle other generics.
			 * N.B.: there should only be unsigned parameters
			 * here (save SGN4), for commands with negative
			 * parameters have been taken care of above.
			 */
			switch (l) {

			case DPL_UNS1:
				p = getc(inf);
				break;

			case DPL_UNS2:
				fGetWord(inf, p);
				break;

			case DPL_UNS3:
				fGet3Byte(inf, p);
				break;

			case DPL_SGN4:
				fGetLong(inf, p);
				break;

			default:
				panic("HandleDVIFile l=%d", l);
			}

			/*
			 * Now that we have the parameter, perform the
			 * command.
			 */
			switch (DVI_DT(c)) {

			case DT_SET:
			case DT_PUT:
				if (!UseThisPage)
					continue;
				if (!CurrentFontOK) {
					ReallyUseFont();
					CurrentFontOK++;
				}
				putc(c, outf);
				switch (l) {

				case DPL_UNS1:
					putc(p, outf);
					CurrentPosition += 2;
					continue;

				case DPL_UNS2:
					PutWord(outf, p);
					CurrentPosition += 3;
					continue;

				case DPL_UNS3:
					Put3Byte(outf, p);
					CurrentPosition += 4;
					continue;

				case DPL_SGN4:
					PutLong(outf, p);
					CurrentPosition += 5;
					continue;
				}

			case DT_FNT:
				CurrentFontIndex = p;
				CurrentFontOK = 0;
				continue;

			case DT_XXX:
				HandleSpecial(c, l, p);
				continue;

			case DT_FNTDEF:
				HandleFontDef(p);
				continue;

			default:
				panic("HandleDVIFile DVI_DT(%d)=%d",
				      c, DVI_DT(c));
			}
			continue;
		}

		switch (c) {	/* handle the few remaining cases */

		case DVI_BOP:
			BeginPage();
			CurrentFontOK = 0;
			break;

		case DVI_EOP:
			EndPage();
			break;

		case DVI_PRE:
			GripeUnexpectedOp("PRE");
			/* NOTREACHED */

		case DVI_POST:
			return;

		case DVI_POSTPOST:
			GripeUnexpectedOp("POSTPOST");
			/* NOTREACHED */

		default:
			GripeUndefinedOp(c);
			/* NOTREACHED */
		}
	}
}
