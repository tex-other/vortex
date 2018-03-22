.ds TE "T\h'-.15m'\v'.2v'E\v'-.2v'\h'-.12m'X
.ds VT "V\h'-.15m'\v'.2v'O\h'-.03m'R\v'-.2v'\h'-.2m'\*(TE
.LP
.ce
.B ABSTRACT
.PP
\*(VT is a source-based interactive document
preparation system, built on the \*(TE typesetting
language, which allows some of the functions of
direct-manipulation systems as well.
This report describes the editor
portion of the prototype system as currently implemented.
.PP
\*(VT is an interactive system based on \*(TE. \*(TE allows
finer control and produces higher quality typesetting than the other
systems commonly available on UNIX.
\*(VT makes these features
more accessible through integration with a powerful editor, an
incremental \*(TE processor and a what-you-see-is-what-you-get output
displayer.
.PP
The first version of \*(VT is nearly finished and has
shown us what problems our early assumptions and the design have
produced.  This document describes the internals of the system from
the point of view of the \*(VT source editor (the user interface).
Since \*(VT is written in three separate pieces, this paper does
not describe the architecture of the entire system.  However, it
does document most of the interfaces within the system.
