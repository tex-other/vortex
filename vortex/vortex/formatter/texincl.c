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

/*
#ifdef VORTEX
#include	<sys/types.h>
#include	<sys/socket.h>
#include	<sys/un.h>
#include	"protocol.h"
#endif
*/

/*************************************/
#ifdef VORTEX
#define	SOCK_PREF	"/tmp/#"
struct _node	*left_extent();
struct _node	*right_extent();

comm_view()
{
  int	sock, msgsock;
  struct sockaddr_un	server;

  sock = socket(AF_UNIX, SOCK_STREAM, 0);
  if (sock < 0) {
    perror("opening stream socket(server)");
    exit(1);
  }
  server.sun_family = AF_UNIX;
  strcpy(server.sun_path, SOCK_PREF);
  strcat(server.sun_path, getenv("USER"));

  if (bind(sock, &server, strlen(server.sun_path)+2)) {
    perror("binding stream socket(server)");
    exit(1);
  }

  if (listen(sock, 1) != 0) {
    perror("listen");
    exit(1);
  }

  for (;;) {
    msgsock = accept(sock ,0 ,0);
    if (msgsock == -1) 
      perror("accept");
    else { 
      view_protocol(msgsock);
      close(msgsock);
      break;
    }
  }
  close(sock);
  unlink(server.sun_path);
}

view_protocol(sock)
int	sock;
{
  char	buff[100];
  struct _node	*cur_node;
  struct _node	*nptr_stack[50];
  struct _node	*irs_beg_buf, *irs_end_buf;
  struct _node	*char_node;
  int	nptr;


  irs_beg_buf = NIL;
  nptr = 50;
  for (;;) {
    if (read(sock, buff, sizeof(buff)) < 0) {
      perror("reading stream socket(server)");
      exit(1);
    }
    switch((int)buff[0]) {
    case FIRSTPAR:
	    cur_node = (struct _node *) par_node_top;
	    xmit(sock, cur_node);
	    break;
    case FIRSTCHILD:
	    switch(cur_node->nd_type) {
	    case NODE_CHAR:
		    xmit(sock, NIL);	/* no more child */
		    break;
	    case NODE_WORD:
		    cur_node = ((struct _unode *) cur_node)->_dn;
		    xmit(sock, cur_node);
		    break;
	    case NODE_PAR:
		    cur_node = ((struct _unode *) cur_node)->_dn;
		    xmit(sock, cur_node);
		    break;
	    case NODE_GROUP:
		    cur_node = ((struct _group *) cur_node)->_dn;
		    xmit(sock, cur_node);
		    break;
	    case NODE_CSEQ:
	    case NODE_RULE:
	    case NODE_SPECIAL:
		    cur_node = ((struct _cseq *) cur_node)->_bon;
		    xmit(sock, cur_node);
		    break;
	    case NODE_MATH:
	    case NODE_DISPLAY:
		    cur_node = ((struct _math *) cur_node)->_dn;
		    xmit(sock, cur_node);
		    break;
	    case NODE_INPUT:
		    cur_node = ((struct _input *) cur_node)->_bon;
		    xmit(sock, cur_node);
		    break;
	    case NODE_SPACE:
		    cur_node = ((struct _space *) cur_node)->_boc;
		    xmit(sock, cur_node);
		    break;
	    default:
		    fprintf(stderr, "Invalid node!?\n");
		    exit(1);
		    break;
	    }
	    break;
    case NEXTELM:
      cur_node = cur_node->nd_rt;
      xmit(sock, cur_node);
      break;
    case PREVELM:
      cur_node = cur_node->nd_lt;
      xmit(sock, cur_node);
      break;
    case SETBEG:
      irs_beg_buf = cur_node;	/* save cur_node as begin of buffer */
      xmit(sock, cur_node);
      break;
    case RESTOREBEG:
      cur_node = irs_beg_buf;
      xmit(sock, cur_node);
      break;
    case SETEND:
      irs_end_buf = cur_node;
      xmit(sock, cur_node);
      break;
    case RESTOREEND:
      cur_node = irs_end_buf;
      xmit(sock, cur_node);
      break;
    case PUSHELM:
      nptr_stack[--nptr] = cur_node;
      if (nptr < 0) {
	fprintf(stderr, "stack over flow\n");
	exit(1);
      }
      xmit(sock, cur_node);
      break;
    case POPELM:
      cur_node = nptr_stack[nptr++];
      if (nptr > 50) {
	fprintf(stderr, "stack under flow\n");
	exit(1);
      }
      xmit(sock, cur_node);
      break;
    case UPELM:
      xmit(sock, cur_node->nd_up);
      break;
    case LEFTELM:
      xmit(sock, cur_node->nd_lt);
      break;
    case RIGHTELM:
      xmit(sock, cur_node->nd_rt);
      break;
    case CSPARAM:
      cur_node = ((struct _cseq *)cur_node)->_boc;
      xmit(sock, cur_node);
      break;
    case THISELM:
      xmit(sock, cur_node);
      break;
    case SAVECHAR:
      char_node = cur_node;
      xmit(sock, cur_node);
      break;
    case RESTORECHAR:
      cur_node = char_node;
      xmit(sock, cur_node);
      break;
    case PARELM:		/* parent elm */
      cur_node = cur_node->nd_up;
      xmit(sock, cur_node);
      break;
    case GETEXTENT:		/* get extent of node */
      get_extent(cur_node, char_node, sock);
      break;
    case CSEXTENT:
      cs_extent(cur_node, char_node, sock);
      break;
    case ENDTREE:
      xmit(sock, cur_node);
      return;
      break;
    default:
      fprintf(stderr, "invalid message(client)\n");
      exit(1);
    }
  }
}

get_extent(cur_node, xnode, sock)
struct _node	*cur_node, *xnode;
int	sock;
{
  struct _node	*beg_node, *end_node; /* left_limit, right_limit */
  int		beg_offset, end_offset;
  int	*par_ptr;
  struct _node	par_node;

  beg_node = left_extent(cur_node);
  end_node = right_extent(cur_node);
  beg_offset = -1 * set_offset(beg_node, xnode);
  end_offset = set_offset(xnode, end_node);
  par_node.nd_type = NODE_PAR;	
  par_ptr = (int *)&par_node;
  *(par_ptr + 1) = beg_offset;
  *(par_ptr + 2) = end_offset;
  xmit(sock, &par_node);
}

cs_extent(cur_node, xnode, sock)
struct _node	*cur_node, *xnode;
int	sock;
{
  int	beg_offset, end_offset;
  struct _cseq	cseq_node;
  int	*cseq_ptr;

  beg_offset = set_offset(xnode, ((struct _cseq *)cur_node)->_boc);
  end_offset = set_offset(xnode, ((struct _cseq *)cur_node)->_eoc);
  cseq_node._ty = NODE_CSEQ;
  cseq_ptr = (int *)&cseq_node;
  *(cseq_ptr + 1) = beg_offset;
  *(cseq_ptr + 2) = end_offset;
  xmit(sock, &cseq_node);
}


/* get left limit of its extent */
struct _node *
left_extent(xnode)
struct _node	*xnode;
{
  if (xnode->nd_type == NODE_CHAR)
    return(xnode);
  else {
    switch(xnode->nd_type) {
      case NODE_WORD:
	return(left_extent(((struct _unode *)xnode)->_dn));
	break;
      case NODE_PAR:
	return(left_extent(((struct _unode *)xnode)->_dn));
	break;
      case NODE_GROUP:
	return(left_extent(((struct _group *)xnode)->_dn));
	break;
      case NODE_CSEQ:
	return(left_extent(((struct _cseq *)xnode)->_bon));
	break;
      case NODE_MATH:
	return(left_extent(((struct _math *)xnode)->_dn));
	break;
      case NODE_DISPLAY:
	return(left_extent(((struct _math *)xnode)->_dn));
	break;
      case NODE_INPUT:
	return(left_extent(((struct _input *)xnode)->_bon));
	break;
      case NODE_SPACE:
	return(left_extent(((struct _space *)xnode)->_boc));
	break;
      case NODE_SPECIAL:
	return(left_extent(((struct _cseq *)xnode)->_bon));
	break;
      default:
	fprintf(stderr, "Invalid node!?\n");
	exit(1);
	break;
      }
  }
}

/* get right limit of its extent */
struct _node *
right_extent(xnode)
struct _node	*xnode;
{
  struct _node	*node_ptr, *node_tmp;

  if (xnode->nd_type == NODE_CHAR)
    return(xnode);
  else {
    switch(xnode->nd_type) {
    case NODE_WORD:
      node_ptr = ((struct _unode *)xnode)->_dn;
comm:
      while(1) {
	node_tmp = node_ptr->nd_rt;
	if (node_tmp == NIL) break;
	if (node_tmp->nd_up != xnode) break;
	node_ptr = node_tmp;
      }
      return(right_extent(node_ptr));
      break;
    case NODE_PAR:
      node_ptr = ((struct _unode *)xnode)->_dn;
      goto comm;
      break;
    case NODE_GROUP:
      node_ptr = ((struct _group *)xnode)->_dn;
      goto comm;
      break;
    case NODE_CSEQ:
      node_ptr = ((struct _cseq *)xnode)->_bon;
      goto comm;
      break;
    case NODE_MATH:
      node_ptr = ((struct _math *)xnode)->_dn;
      goto comm;
      break;
    case NODE_DISPLAY:
      node_ptr = ((struct _math *)xnode)->_dn;
      goto comm;
      break;
    case NODE_INPUT:
      node_ptr = ((struct _input *)xnode)->_dn;
      goto comm;
      break;
    case NODE_SPACE:
      return(((struct _space *)xnode)->_eoc);
      break;
    case NODE_SPECIAL:
      node_ptr = ((struct _cseq *)xnode)->_bon;
      goto comm;
      break;
    default:
	fprintf(stderr, "Invalid node!?\n");
	exit(1);
	break;
      }
  }
}

/* count IRs node between two node */
set_offset(nd_begin, nd_end)
struct _node	*nd_begin, *nd_end;
{
  int	cnt;
  struct _node	*node_ptr;
  
  node_ptr = nd_begin;
  cnt = 0;
  while(node_ptr != nd_end) {
    cnt++;
    node_ptr = node_ptr->nd_rt;
    if (node_ptr->nd_char == (char)-1) break;
  }
  return(cnt);
}


xmit(sock, xnode)
int	sock;
struct _node	*xnode;
{
  char	buff[100];

  if (xnode != NIL) {
    bcopy(xnode, buff, nodesize(xnode));
    if (write(sock, buff, nodesize(xnode)) < 0) {
      perror("writing on stream socket(server)");
      exit(1);
    }
   } else{
      buff[0] = 0;
      if (write(sock, buff, 1) < 0) {
	perror("writing on stream socket(server)");
	exit(1);
      }
    }
}

nodesize(xnode)
struct _node	*xnode;
{
  switch(xnode->nd_type) {
  case NODE_CHAR:
    return(sizeof(struct _char));
  case NODE_WORD:
    return(sizeof(struct _unode));
  case NODE_PAR:
    return(sizeof(struct _unode));
  case NODE_GROUP:
    return(sizeof(struct _group));
  case NODE_CSEQ:
  case NODE_SPECIAL:
    return(sizeof(struct _cseq));
  case NODE_MATH:
  case NODE_DISPLAY:
    return(sizeof(struct _math));
  case NODE_INPUT:
    return(sizeof(struct _input));
  case NODE_SPACE:
    return(sizeof(struct _space));
  default:
    return(sizeof(struct _node));
    break;
  }
}

#endif
/*************************************/
