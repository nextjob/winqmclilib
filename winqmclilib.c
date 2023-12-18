/* winQMclilib.c
 * based on: QMCLILIB.C
 * QM Client C library
 * Copyright (c) 2007 Ladybridge Systems, All Rights Reserved
 *
 * Licensed under the terms of any of the following licenses at your
 * choice:
 *
 *  - GNU General Public License Version 2 or later (the "GPL")
 *    http://www.gnu.org/licenses/gpl.html
 *
 *  - GNU Lesser General Public License Version 2.1 or later (the "LGPL")
 *    http://www.gnu.org/licenses/lgpl.html
 *
 * Ladybridge Systems can be contacted via the www.openqm.com web site.
 * 
 * ScarletDME Wiki: https://scarlet.deltasoft.com
 *
 * START-HISTORY (winQMclilib):
 * xxDec23 mab add more functions, at this build we now include:
 * QMCallx
 * QMClose
 * QMConnect
 * QMConnected
 * QMDcount
 * QMDebug
 * QMDisconnect
 * QMDisconnectAll
 * QMError
 * QMExecute
 * QMExtract
 * QMFree
 * QMGetArg
 * QMGetSession
 * QMIns
 * QMLocate
 * QMOpen
 * QMRead
 * QMReadl
 * QMReadu
 * QMRecordlock
 * QMRelease
 * QMReplace
 * QMSetSession
 * QMStatus
 * QMWrite
 * QMWriteu
 * xxSep23 mab attempt windows port, using c++ builder ver11.3 CE
 * This port is not intended to be a complete port of qmclilib.c, at this time only porting functions that I currently need.
 * Also will probably not include the call function, but instead the Callx / Getarg functions of newer client versions.
 * Warning: winqmclilib does not maintian a storage area for Getarg parameters for each session.
 *  Using Callx will "overwrite" the previous Callx parameters regardless of session number.
 *  A solution would be to add the return call buffers to the session structure....
 *    Or size the pointer array CallArgArray to have a slot for the max number of call args * the max number of sessions
 *    char *    CallArgArray[MAX_ARGS*MAX_SESSIONS]
 *    Then use session_idx*MAX_ARGS as the offset into CallArgArray for the sessions QMCall arguments
 * Notes: There seems to be an issue with char vs unsigned char when using the memchr c function (void *memchr(const void *str, int c, size_t n).
 *   If passing the int c parameter as a char, characters > 127 cannot be found (interpreted as a neg integer?).
 *   The gcc compiler & linux runtime seems to be fine with it
 *     not so with C++ BUilder / Windows. Need to use unsigned Char.
 *   I cannot find where the transfer buffer "buff" is freed, need to test for memory leak to see if this is really the case.
 *     For now I have added code to disconnect() to free buff if there are no more remainging active connections.
 *
 *   Rem "C:\Program Files\Microsoft Visual Studio\2022\Community\VC\Tools\MSVC\14.38.33130\bin\Hostx64\x86\dumpbin.exe" /exports winqmclilib.dll
 *   To get listing of functions
 *
 * This is a work in progress.......
 *
 * START-HISTORY (ScarletDME):
 * 15Jan22 gwb Fixed issue #26 - resolves CWE-197 check, "Comparison of narrow type with wide type in loop condition."
 *
 * 13Jan22 gwb Fixed issue #29 (QMCONFIG environment variable still being referenced)
 * 
 * 09Jan22 gwb Fixes to warnings related out output specifiers in printf().
 *             (mostly changing %ld to %d)
 *
 * 28Feb20 gwb Changed integer declarations to be portable across address
 *             space sizes (32 vs 64 bit)
 *
 * 23Feb20 gwb Cleared variable set but not used warnings in QMReadList and
 *             context_error().  context_error() should be revisited in the
 *             future.  See the comment in the function for more info.
 *             Converted K&R style function declarations to ANSI style.
 *             found an instance where an attempt at reading /etc/qmconfig 
 *             could happen, changed to /ets/scarlet.conf
 * 
 * START-HISTORY (OpenQM):
 * 06 Apr 09        Add LGPL licence
 * 19 Oct 07  2.6-5 Method used to recognise IP address in qmconnect() was
 *                  inadequate.
 * 01 Jul 07  2.5-7 Extensive change for PDA merge.
 * 21 May 07  2.5-5 0553 QMReplace() handled negative value or subvalue position
 *                  wrongly.
 * 02 Nov 06  2.4-15 MATCHES template codes should be case insensitive.
 * 11 Aug 06  2.4-11 0512 Subvalue level QMLocate() started the scan at the
 *                   incorrect position.
 * 20 Mar 06  2.3-8 Added QMMarkMapping().
 * 17 Feb 06  2.3-6 Import ctype.c for character library functions.
 * 17 Feb 06  2.3-6 Call initialise_client() from all paths that could be first
 *                  entry to library.
 * 05 Jan 06  2.3-3 Added QMSelectLeft(), QMSelectRight(), QMSetLeft() and
 *                  QMSetRight().
 * 28 Dec 05  2.3-3 Revised to use localisable ctype functions.
 * 22 Nov 05  2.2-17 Do our own isdigit() and isalpha() on NIX systems.
 * 14 Oct 05  2.2-15 0421 Call waitpid() on termination of local connection to
 *                   get rid of defunct process (Not Windows).
 * 08 Sep 05  2.2-10 Added QMLogto().
 * 30 Aug 05  2.2-9 Added big endian support.
 * 22 Aug 05  2.2-8 Multiple session support.
 * 26 Jul 05  2.2-6 0380 QMIns() postmark logic was wrong.
 * 25 Jul 05  2.2-6 0379 QMLocate() was returning 2, not 1, for the insertion
 *                  position when inserting into a null item.
 * 18 Apr 05  2.1-12 Close process handle after process creation.
 * 01 Feb 05  2.1-5 Trap zero length host name as this crashes the socket
 *                  library.
 * 26 Oct 04  2.0-7 0272 Cannot use sizeof() for packet header transmission
 *                  size.
 * 26 Oct 04  2.0-7 Increased MAX_ID_LEN to absolute limit of current file
 *                  sysemt implementation.
 * 21 Oct 04  2.0-6 0267 QMSelectIndex() was sending the wrong length for the
 *                  indexed value string.
 * 30 Sep 04  2.0-3 Added QMRecordlock().
 * 16 Sep 04  2.0-1 OpenQM launch. Earlier history details suppressed.
 * END-HISTORY
 *
 * START-DESCRIPTION:
 *
 *
 *
 * Session Management
 * ==================
 * QMConnect()
 * QMConnected()
 * QMConnectLocal()
 * QMDisconnect()
 * QMDisconnectAll()
 * QMGetSession()
 * QMSetSession()
 * QMLogto()
 * 
 * 
 * File Handling
 * =============
 * QMOpen()
 * QMClose()
 * QMRead()
 * QMReadl()
 * QMReadu()
 * QMWrite()
 * QMWriteu()
 * QMDelete()
 * QMDeleteu()
 * QMRelease()
 * QMSelect()
 * QMSelectIndex()
 * QMSelectLeft()
 * QMSelectRight()
 * QMSetLeft()
 * QMSetRight()
 * QMReadNext()
 * QMClearSelect()
 * QMRecordlock()
 * QMMarkMapping()
 *
 *
 * Dynamic Array Handling
 * ======================
 * QMExtract()
 * QMReplace()
 * QMIns()
 * QMDel()
 * QMLocate()
 *
 *
 * String Handling
 * ===============
 * QMChange()
 * QMDcount()
 * QMField()
 * QMMatch()
 * QMMatchfield()
 * QMFree()
 *
 * Command Execution
 * =================
 * QMExecute()
 * QMRespond()
 * QMEndCommand()
 *
 * Subroutine Execution
 * ====================
 * QMCall()
 *
 *
 * Error Handling
 * ==============
 * QMError()
 * QMStatus()
 *
 *
 * Internal
 * ========
 * QMDebug()
 * QMEnterPackage()
 * QMExitPackage()
 *
 * END-DESCRIPTION
 *
 * START-CODE
 */

#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <fcntl.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
/* next 3 for windows? */
#include <io.h>
#include <winsock2.h>
#include <stdint.h>

/* define the @#!$ BSD style int types */
typedef uint32_t u_int32_t;

#define Public
/* #include "qmdefs.h" */
/* following pulled from qmdefs.h at least for now
	there is a bunch of stuff in qmdefs.h that will not resolve with c++ builder
	some of the int defines, bigended stuff
*/
#define Private static

#define MAX_PATHNAME_LEN 255    /* Changes affect file headers */
#define MAX_ID_LEN 255          /* Increasing requires major file changes */
#define MAX_CALL_NAME_LEN 63    /* Cannot exceed MAX_ID_LEN */
#define MAX_TRIGGER_NAME_LEN 32 /* Increasing would alter file header */
#define MAX_PROGRAM_NAME_LEN 128
#define MAX_USERNAME_LEN 32
#define MAX_MATCH_TEMPLATE_LEN 256
#define MAX_MATCHED_STRING_LEN 8192
#define MAX_PACKAGES 32
#define MAX_PACKAGE_NAME_LEN 15
#define MAX_ACCOUNT_NAME_LEN 32
#define MAX_SORTMRG 10
#define MAX_SORT_KEYS 32
#define MAX_SORT_KEY_LEN 1024


 typedef int16_t bool;
#define FALSE 0
#define TRUE 1

#define ShortInt(n) (n)
#define LongInt(n) (n)

#define ALIGN2 __attribute__((aligned(2), packed))

/* ======================================================================
   Case conversion macros and data                                        */
typedef unsigned char u_char;    /* u_char is a  BSD type ?? */
Public char uc_chars[256];
Public char lc_chars[256];
#define UpperCase(c) (uc_chars[((u_char)(c))])
#define LowerCase(c) (lc_chars[((u_char)(c))])
Public u_char char_types[256];
#define CT_ALPHA 0x01
#define CT_DIGIT 0x02
#define CT_GRAPH 0x04
#define CT_MARK 0x08
#define CT_DELIM 0x10
#define IsAlnum(c) (char_types[((u_char)(c))] & (CT_ALPHA | CT_DIGIT))
#define IsAlpha(c) (char_types[((u_char)(c))] & CT_ALPHA)
#define IsDigit(c) (char_types[((u_char)(c))] & CT_DIGIT)
#define IsGraph(c) (char_types[((u_char)(c))] & CT_GRAPH)
#define IsDelim(c) (char_types[((u_char)(c))] & CT_DELIM)
#define IsMark(c) (char_types[((u_char)(c))] & CT_MARK)

/*
  these do not work the same as gcc,
  you end up with the interger value of the neg number, ie
  #define TEXT_MARK ((char)-5)
  resolves as -5

  #define SUBVALUE_MARK ((char)-4)
  #define VALUE_MARK ((char)-3)
  #define FIELD_MARK ((char)-2)
  #define ITEM_MARK ((char)-1)

  */
#define U_TEXT_MARK ((u_char)'\xFB')
#define U_SUBVALUE_MARK ((u_char)'\xFC')
#define U_VALUE_MARK ((u_char)'\xFD')
#define U_FIELD_MARK ((u_char)'\xFE')
#define U_ITEM_MARK ((u_char)'\xFF')

/* end of qmdefs.h lazyness  */

/* #include "qmnet.h"   do not include until i know why */

void set_default_character_maps(void);

/* include <sys/wait.h> not on windows*/
/* #include "linuxlb.h" not on windows*/
/* removed, call net_error directly #define NetErr(msg, winerr, lnxerr) net_error(msg, lnxerr) */

#define DLLEntry  __declspec(dllexport)

#define FOPEN_READ_MODE "r"

#include "err.h"
#include "revstamp.h"
#include "qmclient.h"

/* Network data */
Private bool OpenSocket(char* host, int16_t port);
Private bool CloseSocket(void);
Private bool read_packet(void);
Private bool write_packet(int type, char* data, int32_t bytes);
Private void net_error(char* prefix);
Private void debug(unsigned char* p, int n);
Private void initialise_client(void);
Private bool FindFreeSession(void);
Private void disconnect(void);

typedef struct ARGDATA ARGDATA;
struct ARGDATA {
  int16_t argno;
  int32_t arglen ALIGN2;
  char text[1];
};

/* Packet buffer */
#define BUFF_INCR 4096
typedef struct INBUFF INBUFF;
struct INBUFF {
  union {
    struct {
      char message[1];
    } abort;
    struct { /* QMCall returned arguments */
      struct ARGDATA argdata;
    } call;
    struct { /* Error text retrieval */
      char text[1];
    } error;
    struct { /* QMExecute */
      char reply[1];
    } execute;
    struct { /* QMOpen */
      int16_t fno;
    } open;
    struct { /* QMRead, QMReadl, QMReadu */
      char rec[1];
    } read;
    struct { /* QMReadList */
      char list[1];
    } readlist;
    struct { /* QMReadNext */
      char id[1];
	} readnext;
    struct { /* QMSelectLeft, QMSelectRight */
      char key[1];
    } selectleft;
  } data;
} ALIGN2;

Private INBUFF* buff = NULL;
Private int32_t buff_size;  /* Allocated size of buffer */
Private int32_t buff_bytes; /* Size of received packet */
Private FILE* srvr_debug = NULL;

/* buffers for Callx return arguments  */
#define MAX_ARGS 20
Private char *    CallArgArray[MAX_ARGS];          /* create an array of pointers, for string arguments (returned by Callx) not sure if this is correct */
Private int       CallArgArraySz[MAX_ARGS];        /* and size of memory allocated for each  string length + terminator */

#define MAX_SESSIONS 4
Private struct {
  bool is_local;
  int16_t context;
#define CX_DISCONNECTED 0 /* No session active */
#define CX_CONNECTED 1    /* Session active but not... */
#define CX_EXECUTING 2    /* ...executing command (implies connected) */
  char qmerror[512 + 1];
  int16_t server_error;
  int32_t qm_status;
  SOCKET sock;
  HANDLE hPipe;
 /* int RxPipe[2];
  int TxPipe[2];
  int pid;  0421 Pid of child process */
} session[MAX_SESSIONS];

Private int16_t session_idx = 0;

/* Matching data */
Private char* component_start;
Private char* component_end;

/* Internally used routines */
/*
void DLLEntry QMDisconnect(void);
void DLLEntry QMEndCommand(void);
*/

/* Internal functions */
/*   adding as we convert from linux to windows   ----
Private char* SelectLeftRight(int16_t fno,
                              char* index_name,
                              int16_t listno,
                              int16_t mode);
Private void SetLeftRight(int16_t fno, char* index_name, int16_t mode);
*/
Private bool context_error(int16_t expected);
/* Private void delete_record(int16_t mode, int fno, char* id);  */

Private char* read_record(int fno, char* id, int* err, int mode);
Private void write_record(int16_t mode, int16_t fno, char* id, char* data);
Private bool GetResponse(void);
Private void Abort(char* msg, bool use_response);
/*  Private char* memstr(char* str, char* substr, int str_len, int substr_len);  */
/*	Private bool match_template(char* string,
							char* template,
							int16_t component,
							int16_t return_component);  */

Private bool message_pair(int type, char* data, int32_t bytes);
Private char* NullString(void);
/* Private char* sysdir(void); */

#define ClearError session[session_idx].qmerror[0] = '\0'

/* ======================================================================
   QMCall()  - Callx catalogued subroutine
   Note, this version does NOT repopulate the returned values of the QMCallx calling arguments, you must use QMGetArg to retrive them */
void DLLEntry QMCallx(char* subrname, int16_t argc, ...) {
  va_list ap;    /* variable arg list, see man va_list */
  int16_t i;
  char* arg;
  char* arg_p;   /* pointer to memory for coping arg string*/
  int subrname_len;
  int32_t arg_len;
  int32_t bytes; /* Total length of outgoing packet */
  int32_t n;
  char* p;
  INBUFF* q;
  struct ARGDATA* argptr;
  int offset;

  if (context_error(CX_CONNECTED))
    return;
  subrname_len = strlen(subrname);
  if ((subrname_len < 1) || (subrname_len > MAX_CALL_NAME_LEN)) {
    Abort("Illegal subroutine name in call", FALSE);
  }
  if ((argc < 0) || (argc > MAX_ARGS)) {
    Abort("Illegal argument count in call", FALSE);
  }
 /* free up any memory allocated for prev callx arg strorage */
  for (i = 0; i < MAX_ARGS; i++) {
	if (CallArgArray[i] != NULL ){
	 free(CallArgArray[i]);
	 CallArgArray[i] = NULL;
	 CallArgArraySz[i] = 0;
	}
  }
  /* Accumulate outgoing packet size */
  bytes = 2;                        /* Subrname length */
  bytes += (subrname_len + 1) & ~1; /* Subrname string (padded) */
  bytes += 2;                       /* Arg count */
  va_start(ap, argc);
  for (i = 0; i < argc; i++) {
    arg = va_arg(ap, char*);
    arg_len = (arg == NULL) ? 0 : strlen(arg);
    bytes +=
        4 + ((arg_len + 1) & ~1); /* Four byte length + arg text (padded) */
  }
  va_end(ap);
  /* Ensure buffer is big enough */
  if (bytes >= buff_size) /* Must reallocate larger buffer */
  {
    n = (bytes + BUFF_INCR - 1) & ~BUFF_INCR;
    q = (INBUFF*)malloc(n);
    if (q == NULL) {
      Abort("Unable to allocate network buffer", FALSE);
    }
    free(buff);
    buff = q;
  }
  /* Set up outgoing packet */
  p = (char*)buff;
  *((int16_t*)p) = ShortInt(subrname_len); /* Subrname length */
  p += 2;
  memcpy(p, subrname, subrname_len); /* Subrname */
  p += (subrname_len + 1) & ~1;
  *((int16_t*)p) = ShortInt(argc); /* Arg count */
  p += 2;
 /* we do 2 things in this block, move callx args to the send buffer    */
 /* and save a copy pointed to by  CallArgArray[i]   for use by GetArg  */
  va_start(ap, argc);
  for (i = 1; i <= argc; i++) {
  /* first move arg to io buffer  */
    arg = va_arg(ap, char*);
    arg_len = (arg == NULL) ? 0 : strlen(arg);
    *((int32_t*)p) = LongInt(arg_len); /* Arg length */
    p += 4;
    if (arg_len)
      memcpy(p, arg, arg_len); /* Arg text */
	p += (arg_len + 1) & ~1;
  /* now save a copy                    */
	arg_p = (char *)malloc((arg_len+1) * sizeof(char));   /* reserver mem for string and terminator */
	/* if we fail to allocate memory, bomb out */
	if (arg_p == NULL) {
	   Abort("Unable to allocate Callx buffer", FALSE);
	} else {
   /* save ponter to allocated memory */
	  CallArgArray[i-1] = arg_p;
	  CallArgArraySz[i-1] = arg_len + 1;  /* along with the buffer size (string sz + terminator)    */
	  if (arg_len == 0) {
		*arg_p = '\0';
	  } else {
   /* copy arg to allocated memory, we are assuming arg is '\0' terminated, probably should check on this */
		strcpy(arg_p,arg);
	  }
	}
  }
  va_end(ap);
  if (!message_pair(SrvrCall, (char*)buff, bytes)) {
    goto err;
  }
  /* Now update CallArgArray with any returned arguments */
  offset = offsetof(INBUFF, data.call.argdata);
  if (offset < buff_bytes) {
    va_start(ap, argc);
	for (i = 1; i <= argc; i++) {
      argptr = (ARGDATA*)(((char*)buff) + offset);
      arg = va_arg(ap, char*);
	  if (i == argptr->argno) {
		arg_len = LongInt(argptr->arglen);
	   /*  memcpy(arg, argptr->text, arg_len);  */
	   /*  arg[arg_len] = '\0';                 */
	   /* check CallArgArray buffer is large enough for returned value */
		if ((CallArgArraySz[i-1]) < (arg_len+1)) {
	   /* not large enough, free and re allocate  */
		  if (CallArgArray[i-1] != NULL)
			 free(CallArgArray[i-1]);
		  CallArgArray[i-1] = malloc((arg_len+1) * sizeof(char));
		  if (CallArgArray[i-1] != NULL){
			CallArgArraySz[i-1] = (arg_len+1) * sizeof(char);
			memcpy(CallArgArray[i-1], argptr->text, arg_len);
			CallArgArray[i-1][arg_len] = '\0';
		  }else{
			Abort("Unable to allocate Callx buffer on return", FALSE);
		  }
		}else{
	  /* existing buffer large enough  */
		  memcpy(CallArgArray[i-1], argptr->text, arg_len);
		  CallArgArray[i-1][arg_len] = '\0';
		}

		offset +=
            offsetof(ARGDATA, text) + ((LongInt(argptr->arglen) + 1) & ~1);
        if (offset >= buff_bytes)
          break;
	  }
	}
    va_end(ap);
  }
err:
  switch (session[session_idx].server_error) {
    case SV_OK:
      break;
    case SV_ON_ERROR:
      Abort("CALL generated an abort event", TRUE);
      break;
    case SV_LOCKED:
    case SV_ELSE:
    case SV_ERROR:
      break;
  }
}
/* ======================================================================
   QMGetArg() Get called subroutine return argument
*/
char* DLLEntry QMGetArg(int ArgNbr) {
  int arg_idx;
  int arg_len;
  char* arg;
  if ((ArgNbr < 1) || (ArgNbr > MAX_ARGS)) {
	Abort("Illegal argument index in call", TRUE);
    return NULL;
  }
  arg_idx = ArgNbr - 1;
  if ((CallArgArray[arg_idx] == NULL)) {
	Abort("Argument value NULL", TRUE);
    return NULL;
  }
  arg_len = strlen(CallArgArray[arg_idx]);
  arg = (char *)malloc((arg_len+1) * sizeof(char));   /* reserver mem for string and terminator */
  /* if we fail to allocate memory, bomb out */
  if (arg == NULL) {
	Abort("GetgArg Memory Allocation Failed", TRUE);
    return NULL;
  }
  strcpy(arg,CallArgArray[arg_idx]);
  return arg;
}

/* ======================================================================
   QMClose()  -  Close a file                                             */
void DLLEntry QMClose(int fno) {
  struct {
    int16_t fno;
  } ALIGN2 packet;
  if (context_error(CX_CONNECTED))
    goto exit_qmclose;
  packet.fno = ShortInt(fno);
  if (!message_pair(SrvrClose, (char*)&packet, sizeof(packet))) {
    goto exit_qmclose;
  }
  switch (session[session_idx].server_error) {
    case SV_ON_ERROR:
	  Abort("CLOSE generated an abort event", TRUE);
      break;
  }
exit_qmclose:
  return;
}


/* ======================================================================
   QMConnect()  -  Open connection to server.                             */
int DLLEntry
QMConnect(char* host, int port, char* username, char* password, char* account) {
  int status = FALSE;
  char login_data[2 + MAX_USERNAME_LEN + 2 + MAX_USERNAME_LEN];
  int n;
  char* p;
  initialise_client();
  if (!FindFreeSession())
    goto exit_qmconnect;
  ClearError;
  session[session_idx].is_local = FALSE;
  n = strlen(host);
  if (n == 0) {
    strcpy(session[session_idx].qmerror, "Invalid host name");
    goto exit_qmconnect;
  }
  /* Set up login data */
  p = login_data;
  n = strlen(username);
  if (n > MAX_USERNAME_LEN) {
    strcpy(session[session_idx].qmerror, "Invalid user name");
    goto exit_qmconnect;
  }
  *((int16_t*)p) = ShortInt(n); /* User name len */
  p += 2;
  memcpy(p, (char*)username, n); /* User name */
  p += n;
  if (n & 1)
    *(p++) = '\0';
  n = strlen(password);
  if (n > MAX_USERNAME_LEN) {
    strcpy(session[session_idx].qmerror, "Invalid password");
    goto exit_qmconnect;
  }
  *((int16_t*)p) = ShortInt(n); /* Password len */
  p += 2;
  memcpy(p, (char*)password, n); /* Password */
  p += n;
  if (n & 1)
    *(p++) = '\0';
  /* Open connection to server */
  if (!OpenSocket((char*)host, port))
    goto exit_qmconnect;
  /* Check username and password */
  n = p - login_data;
  if (!message_pair(SrvrLogin, login_data, n)) {
    goto exit_qmconnect;
  }
  if (session[session_idx].server_error != SV_OK) {
    if (session[session_idx].server_error == SV_ON_ERROR) {
      n = buff_bytes - offsetof(INBUFF, data.abort.message);
      if (n > 0) {
        memcpy(session[session_idx].qmerror, buff->data.abort.message, n);
        buff->data.abort.message[n] = '\0';
      }
    }
    goto exit_qmconnect;
  }
  /* Now attempt to attach to required account */
  if (!message_pair(SrvrAccount, account, strlen(account))) {
    goto exit_qmconnect;
  }
  session[session_idx].context = CX_CONNECTED;
  status = TRUE;
exit_qmconnect:
  if (!status)
    CloseSocket();
  return status;
}

/* ======================================================================
   QMConnected()  -  Are we connected?                                    */
int DLLEntry QMConnected() {
  ClearError;
  return (session[session_idx].context == CX_DISCONNECTED) ? FALSE : TRUE;
}

/* ======================================================================
   QMDcount()  -  Count fields, values or subvalues                       */
int DLLEntry QMDcount(char* src, char* delim_str) {
  int32_t src_len;
  char* p;
  int32_t ct = 0;
  unsigned char delim;  /* note need unsigned char for memchr to work > 127 character, is this a c++ builder thing? */

  /* initialise_client();  */
  if (strlen(delim_str) != 0) {
	delim = *delim_str;
	src_len = strlen(src);
	if (src_len != 0) {
	  ct = 1;
	  while ((p = memchr(src, delim, src_len)) != NULL) {
        src_len -= (1 + p - src);
        src = p + 1;
		ct++;
      }
	}
  }
  return ct;
}

/* ======================================================================
   QMDebug()  -  Turn on/off packet debugging                             */
void DLLEntry QMDebug(bool mode)
{
  if (mode && (srvr_debug == NULL)) /* Turn on */
  {
    srvr_debug = fopen("C:\\CLIDBG", "wt");
  }
  if (!mode && (srvr_debug != NULL)) /* Turn off */
  {
    fclose(srvr_debug);
    srvr_debug = NULL;
  }
}


/* ======================================================================
   QMDisconnect()  -  Close connection to server.                         */
void DLLEntry QMDisconnect() {
  if (session[session_idx].context != CX_DISCONNECTED) {
    disconnect();
  }
}

/* ======================================================================
   QMDisconnectAll()  -  Close connection to all servers.                 */
void DLLEntry QMDisconnectAll() {
  int16_t i;
  for (i = 0; i < MAX_SESSIONS; i++) {
    if (session[session_idx].context != CX_DISCONNECTED) {
      session_idx = i;
      disconnect();
    }
  }
}

/* ======================================================================
   QMError()  -  Return extended error string                             */
char* DLLEntry QMError() {
  return session[session_idx].qmerror;
}

/* ======================================================================
   QMExecute()  -  Execute a command                                      */
char* DLLEntry QMExecute(char* cmnd, int* err) {
  int32_t reply_len = 0;
  char* reply;
  if (context_error(CX_CONNECTED))
    goto exit_qmexecute;
  if (!message_pair(SrvrExecute, cmnd, strlen(cmnd))) {
    goto exit_qmexecute;
  }
  switch (session[session_idx].server_error) {
    case SV_PROMPT:
      session[session_idx].context = CX_EXECUTING;
      /* **** FALL THROUGH **** */
    case SV_OK:
      reply_len = buff_bytes - offsetof(INBUFF, data.execute.reply);
      break;
    case SV_ON_ERROR:
      Abort("EXECUTE generated an abort event", TRUE);
      break;
  }
exit_qmexecute:
  reply = malloc(reply_len + 1);
  strcpy(reply, buff->data.execute.reply);
  *err = session[session_idx].server_error;
  return reply;
}
/* ======================================================================
   QMExtract()  -  Extract field, value or subvalue                       */
char* DLLEntry QMExtract(char* src, int fno, int vno, int svno) {
  int32_t src_len;
  char* p;
  char* result;
  initialise_client();
  src_len = strlen(src);
  if (src_len == 0)
    goto null_result; /* Extracting from null string */
  /* Setp 1  -  Initialise variables */
  if (fno < 1)
    fno = 1;
  /* Step 2  -  Position to start of item */
  /* Skip to start field */
  while (--fno) {
	p = memchr(src, U_FIELD_MARK, src_len);
	if (p == NULL)
      goto null_result; /* No such field */
    src_len -= (1 + p - src);
    src = p + 1;
  }
  p = memchr(src, U_FIELD_MARK, src_len);
  if (p != NULL)
    src_len = p - src; /* Adjust to ignore later fields */
  if (vno < 1)
    goto done; /* Extracting whole field */
  /* Skip to start value */
  while (--vno) {
	p = memchr(src, U_VALUE_MARK, src_len);
    if (p == NULL)
      goto null_result; /* No such value */
    src_len -= (1 + p - src);
    src = p + 1;
  }
  p = memchr(src, U_VALUE_MARK, src_len);
  if (p != NULL)
    src_len = p - src; /* Adjust to ignore later values */
  if (svno < 1)
    goto done; /* Extracting whole value */
  /* Skip to start subvalue */
  while (--svno) {
	p = memchr(src, U_SUBVALUE_MARK, src_len);
    if (p == NULL)
      goto null_result; /* No such subvalue */
    src_len -= (1 + p - src);
    src = p + 1;
  }
  p = memchr(src, U_SUBVALUE_MARK, src_len);
  if (p != NULL)
    src_len = p - src; /* Adjust to ignore later fields */
done:
  result = malloc(src_len + 1);
  memcpy(result, src, src_len);
  result[src_len] = '\0';
  return result;
null_result:
  return NullString();
}
/* ======================================================================
   QMFree()  -  Free memory returned by other functions                   */
void DLLEntry QMFree(void* p) {
  free(p);
}

/* ======================================================================
   QMGetSession()  -  Return session index                                */
int DLLEntry QMGetSession() {
  return session_idx;
}

/* ======================================================================
   QMIns()  -  Insert field, value or subvalue                            */
char* DLLEntry QMIns(char* src, int fno, int vno, int svno, char* new) {
  int32_t src_len;
  char* pos;        /* Rolling source pointer */
  int32_t bytes;   /* Remaining bytes counter */
  int32_t ins_len; /* Length of inserted data */
  int32_t new_len;
  char* new_str;
  char* p;
  int i;   /* resolves CWE-197 check, "Comparison of narrow type with wide type in loop condition." */
  int32_t n;
  int16_t fm = 0;
  int16_t vm = 0;
  int16_t sm = 0;
  unsigned char postmark = '\0';
  initialise_client();
  src_len = strlen(src);
  ins_len = strlen(new);
  pos = src;
  bytes = src_len;
  if (fno < 1)
    fno = 1;
  if (vno < 0)
    vno = 0;
  if (svno < 0)
    svno = 0;
  if (src_len == 0) { /* Inserting in null string */
    if (fno > 1)
      fm = fno - 1;
    if (vno > 1)
      vm = vno - 1;
    if (svno > 1)
      sm = svno - 1;
    goto done;
  }
  /* Skip to start field */
  for (i = 1; i < fno; i++) {
	p = memchr(pos, U_FIELD_MARK, bytes);
    if (p == NULL) { /* No such field */
      fm = fno - i;
      if (vno > 1)
        vm = vno - 1;
      if (svno > 1)
        sm = svno - 1;
      pos = src + src_len;
      goto done;
    }
    bytes -= (1 + p - pos);
    pos = p + 1;
  }
  p = memchr(pos, U_FIELD_MARK, bytes);
  if (p != NULL)
    bytes = p - pos; /* Length of field */
  if (vno == 0) { /* Inserting field */
	postmark = U_FIELD_MARK;
    goto done;
  }
  /* Skip to start value */
  for (i = 1; i < vno; i++) {
	p = memchr(pos, U_VALUE_MARK, bytes);
    if (p == NULL) { /* No such value */
      vm = vno - i;
      if (svno > 1)
        sm = svno - 1;
      pos += bytes;
      goto done;
    }
    bytes -= (1 + p - pos);
    pos = p + 1;
  }
  p = memchr(pos, U_VALUE_MARK, bytes);
  if (p != NULL)
    bytes = p - pos; /* Length of value, excluding end mark */
  if (svno == 0) { /* Inserting value */
	postmark = U_VALUE_MARK;
    goto done;
  }
  /* Skip to start subvalue */
  for (i = 1; i < svno; i++) {
	p = memchr(pos, U_SUBVALUE_MARK, bytes);
    if (p == NULL) { /* No such subvalue */
      sm = svno - i;
      pos += bytes;
      goto done;
    }
    bytes -= (1 + p - pos);
    pos = p + 1;
  }
  postmark = U_SUBVALUE_MARK;
done:
  /* Now construct new string inserting fm, vm and sm marks and new data
    at 'pos'.                                                           */
  n = pos - src; /* Length of leading substring */
  if ((n == src_len) || (IsDelim(src[n]) && src[n] > postmark)) { /* 0380 */
    postmark = '\0';
  }
  new_len = src_len + fm + vm + sm + ins_len + (postmark != '\0');
  new_str = malloc(new_len + 1);
  p = new_str;
  if (n) {
    memcpy(p, src, n);
    p += n;
  }
  while (fm--)
	*(p++) = U_FIELD_MARK;
  while (vm--)
	*(p++) = U_VALUE_MARK;
  while (sm--)
    *(p++) = U_SUBVALUE_MARK;
  if (ins_len) {
    memcpy(p, new, ins_len);
    p += ins_len;
  }
  if (postmark != '\0')
    *(p++) = postmark;
  n = src_len - n; /* Length of trailing substring */
  if (n) {
    memcpy(p, pos, n);
    p += n;
  }
  *p = '\0';
  return new_str;
}

/* ======================================================================
   QMLocate()  -  Search dynamic array                                    */

int DLLEntry QMLocate(char* item,
                      char* src,
                      int fno,
                      int vno,
                      int svno,
                      int* pos,
                      char* order) {
  int item_len;
  int src_len;
  char* p;
  char* q;
  bool ascending = TRUE;
  bool left = TRUE;
  bool sorted = FALSE;
  int16_t idx = 1;
  int d;
  bool found = FALSE;
  int i;
  int bytes;
  char mark;
  int n;
  int x;
  char* s1;
  char* s2;

  initialise_client();

  /* Establish sort mode */

  i = strlen(order);
  if (i >= 1) {
    sorted = TRUE;
    ascending = (order[0] != 'D');
  }

  if (i >= 2)
    left = (order[1] != 'R');

  item_len = strlen(item); /* Length of item to find */

  src_len = strlen(src);

  p = src;
  bytes = src_len;

  if (fno < 1)
    fno = 1;

  /* Scan to start field */

  mark = U_FIELD_MARK;
  idx = fno;
  for (i = 1; i < fno; i++) {
    if (bytes == 0)
      goto done;
	q = memchr(p, U_FIELD_MARK, bytes);
    if (q == NULL)
      goto done; /* No such field */
    bytes -= (1 + q - p);
    p = q + 1;
  }

  if (vno > 0) /* Searching for value or subvalue */
  {
	q = memchr(p, U_FIELD_MARK, bytes);
    if (q != NULL)
      bytes = q - p; /* Limit view to length of field */

	mark = U_VALUE_MARK;
    idx = vno;
    for (i = 1; i < vno; i++) {
      if (bytes == 0)
        goto done;
	  q = memchr(p, U_VALUE_MARK, bytes);
      if (q == NULL)
        goto done; /* No such value */
      bytes -= (1 + q - p);
      p = q + 1;
    }

    if (svno > 0) /* Searching for subvalue */
    {
	  q = memchr(p, U_VALUE_MARK, bytes);
      if (q != NULL)
        bytes = q - p; /* Limit view to length of value */

	  mark = U_SUBVALUE_MARK;
      idx = svno;
      for (i = 1; i < svno; i++) /* 0512 */
      {
        if (bytes == 0)
          goto done;
        q = memchr(p, U_SUBVALUE_MARK, bytes);
        if (q == NULL)
          goto done; /* No such subvalue */
        bytes -= (1 + q - p);
        p = q + 1;
      }
    }
  }

  /* We are now at the start position for the search and 'mark' contains the
    delimiting mark character.  Because we have adjusted 'bytes' to limit
    our view to the end of the item, we do not need to worry about higher
    level marks.  Examine successive items from this point.                 */

  if (bytes == 0) {
    if (item_len == 0)
      found = TRUE;
    goto done;
  }

  do {
    q = memchr(p, mark, bytes);
    n = (q == NULL) ? bytes : (q - p); /* Length of entry */
    if ((n == item_len) && (memcmp(p, item, n) == 0)) {
      found = TRUE;
      goto done;
    }

    if (sorted) /* Check if we have gone past correct position */
    {
      if (left || (n == item_len)) {
        d = memcmp(p, item, min(n, item_len));
        if (d == 0) {
          if ((n > item_len) == ascending)
            goto done;
        } else if ((d > 0) == ascending)
          goto done;
      } else /* Right sorted and lengths differ */
      {
        x = n - item_len;
        s1 = p;
        s2 = item;
        if (x > 0) /* Array entry longer than item to find */
        {
          do {
            d = *(s1++) - ' ';
          } while ((d == 0) && --x);
        } else /* Array entry shorter than item to find */
        {
          do {
            d = ' ' - *(s2++);
          } while ((d == 0) && ++x);
        }
        if (d == 0)
          d = memcmp(s1, s2, min(n, item_len));
        if ((d > 0) == ascending)
          goto done;
      }
    }

    bytes -= (1 + q - p);
    p = q + 1;
    idx++;
  } while (q);

done:
  *pos = idx;
  return found;
}


/* ======================================================================
   QMOpen()  -  Open file                                                 */
int DLLEntry QMOpen(char* filename) {
  int fno = 0;
  if (context_error(CX_CONNECTED))
    goto exit_qmopen;
  if (!message_pair(SrvrOpen, filename, strlen(filename))) {
    goto exit_qmopen;
  }
  switch (session[session_idx].server_error) {
    case SV_OK:
      fno = ShortInt(buff->data.open.fno);
      break;
    case SV_ON_ERROR:
    case SV_ELSE:
    case SV_ERROR:
      break;
  }
exit_qmopen:
  return fno;
}

/* ======================================================================
   QMRead()  -  Read record                                               */
char* DLLEntry QMRead(int fno, char* id, int* err) {
  return read_record(fno, id, err, SrvrRead);
}

/* ======================================================================
   QMReadl()  -  Read record with shared lock                             */
char* DLLEntry QMReadl(int fno, char* id, int wait, int* err) {
  return read_record(fno, id, err, (wait) ? SrvrReadlw : SrvrReadl);
}

/* ======================================================================
   QMReadu()  -  Read record with exclusive lock                          */
char* DLLEntry QMReadu(int fno, char* id, int wait, int* err) {
  return read_record(fno, id, err, (wait) ? SrvrReaduw : SrvrReadu);
}

/* ======================================================================
   QMRecordlock()  -  Get lock on a record                                */
void DLLEntry QMRecordlock(int fno, char* id, int update_lock, int wait) {
  int id_len;
  int16_t flags;
  struct {
    int16_t fno;
    int16_t flags; /* 0x0001 : Update lock */
                     /* 0x0002 : No wait */
    char id[MAX_ID_LEN];
  } ALIGN2 packet;
  if (!context_error(CX_CONNECTED)) {
    packet.fno = ShortInt(fno);
    id_len = strlen(id);
    if (id_len > MAX_ID_LEN) {
      session[session_idx].server_error = SV_ON_ERROR;
      session[session_idx].qm_status = ER_IID;
    } else {
      memcpy(packet.id, id, id_len);
      flags = (update_lock) ? 1 : 0;
      if (!wait)
        flags |= 2;
      packet.flags = ShortInt(flags);
      if (!message_pair(SrvrLockRecord, (char*)&packet, id_len + 4)) {
        session[session_idx].server_error = SV_ON_ERROR;
      }
    }
    switch (session[session_idx].server_error) {
      case SV_OK:
        break;
      case SV_ON_ERROR:
        Abort("RECORDLOCK generated an abort event", TRUE);
        break;
      case SV_LOCKED:
      case SV_ELSE:
      case SV_ERROR:
        break;
    }
  }
}
/* ======================================================================
   QMRelease()  -  Release lock                                           */
void DLLEntry QMRelease(int fno, char* id) {
  int id_len;
  struct {
    int16_t fno;
    char id[MAX_ID_LEN];
  } ALIGN2 packet;
  if (context_error(CX_CONNECTED))
    goto exit_release;
  packet.fno = ShortInt(fno);
  if (fno == 0) /* Release all locks */
  {
    id_len = 0;
  } else {
    id_len = strlen(id);
    if (id_len > MAX_ID_LEN) {
      session[session_idx].server_error = SV_ON_ERROR;
      session[session_idx].qm_status = ER_IID;
      goto release_error;
    }
    memcpy(packet.id, id, id_len);
  }
  if (!message_pair(SrvrRelease, (char*)&packet, id_len + 2)) {
    goto exit_release;
  }
release_error:
  switch (session[session_idx].server_error) {
    case SV_OK:
      break;
    case SV_ON_ERROR:
      Abort("RELEASE generated an abort event", TRUE);
      break;
    case SV_LOCKED:
    case SV_ELSE:
    case SV_ERROR:
      break;
  }
exit_release:
  return;
}
/* ======================================================================
   QMReplace()  -  Replace field, value or subvalue                       */
char* DLLEntry QMReplace(char* src, int fno, int vno, int svno, char* new) {
  int32_t src_len;
  char* pos;        /* Rolling source pointer */
  int32_t bytes;   /* Remaining bytes counter */
  int32_t ins_len; /* Length of inserted data */
  int32_t new_len;
  char* new_str;
  char* p;
  int i;
  int32_t n;
  int16_t fm = 0;
  int16_t vm = 0;
  int16_t sm = 0;
  initialise_client();
  src_len = strlen(src);
  ins_len = strlen(new);
  pos = src;
  bytes = src_len;
  if (src_len == 0) /* Replacing in null string */
  {
    if (fno > 1)
      fm = fno - 1;
    if (vno > 1)
      vm = vno - 1;
    if (svno > 1)
      sm = svno - 1;
    bytes = 0;
    goto done;
  }
  if (fno < 1) /* Appending a new field */
  {
    pos = src + src_len;
    fm = 1;
    if (vno > 1)
      vm = vno - 1;
    if (svno > 1)
      sm = svno - 1;
    bytes = 0;
    goto done;
  }
  /* Skip to start field */
  for (i = 1; i < fno; i++) {
	p = memchr(pos,U_FIELD_MARK, bytes);
    if (p == NULL) /* No such field */
    {
      fm = fno - i;
      if (vno > 1)
        vm = vno - 1;
      if (svno > 1)
        sm = svno - 1;
      pos = src + src_len;
      bytes = 0;
      goto done;
    }
    bytes -= (1 + p - pos);
    pos = p + 1;
  }
  p = memchr(pos,U_FIELD_MARK, bytes);
  if (p != NULL)
    bytes = p - pos; /* Length of field */
  if (vno == 0)
    goto done; /* Replacing whole field */
  if (vno < 0) /* Appending new value */
  {
    if (p != NULL)
      pos = p; /* 0553 */
    else
      pos += bytes; /* 0553 */
    if (bytes)
      vm = 1; /* 0553 */
    if (svno > 1)
      sm = svno - 1;
    bytes = 0;
    goto done;
  }
  /* Skip to start value */
  for (i = 1; i < vno; i++) {
	p = memchr(pos, U_VALUE_MARK, bytes);
    if (p == NULL) /* No such value */
    {
      vm = vno - i;
      if (svno > 1)
        sm = svno - 1;
      pos += bytes;
      bytes = 0;
      goto done;
    }
    bytes -= (1 + p - pos);
    pos = p + 1;
  }
  p = memchr(pos, U_VALUE_MARK, bytes);
  if (p != NULL)
    bytes = p - pos; /* Length of value, including end mark */
  if (svno == 0)
    goto done; /* Replacing whole value */
  if (svno < 1) /* Appending new subvalue */
  {
    if (p != NULL)
      pos = p; /* 0553 */
    else
      pos += bytes; /* 0553 */
    if (bytes)
      sm = 1; /* 0553 */
    bytes = 0;
    goto done;
  }
  /* Skip to start subvalue */
  for (i = 1; i < svno; i++) {
	p = memchr(pos, U_SUBVALUE_MARK, bytes);
    if (p == NULL) /* No such subvalue */
    {
      sm = svno - i;
      pos += bytes;
      bytes = 0;
      goto done;
    }
    bytes -= (1 + p - pos);
    pos = p + 1;
  }
  p = memchr(pos, U_SUBVALUE_MARK, bytes);
  if (p != NULL)
    bytes = p - pos; /* Length of subvalue, including end mark */
done:
  /* Now construct new string with 'bytes' bytes omitted starting at 'pos',
    inserting fm, vm and sm marks and new data                             */
  new_len = src_len - bytes + fm + vm + sm + ins_len;
  new_str = malloc(new_len + 1);
  p = new_str;
  n = pos - src; /* Length of leading substring */
  if (n) {
    memcpy(p, src, n);
    p += n;
  }
  while (fm--)
	*(p++) = U_FIELD_MARK;
  while (vm--)
	*(p++) = U_VALUE_MARK;
  while (sm--)
    *(p++) = U_SUBVALUE_MARK;
  if (ins_len) {
    memcpy(p, new, ins_len);
    p += ins_len;
  }
  n = src_len - (bytes + n); /* Length of trailing substring */
  if (n) {
    memcpy(p, pos + bytes, n);
    p += n;
  }
  *p = '\0';
  return new_str;
}

/* ======================================================================
   QMSetSession()  -  Set session index                                   */
int DLLEntry QMSetSession(int idx) {
  if ((idx < 0) || (idx >= MAX_SESSIONS) ||
      (session[idx].context == CX_DISCONNECTED)) {
    return FALSE;
  }
  session_idx = idx;
  return TRUE;
}

/* ======================================================================
   QMStatus()  -  Return STATUS() value                                   */
int DLLEntry QMStatus() {
  return session[session_idx].qm_status;
}

/* ======================================================================
   QMWrite()  -  Write record                                             */
void DLLEntry QMWrite(int fno, char* id, char* data) {
  write_record(SrvrWrite, fno, id, data);
}

/* ======================================================================
   QMWriteu()  -  Write record, retaining lock                            */
void DLLEntry QMWriteu(int fno, char* id, char* data) {
  write_record(SrvrWriteu, fno, id, data);
}


/* ==================  static functions start below ===================== */

/* ====================================================================== */

Private void initialise_client() {
  int16_t i;
  /* if buff has been allocated, we have been here once before, skip */
  if (buff == NULL) {
	set_default_character_maps();
	buff_size = 2048;
    buff = (INBUFF*)malloc(buff_size);
	for (i = 0; i < MAX_SESSIONS; i++) {
	  session[i].context = CX_DISCONNECTED;
	  session[i].is_local = FALSE;
	  session[i].qmerror[0] = '\0';
	  session[i].hPipe = INVALID_HANDLE_VALUE;
	  session[i].sock = INVALID_SOCKET;
	}
	  /* initialize the arg pointer array for EngCall */
	for (i = 0; i < MAX_ARGS; i++) {
	  CallArgArray[i] = NULL;
	  CallArgArraySz[i] = 0;
	}
  }
}

/* ====================================================================== */
Private bool FindFreeSession() {
  int16_t i;
  /* Find a free session table entry */
  for (i = 0; i < MAX_SESSIONS; i++) {
	if (session[i].context == CX_DISCONNECTED)
	  break;
  }
  if (i == MAX_SESSIONS) {
	/* Must return error via a currently connected session */
	strcpy(session[session_idx].qmerror, "Too many sessions");
	return FALSE;
  }
  session_idx = i;
  return TRUE;
}

/* ====================================================================== */

Private void disconnect() {
  int16_t i;
  if (srvr_debug != NULL) {
	fprintf(srvr_debug, "Disconnect session %d \n", session_idx);
    fflush(srvr_debug);
  }
  (void)write_packet(SrvrQuit, NULL, 0);
  if (session[session_idx].is_local) {
	if (session[session_idx].hPipe != INVALID_HANDLE_VALUE) {
	  CloseHandle(session[session_idx].hPipe);
	  session[session_idx].hPipe = INVALID_HANDLE_VALUE;
    }
  } else
	(void)CloseSocket();
  session[session_idx].context = CX_DISCONNECTED;
 /* I cannot find where the original transfer buffer "buff" is freed on exit
	So look for connected session, if none, release buffer                */
  for (i = 0; i < MAX_SESSIONS; i++) {
	if (session[i].context == CX_CONNECTED)
	  break;
  }

  if (i == MAX_SESSIONS) {
	/* looked at all the sessions and none connected free buff */
	if (buff != NULL) {
	  free(buff);
	  buff = NULL;
	}
   /* free the callx arg buffers if they are still around */
   /* this needs modifing if we go with more than 1 session, see comments at top */
	for (i = 0; i < MAX_ARGS; i++) {
	  if (CallArgArray[i] != NULL ){
		free(CallArgArray[i]);
		CallArgArray[i] = NULL;
		CallArgArraySz[i] = 0;
	}
  }

  }
}

/* ======================================================================
   message_pair()  -  Send message and receive response                  
bool message_pair(session, type, data, bytes) struct SESSION* session;
int type;
unsigned char* data;
int32_t bytes;
{
  if (write_packet(session, type, data, bytes)) {
    return GetResponse(session);
  }
  return FALSE;
} */

/* ======================================================================
   message_pair()  -  Send packet and receive response                    */
Private bool message_pair(int type, char* data, int32_t bytes) {
  if (write_packet(type, data, bytes)) {
    return GetResponse();
  }
  return FALSE;
}
/* ====================================================================== */

Private char* NullString() {
  char* p;
  p = malloc(1);
  *p = '\0';
  return p;
}

/* ======================================================================
   OpenSocket()  -  Open connection to server                            */
Private bool OpenSocket(char* host, int16_t port) {
  bool status = FALSE;
  WSADATA wsadata;
  u_int32_t nInterfaceAddr;
  struct sockaddr_in sock_addr;
  int nPort;
  struct hostent* hostdata;
  char ack_buff;
  int n;
  unsigned int n1, n2, n3, n4;
  if (port < 0)
    port = 4243;
  /* Start Winsock up */
  if (WSAStartup(MAKEWORD(1, 1), &wsadata) != 0) {
    sprintf(session->qmerror, "WSAStartup error");
    goto exit_opensocket;
  }
  if ((sscanf(host, "%u.%u.%u.%u", &n1, &n2, &n3, &n4) == 4) && (n1 <= 255) &&
      (n2 <= 255) && (n3 <= 255) && (n4 <= 255)) {
    /* Looks like an IP address */
    nInterfaceAddr = inet_addr(host);
  } else {
	hostdata = gethostbyname(host);
    if (hostdata == NULL) {
	  net_error("gethostbyname()");
      goto exit_opensocket;
    }
    nInterfaceAddr = *((int32_t*)(hostdata->h_addr));
  }
  nPort = htons(port);
 session[session_idx].sock = socket(AF_INET, SOCK_STREAM, 0);
  if (session[session_idx].sock == INVALID_SOCKET) {
	net_error("socket()");
	goto exit_opensocket;
  }
  sock_addr.sin_family = AF_INET;
  sock_addr.sin_addr.s_addr = nInterfaceAddr;
  sock_addr.sin_port = nPort;
  if (connect(session[session_idx].sock, (struct sockaddr*)&sock_addr, sizeof(sock_addr))) {
	net_error("connect()");
	goto exit_opensocket;
  }
  n = TRUE;
  setsockopt(session[session_idx].sock, IPPROTO_TCP, TCP_NODELAY, (char*)&n, sizeof(int));
  /* Wait for an Ack character to arrive before we assume the connection
    to be open and working. This is necessary because Linux loses anything
    we send before the QM process is up and running.                       */
  do {
	if (recv(session[session_idx].sock, &ack_buff, 1, 0) < 1)
      goto exit_opensocket;
  } while (ack_buff != '\x06');
  status = 1;
exit_opensocket:
  return status;
}

/* ======================================================================
   CloseSocket()  -  Close connection to server                          */
Private bool CloseSocket() {
  bool status = FALSE;
  if (session[session_idx].sock == INVALID_SOCKET)
    goto exit_closesocket;
  closesocket(session[session_idx].sock);
  session[session_idx].sock = INVALID_SOCKET;
  status = TRUE;
exit_closesocket:
  return status;
}
/* ======================================================================
   NetErr                                                               */
static void net_error(char* prefix) {
  char msg[80];
   sprintf(msg, "Error %d from %s", WSAGetLastError(), prefix);
}
/* ======================================================================
   read_packet()  -  Read a QM data packet                                */
Private bool read_packet() {
  int rcvd_bytes;       /* Length of received packet fragment */
  int32_t packet_bytes; /* Total length of incoming packet */
  int rcv_len;
  int32_t n;
  unsigned char* p;
  /* 0272 restructured */
  struct {
    int32_t packet_length;
    int16_t server_error ALIGN2;
    int32_t status ALIGN2;
  } in_packet_header;
#define IN_PKT_HDR_BYTES 10
  if ((!session[session_idx].is_local) && (session[session_idx].sock == INVALID_SOCKET))
    return FALSE;
  /* Read packet header */
  p = (char*)&in_packet_header;
  buff_bytes = 0;
  do {
	rcv_len = IN_PKT_HDR_BYTES - buff_bytes;
	if (session[session_idx].is_local) {
	  if (!ReadFile(session[session_idx].hPipe, p, rcv_len, (DWORD*)&rcvd_bytes, NULL))
        return FALSE;
    } else {
	  if ((rcvd_bytes = recv(session[session_idx].sock, p, rcv_len, 0)) <= 0)
        return FALSE;
    }
	buff_bytes += rcvd_bytes;
    p += rcvd_bytes;
  } while (buff_bytes < IN_PKT_HDR_BYTES);
  if (srvr_debug != NULL)
    debug((char*)&in_packet_header, IN_PKT_HDR_BYTES);
  /* Calculate remaining bytes to read */
  packet_bytes = in_packet_header.packet_length - IN_PKT_HDR_BYTES;
  if (srvr_debug != NULL) {
	fprintf(srvr_debug, "IN (%d bytes)\n", packet_bytes);
    fflush(srvr_debug);
  }
  if (packet_bytes >= buff_size) /* Must reallocate larger buffer */
  {
	free(buff);
	n = (packet_bytes + BUFF_INCR) & ~(BUFF_INCR - 1);
	buff = (INBUFF*)malloc(n);
	if (buff == NULL)
	  return FALSE;
	buff_size = n;
    if (srvr_debug != NULL) {
	  fprintf(srvr_debug, "Resized buffer to %d bytes\n", buff_size);
	  fflush(srvr_debug);
	}
  }
  /* Read data part of packet */
  p = (char*)(buff);
  buff_bytes = 0;
  while (buff_bytes < packet_bytes) {
	rcv_len = min(buff_size - buff_bytes, 16384);
	if (session[session_idx].is_local) {
	  if (!ReadFile(session[session_idx].hPipe, p, rcv_len, (DWORD*)&rcvd_bytes, NULL))
		return FALSE;
    } else {
	  if ((rcvd_bytes = recv(session[session_idx].sock, p, rcv_len, 0)) <= 0)
        return FALSE;
    }
	buff_bytes += rcvd_bytes;
	p += rcvd_bytes;
  }
  ((char*)(buff))[buff_bytes] = '\0';
  if (srvr_debug != NULL)
    debug((char*)(buff), buff_bytes);
  session->server_error = in_packet_header.server_error;
  session->qm_status = in_packet_header.status;
  return TRUE;
}

/* ======================================================================
   write_packet()  -  Send QM data packet  from  qmclient                 */

Private bool write_packet(int type,
                          char* data,
                          int32_t bytes) {
  DWORD n;

  struct {
	int32_t length;
    int16_t type;
  } ALIGN2 packet_header;
#define PKT_HDR_BYTES 6

  if ((!session[session_idx].is_local) && (session[session_idx].sock == INVALID_SOCKET))
	return FALSE;

  packet_header.length = bytes + PKT_HDR_BYTES; /* 0272 */
  packet_header.type = type;
  if (session[session_idx].is_local) {
	if (!WriteFile(session[session_idx].hPipe, (char*)&packet_header, PKT_HDR_BYTES, &n,
                   NULL)) {
      return FALSE;
    }
  } else {
	if (send(session[session_idx].sock, (unsigned char*)&packet_header, PKT_HDR_BYTES, 0) !=
        PKT_HDR_BYTES)
      return FALSE;
  }

  if (srvr_debug != NULL) {
	fprintf(srvr_debug, "OUT (%d bytes). Type %d\n", packet_header.length,
			(int)packet_header.type);
    fflush(srvr_debug);
  }

  if ((data != NULL) && (bytes > 0)) {
	if (session[session_idx].is_local) {
	  if (!WriteFile(session[session_idx].hPipe, data, (DWORD)bytes, &n, NULL)) {
        return FALSE;
      }
    } else {
	  if (send(session[session_idx].sock, data, bytes, 0) != bytes)
        return FALSE;
    }

    if (srvr_debug != NULL)
      debug(data, bytes);
  }

  return TRUE;
}

/* ======================================================================
   context_error()  - Check for appropriate context                       */
Private bool context_error(int16_t expected) {
  /* the char* p throws a warning about the variable being set, but never used.
 * The annoying thing about this is that p is being set to useful values,
 * but that detailed information is never making it out of the routine.
 * It appears that many of these extended descriptions are also living
 * in qmclient.c where they're used as part of a Windows message box
 * output.  I'm going to comment out the use here in order to clean up the
 * error, but some method of leveraging this extended information should be
 * developed. -gwb 23Feb20
 */
  // char* p; triggers a variable set but never used warning.
  if (session[session_idx].context != expected) {
    switch (session[session_idx].context) {
      case CX_DISCONNECTED:
        //   p = "A server function has been attempted when no connection has been "
        //       "established";
        break;
      case CX_CONNECTED:
        switch (expected) {
          case CX_DISCONNECTED:
            // p = "A function has been attempted which is not allowed when a "
            //     "connection has been established";
            break;
          case CX_EXECUTING:
            // p = "Cannot send a response or end a command when not executing a "
            //     "server command";
            break;
        }
        break;
      case CX_EXECUTING:
        //   p = "A new server function has been attempted while executing a server "
        //       "command";
        break;
      default:
        // p = "A function has been attempted in the wrong context";
        break;
    }
    return TRUE;
  }
  return FALSE;
}

/* ======================================================================
   read_record()  -  Common path for READ, READL and READU                */
Private char* read_record(int fno, char* id, int* err, int mode) {
  int32_t status;
  int32_t rec_len = 0;
  int id_len;
  char* rec;
  struct {
    int16_t fno;
    char id[MAX_ID_LEN];
  } ALIGN2 packet;
  if (context_error(CX_CONNECTED))
    goto exit_read;
  id_len = strlen(id);
  if ((id_len < 1) || (id_len > MAX_ID_LEN)) {
    session[session_idx].qm_status = ER_IID;
    status = SV_ON_ERROR;
    goto exit_read;
  }
  packet.fno = ShortInt(fno);
  memcpy(packet.id, id, id_len);
  if (!message_pair(mode, (char*)&packet, id_len + 2)) {
    goto exit_read;
  }
  switch (session[session_idx].server_error) {
    case SV_OK:
      rec_len = buff_bytes - offsetof(INBUFF, data.read.rec);
      break;
    case SV_ON_ERROR:
      Abort("Read generated an abort event", TRUE);
      break;
    case SV_LOCKED:
    case SV_ELSE:
    case SV_ERROR:
      break;
  }
  status = session[session_idx].server_error;
exit_read:
  rec = malloc(rec_len + 1);
  memcpy(rec, buff->data.read.rec, rec_len);
  rec[rec_len] = '\0';
  *err = status;
  return rec;
}

/* ====================================================================== */

Private void write_record(int16_t mode, int16_t fno, char* id, char* data) {
  int id_len;
  int32_t data_len;
  int bytes;
  INBUFF* q;
  struct PACKET {
    int16_t fno;
    int16_t id_len;
    char id[1];
  } ALIGN2;
  if (context_error(CX_CONNECTED))
    goto exit_write;
  id_len = strlen(id);
  if ((id_len < 1) || (id_len > MAX_ID_LEN)) {
    Abort("Illegal record id", FALSE);
    session[session_idx].qm_status = ER_IID;
    goto exit_write;
  }
  data_len = strlen(data);
  /* Ensure buffer is big enough for this record */
  bytes = sizeof(struct PACKET) + id_len + data_len;
  if (bytes >= buff_size) /* Must reallocate larger buffer */
  {
    bytes = (bytes + BUFF_INCR - 1) & ~BUFF_INCR;
    q = (INBUFF*)malloc(bytes);
    if (q == NULL) {
      Abort("Insufficient memory", FALSE);
      session[session_idx].qm_status = ER_SRVRMEM;
      goto exit_write;
    }
    free(buff);
    buff = q;
    buff_size = bytes;
  }
  /* Set up outgoing packet */
  ((struct PACKET*)buff)->fno = ShortInt(fno);
  ((struct PACKET*)buff)->id_len = ShortInt(id_len);
  memcpy(((struct PACKET*)buff)->id, id, id_len);
  memcpy(((struct PACKET*)buff)->id + id_len, data, data_len);
  if (!message_pair(mode, (char*)buff,
                    offsetof(struct PACKET, id) + id_len + data_len)) {
    goto exit_write;
  }
  switch (session[session_idx].server_error) {
    case SV_OK:
      break;
    case SV_ON_ERROR:
      Abort("Write generated an abort event", TRUE);
      break;
    case SV_LOCKED:
    case SV_ELSE:
    case SV_ERROR:
      break;
  }
exit_write:
  return;
}


/* ====================================================================== */
bool GetResponse() {
  if (!read_packet())
    return FALSE;
  if (session[session_idx].server_error == SV_ERROR) {
	strcpy(session[session_idx].qmerror, "Unable to retrieve error text");
	write_packet(SrvrGetError, NULL, 0);
	if (read_packet())
	  strcpy(session[session_idx].qmerror, buff->data.error.text);
	return FALSE;
  }
  return TRUE;
}

/* ====================================================================== */

Private void Abort(char* msg, bool use_response) {
  char abort_msg[1024 + 1];
  int n;
  char* p;
  strcpy(abort_msg, msg);
  if (use_response) {
    n = buff_bytes - offsetof(INBUFF, data.abort.message);
    if (n > 0) {
      p = abort_msg + strlen(msg);
      *(p++) = '\r';
      memcpy(p, buff->data.abort.message, n);
      *(p + n) = '\0';
    }
  }
  fprintf(stderr, "%s\n", abort_msg);
}



/* ======================================================================
   debug()  -  Debug function                                             */
Private void debug(unsigned char* p, int n) {
  int16_t i;
  int16_t j;
  unsigned char c;
  char s[72 + 1];
  static char hex[] = "0123456789ABCDEF";
  for (i = 0; i < n; i += 16) {
    memset(s, ' ', 72);
    s[72] = '\0';
    s[0] = hex[i >> 12];
    s[1] = hex[(i >> 8) & 0x0F];
    s[2] = hex[(i >> 4) & 0x0F];
    s[3] = hex[i & 0x0F];
    s[4] = ':';
    s[54] = '|';
    for (j = 0; (j < 16) && ((j + i) < n); j++) {
      c = *(p++);
      s[6 + 3 * j] = hex[c >> 4];
      s[7 + 3 * j] = hex[c & 0x0F];
	  s[56 + j] = ((c >= 32) && (c < 128)) ? c : '.';
    }
    fprintf(srvr_debug, "%s\n", s);
  }
  fprintf(srvr_debug, "\n");
  fflush(srvr_debug);
}

/* Not exactly sure what the following  means, but I need some of the functions from
  ctypes.c. I am going to just copy them as needed (endless issues with importing qm.h and what comes along.....  */
/* When building a simply .o file instead of a DLL, we need to import
	  the routines from ctype.c into this module.                        */
/* #include "ctype.c" */
/* ======================================================================
   Set default_character maps                                             */
void set_default_character_maps() {
  int i;
  int j;
  for (i = 0; i < 256; i++) {
    uc_chars[i] = (char)i;
    lc_chars[i] = (char)i;
    char_types[i] = 0;
  }
  for (i = 'a', j = 'A'; i <= 'z'; i++, j++) {
    uc_chars[i] = j;
    lc_chars[j] = i;
    char_types[i] |= CT_ALPHA;
    char_types[j] |= CT_ALPHA;
  }
  for (i = '0'; i <= '9'; i++) {
    char_types[i] |= CT_DIGIT;
  }
  for (i = 33; i <= 126; i++) {
    char_types[i] |= CT_GRAPH;
  }
  char_types[U_TEXT_MARK] |= CT_MARK;
  char_types[U_SUBVALUE_MARK] |= CT_MARK | CT_DELIM;
  char_types[U_VALUE_MARK] |= CT_MARK | CT_DELIM;
  char_types[U_FIELD_MARK] |= CT_MARK | CT_DELIM;
  char_types[U_ITEM_MARK] |= CT_MARK;
}


/* END-CODE */
