# winqmclilib
 Windows port of qmclilib
  
  START-HISTORY (winQMclilib):
  
  xxSep23 mab attempt windows port, using c++ builder ver11.3 CE
  
  This port is currently not a complete port of qmclilib.c, at this time I have only ported functions that I currently need.
  Port does not include the call function, but instead the Callx / Getarg functions of newer client versions.
  
  Warning: winqmclilib does not maintian a storage area for Getarg parameters for each session. Using Callx will "overwrite" the previous Callx
   parameters regardless of session number.
  
  Notes: There seems to be an issue with char vs unsigned char when using the memchr c function (void *memchr(const void *str, int c, size_t n).
  If passing the int c parameter as a char, characters > 127 cannot be found (interpreted as a neg integer?).
  The gcc compiler & linux libraries seems to be fine with it, not so with C++ Builder / Windows. Need to use unsigned Char.
	  
  I cannot find where the transfer buffer "buff" is freed, need to test for memory leak to see if this is really the case.
  For now I have added code to disconnect() to free buff if there are no more remaining active connections.
  
  For now I have commented out #include "qmdefs.h" and pulled the required information from the include file into winqmclilib.c
  There is a bunch of stuff in qmdefs.h that will not resolve with c++ builder some of the int defines, bigended stuff. Lazy on my part, yes.
  
  This port was made with C++ Builder ver 11.3 CE, originally I tried to build with MSVC but was unsuccessful.
  (Most likely due to my not being familiar with the tool set.)  Additionally, the qm2.6.6 version was build with Borland C, qm changed to
  Microsoft C in release 3.2-2 

  Addition of missing functions to winQMclilib "should" be a copy and paste from qmclilib.c, however YMMV!	  
  
  This is a work in progress.......
