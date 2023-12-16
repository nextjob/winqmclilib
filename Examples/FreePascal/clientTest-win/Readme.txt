 Very minimal testing of winqmclilib
 connect to server
 open a file and read a record
  this also test QMDcount and QMExtract
 change the record in the text box (and the record id if you want)
 write it back out
 this also tests QMIns

 Call button tests QMCallx:
 Add the following to your ScarletDME account,compile and catalog:
 
 BP testsub
 
* very simple test of QMCallx
*  open TESTDATA
*  create a record made up of passed values
*  add a field with a time date stamp
*  write the record as TESTSUB
*  return the field with the time date stamp to the caller
01: subroutine testsub(a1, a2, a3)
02: open 'TESTDATA' to TS ELSE ABORT
03: a2rtn = a2:  ' Plus we added TIMEDATE ':TIMEDATE()
04: rec = ''
5:  Rec = a1
06: rec<2> = a2
07: rec<3> = a3
08: rec<4> = a2rtn
09: write rec on TS, "TESTSUB"
10: a2 = a2rtn
11: Return
12: End