  Free Pascal project for very minimal testing of winqmclilib.
  
  Prebuilt copy of winqmclilib.dll is found here:
  https://github.com/nextjob/winqmclilib_dll
  
  Install the Free Pascal IDE Lazarus:
  https://www.lazarus-ide.org/
  
  Click on the project file clientTest.lpr and build
  
  Prebuilt copy of clientTest is found here:
  
  
  Short and not very helpful instructions on how to use clientTest:
  
  Upper Memo will display responses and status codes, lower Memo is used for read / write actions.
  
  Enter Address, User Name, Password and Account (next to login button) in associated fields, click login to connect.
  (Note I have my default info in these fields to speed up testing)
  
   Response in upper memo:
  
    Connecting to QM Server: 192.168.0.253
    Connection stat: 1
    Connected


  Enter a known file name in the field next to the Open button (in my test case the file name is TESTDATA).
  Click the Open button.
   
  Response in the upper memo:
  
	Open successful: TESTDATA FileNbr: 1
	
  Note: Open populates the File Number file for both the Close and Read / Write test.
  
  Enter a known Record Id in the field Record Id and click Read.
  
  Response in upper memo is a bunch of debug info, Response in lower memo should be the actual record with field markers converted to line endings.
  
  Using the lower memo change the record, and click Write.
  
  The record should be updated, verify via terminal logged into ScarletDME.

  The QMExecute button will execute the command selected from the drop down list.
  
  Response should show up in upper memo.
  
  The Call button test Callx, and GetArg by executing the BASIC subroutine found in BP testsub, code below.
  This subroutine must be compiled and cataloged in the account.
  
  Response in upper memo:
  
    Attempt to call testsub
    Attempt retrieve arg 2
    arg 2: and arg2 Plus we added TIMEDATE 09:30:55 19 DEC 2023

  BP testsub: 
 
    * very simple test of QMCallx
    *  open TESTDATA
    *  create a record made up of passed values
    *  add a field with a time date stamp
    *  write the record as TESTSUB
    *  return the field with the time date stamp to the caller
     subroutine testsub(a1, a2, a3)
     open 'TESTDATA' to TS ELSE ABORT
     a2rtn = a2:  ' Plus we added TIMEDATE ':TIMEDATE()
     rec = ''
     rec = a1
     rec<2> = a2
     rec<3> = a3
     rec<4> = a2rtn
     write rec on TS, "TESTSUB"
     a2 = a2rtn
     Return
     End
 
   Click Logout to disconnect.
   
