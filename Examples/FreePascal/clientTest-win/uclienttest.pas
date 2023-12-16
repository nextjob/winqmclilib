unit uclienttest;
//  very minimal testing of winqmclilib
//  connect to server
//  open a file and read a record
//   this also test QMDcount and QMExtract
//  change the record in the text box (and the record id if you want)
//  write it back out
//  this also tests QMIns
// 
//  Call button tests QMCallx:
//  Add the following to your ScarletDME account
//  BP testsub
// * very simple test of QMCallx
// *  open TESTDATA
// *  create a record made up of passed values
// *  add a field with a time date stamp
// *  write the record as TESTSUB
// *  return the field with the time date stamp to the caller
// 01:  subroutine testsub(a1, a2, a3)
// 02: open 'TESTDATA' to TS ELSE ABORT
// 03:  a2rtn = a2:  ' Plus we added TIMEDATE ':TIMEDATE()
// 04: rec = ''
// 5:  Rec = a1
// 06: rec<2> = a2
// 07: rec<3> = a3
// 08: rec<4> = a2rtn
// 09: write rec on TS, "TESTSUB"
// 10: a2 = a2rtn
// 11: Return
// 12: End
//
// LINUX Note: for this to compile and run  we need to copy  ./bin/libqmcli.so to /usr/lib/libqmcli.so
// If you forget when running program from Lazarus IDE you will get:  "Execution stopped with exit-code 127 ($007F)"
// If you view the console window (view -> debug windows -> console in/out
// you will see a message about qmclilib.so not found.
// cannot figure out LD_LIBRARY_PATH setting with Lazarus, and linker options do not seem to work.
// needs to be there on a running system anyway. need to investigate adding to EngMakeFile
//
// note on {$codePage CP1252}:
//
//  if we use the default codepage  ?Utf8? we will have problems with LCL controls becuase of
//  Item mark char(255)
//  Field mark char(254)
//  Value mark char(253)
//  Subvalue mark char(252)
//  Text mark char(251)
//
//  Rem for records:
//  Field Mark
//        Value Mark
//              SubValue Mark
//
//  need to find a work around if using lcl controls directly on ScarletDME records
//

{$mode objfpc}{$H+}
{$codePage CP1252}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, MaskEdit,
  SynEdit, winqmclilibapi;

type

  { TForm1 }

  TForm1 = class(TForm)
    btnLogin: TButton;
    btnOpen: TButton;
    btnRead: TButton;
    btnClose: TButton;
    btnWrite: TButton;
    Button1: TButton;
    Button2: TButton;
    edtAddress: TEdit;
    EdtPass: TEdit;
    edtuser: TEdit;
    edtRecId: TEdit;
    edtFileName: TEdit;
    edtAccount: TEdit;
    edtFileOpen: TMaskEdit;
    Label1: TLabel;
    Label2: TLabel;
    edtFileClose: TMaskEdit;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Memo1: TMemo;
    SynEdit1: TSynEdit;
    procedure btnCloseClick(Sender: TObject);
    procedure btnLoginClick(Sender: TObject);
    procedure btnOpenClick(Sender: TObject);
    procedure btnReadClick(Sender: TObject);
    procedure btnWriteClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
  private

  public

  end;

  // define needed QMCLient functions not currently found in tisQMClient
{  function QMCallx(subrname: PAnsiChar; argc: integer;
    a1, a2, a3: PAnsiChar): integer;
    cdecl; external;
  function QmGetArg(ArgNo: integer): PAnsiChar; cdecl;
    external;
  procedure QMFree(p: PAnsiChar); cdecl; external;
}
const
  //QM_Client Stuff
  QM_server = '127.0.0.1';
  QM_port = -1;

  //  qm field delimiters

  QM_AM = #254;  // attribute or field mark
  QM_VM = #253;  // value or item mark
  QM_SVM = #252;  // subvalue mark

  //* QM Server error status values */
  SV_OK = 0;   //* Action successful                       */
  SV_ON_ERROR = 1;   //* Action took ON ERROR clause             */
  SV_ELSE = 2;   //* Action took ELSE clause                 */
  SV_ERROR = 3;   //* Action failed. Error text available     */
  SV_LOCKED = 4;   //* Action took LOCKED clause               */
  SV_PROMPT = 5;   //* Server requesting input                 */

var
  Form1: TForm1;


implementation

{$R *.lfm}

{ TForm1 }

// Note: I would suggest using RawByeString instead for the input...
function BinStr2Hex(S: AnsiString{RawByteString}): AnsiString;
begin
  SetLength(Result, Length(S)*2);
  BinToHex(PAnsiChar(S), PAnsiChar(Result), Length(S));
  // or: BinToHex(S[1], PAnsiChar(Result), Length(S));
  // or: BinToHex(Pointer(S), PAnsiChar(Result), Length(S));
  Result := AnsiLowerCase(Result);
end;

procedure TForm1.FormShow(Sender: TObject);
begin
   edtAddress.text := '192.168.0.253';
   edtUser.Text := 'username';
   edtAccount.Text := 'QMTEST';
end;

procedure TForm1.btnReadClick(Sender: TObject);
var
  fileno, err, lns, ln : integer;
//  myrec : RawByteString;
  sptr  : PAnsiChar;
  myrec, myline : AnsiString;

begin
  if TryStrToInt(edtFileOpen.Text,fileno) then
    if fileno > 0 then
      Begin
//      myrec := EngRead(fileno,PAnsiChar(edtRecId.Text), Perr);
        sptr := QMRead(fileno,PAnsiChar(edtRecId.Text), err);
        myrec := PAnsiChar(sptr);
        QMFree(sptr);
//        SetCodePage(RawByteString(myrec), CP1252, False);
        Memo1.Lines.Add('Read Results of : ' + edtFileName.Text + ' ' + edtRecId.Text + ' err: ' + IntToStr(err));
        err := QMStatus();
        Memo1.Lines.Add('QMStatus: ' + intTostr(err) + ' QMError: ' + QMerror());
        Memo1.Lines.Add('rec: ' + myrec);
        Memo1.Lines.Add('end of record');
        Memo1.Lines.Add('Hex:');
        Memo1.Lines.Add(BinStr2Hex(myrec));
        Memo1.Lines.Add('end of record');
//  break record down by fields and create a line in sysedit
        if err = 0 then
          Begin
            SynEdit1.Lines.Clear;
            lns := QMDcount(PAnsiChar(myrec),PAnsiChar(QM_AM));
            Memo1.Lines.Add('Lines / Fields in rec: ' + intToStr(lns));
            for ln := 1 to lns do
              begin
                sptr := QMExtract(PAnsiChar(myrec),Ln,0,0);
                myline :=  PAnsiChar(sptr);
                QMFree(sptr);
                SynEdit1.Lines.Add(myline);
              end;
          end;

      end;

end;

procedure TForm1.btnWriteClick(Sender: TObject);
var
  fileno, err, Lns : integer;
  myrec : AnsiString;
  sptr  : PAnsiChar;
begin

  if TryStrToInt(edtFileOpen.Text,fileno) then
    if fileno > 0 then
    Begin
      Lns := Synedit1.lines.Count;
      myrec := '';
      for Lns := 0 to Synedit1.lines.Count-1 do
        Begin
         // rem insert is "insert before"
         // Note, would be a lot easier and quicker to append the field_mark in pascal but I want to test out QMIns
          sptr := QMIns(PAnsiChar(myrec), Lns+1, 0, 0, PAnsiChar(SynEdit1.Lines[Lns]));
          myrec := PAnsiChar(sptr);
          QMFree(sptr);
        end;
       //err := EngWrite(fileno,edtRecId.Text,myrec);
      QMWrite(fileno,PAnsichar(edtRecId.Text),PAnsiChar(myrec));
      Memo1.Lines.Add('Write Results of : ' + edtFileName.Text + ' ' + edtRecId.Text );
      err := QMStatus();
      Memo1.Lines.Add('QMStatus: ' + intTostr(err) + ' QMError: ' + QMerror());
      Memo1.Lines.Add('rec: ' + myrec);
      Memo1.Lines.Add('end of record');
    end;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  subname : AnsiString;
  str1, str2, str3, arg2 : AnsiString;
  sptr  : PAnsiChar;

begin
  // attempt EngCall
  subname := 'testsub';
  str1 := 'my arg 1';
  str2 := 'and arg2';
  str3 := 'last arg 3';

  Memo1.Lines.Add('Attempt to call ' + subname);
  QMCallx(PAnsiChar(subname), 3, PAnsiChar(str1), PAnsiChar(str2), PAnsiChar(str3));
//  QMCall(subname,512,str1,str2,str3);
  Memo1.Lines.Add('Attempt retrieve arg 2');
  sptr := QMGetArg(2);
  str2 := PAnsiChar(sptr);
  QMFree(sptr);
  Memo1.Lines.Add('arg 2: ' + str2);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  if (QMconnected > 0) then
  begin
    QMDisconnect;
    Memo1.Lines.Add('Disconnect');
  end;
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
// if we forget to disconnect, the i/o buffer in the clientAPI does not get freed!
begin
  if (QMconnected > 0) then
  begin
    QMDisconnect;
  end;
end;

procedure TForm1.btnLoginClick(Sender: TObject);
// Rem: If Security is enabled on QM,
//  and user not defined in Secuirty subsytem
//  connect will fail!
//  See QM ref manual
var
  stat : integer;
begin
  if (QMconnected > 0) then
  begin
   Memo1.Lines.Add('QM is currently Connected')
  end else
    begin
          Memo1.Lines.Add( 'Connecting to QM Server: ' + edtAddress.text);
 //  attempt connection with data entered on LoginFrm
          stat := QMConnect(PAnsiChar(edtAddress.text), QM_port, PAnsiChar(edtUser.Text), PAnsiChar(edtPass.Text), PAnsiChar(edtAccount.Text));
          Memo1.Lines.Add('Connection stat: ' + inttostr(stat));
          if (stat = 1) then
             Memo1.Lines.Add('Connected')
          else
            begin
              Memo1.Lines.Add('Connection failed: ' + QMError())
            end;

   end;
end;
procedure TForm1.btnOpenClick(Sender: TObject);
var
  fileno : integer;
begin
  fileno := QMOpen(PAnsiChar(edtFileName.Text));
  if fileno > 0 then
  begin
    edtFileClose.Text := intToStr(fileno);
    edtFileOpen.Text :=  intToStr(fileno);
    Memo1.Lines.Add('Open successful: ' + edtFileName.Text + ' FileNbr: ' + IntToStr(fileno));
  end
  else
     Memo1.Lines.Add('Open unsuccessful, QMStatus: ' + IntToStr(QMStatus()) + '  QMError: ' + QMError());
end;

procedure TForm1.btnCloseClick(Sender: TObject);
var
  fileno : integer;
begin
  if TryStrToInt(edtFileClose.Text,fileno) then
    if fileno > 0 then
      Begin
        QMClose(fileno);
        Memo1.Lines.Add('Closed File Number: ' + IntToStr(fileno));
      end;
end;

end.

