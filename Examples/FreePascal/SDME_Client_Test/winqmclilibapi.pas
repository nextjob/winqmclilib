unit winQmCliLibAPI;
{ winQMCliLibAPI wrapper for Free Pascal developed with
  extreme reliance on tisQMClientAPI.pas / tisQMClient.pas  see below.

 THIS SOFTWARE IS PROVIDED "AS IS" AND ANY
 EXPRESSED OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS BE LIABLE FOR ANY
 DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
}

{The Delphi4QM Delphi and Free Pascal wrapper for The QMClient C API is
 distributed under a BSD style license.

 tisQMClientAPI.pas
 Delphi4QM - A Delphi and Free Pascal Wrapper for the QMClient C API
 Copyright (c) 2006-2013, Trident Information Systems, Inc.
 All rights reserved.

 Redistribution and use in source and binary forms, with or without
 modification, is permitted provided that the following conditions are met:

     * Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.
     * Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in the
       documentation and/or other materials provided with the distribution.
     * The name, "Trident Information Systems" may not be used to endorse
       or promote products derived from this software without specific prior
       written permission.

 THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS "AS IS" AND ANY
 EXPRESSED OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS BE LIABLE FOR ANY
 DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
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
 
 
}


interface
//uses ctypes; // living dangerously and not using


procedure QMCallx(subrname: PAnsiChar; argc: Integer; a1, a2, a3: PAnsiChar); cdecl;
procedure QMClose(fno: Integer); cdecl;
function  QMConnect(host: PAnsiChar; port: Integer; username, password, account: PAnsiChar): Integer; cdecl;
function  QMConnected: Integer; cdecl;
function  QMDcount(src, delim: PAnsiChar): Integer; cdecl;
procedure QMDebug(mode: Int16); cdecl;
procedure QMDisconnect; cdecl;
procedure QMDisconnectAll; cdecl;
function  QMError: PAnsiChar; cdecl;
function  QMExecute(cmmd: PAnsiChar; err: PInteger): PAnsiChar; cdecl;
function  QMExtract(src: PAnsiChar; fno, vno, svno: Integer): PAnsiChar; cdecl;
procedure QMFree(p: PAnsiChar); cdecl;
function  QmGetArg(ArgNo: integer): PAnsiChar; cdecl;
function  QMGetSession: Integer; cdecl;
function  QMIns(src: PAnsiChar; fno, vno, svno: Integer; new_string: PAnsiChar): PAnsiChar; cdecl;
function  QMOpen(filename: PAnsiChar): Integer; cdecl;
function  QMRead(fno: Integer; id: PAnsiChar; var err: Integer): PAnsiChar; cdecl;
function  QMReadu(fno: Integer; id: PAnsiChar; wait: Integer; var err: Integer): PAnsiChar; cdecl;
function  QmSetSession(sess: integer): Integer; cdecl;
function  QMStatus : Integer; cdecl;
procedure QMWrite(fno: Integer; id, data: PAnsiChar); cdecl;

implementation

procedure QMCallx(subrname: PAnsiChar; argc: Integer; a1, a2, a3: PAnsiChar); cdecl; external 'winqmclilib.dll' name 'QMCallx';
procedure QMClose(fno: Integer); cdecl; external 'winqmclilib.dll' name 'QMClose';
function  QMConnect(host: PAnsiChar; port: Integer; username, password, account: PAnsiChar): Integer; cdecl; external 'winqmclilib.dll' name 'QMConnect';
function  QMConnected: Integer; cdecl; external 'winqmclilib.dll' name 'QMConnected';
procedure QMDisconnect; cdecl; external 'winqmclilib.dll' name 'QMDisconnect';
procedure QMDisconnectAll; cdecl; external 'winqmclilib.dll' name 'QMDisconnectAll';
function  QMDcount(src, delim: PAnsiChar): Integer; cdecl; external 'winqmclilib.dll' name 'QMDcount';
function  QMError: PAnsiChar; cdecl; external 'winqmclilib.dll' name 'QMError';
function  QMExecute(cmmd: PAnsiChar; err: PInteger): PAnsiChar; cdecl; external 'winqmclilib.dll' name 'QMExecute';
function  QMExtract(src: PAnsiChar; fno, vno, svno: Integer): PAnsiChar; cdecl; external 'winqmclilib.dll' name 'QMExtract';
procedure QMFree(p: PAnsiChar); cdecl; external 'winqmclilib.dll' name 'QMFree';
function  QmGetArg(ArgNo: integer): PAnsiChar; cdecl; external 'winqmclilib.dll' name 'QMGetArg';
function  QMGetSession: Integer; cdecl; external 'winqmclilib.dll' name 'QMGetSession';
function  QMIns(src: PAnsiChar; fno, vno, svno: Integer; new_string: PAnsiChar): PAnsiChar; cdecl; external 'winqmclilib.dll' name 'QMIns';
procedure QMDebug(mode: Int16 ); cdecl; external 'winqmclilib.dll' name 'QMDebug';
function  QMOpen(filename: PAnsiChar): Integer; cdecl; external 'winqmclilib.dll' name 'QMOpen';
function  QMRead(fno: Integer; id: PAnsiChar; var err: Integer): PAnsiChar; cdecl; external 'winqmclilib.dll' name 'QMRead';
function  QMReadu(fno: Integer; id: PAnsiChar; wait: Integer;  var err: Integer): PAnsiChar; cdecl; external 'winqmclilib.dll' name 'QMReadu';
function  QmSetSession(sess: integer): Integer; cdecl; external 'winqmclilib.dll' name 'QmSetSession';
function  QMStatus : Integer; cdecl; external 'winqmclilib.dll' name 'QMStatus';
procedure QMWrite(fno: Integer; id, data: PAnsiChar); cdecl; external 'winqmclilib.dll' name 'QMWrite';

end.
