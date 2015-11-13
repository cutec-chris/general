{*******************************************************************************
  Copyright (C) Christian Ulrich info@cu-tec.de

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or commercial alternative
  contact us for more information

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
Created 07.10.2015
*******************************************************************************}
unit genpythonscript;

//TODO:Debugging seems to be only over pdb possible wich could be used from script itself or as commandline tool

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Utils, genscript, PythonEngine;

type
  TPythonThread = class(TThread)
  private
    IOData : string;
    fEngine : TPythonEngine;
    fIO: TPythonInputOutput;
    FScript: TScript;
    procedure fIOReceiveData(Sender: TObject; var Data: AnsiString);
    procedure fIOSendData(Sender: TObject; const Data: AnsiString);
    procedure DoReadData;
    procedure DoWriteData;
  public
    procedure Init;
    procedure Execute; override;
    property Script : TScript read FScript write FScript;
  end;

  { TPythonScript }

  TPythonScript = class(TScript)
  private
    FThread : TPythonThread;
  protected
    function GetTyp: string; override;
  public
    procedure Init; override;
    function Execute(aParameters: Variant;Debug : Boolean = false): Boolean; override;
    destructor Destroy; override;
  end;

implementation

procedure TPythonThread.Execute;
begin
  while not Terminated do
    begin
      fEngine.ExecString(Script.Source);
      Suspend;
    end;
end;

procedure TPythonThread.fIOReceiveData(Sender: TObject; var Data: AnsiString);
begin
  Synchronize(@DoWriteData);
  IOData:=Data;
end;

procedure TPythonThread.fIOSendData(Sender: TObject; const Data: AnsiString);
begin
  IOData:=Data;
  Synchronize(@DoReadData);
end;

procedure TPythonThread.DoReadData;
begin
  if Assigned(Script.WriteLn) then
    Script.WriteLn(IOData);
end;

procedure TPythonThread.DoWriteData;
begin
  if Assigned(Script.Readln) then
    Script.Readln(IOData);
end;

function TPythonScript.GetTyp: string;
begin
  Result := 'Python';
end;

procedure TPythonScript.Init;
begin
  FThread := TPythonThread.Create(True);
  FThread.Script:=Self;
  FThread.Init;
end;

procedure TPythonThread.Init;
begin
  fEngine := TPythonEngine.Create(nil);
  fIO := TPythonInputOutput.Create(nil);
  fIO.OnReceiveData:=@fIOReceiveData;
  fIO.OnSendData:=@fIOSendData;
  fEngine.IO := fIO;
  fEngine.RedirectIO:=True;
  fEngine.InitThreads:=True;
  fEngine.Initialize;
end;

function TPythonScript.Execute(aParameters: Variant; Debug: Boolean): Boolean;
begin
  Result := False;
  try
    FThread.Resume;
    while not FThread.Suspended do
      begin
        if Assigned(Self.OnRunLine) then
          OnRunLine(Self,'',-1,-1,-1)
        else
          sleep(100);
      end;
    Result := True;
  except
    on e : Exception do
      if Assigned(Writeln) then
        Writeln(e.Message);
  end;
end;

destructor TPythonScript.Destroy;
begin
  FreeAndNil(FThread);
  inherited Destroy;
end;

initialization

end.

