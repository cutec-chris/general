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
  { TPythonScript }

  TPythonScript = class(TScript)
    procedure fInternalsInitialization(Sender: TObject);
  private
    fEngine : TPythonEngine;
    fIO: TPythonInputOutput;
    fInternals: TPythonModule;
    FLines : TStringList;
    FRunning: Boolean;
    FStopping : Boolean;
    procedure fIOReceiveData(Sender: TObject; var Data: AnsiString);
    procedure fIOSendData(Sender: TObject; const Data: AnsiString);
  protected
    function GetTyp: string; override;
  public
    procedure Init; override;
    function IsRunning: Boolean; override;
    function GetStatus: TScriptStatus; override;
    function Stop: Boolean; override;
    function Execute(aParameters: Variant;Debug : Boolean = false): Boolean; override;
    function GetVarContents(Identifier: string): string; override;
    function RunScriptFunction(const Params: array of Variant; aName: string
  ): Variant; override;
    destructor Destroy; override;
  end;

implementation

var
  aScript : TPythonScript;

function prometinternals_callline(self, args: PPyObject
  ): PPyObject; cdecl;
var
  aArg : Variant;
begin
  aArg := aScript.fEngine.PyObjectAsVariant(args);
  if Assigned(aScript.OnRunLine) then
    aScript.OnRunLine(aScript,'',0,aArg[0],0);
  if not aScript.FStopping then
    Result:=aScript.fEngine.ReturnNone
  else
    Result:=nil;
end;

procedure TPythonScript.fInternalsInitialization(Sender: TObject);
begin
  aScript := Self;
  with Sender as TPythonModule do
    AddMethod( '_CallLineInfo', @prometinternals_callline, '_CallLineInfo' );
end;

procedure TPythonScript.fIOReceiveData(Sender: TObject; var Data: AnsiString);
begin
  if Assigned(Readln) then
    Readln(Data);
end;

procedure TPythonScript.fIOSendData(Sender: TObject; const Data: AnsiString);
var
  aData: String;
begin
  if Assigned(WriteLn) then
    begin
      if pos(';prometinternals._CallLineInfo(',Data)>0 then
        aData := copy(Data,0,pos(';prometinternals._CallLineInfo(',Data))
      else aData := Data;
      WriteLn(aData);
    end;
end;

function TPythonScript.GetTyp: string;
begin
  Result := 'Python';
end;

procedure TPythonScript.Init;
begin
  FLines:=nil;
  FRunning:=False;
  FStopping:=False;
  FreeAndNil(fEngine);
  fEngine := TPythonEngine.Create(nil);
  fIO := TPythonInputOutput.Create(nil);
  fIO.OnReceiveData:=@fIOReceiveData;
  fIO.OnSendData:=@fIOSendData;
  FreeAndNil(fInternals);
  fInternals := TPythonModule.Create(nil);
  fInternals.OnInitialization:=@fInternalsInitialization;
  fInternals.ModuleName:='prometinternals';
  fInternals.Engine:=fEngine;
  fEngine.InitScript.Add('import prometinternals');
  fEngine.IO := fIO;
  fEngine.RedirectIO:=True;
  fEngine.FatalAbort:=False;
  fEngine.FatalMsgDlg:=True;
  fEngine.Initialize;
end;

function TPythonScript.IsRunning: Boolean;
begin
  Result:=FRunning;
end;

function TPythonScript.GetStatus: TScriptStatus;
begin
  if IsRunning then
    Result := ssRunning
  else Result := ssNone;
end;

function TPythonScript.Stop: Boolean;
begin
  if IsRunning then
    begin
      FreeAndNil(FLines);
      FStopping := True;
      Result:=True;
    end;
end;

function TPythonScript.Execute(aParameters: Variant; Debug: Boolean): Boolean;
var
  aLine: Integer=0;
  aPosition : Integer=0;
  i: Integer;
  aData: String;
begin
  Result := False;
  try
    FRunning := True;
    if Debug then
      begin
        FLines := TStringList.Create;
        FLines.Text:=Source;
        for i := 0 to FLines.Count-1 do
          if (copy(trim(FLines[i]),length(trim(FLines[i])),1)<>':')
          and (trim(FLines[i])<>'')
          then
            FLines[i] := FLines[i]+';prometinternals._CallLineInfo('+IntToStr(i)+')';
        fEngine.ExecString(FLines.Text);
        FreeAndNil(FLines);
      end
    else fEngine.ExecString(Source);
    FRunning:=False;
    Result := True;
  except
    on e : Exception do
      begin
        if pos(';prometinternals._CallLineInfo(',e.Message)>0 then
          aData := copy(e.Message,0,pos(';prometinternals._CallLineInfo(',e.Message))
        else aData := e.Message;
        if Assigned(Writeln) then
          Writeln(aData);
        FRunning:=False;
        FreeAndNil(FLines);
        Init;
        FStopping:=False;
      end;
  end;
end;

function TPythonScript.GetVarContents(Identifier: string): string;
begin
  Result:='';//TODO:implement me
end;

function TPythonScript.RunScriptFunction(const Params: array of Variant;
  aName: string): Variant;
begin
  Result:=Null;//TODO:implement me
end;

destructor TPythonScript.Destroy;
begin
  FreeAndNil(fIO);
  FreeAndNil(fEngine);
  FreeAndNil(fInternals);
  inherited Destroy;
end;

initialization

end.

