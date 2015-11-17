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
unit genscript;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Variants, Utils;

type
  TScript = class;
  TLineEvent = procedure(Sender : TScript;Module : string;Position,Row,Col : Integer) of object;
  TLineMessageEvent = procedure(Sender : TScript;Module,Message : string;Position,Row,Col : Integer) of object;
  TScriptStatus = (ssNone,ssRunning,ssPaused);
  { TScript }

  TScript = class
  private
    FCompileMessage: TLineMessageEvent;
    FDWrFunc: TStrOutFunc;
    FIdle: TNotifyEvent;
    FResults: string;
    FRlFunc: TStrInFunc;
    FRunLine: TLineEvent;
    FSource: string;
    FStatus: char;
    FStatusChanged: TNotifyEvent;
    FWrFunc: TStrOutFunc;
    FWriFunc: TStrOutFunc;
    procedure SetStatus(AValue: char);
  protected
    function GetTyp: string;virtual;abstract;
    procedure InternalWrite(const s: string);
    procedure InternalWriteln(const s: string);
    procedure InternalDebugln(const s: string);
    procedure InternalReadln(var s: string);
    procedure SetSource(AValue: string);virtual;
    function GetStatus: TScriptStatus;virtual;
  public
    Parameters : Variant;
    function Execute(aParameters : Variant;Debug : Boolean = false) : Boolean;virtual;
    procedure Init;virtual;
    property Source : string read FSource write SetSource;
    property Status : TScriptStatus read GetStatus;
    property Results : string read FResults write FResults;
    property Write : TStrOutFunc read FWriFunc write FWriFunc;
    property Writeln : TStrOutFunc read FWrFunc write FWRFunc;
    property Debugln : TStrOutFunc read FDWrFunc write FDWRFunc;
    property Readln : TStrInFunc read FRlFunc write FRlFunc;
    property Typ : string read GetTyp;
    function StepInto : Boolean;virtual;
    function StepOver : Boolean;virtual;
    function Pause : Boolean;virtual;
    function Resume : Boolean;virtual;
    function Stop : Boolean;virtual;
    function IsRunning : Boolean;virtual;
    function GetVarContents(Identifier : string) : string;virtual;
    property OnStatusChanged : TNotifyEvent read FStatusChanged write FStatusChanged;
    property OnCompileMessage : TLineMessageEvent read FCompileMessage write FCompileMessage;
    property OnRunLine : TLineEvent read FRunLine write FRunLine;
    property OnIdle : TNotifyEvent read FIdle write FIdle;
  end;
  TScriptClass = class of TScript;

  { TByteCodeScript }

  TByteCodeScript = class(TScript)
  private
    FByteCode: string;
  protected
    procedure SetSource(AValue: string); override;
  public
    property ByteCode : string read FByteCode write FByteCode;
    function Compile : Boolean;virtual;abstract;
    constructor Create;virtual;
  end;

implementation

procedure TByteCodeScript.SetSource(AValue: string);
begin
  inherited SetSource(AValue);
  ByteCode:='';
end;

constructor TByteCodeScript.Create;
begin
  ByteCode := '';
end;
procedure TScript.SetStatus(AValue: char);
begin
  if FStatus=AValue then Exit;
  FStatus:=AValue;
  if Assigned(FStatusChanged) then
    FStatusChanged(Self);
end;

function TScript.GetStatus: TScriptStatus;
begin
  Result := ssNone;
end;

procedure TScript.SetSource(AValue: string);
begin
  if FSource=AValue then Exit;
  FSource:=AValue;
end;

procedure TScript.InternalWrite(const s: string);
begin
  if Assigned(FWriFunc) then FWriFunc(s);
end;

procedure TScript.InternalWriteln(const s: string);
begin
  if Assigned(FWrFunc) then FWrFunc(s);
end;

procedure TScript.InternalDebugln(const s: string);
begin
  if Assigned(FDWrFunc) then FDWrFunc(s);
end;

procedure TScript.InternalReadln(var s: string);
begin
  if Assigned(FRlFunc) then FRlFunc(s);
end;

function TScript.Execute(aParameters: Variant; Debug: Boolean): Boolean;
begin
  Parameters:=aParameters;
end;

procedure TScript.Init;
begin
end;

function TScript.StepInto: Boolean;
begin
  Result := False;
end;

function TScript.StepOver: Boolean;
begin
  Result := False;
end;

function TScript.Pause: Boolean;
begin
  Result := False;
end;

function TScript.Resume: Boolean;
begin
  Result := False;
end;

function TScript.Stop: Boolean;
begin
  Result := False;
end;

function TScript.IsRunning: Boolean;
begin
  Result := False;
end;

function TScript.GetVarContents(Identifier: string): string;
begin
  Result := '';
end;

end.

