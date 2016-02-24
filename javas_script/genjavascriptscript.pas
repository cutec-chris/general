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
unit genjavascriptscript;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Utils ,genscript;

type
  { TPythonScript }

  TJavascriptScript = class(TScript)
  private
    Besen : TBESEN;
    FLines : TStringList;
    FRunning: Boolean;
    FStopping : Boolean;
  protected
    function GetTyp: string; override;
  public
    procedure Init; override;
    function IsRunning: Boolean; override;
    function GetStatus: TScriptStatus; override;
    function Stop: Boolean; override;
    function Execute(aParameters: Variant;Debug : Boolean = false): Boolean; override;
    destructor Destroy; override;
  end;

implementation

var
  aScript : TJavascriptScript;

function TJavascriptScript.GetTyp: string;
begin
  Result := 'JavaScript';
end;

procedure TJavascriptScript.Init;
begin
  FRunning:=False;
  FStopping:=False;
  if not Assigned(Besen) then Besen := TBESEN.Create;
end;

function TJavascriptScript.IsRunning: Boolean;
begin
  Result:=FRunning;
end;

function TJavascriptScript.GetStatus: TScriptStatus;
begin
  if IsRunning then
    Result := ssRunning
  else Result := ssNone;
end;

function TJavascriptScript.Stop: Boolean;
begin
  if IsRunning then
    begin
      FStopping := True;
      Result:=True;
    end;
end;

function TJavascriptScript.Execute(aParameters: Variant; Debug: Boolean): Boolean;
var
  aLine: Integer=0;
  aPosition : Integer=0;
  i: Integer;
begin
  Result := False;
  try
    FRunning := True;
    //Exec
    Besen.Execute(Source);
    FRunning:=False;
    Result := True;
  except
    on e : Exception do
      begin
        if Assigned(Writeln) then
          Writeln(e.Message);
        FRunning:=False;
        FreeAndNil(FLines);
        Init;
        FStopping:=False;
      end;
  end;
end;

destructor TJavascriptScript.Destroy;
begin
  if Assigned(Besen) then FreeAndNil(Besen);
  inherited Destroy;
end;

initialization

end.

