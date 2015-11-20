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
Created 08.08.2014
*******************************************************************************}
unit genluascript;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,genscript,lua53;

type
  TLuaScript = class(TScript)
  private
  protected
    function GetTyp: string; override;
  public
    function Execute(aParameters: Variant;Debug : Boolean = false): Boolean; override;
  end;

var
  FScript : TLuaScript;

implementation

function TLuaScript.GetTyp: string;
begin
  Result := 'Lua';
end;

procedure LuaLineInfo(L : Plua_State;ar : Plua_Debug);cdecl;
begin
  if Assigned(FScript) and Assigned(FScript.OnRunLine) then
    FScript.OnRunLine(FScript,'',ar^.currentline,ar^.currentline,0);
end;

function TLuaScript.Execute(aParameters: Variant; Debug: Boolean): Boolean;
var
  L: Plua_State;
  res: integer;
begin
  FScript := Self;
  Result := False;
  L := luaL_newstate();
  lua_sethook(L, @LuaLineInfo, LUA_MASKLINE, 0);
  luaL_openlibs(L);
  res := luaL_dostring(L,PChar(Source));
  Result := res=0;
  lua_close(L);
end;

end.

