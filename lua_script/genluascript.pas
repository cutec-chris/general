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
    function Execute(aParameters: Variant): Boolean; override;
  end;

implementation

function TLuaScript.GetTyp: string;
begin
  Result := 'Lua';
end;

function TLuaScript.Execute(aParameters: Variant): Boolean;
var
  L: Plua_State;
  res: integer;
begin
  Result := False;
  {
  L := lua_newstate();
  luaL_openlibs(L);
  res := luaL_dostring(L,Source);
  Result := res=0;
  lua_close(L);
  }
end;

end.

