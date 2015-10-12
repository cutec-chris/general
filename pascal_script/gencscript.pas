unit gencscript;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, genpascalscript, uPSCompiler;

type
  TCScript = class(TPascalScript)
  public
    function Compile: Boolean; override;
  end;

implementation

function TCScript.Compile: Boolean;
begin

end;

end.

