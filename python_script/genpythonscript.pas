unit genpythonscript;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Utils, genscript, PythonEngine;

type
  TPythonScript = class(TScript)
    procedure fIOReceiveData(Sender: TObject; var Data: AnsiString);
  private
    fEngine : TPythonEngine;
    fIO: TPythonInputOutput;
  protected
    function GetTyp: string; override;
  public
    procedure Init; override;
    function Execute(aParameters: Variant): Boolean; override;
    destructor Destroy; override;
  end;

implementation

procedure TPythonScript.fIOReceiveData(Sender: TObject; var Data: AnsiString);
begin
  if Assigned(Writeln) then
    Writeln(Data);
end;

function TPythonScript.GetTyp: string;
begin
  Result := 'Python';
end;

procedure TPythonScript.Init;
begin
  fEngine := TPythonEngine.Create(nil);
  fIO := TPythonInputOutput.Create(nil);
  fIO.OnReceiveData:=@fIOReceiveData;
  fEngine.IO := fIO;
  fEngine.Initialize;
end;

function TPythonScript.Execute(aParameters: Variant): Boolean;
begin
  try
    fEngine.ExecString(Source)
  except
    on e : Exception do
      if Assigned(Writeln) then
        Writeln(e.Message);
  end;
end;

destructor TPythonScript.Destroy;
begin
  fIO.Free;
  fEngine.Free;
  inherited Destroy;
end;

initialization

end.

