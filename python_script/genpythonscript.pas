unit genpythonscript;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Utils, genscript, PythonEngine;

type

  { TPythonScript }

  TPythonScript = class(TScript)
    procedure fIOReceiveData(Sender: TObject; var Data: AnsiString);
    procedure fIOSendData(Sender: TObject; const Data: AnsiString);
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
  if Assigned(Readln) then
    Readln(Data);
end;

procedure TPythonScript.fIOSendData(Sender: TObject; const Data: AnsiString);
begin
  if Assigned(WriteLn) then
    WriteLn(Data);
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
  fIO.OnSendData:=@fIOSendData;
  fEngine.IO := fIO;
  fEngine.RedirectIO:=True;
  fEngine.Initialize;
end;

function TPythonScript.Execute(aParameters: Variant): Boolean;
begin
  Result := False;
  try
    fEngine.ExecString(Source);
    Result := True;
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

