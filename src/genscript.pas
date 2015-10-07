unit genscript;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Variants;

type
  TScript = class
  private
    FResults: string;
    FRunLine: TNotifyEvent;
    FSource: string;
    FStatus: char;
    FStatusChanged: TNotifyEvent;
    procedure SetStatus(AValue: char);
  protected
    function GetTyp: string;virtual;abstract;
  public
    Parameters : Variant;
    function Execute(aParameters : Variant) : Boolean;virtual;
    property Source : string read FSource write FSource;
    property Status : char read FStatus write SetStatus;
    property Results : string read FResults write FResults;
    property Typ : string read GetTyp;
    property OnStatusChanged : TNotifyEvent read FStatusChanged write FStatusChanged;
    property OnRunLine : TNotifyEvent read FRunLine write FRunLine;
  end;
  TScriptClass = class of TScript;

  { TByteCodeScript }

  TByteCodeScript = class(TScript)
  private
    FByteCode: string;
  public
    property ByteCode : string read FByteCode write FByteCode;
    function Compile : Boolean;virtual;abstract;
    constructor Create;virtual;
  end;

implementation

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

function TScript.Execute(aParameters: Variant): Boolean;
begin
  Parameters:=aParameters;
end;

end.

