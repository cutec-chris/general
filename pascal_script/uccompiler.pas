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
Created 14.10.2015
*******************************************************************************}
unit uccompiler;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, uPSCompiler, uPSUtils,genpascalscript;

type
  TOnNotifyEvent = function(Sender : TObject) : Boolean of object;
  { TPSOCParser }

  TPSOCParser = class(TPSPascalParser)
  protected
    FData: TbtString;
    FText: PAnsiChar;
    FLastEnterPos, FRow, FRealPosition, FTokenLength: Cardinal;
    FTokenId: TPSPasToken;
    FToken: TbtString;
    FOriginalToken: TbtString;
    FParserError: TPSParserErrorEvent;
    FEnableComments: Boolean;
    FEnableWhitespaces: Boolean;
    function GetCol: Cardinal;
    // only applicable when Token in [CSTI_Identifier, CSTI_Integer, CSTI_Real, CSTI_String, CSTI_Char, CSTI_HexInt]
  public
    property EnableComments: Boolean read FEnableComments write FEnableComments;
    property EnableWhitespaces: Boolean read FEnableWhitespaces write FEnableWhitespaces;
    procedure Next; virtual;
    property GetToken: TbtString read FToken;
    property OriginalToken: TbtString read FOriginalToken;
    property CurrTokenPos: Cardinal read FRealPosition;
    property CurrTokenID: TPSPasToken read FTokenId;
    property Row: Cardinal read FRow;
    property Col: Cardinal read GetCol;
    procedure SetText(const Data: TbtString); virtual;
    property OnParserError: TPSParserErrorEvent read FParserError write FParserError;
  end;

  { TPSOCCompiler }

  TPSOCCompiler = class(TIPSPascalCompiler)
    procedure ParserError(Parser: TObject; Kind: TPSParserErrorKind);
  private
    FOutput: String;
    FMessages: TPSList;
    FParser: TPSOCParser;
    FAllowDuplicateRegister: Boolean;
    FAllowNoBegin: Boolean;
    FAllowNoEnd: Boolean;
    FAllowUnit: Boolean;
    FBooleanShortCircuit: Boolean;
    FIsUnit: Boolean;
    FRegProcs: TPSList;
    FConstants: TPSList;
    FProcs: TPSList;
    FTypes: TPSList;
    FAttributeTypes: TPSList;
    FVars: TPSList;
    FOnExportCheck: TPSOnExportCheck;
    FOnUses: TPSOnUses;
    FUnitName: tbtString;
    FUtf8Decode: Boolean;
  protected
    FDebugOutput: tbtString;
    FOnExternalProc: TPSOnExternalProc;
    FOnUseVariable: TPSOnUseVariable;
    FOnBeforeOutput: TOnNotifyEvent;
    FOnBeforeCleanup: TOnNotifyEvent;
    FOnWriteLine: TPSOnWriteLineEvent;
    FContinueOffsets, FBreakOffsets: TPSList;
    FOnTranslateLineInfo: TPSOnTranslateLineInfoProc;
    FAutoFreeList: TPSList;
    FClasses: TPSList;
    FParserHadError: Boolean;
    FOnFunctionStart: TPSOnFunction;
    FOnFunctionEnd: TPSOnFunction;
		FWithCount: Integer;
		FTryCount: Integer;
    FExceptFinallyCount: Integer;

    FUnitInits : TPSList; //nvds
    FUnitFinits: TPSList; //nvds
    FUses      : TPSStringList;
    fUnits     : TPSUnitList;
    fUnit      : TPSUnit;
    fModule    : tbtString;
    fInCompile : Integer;

    FCurrUsedTypeNo: Cardinal;
    FGlobalBlock: TPSBlockInfo;

    function IsBoolean(aType: TPSType): Boolean;
    function FindBaseType(BaseType: TPSBaseType): TPSType;

    function IsIntBoolType(aType: TPSType): Boolean;
    function GetTypeCopyLink(p: TPSType): TPSType;

    function GetMsgCount: Longint;
    function GetMsg(l: Longint): TPSPascalCompilerMessage;

    procedure DefineStandardTypes;
    procedure DefineStandardProcedures;

    function MakeExportDecl(decl: TPSParametersDecl): tbtString;
    function NewProc(const OriginalName, Name: tbtString): TPSInternalProcedure;

    procedure WriteDebugData(const s: tbtString);
  public
    function FindProc(const aName: tbtString): Cardinal;
    function MakeDecl(decl: TPSParametersDecl): tbtString;
    function Compile(const s: tbtString): Boolean;
    function GetOutput(var s: tbtString): Boolean;
    function GetDebugOutput(var s: tbtString): Boolean;
    procedure Clear;
    constructor Create;
    destructor Destroy; override;

    property MsgCount: Longint read GetMsgCount;
    property Msg[l: Longint]: TPSPascalCompilerMessage read GetMsg;
    property OnTranslateLineInfo: TPSOnTranslateLineInfoProc read FOnTranslateLineInfo write FOnTranslateLineInfo;
    property OnUses: TPSOnUses read FOnUses write FOnUses;
    property OnExportCheck: TPSOnExportCheck read FOnExportCheck write FOnExportCheck;
    property OnWriteLine: TPSOnWriteLineEvent read FOnWriteLine write FOnWriteLine;
    property OnExternalProc: TPSOnExternalProc read FOnExternalProc write FOnExternalProc;
    property OnUseVariable: TPSOnUseVariable read FOnUseVariable write FOnUseVariable;
    property OnBeforeOutput: TOnNotifyEvent read FOnBeforeOutput write FOnBeforeOutput;
    property OnBeforeCleanup: TOnNotifyEvent read FOnBeforeCleanup write FOnBeforeCleanup;
    property OnFunctionStart: TPSOnFunction read FOnFunctionStart write FOnFunctionStart;
    property OnFunctionEnd: TPSOnFunction read FOnFunctionEnd write FOnFunctionEnd;
    property IsUnit: Boolean read FIsUnit;
    property AllowNoBegin: Boolean read FAllowNoBegin write FAllowNoBegin;
    property AllowUnit: Boolean read FAllowUnit write FAllowUnit;
    property AllowNoEnd: Boolean read FAllowNoEnd write FAllowNoEnd;
    property AllowDuplicateRegister : Boolean read FAllowDuplicateRegister write FAllowDuplicateRegister;
    property BooleanShortCircuit: Boolean read FBooleanShortCircuit write FBooleanShortCircuit;
    property UTF8Decode: Boolean read FUtf8Decode write FUtf8Decode;
    {$WARNINGS OFF}
    property UnitName: tbtString read FUnitName;
    {$WARNINGS ON}
  end;

implementation

{ TPSOCParser }
type
  TRTab = record
    name: TbtString;
    c: TPSPasToken;
  end;

const
  KEYWORD_COUNT = 42;  //*NVDS
  LookupTable: array[0..KEYWORD_COUNT - 1] of TRTab = (
      (name: '&&'; c: CSTII_and),
      (name: 'ARRAY'; c: CSTII_array),
      (name: '{'; c: CSTII_begin),
      (name: 'CASE'; c: CSTII_case),
      (name: 'CHR'; c: CSTII_chr),
      (name: 'CLASS'; c: CSTII_class),
      (name: 'CONST'; c: CSTII_const),
      (name: 'DO'; c: CSTII_do),
      (name: 'DOWNTO'; c: CSTII_downto),
      (name: 'ELSE'; c: CSTII_else),
      (name: '}'; c: CSTII_end),
      (name: 'EXCEPT'; c: CSTII_except),
      (name: 'EXIT'; c: CSTII_exit),
      (name: 'EXPORT'; c: CSTII_Export),
      (name: 'EXTERNAL'; c: CSTII_External),
      (name: 'FOR'; c: CSTII_for),
      (name: 'GOTO'; c: CSTII_Goto),
      (name: 'IF'; c: CSTII_if),
      (name: 'MOD'; c: CSTII_mod),
      (name: 'NIL'; c: CSTII_nil),
      (name: 'NOT'; c: CSTII_not),
      (name: 'OF'; c: CSTII_of),
      (name: 'OR'; c: CSTII_or),
      (name: 'ORD'; c: CSTII_ord),
      (name: 'OUT'; c: CSTII_Out),
      (name: 'OVERRIDE'; c: CSTII_override),
      //(name: 'DEFAULT'; c: CSTII_default), //Birb (if added, don't forget to increase KEYWORD_COUNT)
      (name: 'PROCEDURE'; c: CSTII_procedure),
      (name: 'RECORD'; c: CSTII_record),
      (name: 'REPEAT'; c: CSTII_repeat),
      (name: 'SET'; c: CSTII_set),
      (name: 'SHL'; c: CSTII_shl),
      (name: 'SHR'; c: CSTII_shr),
      (name: 'THEN'; c: CSTII_then),
      (name: 'TO'; c: CSTII_to),
      (name: 'TYPE'; c: CSTII_type),
      (name: 'UNTIL'; c: CSTII_until),
      (name: 'INCLUDE'; c: CSTII_uses),
      (name: 'VAR'; c: CSTII_var),
      (name: 'VIRTUAL'; c: CSTII_virtual),
      (name: 'WHILE'; c: CSTII_while),
      (name: 'WITH'; c: CSTII_with),
      (name: 'XOR'; c: CSTII_xor));


function TPSOCParser.GetCol: Cardinal;
begin

end;

procedure TPSOCParser.Next;
var
  Err: TPSParserErrorKind;
  FLastUpToken: TbtString;
  function CheckReserved(Const S: ShortString; var CurrTokenId: TPSPasToken): Boolean;
  var
    L, H, I: LongInt;
    J: tbtChar;
    SName: ShortString;
  begin
    L := 0;
    J := S[0];
    H := KEYWORD_COUNT-1;
    while L <= H do
    begin
      I := (L + H) shr 1;
      SName := LookupTable[i].Name;
      if J = SName[0] then
      begin
        if S = SName then
        begin
          CheckReserved := True;
          CurrTokenId := LookupTable[I].c;
          Exit;
        end;
        if S > SName then
          L := I + 1
        else
          H := I - 1;
      end else
        if S > SName then
          L := I + 1
        else
          H := I - 1;
    end;
    CheckReserved := False;
  end;
  //-------------------------------------------------------------------

  function _GetToken(CurrTokenPos, CurrTokenLen: Cardinal): TbtString;
  var
    s: tbtString;
  begin
    SetLength(s, CurrTokenLen);
    Move(FText[CurrTokenPos], S[1], CurrtokenLen);
    Result := s;
  end;

  function ParseToken(var CurrTokenPos, CurrTokenLen: Cardinal; var CurrTokenId: TPSPasToken): TPSParserErrorKind;
  {Parse the token}
  var
    ct, ci: Cardinal;
    hs: Boolean;
    p: {$IFDEF DELPHI4UP}PAnsiChar{$ELSE}PChar{$ENDIF};
  begin
    ParseToken := iNoError;
    ct := CurrTokenPos;
    case FText[ct] of
      #0:
        begin
          CurrTokenId := CSTI_EOF;
          CurrTokenLen := 0;
        end;
      'A'..'Z', 'a'..'z', '_':
        begin
          ci := ct + 1;
          while (FText[ci] in ['_', '0'..'9', 'a'..'z', 'A'..'Z']) do begin
            Inc(ci);
          end;
          CurrTokenLen := ci - ct;

          FLastUpToken := _GetToken(CurrTokenPos, CurrtokenLen);
          p := {$IFDEF DELPHI4UP}PAnsiChar{$ELSE}pchar{$ENDIF}(FLastUpToken);
          while p^<>#0 do
          begin
            if p^ in [#97..#122] then
              Dec(Byte(p^), 32);
            inc(p);
          end;
          if not CheckReserved(FLastUpToken, CurrTokenId) then
          begin
            CurrTokenId := CSTI_Identifier;
          end;
        end;
      '$':
        begin
          ci := ct + 1;

          while (FText[ci] in ['0'..'9', 'a'..'f', 'A'..'F'])
            do Inc(ci);

          CurrTokenId := CSTI_HexInt;
          CurrTokenLen := ci - ct;
        end;

      '0'..'9':
        begin
          hs := False;
          ci := ct;
          while (FText[ci] in ['0'..'9']) do
          begin
            Inc(ci);
            if (FText[ci] = '.') and (not hs) then
            begin
              if FText[ci+1] = '.' then break;
              hs := True;
              Inc(ci);
            end;
          end;
          if (FText[ci] in ['E','e']) and ((FText[ci+1] in ['0'..'9'])
            or ((FText[ci+1] in ['+','-']) and (FText[ci+2] in ['0'..'9']))) then
          begin
            hs := True;
            Inc(ci);
            if FText[ci] in ['+','-'] then
              Inc(ci);
            repeat
              Inc(ci);
            until not (FText[ci] in ['0'..'9']);
          end;

          if hs
            then CurrTokenId := CSTI_Real
          else CurrTokenId := CSTI_Integer;

          CurrTokenLen := ci - ct;
        end;


      #39:
        begin
          ci := ct + 1;
          while true do
          begin
            if (FText[ci] = #0) or (FText[ci] = #13) or (FText[ci] = #10) then Break;
            if (FText[ci] = #39) then
            begin
              if FText[ci+1] = #39 then
                Inc(ci)
              else
                Break;
            end;
            Inc(ci);
          end;
          if FText[ci] = #39 then
            CurrTokenId := CSTI_String
          else
          begin
            CurrTokenId := CSTI_String;
            ParseToken := iStringError;
          end;
          CurrTokenLen := ci - ct + 1;
        end;
      '#':
        begin
          ci := ct + 1;
          if FText[ci] = '$' then
          begin
            inc(ci);
            while (FText[ci] in ['A'..'F', 'a'..'f', '0'..'9']) do begin
              Inc(ci);
            end;
            CurrTokenId := CSTI_Char;
            CurrTokenLen := ci - ct;
          end else
          begin
            while (FText[ci] in ['0'..'9']) do begin
              Inc(ci);
            end;
            if FText[ci] in ['A'..'Z', 'a'..'z', '_'] then
            begin
              ParseToken := iCharError;
              CurrTokenId := CSTI_Char;
            end else
              CurrTokenId := CSTI_Char;
            CurrTokenLen := ci - ct;
          end;
        end;
      '=':
        begin
          CurrTokenId := CSTI_Equal;
          CurrTokenLen := 1;
        end;
      '>':
        begin
          if FText[ct + 1] = '=' then
          begin
            CurrTokenid := CSTI_GreaterEqual;
            CurrTokenLen := 2;
          end else
          begin
            CurrTokenid := CSTI_Greater;
            CurrTokenLen := 1;
          end;
        end;
      '<':
        begin
          if FText[ct + 1] = '=' then
          begin
            CurrTokenId := CSTI_LessEqual;
            CurrTokenLen := 2;
          end else
            if FText[ct + 1] = '>' then
            begin
              CurrTokenId := CSTI_NotEqual;
              CurrTokenLen := 2;
            end else
            begin
              CurrTokenId := CSTI_Less;
              CurrTokenLen := 1;
            end;
        end;
      ')':
        begin
          CurrTokenId := CSTI_CloseRound;
          CurrTokenLen := 1;
        end;
      '(':
        begin
          if FText[ct + 1] = '*' then
          begin
            ci := ct + 1;
            while (FText[ci] <> #0) do begin
              if (FText[ci] = '*') and (FText[ci + 1] = ')') then
                Break;
              if FText[ci] = #13 then
              begin
                inc(FRow);
                if FText[ci+1] = #10 then
                  inc(ci);
                FLastEnterPos := ci +1;
              end else if FText[ci] = #10 then
              begin
                inc(FRow);
                FLastEnterPos := ci +1;
              end;
              Inc(ci);
            end;
            if (FText[ci] = #0) then
            begin
              CurrTokenId := CSTIINT_Comment;
              ParseToken := iCommentError;
            end else
            begin
              CurrTokenId := CSTIINT_Comment;
              Inc(ci, 2);
            end;
            CurrTokenLen := ci - ct;
          end
          else
          begin
            CurrTokenId := CSTI_OpenRound;
            CurrTokenLen := 1;
          end;
        end;
      '[':
        begin
          CurrTokenId := CSTI_OpenBlock;
          CurrTokenLen := 1;
        end;
      ']':
        begin
          CurrTokenId := CSTI_CloseBlock;
          CurrTokenLen := 1;
        end;
      ',':
        begin
          CurrTokenId := CSTI_Comma;
          CurrTokenLen := 1;
        end;
      '.':
        begin
          if FText[ct + 1] = '.' then
          begin
            CurrTokenLen := 2;
            CurrTokenId := CSTI_TwoDots;
          end else
          begin
            CurrTokenId := CSTI_Period;
            CurrTokenLen := 1;
          end;
        end;
      '@':
        begin
          CurrTokenId := CSTI_AddressOf;
          CurrTokenLen := 1;
        end;
      '^':
        begin
          CurrTokenId := CSTI_Dereference;
          CurrTokenLen := 1;
        end;
      ';':
        begin
          CurrTokenId := CSTI_Semicolon;
          CurrTokenLen := 1;
        end;
      ':':
        begin
          if FText[ct + 1] = '=' then
          begin
            CurrTokenId := CSTI_Assignment;
            CurrTokenLen := 2;
          end else
          begin
            CurrTokenId := CSTI_Colon;
            CurrTokenLen := 1;
          end;
        end;
      '+':
        begin
          CurrTokenId := CSTI_Plus;
          CurrTokenLen := 1;
        end;
      '-':
        begin
          CurrTokenId := CSTI_Minus;
          CurrTokenLen := 1;
        end;
      '*':
        begin
          CurrTokenId := CSTI_Multiply;
          CurrTokenLen := 1;
        end;
      '/':
        begin
          if FText[ct + 1] = '/' then
          begin
            ci := ct + 1;
            while (FText[ci] <> #0) and (FText[ci] <> #13) and
              (FText[ci] <> #10) do begin
              Inc(ci);
            end;
            if (FText[ci] = #0) then
            begin
              CurrTokenId := CSTIINT_Comment;
            end else
            begin
              CurrTokenId := CSTIINT_Comment;
            end;
            CurrTokenLen := ci - ct;
          end else
          begin
            CurrTokenId := CSTI_Divide;
            CurrTokenLen := 1;
          end;
        end;
      #32, #9, #13, #10:
        begin
          ci := ct;
          while (FText[ci] in [#32, #9, #13, #10]) do
          begin
            if FText[ci] = #13 then
            begin
              inc(FRow);
              if FText[ci+1] = #10 then
                inc(ci);
              FLastEnterPos := ci +1;
            end else if FText[ci] = #10 then
            begin
              inc(FRow);
              FLastEnterPos := ci +1;
            end;
            Inc(ci);
          end;
          CurrTokenId := CSTIINT_WhiteSpace;
          CurrTokenLen := ci - ct;
        end;
      '{':
        begin
          ci := ct + 1;
          while (FText[ci] <> #0) and (FText[ci] <> '}') do begin
            if FText[ci] = #13 then
            begin
              inc(FRow);
              if FText[ci+1] = #10 then
                inc(ci);
              FLastEnterPos := ci + 1;
            end else if FText[ci] = #10 then
            begin
              inc(FRow);
              FLastEnterPos := ci + 1;
            end;
            Inc(ci);
          end;
          if (FText[ci] = #0) then
          begin
            CurrTokenId := CSTIINT_Comment;
            ParseToken := iCommentError;
          end else
            CurrTokenId := CSTIINT_Comment;
          CurrTokenLen := ci - ct + 1;
        end;
    else
      begin
        ParseToken := iSyntaxError;
        CurrTokenId := CSTIINT_Comment;
        CurrTokenLen := 1;
      end;
    end;
  end;
  //-------------------------------------------------------------------
begin
  if FText = nil then
  begin
    FTokenLength := 0;
    FRealPosition := 0;
    FTokenId := CSTI_EOF;
    Exit;
  end;
  repeat
    FRealPosition := FRealPosition + Cardinal(FTokenLength);
    Err := ParseToken(FRealPosition, Cardinal(FTokenLength), FTokenID);
    if Err <> iNoError then
    begin
      FTokenLength := 0;
      FTokenId := CSTI_EOF;
      FToken := '';
      FOriginalToken := '';
      if @FParserError <> nil then FParserError(Self, Err);
      exit;
    end;

    case FTokenID of
      CSTIINT_Comment: if not FEnableComments then Continue else
        begin
          SetLength(FOriginalToken, FTokenLength);
          Move(FText[CurrTokenPos], FOriginalToken[1], FTokenLength);
          FToken := FOriginalToken;
        end;
      CSTIINT_WhiteSpace: if not FEnableWhitespaces then Continue else
        begin
          SetLength(FOriginalToken, FTokenLength);
          Move(FText[CurrTokenPos], FOriginalToken[1], FTokenLength);
          FToken := FOriginalToken;
        end;
      CSTI_Integer, CSTI_Real, CSTI_String, CSTI_Char, CSTI_HexInt:
        begin
          SetLength(FOriginalToken, FTokenLength);
          Move(FText[CurrTokenPos], FOriginalToken[1], FTokenLength);
          FToken := FOriginalToken;
        end;
      CSTI_Identifier:
        begin
          SetLength(FOriginalToken, FTokenLength);
          Move(FText[CurrTokenPos], FOriginalToken[1], FTokenLength);
          FToken := FLastUpToken;
        end;
    else
      begin
        FOriginalToken := '';
        FToken := '';
      end;
    end;
    Break;
  until False;
end;

procedure TPSOCParser.SetText(const Data: TbtString);
begin
  FData := Data;
  FText := Pointer(FData);
  FTokenLength := 0;
  FRealPosition := 0;
  FTokenId := CSTI_EOF;
  FLastEnterPos := 0;
  FRow := 1;
  Next;
end;

{ TPSOCCompiler }

procedure TPSOCCompiler.ParserError(Parser: TObject; Kind: TPSParserErrorKind);
begin

end;

function TPSOCCompiler.IsBoolean(aType: TPSType): Boolean;
begin

end;

function TPSOCCompiler.FindBaseType(BaseType: TPSBaseType): TPSType;
begin

end;

function TPSOCCompiler.IsIntBoolType(aType: TPSType): Boolean;
begin

end;

function TPSOCCompiler.GetTypeCopyLink(p: TPSType): TPSType;
begin

end;

function TPSOCCompiler.GetMsgCount: Longint;
begin

end;

function TPSOCCompiler.GetMsg(l: Longint): TPSPascalCompilerMessage;
begin

end;

procedure TPSOCCompiler.DefineStandardTypes;
begin

end;

procedure TPSOCCompiler.DefineStandardProcedures;
begin

end;

function TPSOCCompiler.MakeExportDecl(decl: TPSParametersDecl): tbtString;
var
  i: Longint;
begin
  if Decl.Result = nil then result := '-1' else
  result := IntToStr(Decl.Result.FinalTypeNo);

  for i := 0 to decl.ParamCount -1 do
  begin
    if decl.Params[i].Mode = pmIn then
      Result := Result + ' @'
    else
      Result := Result + ' !';
    Result := Result + inttostr(decl.Params[i].aType.FinalTypeNo);
  end;
end;

function TPSOCCompiler.NewProc(const OriginalName, Name: tbtString
  ): TPSInternalProcedure;
begin
  Result := TPSInternalProcedure.Create;
  Result.OriginalName := OriginalName;
  Result.Name := Name;
  //Result.DeclareUnit:=fModule;
  Result.DeclarePos := FParser.CurrTokenPos;
  Result.DeclareRow := FParser.Row;
  Result.DeclareCol := FParser.Col;
  FProcs.Add(Result);
end;

procedure TPSOCCompiler.WriteDebugData(const s: tbtString);
begin
  FDebugOutput := FDebugOutput + s;
end;

function TPSOCCompiler.FindProc(const aName: tbtString): Cardinal;
var
  l, h: Longint;
  x: TPSProcedure;
  xr: TPSRegProc;
  name: tbtString;

begin
  name := FastUpperCase(aName);
  h := MakeHash(Name);
  if FProcs = nil then
  begin
    result := InvalidVal;
    Exit;
  end;

  for l := FProcs.Count - 1 downto 0 do
  begin
    x := FProcs.Data^[l];
    if x.ClassType = TPSInternalProcedure then
    begin
      if (TPSInternalProcedure(x).NameHash = h) and
        (TPSInternalProcedure(x).Name = Name) then
      begin
        Result := l;
        exit;
      end;
    end
    else
    begin
      if (TPSExternalProcedure(x).RegProc.NameHash = h) and
        (TPSExternalProcedure(x).RegProc.Name = Name)then
      begin
        Result := l;
        exit;
      end;
    end;
  end;
  for l := FRegProcs.Count - 1 downto 0 do
  begin
    xr := FRegProcs[l];
    if (xr.NameHash = h) and (xr.Name = Name) then
    begin
      x := TPSExternalProcedure.Create;
      TPSExternalProcedure(x).RegProc := xr;
      FProcs.Add(x);
      Result := FProcs.Count - 1;
      exit;
    end;
  end;
  Result := InvalidVal;
end; {findfunc}

function TPSOCCompiler.MakeDecl(decl: TPSParametersDecl): tbtString;
var
  i: Longint;
begin
  if Decl.Result = nil then result := '0' else
  result := Decl.Result.Name;

  for i := 0 to decl.ParamCount -1 do
  begin
    if decl.Params[i].Mode = pmIn then
      Result := Result + ' @'
    else
      Result := Result + ' !';
    Result := Result + decl.Params[i].aType.Name;
  end;
end;

procedure CopyVariantContents(Src, Dest: PIfRVariant);
begin
  case src.FType.BaseType of
    btu8, bts8: dest^.tu8 := src^.tu8;
    btu16, bts16: dest^.tu16 := src^.tu16;
    btenum, btu32, bts32: dest^.tu32 := src^.tu32;
    btsingle: Dest^.tsingle := src^.tsingle;
    btdouble: Dest^.tdouble := src^.tdouble;
    btextended: Dest^.textended := src^.textended;
    btCurrency: Dest^.tcurrency := Src^.tcurrency;
    btchar: Dest^.tchar := src^.tchar;
    {$IFNDEF PS_NOINT64}bts64: dest^.ts64 := src^.ts64;{$ENDIF}
    btset, btstring: tbtstring(dest^.tstring) := tbtstring(src^.tstring);
    {$IFNDEF PS_NOWIDESTRING}
    btunicodestring: tbtunicodestring(dest^.tunistring) := tbtunicodestring(src^.tunistring);
    btwidestring: tbtwidestring(dest^.twidestring) := tbtwidestring(src^.twidestring);
    btwidechar: Dest^.tchar := src^.tchar;
    {$ENDIF}
  end;
end;

function DuplicateVariant(Src: PIfRVariant): PIfRVariant;
begin
  New(Result);
  FillChar(Result^, SizeOf(TIfRVariant), 0);
  CopyVariantContents(Src, Result);
end;

procedure InitializeVariant(Vari: PIfRVariant; FType: TPSType);
begin
  FillChar(vari^, SizeOf(TIfRVariant), 0);
  if FType.BaseType = btSet then
  begin
    SetLength(tbtstring(vari^.tstring), TPSSetType(FType).ByteSize);
    fillchar(tbtstring(vari^.tstring)[1], length(tbtstring(vari^.tstring)), 0);
  end;
  vari^.FType := FType;
end;

function NewVariant(FType: TPSType): PIfRVariant;
begin
  New(Result);
  InitializeVariant(Result, FType);
end;

procedure FinalizeA(var s: tbtString); overload; begin s := ''; end;
{$IFNDEF PS_NOWIDESTRING}
procedure FinalizeW(var s: tbtwidestring); overload; begin s := ''; end;
procedure FinalizeU(var s: tbtunicodestring); overload; begin s := ''; end;
{$ENDIF}
procedure FinalizeVariant(var p: TIfRVariant);
begin
  if (p.FType.BaseType = btString) or (p.FType.basetype = btSet) then
    finalizeA(tbtstring(p.tstring))
  {$IFNDEF PS_NOWIDESTRING}
  else if p.FType.BaseType = btWideString then
    finalizeW(tbtWideString(p.twidestring)) // tbtwidestring
  else if p.FType.BaseType = btUnicodeString then
    finalizeU(tbtUnicodeString(p.tunistring)); // tbtwidestring
  {$ENDIF}
end;

procedure DisposeVariant(p: PIfRVariant);
begin
  if p <> nil then
  begin
    FinalizeVariant(p^);
    Dispose(p);
  end;
end;


function TPSOCCompiler.Compile(const s: tbtString): Boolean;
  procedure Cleanup;
  var
    I: Longint;
    PT: TPSType;
  begin
    {$IFDEF PS_USESSUPPORT}
    if fInCompile>1 then
    begin
      dec(fInCompile);
      exit;
    end;
    {$ENDIF}

    FGlobalBlock.Free;
    FGlobalBlock := nil;

    for I := 0 to FRegProcs.Count - 1 do
      TObject(FRegProcs[I]).Free;
    FRegProcs.Free;
    for i := 0 to FConstants.Count -1 do
    begin
      TPSConstant(FConstants[I]).Free;
    end;
    Fconstants.Free;
    for I := 0 to FVars.Count - 1 do
    begin
      TPSVar(FVars[I]).Free;
    end;
    FVars.Free;
    FVars := nil;
    for I := 0 to FProcs.Count - 1 do
      TPSProcedure(FProcs[I]).Free;
    FProcs.Free;
    FProcs := nil;
    //reverse free types: a custom type's attribute value type may point to a base type
    for I := FTypes.Count - 1 downto 0 do
    begin
      PT := TPSType(FTypes[I]);
      pt.Free;
    end;
    FTypes.Free;

    for i := FClasses.Count -1 downto 0 do
    begin
      TPSCompileTimeClass(FClasses[I]).Free;
    end;
    FClasses.Free;
    for i := FAttributeTypes.Count -1 downto 0 do
    begin
      TPSAttributeType(FAttributeTypes[i]).Free;
    end;
    FAttributeTypes.Free;
    FAttributeTypes := nil;

    for I := 0 to FUnitInits.Count - 1 do        //nvds
    begin                                        //nvds
      TPSBlockInfo(FUnitInits[I]).free;          //nvds
    end;                                         //nvds
    FUnitInits.Free;                             //nvds
    FUnitInits := nil;                           //
    for I := 0 to FUnitFinits.Count - 1 do       //nvds
    begin                                        //nvds
      TPSBlockInfo(FUnitFinits[I]).free;         //nvds
    end;                                         //nvds
    FUnitFinits.Free;                            //
    FUnitFinits := nil;                          //

    FreeAndNil(fUnits);
    FreeAndNil(FUses);
    fInCompile:=0;
  end;

  function MakeOutput: Boolean;

    procedure WriteByte(b: Byte);
    begin
      FOutput := FOutput + tbtChar(b);
    end;
    procedure WriteData(const Data; Len: Longint);
    var
      l: Longint;
    begin
      if Len < 0 then Len := 0;
      l := Length(FOutput);
      SetLength(FOutput, l + Len);
      Move(Data, FOutput[l + 1], Len);
    end;
    procedure WriteLong(l: Cardinal);
    begin
      WriteData(l, 4);
    end;
    procedure WriteVariant(p: PIfRVariant);
    begin
      WriteLong(p^.FType.FinalTypeNo);
      case p^.FType.BaseType of
      btType: WriteLong(p^.ttype.FinalTypeNo);
      {$IFNDEF PS_NOWIDESTRING}
      btWideString:
        begin
          WriteLong(Length(tbtWideString(p^.twidestring)));
          WriteData(tbtwidestring(p^.twidestring)[1], 2*Length(tbtWideString(p^.twidestring)));
        end;
      btUnicodeString:
        begin
          WriteLong(Length(tbtUnicodestring(p^.twidestring)));
          WriteData(tbtUnicodestring(p^.twidestring)[1], 2*Length(tbtUnicodestring(p^.twidestring)));
        end;
      btWideChar: WriteData(p^.twidechar, 2);
      {$ENDIF}
      btSingle: WriteData(p^.tsingle, sizeof(tbtSingle));
      btDouble: WriteData(p^.tsingle, sizeof(tbtDouble));
      btExtended: WriteData(p^.tsingle, sizeof(tbtExtended));
      btCurrency: WriteData(p^.tsingle, sizeof(tbtCurrency));
      btChar: WriteData(p^.tchar, 1);
      btSet:
        begin
          WriteData(tbtString(p^.tstring)[1], Length(tbtString(p^.tstring)));
        end;
      btString:
        begin
          WriteLong(Length(tbtString(p^.tstring)));
          WriteData(tbtString(p^.tstring)[1], Length(tbtString(p^.tstring)));
        end;
      btenum:
        begin
          if TPSEnumType(p^.FType).HighValue <=256 then
            WriteData( p^.tu32, 1)
          else if TPSEnumType(p^.FType).HighValue <=65536 then
            WriteData(p^.tu32, 2)
          else
            WriteData(p^.tu32, 4);
        end;
      bts8,btu8: WriteData(p^.tu8, 1);
      bts16,btu16: WriteData(p^.tu16, 2);
      bts32,btu32: WriteData(p^.tu32, 4);
      {$IFNDEF PS_NOINT64}
      bts64: WriteData(p^.ts64, 8);
      {$ENDIF}
      btProcPtr: WriteData(p^.tu32, 4);
      {$IFDEF DEBUG}
      else
          asm int 3; end;
      {$ENDIF}
      end;
    end;
    procedure WriteAttributes(attr: TPSAttributes);
    var
      i, j: Longint;
    begin
      WriteLong(attr.Count);
      for i := 0 to Attr.Count -1 do
      begin
        j := Length(attr[i].AType.Name);
        WriteLong(j);
        WriteData(Attr[i].AType.Name[1], j);
        WriteLong(Attr[i].Count);
        for j := 0 to Attr[i].Count -1 do
        begin
          WriteVariant(Attr[i][j]);
        end;
      end;
    end;
    procedure WriteTypes;
    var
      l, n: Longint;
      bt: TPSBaseType;
      x: TPSType;
      s: tbtString;
      FExportName: tbtString;
      Items: TPSList;
      procedure WriteTypeNo(TypeNo: Cardinal);
      begin
        WriteData(TypeNo, 4);
      end;
    begin
      Items := TPSList.Create;
      try
        for l := 0 to FCurrUsedTypeNo -1 do
          Items.Add(nil);
        for l := 0 to FTypes.Count -1 do
        begin
          x := FTypes[l];
          if x.Used then
            Items[x.FinalTypeNo] := x;
        end;
        for l := 0 to Items.Count - 1 do
        begin
          x := Items[l];
          if x.ExportName then
            FExportName := x.Name
          else
            FExportName := '';
          if (x.BaseType = btExtClass) and (x is TPSUndefinedClassType) then
          begin
            x := GetTypeCopyLink(TPSUndefinedClassType(x).ExtClass.SelfType);
          end;
          bt := x.BaseType;
          if (x.BaseType = btType) or (x.BaseType = btNotificationVariant) then
          begin
            bt := btU32;
          end else
          if (x.BaseType = btEnum) then begin
            if TPSEnumType(x).HighValue <= 256 then
              bt := btU8
            else if TPSEnumType(x).HighValue <= 65536 then
              bt := btU16
            else
              bt := btU32;
          end;
          if FExportName <> '' then
          begin
            WriteByte(bt + 128);
          end
          else
            WriteByte(bt);
  {$IFNDEF PS_NOINTERFACES} if x.BaseType = btInterface then
          begin
            WriteData(TPSInterfaceType(x).Intf.Guid, Sizeof(TGuid));
          end else {$ENDIF} if x.BaseType = btClass then
          begin
            WriteLong(Length(TPSClassType(X).Cl.ClassName));
            WriteData(TPSClassType(X).Cl.ClassName[1], Length(TPSClassType(X).Cl.ClassName));
          end else
          if (x.BaseType = btProcPtr) then
          begin
            s := DeclToBits(TPSProceduralType(x).ProcDef);
            WriteLong(Length(s));
            WriteData(s[1], Length(s));
          end else
          if (x.BaseType = btSet) then
          begin
            WriteLong(TPSSetType(x).BitSize);
          end else
          if (x.BaseType = btArray) or (x.basetype = btStaticArray) then
          begin
            WriteLong(TPSArrayType(x).ArrayTypeNo.FinalTypeNo);
            if (x.baseType = btstaticarray) then begin
              WriteLong(TPSStaticArrayType(x).Length);
              WriteLong(TPSStaticArrayType(x).StartOffset);      //<-additional StartOffset
            end;
          end else if x.BaseType = btRecord then
          begin
            n := TPSRecordType(x).RecValCount;
            WriteData( n, 4);
            for n := 0 to TPSRecordType(x).RecValCount - 1 do
              WriteTypeNo(TPSRecordType(x).RecVal(n).aType.FinalTypeNo);
          end;
          if FExportName <> '' then
          begin
            WriteLong(Length(FExportName));
            WriteData(FExportName[1], length(FExportName));
          end;
          WriteAttributes(x.Attributes);
        end;
      finally
        Items.Free;
      end;
    end;
    procedure WriteVars;
    var
      l,j : Longint;
      x: TPSVar;
    begin
      for l := 0 to FVars.Count - 1 do
      begin
        x := FVars[l];
        if x.SaveAsPointer then
        begin
          for j := FTypes.count -1 downto 0 do
          begin
            if TPSType(FTypes[j]).BaseType = btPointer then
            begin
              WriteLong(TPSType(FTypes[j]).FinalTypeNo);
              break;
            end;
          end;
        end else
          WriteLong(x.aType.FinalTypeNo);
        if x.exportname <> '' then
        begin
          WriteByte( 1);
          WriteLong(Length(X.ExportName));
          WriteData( X.ExportName[1], length(X.ExportName));
        end else
          WriteByte( 0);
      end;
    end;
    procedure WriteProcs;
    var
      l: Longint;
      xp: TPSProcedure;
      xo: TPSInternalProcedure;
      xe: TPSExternalProcedure;
      s: tbtString;
      att: Byte;
    begin
      for l := 0 to FProcs.Count - 1 do
      begin
        xp := FProcs[l];
        if xp.Attributes.Count <> 0 then att := 4 else att := 0;
        if xp.ClassType = TPSInternalProcedure then
        begin
          xo := TPSInternalProcedure(xp);
          xo.OutputDeclPosition := Length(FOutput);
          WriteByte(att or 2); // exported
          WriteLong(0); // offset is unknown at this time
          WriteLong(0); // length is also unknown at this time
          WriteLong(Length(xo.Name));
          WriteData( xo.Name[1], length(xo.Name));
          s := MakeExportDecl(xo.Decl);
          WriteLong(Length(s));
          WriteData( s[1], length(S));
        end
        else
        begin
          xe := TPSExternalProcedure(xp);
          if xe.RegProc.ImportDecl <> '' then
          begin
            WriteByte( att or 3); // imported
            if xe.RegProc.ExportName then
            begin
              WriteByte(Length(xe.RegProc.Name));
              WriteData(xe.RegProc.Name[1], Length(xe.RegProc.Name) and $FF);
            end else begin
              WriteByte(0);
            end;
            WriteLong(Length(xe.RegProc.ImportDecl));
            WriteData(xe.RegProc.ImportDecl[1], Length(xe.RegProc.ImportDecl));
          end else begin
            WriteByte(att or 1); // imported
            WriteByte(Length(xe.RegProc.Name));
            WriteData(xe.RegProc.Name[1], Length(xe.RegProc.Name) and $FF);
          end;
        end;
        if xp.Attributes.Count <> 0 then
          WriteAttributes(xp.Attributes);
      end;
    end;
    procedure WriteProcs2;
    var
      l: Longint;
      L2: Cardinal;
      x: TPSProcedure;
    begin
      for l := 0 to FProcs.Count - 1 do
      begin
        x := FProcs[l];
        if x.ClassType = TPSInternalProcedure then
        begin
          if TPSInternalProcedure(x).Data = '' then
            TPSInternalProcedure(x).Data := Chr(Cm_R);
          L2 := Length(FOutput);
          Move(L2, FOutput[TPSInternalProcedure(x).OutputDeclPosition + 2], 4);
          // write position
          WriteData(TPSInternalProcedure(x).Data[1], Length(TPSInternalProcedure(x).Data));
          L2 := Cardinal(Length(FOutput)) - L2;
          Move(L2, FOutput[TPSInternalProcedure(x).OutputDeclPosition + 6], 4); // write length
        end;
      end;
    end;
    function FindMainProc: Cardinal;
    var
      l: Longint;
      Proc : TPSInternalProcedure;
      ProcData : tbtString;
      Calls : Integer;

      procedure WriteProc(const aData: Longint);
      var
        l: Longint;
      begin
        ProcData := ProcData + Chr(cm_c);
        l := Length(ProcData);
        SetLength(ProcData, l + 4);
        Move(aData, ProcData[l + 1], 4);
        inc(Calls);
      end;
    begin
      ProcData := ''; Calls := 1;
      for l := 0 to FUnitInits.Count-1 do
        if (FUnitInits[l] <> nil) and
           (TPSBlockInfo(FUnitInits[l]).Proc.Data<>'') then
          WriteProc(TPSBlockInfo(FUnitInits[l]).ProcNo);

      WriteProc(FGlobalBlock.ProcNo);

      for l := FUnitFinits.Count-1 downto 0 do
        if (FUnitFinits[l] <> nil) and
           (TPSBlockInfo(FUnitFinits[l]).Proc.Data<>'') then
          WriteProc(TPSBlockInfo(FUnitFinits[l]).ProcNo);

      if Calls = 1 then begin
        Result := FGlobalBlock.ProcNo;
      end else
      begin
        Proc := NewProc('Master proc', '!MASTERPROC');
        Result := FindProc('!MASTERPROC');
        Proc.data := Procdata + Chr(cm_R);
      end;
    end;
    procedure CreateDebugData;
    var
      I: Longint;
      p: TPSProcedure;
      pv: TPSVar;
      s: tbtString;
    begin
      s := #0;
      for I := 0 to FProcs.Count - 1 do
      begin
        p := FProcs[I];
        if p.ClassType = TPSInternalProcedure then
        begin
          if TPSInternalProcedure(p).Name = PSMainProcName then
            s := s + #1
          else
            s := s + TPSInternalProcedure(p).OriginalName + #1;
        end
        else
        begin
          s := s+ TPSExternalProcedure(p).RegProc.OrgName + #1;
        end;
      end;
      s := s + #0#1;
      for I := 0 to FVars.Count - 1 do
      begin
        pv := FVars[I];
        s := s + pv.OrgName + #1;
      end;
      s := s + #0;
      WriteDebugData(s);
    end;
  var                       //nvds
    MainProc : Cardinal;    //nvds

  begin
    if @FOnBeforeOutput <> nil then
    begin
      if not FOnBeforeOutput(Self) then
      begin
        Result := false;
        exit;
      end;
    end;
    MainProc := FindMainProc; //NvdS (need it here becose FindMainProc can create a New proc.
    CreateDebugData;
    WriteLong(PSValidHeader);
    WriteLong(PSCurrentBuildNo);
    WriteLong(FCurrUsedTypeNo);
    WriteLong(FProcs.Count);
    WriteLong(FVars.Count);
    WriteLong(MainProc);  //nvds
    WriteLong(0);
    WriteTypes;
    WriteProcs;
    WriteVars;
    WriteProcs2;

    Result := true;
  end;

  function CheckExports: Boolean;
  var
    i: Longint;
    p: TPSProcedure;
  begin
    if @FOnExportCheck = nil then
    begin
      result := true;
      exit;
    end;
    for i := 0 to FProcs.Count -1 do
    begin
      p := FProcs[I];
      if p.ClassType = TPSInternalProcedure then
      begin
        if not FOnExportCheck(Self, TPSInternalProcedure(p), MakeDecl(TPSInternalProcedure(p).Decl)) then
        begin
          Result := false;
          exit;
        end;
      end;
    end;
    Result := True;
  end;
  function DoConstBlock: Boolean;
  var
    COrgName: tbtString;
    CTemp, CValue: PIFRVariant;
    Cp: TPSConstant;
    TokenPos, TokenRow, TokenCol: Integer;
  begin
    FParser.Next;
    repeat
      if FParser.CurrTokenID <> CSTI_Identifier then
      begin
        MakeError('', ecIdentifierExpected, '');
        Result := False;
        Exit;
      end;
      TokenPos := FParser.CurrTokenPos;
      TokenRow := FParser.Row;
      TokenCol := FParser.Col;
      COrgName := FParser.OriginalToken;
      if IsDuplicate(FastUpperCase(COrgName), [dcVars, dcProcs, dcConsts]) then
      begin
        MakeError('', ecDuplicateIdentifier, COrgName);
        Result := False;
        exit;
      end;
      FParser.Next;
      if FParser.CurrTokenID <> CSTI_Equal then
      begin
        MakeError('', ecIsExpected, '');
        Result := False;
        Exit;
      end;
      FParser.Next;
      CValue := ReadConstant(FParser, CSTI_SemiColon);
      if CValue = nil then
      begin
        Result := False;
        Exit;
      end;
      if FParser.CurrTokenID <> CSTI_Semicolon then
      begin
        MakeError('', ecSemicolonExpected, '');
        Result := False;
        exit;
      end;
      cp := TPSConstant.Create;
      cp.Orgname := COrgName;
      cp.Name := FastUpperCase(COrgName);
      {$IFDEF PS_USESSUPPORT}
      cp.DeclareUnit:=fModule;
      {$ENDIF}
      cp.DeclarePos := TokenPos;
      cp.DeclareRow := TokenRow;
      cp.DeclareCol := TokenCol;
      New(CTemp);
      InitializeVariant(CTemp, CValue.FType);
      CopyVariantContents(cvalue, CTemp);
      cp.Value := CTemp;
      FConstants.Add(cp);
      DisposeVariant(CValue);
      FParser.Next;
    until FParser.CurrTokenId <> CSTI_Identifier;
    Result := True;
  end;

  function ProcessUses: Boolean;
  var
    {$IFNDEF PS_USESSUPPORT}
    FUses: TIfStringList;
    {$ENDIF}
    I: Longint;
    s: tbtString;
    {$IFDEF PS_USESSUPPORT}
    Parse: Boolean;
    ParseUnit: tbtString;
    ParserPos: TPSPascalParser;
    {$ENDIF}
  begin
    FParser.Next;
    {$IFNDEF PS_USESSUPPORT}
    FUses := TIfStringList.Create;
    FUses.Add('SYSTEM');
    {$ENDIF}
    repeat
      if FParser.CurrTokenID <> CSTI_Identifier then
      begin
        MakeError('', ecIdentifierExpected, '');
        {$IFNDEF PS_USESSUPPORT}
        FUses.Free;
        {$ENDIF}
        Result := False;
        exit;
      end;
      s := FParser.GetToken;
      {$IFDEF PS_USESSUPPORT}
      Parse:=true;
      {$ENDIF}
      for i := 0 to FUses.Count -1 do
      begin
        if FUses[I] = s then
        begin
          {$IFNDEF PS_USESSUPPORT}
          MakeError('', ecDuplicateIdentifier, s);
          FUses.Free;
          Result := False;
          exit;
          {$ELSE}
          Parse:=false;
          {$ENDIF}
        end;
      end;
    {$IFDEF PS_USESSUPPORT}
      if fUnits.GetUnit(S).HasUses(fModule) then
      begin
        MakeError('', ecCrossReference, s);
        Result := False;
        exit;
      end;

      fUnit.AddUses(S);

      if Parse then
      begin
      {$ENDIF}
        FUses.Add(s);
        if @FOnUses <> nil then
        begin
          try
            {$IFDEF PS_USESSUPPORT}
            OldFileName:=fModule;
            fModule:=FParser.OriginalToken;
            ParseUnit:=FParser.OriginalToken;
            ParserPos:=FParser;
            {$ENDIF}
            if not OnUses(Self, FParser.GetToken) then
            begin
              {$IFNDEF PS_USESSUPPORT}
              FUses.Free;
              {$ELSE}
              FParser:=ParserPos;
              fModule:=OldFileName;
              MakeError(OldFileName, ecUnitNotFoundOrContainsErrors, ParseUnit);
              {$ENDIF}
              Result := False;
              exit;
            end;
            {$IFDEF PS_USESSUPPORT}
            fModule:=OldFileName;
            {$ENDIF}
          except
            on e: Exception do
            begin
              MakeError('', ecCustomError, tbtstring(e.Message));
              {$IFNDEF PS_USESSUPPORT}
              FUses.Free;
              {$ENDIF}
              Result := False;
              exit;
            end;
          end;
        end;
      {$IFDEF PS_USESSUPPORT}
      end;
      {$ENDIF}
      FParser.Next;
      if FParser.CurrTokenID = CSTI_Semicolon then break
      else if FParser.CurrTokenId <> CSTI_Comma then
      begin
        MakeError('', ecSemicolonExpected, '');
        Result := False;
        {$IFNDEF PS_USESSUPPORT}
        FUses.Free;
        {$ENDIF}
        exit;
      end;
      FParser.Next;
    until False;
    {$IFNDEF PS_USESSUPPORT}
    FUses.Free;
    {$ENDIF}
    FParser.next;
    Result := True;
  end;
var
  i: Integer;
  Proc: TPSProcedure;
begin
  Result := False;
  FUnitInits := TPSList.Create; //nvds
  FUnitFinits:= TPSList.Create; //nvds

  FUses:=TIFStringList.Create;
  FUnits:=TPSUnitList.Create;
  FGlobalBlock := TPSBlockInfo.Create(nil);
  FGlobalBlock.SubType := tMainBegin;

  //FGlobalBlock.Proc := NewProc(PSMainProcNameOrg, PSMainProcName);
  //FGlobalBlock.ProcNo := FindProc(PSMainProcName);

  DefineStandardTypes;
  DefineStandardProcedures;

  fUnit:=fUnits.GetUnit(fModule);
  FParserHadError := False;
  inc(fInCompile);

  repeat
    if FParser.CurrTokenId = CSTI_EOF then
    begin
      if FParserHadError then
      begin
        Cleanup;
        exit;
      end;
      if FAllowNoEnd then
        Break
      else
      begin
        MakeError('', ecUnexpectedEndOfFile, '');
        Cleanup;
        exit;
      end;
    end;
    if (FParser.CurrTokenID = CSTII_Uses) then
    begin
      if not ProcessUses then
      begin
         Cleanup;
        exit;
      end;
    end
    else if (FParser.CurrTokenId = CSTII_Label) then
    begin
      if not ProcessLabel(FGlobalBlock.Proc) then
      begin
        Cleanup;
        exit;
      end;
    end
    else if (FParser.CurrTokenId = CSTII_Const) then
    begin
      if not DoConstBlock then
      begin
        Cleanup;
        exit;
      end;
    end
    else if (FParser.CurrTokenId = CSTII_Begin)
         then
    begin
        FGlobalBlock.Proc.DeclarePos := FParser.CurrTokenPos;
        FGlobalBlock.Proc.DeclareRow := FParser.Row;
        FGlobalBlock.Proc.DeclareCol := FParser.Col;
        if ProcessSub(FGlobalBlock) then
        begin
          break;
        end
        else
        begin
          Cleanup;
          exit;
        end;
    end
    else if (Fparser.CurrTokenId = CSTII_End) and (FAllowNoBegin or FIsUnit) then
    begin
      FParser.Next;
      if FParser.CurrTokenID <> CSTI_Period then
      begin
        MakeError('', ecPeriodExpected, '');
        Cleanup;
        exit;
      end;
      break;
    end else
    begin
      MakeError('', ecBeginExpected, '');
      Cleanup;
      exit;
    end;
  until False;

  {$IFDEF PS_USESSUPPORT}
  dec(fInCompile);
  if fInCompile=0 then
  begin
  {$ENDIF}
    if not ProcessLabelForwards(FGlobalBlock.Proc) then
    begin
      Cleanup;
      exit;
    end;
    // NVDS: Do we need to check here also do a ProcessLabelForwards() for each Initialisation/finalization block?

    for i := 0 to FProcs.Count -1 do
    begin
      Proc := FProcs[I];
      if (Proc.ClassType = TPSInternalProcedure) and (TPSInternalProcedure(Proc).Forwarded) then
      begin
        with MakeError('', ecUnsatisfiedForward, TPSInternalProcedure(Proc).Name) do
        begin
          Pos := TPSInternalProcedure(Proc).DeclarePos;
          Row := TPSInternalProcedure(Proc).DeclareRow;
          Col := TPSInternalProcedure(Proc).DeclareCol;
        end;
        Cleanup;
        Exit;
      end;
    end;
    if not CheckExports then
    begin
      Cleanup;
      exit;
    end;
    for i := 0 to FVars.Count -1 do
    begin
      if not TPSVar(FVars[I]).Used then
      begin
        with MakeHint({$IFDEF PS_USESSUPPORT}TPSVar(FVars[I]).DeclareUnit{$ELSE}''{$ENDIF}, ehVariableNotUsed, TPSVar(FVars[I]).Name) do
        begin
          Pos := TPSVar(FVars[I]).DeclarePos;
          Row := TPSVar(FVars[I]).DeclareRow;
          Col := TPSVar(FVars[I]).DeclareCol;
        end;
      end;
    end;

    Result := MakeOutput;
    Cleanup;
end;

function TPSOCCompiler.GetOutput(var s: tbtString): Boolean;
begin

end;

function TPSOCCompiler.GetDebugOutput(var s: tbtString): Boolean;
begin

end;

procedure TPSOCCompiler.Clear;
begin

end;

constructor TPSOCCompiler.Create;
begin
  FParser := TPSOCParser.Create;
  FParser.OnParserError:=ParserError;
  FAutoFreeList := TPSList.Create;
  FOutput := '';
  FAllowDuplicateRegister := true;
  FAllowUnit := true;
  FMessages := TPSList.Create;
end;

destructor TPSOCCompiler.Destroy;
begin
  Clear;
  FAutoFreeList.Free;

  FMessages.Free;
  FParser.Free;
  inherited Destroy;
end;

end.

