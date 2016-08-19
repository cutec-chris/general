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
unit genpascalscript;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uPSCompiler,db,
  uPSC_classes, uPSC_DB, uPSC_dateutils, uPSC_dll, uPSRuntime,
  uPSR_classes, uPSR_DB, uPSR_dateutils, uPSR_dll, uPSUtils,
  uPSR_std,uPSC_std,uPSDebugger,
  Process,usimpleprocess,Utils,variants,dynlibs,
  RegExpr,MathParser,genscript;

type
  TSleepFunc = procedure(MiliSecValue : cardinal);

  TLoadedLib = class
  public
    Name : string;
    LibName : string;
    Code : string;
    Handle : THandle;
    constructor Create;
  end;

  TPascalScript = class;

  TInternalFindRec = Record
    Time : TDateTime;
    Size : Int64;
    Attr : Longint;
    Name : string;
  end;

  TIPSPascalCompiler = class(TPSPascalCompiler)
  private
    FObj: TObject;
  public
    property Obj : TObject read FObj write FObj;
  end;

  TPascalOnUses = function(Sender: TPascalScript; const Name: tbtString; OnlyAdditional : Boolean): Boolean of object;
  TWriteStringEvent = procedure(Sender: TObject;s: String) of object;

  { TPascalScript }

  TPascalScript = class(TByteCodeScript)
  private
    CompleteOutput : string;
    FExecStep: TNotifyEvent;
    FOnUses: TPascalOnUses;
    FExecWriteString : TWriteStringEvent;
    FProcesses: TList;
    FRuntime : TPSExec;
    FRuntimeFree: Boolean;
    FCompiler: TIPSPascalCompiler;
    FCompilerFree: Boolean;
    FClassImporter: TPSRuntimeClassImporter;
    FToolRegistered: TStrOutFunc;
    FFindRec : TSearchRec;
    procedure InternalFindClose(var FindRec: TInternalFindRec);
    function InternalFindFirst(const FileName: String; var FindRec: TInternalFindRec): Boolean;
    function InternalFindNext(var FindRec: TInternalFindRec): Boolean;
    procedure SetClassImporter(AValue: TPSRuntimeClassImporter);
    procedure SetCompiler(AValue: TIPSPascalCompiler);
    procedure SetRuntime(AValue: TPSExec);
  protected
    procedure InternalChDir(Directory : string);
    procedure InternalMkDir(Directory : string);
    function InternalApplicationDir : string;
    function InternalDirectoryExists(Const Directory : String) : Boolean;

    function InternalExec(cmd : string) : Integer;
    function InternalExecAndWatch(cmd : string;OnWriteln : TWriteStringEvent) : Integer;
    function InternalVisualExec(cmd : string) : Integer;
    function InternalVisualExecAndWatch(cmd : string;OnWriteln : TWriteStringEvent) : Integer;
    procedure InternalExecWrite(Pid : Integer;cmd : string);
    function InternalExecActive(Pid : Integer): Boolean;
    function InternalExecResult(Pid : Integer): Integer;
    function InternalKill(Pid : Integer): Boolean;
    procedure InternalBeep;
    procedure InternalSleep(MiliSecValue: LongInt);virtual;

    function InternalRebootMashine : Boolean;
    function InternalShutdownMashine : Boolean;
    function InternalWakeMashine(Mac,Ip : string) : Boolean;

    function InternalTimeToStr(Time: TDateTime): string;
    function InternalDateTimeToStr(Time: TDateTime): string;
    function InternalFormat(Fmt: string; Args: array of const): string;

    function InternalMathParse(Input: string): string;
    function GetTyp: string; override;
    function GetStatus: TScriptStatus; override;

    function InternalGetTempDir : string;
  public
    procedure Init; override;
    function InternalUses(Comp : TPSPascalCompiler;cName : string) : Boolean;virtual;
    function Execute(aParameters: Variant;Debug : Boolean = false): Boolean; override;
    property Runtime : TPSExec read FRuntime write SetRuntime;
    property ClassImporter : TPSRuntimeClassImporter read FClassImporter write SetClassImporter;
    property Compiler : TIPSPascalCompiler read FCompiler write SetCompiler;
    function AddMethodEx(Slf, Ptr: Pointer; const Decl: tbtstring; CallingConv: uPSRuntime.TPSCallingConvention): Boolean;
    function AddMethod(Slf, Ptr: Pointer; const Decl: tbtstring): Boolean;
    function AddFunction(Ptr: Pointer; const Decl: tbtstring): Boolean;
    function AddFunctionEx(Ptr: Pointer; const Decl: tbtstring;
      CallingConv: uPSRuntime.TPSCallingConvention): Boolean;
    property OnUses : TPascalOnUses read FOnUses write FOnUses;
    function Compile: Boolean; override;
    property Output : string read CompleteOutput;
    function Stop: Boolean; override;
    function StepInto: Boolean; override;
    function StepOver: Boolean; override;
    function Resume: Boolean; override;
    function Pause: Boolean; override;
    function IsRunning: Boolean; override;
    function RunScriptFunction(const Params: array of Variant;fName : string): Variant; override;
    function GetVarContents(Identifier: string): string;override;
    constructor Create;override;
    destructor Destroy; override;
    property OnExecuteStep : TNotifyEvent read FExecStep write FExecStep;
    property OnToolRegistering : TStrOutFunc read FToolRegistered write FToolRegistered;
    procedure OpenTool(aName : string);
  end;

type
  TScriptSleepFunction = procedure (aTime : LongInt); StdCall;
  TScriptInputBoxFunction = function(const Caption, Prompt, Default : string) : string;
var
  LoadedLibs : TList;
  ActRuntime : TScript;
  DoSleep : TScriptSleepFunction = nil;
  DoInputBox : TScriptInputBoxFunction = nil;

  procedure DoCleanUp;

implementation

uses
  {$ifdef WINDOWS}
  Windows,mmsystem
  {$endif}
  {$ifdef UNIX}
  BaseUnix
  {$endif}
  ;
resourcestring
  STR_RUNTIME_ERROR='[Laufzeitfehler] %s(%d:%d), bytecode(%d:%d): %s'; //Birb

type
  aProcSleepT = procedure(aSleep : TScriptSleepFunction);stdcall;

procedure OwnSleep(aTime : Integer);stdcall;
begin
  if Assigned(DoSleep) then
    DoSleep(aTime)
  else Sleep(aTime);
end;
type
  TMsgDlgType    = (mtWarning, mtError, mtInformation, mtConfirmation, mtCustom);
  TMsgDlgBtn     = (mbYes, mbNo, mbOK, mbCancel, mbAbort, mbRetry, mbIgnore,
                    mbAll, mbNoToAll, mbYesToAll, mbHelp, mbClose);
  TMsgDlgButtons = set of TMsgDlgBtn;

const
  mrNone = 0;
  mrOk = 1;
  mrCancel = 2;
  mrAbort = 3;
  mrRetry = 4;
  mrIgnore = 5;
  mrYes = 6;
  mrNo = 7;
  mrAll = 8;
  mrNoToAll = 9;
  mrYesToAll = 10;

function MessageDlgC(aMsg: string; DlgType: TMsgDlgType;
            Buttons: TMsgDlgButtons; const HelpKeyword: string): Integer;
var
  res: LongInt;
begin
  {$ifdef WINDOWS}
  if (mbYes in Buttons) and (mbNo in Buttons) and (mbCancel in Buttons) then
    res := MessageBox(0,PChar(UniToSys(aMsg)),PChar('Frage'),MB_YESNOCANCEL+MB_ICONINFORMATION)
  else if (mbYes in Buttons) and (mbNo in Buttons) then
    res := MessageBox(0,PChar(UniToSys(aMsg)),PChar('Frage'),MB_YESNO+MB_ICONINFORMATION)
  else if mbOK in Buttons then
    res := MessageBox(0,PChar(UniToSys(aMsg)),PChar('Frage'),MB_OK+MB_ICONINFORMATION)
  else if (mbOK in Buttons) and (mbCancel in Buttons) then
    res := MessageBox(0,PChar(UniToSys(aMsg)),PChar('Frage'),MB_OKCANCEL+MB_ICONINFORMATION);
  case res of
  IDYES:Result := mrYes;
  IDNO:Result := mrNO;
  IDOK:Result := mrOK;
  IDCANCEL:Result := mrCancel;
  end;
  {$endif}
end;

function InputboxC ( const Caption, Prompt, Default : string ) : string;
begin
  Result := '';
  if Assigned(DoInputBox) then
    Result := DoInputBox(Caption,Prompt,Default);
end;

procedure ShowMessageC(const aMsg: string);
begin
  {$ifdef WINDOWS}
  MessageBox(0,PChar(UniToSys(aMsg)),PChar('Information'),MB_ICONINFORMATION);
  {$endif}
end;

type
  aProcT = function : pchar;stdcall;
  aProcT2 = procedure;stdcall;

procedure DoCleanUp;
var
  i: Integer;
  aProc: aProcT2;
  aLib: TLoadedLib;
begin
  for i := 0 to LoadedLibs.Count-1 do
    begin
      try
        aLib := TLoadedLib(LoadedLibs[i]);
        aProc := aprocT2(dynlibs.GetProcAddress(aLib.Handle,'ScriptCleanup'));
        if Assigned(aProc) then
          aProc;
      except
      end;
    end;
end;

function UnloadProcInt(Caller: TPSExec; p: TPSExternalProcRec; Global, Stack: TPSStack): Boolean;
begin
  DoCleanUp;
  UnloadProc(Caller,p,Global,Stack);
end;

function IProcessDllImport(Sender: TPSExec; p: TPSExternalProcRec; Tag: Pointer
  ): Boolean;
var
  i: LongInt;
  pv: PIFProcRec;
  h: LongInt;
  aLib: String;
  Caller: TPSExec;
  a: Integer;
  s: String;
  ph: PLoadedDll;
  aLibName: ShortString;
  actLib: String;
  aProc: aProcSleepT;
  bLib: TLoadedLib;
  aSelName: ShortString;
  aSelhandle: sysutils.THandle;
  Found: Boolean;
  Shouldtell: Boolean;
begin
  Result := ProcessDllImport(Sender,p);

  aLib := ExtractFileName(lowercase(copy(p.Decl,5,length(p.Decl))));
  if rpos('.',aLib)>0 then
    aLibName := lowercase(copy(aLib,0,rpos('.',aLib)-1))
  else if rpos(#0,aLib)>0 then
    aLibName := lowercase(copy(aLib,0,pos(#0,aLib)-1))
  else
    aLibName:=lowercase(aLib);
  Found := False;
  for a := 0 to LoadedLibs.Count-1 do
    begin
      bLib := TLoadedLib(LoadedLibs[a]);
      aSelName := lowercase(bLib.Name);
      aSelhandle := bLib.Handle;
      if (aLibName=aSelName)  then
        begin
          if (aSelhandle=0) then
            begin
              Caller := Sender;
              i := 2147483647; // maxint
              repeat
                ph := Caller.FindProcResource2(@dllFree, i);
                if (ph = nil) then break;
                actLib := ExtractFileName(lowercase(copy(ph^.dllname,0,rpos('.',ph^.dllname)-1)));
                if rpos('.',ph^.dllname)=0 then
                  actLib := lowercase(ph^.dllname);
                if (actLib = aLibName) then
                  begin
                    TLoadedLib(LoadedLibs[a]).Handle := ph^.dllhandle;
                    aProc := aProcSleepT(dynlibs.GetProcAddress(ph^.dllhandle,'ScriptSetSleep'));
                    if Assigned(aProc) then
                      begin
                        aProc(@OwnSleep);
                      end;
                    Found := True;
                  end;
              until false;
            end
          else Found:=True;
          break;
        end;
    end;
  if not Found then
    Shouldtell := True;
end;
function ReplaceRegExprIfMatch(const ARegExpr, AInputStr, AReplaceStr : RegExprString;
      AUseSubstitution : boolean = False) : RegExprString;
begin
  Result := '';
  with TRegExpr.Create do try
    Expression := ARegExpr;
    if Exec(AInputStr) then
      Result := Replace (AInputStr, AReplaceStr, AUseSubstitution);
    finally Free;
   end;
end;

procedure OnRunActLine(Sender: TPSExec);
begin
  if Assigned(ActRuntime) and Assigned(ActRuntime.OnRunLine) then
    begin
      ActRuntime.OnRunLine(ActRuntime,Sender.UnitName,-1,-1,-1);
    end;
end;

procedure OnSourceLine(Sender: TPSDebugExec; const Name: tbtstring; Position,
  Row, Col: Cardinal);
begin
  if Assigned(ActRuntime) and Assigned(ActRuntime.OnRunLine) then
    begin
      ActRuntime.OnRunLine(ActRuntime,Name,Position,Row,Col);
    end;
end;

procedure IdleCall(Sender: TPSDebugExec);
begin
  if Assigned(ActRuntime) and Assigned(ActRuntime.OnIdle) then
    ActRuntime.OnIdle(ActRuntime);
end;

function TPascalScript.InternalFindFirst(const FileName: String; var FindRec: TInternalFindRec): Boolean;
begin
  try
    Result := FindFirst(UniToSys(FileName),faAnyFile or faDirectory,FFindRec)=0;
    if Result then
      begin
        FindRec.Attr:=FFindRec.Attr;
        FindRec.Name:=SysToUni(FFindRec.Name);
        FindRec.Size:=FFindRec.Size;
        FindRec.Time:=FileDateToDateTime(FFindRec.Time);
      end;
  except
    Result := False;
  end;
end;
function TPascalScript.InternalFindNext(var FindRec: TInternalFindRec): Boolean;
begin
  try
    Result := FindNext(FFindRec)=0;
    if Result then
      begin
        FindRec.Attr:=FFindRec.Attr;
        FindRec.Name:=SysToUni(FFindRec.Name);
        FindRec.Size:=FFindRec.Size;
        FindRec.Time:=FileDateToDateTime(FFindRec.Time);
      end;
  except
    Result := False;
  end;
end;
procedure TPascalScript.InternalFindClose(var FindRec: TInternalFindRec);
begin
  SysUtils.FindClose(FFindRec);
end;
function TPascalScript.InternalUses(Comp: TPSPascalCompiler; cName: string
  ): Boolean;
var
  aLib: TLibHandle;
  aProc: aProcT;
  Procs : TStringList;
  sProc: String;
  i: Integer;
  aLibName: TbtString;
  tmp: String;
  newUnit: String;
  tmp1,tmp2: String;
  NewLib: TLoadedLib;
  tmp3: String;
  bLib: TLoadedLib;
  FLibCompiled: Boolean = false;
  a: Integer;
  aMsg: TPSPascalCompilerMessage;

  function FindLib(aPath,aName : string) : string;
  begin
    Result := '';
    if FileExists(UniToSys(aPath+lowercase(aName)+'.dll')) then
      Result := UniToSys(aPath+lowercase(aName)+'.dll');
    if FileExists(aPath+lowercase(aName)+'.so') then
      Result := aPath+lowercase(aName)+'.so';
    if FileExists(aPath+'lib'+lowercase(aName)+'.so') then
      Result := aPath+'lib'+lowercase(aName)+'.so';
    if FileExists(aPath+lowercase(aName)+'.dylib') then
      Result := aPath+lowercase(aName)+'.dylib';
  end;

begin
  Result := True;
  try
    if lowercase(cName)='system' then
      begin
        try
          AddMethod(Self,@TPascalScript.InternalChDir,'procedure ChDir(Dir : string);');
          AddMethod(Self,@TPascalScript.InternalMkDir,'procedure MkDir(Dir : string);');
          AddMethod(Self,@TPascalScript.InternalClearScreen,'procedure ClearScreen;');
          AddMethod(Self,@TPascalScript.InternalBringToFront,'procedure BringToFront;');
        except
        end;
        uPSC_std.SIRegister_Std(Comp);
        uPSR_std.RIRegister_Std(FClassImporter);
      end
    else if lowercase(cName)='classes' then
      begin
        uPSC_classes.SIRegister_Classes(Comp,true);
        uPSR_classes.RIRegister_Classes(FClassImporter,true);
      end
    else if lowercase(cName)='sysutils' then
      begin
        InternalUses(Comp,'DATEUTILS');
        AddMethod(Self,@TPascalScript.InternalBeep,'procedure Beep;');
        AddMethod(Self,@TPascalScript.InternalSleep,'procedure Sleep(MiliSecValue : LongInt);');
        Comp.AddTypeS('TReplaceFlag','(rfReplaceAll, rfIgnoreCase)');
        Comp.AddTypeS('TReplaceFlags','set of TReplaceFlag');
        AddFunction(@StringReplace,'function StringReplace(const S, OldPattern, NewPattern: string;  Flags: TReplaceFlags): string;');
        AddMethod(Self,@TPascalScript.InternalTimeToStr,'function TimeToStr(Time: TDateTime): string;');
        AddMethod(Self,@TPascalScript.InternalDateTimeToStr,'function DateTimeToStr(DateTime: TDateTime): string;');
        AddMethod(Self,@TPascalScript.InternalFormat,'function Format(Fmt: string;Args: array of const):string;');
        AddMethod(Self,@TPascalScript.InternalDirectoryExists,'function DirectoryExists(Const Directory : String) : Boolean;');
        AddMethod(Self,@TPascalScript.InternalGetTempDir,'function GetTempDir : String;');
        AddMethod(Self,@TPascalScript.InternalApplicationDir,'function GetApplicationDir : String;');
        AddFunction(@IntToHex,'function IntToHex(Value: integer; Digits: integer) : string;');
        AddFunction(@GetCurrentDir,'function GetCurrentDir : string;');
        AddFunction(@SetCurrentDir,'function SetCurrentDir ( const Dir : string ) : Boolean;');
        AddFunction(@FileExists,'function FileExists (Const FileName : String) : Boolean;');
        AddFunction(@SysUtils.DeleteFile,'Function DeleteFile (Const FileName : String) : Boolean;');
        Comp.AddTypeS('TFindRec','record' +
                                 ' Time : TDateTime;'+
                                 ' Size : Int64;'+
                                 ' Attr : Longint;'+
                                 ' Name : string;'+
                                 'end');
        AddMethod(Self,@TPascalScript.InternalFindFirst,'function FindFirst(const FileName: String; var FindRec: TFindRec): Boolean;');
        AddMethod(Self,@TPascalScript.InternalFindNext,'function FindNext(var FindRec: TFindRec): Boolean;');
        AddMethod(Self,@TPascalScript.InternalFindClose,'function FindClose(var FindRec: TFindRec): Boolean;');
        AddFunction(@SysUtils.GetEnvironmentVariable,'function GetEnvironmentVariable(Const EnvVar : String) : String;');
        AddFunction(@SysUtils.GetEnvironmentVariableCount,'function GetEnvironmentVariableCount : Integer;');
      end
    else if lowercase(cName)='exec' then
      begin
        Comp.AddTypeS('TWriteStringEvent', 'procedure (Sender: TObject;s : string)');
        AddMethod(Self,@TPascalScript.InternalExec,'function Exec(cmd : string) : Integer;');
        AddMethod(Self,@TPascalScript.InternalExecAndWatch,'function ExecAndWatch(cmd : string;OnWriteln : TWriteStringEvent) : Integer;');
        AddMethod(Self,@TPascalScript.InternalVisualExec,'function ExecVisual(cmd : string) : Integer;');
        AddMethod(Self,@TPascalScript.InternalVisualExecAndWatch,'function ExecVisualAndWatch(cmd : string;OnWriteln : TWriteStringEvent) : Integer;');
        AddMethod(Self,@TPascalScript.InternalExecWrite,'procedure ExecWrite(Pid : Integer;cmd : string);');
        AddMethod(Self,@TPascalScript.InternalExecActive,'function ExecActive(Pid : Integer) : Boolean;');
        AddMethod(Self,@TPascalScript.InternalExecResult,'function ExecResult(Pid : Integer) : Integer;');
        AddMethod(Self,@TPascalScript.InternalKill,'function Kill(Pid : Integer) : Boolean;');
      end
    else if lowercase(cName)='mashine' then
      begin
        AddMethod(Self,@TPascalScript.InternalRebootMashine,'function RebootMashine : Boolean;');
        AddMethod(Self,@TPascalScript.InternalShutdownMashine,'function ShutdownMashine : Boolean;');
      end
    else if lowercase(cName)='db' then
      begin
        uPSC_DB.SIRegister_DB(Comp);
        uPSR_DB.RIRegister_DB(FClassImporter);
      end
    else if lowercase(cName)='variants' then
      begin
        AddFunction(@VarArrayOf,'function VarArrayOf(const Values: array of Variant): Variant;');
      end
    else if lowercase(cName)='dateutils' then
      begin
        uPSC_dateutils.RegisterDateTimeLibrary_C(Comp);
        uPSR_dateutils.RegisterDateTimeLibrary_R(Runtime);
      end
    else if lowercase(cName)='regexpr' then
      begin
        InternalUses(Comp,'CLASSES');
        AddFunction(@ExecRegExpr,'function ExecRegExpr (const ARegExpr, AInputStr : String) : boolean;');
        AddFunction(@ReplaceRegExpr,'function ReplaceRegExpr (const ARegExpr, AInputStr, AReplaceStr : String; AUseSubstitution : boolean) : String;');
        AddFunction(@SplitRegExpr,'procedure SplitRegExpr (const ARegExpr, AInputStr : String; APieces : TStrings);');
        AddFunction(@ReplaceRegExprIfMatch,'function ReplaceRegExprIfMatch (const ARegExpr, AInputStr, AReplaceStr : String; AUseSubstitution : boolean) : String;');
      end
    else if lowercase(cName)='mathparser' then
      begin
        InternalUses(Comp,'CLASSES');
        AddMethod(Self,@TPascalScript.InternalMathParse,'function MathParse(Input : string) : string;');
      end
    else if lowercase(cName)='dialogs' then
      begin
        {$IF defined(LCLNOGUI)}
        Result := false;
        {$ELSE}
        Comp.AddConstantN('mrNone', 'Integer').Value^.ts32 := 0;
        Comp.AddConstantN('mrOk', 'Integer').Value^.ts32 := 1;
        Comp.AddConstantN('mrCancel', 'Integer').Value^.ts32 := 2;
        Comp.AddConstantN('mrAbort', 'Integer').Value^.ts32 := 3;
        Comp.AddConstantN('mrRetry', 'Integer').Value^.ts32 := 4;
        Comp.AddConstantN('mrIgnore', 'Integer').Value^.ts32 := 5;
        Comp.AddConstantN('mrYes', 'Integer').Value^.ts32 := 6;
        Comp.AddConstantN('mrNo', 'Integer').Value^.ts32 := 7;
        Comp.AddConstantN('mrAll', 'Integer').Value^.ts32 := 8;
        Comp.AddConstantN('mrNoToAll', 'Integer').Value^.ts32 := 9;
        Comp.AddConstantN('mrYesToAll', 'Integer').Value^.ts32 := 10;
        Comp.AddTypeS('TMsgDlgType', '( mtWarning, mtError, mtInformation, mtConfirmation, mtCustom )');
        Comp.AddTypeS('TMsgDlgBtn', '( mbYes, mbNo, mbOK, mbCancel, mbAbort, mbRetry, mbIgnore, mbAll, mbNoToAll, mbYesToAll, mbHelp )');
        Comp.AddTypeS('TMsgDlgButtons', 'set of TMsgDlgBtn');
        Comp.AddConstantN('mbYesNoCancel','LongInt').Value^.ts32 := ord(0) or ord(1) or ord(3);
        Comp.AddConstantN('mbOKCancel','LongInt').Value^.ts32 := ord(2) or ord(3);
        Comp.AddConstantN('mbAbortRetryIgnore','LongInt').Value^.ts32 := ord(4) or ord(5) or ord(6);
        AddFunction(@MessageDlgC,'Function MessageDlg( const Msg : string; DlgType : TMsgDlgType; Buttons : TMsgDlgButtons; HelpCtx : Longint) : Integer');
        AddFunction(@ShowMessageC,'Procedure ShowMessage( const Msg : string)');
        AddFunction(@InputBoxC,'function Inputbox ( const Caption, Prompt, Default : string ) : string;');
        Result := True;
        {$ENDIF}
      end
    else
      begin
        Result := False;
        if Assigned(FOnUses) then
          Result := FOnUses(Self,cName,Result) or Result;
        if not Result then
          begin
            aLibName := FindLib(ExtractFilePath(ParamStr(0)),cName);
            if aLibName='' then
              aLibName := FindLib(ExtractFilePath(ParamStr(0))+'scriptplugins'+DirectorySeparator,cName);
            if aLibName='' then
              aLibName := FindLib(ExtractFilePath(ParamStr(0))+'..'+DirectorySeparator+'scriptplugins'+DirectorySeparator,cName);
            if FileExists(aLibname) then
              begin
                if not Assigned(Comp.OnExternalProc) then
                  uPSC_dll.RegisterDll_Compiletime(Comp);
                Runtime.AddSpecialProcImport('dll', @IProcessDllImport, nil);
                Runtime.RegisterFunctionName('UNLOADDLL', @UnloadProcInt, nil, nil);
                Runtime.RegisterFunctionName('DLLGETLASTERROR', @GetLastErrorProc, nil, nil);
                for i := 0 to LoadedLibs.Count-1 do
                  if TLoadedLib(LoadedLibs[i]).Name=cName then
                    begin
                      bLib := TLoadedLib(LoadedLibs[i]);
                      Result := Comp.Compile(bLib.Code);
                      for a := 0 to Comp.MsgCount-1 do
                        begin
                          CompleteOutput:=CompleteOutput+Compiler.Msg[a].MessageToString+LineEnding;
                          aMsg := Comp.Msg[a];
                          if Assigned(OnCompileMessage) then OnCompileMessage(Self,aMsg.UnitName,Comp.Msg[i].MessageToString,Comp.Msg[i].Pos,Comp.Msg[i].Row,Comp.Msg[i].Col);
                        end;
                      if not Result then
                        Debugln('Failed to compile Library:'+aLibName+' '+CompleteOutput);
                      exit;
                    end;
                aLib := LoadLibrary(PChar(aLibName));
                if aLib <> dynlibs.NilHandle  then
                  begin
                    aProc := aprocT(dynlibs.GetProcAddress(aLib,'ScriptDefinition'));
                    if Assigned(aProc) then
                      begin
                        newUnit := 'unit '+cname+';'+LineEnding+'interface'+LineEnding+'type';
                        Procs := TStringList.Create;
                        sProc := aProc();
                        Procs.text := sProc;
                        for i := 0 to procs.Count-1 do
                          begin
                            sProc := trim(procs[i]);
                            if (copy(lowercase(trim(sProc)),0,8)='function')
                            or (copy(lowercase(trim(sProc)),0,9)='procedure') then
                              begin
                                tmp := copy(sProc,pos(' ',sProc)+1,length(sProc));
                                if pos('(',tmp)>0 then
                                  tmp := copy(tmp,0,pos('(',tmp)-1);
                                if pos(':',tmp)>0 then
                                  tmp := trim(copy(tmp,0,pos(':',tmp)-1))
                                else if pos(';',tmp)>0 then
                                  tmp := trim(copy(tmp,0,pos(';',tmp)-1));
                                if pos(')',sProc)>0 then
                                  tmp1 := copy(sProc,0,pos(')',sProc))
                                else tmp1 := '';
                                tmp3 := copy(sProc,length(tmp1)+1,length(sProc));
                                tmp1 := tmp1+copy(tmp3,0,pos(';',tmp3));
                                tmp2 := copy(sProc,pos(')',sProc)+1,length(sProc));
                                tmp2 := copy(tmp2,pos(';',tmp2)+1,Length(sProc));
                                tmp2 := copy(tmp2,0,pos(';',tmp2)-1);
                                if tmp2<>'' then
                                  tmp2 := ' '+tmp2;
                                tmp := '  '+tmp1+'external '''+tmp+'@'+aLibname+tmp2+''';';
                              end
                            else tmp := '  '+sProc;
                            newUnit := newUnit+LineEnding+tmp;
                          end;
                        newUnit := newUnit+LineEnding+'implementation'+lineending+'end.';
                        NewLib := TLoadedLib.Create;
                        NewLib.Name:=cName;
                        NewLib.Code:=newUnit;
                        LoadedLibs.Add(NewLib);
                        Result := Comp.Compile(newUnit);
                        for i := 0 to Comp.MsgCount-1 do
                          begin
                            CompleteOutput:=CompleteOutput+Compiler.Msg[i].MessageToString+LineEnding;
                            aMsg := Comp.Msg[i];
                            if Assigned(OnCompileMessage) then OnCompileMessage(Self,aMsg.UnitName,Comp.Msg[i].MessageToString,Comp.Msg[i].Pos,Comp.Msg[i].Row,Comp.Msg[i].Col);
                          end;
                        if not Result then
                          Debugln('Failed to compile Library(2):'+aLibName+' '+CompleteOutput);
                        Procs.Free;
                      end
                    else
                      begin
                        aProc := aprocT(dynlibs.GetProcAddress(aLib,'ScriptUnitDefinition'));
                        if Assigned(aProc) then
                          begin
                            newUnit := '';
                            sProc := aProc();
                            NewLib := TLoadedLib.Create;
                            NewLib.Name:=cName;
                            NewLib.Code:=StringReplace(sProc,'%dllpath%',aLibName,[rfReplaceAll]);
                            LoadedLibs.Add(NewLib);
                            Result := Comp.Compile(NewLib.Code);
                            for i := 0 to Comp.MsgCount-1 do
                              begin
                                CompleteOutput:=CompleteOutput+Compiler.Msg[i].MessageToString+LineEnding;
                                aMsg := Comp.Msg[i];
                                if Assigned(OnCompileMessage) then OnCompileMessage(Self,aMsg.UnitName,Comp.Msg[i].MessageToString,Comp.Msg[i].Pos,Comp.Msg[i].Row,Comp.Msg[i].Col);
                              end;
                            if not Result then
                              Debugln('Failed to compile Library(3):'+aLibName+' '+CompleteOutput);
                          end;
                      end;
                    aProc := aprocT(dynlibs.GetProcAddress(aLib,'ScriptTool'));
                    if Assigned(aProc) then
                      begin
                        if Assigned(OnToolRegistering) then
                          FToolRegistered(cName);
                      end;
                    FreeLibrary(aLib);
                  end
                else
                  Debugln('Library  clould not be loaded '+aLibName);
              end
            else //unit uses
              begin
                Result := False;
              end;
          end;
      end;
  except
    begin
      raise;
      Result := False; // will halt compilation
    end;
  end;
  if Assigned(FOnUses) then
    Result := FOnUses(Self,cName,Result) or Result;
end;
function ExtendICompiler(Sender: TPSPascalCompiler; const Name: tbtString
  ): Boolean;
begin
  Result := TPascalScript(TIPSPascalCompiler(Sender).Obj).InternalUses(Sender,Name);
end;
constructor TLoadedLib.Create;
begin
  Handle:=0;
end;
function TPascalScript.InternalExec(cmd: string): Integer;
var
  aLine: String;
  FProcess: TProcess;
begin
  FProcess := TProcess.Create(nil);
  Result := FProcesses.Add(FProcess);
  FProcess.CommandLine:=cmd;
  FProcess.Options:=[poUsePipes,poStderrToOutPut];
  FExecWriteString := nil;
  FProcess.ShowWindow:=swoHIDE;
  {$ifdef lcl}
  FProcess.PipeBufferSize:=1;
  {$endif}
  CompleteOutput:='';
  try
    FProcess.Execute;
  except
    on e : exception do
      begin
        aLine := 'Error:'+e.Message;
        if Assigned(FRuntime) then
          if FRuntime.GetProc('EXECLINERECEIVED') <> InvalidVal then
            FRuntime.RunProcPN([aLine],'EXECLINERECEIVED');
      end;
  end;
end;

function TPascalScript.InternalExecAndWatch(cmd: string;
  OnWriteln: TWriteStringEvent): Integer;
var
  aLine: String;
  FProcess: TProcess;
begin
  FProcess := TProcess.Create(nil);
  Result := FProcesses.Add(FProcess);
  FProcess.CommandLine:=cmd;
  FProcess.Options:=[poUsePipes,poStderrToOutPut];
  FProcess.ShowWindow:=swoHIDE;
  FExecWriteString := OnWriteln;
  {$ifdef lcl}
  FProcess.PipeBufferSize:=1;
  {$endif}
  CompleteOutput:='';
  try
    FProcess.Execute;
  except
    on e : exception do
      begin
        aLine := 'Error:'+e.Message;
        if Assigned(FRuntime) then
          if FRuntime.GetProc('EXECLINERECEIVED')<>InvalidVal then
            FRuntime.RunProcPN([aLine],'EXECLINERECEIVED');
        if Assigned(FExecWriteString) then
          FExecWriteString(Self,aLine);
      end;
  end;
end;

function TPascalScript.InternalVisualExec(cmd: string): Integer;
var
  aLine: String;
  FProcess: TProcess;
begin
  FProcess := TProcess.Create(nil);
  Result := FProcesses.Add(FProcess);
  FProcess.CommandLine:=cmd;
  FProcess.Options:=[poUsePipes,poStderrToOutPut];
  FProcess.ShowWindow:=swoShow;
  FExecWriteString := nil;
  {$ifdef lcl}
  FProcess.PipeBufferSize:=1;
  {$endif}
  CompleteOutput:='';
  try
    FProcess.Execute;
  except
    on e : exception do
      begin
        aLine := 'Error:'+e.Message;
        if Assigned(FRuntime) then
          if FRuntime.GetProc('EXECLINERECEIVED') <> InvalidVal then
            FRuntime.RunProcPN([aLine],'EXECLINERECEIVED');
      end;
  end;
end;

function TPascalScript.InternalVisualExecAndWatch(cmd: string;
  OnWriteln: TWriteStringEvent): Integer;
var
  aLine: String;
  FProcess: TProcess;
begin
  FProcess := TProcess.Create(nil);
  Result := FProcesses.Add(FProcess);
  FProcess.CommandLine:=cmd;
  FProcess.Options:=[poUsePipes,poStderrToOutPut];
  FProcess.ShowWindow:=swoShow;
  FExecWriteString := OnWriteln;
  {$ifdef lcl}
  FProcess.PipeBufferSize:=1;
  {$endif}
  CompleteOutput:='';
  try
    FProcess.Execute;
  except
    on e : exception do
      begin
        aLine := 'Error:'+e.Message;
        if Assigned(FRuntime) then
          if FRuntime.GetProc('EXECLINERECEIVED')<>InvalidVal then
            FRuntime.RunProcPN([aLine],'EXECLINERECEIVED');
        if Assigned(FExecWriteString) then
          FExecWriteString(Self,aLine);
      end;
  end;
end;

procedure TPascalScript.InternalExecWrite(Pid: Integer; cmd: string);
var
  FProcess: TProcess = nil;
begin
  if FProcesses.Count>Pid then
    FProcess := TProcess(FProcesses[Pid]);
  if Assigned(FProcess) and FProcess.Active then
    FProcess.Input.WriteAnsiString(cmd);
end;

function TPascalScript.InternalExecActive(Pid: Integer): Boolean;
var
  ReadSize: LongInt;
  Buffer : string;
  ReadCount: LongInt;
  aLine: String;
  FProcess: TProcess = nil;
begin
  if FProcesses.Count>Pid then
    FProcess := TProcess(FProcesses[Pid]);
  Result := Assigned(FProcess) and FProcess.Active;
  if Assigned(FProcess) and Assigned(FRuntime) and (FRuntime.Status=uPSRuntime.isRunning) then
    begin
      ReadSize := FProcess.Output.NumBytesAvailable;
      while ReadSize>0 do
        begin
          Setlength(Buffer,ReadSize);
          ReadCount := FProcess.Output.Read(Buffer[1], ReadSize);
          CompleteOutput:=CompleteOutput+SysToUni(copy(Buffer,0,ReadCount));
          ReadSize := FProcess.Output.NumBytesAvailable;
        end;
      while pos(#10,CompleteOutput)>0 do
        begin
          aLine := copy(CompleteOutput,0,pos(#10,CompleteOutput)-1);
          if Assigned(FRuntime) then
            if FRuntime.GetProc('EXECLINERECEIVED')<>InvalidVal then
              FRuntime.RunProcPN([aLine],'EXECLINERECEIVED');
          if Assigned(FExecWriteString) then
            FExecWriteString(Self,aLine);
          CompleteOutput:=copy(CompleteOutput,pos(#10,CompleteOutput)+1,length(CompleteOutput));
        end;
    end;
  sleep(1);
end;
function TPascalScript.InternalExecResult(Pid: Integer): Integer;
var
  FProcess: TProcess = nil;
begin
  Result := -1;
  if FProcesses.Count>Pid then
    begin
      FProcess := TProcess(FProcesses[Pid]);
      Result := FProcess.ExitStatus;
    end;
end;
function TPascalScript.InternalKill(Pid: Integer): Boolean;
var
  FProcess: TProcess = nil;
begin
  try
    if FProcesses.Count>Pid then
      FProcess := TProcess(FProcesses[Pid]);
    Result := Assigned(FProcess);
    if Result then
      begin
        {$ifdef UNIX}
        FpKill(FProcess.ProcessID,SIGINT);
        sleep(100);
        {$endif}
        if FProcess.Running then
          FProcess.Terminate(0);
        while FProcess.Running do InternalExecActive(Pid);
        InternalExecActive(Pid);
      end;
  except
  end;
end;
procedure TPascalScript.InternalBeep;
begin
  SysUtils.Beep;
end;
procedure TPascalScript.InternalSleep(MiliSecValue: LongInt);
begin
  OwnSleep(MiliSecValue);
end;

function TPascalScript.InternalRebootMashine: Boolean;
{$ifdef Windows}
var
  hLib: Handle;
  hProc: procedure;stdcall;
{$endif}
begin
{$ifdef Windows}
  WinExec('shutdown.exe -r -t 0', 0);
{$else}
  SysUtils.ExecuteProcess('/sbin/shutdown',['-r','now']);
{$endif}
end;
function TPascalScript.InternalShutdownMashine: Boolean;
{$ifdef Windows}
var
  hLib: Handle;
  hProc: procedure;stdcall;
{$endif}
begin
{$ifdef Windows}
{ Windows NT or newer }
  WinExec('shutdown.exe -s -t 0', 0);
{ Earlier than Windows NT }
  {$IFDEF UNICODE}
  hLib:=LoadLibraryW('user.dll');
  {$ELSE}
  hLib:=LoadLibraryA('user.dll');
  {$ENDIF}
  if hLib<>0 then begin
    if GetProcAddress(hLib, 'ExitWindows')<>Pointer(0) then
      begin
        Pointer(hProc):=GetProcAddress(hLib, 'ExitWindows');
        hProc;
        FreeLibrary(hLib);
      end;
    end;
{$else}
  SysUtils.ExecuteProcess('/sbin/shutdown',['-h','now']);
{$endif}
end;
function TPascalScript.InternalWakeMashine(Mac, Ip: string): Boolean;
begin
  Result := True;
  //WakeOnLan(Mac,Ip);
end;

function TPascalScript.InternalTimeToStr(Time: TDateTime): string;
begin
  Result := TimeToStr(Time);
end;

function TPascalScript.InternalDateTimeToStr(Time: TDateTime): string;
begin
  Result := DateTimeToStr(Time);
end;

function TPascalScript.InternalFormat(Fmt: string; Args: array of const): string;
begin
  Result := Format(Fmt,Args);
end;

function TPascalScript.InternalMathParse(Input: string): string;
var
  aParser: TMathParser;
  aTree: PTTermTreeNode;
begin
  Result := '';
  aParser := TMathParser.Create;
  try
    aTree := aParser.ParseTerm(Input);
    Result := FloatToStr(aParser.CalcTree(aTree))
  except
    on e : Exception do
      Result := e.message;
  end;
  aParser.Free;
end;

function TPascalScript.GetTyp: string;
begin
  Result := 'Pascal';
end;

function TPascalScript.GetStatus: TScriptStatus;
begin
  Result := ssNone;
  if Assigned(FRuntime) then
    begin
      if (not(FRuntime is TPSDebugExec)) then
        begin
          case FRuntime.Status of
          TPSStatus.isRunning:Result := ssRunning;
          isPaused:Result:=ssPaused;
          end;
        end
      else
        begin
          case TPSDebugExec(FRuntime).DebugMode of
          dmPaused,dmStepInto,dmStepOver:Result := ssPaused;
          dmRun:
            begin
              if FRuntime.Status=TPSStatus.isRunning then Result:=ssRunning;
            end;
          end;
        end;
    end;
end;

function TPascalScript.InternalGetTempDir: string;
begin
  Result := GetTempDir;
end;

procedure TPascalScript.Init;
begin
  FProcesses := TList.Create;
  FCompiler:= TIPSPascalCompiler.Create;
  FCompiler.AllowUnit:=True;
  FCompilerFree:=True;
  FRuntime:= TPSExec.Create;
  FRuntimeFree := True;
  FClassImporter:= TPSRuntimeClassImporter.CreateAndRegister(FRuntime, false);
end;

procedure TPascalScript.SetCompiler(AValue: TIPSPascalCompiler);
begin
  if FCompiler=AValue then Exit;
  try
    if FCompilerFree then
      FCompiler.Free;
  except
  end;
  FCompiler:=AValue;
  FCompilerFree := False;
  FCompiler.AllowUnit:=True;
end;
procedure TPascalScript.SetClassImporter(AValue: TPSRuntimeClassImporter);
begin
  if FClassImporter=AValue then Exit;
  if Assigned(FClassImporter) then
    FreeAndNil(FClassImporter);
  FClassImporter:=AValue;
end;
procedure TPascalScript.SetRuntime(AValue: TPSExec);
begin
  if FRuntime=AValue then Exit;
  if FRuntimeFree then
    FRuntime.Free;
  FRuntime:=AValue;
  FRuntimeFree:=False;
end;

procedure TPascalScript.InternalChDir(Directory: string);
begin
  chdir(UniToSys(Directory));
end;
procedure TPascalScript.InternalMkDir(Directory: string);
begin
  CreateDir(UniToSys(Directory));
end;

function TPascalScript.InternalApplicationDir: string;
begin
  Result := ExtractFileDir(ParamStr(0));
end;

function TPascalScript.InternalDirectoryExists(const Directory: String
  ): Boolean;
begin
  Result := DirectoryExists(UniToSys(Directory));
end;

function TPascalScript.Execute(aParameters: Variant; Debug: Boolean): Boolean;
var
  i: Integer;
  aDir: String;
  aProc: aProcT2;
  aRow: Integer;
  aMsg: String;
begin
  if Debug and (not (Runtime is TPSDebugExec)) then
    begin
      FreeAndNil(FRuntime);
      FRuntime:=TPSDebugExec.Create;
      FreeAndNil(FClassImporter);
      FClassImporter:= TPSRuntimeClassImporter.CreateAndRegister(FRuntime, false);
      (FRuntime as TPSDebugExec).OnSourceLine:=@OnSourceLine;
      (FRuntime as TPSDebugExec).OnIdleCall:=@IdleCall;
      (FRuntime as TPSDebugExec).DebugEnabled:=True;
      ByteCode:='';
    end
  else if (not (Runtime is TPSDebugExec)) then
    FRuntime.OnRunLine:=@OnRunActLine;
  aDir := GetCurrentDir;
  SetCurrentDir(GetHomeDir);
  Parameters:=aParameters;
  if ByteCode='' then
    Result := Compile
  else Result := True;
  Results:='';
  for i:= 0 to Compiler.MsgCount - 1 do
    if Length(Results) = 0 then
      Results:= Compiler.Msg[i].MessageToString
    else
      Results:= Results + #13#10 + Compiler.Msg[i].MessageToString;
  if Result then
    begin
      try
        ActRuntime := Self;
        Result := FRuntime.RunScript
              and (FRuntime.ExceptionCode = erNoError);
        if not Result then
          begin
            Results:= PSErrorToString(FRuntime.LastEx, '');
            //aMsg := Format(STR_RUNTIME_ERROR, ['File', Debugger.ExecErrorRow,Debugger.ExecErrorCol,Debugger.ExecErrorProcNo,Debugger.ExecErrorByteCodePosition,Debugger.ExecErrorToString])
            if Assigned(OnCompileMessage) then OnCompileMessage(Self,'',TIFErrorToString(Runtime.ExceptionCode, Runtime.ExceptionString),Runtime.ExceptionPos,0,0);
            ByteCode:='';//recompile on unsuccesful execution
          end;
        try
          for i := 0 to FProcesses.Count-1 do
            InternalKill(i);
        except
        end;
      except
        on e : Exception do
          begin
            Results:=e.Message;
            Result := false;
          end;
      end;
      try
        DoCleanup;
      except
      end;
    end;
  SetCurrentDir(aDir);
end;

function TPascalScript.AddMethodEx(Slf, Ptr: Pointer; const Decl: tbtstring;
  CallingConv: uPSRuntime.TPSCallingConvention): Boolean;
var
  P: TPSRegProc;
begin
  p := FCompiler.AddDelphiFunction(Decl);
  if p <> nil then
  begin
    FRuntime.RegisterDelphiMethod(Slf, Ptr, p.Name, CallingConv);
    Result := True;
  end else Result := False;
end;
function TPascalScript.AddMethod(Slf, Ptr: Pointer; const Decl: tbtstring
  ): Boolean;
begin
  Result := AddMethodEx(Slf, Ptr, Decl, cdRegister);
end;
function TPascalScript.AddFunction(Ptr: Pointer; const Decl: tbtstring
  ): Boolean;
begin
  Result := AddFunctionEx(Ptr, Decl, cdRegister);
end;
function TPascalScript.AddFunctionEx(Ptr: Pointer; const Decl: tbtstring;
  CallingConv: uPSRuntime.TPSCallingConvention): Boolean;
var
  P: TPSRegProc;
begin
  p := FCompiler.AddDelphiFunction(Decl);
  if p <> nil then
  begin
    FRuntime.RegisterDelphiFunction(Ptr, p.Name, CallingConv);
    Result := True;
  end else Result := False;
end;
function TPascalScript.Compile: Boolean;
var
  i: Integer;
  aBytecode: tbtString;
  aDebugData: tbtString;
  aMsg: TPSPascalCompilerMessage;
begin
  Result := False;
  if not Assigned(Compiler) then exit;
  CompleteOutput:='';
  Compiler.Obj := Self;
  Compiler.OnUses:= @ExtendICompiler;
  Result:= Compiler.Compile(Source)
       and Compiler.GetOutput(aBytecode);
  ByteCode:=aBytecode;
  for i := 0 to Compiler.MsgCount-1 do
    begin
      CompleteOutput:=CompleteOutput+Compiler.Msg[i].MessageToString+LineEnding;
      aMsg := Compiler.Msg[i];
      if Assigned(OnCompileMessage) then OnCompileMessage(Self,aMsg.UnitName,Compiler.Msg[i].MessageToString,Compiler.Msg[i].Pos,Compiler.Msg[i].Row,Compiler.Msg[i].Col);
    end;
  Runtime.Clear;
  if FRuntime is TPSDebugExec then
    begin
      Compiler.GetDebugOutput(aDebugData);
      result := TPSDebugExec(FRuntime).LoadData(Bytecode);
      TPSDebugExec(FRuntime).LoadDebugData(aDebugData);
    end
  else
    Result:= Result and FRuntime.LoadData(Bytecode);
  if Result and (not (Runtime is TPSDebugExec)) then
    FRuntime.OnRunLine:=@OnRunActLine;
end;

function TPascalScript.Stop: Boolean;
begin
  Result:=False;
  if Assigned(FRuntime) then
    begin
      FRuntime.Stop;
      FRuntime.Cleanup;
      Result := True;
    end;
end;

function TPascalScript.StepInto: Boolean;
begin
  Result:=False;
  if Assigned(FRuntime) and (FRuntime is TPSDebugExec) then
    begin
      TPSDebugExec(FRuntime).StepInto;
      Result := True;
    end;
end;

function TPascalScript.StepOver: Boolean;
begin
  Result:=False;
  if Assigned(FRuntime) and (FRuntime is TPSDebugExec) then
    begin
      TPSDebugExec(FRuntime).StepOver;
      Result := True;
    end;
end;

function TPascalScript.Resume: Boolean;
begin
  Result:=False;
  if Assigned(FRuntime) and (FRuntime is TPSDebugExec) then
    begin
      TPSDebugExec(FRuntime).Run;
      Result := True;
    end
  else if Assigned(FRuntime) then
    begin
      FRuntime.RunScript;
      Result := True;
    end;
end;

function TPascalScript.Pause: Boolean;
begin
  Result:=False;
  if Assigned(FRuntime) then
    begin
      FRuntime.Pause;
      Result := True;
    end;
end;

function TPascalScript.IsRunning: Boolean;
begin
  Result:=(Status=ssRunning) or (Status=ssPaused);
end;

function TPascalScript.RunScriptFunction(const Params: array of Variant;fName : string): Variant;
var
  tmp: TbtString;
  aCode: TPSError;
begin
  try
    Result := True;
    if ByteCode='' then Result := Compile;
    if Result then
      Result := Runtime.RunProcPN(Params,fName);
  except
    on e : Exception do
      begin
        aCode := Runtime.ExceptionCode;
        if aCode <> ErNoError then
          begin
            tmp:= TIFErrorToString(Runtime.ExceptionCode, Runtime.ExceptionString)+' '+IntToStr(Runtime.ExceptionPos);
            raise Exception.Create(tmp);
          end
        else raise;
      end;
  end;
end;

function TPascalScript.GetVarContents(Identifier: string): string;
var
  i: Longint;
  pv: PIFVariant;
  s1, s: tbtstring;
begin
  Result := '';
  if (FRuntime is TPSDebugExec) and (GetStatus=ssPaused) then
    with TPSDebugExec(FRuntime) do
      begin
        s := Uppercase(Identifier);
        if pos('.', s) > 0 then
        begin
          s1 := copy(s,1,pos('.', s) -1);
          delete(s,1,pos('.', Identifier));
        end else begin
          s1 := s;
          s := '';
        end;
        pv := nil;
        for i := 0 to TPSDebugExec(Runtime).CurrentProcVars.Count -1 do
        begin
          if Uppercase(TPSDebugExec(Runtime).CurrentProcVars[i]) =  s1 then
          begin
            pv := TPSDebugExec(Runtime).GetProcVar(i);
            break;
          end;
        end;
        if pv = nil then
        begin
          for i := 0 to TPSDebugExec(Runtime).CurrentProcParams.Count -1 do
          begin
            if Uppercase(TPSDebugExec(Runtime).CurrentProcParams[i]) =  s1 then
            begin
              pv := TPSDebugExec(Runtime).GetProcParam(i);
              break;
            end;
          end;
        end;
        if pv = nil then
        begin
          for i := 0 to TPSDebugExec(Runtime).GlobalVarNames.Count -1 do
          begin
            if Uppercase(TPSDebugExec(Runtime).GlobalVarNames[i]) =  s1 then
            begin
              pv := TPSDebugExec(Runtime).GetGlobalVar(i);
              break;
            end;
          end;
        end;
        if pv <> nil then
          Result := PSVariantToString(NewTPSVariantIFC(pv, False), s);
      end;
end;

constructor TPascalScript.Create;
begin
  inherited Create;
end;

destructor TPascalScript.Destroy;
var
  i: Integer;
  aProc: TProcess;
begin
  if Assigned(FProcesses) then
    begin
      for i := 0 to FProcesses.Count-1 do
        if Assigned(FProcesses[i]) then
          begin
            aProc := TProcess(FProcesses[i]);
            aProc.Free;
            FProcesses[i]:=nil;
          end;
      FProcesses.Clear;
      FreeAndNil(FProcesses);
    end;
  Pause;
  Stop;
  if FCompilerFree then
    FCompiler.Free;
  if FRuntimeFree then
    FreeAndNil(FRuntime);
  inherited Destroy;
end;

procedure TPascalScript.OpenTool(aName: string);
var
  i: Integer;
  aProc: aProcT2;
begin
  for i := 0 to LoadedLibs.Count-1 do
    if lowercase(TLoadedLib(LoadedLibs[i]).Name)=lowercase(aName) then
      begin
        aProc := aprocT2(dynlibs.GetProcAddress(TLoadedLib(LoadedLibs[i]).Handle,'ScriptTool'));
        if Assigned(aProc) then
          aProc;
      end;
end;

procedure ScriptBeep;
begin
  {$ifdef WINDOWS}
  Windows.Beep(1000,120);
  //PlaySoundW(PWideChar('Beep'),hInstance, SND_ASYNC);
  {$endif}
end;

initialization
  LoadedLibs := TList.Create;
  SysUtils.OnBeep:=@ScriptBeep;
finalization
  LoadedLibs.Clear;
  LoadedLibs.Free;
end.

