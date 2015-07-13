unit pwgenerator;

{$mode objfpc}{$H+}

interface

uses
  Sysutils;

CONST
  PWLOWERCASESTRINGS: String = 'abcdefghijklmnopqrstuvwxyz';
  PWUPPERCASESTRINGS: String = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  PWNUMBERS: String = '0123456789';
  PWSPECIAL   = '$@&#!+%.,;-=';
  PROJECTNAME = 'Advanced Password Generator V1.1.4';
  PROJECTURL = 'https://sourceforge.net/projects/apwg/';
  WIKIURL    = 'https://sourceforge.net/p/apwg/wiki/Home/';
  COPYRIGHT  = 'Copyright (c) 2014 by Viktor Tassi';

type


  TApwGen = record
     MinSize : Integer;// = 8;
     MaxSize : Integer;// = 8;
     LowerCases : Boolean;// = True;
     UpperCases : Boolean;// = True;
     Numbers : Boolean;// = False;
     Specials : Boolean;
     AllUnique : Boolean;// = True;
     CustomChars : String;// = '';
   end;

  function GeneratePWD(var Apwgen:TApwGen):String;
  procedure ReRandomize;

  var
   ApwGen: TApwGen =( Minsize:8;
                       MaxSize:8;
                       LowerCases:True;
                       UpperCases:True;
                       Numbers:True;
                       Specials:False;
                       AllUnique:True;
                       CustomChars:''
                       );


implementation

{ TApwGen }

function GeneratePWD(var Apwgen:TApwGen): String;
var
  chars, tmp : String;
  i, j, k, pwlen : Integer;
  c: char;
begin
 result := '';
 chars :='';
 if Apwgen.Minsize < 0 then
   Apwgen.MinSize:=0;

 if Apwgen.MaxSize < Apwgen.MinSize then
   Apwgen.MaxSize:= Apwgen.MinSize;

 if Apwgen.MinSize <1 then exit;

 if Apwgen.LowerCases then
    chars:=chars+PWLOWERCASESTRINGS;

 if Apwgen.UpperCases then
      chars:=chars+PWUPPERCASESTRINGS;

 if Apwgen.Numbers then
      chars:=chars+PWNUMBERS;

 if Apwgen.Specials then
      chars:=chars+PWSPECIAL;

 if Apwgen.CustomChars <> '' then
      chars := chars+Apwgen.CustomChars;

 if chars = '' then exit;

 //Dedup
 tmp := chars;
 chars := '';

 for i := 1 to length(tmp) do
   if ansipos(tmp[i],chars) = 0 then
     chars:= chars+tmp[i];

 ReRandomize;

  //Shuffle
  if length(chars)>2 then
   for k:=3 to random(4096)+1 do;
    for i := length(chars) downto 2 do
      begin
      c:= chars[i];
      j:= Succ(Random(length(chars)));
      chars[i] := chars[j];
      chars[j] := c;
      end;

  if Apwgen.MinSize <> Apwgen.MaxSize then
    pwlen := random(Apwgen.MaxSize - Apwgen.MinSize) + Apwgen.MinSize
  else
    pwlen := Apwgen.MinSize;

  for i := 1 to pwlen do begin

    ReRandomize;

    tmp := chars[random(length(chars))+1];

    if Apwgen.AllUnique then
      if ansipos(tmp,result) > 0 then
        for j := 1 to length(chars) do begin
          tmp := chars[random(length(chars))+1];
          if ansipos(tmp,result) = 0 then break;
        end;
      result := result + tmp;

  end;
end;

procedure ReRandomize;
var
    g:TGuid;
begin
  CreateGUID(g);
  RandSeed:= RandSeed XOR g.clock_seq_low XOR g.Data1 XOR g.Data2;
end;


end.

