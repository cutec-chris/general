unit comparewild;
{Copyright (C) 2007 Thomas Kelsey

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.}


// KNOWN BUGS: accepts wildcards in target string
interface

function WildComp(const mask: String; const target: String): Boolean;

function Test: Boolean;

implementation

function WildComp(const mask: String; const target: String): Boolean;

    // '*' matches greedy & ungreedy
    // simple recursive descent parser - not fast but easy to understand
    function WComp(const maskI: Integer; const targetI: Integer): Boolean;
    begin

        if maskI > Length(mask) then begin
            Result := targetI = Length(target) + 1;
            Exit;
        end;
        if targetI > Length(target) then begin
            // unread chars in filter or would have read '#0'
            Result := False;
            Exit;
        end;

        case mask[maskI] of
            '*':
                // '*' doesnt match '.'
                if target[targetI] <> '.' then
                    // try with and without ending match - but always matches at least one char
                    Result := WComp(succ(maskI), Succ(targetI)) or WComp(maskI, Succ(targetI))
                else
                    Result := False;
            '?':
                // ? doesnt match '.'
                if target[targetI] <> '.' then
                    Result := WComp(succ(maskI), Succ(targetI))
                else
                    Result := False;

            else     // includes '.' which only matches itself
                if mask[maskI] = target[targetI] then
                    Result := WComp(succ(maskI), Succ(targetI))
                else
                    Result := False;
        end;// case

    end;

begin
    WildComp := WComp(1, 1);
end;

function Test: Boolean;
begin
Result := WildComp('a*.bmp', 'auto.bmp');
    Result := Result and (not WildComp('a*x.bmp', 'auto.bmp'));
    Result := Result and WildComp('a*o.bmp', 'auto.bmp');
    Result := Result and (not WildComp('a*tu.bmp', 'auto.bmp'));
    Result := Result and WildComp('a*o.b*p', 'auto.bmp') and (WildComp('a*to.*', 'auto.bmp'));
    Result := Result and WildComp('a**o.b*p', 'auto.bmp');
    Result := Result and (WildComp('*ut*.**', 'auto.bmp'));
    Result := Result and (WildComp('*ut*.*.*', 'auto.bmp.splack'));
    Result := Result and WildComp('**.**', 'auto.bmp') and (not WildComp('*ut*', 'auto.bmp'));
    // '*' = at least 1 char
    Result := Result and not WildComp('**', 'a');
    // shows '.' <> '*'
    Result := Result and (not WildComp('*ut*.*', 'auto.bmp.foo'));
    // shows un-greedy match
    Result := Result and (WildComp('*ut', 'autout'));

    Result := Result and (not WildComp('auto?', 'auto'));
    Result := Result and not WildComp('?uto', 'uto');
    Result := Result and WildComp('aut?', 'auto');
    Result := Result and WildComp('???', 'uto');
    Result := Result and not WildComp('????', 'uto');
    Result := Result and not WildComp('??', 'uto');

end;
end.
