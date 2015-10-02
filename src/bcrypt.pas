unit Bcrypt;

{
    Bcrypt is an algorithm designed for hashing passwords, and only passwords.
            i.e. It's not a generic, high-speed, generic hashing algorithm.
                      It's computationally and memory expensive
                      It's limited to passwords of 55 bytes.

    http://static.usenix.org/events/usenix99/provos/provos.pdf

    It uses the Blowfish encryption algorithm, but with an "expensive key setup"
    modification, contained in the function EksBlowfishSetup.

    Ian Boyd  5/3/2012
    Public Domain

    v1.0 - Initial release
}

interface

uses
    Blowfishbc, Types, Math, ComObj;

type
    UnicodeString = WideString;

    TBCrypt = class(TObject)
    private
            class function TryParseHashString(const hashString: AnsiString;
                            out version: string; out Cost: Integer; out Salt{, Hash}: TByteDynArray): Boolean;
    protected
            class function EksBlowfishSetup(const Cost: Integer; salt, key: array of Byte): TBlowfishData;
            class procedure ExpandKey(var state: TBlowfishData; salt, key: array of Byte);
            class function CryptCore(const Cost: Integer; Key: array of Byte; salt: array of Byte): TByteDynArray;

            class function FormatPasswordHashForBsd(const cost: Integer; const salt: array of Byte; const hash: array of Byte): AnsiString;

            class function BsdBase64Encode(const data: array of Byte; BytesToEncode: Integer): AnsiString;
            class function BsdBase64Decode(const s: AnsiString): TByteDynArray;

            class function WideStringToUtf8(const Source: UnicodeString): AnsiString;

            class function SelfTestA: Boolean; //known test vectors
            class function SelfTestB: Boolean; //BSD's base64 encoder/decoder
            class function SelfTestC: Boolean; //unicode strings in UTF8
            class function SelfTestD: Boolean; //different length passwords
            class function SelfTestE: Boolean; //salt rng

            class function GenRandomBytes(len: Integer; const data: Pointer): HRESULT;
    public
            //Hashes a password into the OpenBSD password-file format (non-standard base-64 encoding). Also validate that BSD style string
            class function HashPassword(const password: UnicodeString): AnsiString; overload;
            class function CheckPassword(const password: UnicodeString; const expectedHashString: AnsiString): Boolean; overload;

            //If you want to handle the cost, salt, and encoding yourself, you can do that.
            class function HashPassword(const password: UnicodeString; const salt: array of Byte; const cost: Integer): TByteDynArray; overload;
            class function CheckPassword(const password: UnicodeString; const salt, hash: array of Byte; const Cost: Integer): Boolean; overload;
            class function GenerateSalt: TByteDynArray;

            class function SelfTest: Boolean;
    end;

implementation

uses
    Windows, SysUtils,
{$IFDEF UnitTests}TestFramework, {$ENDIF}
    ActiveX;

const
    BCRYPT_COST = 10; //cost determintes the number of rounds. 10 = 2^10 rounds (1024)
    BCRYPT_SALT_LEN = 16; //bcrypt uses 128-bit (16-byte) salt (This isn't an adjustable parameter, just a name for a constant)

    BsdBase64EncodeTable: array[0..63] of Char =
                    { 0:} './'+
                    { 2:} 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'+
                    {28:} 'abcdefghijklmnopqrstuvwxyz'+
                    {54:} '0123456789';

                    //the traditional base64 encode table:
                    //'ABCDEFGHIJKLMNOPQRSTUVWXYZ' +
                    //'abcdefghijklmnopqrstuvwxyz' +
                    //'0123456789+/';

    BsdBase64DecodeTable: array[#0..#127] of Integer = (
                    {  0:} -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,  // ________________
                    { 16:} -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,  // ________________
                    { 32:} -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,  0,  1,  // ______________./
                    { 48:} 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, -1, -1, -1, -1, -1, -1,  // 0123456789______
                    { 64:} -1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16,  // _ABCDEFGHIJKLMNO
                    { 80:} 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, -1, -1, -1, -1, -1,  // PQRSTUVWXYZ_____
                    { 96:} -1, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42,  // _abcdefghijklmno
                    {113:} 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, -1, -1, -1, -1, -1); // pqrstuvwxyz_____

    TestVectors: array[1..20, 1..3] of string = (
                    ('',                                   '$2a$06$DCq7YPn5Rq63x1Lad4cll.',    '$2a$06$DCq7YPn5Rq63x1Lad4cll.TV4S6ytwfsfvkgY8jIucDrjc8deX1s.'),
                    ('',                                   '$2a$08$HqWuK6/Ng6sg9gQzbLrgb.',    '$2a$08$HqWuK6/Ng6sg9gQzbLrgb.Tl.ZHfXLhvt/SgVyWhQqgqcZ7ZuUtye'),
                    ('',                                   '$2a$10$k1wbIrmNyFAPwPVPSVa/ze',    '$2a$10$k1wbIrmNyFAPwPVPSVa/zecw2BCEnBwVS2GbrmgzxFUOqW9dk4TCW'),
                    ('',                                   '$2a$12$k42ZFHFWqBp3vWli.nIn8u',    '$2a$12$k42ZFHFWqBp3vWli.nIn8uYyIkbvYRvodzbfbK18SSsY.CsIQPlxO'),
                    ('a',                                  '$2a$06$m0CrhHm10qJ3lXRY.5zDGO',    '$2a$06$m0CrhHm10qJ3lXRY.5zDGO3rS2KdeeWLuGmsfGlMfOxih58VYVfxe'),
                    ('a',                                  '$2a$08$cfcvVd2aQ8CMvoMpP2EBfe',    '$2a$08$cfcvVd2aQ8CMvoMpP2EBfeodLEkkFJ9umNEfPD18.hUF62qqlC/V.'),
                    ('a',                                  '$2a$10$k87L/MF28Q673VKh8/cPi.',    '$2a$10$k87L/MF28Q673VKh8/cPi.SUl7MU/rWuSiIDDFayrKk/1tBsSQu4u'),
                    ('a',                                  '$2a$12$8NJH3LsPrANStV6XtBakCe',    '$2a$12$8NJH3LsPrANStV6XtBakCez0cKHXVxmvxIlcz785vxAIZrihHZpeS'),
                    ('abc',                                '$2a$06$If6bvum7DFjUnE9p2uDeDu',    '$2a$06$If6bvum7DFjUnE9p2uDeDu0YHzrHM6tf.iqN8.yx.jNN1ILEf7h0i'),
                    ('abc',                                '$2a$08$Ro0CUfOqk6cXEKf3dyaM7O',    '$2a$08$Ro0CUfOqk6cXEKf3dyaM7OhSCvnwM9s4wIX9JeLapehKK5YdLxKcm'),
                    ('abc',                                '$2a$10$WvvTPHKwdBJ3uk0Z37EMR.',    '$2a$10$WvvTPHKwdBJ3uk0Z37EMR.hLA2W6N9AEBhEgrAOljy2Ae5MtaSIUi'),
                    ('abc',                                '$2a$12$EXRkfkdmXn2gzds2SSitu.',    '$2a$12$EXRkfkdmXn2gzds2SSitu.MW9.gAVqa9eLS1//RYtYCmB1eLHg.9q'),
                    ('abcdefghijklmnopqrstuvwxyz',         '$2a$06$.rCVZVOThsIa97pEDOxvGu',    '$2a$06$.rCVZVOThsIa97pEDOxvGuRRgzG64bvtJ0938xuqzv18d3ZpQhstC'),
                    ('abcdefghijklmnopqrstuvwxyz',         '$2a$08$aTsUwsyowQuzRrDqFflhge',    '$2a$08$aTsUwsyowQuzRrDqFflhgekJ8d9/7Z3GV3UcgvzQW3J5zMyrTvlz.'),
                    ('abcdefghijklmnopqrstuvwxyz',         '$2a$10$fVH8e28OQRj9tqiDXs1e1u',    '$2a$10$fVH8e28OQRj9tqiDXs1e1uxpsjN0c7II7YPKXua2NAKYvM6iQk7dq'),
                    ('abcdefghijklmnopqrstuvwxyz',         '$2a$12$D4G5f18o7aMMfwasBL7Gpu',    '$2a$12$D4G5f18o7aMMfwasBL7GpuQWuP3pkrZrOAnqP.bmezbMng.QwJ/pG'),
                    ('~!@#$%^&*()      ~!@#$%^&*()PNBFRD', '$2a$06$fPIsBO8qRqkjj273rfaOI.',    '$2a$06$fPIsBO8qRqkjj273rfaOI.HtSV9jLDpTbZn782DC6/t7qT67P6FfO'),
                    ('~!@#$%^&*()      ~!@#$%^&*()PNBFRD', '$2a$08$Eq2r4G/76Wv39MzSX262hu',    '$2a$08$Eq2r4G/76Wv39MzSX262huzPz612MZiYHVUJe/OcOql2jo4.9UxTW'),
                    ('~!@#$%^&*()      ~!@#$%^&*()PNBFRD', '$2a$10$LgfYWkbzEvQ4JakH7rOvHe',    '$2a$10$LgfYWkbzEvQ4JakH7rOvHe0y8pHKF9OaFgwUZ2q7W2FFZmZzJYlfS'),
                    ('~!@#$%^&*()      ~!@#$%^&*()PNBFRD', '$2a$12$WApznUOJfkEGSmYRfnkrPO',    '$2a$12$WApznUOJfkEGSmYRfnkrPOr466oFDCaj4b6HY3EXGvfxm43seyhgC')
    );


{$IFDEF UnitTests}
type
    TBCryptTests = class(TTestCase)
    public
            procedure SelfTest;

            //These are just too darn slow (as they should be) for continuous testing
            procedure SelfTestA_KnownTestVectors;
            procedure SelfTestC_UnicodeStrings;
            procedure SelfTestD_VariableLengthPasswords;
    published
            procedure SelfTestB_Base64EncoderDecoder;
    end;
{$ENDIF}

const
    advapi32 = 'advapi32.dll';

function CryptAcquireContextW(out phProv: THandle; pszContainer: PWideChar; pszProvider: PWideChar; dwProvType: DWORD; dwFlags: DWORD): BOOL; stdcall; external advapi32;
function CryptReleaseContext(hProv: THandle; dwFlags: DWORD): BOOL; stdcall; external advapi32;
function CryptGenRandom(hProv: THandle; dwLen: DWORD; pbBuffer: Pointer): BOOL; stdcall; external advapi32;

{ TBCrypt }

class function TBCrypt.HashPassword(const password: UnicodeString): AnsiString;
var
    cost: Integer;
    salt: TByteDynArray;
    hash: TByteDynArray;
begin
{       bcrypt was designed for OpenBSD, where hashes in the password file have a
    certain format.

    The convention used in BSD when generating password hash strings is to format it as:
                    $version$salt$hash

    MD5 hash uses version "1":
                    "$"+"1"+"$"+salt+hash

    bcrypt uses version "2a", but also encodes the cost

                    "$"+"2a"+"$"+rounds+"$"+salt+hash

    e.g.
                    $2a$10$Ro0CUfOqk6cXEKf3dyaM7OhSCvnwM9s4wIX9JeLapehKK5YdLxKcm
                    $==$==$======================-------------------------------

    The benfit of this scheme is:
                    - the number of rounds
                    - the salt used

    This means that stored hashes are backwards and forwards compatible with
    changing the number of rounds
}
    salt := GenerateSalt();
    cost := BCRYPT_COST;

    //utf8 := TBCrypt.WideStringToUtf8(password);
    hash := TBCrypt.HashPassword(password, salt, cost);

    Result := FormatPasswordHashForBsd(cost, salt, hash);
end;

class function TBCrypt.GenerateSalt: TByteDynArray;
var
    type4Uuid: TGUID;
    salt: TByteDynArray;
begin
    //Salt is a 128-bit (16 byte) random value
    SetLength(salt, BCRYPT_SALT_LEN);

    //Type 4 UUID (RFC 4122) is a handy source of (almost) 128-bits of random data (actually 120 bits)
    //But the security doesn't come from the salt being secret, it comes from the salt being different each time
    OleCheck(CoCreateGUID(Type4Uuid));

    Move(type4Uuid.D1, salt[0], BCRYPT_SALT_LEN); //16 bytes

    Result := salt;
end;

class function TBCrypt.HashPassword(const password: UnicodeString; const salt: array of Byte; const cost: Integer): TByteDynArray;
var
    key: TByteDynArray;
    len: Integer;
    utf8Password: AnsiString;
begin
    //pseudo-standard dictates that unicode strings are converted to UTF8 (rather than UTF16, UTF32, UTF16LE, etc)
    utf8Password := TBCrypt.WideStringToUtf8(password);

    //key is 56 bytes.
    //bcrypt version 2a defines that we include the null terminator
    //this leaves us with 55 characters we can include
    len := Length(utf8Password);
    if len > 55 then
            len := 55;

    SetLength(key, len+1); //+1 for the null terminator

    if Length(utf8Password) > 0 then
            Move(utf8Password[1], key[0], len);

    //set the final null terminator
    key[len] := 0;

    Result := TBCrypt.CryptCore(cost, key, salt);
end;

class function TBCrypt.CryptCore(const Cost: Integer; key, salt: array of Byte): TByteDynArray;
var
    state: TBlowfishData;
    i: Integer;
    plainText: array[0..23] of Byte;
    cipherText: array[0..23] of Byte;

const
    magicText: AnsiString = 'OrpheanBeholderScryDoubt'; //the 24-byte data we will be encrypting 64 times
begin
    state := TBCrypt.EksBlowfishSetup(cost, salt, key);

    //Conceptually we are encrypting "OrpheanBeholderScryDoubt" 64 times
    Move(magicText[1], plainText[0], 24);

    for i := 1 to 64 do
    begin
            //The painful thing is that the plaintext is 24 bytes long; this is three 8-byte blocks.
            //Which means we have to do the EBC encryption on 3 separate sections.
            BlowfishEncryptECB(state, Pointer(@plainText[ 0]), Pointer(@cipherText[ 0]));
            BlowfishEncryptECB(state, Pointer(@plainText[ 8]), Pointer(@cipherText[ 8]));
            BlowfishEncryptECB(state, Pointer(@plainText[16]), Pointer(@cipherText[16]));

            Move(cipherText[0], plainText[0], 24);
    end;

    SetLength(Result, 24);
    Move(cipherText[0], Result[0], 24);
end;


class function TBCrypt.EksBlowfishSetup(const Cost: Integer; salt, key: array of Byte): TBlowfishData;
var
    rounds: Cardinal; //rounds = 2^cost
    i: Integer;
    Len: Integer;
const
    zero: array[0..15] of Byte = (0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0);
begin
    //Expensive key setup
    if (cost < 4) or (cost > 31) then
            raise Exception.Create('Blowfish: Cost ('+IntToStr(Cost)+') must be between 4..31');

    Len := Length(key);
    if (Len > 56) then
            raise Exception.Create('Blowfish: Key must be between 1 and 56 bytes long');

    if Length(salt) <> BCRYPT_SALT_LEN then
            raise Exception.Create('Blowfish: salt must be 16 bytes');

    //Copy S and P boxes into local state
    BlowfishInitState(Result);

    Self.ExpandKey(Result, salt, key);

    //rounds = 2^cost
    rounds := 1 shl cost;

    for i := 1 to rounds do
    begin
            Self.ExpandKey(Result, zero, key);
            Self.ExpandKey(Result, zero, salt);
    end;

    //Result := what it is
end;

class procedure TBCrypt.ExpandKey(var State: TBlowfishData; salt, key: array of Byte);
var
    i, j, k: Integer;
    A: DWord;
    KeyB: PByteArray;
    Block: array[0..7] of Byte;
    Len: Integer;
    saltHalf: Integer;
begin
    //ExpandKey phase of the Expensive key setup
    Len := Length(key);
    if (Len > 56) then
            raise Exception.Create('Blowfish: Key must be between 1 and 56 bytes long');

    {
            XOR all the subkeys in the P-array with the encryption key
            The first 32 bits of the key are XORed with P1, the next 32 bits with P2, and so on.
            The key is viewed as being cyclic; when the process reaches the end of the key,
            it starts reusing bits from the beginning to XOR with subkeys.
    }
    if len > 0 then
    begin
            KeyB := PByteArray(@key[0]);
            k := 0;
            for i := 0 to 17 do
            begin
                    A :=      KeyB^[(k+3) mod Len];
                    A := A + (KeyB^[(k+2) mod Len] shl 8);
                    A := A + (KeyB^[(k+1) mod Len] shl 16);
                    A := A + (KeyB^[k]             shl 24);
                    State.PBoxM[i] := State.PBoxM[i] xor A;
                    k := (k+4) mod Len;
            end;
    end;

    //Blowfsh-encrypt the first 64 bits of its salt argument using the current state of the key schedule.
    BlowfishEncryptECB(State, @salt[0], @Block);

    //The resulting ciphertext replaces subkeys P1 and P2.
    State.PBoxM[0] := Block[3] + (Block[2] shl 8) + (Block[1] shl 16) + (Block[0] shl 24);
    State.PBoxM[1] := Block[7] + (Block[6] shl 8) + (Block[5] shl 16) + (Block[4] shl 24);

    saltHalf := 1;
    for i := 1 to 8 do
    begin
            //That same ciphertext is also XORed with the second 64-bits of salt
            for k := 0 to 7 do
                    block[k] := block[k] xor salt[(saltHalf*8)+k]; //Salt is 0..15 (0..7 is first block, 8..15 is second block)
            saltHalf := saltHalf xor 1;

            //and the result encrypted with the new state of the key schedule
            BlowfishEncryptECB(State, @Block, @Block);

            // The output of the second encryption replaces subkeys P3 and P4. (P[2] and P[3])
            State.PBoxM[i*2] :=   Block[3] + (Block[2] shl 8) + (Block[1] shl 16) + (Block[0] shl 24);
            State.PBoxM[i*2+1] := Block[7] + (Block[6] shl 8) + (Block[5] shl 16) + (Block[4] shl 24);
    end;

    //When ExpandKey finishes replacing entries in the P-Array, it continues on replacing S-box entries two at a time.
    for j := 0 to 3 do
    begin
            for i := 0 to 127 do
            begin
                    //That same ciphertext is also XORed with the second 64-bits of salt
                    for k := 0 to 7 do
                            block[k] := block[k] xor salt[(saltHalf*8 mod 16)+k]; //Salt is 0..15 (0..7 is first block, 8..15 is second block)
                    saltHalf := saltHalf xor 1;

                    //and the result encrypted with the new state of the key schedule
                    BlowfishEncryptECB(State, @Block, @Block);

                    // The output of the second encryption replaces subkeys S1 and P2. (S[0] and S[1])
                    State.SBoxM[j, i*2] :=   Block[3] + (Block[2] shl 8) + (Block[1] shl 16) + (Block[0] shl 24);
                    State.SBoxM[j, i*2+1] := Block[7] + (Block[6] shl 8) + (Block[5] shl 16) + (Block[4] shl 24);
            end;
    end;
end;

class function TBCrypt.CheckPassword(const password: UnicodeString; const salt, hash: array of Byte; const Cost: Integer): Boolean;
var
    candidateHash: TByteDynArray;
    len: Integer;
begin
    Result := False;

    candidateHash := TBCrypt.HashPassword(password, salt, cost);

    len := Length(hash);
    if Length(candidateHash) <> len then
            Exit;

    Result := CompareMem(@candidateHash[0], @hash[0], len);
end;

class function TBCrypt.TryParseHashString(const hashString: AnsiString;
            out version: string; out Cost: Integer; out Salt: TByteDynArray{; out Hash: TByteDynArray}): Boolean;
var
    work: AnsiString;
    s: AnsiString;
begin
    Result := False;

    {
            Pick apart our specially formatted hash string

            $2a$nn$(22 character salt, b64 encoded)(32 character hash, b64 encoded)

            We also need to accept version 2, the original version
    }
    if Length(hashString) < 28 then
            Exit;

    //get the version prefix (we support "2a" and the older "2", since they are the same thing)
    if AnsiSameText(Copy(hashString, 1, 4), '$2a$') then
    begin
            version := Copy(hashString, 2, 2);
            work := Copy(hashString, 5, 25);
    end
    else if AnsiSameText(Copy(hashString, 1, 3), '$2$') then
    begin
            version := Copy(hashString, 2, 1);
            work := Copy(hashString, 4, 25);
    end
    else
            Exit;

    //Next two characters must be a length
    s := Copy(work, 1, 2);
    cost := StrToIntDef(s, -1);
    if cost < 0 then
            Exit;

    //Next is a separator
    if work[3] <> '$' then
            Exit;

    //Next 22 are the salt
    s := Copy(work, 4, 22);
    Salt := BsdBase64Decode(s); //salt is always 16 bytes

{       //next 32 is hash
    s := Copy(work, 26, 32);
    SetLength(Hash, 24); //hash is always 24 bytes}

    Result := True;
end;

class function TBCrypt.CheckPassword(const password: UnicodeString; const expectedHashString: AnsiString): Boolean;
var
    version: string;
    cost: Integer;
    salt: TByteDynArray;
    hash: TByteDynArray;
    actualHashString: string;
begin
    if not TryParseHashString(expectedHashString,
                    {out}version, {out}cost, {out}salt) then
            raise Exception.Create('Invalid hash string');

    hash := TBCrypt.HashPassword(password, salt, cost);

    actualHashString := FormatPasswordHashForBsd(cost, salt, hash);

    Result := (actualHashString = expectedHashString);
end;

class function TBCrypt.BsdBase64Encode(const data: array of Byte; BytesToEncode: Integer): AnsiString;

    function EncodePacket(b1, b2, b3: Byte; Len: Integer): AnsiString;
    begin
            Result := '';

            Result := Result + BsdBase64EncodeTable[b1 shr 2];
            Result := Result + BsdBase64EncodeTable[((b1 and $03) shl 4) or (b2 shr 4)];
            if Len < 2 then Exit;

            Result := Result + BsdBase64EncodeTable[((b2 and $0f) shl 2) or (b3 shr 6)];
            if Len < 3 then Exit;

            Result := Result + BsdBase64EncodeTable[b3 and $3f];
    end;

var
    i: Integer;
    len: Integer;
    b1, b2: Integer;
begin
    Result := '';

    len := BytesToEncode;
    if len = 0 then
            Exit;

    if len > Length(data) then
            raise Exception.Create('Invalid length');

    //encode whole 3-byte chunks  TV4S 6ytw fsfv kgY8 jIuc Drjc 8deX 1s.
    i := Low(data);
    while len >= 3 do
    begin
            Result := Result+EncodePacket(data[i], data[i+1], data[i+2], 3);
            Inc(i, 3);
            Dec(len, 3);
    end;

    if len = 0 then
            Exit;

    //encode partial final chunk
    Assert(len < 3);
    if len >= 1 then
            b1 := data[i]
    else
            b1 := 0;
    if len >= 2 then
            b2 := data[i+1]
    else
            b2 := 0;
    Result := Result+EncodePacket(b1, b2, 0, len);
end;

class function TBCrypt.SelfTest: Boolean;
begin
    Result :=
                    SelfTestA and  //known test vectors
                    SelfTestB and  //the base64 encoder/decoder
                    SelfTestC and  //unicode strings
                    SelfTestD;     //different length passwords
end;

class function TBCrypt.FormatPasswordHashForBsd(const cost: Integer; const salt, hash: array of Byte): AnsiString;
var
    saltString: AnsiString;
    hashString: AnsiString;
begin
    saltString := BsdBase64Encode(salt, Length(salt));
    hashString := BsdBase64Encode(hash, Length(hash)-1); //Yes, everything except the last byte
            //OpenBSD, in the pseudo-base64 implementation, doesn't include the last byte of the hash
            //Nobody knows why, but that's what all exists tests do - so it's what i do

    Result := Format('$2a$%.2d$%s%s', [cost, saltString, hashString]);
end;

class function TBCrypt.BsdBase64Decode(const s: AnsiString): TByteDynArray;

    function Char64(character: AnsiChar): Integer;
    begin
            if (Ord(character) > Length(BsdBase64DecodeTable)) then
            begin
                    Result := -1;
                    Exit;
            end;

            Result := BsdBase64DecodeTable[character];
    end;

    procedure Append(value: Byte);
    var
            i: Integer;
    begin
            i := Length(Result);
            SetLength(Result, i+1);
            Result[i] := value;
    end;

var
    i: Integer;
    len: Integer;
    c1, c2, c3, c4: Integer;
begin
    SetLength(Result, 0);

    len := Length(s);
    i := 1;
    while i <= len do
    begin
            // We'll need to have at least 2 character to form one byte.
            // Anything less is invalid
            if (i+1) > len then
            begin
                    raise Exception.Create('Invalid base64 hash string');
//                      Break;
            end;

            c1 := Char64(s[i]);
            Inc(i);
            c2 := Char64(s[i]);
            Inc(i);

            if (c1 = -1) or (c2 = -1) then
            begin
                    raise Exception.Create('Invalid base64 hash string');
//                      Break;
            end;

            //Now we have at least one byte in c1|c2
            // c1 = ..111111
            // c2 = ..112222
            Append( ((c1 and $3f) shl 2) or (c2 shr 4) );

            //If there's a 3rd character, then we can use c2|c3 to form the second byte
            if (i > len) then
                    Break;
            c3 := Char64(s[i]);
            Inc(i);

            if (c3 = -1) then
            begin
                    raise Exception.Create('Invalid base64 hash string');
//                      Break;
            end;

            //Now we have the next byte in c2|c3
            // c2 = ..112222
            // c3 = ..222233
            Append( ((c2 and $0f) shl 4) or (c3 shr 2) );

            //If there's a 4th caracter, then we can use c3|c4 to form the third byte
            if i > len then
                    Break;
            c4 := Char64(s[i]);
            Inc(i);

            if (c4 = -1) then
            begin
                    raise Exception.Create('Invalid base64 hash string');
//                      Break;
            end;

            //Now we have the next byte in c3|c4
            // c3 = ..222233
            // c4 = ..333333
            Append( ((c3 and $03) shl 6) or c4 );
    end;
end;

class function TBCrypt.WideStringToUtf8(const Source: UnicodeString): AnsiString;
var
    cpStr: AnsiString;
    strLen: Integer;
    dw: DWORD;
const
    CodePage = CP_UTF8;
begin
    if Length(Source) = 0 then
    begin
            Result := '';
            Exit;
    end;

    // Determine real size of destination string, in bytes
    strLen := WideCharToMultiByte(CodePage, 0,
                    PWideChar(Source), Length(Source), //Source
                    nil, 0, //Destination
                    nil, nil);
    if strLen = 0 then
    begin
            dw := GetLastError;
            raise EConvertError.Create('[WideStringToUtf8] Could not get length of destination string. Error '+IntToStr(dw)+' ('+SysErrorMessage(dw)+')');
    end;

    // Allocate memory for destination string
    SetLength(cpStr, strLen);

    // Convert source UTF-16 string (WideString) to the destination using the code-page
    strLen := WideCharToMultiByte(CodePage, 0,
                    PWideChar(Source), Length(Source), //Source
                    PChar(cpStr), strLen, //Destination
                    nil, nil);
    if strLen = 0 then
    begin
            dw := GetLastError;
            raise EConvertError.Create('[WideStringToUtf8] Could not convert utf16 to utf8 string. Error '+IntToStr(dw)+' ('+SysErrorMessage(dw)+')');
    end;

    Result := cpStr
end;


class function TBCrypt.SelfTestB: Boolean;
var
    i: Integer;
    salt: AnsiString;
    encoded: AnsiString;
    data: TByteDynArray;
    recoded: AnsiString;
begin
    for i := Low(TestVectors) to High(TestVectors) do
    begin
            salt := TestVectors[i,2];

            encoded := Copy(salt, 8, 22); //salt is always 22 characters

            data := TBCrypt.BsdBase64Decode(encoded);

            recoded := TBCrypt.BsdBase64Encode(data, Length(data));
            if (recoded <> encoded) then
                    raise Exception.Create('BSDBase64 encoder self-test failed');
    end;

    Result := True;
end;

class function TBCrypt.SelfTestA: Boolean;
var
    i: Integer;

    procedure t(const password: AnsiString; const HashSalt: AnsiString; const ExpectedHashString: AnsiString);
    var
            version: string;
            cost: Integer;
            salt: TByteDynArray;
            hash: TByteDynArray;
            actualHashString: AnsiString;
    begin
            //Extract "$2a$06$If6bvum7DFjUnE9p2uDeDu" rounds and base64 salt from the HashSalt
            if not TBCrypt.TryParseHashString(HashSalt, {out}version, {out}cost, {out}salt) then
                    raise Exception.Create('bcrypt self-test failed: invalid versionsalt "'+HashSalt+'"');

            hash := TBCrypt.HashPassword(password, salt, cost);
            actualHashString := TBCrypt.FormatPasswordHashForBsd(cost, salt, hash);

            if actualHashString <> ExpectedHashString then
                    raise Exception.CreateFmt('bcrypt self-test failed. actual hash "%s" did not match expected hash "%s"', [actualHashString, ExpectedHashString]);
    end;

begin
    for i := Low(TestVectors) to High(TestVectors) do
    begin
            t(TestVectors[i,1], TestVectors[i,2], TestVectors[i,3] );
    end;

    Result := True;
end;

class function TBCrypt.SelfTestC: Boolean;
var
    s: UnicodeString;
    hash: AnsiString;
const
    n: UnicodeString=''; //n=nothing.
                    //Work around bug in Delphi compiler when building widestrings
                    //http://stackoverflow.com/a/7031942/12597
begin
    {
            We test that the it doesn't choke on international characters
            This was a bug in a version of bcrypt somewhere, that we do not intend to duplicate
    }
    s := n+#$03C0+#$03C0+#$03C0+#$03C0+#$03C0+#$03C0+#$03C0+#$03C0; //U+03C0: Greek Small Letter Pi
    hash := TBCrypt.HashPassword(s);
    if not TBCrypt.CheckPassword(s, hash) then
            raise Exception.Create('Failed to validate unicode string "'+s+'"');


    s := n+#$03C0+#$03C0+#$03C0+#$03C0+#$03C0+#$03C0+#$03C0+#$03C0; //U+03C0: Greek Small Letter Pi
    hash := TBCrypt.HashPassword(s);
    if not TBCrypt.CheckPassword(s, hash) then
            raise Exception.Create('Failed to validate unicode string "'+s+'"');

    Result := True;
end;

{ TBCryptTests }

{$IFDEF UnitTests}
procedure TBCryptTests.SelfTest;
begin
    CheckTrue(TBCrypt.SelfTest);
end;
{$ENDIF}

class function TBCrypt.SelfTestD: Boolean;
var
    i: Integer;
    password: string;
    hash: string;
begin
    for i := 0 to 56 do
    begin
            password := Copy('The quick brown fox jumped over the lazy dog then sat on a log', 1, i);
            hash := TBCrypt.HashPassword(password);
            if (hash = '') then
                    raise Exception.Create('hash creation failed');
    end;

    Result := True;
end;

{$IFDEF UnitTests}
procedure TBCryptTests.SelfTestA_KnownTestVectors;
begin
    CheckTrue(TBCrypt.SelfTestA);
end;

procedure TBCryptTests.SelfTestB_Base64EncoderDecoder;
begin
    CheckTrue(TBCrypt.SelfTestB);
end;

procedure TBCryptTests.SelfTestC_UnicodeStrings;
begin
    CheckTrue(TBCrypt.SelfTestC);
end;

procedure TBCryptTests.SelfTestD_VariableLengthPasswords;
begin
    CheckTrue(TBCrypt.SelfTestD);
end;
{$ENDIF}

class function TBCrypt.GenRandomBytes(len: Integer; const data: Pointer): HRESULT;
var
    hProv: THandle;
const
    PROV_RSA_FULL = 1;
    CRYPT_VERIFYCONTEXT = DWORD($F0000000);
    CRYPT_SILENT         = $00000040;
begin
    if not CryptAcquireContextW(hPRov, nil, nil, PROV_RSA_FULL, CRYPT_VERIFYCONTEXT or CRYPT_SILENT) then
    begin
            Result := HResultFromWin32(GetLastError);
            Exit;
    end;
    try
            if not CryptGenRandom(hProv, len, data) then
            begin
                    Result := HResultFromWin32(GetLastError);
                    Exit;
            end;
    finally
            CryptReleaseContext(hProv, 0);
    end;

    Result := S_OK;
end;

class function TBCrypt.SelfTestE: Boolean;
var
    salt: TByteDynArray;
begin
    salt := TBCrypt.GenerateSalt;
    if Length(salt) <> BCRYPT_SALT_LEN then
            raise Exception.Create('BCrypt selftest failed; invalid salt length');

    Result := True;
end;

initialization
{$IFDEF UnitTests}
    RegisterTest('Library', TBCryptTests.Suite);
{$ENDIF}

end.

