unit EncConv;

{$mode objfpc}{$H+}

interface

{.$Define DisableAsianCodePages}

uses
  SysUtils, Classes, LazUTF8;

var
  ConvertEncodingFromUtf8RaisesException: boolean = False;

//encoding names
const
  EncodingUTF8 = 'utf8';
  EncodingUTF8BOM = 'utf8bom'; // UTF-8 with byte order mark
  EncodingUCS2LE = 'ucs2le'; // UCS 2 byte little endian
  EncodingUCS2BE = 'ucs2be'; // UCS 2 byte big endian

  EncodingCP1250 = 'cp1250';
  EncodingCP1251 = 'cp1251';
  EncodingCP1252 = 'cp1252';
  EncodingCP1253 = 'cp1253';
  EncodingCP1254 = 'cp1254';
  EncodingCP1255 = 'cp1255';
  EncodingCP1256 = 'cp1256';
  EncodingCP1257 = 'cp1257';
  EncodingCP1258 = 'cp1258';

  EncodingCP437 = 'cp437';
  EncodingCP850 = 'cp850';
  EncodingCP852 = 'cp852';
  EncodingCP866 = 'cp866';
  EncodingCP874 = 'cp874';

  EncodingCP932 = 'cp932';
  EncodingCP936 = 'cp936';
  EncodingCP949 = 'cp949';
  EncodingCP950 = 'cp950';

  EncodingCPMac = 'macintosh';
  EncodingCPKOI8 = 'koi8';

  EncodingCPIso1 = 'iso88591';
  EncodingCPIso2 = 'iso88592';
  EncodingCPIso15 = 'iso885915';

//signatures in ansi
const
  UTF8BOM = #$EF#$BB#$BF;
  UTF16BEBOM = #$FE#$FF;
  UTF16LEBOM = #$FF#$FE;
  UTF32BEBOM = #0#0#$FE#$FF;
  UTF32LEBOM = #$FE#$FF#0#0;

function ConvertEncodingFromUTF8(const s, ToEncoding: string; out Encoded: boolean): string;
function ConvertEncodingToUTF8(const s, FromEncoding: string; out Encoded: boolean): string;

type
  TConvertEncodingFunction = function(const s: string): string;
  TConvertUTF8ToEncodingFunc = function(const s: string): string;
  TCharToUTF8Table = array[char] of PChar;
  TUnicodeToCharID = function(Unicode: cardinal): integer;
var
  ConvertAnsiToUTF8: TConvertEncodingFunction = nil;
  ConvertUTF8ToAnsi: TConvertUTF8ToEncodingFunc = nil;

function UTF8BOMToUTF8(const s: string): string; // UTF8 with BOM
function ISO_8859_1ToUTF8(const s: string): string; // central europe
function ISO_8859_15ToUTF8(const s: string): string; // Western European languages
function ISO_8859_2ToUTF8(const s: string): string; // eastern europe
function CP1250ToUTF8(const s: string): string; // central europe
function CP1251ToUTF8(const s: string): string; // cyrillic
function CP1252ToUTF8(const s: string): string; // latin 1
function CP1253ToUTF8(const s: string): string; // greek
function CP1254ToUTF8(const s: string): string; // turkish
function CP1255ToUTF8(const s: string): string; // hebrew
function CP1256ToUTF8(const s: string): string; // arabic
function CP1257ToUTF8(const s: string): string; // baltic
function CP1258ToUTF8(const s: string): string; // vietnam
function CP437ToUTF8(const s: string): string;  // DOS central europe
function CP850ToUTF8(const s: string): string;  // DOS western europe
function CP852ToUTF8(const s: string): string;  // DOS central europe
function CP866ToUTF8(const s: string): string;  // DOS and Windows console's cyrillic
function CP874ToUTF8(const s: string): string;  // thai
function KOI8ToUTF8(const s: string): string;  // russian cyrillic
function MacintoshToUTF8(const s: string): string;  // Macintosh, alias Mac OS Roman
function SingleByteToUTF8(const s: string; const Table: TCharToUTF8Table): string;
function UCS2LEToUTF8(const s: string): string; // UCS2-LE 2byte little endian
function UCS2BEToUTF8(const s: string): string; // UCS2-BE 2byte big endian

function UTF8ToUTF8BOM(const s: string): string; // UTF8 with BOM
function UTF8ToISO_8859_1(const s: string): RawByteString; // central europe
function UTF8ToISO_8859_2(const s: string): RawByteString; // eastern europe
function UTF8ToISO_8859_15(const s: string): RawByteString; // Western European languages
function UTF8ToCP1250(const s: string): RawByteString; // central europe
function UTF8ToCP1251(const s: string): RawByteString; // cyrillic
function UTF8ToCP1252(const s: string): RawByteString; // latin 1
function UTF8ToCP1253(const s: string): RawByteString; // greek
function UTF8ToCP1254(const s: string): RawByteString; // turkish
function UTF8ToCP1255(const s: string): RawByteString; // hebrew
function UTF8ToCP1256(const s: string): RawByteString; // arabic
function UTF8ToCP1257(const s: string): RawByteString; // baltic
function UTF8ToCP1258(const s: string): RawByteString; // vietnam
function UTF8ToCP437(const s: string): RawByteString;  // DOS central europe
function UTF8ToCP850(const s: string): RawByteString;  // DOS western europe
function UTF8ToCP852(const s: string): RawByteString;  // DOS central europe
function UTF8ToCP866(const s: string): RawByteString;  // DOS and Windows console's cyrillic
function UTF8ToCP874(const s: string): RawByteString;  // thai
function UTF8ToKOI8(const s: string): RawByteString;  // russian cyrillic
function UTF8ToKOI8U(const s: string): RawByteString;  // ukrainian cyrillic
function UTF8ToKOI8RU(const s: string): RawByteString;  // belarussian cyrillic
function UTF8ToMacintosh(const s: string): RawByteString;  // Macintosh, alias Mac OS Roman
// custom conversion
function UTF8ToSingleByte(const s: string; const UTF8CharConvFunc: TUnicodeToCharID): string;

function UTF8ToUCS2LE(const s: string): string; // UCS2-LE 2byte little endian without BOM
function UTF8ToUCS2BE(const s: string): string; // UCS2-BE 2byte big endian without BOM

{$IFnDEF DisableAsianCodePages}
// Asian encodings
function CP932ToUTF8(const s: string): string;      // Japanese
function CP936ToUTF8(const s: string): string;      // Chinese
function CP949ToUTF8(const s: string): string;      // Korea
function CP950ToUTF8(const s: string): string;      // Chinese Complex

function DBCSToUTF8(const s: string; CodeP: integer): string;

function UTF8ToCP932(const s: string): RawByteString; // Japanese
function UTF8ToCP936(const s: string): RawByteString; // Chinese, essentially the same as GB 2312 and a predecessor to GB 18030
function UTF8ToCP949(const s: string): RawByteString; // Korea
function UTF8ToCP950(const s: string): RawByteString; // Chinese Complex

// Common function used by all UTF8ToXXX functions.
function UTF8ToDBCS(const s: string; const UTF8CharConvFunc: TUnicodeToCharID): string;
{$ENDIF}

implementation

{$IFnDEF DisableAsianCodePages}
{$include encconv_asiancodepages.inc}
{$include encconv_asiancodepagefunctions.inc}
{$ENDIF}

{$include encconv_commoncodepages.inc}
{$include encconv_commoncodepagefunctions.inc}

function UTF8BOMToUTF8(const s: string): string;
begin
  Result:=copy(s,4,length(s));
end;

function ISO_8859_1ToUTF8(const s: string): string;
begin
  Result:=SingleByteToUTF8(s,ArrayISO_8859_1ToUTF8);
end;

function ISO_8859_15ToUTF8(const s: string): string;
begin
  Result:=SingleByteToUTF8(s,ArrayISO_8859_15ToUTF8);
end;

function ISO_8859_2ToUTF8(const s: string): string;
begin
  Result:=SingleByteToUTF8(s,ArrayISO_8859_2ToUTF8);
end;

function CP1250ToUTF8(const s: string): string;
begin
  Result:=SingleByteToUTF8(s,ArrayCP1250ToUTF8);
end;

function CP1251ToUTF8(const s: string): string;
begin
  Result:=SingleByteToUTF8(s,ArrayCP1251ToUTF8);
end;

function CP1252ToUTF8(const s: string): string;
begin
  Result:=SingleByteToUTF8(s,ArrayCP1252ToUTF8);
end;

function CP1253ToUTF8(const s: string): string;
begin
  Result:=SingleByteToUTF8(s,ArrayCP1253ToUTF8);
end;

function CP1254ToUTF8(const s: string): string;
begin
  Result:=SingleByteToUTF8(s,ArrayCP1254ToUTF8);
end;

function CP1255ToUTF8(const s: string): string;
begin
  Result:=SingleByteToUTF8(s,ArrayCP1255ToUTF8);
end;

function CP1256ToUTF8(const s: string): string;
begin
  Result:=SingleByteToUTF8(s,ArrayCP1256ToUTF8);
end;

function CP1257ToUTF8(const s: string): string;
begin
  Result:=SingleByteToUTF8(s,ArrayCP1257ToUTF8);
end;

function CP1258ToUTF8(const s: string): string;
begin
  Result:=SingleByteToUTF8(s,ArrayCP1258ToUTF8);
end;

function CP437ToUTF8(const s: string): string;
begin
  Result:=SingleByteToUTF8(s,ArrayCP437ToUTF8);
end;

function CP850ToUTF8(const s: string): string;
begin
  Result:=SingleByteToUTF8(s,ArrayCP850ToUTF8);
end;

function CP852ToUTF8(const s: string): string;
begin
  Result:=SingleByteToUTF8(s,ArrayCP852ToUTF8);
end;

function CP866ToUTF8(const s: string): string;
begin
  Result:=SingleByteToUTF8(s,ArrayCP866ToUTF8);
end;

function CP874ToUTF8(const s: string): string;
begin
  Result:=SingleByteToUTF8(s,ArrayCP874ToUTF8);
end;

function KOI8ToUTF8(const s: string): string;
begin
  Result:=SingleByteToUTF8(s,ArrayKOI8ToUTF8);
end;

function MacintoshToUTF8(const s: string): string;
begin
  Result:=SingleByteToUTF8(s,ArrayMacintoshToUTF8);
end;

function SingleByteToUTF8(const s: string; const Table: TCharToUTF8Table): string;
var
  len: Integer;
  i: Integer;
  Src: PChar;
  Dest: PChar;
  p: PChar;
  c: Char;
begin
  if s='' then begin
    Result:='';
    exit;
  end;
  len:=length(s);
  SetLength(Result,len*4);// UTF-8 is at most 4 bytes
  Src:=PChar(s);
  Dest:=PChar(Result);
  for i:=1 to len do begin
    c:=Src^;
    inc(Src);
    if ord(c)<128 then begin
      Dest^:=c;
      inc(Dest);
    end else begin
      p:=Table[c];
      if p<>nil then begin
        while p^<>#0 do begin
          Dest^:=p^;
          inc(p);
          inc(Dest);
        end;
      end;
    end;
  end;
  SetLength(Result,{%H-}PtrUInt(Dest)-PtrUInt(Result));
end;

function UCS2LEToUTF8(const s: string): string;
var
  len: Integer;
  Src: PWord;
  Dest: PChar;
  i: Integer;
  c: Word;
begin
  len:=length(s) div 2;
  if len=0 then
    exit('');
  SetLength(Result,len*3);// UTF-8 is at most 3/2 times the size
  Src:=PWord(Pointer(s));
  Dest:=PChar(Result);
  for i:=1 to len do begin
    c:=LEtoN(Src^);
    inc(Src);
    if ord(c)<128 then begin
      Dest^:=chr(c);
      inc(Dest);
    end else begin
      inc(Dest,UnicodeToUTF8SkipErrors(c,Dest));
    end;
  end;
  len:={%H-}PtrUInt(Dest)-PtrUInt(Result);
  if len>length(Result) then
    raise Exception.Create('');
  SetLength(Result,len);
end;

function UCS2BEToUTF8(const s: string): string;
var
  len: Integer;
  Src: PWord;
  Dest: PChar;
  i: Integer;
  c: Word;
begin
  len:=length(s) div 2;
  if len=0 then
    exit('');
  SetLength(Result,len*3);// UTF-8 is at most three times the size
  Src:=PWord(Pointer(s));
  Dest:=PChar(Result);
  for i:=1 to len do begin
    c:=BEtoN(Src^);
    inc(Src);
    if ord(c)<128 then begin
      Dest^:=chr(c);
      inc(Dest);
    end else begin
      inc(Dest,UnicodeToUTF8SkipErrors(c,Dest));
    end;
  end;
  len:={%H-}PtrUInt(Dest)-PtrUInt(Result);
  if len>length(Result) then
    raise Exception.Create('');
  SetLength(Result,len);
end;

function UTF8ToUTF8BOM(const s: string): string;
begin
  Result:=UTF8BOM+s;
end;

procedure InternalUTF8ToCP(const s: string;
  const UTF8CharConvFunc: TUnicodeToCharID;
  out TheResult: RawByteString); inline;
begin
  TheResult:=UTF8ToSingleByte(s,UTF8CharConvFunc);
end;

function UTF8ToISO_8859_1(const s: string): RawByteString;
begin
  InternalUTF8ToCP(s,@UnicodeToISO_8859_1,Result);
end;

function UTF8ToISO_8859_2(const s: string): RawByteString;
begin
  InternalUTF8ToCP(s,@UnicodeToISO_8859_2,Result);
end;

function UTF8ToISO_8859_15(const s: string): RawByteString;
begin
  InternalUTF8ToCP(s,@UnicodeToISO_8859_15,Result);
end;

function UTF8ToCP1250(const s: string): RawByteString;
begin
  // system conversion fails for character #129 -> using table
  InternalUTF8ToCP(s,@UnicodeToCP1250,Result);
end;

function UTF8ToCP1251(const s: string): RawByteString;
begin
  // system conversion fails for character #152 -> using table
  InternalUTF8ToCP(s,@UnicodeToCP1251,Result);
end;

function UTF8ToCP1252(const s: string): RawByteString;
begin
  // system conversion fails for character #128 -> using table
  InternalUTF8ToCP(s,@UnicodeToCP1252,Result);
end;

function UTF8ToCP1253(const s: string): RawByteString;
begin
  // system conversion fails for character #129 -> using table
  InternalUTF8ToCP(s,@UnicodeToCP1253,Result);
end;

function UTF8ToCP1254(const s: string): RawByteString;
begin
  // system conversion fails for character #129 -> using table
  InternalUTF8ToCP(s,@UnicodeToCP1254,Result);
end;

function UTF8ToCP1255(const s: string): RawByteString;
begin
  // system conversion fails for character #129 -> using table
  InternalUTF8ToCP(s,@UnicodeToCP1255,Result);
end;

function UTF8ToCP1256(const s: string): RawByteString;
begin
  InternalUTF8ToCP(s,@UnicodeToCP1256,Result);
end;

function UTF8ToCP1257(const s: string): RawByteString;
begin
  // system conversion fails for character #129 -> using table
  InternalUTF8ToCP(s,@UnicodeToCP1257,Result);
end;

function UTF8ToCP1258(const s: string): RawByteString;
begin
  // system conversion fails for character #129 -> using table
  InternalUTF8ToCP(s,@UnicodeToCP1258,Result);
end;

function UTF8ToCP437(const s: string): RawByteString;
begin
  InternalUTF8ToCP(s,@UnicodeToCP437,Result);
end;

function UTF8ToCP850(const s: string): RawByteString;
begin
  InternalUTF8ToCP(s,@UnicodeToCP850,Result);
end;

function UTF8ToCP852(const s: string): RawByteString;
begin
  InternalUTF8ToCP(s,@UnicodeToCP852,Result);
end;

function UTF8ToCP866(const s: string): RawByteString;
begin
  InternalUTF8ToCP(s,@UnicodeToCP866,Result);
end;

function UTF8ToCP874(const s: string): RawByteString;
begin
  // system conversion fails for character #129 -> using table
  InternalUTF8ToCP(s,@UnicodeToCP874,Result);
end;

function UTF8ToKOI8(const s: string): RawByteString;
begin
  InternalUTF8ToCP(s,@UnicodeToKOI8,Result);
end;

function UTF8ToKOI8U(const s: string): RawByteString;
begin
  InternalUTF8ToCP(s,@UnicodeToKOI8U,Result);
end;

function UTF8ToKOI8RU(const s: string): RawByteString;
begin
  InternalUTF8ToCP(s,@UnicodeToKOI8RU,Result);
end;

function UTF8ToMacintosh(const s: string): RawByteString;
begin
  InternalUTF8ToCP(s,@UnicodeToMacintosh,Result);
end;

function UTF8ToISO_8859_1(const s: string): string;
begin
  Result:=UTF8ToSingleByte(s,@UnicodeToISO_8859_1);
end;

function UTF8ToISO_8859_15(const s: string): string;
begin
  Result:=UTF8ToSingleByte(s,@UnicodeToISO_8859_15);
end;

function UTF8ToISO_8859_2(const s: string): string;
begin
  Result:=UTF8ToSingleByte(s,@UnicodeToISO_8859_2);
end;

function UTF8ToCP1250(const s: string): string;
begin
  Result:=UTF8ToSingleByte(s,@UnicodeToCP1250);
end;

function UTF8ToCP1251(const s: string): string;
begin
  Result:=UTF8ToSingleByte(s,@UnicodeToCP1251);
end;

function UTF8ToCP1252(const s: string): string;
begin
  Result:=UTF8ToSingleByte(s,@UnicodeToCP1252);
end;

function UTF8ToCP1253(const s: string): string;
begin
  Result:=UTF8ToSingleByte(s,@UnicodeToCP1253);
end;

function UTF8ToCP1254(const s: string): string;
begin
  Result:=UTF8ToSingleByte(s,@UnicodeToCP1254);
end;

function UTF8ToCP1255(const s: string): string;
begin
  Result:=UTF8ToSingleByte(s,@UnicodeToCP1255);
end;

function UTF8ToCP1256(const s: string): string;
begin
  Result:=UTF8ToSingleByte(s,@UnicodeToCP1256);
end;

function UTF8ToCP1257(const s: string): string;
begin
  Result:=UTF8ToSingleByte(s,@UnicodeToCP1257);
end;

function UTF8ToCP1258(const s: string): string;
begin
  Result:=UTF8ToSingleByte(s,@UnicodeToCP1258);
end;

function UTF8ToCP437(const s: string): string;
begin
  Result:=UTF8ToSingleByte(s,@UnicodeToCP437);
end;

function UTF8ToCP850(const s: string): string;
begin
  Result:=UTF8ToSingleByte(s,@UnicodeToCP850);
end;

function UTF8ToCP852(const s: string): string;
begin
  Result:=UTF8ToSingleByte(s,@UnicodeToCP852);
end;

function UTF8ToCP866(const s: string): string;
begin
  Result:=UTF8ToSingleByte(s,@UnicodeToCP866);
end;

function UTF8ToCP874(const s: string): string;
begin
  Result:=UTF8ToSingleByte(s,@UnicodeToCP874);
end;

function UTF8ToKOI8(const s: string): string;
begin
  Result:=UTF8ToSingleByte(s,@UnicodeToKOI8);
end;

function UTF8ToKOI8U(const s: string): string;
begin
  Result:=UTF8ToSingleByte(s,@UnicodeToKOI8U);
end;

function UTF8ToKOI8RU(const s: string): string;
begin
  Result:=UTF8ToSingleByte(s,@UnicodeToKOI8RU);
end;

function UTF8ToMacintosh(const s: string): string;
begin
  Result:=UTF8ToSingleByte(s,@UnicodeToMacintosh);
end;

function UTF8ToSingleByte(const s: string; const UTF8CharConvFunc: TUnicodeToCharID): string;
var
  len, i, CharLen: Integer;
  Src, Dest: PChar;
  c: Char;
  Unicode: LongWord;
begin
  if s='' then begin
    Result:='';
    exit;
  end;
  len:=length(s);
  SetLength(Result,len);
  Src:=PChar(s);
  Dest:=PChar(Result);
  while len>0 do begin
    c:=Src^;
    if c<#128 then begin
      Dest^:=c;
      inc(Dest);
      inc(Src);
      dec(len);
    end else begin
      Unicode:=UTF8CodepointToUnicode(Src,CharLen);
      inc(Src,CharLen);
      dec(len,CharLen);
      i:=UTF8CharConvFunc(Unicode);
      //writeln('UTF8ToSingleByte Unicode=',Unicode,' CharLen=',CharLen,' c="',copy(s,Src-PChar(s)+1-CharLen,CharLen),'" i=',i);
      if i>=0 then begin
        Dest^:=chr(i);
        inc(Dest);
      end
      else
      if ConvertEncodingFromUtf8RaisesException then
        raise EConvertError.Create('Cannot convert UTF8 to single byte');
    end;
  end;
  SetLength(Result,Dest-PChar(Result));
end;

function UTF8ToUCS2LE(const s: string): string;
var
  len: Integer;
  Src: PChar;
  Dest: PWord;
  c: Char;
  Unicode: LongWord;
  CharLen: integer;
begin
  if s='' then begin
    Result:='';
    exit;
  end;
  len:=length(s);
  SetLength(Result,len*2);
  Src:=PChar(s);
  Dest:=PWord(Pointer(Result));
  while len>0 do begin
    c:=Src^;
    if c<#128 then begin
      Dest^:=NtoLE(Word(ord(c)));
      inc(Dest);
      inc(Src);
      dec(len);
    end else begin
      Unicode:=UTF8CodepointToUnicode(Src,CharLen);
      inc(Src,CharLen);
      dec(len,CharLen);
      if Unicode<=$ffff then begin
        Dest^:=NtoLE(Word(Unicode));
        inc(Dest);
      end;
    end;
  end;
  len:={%H-}PtrUInt(Dest)-PtrUInt(Result);
  if len>length(Result) then
    raise Exception.Create('');
  SetLength(Result,len);
end;

function UTF8ToUCS2BE(const s: string): string;
var
  len: Integer;
  Src: PChar;
  Dest: PWord;
  c: Char;
  Unicode: LongWord;
  CharLen: integer;
begin
  if s='' then begin
    Result:='';
    exit;
  end;
  len:=length(s);
  SetLength(Result,len*2);
  Src:=PChar(s);
  Dest:=PWord(Pointer(Result));
  while len>0 do begin
    c:=Src^;
    if c<#128 then begin
      Dest^:=NtoBE(Word(ord(c)));
      inc(Dest);
      inc(Src);
      dec(len);
    end else begin
      Unicode:=UTF8CodepointToUnicode(Src,CharLen);
      inc(Src,CharLen);
      dec(len,CharLen);
      if Unicode<=$ffff then begin
        Dest^:=NtoBE(Word(Unicode));
        inc(Dest);
      end;
    end;
  end;
  len:={%H-}PtrUInt(Dest)-PtrUInt(Result);
  if len>length(Result) then
    raise Exception.Create('');
  SetLength(Result,len);
end;


function ConvertEncodingFromUTF8(const s, ToEncoding: string; out Encoded: boolean): string;
var
  ATo: string;
begin
  Result:=s;
  Encoded:=true;
  ATo:=ToEncoding;

  if ATo=EncodingUTF8BOM then begin Result:=UTF8ToUTF8BOM(s); exit; end;
  if ATo=EncodingCPIso1 then begin Result:=UTF8ToISO_8859_1(s); exit; end;
  if ATo=EncodingCPIso15 then begin Result:=UTF8ToISO_8859_15(s); exit; end;
  if ATo=EncodingCPIso2 then begin Result:=UTF8ToISO_8859_2(s); exit; end;
  if ATo=EncodingCP1250 then begin Result:=UTF8ToCP1250(s); exit; end;
  if ATo=EncodingCP1251 then begin Result:=UTF8ToCP1251(s); exit; end;
  if ATo=EncodingCP1252 then begin Result:=UTF8ToCP1252(s); exit; end;
  if ATo=EncodingCP1253 then begin Result:=UTF8ToCP1253(s); exit; end;
  if ATo=EncodingCP1254 then begin Result:=UTF8ToCP1254(s); exit; end;
  if ATo=EncodingCP1255 then begin Result:=UTF8ToCP1255(s); exit; end;
  if ATo=EncodingCP1256 then begin Result:=UTF8ToCP1256(s); exit; end;
  if ATo=EncodingCP1257 then begin Result:=UTF8ToCP1257(s); exit; end;
  if ATo=EncodingCP1258 then begin Result:=UTF8ToCP1258(s); exit; end;
  if ATo=EncodingCP437 then begin Result:=UTF8ToCP437(s); exit; end;
  if ATo=EncodingCP850 then begin Result:=UTF8ToCP850(s); exit; end;
  if ATo=EncodingCP852 then begin Result:=UTF8ToCP852(s); exit; end;
  if ATo=EncodingCP866 then begin Result:=UTF8ToCP866(s); exit; end;
  if ATo=EncodingCP874 then begin Result:=UTF8ToCP874(s); exit; end;
  {$IFnDEF DisableAsianCodePages}
  if ATo=EncodingCP936 then begin Result:=UTF8ToCP936(s); exit; end;
  if ATo=EncodingCP950 then begin Result:=UTF8ToCP950(s); exit; end;
  if ATo=EncodingCP949 then begin Result:=UTF8ToCP949(s); exit; end;
  if ATo=EncodingCP932 then begin Result:=UTF8ToCP932(s); exit; end;
  {$ENDIF}
  if ATo=EncodingCPKOI8 then begin Result:=UTF8ToKOI8(s); exit; end;
  if ATo=EncodingCPMac then begin Result:=UTF8ToMacintosh(s); exit; end;
  if ATo=EncodingUCS2LE then begin Result:=UTF8ToUCS2LE(s); exit; end;
  if ATo=EncodingUCS2BE then begin Result:=UTF8ToUCS2BE(s); exit; end;

  Encoded:= false;
end;

function ConvertEncodingToUTF8(const s, FromEncoding: string; out Encoded: boolean): string;
var
  AFrom: string;
begin
  Result:=s;
  Encoded:=true;
  AFrom:=FromEncoding;

  if AFrom=EncodingUTF8BOM then begin Result:=UTF8BOMToUTF8(s); exit; end;
  if AFrom=EncodingCPIso1 then begin Result:=ISO_8859_1ToUTF8(s); exit; end;
  if AFrom=EncodingCPIso15 then begin Result:=ISO_8859_15ToUTF8(s); exit; end;
  if AFrom=EncodingCPIso2 then begin Result:=ISO_8859_2ToUTF8(s); exit; end;
  if AFrom=EncodingCP1250 then begin Result:=CP1250ToUTF8(s); exit; end;
  if AFrom=EncodingCP1251 then begin Result:=CP1251ToUTF8(s); exit; end;
  if AFrom=EncodingCP1252 then begin Result:=CP1252ToUTF8(s); exit; end;
  if AFrom=EncodingCP1253 then begin Result:=CP1253ToUTF8(s); exit; end;
  if AFrom=EncodingCP1254 then begin Result:=CP1254ToUTF8(s); exit; end;
  if AFrom=EncodingCP1255 then begin Result:=CP1255ToUTF8(s); exit; end;
  if AFrom=EncodingCP1256 then begin Result:=CP1256ToUTF8(s); exit; end;
  if AFrom=EncodingCP1257 then begin Result:=CP1257ToUTF8(s); exit; end;
  if AFrom=EncodingCP1258 then begin Result:=CP1258ToUTF8(s); exit; end;
  if AFrom=EncodingCP437 then begin Result:=CP437ToUTF8(s); exit; end;
  if AFrom=EncodingCP850 then begin Result:=CP850ToUTF8(s); exit; end;
  if AFrom=EncodingCP852 then begin Result:=CP852ToUTF8(s); exit; end;
  if AFrom=EncodingCP866 then begin Result:=CP866ToUTF8(s); exit; end;
  if AFrom=EncodingCP874 then begin Result:=CP874ToUTF8(s); exit; end;
  {$IFnDEF DisableAsianCodePages}
  if AFrom=EncodingCP936 then begin Result:=CP936ToUTF8(s); exit; end;
  if AFrom=EncodingCP950 then begin Result:=CP950ToUTF8(s); exit; end;
  if AFrom=EncodingCP949 then begin Result:=CP949ToUTF8(s); exit; end;
  if AFrom=EncodingCP932 then begin Result:=CP932ToUTF8(s); exit; end;
  {$ENDIF}
  if AFrom=EncodingCPKOI8 then begin Result:=KOI8ToUTF8(s); exit; end;
  if AFrom=EncodingCPMac then begin Result:=MacintoshToUTF8(s); exit; end;
  if AFrom=EncodingUCS2LE then begin Result:=UCS2LEToUTF8(s); exit; end;
  if AFrom=EncodingUCS2BE then begin Result:=UCS2BEToUTF8(s); exit; end;

  Encoded:= false;
end;

end.
