{
Codepage convertion functions, based on Lazarus' LConvEncoding code,
but with simpler API.
(c) 2019 Alexey Torgashin
License: the same as Lazarus has
}
unit EncConv;

{$mode objfpc}{$H+}

interface

{.$Define encconv_noasian}

uses
  {$ifdef windows}
  Windows,
  {$endif}
  SysUtils, Classes, LazUTF8;

type
  TEncConvId = (
    eidUTF8,
    eidUTF8BOM,
    eidUCS2LE,
    eidUCS2BE,

    eidCP1250,
    eidCP1251,
    eidCP1252,
    eidCP1253,
    eidCP1254,
    eidCP1255,
    eidCP1256,
    eidCP1257,
    eidCP1258,

    eidCP437,
    eidCP850,
    eidCP852,
    eidCP866,
    eidCP874,

    {$IFnDEF encconv_noasian}
    eidCP932,
    eidCP936,
    eidCP949,
    eidCP950,
    {$ENDIF}

    eidISO1,
    eidISO2,
    eidISO15,

    eidCPMac,
    eidKOI8R,
    eidKOI8U,
    eidKOI8RU
    );

const
  cEncConvNames: array[TEncConvId] of string = (
    'utf8',
    'utf8_bom',
    'utf16le',
    'utf16be',

    'cp1250',
    'cp1251',
    'cp1252',
    'cp1253',
    'cp1254',
    'cp1255',
    'cp1256',
    'cp1257',
    'cp1258',

    'cp437',
    'cp850',
    'cp852',
    'cp866',
    'cp874',

    {$IFnDEF encconv_noasian}
    'cp932',
    'cp936',
    'cp949',
    'cp950',
    {$ENDIF}

    'iso88591',
    'iso88592',
    'iso885915',

    'mac',
    'koi8r',
    'koi8u',
    'koi8ru'
    );

function EncConvFindEncoding(const s: string): TEncConvId;

function EncConvertFromUTF8(const S: string; Enc: TEncConvId): string;
function EncConvertToUTF8(const S: string; Enc: TEncConvId): string;

type
  TEncConvTable = array[char] of PChar;
  TEncConvUnicodeToCharID = function(Unicode: cardinal): integer;
  TEncConvStringFunction = function(const S: string): string;

type
  TEncConvErrorMode = (
    eemSkip,
    eemException,
    eemReplace,
    eemReturnEmpty
    );

var
  EncConvErrorMode: TEncConvErrorMode = eemReplace;

function EncConvGetANSI: TEncConvId;
function EncConvGetOEM: TEncConvId;


implementation

{$IFnDEF encconv_noasian}
{$include encconv_asiancodepages.inc}
{$include encconv_asiancodepagefunctions.inc}
{$ENDIF}

{$include encconv_commoncodepages.inc}
{$include encconv_commoncodepagefunctions.inc}

function StrNone(const S: string): string;
begin
  Result:= S;
end;

function SingleByteToUTF8(const s: string; const Table: TEncConvTable): string;
forward;
function UTF8ToSingleByte(const s: string; const UTF8CharConvFunc: TEncConvUnicodeToCharID): string;
forward;

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

function KOI8RToUTF8(const s: string): string;
begin
  Result:=SingleByteToUTF8(s,ArrayKOI8RToUTF8);
end;

function KOI8UToUTF8(const s: string): string;
begin
  Result:=SingleByteToUTF8(s,ArrayKOI8UToUTF8);
end;

function KOI8RUToUTF8(const s: string): string;
begin
  Result:=SingleByteToUTF8(s,ArrayKOI8RUToUTF8);
end;

function MacintoshToUTF8(const s: string): string;
begin
  Result:=SingleByteToUTF8(s,ArrayMacintoshToUTF8);
end;

function SingleByteToUTF8(const s: string; const Table: TEncConvTable): string;
var
  len: Integer;
  i: Integer;
  Src: PChar;
  Dest: PChar;
  p: PChar;
  c: Char;
begin
  if s='' then exit('');
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
const
  UTF8BOM = #$EF#$BB#$BF;
begin
  Result:=UTF8BOM+s;
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

function UTF8ToKOI8R(const s: string): string;
begin
  Result:=UTF8ToSingleByte(s,@UnicodeToKOI8R);
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

function UTF8ToSingleByte(const s: string; const UTF8CharConvFunc: TEncConvUnicodeToCharID): string;
var
  len, i, CharLen: Integer;
  Src, Dest: PChar;
  c: Char;
  Unicode: LongWord;
begin
  if s='' then exit('');
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
      case EncConvErrorMode of
        eemException:
          raise EConvertError.Create('Cannot convert UTF8 to single byte');
        eemReplace:
          begin
            Dest^ := '?';
            Inc(Dest);
          end;
        eemReturnEmpty:
          exit('');
      end;
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

function EncConvFindEncoding(const s: string): TEncConvId;
var
  e: TEncConvId;
begin
  for e:= Low(cEncConvNames) to High(cEncConvNames) do
    if s=cEncConvNames[e] then
      exit(e);
  Result:= eidUTF8;
end;

const
  FunctionsToUTF8: array[TEncConvId] of TEncConvStringFunction = (
    @StrNone,
    @UTF8BOMToUTF8,
    @UCS2LEToUTF8,
    @UCS2BEToUTF8,
    @CP1250ToUTF8,
    @CP1251ToUTF8,
    @CP1252ToUTF8,
    @CP1253ToUTF8,
    @CP1254ToUTF8,
    @CP1255ToUTF8,
    @CP1256ToUTF8,
    @CP1257ToUTF8,
    @CP1258ToUTF8,
    @CP437ToUTF8,
    @CP850ToUTF8,
    @CP852ToUTF8,
    @CP866ToUTF8,
    @CP874ToUTF8,
    {$IFnDEF encconv_noasian}
    @CP932ToUTF8,
    @CP936ToUTF8,
    @CP949ToUTF8,
    @CP950ToUTF8,
    {$ENDIF}
    @ISO_8859_1ToUTF8,
    @ISO_8859_2ToUTF8,
    @ISO_8859_15ToUTF8,
    @MacintoshToUTF8,
    @KOI8RToUTF8,
    @KOI8UToUTF8,
    @KOI8RUToUTF8
  );

  FunctionsFromUTF8: array[TEncConvId] of TEncConvStringFunction = (
    @StrNone,
    @UTF8ToUTF8BOM,
    @UTF8ToUCS2LE,
    @UTF8ToUCS2BE,
    @UTF8ToCP1250,
    @UTF8ToCP1251,
    @UTF8ToCP1252,
    @UTF8ToCP1253,
    @UTF8ToCP1254,
    @UTF8ToCP1255,
    @UTF8ToCP1256,
    @UTF8ToCP1257,
    @UTF8ToCP1258,
    @UTF8ToCP437,
    @UTF8ToCP850,
    @UTF8ToCP852,
    @UTF8ToCP866,
    @UTF8ToCP874,
    {$IFnDEF encconv_noasian}
    @UTF8ToCP932,
    @UTF8ToCP936,
    @UTF8ToCP949,
    @UTF8ToCP950,
    {$ENDIF}
    @UTF8ToISO_8859_1,
    @UTF8ToISO_8859_2,
    @UTF8ToISO_8859_15,
    @UTF8ToMacintosh,
    @UTF8ToKOI8R,
    @UTF8ToKOI8U,
    @UTF8ToKOI8RU
  );


function EncConvertFromUTF8(const S: string; Enc: TEncConvId): string;
begin
  Result:= FunctionsFromUTF8[Enc](S);
end;

function EncConvertToUTF8(const S: string; Enc: TEncConvId): string;
begin
  Result:= FunctionsToUTF8[Enc](S);
end;


{$ifdef windows}
var
  _SavedANSI: TEncConvId = eidUTF8;
  _SavedOEM: TEncConvId = eidUTF8;
{$endif}

function EncConvGetANSI: TEncConvId;
begin
  {$ifdef windows}
  if _SavedANSI<>eidUTF8 then
    exit(_SavedANSI);
  case Windows.GetACP of
    1250: Result:= eidCP1250;
    1251: Result:= eidCP1251;
    1252: Result:= eidCP1252;
    1253: Result:= eidCP1253;
    1254: Result:= eidCP1254;
    1255: Result:= eidCP1255;
    1256: Result:= eidCP1256;
    1257: Result:= eidCP1257;
    1258: Result:= eidCP1258;
    874: Result:= eidCP874;
    932: Result:= eidCP932;
    936: Result:= eidCP936;
    949: Result:= eidCP949;
    950: Result:= eidCP950;
    else Result:= eidCP1252;
  end;
  _SavedANSI:= Result;
  {$else}
  Result:= eidCP1252;
  {$endif}
end;

function EncConvGetOEM: TEncConvId;
begin
  {$ifdef windows}
  if _SavedOEM<>eidUTF8 then
    exit(_SavedOEM);
  case Windows.GetOEMCP of
    437: Result:= eidCP437;
    850: Result:= eidCP850;
    852: Result:= eidCP852;
    866: Result:= eidCP866;
    874: Result:= eidCP874;
    932: Result:= eidCP932;
    936: Result:= eidCP936;
    949: Result:= eidCP949;
    950: Result:= eidCP950;
    else Result:= eidCP437;
  end;
  _SavedOEM:= Result;
  {$else}
  Result:= eidCP437;
  {$endif}
end;


end.
