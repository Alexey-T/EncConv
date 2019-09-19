{
 *****************************************************************************
  This file is part of LazUtils.

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}

function DBCSToUTF8(const s: string; const ArrayUni, ArrayCP: array of word): string;
var
  len, l: Integer;
  Src, Dest: PChar;
  c: char;
  code: word;
begin
  if s = '' then exit('');
  len := length(s);
  SetLength(Result, len * 4);// Asia UTF-8 is at most 4 bytes
  Src  := PChar(s);
  Dest := PChar(Result);
  repeat
    c := Src^;
    Inc(Src);
    if Ord(c) < 128 then
    begin
      if (c=#0) and (Src-PChar(s)>=len) then break;
      Dest^ := c;
      Inc(Dest);
    end
    else
    begin
      code := Byte(c) shl 8;
      c:=Src^;
      if (c=#0) and (Src-PChar(s)>=len) then break;
      code := code + Byte(c);
      Inc(Src);

      code := ArrayUni[SearchTable(ArrayCP, code)];
      if code>0 then
      begin
        l:=UnicodeToUTF8Inline(code,Dest);
        inc(Dest,l);
      end;
    end;
  until false;
  SetLength(Result, {%H-}PtrUInt(Dest) - PtrUInt(Result));
end;


function CP936ToUTF8(const s: string): string;
begin
  Result := DBCSToUTF8(s, Uni936C, CP936CC);
end;

function CP950ToUTF8(const s: string): string;
begin
  Result := DBCSToUTF8(s, Uni950C, CP950CC);
end;

function CP949ToUTF8(const s: string): string;
begin
  Result := DBCSToUTF8(s, Uni949C, CP949CC);
end;

function CP932ToUTF8(const s: string): string;
begin
  Result := DBCSToUTF8(s, Uni932C, CP932CC);
end;

function UnicodeToCP936(Unicode: cardinal): integer;
begin
  case Unicode of
    0..127: Result := Unicode;
    else
      Result := CP936CU[SearchTable(Uni936U, Unicode)];
  end;
end;

function UnicodeToCP950(Unicode: cardinal): integer;
begin
  case Unicode of
    0..127: Result := Unicode;
    else
      Result := CP950CU[SearchTable(Uni950U, Unicode)];
  end;
end;

function UnicodeToCP949(Unicode: cardinal): integer;
begin
  case Unicode of
    0..127: Result := Unicode;
    else
      Result := CP949CU[SearchTable(Uni949U, Unicode)];
  end;
end;

function UnicodeToCP932(Unicode: cardinal): integer;
begin
  case Unicode of
    0..127: Result := Unicode;
    else
      Result := CP932CU[SearchTable(Uni932U, Unicode)];
  end;
end;

function UTF8ToDBCS(const s: string; const UTF8CharConvFunc: TEncConvUnicodeToCharID): string;
var
  len, i, CharLen: integer;
  Src, Dest: PChar;
  c: char;
  Unicode: longword;
begin
  if s = '' then exit('');
  len := length(s);
  SetLength(Result, len); // DBCS needs at most space as UTF-8
  Src  := PChar(s);
  Dest := PChar(Result);
  repeat
    c := Src^;
    if c < #128 then
    begin
      if (c=#0) and (Src-PChar(s)>=len) then break;
      Dest^ := c;
      Inc(Dest);
      Inc(Src);
    end
    else
    begin
      Unicode := UTF8CodepointToUnicode(Src, CharLen);
      Inc(Src, CharLen);
      i := UTF8CharConvFunc(Unicode);
      //writeln(Format('%X', [i]));
      if i >= 0 then
      begin
        if i > $ff then
        begin
          Dest^ := chr(i shr 8);
          Inc(Dest);
          Dest^ := chr(i);
        end
        else
          Dest^ := chr(i);
        Inc(Dest);
      end
      else
      case EncConvErrorMode of
        eemException:
          raise EConvertError.Create('Cannot convert UTF8 to DBCS code page');
        eemReplace:
          begin
            Dest^ := '?';
            Inc(Dest);
          end;
        eemReturnEmpty:
          exit('');
      end;
    end;
  until false;
  //SetLength(Result, Dest - PChar(Result));
  SetLength(Result, {%H-}PtrUInt(Dest) - PtrUInt(Result));
end;

function UTF8ToCP932(const s: string): string;
begin
  Result := UTF8ToDBCS(s, @UnicodeToCP932);
end;

function UTF8ToCP936(const s: string): string;
begin
  Result := UTF8ToDBCS(s, @UnicodeToCP936);
end;

function UTF8ToCP949(const s: string): string;
begin
  Result := UTF8ToDBCS(s, @UnicodeToCP949);
end;

function UTF8ToCP950(const s: string): string;
begin
  Result := UTF8ToDBCS(s, @UnicodeToCP950);
end;


