// Code from lazutf8.pas

function UnicodeToUTF8Inline(CodePoint: cardinal; Buf: PChar): integer;
begin
  case CodePoint of
    0..$7f:
      begin
        Result:=1;
        Buf[0]:=char(byte(CodePoint));
      end;
    $80..$7ff:
      begin
        Result:=2;
        Buf[0]:=char(byte($c0 or (CodePoint shr 6)));
        Buf[1]:=char(byte($80 or (CodePoint and $3f)));
      end;
    $800..$ffff:
      begin
        Result:=3;
        Buf[0]:=char(byte($e0 or (CodePoint shr 12)));
        Buf[1]:=char(byte((CodePoint shr 6) and $3f) or $80);
        Buf[2]:=char(byte(CodePoint and $3f) or $80);
      end;
    $10000..$10ffff:
      begin
        Result:=4;
        Buf[0]:=char(byte($f0 or (CodePoint shr 18)));
        Buf[1]:=char(byte((CodePoint shr 12) and $3f) or $80);
        Buf[2]:=char(byte((CodePoint shr 6) and $3f) or $80);
        Buf[3]:=char(byte(CodePoint and $3f) or $80);
      end;
  else
    Result:=0;
  end;
end;

function UnicodeToUTF8SkipErrors(CodePoint: cardinal; Buf: PChar): integer; inline;
begin
  Result:=UnicodeToUTF8Inline(CodePoint,Buf);
end;


function UTF8CodepointToUnicode(p: PChar; out CodepointLen: integer): Cardinal;
{ if p=nil then CodepointLen=0 otherwise CodepointLen>0
  If there is an encoding error the Result is 0 and CodepointLen=1.
  Use UTF8FixBroken to fix UTF-8 encoding.
  It does not check if the codepoint is defined in the Unicode tables.
}
begin
  if p<>nil then begin
    if ord(p^)<%11000000 then begin
      // regular single byte character (#0 is a normal char, this is pascal ;)
      Result:=ord(p^);
      CodepointLen:=1;
    end
    else if ((ord(p^) and %11100000) = %11000000) then begin
      // starts with %110 => could be double byte character
      if (ord(p[1]) and %11000000) = %10000000 then begin
        CodepointLen:=2;
        Result:=((ord(p^) and %00011111) shl 6) or (ord(p[1]) and %00111111);
        if Result<(1 shl 7) then begin
          // wrong encoded, could be an XSS attack
          Result:=0;
        end;
      end else begin
        Result:=ord(p^);
        CodepointLen:=1;
      end;
    end
    else if ((ord(p^) and %11110000) = %11100000) then begin
      // starts with %1110 => could be triple byte character
      if ((ord(p[1]) and %11000000) = %10000000)
      and ((ord(p[2]) and %11000000) = %10000000) then begin
        CodepointLen:=3;
        Result:=((ord(p^) and %00011111) shl 12)
                or ((ord(p[1]) and %00111111) shl 6)
                or (ord(p[2]) and %00111111);
        if Result<(1 shl 11) then begin
          // wrong encoded, could be an XSS attack
          Result:=0;
        end;
      end else begin
        Result:=ord(p^);
        CodepointLen:=1;
      end;
    end
    else if ((ord(p^) and %11111000) = %11110000) then begin
      // starts with %11110 => could be 4 byte character
      if ((ord(p[1]) and %11000000) = %10000000)
      and ((ord(p[2]) and %11000000) = %10000000)
      and ((ord(p[3]) and %11000000) = %10000000) then begin
        CodepointLen:=4;
        Result:=((ord(p^) and %00001111) shl 18)
                or ((ord(p[1]) and %00111111) shl 12)
                or ((ord(p[2]) and %00111111) shl 6)
                or (ord(p[3]) and %00111111);
        if Result<(1 shl 16) then begin
          // wrong encoded, could be an XSS attack
          Result:=0;
        end;
      end else begin
        Result:=ord(p^);
        CodepointLen:=1;
      end;
    end
    else begin
      // invalid character
      Result:=ord(p^);
      CodepointLen:=1;
    end;
  end else begin
    Result:=0;
    CodepointLen:=0;
  end;
end;

