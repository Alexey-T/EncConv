unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    L: TListBox;
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

uses EncConv;

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
const
  c_ru = 'Просто';
  c_jp = 'のＭＧＣを振り返る';
var
  ok: boolean;
  s1, s2: string;
begin
  L.Items.Add('test for russian...');
  s1:= EncConvertFromUTF8(c_ru, eidCP1251);
  L.Items.Add('conv from utf8: '+s1);
  s2:= EncConvertToUTF8(s1, eidCP1251);
  L.Items.Add('conv to utf8: '+s2);
  ok:= c_ru=s2;
  if ok then
    s1:= 'test ok'
  else
    s1:= 'test failed';
  L.Items.Add(s1);

  L.Items.Add('');
  L.Items.Add('test for japanese...');
  s1:= EncConvertFromUTF8(c_jp, eidCP932);
  L.Items.Add('conv from utf8: '+s1);
  s2:= EncConvertToUTF8(s1, eidCP932);
  L.Items.Add('conv to utf8: '+s2);
  ok:= c_jp=s2;
  if ok then
    s1:= 'test ok'
  else
    s1:= 'test failed';
  L.Items.Add(s1);

end;

end.

