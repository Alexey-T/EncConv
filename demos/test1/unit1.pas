unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    ComboBox1: TComboBox;
    L: TListBox;
    Label1: TLabel;
    Panel1: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
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

procedure TForm1.Button1Click(Sender: TObject);
const
  c_ru = 'Просто';
  c_jp = 'のＭＧＣを振り返る';
  c_mix = 'пэцを振りeng';
  err: array[boolean] of string = ('test failed', 'test ok');
var
  s1, s2: string;
begin
  L.Items.Clear;

  L.Items.Add('test for russian...');
  s1:= EncConvertFromUTF8(c_ru, eidCP1251);
  s2:= EncConvertToUTF8(s1, eidCP1251);
  L.Items.Add('conv to utf8: '+s2);
  L.Items.Add(err[c_ru=s2]);

  L.Items.Add('');
  L.Items.Add('test for japanese...');
  s1:= EncConvertFromUTF8(c_jp, eidCP932);
  s2:= EncConvertToUTF8(s1, eidCP932);
  L.Items.Add('conv to utf8: '+s2);
  L.Items.Add(err[c_jp=s2]);

  L.Items.Add('');
  L.Items.Add('test for mixed ru/jp, must fail...');
  s1:= EncConvertFromUTF8(c_mix, eidCP1251);
  s2:= EncConvertToUTF8(s1, eidCP1251);
  L.Items.Add('conv to utf8: '+s2);
  L.Items.Add(err[c_mix=s2]);

end;

procedure TForm1.ComboBox1Change(Sender: TObject);
begin
  EncConvErrorMode:= TEncConvErrorMode(ComboBox1.ItemIndex);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ComboBox1.ItemIndex:= Ord(EncConvErrorMode);
end;

end.

