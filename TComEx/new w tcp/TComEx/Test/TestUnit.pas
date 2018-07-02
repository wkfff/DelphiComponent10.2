unit TestUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComEx, StdCtrls, ExtCtrls, Spin;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    Button1: TButton;
    Button2: TButton;
    Timer1: TTimer;
    SpinEdit1: TSpinEdit;
    Button3: TButton;
    Panel1: TPanel;
    SpinEdit2: TSpinEdit;
    Button4: TButton;
    CheckBox1: TCheckBox;
    ComEx1: TComEx;
    ComEx2: TComEx;
    RG1: TRadioGroup;
    RadioGroup1: TRadioGroup;
    ComboBox1: TComboBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses uComandi;

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
Var t:TCommTimeouts;
begin
t:=ComEx1.TimeOuts;
ComEx1.Memo:=Memo1;
ComEx2.Memo:=Memo1;
ComEx1.UseOverlap:=True;
ComEx2.UseOverlap:=True;
t.ReadIntervalTimeout:=3000;
t.ReadTotalTimeoutMultiplier:=10;
ComEx1.TimeOuts:=t;
ComEx2.TimeOuts:=ComEx1.TimeOuts;
end;

procedure TForm1.Button3Click(Sender: TObject);
Var i:Integer;
begin
ComEx1.Close;
if CheckBox2.Checked then
   begin
    ComEx1.UseNet:=nmMTCPIP;
    ComEx1.UseCrypt:=False;
    ComEx1.NetPort:=5050;
    ComEx1.NetFile:='localhost';
   end
else if CheckBox3.Checked then
   begin
    ComEx1.UseNet:=nmSTCPIP;
    ComEx1.UseCrypt:=False;
    ComEx1.NetPort:=5050;
    ComEx1.NetFile:='localhost';
   end
else
   begin
    ComEx1.UseNet:=nmNone;
    ComEx1.UseCrypt:=False;
    case RG1.itemIndex of
     0:ComEx1.BaudRate:=CBR_4800;
     1:ComEx1.BaudRate:=CBR_9600;
     2:ComEx1.BaudRate:=CBR_2400;
    end;
    ComEx1.Porta:=SpinEdit1.Value;
    case RadioGroup1.ItemIndex of
     0:ComEx1.ByteSize:=7;
     else ComEx1.ByteSize:=8;
    end;
    //NOPARITY = 0;    ODDPARITY = 1;  EVENPARITY = 2;  MARKPARITY = 3;  SPACEPARITY = 4;
    case ComboBox1.ItemHeight of
     1:ComEx1.Parity:=EVENPARITY;
     2:ComEx1.Parity:=ODDPARITY;
     else ComEx1.Parity:=NOPARITY;
    end;
    ComEx1.MaxLogSize:=1000;
   end;
ComEx1.LogFn:=extractFilePath(paramstr(0))+'Com0'+IntToStr(ComEx1.Porta)+'.log';
if CheckBox1.Checked then
   begin
    ComEx1.LogOnFile:=True;
    ComEx1.LabelArray1[SOC]:='SOC';
    ComEx1.LabelArray1[SOD]:='SOD';
    ComEx1.ByteValueToLabelArray1:=0;
    for i:=0 to 255 do
        ComEx1.LabelArray2[i]:=NomiComandi[i];
    ComEx1.ByteValueToLabelArray2:=2;
    ComEx1.LabelArray3[ETX]:='ETX';
    ComEx1.ByteValueToLabelArray3:=19;
    ComEx1.BufferToHexString:=True;
   end;
ComEx1.Open;
end;

procedure TForm1.Button4Click(Sender: TObject);
Var i:Integer;
begin
ComEx2.Close;
ComEx2.UseNet:=nmNone;
ComEx2.UseCrypt:=False;
case RG1.itemIndex of
 0:ComEx2.BaudRate:=CBR_4800;
 1:ComEx2.BaudRate:=CBR_9600;
end;
ComEx2.Porta:=SpinEdit2.Value;
ComEx2.MaxLogSize:=1000;
ComEx2.LogFn:=extractFilePath(paramstr(0))+'Com0'+IntToStr(ComEx2.Porta)+'.log';
if CheckBox1.Checked then
   begin
    ComEx2.LogOnFile:=True;
    ComEx2.LabelArray1[SOC]:='SOC';
    ComEx2.LabelArray1[SOD]:='SOD';
    ComEx2.ByteValueToLabelArray1:=0;
    for i:=0 to 255 do
        ComEx2.LabelArray2[i]:=NomiComandi[i];
    ComEx2.ByteValueToLabelArray2:=2;
    ComEx2.LabelArray3[ETX]:='ETX';
    ComEx2.ByteValueToLabelArray3:=19;
    ComEx2.BufferToHexString:=True;
   end;
ComEx2.Open;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
ComEx1.Purge;
ComEx2.Purge;
Timer1.Enabled:=True;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
Timer1.Enabled:=False;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
Var b:Array[0..1000] of byte;
begin
Timer1.Enabled:=False;
ComEx1.Read(b,1); //20
ComEx2.Read(b,36);
Timer1.Enabled:=True;
end;

end.
