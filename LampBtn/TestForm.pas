unit TestForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, TEx2Header, TExDef, StdCtrls, Led, sPanel,
  CtHeader, sSkinManager, sLabel, ButtonNumber, Lampada;

type
  TForm1 = class(TForm)
    TEx2Header1: TTEx2Header;
    RadioGroup1: TRadioGroup;
    Button1: TButton;
    FontDialog1: TFontDialog;
    TEx2Header2: TTEx2Header;
    Edit1: TEdit;
    Button2: TButton;
    Timer1: TTimer;
    Timer2: TTimer;
    Timer3: TTimer;
    Button3: TButton;
    TEx2Header3: TTEx2Header;
    Button4: TButton;
    sSkinManager1: TsSkinManager;
    sLabelFX1: TsLabelFX;
    Led1: TLed;
    Lampada1: TLampada;
    procedure RadioGroup1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Timer2Timer(Sender: TObject);
    procedure Timer3Timer(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var Form1: TForm1;

implementation

//uses Unit2;

{$R *.dfm}

procedure TForm1.RadioGroup1Click(Sender: TObject);
begin
TEx2Header1.HeaderRes:=THeaderRes(RadioGroup1.ItemIndex);
case TEx2Header1.HeaderRes of
 hr640:Begin
        ClientWidth:=640;
        ClientHeight:=480;
       end;
 hr800:begin
        ClientWidth:=800;
        ClientHeight:=600;
       end;
 hr1024:begin
         ClientWidth:=1024;
         ClientHeight:=768;
        end;
 hr1280:begin
         ClientWidth:=1280;
         ClientHeight:=768;
        end;
end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
left:=0; top:=0;
RadioGroup1.ItemIndex:=Ord(TEx2Header1.HeaderRes);
RadioGroup1Click(NIL);
with TButtonNumber.Create(self) do
     begin
      parent:=self;
     end;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
if FontDialog1.Execute then
   TEx2Header1.ClockFont:=FontDialog1.Font;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
TEx2Header1.Passo:=StrToInt(edit1.Text);
end;

procedure TForm1.Timer2Timer(Sender: TObject);
begin
TEx2Header1.LedSupAcceso:=Bool(Random(2));
end;

procedure TForm1.Timer3Timer(Sender: TObject);
begin
TEx2Header1.LedPlcAcceso:=Bool(Random(2));
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
TEx2Header1.LedAlAcceso:=Bool(Random(2));
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
TEx2Header1.Programma:='Programm1 234';
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
TEx2Header1.MasterSlaveStatus:=TEx2Header1.MasterSlaveStatus+1;
end;

end.
