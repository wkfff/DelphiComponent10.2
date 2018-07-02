unit TestUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, Lampada, ButtonLed, Selettore, ButtonNumber,
  TEx2Header, MenuButton, pngimage, TermoUnit, ButtonFunction, ComCtrls;

type
  TForm1 = class(TForm)
    MenuButton4: TMenuButton;
    ButtonLed1: TButtonLed;
    Selettore1: TSelettore;
    Lampada1: TLampada;
    StazDosaggio1: TStazDosaggio;
    ButtonFunction1: TButtonFunction;
    TEx2Header1: TTEx2Header;
    Panel1: TPanel;
    MenuButton1: TMenuButton;
    MenuButton2: TMenuButton;
    MenuButton3: TMenuButton;
    MenuButton5: TMenuButton;
    MenuButton6: TMenuButton;
    PC1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    TabSheet5: TTabSheet;
    Lampada2: TLampada;
    procedure TEx2Header1Click(Sender: TObject);
    procedure Selettore1Click(Sender: TObject);
    procedure ButtonLed1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Lampada1ON(Sender: TObject);
    procedure TabSheet2Show(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure MenuButton3Click(Sender: TObject);
  private

  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.TEx2Header1Click(Sender: TObject);
begin
TEx2Header1.ClockFont.Color:=clyellow;
end;

procedure TForm1.Selettore1Click(Sender: TObject);
begin
ButtonLed1.Blink:=Selettore1.Position=2;
end;

procedure TForm1.ButtonLed1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
ButtonLed1.Blink:=False;
end;

procedure TForm1.Lampada1ON(Sender: TObject);
begin
Lampada1.Caption:='Warn';
Lampada1.OnON:=NIL;
end;

procedure TForm1.TabSheet2Show(Sender: TObject);
begin
Lampada2.Selected:=NOT Lampada2.Selected;
end;

procedure TForm1.FormCreate(Sender: TObject);
Var i:integer;
begin
for i:=0 to PC1.PageCount-1 do PC1.Pages[i].TabVisible:=False;
PC1.Pages[0].Show;
end;

procedure TForm1.MenuButton3Click(Sender: TObject);
begin
Caption:='3';
end;

end.
