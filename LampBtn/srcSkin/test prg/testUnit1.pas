unit testUnit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, CtHeader, ExtCtrls, sPanel, sSkinProvider, sSkinManager, StdCtrls,
  TEx2Header, TexDef;

type
  TForm1 = class(TForm)
    sSkinManager1: TsSkinManager;
    sSkinProvider1: TsSkinProvider;
    TEx2Header1: TTEx2Header;
    CtHeader1: TCtHeader;
    RadioGroup1: TRadioGroup;
    procedure RadioGroup1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.RadioGroup1Click(Sender: TObject);
begin
if RadioGroup1.ItemIndex=0 then
   begin
    CtHeader1.HeaderRes:=hr640;
    ClientWidth:=640;
   end
else
   begin
    CtHeader1.HeaderRes:=hr1024;
    ClientWidth:=1024;
   end;
end;

end.
