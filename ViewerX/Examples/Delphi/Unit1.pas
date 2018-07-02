unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.OleCtrls, ViewerX_TLB;

type
  TForm1 = class(TForm)
    CSC_ViewerXControl1: TCSC_ViewerXControl;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
CSC_ViewerXControl1.ProxyType:=VPT_ULTRA_REPEATER;
CSC_ViewerXControl1.ProxyIP:='192.168.0.98';
CSC_ViewerXControl1.ProxyPort:=5901;
CSC_ViewerXControl1.HostIP:='20.20.20.206';
CSC_ViewerXControl1.Password:='termo';
CSC_ViewerXControl1.Connect;
CSC_ViewerXControl1.StretchMode:=SSM_ASPECT;
end;

end.
