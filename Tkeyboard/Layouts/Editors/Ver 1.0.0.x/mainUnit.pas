unit mainUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, KeyboardLayoutEditor, ExtCtrls, Keyboard, StdCtrls, Menus;

type
  TMainForm = class(TForm)
    KeyboardLayoutEditor1: TKeyboardLayoutEditor;
    Button1: TButton;
    Button2: TButton;
    OD1: TOpenDialog;
    SD1: TSaveDialog;
    Panel1: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Button3: TButton;
    Button4: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure KeyboardLayoutEditor1KeyClicked(Key: TKey);
    procedure Button2Click(Sender: TObject);
    procedure KeyboardLayoutEditor1KeyRightClicked(Key: TKey);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
    cd:String;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.FormCreate(Sender: TObject);
begin
cd:=ExtractFilePath(Application.ExeName);
OD1.InitialDir:=cd+'Layouts\';
SD1.InitialDir:=cd+'Layouts\';
end;

procedure TMainForm.Button1Click(Sender: TObject);
begin
if OD1.Execute then
   begin
    KeyboardLayoutEditor1.LoadLayout(OD1.FileName);
    Label2.Caption:='Current layout: '+OD1.FileName;
   end;
   //KeyboardLayoutEditor1.LoadLayout(cd+'Layouts\termo_std.kly');
end;

procedure TMainForm.KeyboardLayoutEditor1KeyClicked(Key: TKey);
begin
KeyboardLayoutEditor1.ShowKeyPropertiesDialog(Key);
end;

procedure TMainForm.Button2Click(Sender: TObject);
begin
if SD1.Execute then
   begin
    KeyboardLayoutEditor1.SaveLayout(SD1.FileName);
    Label2.Caption:='Current layout: '+OD1.FileName;
   end;
end;

procedure TMainForm.KeyboardLayoutEditor1KeyRightClicked(Key: TKey);
begin
Case MessageDlg('Delete key?', mtConfirmation, [mbYes, mbNo], 0) of
 mrYes:KeyboardLayoutEditor1.DeleteKey(Key);
end;
end;

procedure TMainForm.Button3Click(Sender: TObject);
begin
KeyboardLayoutEditor1.NewKey;
end;

procedure TMainForm.Button4Click(Sender: TObject);
begin
ShowMessage('Right click ok key to be deleted.');
end;

end.
