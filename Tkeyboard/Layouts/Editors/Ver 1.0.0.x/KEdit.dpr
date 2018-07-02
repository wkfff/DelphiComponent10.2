program KEdit;

uses
  Forms,
  mainUnit in 'mainUnit.pas' {MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Virtual keyboard layout editor';
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
