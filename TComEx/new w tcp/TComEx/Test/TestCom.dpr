program TestCom;

uses
  Forms,
  TestUnit in 'TestUnit.pas' {Form1},
  uComandi in '..\..\..\Software Develop\Delphi Class\TermoCONST\uComandi.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
