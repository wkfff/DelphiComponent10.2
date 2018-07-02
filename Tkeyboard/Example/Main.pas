//-----------------------------------------------------------------------------
// Project: TKeyboardExample
// Unit:    Main
// Author:  Martin Geier
//
// Purpose:
// Example to demonstrate the use of TKeyboard. TKeyboard must be installed into
// the Delphi IDE for this example to work.
//
// History: 06.04.2002: created
//-----------------------------------------------------------------------------
unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Keyboard;

type
  TForm1 = class(TForm)
    Keyboard1: TKeyboard;
    Edit1: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    procedure Keyboard1Key(Sender: TObject; Key: TKey; KeyLevel: TKeyLevel;
      KeyValue: String);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Keyboard1Key(Sender: TObject; Key: TKey;
  KeyLevel: TKeyLevel; KeyValue: String);
begin
  Label3.Caption := Key.KeyCaption[KeyLevel];
end;

end.
