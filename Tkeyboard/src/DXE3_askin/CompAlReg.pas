//-----------------------------------------------------------------------------
// Project: TKeyboard
// Unit:    CompReg
// Author:  Martin
//
// Purpose:
// Registers components and property editors
//
// History: 25.11.2001: created
//-----------------------------------------------------------------------------
unit CompAlReg;

interface

uses Classes, keyboardAL, keyboardALlayouteditor, DesignIntf;

procedure Register;

implementation

uses LayoutAlProperty;

procedure Register;
begin
  RegisterComponents('Touch Screen', [TKeyboardAL]);
  RegisterComponents('Touch Screen', [TKeyboardALLayoutEditor]);

  RegisterPropertyEditor(TypeInfo(TKeyCollection), TKeyboardAL, 'Keys', TLayoutAlProperty);
end;

end.
