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
unit CompReg;

interface

uses Classes, keyboard, DsgnIntf;

procedure Register;

implementation

uses LayoutProperty;

procedure Register;
begin
  RegisterComponents('Touch Screen', [TKeyboard]);

  RegisterPropertyEditor(TypeInfo(TKeyCollection), TKeyboard, 'Keys', TLayoutProperty);
end;

end.
