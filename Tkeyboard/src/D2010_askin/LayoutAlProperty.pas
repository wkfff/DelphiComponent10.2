//-----------------------------------------------------------------------------
// Project: TKeyboard
// Unit:    LayoutLaProperty
// Author:  Martin
//
// Purpose:
// Property Editor for Keyboard Layout property
//
// History: 24.11.2001: created
//-----------------------------------------------------------------------------

unit LayoutAlProperty;

interface

uses designEditors, DesignIntf;

type
  TLayoutAlProperty = class(TPropertyEditor)
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: String; override;
    procedure Edit; override;
  end;


implementation

uses LayoutAlF, KeyboardAL;

{ TLayoutAlProperty }

function TLayoutAlProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

function TLayoutalProperty.GetValue: String;
begin
  Result := 'TKeyCollection';
end;

procedure TLayoutAlProperty.Edit;
var
  LayoutForm: TLayoutAlForm;
  Keys: TKeyCollection;
begin
  inherited;

  Keys := TKeyCollection(GetOrdValue);

  LayoutForm := TLayoutAlForm.Create(nil, Keys);
  try
    LayoutForm.ShowModal;
  finally
    LayoutForm.Free;
  end;

  SetOrdValue(Integer(Keys));
end;

end.
