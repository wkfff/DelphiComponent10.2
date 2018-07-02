//-----------------------------------------------------------------------------
// Project: TKeyboard
// Unit:    LayoutProperty
// Author:  Martin
//
// Purpose:
// Property Editor for Keyboard Layout property
//
// History: 24.11.2001: created
//-----------------------------------------------------------------------------

unit LayoutProperty;

interface

uses DsgnIntf;


type
  TLayoutProperty = class(TPropertyEditor)
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: String; override;
    procedure Edit; override;
  end;


implementation

uses LayoutF, Keyboard;

{ TLayoutProperty }

//-----------------------------------------------------------------------------
// TLayoutProperty.GetAttributes
//-----------------------------------------------------------------------------
function TLayoutProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

//-----------------------------------------------------------------------------
// TLayoutProperty.GetValue
//-----------------------------------------------------------------------------
function TLayoutProperty.GetValue: String;
begin
  Result := 'TKeyCollection';
end;

//-----------------------------------------------------------------------------
// TLayoutProperty.Edit
//
// Open LayoutForm when property editor should edit
//-----------------------------------------------------------------------------
procedure TLayoutProperty.Edit;
var
  LayoutForm: TLayoutForm;
  Keys: TKeyCollection;
begin
  inherited;

  Keys := TKeyCollection(GetOrdValue);

  LayoutForm := TLayoutForm.Create(nil, Keys);
  try
    LayoutForm.ShowModal;
  finally
    LayoutForm.Free;
  end;

  SetOrdValue(Integer(Keys));
end;

end.
