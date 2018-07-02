unit sCurrencyEdit;
{$I sDefs.inc}

interface

uses
  Windows, SysUtils, Classes, Controls,
  sCurrEdit, sConst;


type
{$IFDEF DELPHI_XE3}[ComponentPlatformsAttribute(pidWin32 or pidWin64)]{$ENDIF}
  TsCurrencyEdit = class(TsCustomNumEdit)
{$IFNDEF NOTFORHELP}
  protected
    FAllowCalculator: boolean;
    procedure ButtonClick; override;
    function CanCalc: boolean; override;
  public
    constructor Create(AOwner:TComponent); override;
  published
    property AllowCalculator: boolean read FAllowCalculator write FAllowCalculator default False;

    property Alignment;
    property AutoSelect;
    property AutoSize;
    property BeepOnError;
    property CheckOnExit;
    property DecimalPlaces;
    property DisplayFormat;
    property DragCursor;
    property DragMode;
    property HideSelection;
    property ImeMode;
    property ImeName;
    property MaxValue;
    property MinValue;
    property ParentShowHint;
    property PopupMenu;
    property ShowButton default False;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
    property Value;
    property Visible;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnContextPopup;

    property HelpContext;
    property OEMConvert;
    property ReadOnly;
    property Enabled;
    property Font;
    property Hint;
    property MaxLength;
    property ParentFont;
    property CharCase;

    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
    property OnEndDock;
    property OnStartDock;
    property OnStartDrag;
{$ENDIF} // NOTFORHELP
  end;


implementation

uses sGlyphUtils;


procedure TsCurrencyEdit.ButtonClick;
begin
  if Assigned(FOnButtonClick) then
    FOnButtonClick(Self)
  else
    inherited;
end;


function TsCurrencyEdit.CanCalc: boolean;
begin
  Result := FAllowCalculator;
end;


constructor TsCurrencyEdit.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  ShowButton := False;
  FDefBmpID := iBTN_CALC;
  SkinData.COC := COC_TsCurrencyEdit;
  Width := 80;
  FAllowCalculator := False;
end;

end.
