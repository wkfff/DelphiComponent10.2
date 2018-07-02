//////////////////////////////////////////////////////////////////////////////////////////
// Project: TKeyboard
// Unit:    KeyboardLayoutEditor
// Author:  Martin Geier
//
// Purpose:
// Keyboard layout editor component for TKeyboard.
//
// History: 17.06.2002: created
//////////////////////////////////////////////////////////////////////////////////////////
unit KeyboardLayoutEditor;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, ExtCtrls, Keyboard, Graphics, Menus;

type
  // event types
	TKeyClickedEvent = procedure(Key: TKey) of object;

  TKeyboardLayoutEditor = class(TCustompanel)
  private
    FKeys: TKeyCollection;
    FDrag: Boolean;
    FMouseDown: Boolean;
    FX0, FY0: Integer;
    FHGrid, FVGrid: Integer;
    FDefaultFont: TFont;
    FGridColor: TColor;
    FOnKeyClicked: TKeyClickedEvent;
    FOnKeyRightClicked: TKeyClickedEvent;
    FLimitDesignArea: Boolean;

    // helper methods
    function CreateKeyControl: TKeyControl;
    procedure CreateControls;
    procedure DestroyControls;
    procedure ShowPropertiesForm(Key: TKey);
    procedure CreateGrid;

    // events assigned to controls for drag and drop positioning
    procedure BtnMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure BtnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure BtnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure BtnClick(Sender: TObject);
    function GetKey(i: Integer): TKey;
    procedure SetGridColor(const Value: TColor);
    procedure SetDefaultFont(const Value: TFont);

  protected
    // initialisation
    procedure Loaded; override;

  public
    property Keys[i: Integer]: TKey read GetKey;  // keys collection

    // construction and destruction
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // loading and saving
    procedure LoadLayout(LayoutFileName: string);
    procedure SaveLayout(LayoutFileName: string);

    // keys management
    procedure ClearAll;
    procedure NewKey;
    procedure DeleteKey(Key: TKey);
    procedure SetKeyFont(Font: TFont);
    procedure ShowKeyPropertiesDialog(Key: TKey);

    // key placement
    procedure MoveKeysLeft(Delta: Integer = HORZ_BASE_UNIT);
    procedure MoveKeysRight(Delta: Integer = HORZ_BASE_UNIT);
    procedure MoveKeysUp(Delta: Integer = VERT_BASE_UNIT);
    procedure MoveKeysDown(Delta: Integer = VERT_BASE_UNIT);

  published
    // publish some useful properties of TCustomPanel
    property Align;
    property BevelOuter;
    property BevelInner;
    property BevelWidth;
    property BorderStyle;
    property BorderWidth;
    property Color;
    property Constraints;
    property Enabled;
    property OnEnter;
    property OnExit;

    // new properties
    property DefaultKeyFont: TFont read FDefaultFont write SetDefaultFont;
    property GridColor: TColor read FGridColor write SetGridColor;
    property LimitDesignArea: Boolean read FLimitDesignArea write FLimitDesignArea;

    // new event properties
		property OnKeyClicked: TKeyClickedEvent read FOnKeyClicked write FOnKeyClicked;
		property OnKeyRightClicked: TKeyClickedEvent read FOnKeyRightClicked write FOnKeyRightClicked;
  end;

implementation

uses KeyPropertiesF, ActnList;

resourcestring
  ERR_DELTA_INVALID = 'A "Delta" value of %d is invalid';

{ TKeyboardLayoutEditor }

//////////////////////////////////////////////////////////////////////////////////////////
// TKeyboardLayoutEditor.LoadLayout
//
// Loads a layout from file. Current layout will be discarded
//////////////////////////////////////////////////////////////////////////////////////////
procedure TKeyboardLayoutEditor.LoadLayout(LayoutFileName: string);
begin
  DestroyControls;
  FKeys.LoadFromFile(LayoutFileName);
  CreateControls;
end;

//////////////////////////////////////////////////////////////////////////////////////////
// TKeyboardLayoutEditor.SaveLayout
//
// Saves the current layout.
//////////////////////////////////////////////////////////////////////////////////////////
procedure TKeyboardLayoutEditor.SaveLayout(LayoutFileName: string);
begin
  FKeys.SaveToFile(LayoutFileName);
end;

//////////////////////////////////////////////////////////////////////////////////////////
// TKeyboardLayoutEditor.CreateControls
//
// Creates all key controls needed for keys in FKeys
//////////////////////////////////////////////////////////////////////////////////////////
procedure TKeyboardLayoutEditor.CreateControls;
var
  i: Integer;
  Ctrl: TKeyControl;
begin
  for i := 0 to FKeys.Count - 1 do
  begin
    Ctrl := CreateKeyControl;
    FKeys[i].AssignControl(Ctrl);
  end;
end;

//////////////////////////////////////////////////////////////////////////////////////////
// TKeyboardLayoutEditor.DestroyControls
//
// Destroys all key controls
//////////////////////////////////////////////////////////////////////////////////////////
procedure TKeyboardLayoutEditor.DestroyControls;
var
  i: Integer;
  Ctrl: TKeyControl;
begin
  for i := 0 to FKeys.Count - 1 do
  begin
    Ctrl := FKeys[i].Control;
    if assigned(Ctrl) then
    begin
      Ctrl.Parent := nil;
      Ctrl.Free;
      FKeys[i].Control := nil;
    end;
  end;
end;

//////////////////////////////////////////////////////////////////////////////////////////
// TKeyboardLayoutEditor.CreateKeyControl
//
// Creates a new KeyControl, assigns it to TKeyboardLayoutEditor and sets events
// for drag and drop positioning, and mouse clicks.
//////////////////////////////////////////////////////////////////////////////////////////
function TKeyboardLayoutEditor.CreateKeyControl: TKeyControl;
var
  Ctrl: TKeyControl;
begin
  Ctrl := TKeyControl.Create(Self);
  Ctrl.Parent := Self;

  // set events of control for drag and drop positioning
  Ctrl.OnMouseMove := BtnMouseMove;
  Ctrl.OnMouseDown := BtnMouseDown;
  Ctrl.OnMouseUp := BtnMouseUp;
  Ctrl.OnClick := BtnClick;
  Result := Ctrl;
end;

//////////////////////////////////////////////////////////////////////////////////////////
// TKeyboardLayoutEditor.BtnClick
//////////////////////////////////////////////////////////////////////////////////////////
procedure TKeyboardLayoutEditor.BtnClick(Sender: TObject);
var
  Key: TKey;
begin
  if not FDrag then
  begin
    // get reference to key
    Key := (Sender as TKeyControl).Key;

    // fire OnKeyClicked event
    if assigned(FOnKeyClicked) then
      FOnKeyClicked(Key);
  end
  else
    FDrag := False;
end;

//////////////////////////////////////////////////////////////////////////////////////////
// TKeyboardLayoutEditor.BtnMouseDown
//////////////////////////////////////////////////////////////////////////////////////////
procedure TKeyboardLayoutEditor.BtnMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Key: TKey;
begin
  if Button = mbLeft then
  begin
    FMouseDown := True;
    FX0 := X;
    FY0 := Y;
  end
  else  // use the mouse down event for the right mouse button as right mouse button click
  begin
    // get reference to key
    Key := (Sender as TKeyControl).Key;

    // fire OnKeyRightClicked event
    if assigned(FOnKeyRightClicked) then
      FOnKeyRightClicked(Key);
  end;
end;

//////////////////////////////////////////////////////////////////////////////////////////
// TKeyboardLayoutEditor.BtnMouseMove
//////////////////////////////////////////////////////////////////////////////////////////
procedure TKeyboardLayoutEditor.BtnMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;
  Ctrl: TKeyControl;
  HalfKeyWidth, HalfKeyHeight: Integer;
begin
  Ctrl := (Sender as TKeyControl);

  if FMouseDown and ((Abs(X - FX0) > 2) or (Abs(Y - FY0) > 2)) then
    FDrag := True;

  if not FDrag then
    exit;

  P := Self.ScreenToClient(Mouse.CursorPos);
  Ctrl.Key.Left := ((P.X div FHGrid) * FHGrid - Ctrl.Width div 2) div HORZ_BASE_UNIT;
  Ctrl.Key.Top  := ((P.Y div FHGrid) * FHGrid - Ctrl.Height div 2) div VERT_BASE_UNIT;

  // check and adjust bounds if LimitDesignArea is True
  if FLimitDesignArea then
  begin
    HalfKeyWidth := Ctrl.Key.Width * HORZ_BASE_UNIT div 2;
    HalfKeyHeight := Ctrl.Key.Height * VERT_BASE_UNIT div 2;

    // horizontal
    if (P.X < HalfKeyWidth) then
      Ctrl.Key.Left := 0;
    if (P.X > (ClientWidth - HalfKeyWidth)) then
      Ctrl.Key.Left := (ClientWidth - 2 * HalfKeyWidth) div HORZ_BASE_UNIT;

    // vertical
    if (P.Y < HalfKeyHeight) then
      Ctrl.Key.Top := 0;
    if (P.Y > (ClientHeight - HalfKeyHeight)) then
      Ctrl.Key.Top := (ClientHeight - 2 * HalfKeyHeight) div VERT_BASE_UNIT;
  end;
end;

//////////////////////////////////////////////////////////////////////////////////////////
// TKeyboardLayoutEditor.BtnMouseUp
//////////////////////////////////////////////////////////////////////////////////////////
procedure TKeyboardLayoutEditor.BtnMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
    FMouseDown := False;
end;

//////////////////////////////////////////////////////////////////////////////////////////
// TKeyboardLayoutEditor.ShowPropertiesForm
//////////////////////////////////////////////////////////////////////////////////////////
procedure TKeyboardLayoutEditor.ShowPropertiesForm(Key: TKey);
var
  KeyPropertiesForm: TKeyPropertiesForm;
begin
  // show Key properties Form
  KeyPropertiesForm := TKeyPropertiesForm.Create(Self, Key);
  try
    if KeyPropertiesForm.ShowModal = mrOK then
      Key.UpdateControl;
  finally
    KeyPropertiesForm.Free;
  end;
end;

//////////////////////////////////////////////////////////////////////////////////////////
// TKeyboardLayoutEditor.Create
//////////////////////////////////////////////////////////////////////////////////////////
constructor TKeyboardLayoutEditor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  // initialize members
  FHGrid := HORZ_BASE_UNIT;
  FVGrid := VERT_BASE_UNIT;
  FKeys := TKeyCollection.Create;
  FGridColor := clSilver;
  FLimitDesignArea := True;
  Caption := '';

  // set default font to MS Sans Serif, 8pt
  FDefaultFont := TFont.Create;
  FDefaultFont.Name := 'MS Sans Serif';
  FDefaultFont.Size := 8;

  CreateGrid;
end;

//////////////////////////////////////////////////////////////////////////////////////////
// TKeyboardLayoutEditor.Destroy
//////////////////////////////////////////////////////////////////////////////////////////
destructor TKeyboardLayoutEditor.Destroy;
begin
  FKeys.Free;
  FDefaultFont.Free;

  inherited;
end;

//////////////////////////////////////////////////////////////////////////////////////////
// TKeyboardLayoutEditor.CreateGrid
//
// Creates a grid spanning 2000x1500 pixels with a grid size of 2 x BASE_UNIT
//////////////////////////////////////////////////////////////////////////////////////////
procedure TKeyboardLayoutEditor.CreateGrid;
var
  i: Integer;
  S: TShape;
begin
  // vertical lines
  i := 0;
  while i <= 2000 do
  begin
    S := TShape.Create(Self);
    S.Shape := stRectangle;
    S.Pen.Style := psSolid;
    S.Pen.Color := FGridColor;
    S.Top := 0;
    S.Height := 1500;
    S.Left := i;
    S.Width := 1;
    i := i + 2 * HORZ_BASE_UNIT;
    S.Parent := Self;
  end;

  // horizontal lines
  i := 0;
  while i <= 1500 do
  begin
    S := TShape.Create(Self);
    S.Shape := stRectangle;
    S.Pen.Style := psSolid;
    S.Pen.Color := GridColor;
    S.Top := i;
    S.Height := 1;
    S.Left := 0;
    S.Width := 2000;
    i := i + 2 * VERT_BASE_UNIT;
    S.Parent := Self;
  end;
end;

//////////////////////////////////////////////////////////////////////////////////////////
// TKeyboardLayoutEditor.GetKey
//////////////////////////////////////////////////////////////////////////////////////////
function TKeyboardLayoutEditor.GetKey(i: Integer): TKey;
begin
  Result := FKeys[i];
end;


//////////////////////////////////////////////////////////////////////////////////////////
// TKeyboardLayoutEditor.ClearAll
//////////////////////////////////////////////////////////////////////////////////////////
procedure TKeyboardLayoutEditor.ClearAll;
begin
  DestroyControls;
  FKeys.DeleteAllKeys;
end;

//////////////////////////////////////////////////////////////////////////////////////////
// TKeyboardLayoutEditor.DeleteKey
//////////////////////////////////////////////////////////////////////////////////////////
procedure TKeyboardLayoutEditor.DeleteKey(Key: TKey);
var
  Ctrl: TKeyControl;
begin
  // get Control
  Ctrl := (Key.Control as TKeyControl);

  // delete key from key collection and free it
  FKeys.DeleteAndFreeKey(Key);

  // free control
  Ctrl.Free;
end;

//////////////////////////////////////////////////////////////////////////////////////////
// TKeyboardLayoutEditor.NewKey
//////////////////////////////////////////////////////////////////////////////////////////
procedure TKeyboardLayoutEditor.NewKey;
var
  K: TKey;
  Ctrl: TKeyControl;
begin
  // create key
  K := TKey.Create(ktNormal);
  K.Top := 0;
  K.Left := 0;
  K.Width := DEFAULT_KEY_WIDTH;
  K.Height := DEFAULT_KEY_HEIGHT;
  K.Font.Assign(FDefaultFont);
  FKeys.AddKey(K);

  // create key control and assign it to the Key
  Ctrl := CreateKeyControl;
  K.AssignControl(Ctrl);
end;

//////////////////////////////////////////////////////////////////////////////////////////
// TKeyboardLayoutEditor.SetGridColor
//////////////////////////////////////////////////////////////////////////////////////////
procedure TKeyboardLayoutEditor.SetGridColor(const Value: TColor);
var
  i: Integer;
begin
  FGridColor := Value;

  // loop over all shape controls of the grid and change the pen color
  for i := 0 to ControlCount - 1 do
    if Controls[i] is TShape then
      (Controls[i] as TShape).Pen.Color := FGridColor;
end;

//////////////////////////////////////////////////////////////////////////////////////////
// TKeyboardLayoutEditor.SetDefaultFont
//////////////////////////////////////////////////////////////////////////////////////////
procedure TKeyboardLayoutEditor.SetDefaultFont(const Value: TFont);
begin
  FDefaultFont.Assign(Value);
end;

//////////////////////////////////////////////////////////////////////////////////////////
// TKeyboardLayoutEditor.SetKeyFont
//
// Assign Font as font for all keys of the current layout and makes it the default font
// for new keys.
//////////////////////////////////////////////////////////////////////////////////////////
procedure TKeyboardLayoutEditor.SetKeyFont(Font: TFont);
var
  i: Integer;
begin
  // make the new font the default font
  FDefaultFont.Assign(Font);

  // loop over all keys and assign new font
  for i := 0 to FKeys.Count - 1 do
  begin
    FKeys[i].Font.Assign(FDefaultFont);
    FKeys[i].UpdateControl;
  end;
end;

//////////////////////////////////////////////////////////////////////////////////////////
// TKeyboardLayoutEditor.MoveKeysDown
//////////////////////////////////////////////////////////////////////////////////////////
procedure TKeyboardLayoutEditor.MoveKeysDown(Delta: Integer);
var
  i: Integer;
begin
  // don't accept Delta < 1
  if Delta < 1 then
    raise Exception.Create(Format(ERR_DELTA_INVALID, [Delta]));

  // check lower bounds
  if FLimitDesignArea then
    for i := 0 to FKeys.Count - 1 do
      if (FKeys[i].Top + FKeys[i].Height + Delta) > ClientHeight div VERT_BASE_UNIT then
        exit;

  for i := 0 to FKeys.Count - 1 do
    FKeys[i].Top := FKeys[i].Top + Delta;
end;

//////////////////////////////////////////////////////////////////////////////////////////
// TKeyboardLayoutEditor.MoveKeysLeft
//////////////////////////////////////////////////////////////////////////////////////////
procedure TKeyboardLayoutEditor.MoveKeysLeft(Delta: Integer);
var
  i: Integer;
begin
  // don't accept Delta < 1
  if Delta < 1 then
    raise Exception.Create(Format(ERR_DELTA_INVALID, [Delta]));

  // check left bounds
  if FLimitDesignArea then
    for i := 0 to FKeys.Count - 1 do
      if FKeys[i].Left < Delta then
        exit;

  for i := 0 to FKeys.Count - 1 do
    FKeys[i].Left := FKeys[i].Left - Delta;
end;

//////////////////////////////////////////////////////////////////////////////////////////
// TKeyboardLayoutEditor.MoveKeysRight
//////////////////////////////////////////////////////////////////////////////////////////
procedure TKeyboardLayoutEditor.MoveKeysRight(Delta: Integer);
var
  i: Integer;
begin
  // don't accept Delta < 1
  if Delta < 1 then
    raise Exception.Create(Format(ERR_DELTA_INVALID, [Delta]));

  // check right bounds
  if FLimitDesignArea then
    for i := 0 to FKeys.Count - 1 do
      if (FKeys[i].Left + FKeys[i].Width + Delta) > ClientWidth div HORZ_BASE_UNIT then
        exit;

  for i := 0 to FKeys.Count - 1 do
    FKeys[i].Left := FKeys[i].Left + Delta;
end;

//////////////////////////////////////////////////////////////////////////////////////////
// TKeyboardLayoutEditor.MoveKeysUp
//////////////////////////////////////////////////////////////////////////////////////////
procedure TKeyboardLayoutEditor.MoveKeysUp(Delta: Integer);
var
  i: Integer;
begin
  // don't accept Delta < 1
  if Delta < 1 then
    raise Exception.Create(Format(ERR_DELTA_INVALID, [Delta]));

  // check upper bounds
  if FLimitDesignArea then
    for i := 0 to FKeys.Count - 1 do
      if FKeys[i].Top < Delta then
        exit;

  for i := 0 to FKeys.Count - 1 do
    FKeys[i].Top := FKeys[i].Top - Delta;
end;

//////////////////////////////////////////////////////////////////////////////////////////
// TKeyboardLayoutEditor.ShowKeyPropertiesDialog
//////////////////////////////////////////////////////////////////////////////////////////
procedure TKeyboardLayoutEditor.ShowKeyPropertiesDialog(Key: TKey);
begin
  ShowPropertiesForm(Key);
end;


//////////////////////////////////////////////////////////////////////////////////////////
// TKeyboardLayoutEditor.Loaded
//////////////////////////////////////////////////////////////////////////////////////////
procedure TKeyboardLayoutEditor.Loaded;
begin
  inherited;

  Caption := '';
end;

end.
