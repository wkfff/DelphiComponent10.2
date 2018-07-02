//-----------------------------------------------------------------------------
// Project: TKeyboard
// Unit:    LayoutProperty
// Author:  Martin
//
// Purpose:
// Property Editor Form for Keyboard "Keys" property
//
// History: 24.11.2001: created
//-----------------------------------------------------------------------------
unit LayoutAlF;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ActnList, Menus, Buttons, KeyboardAL,
  sSpeedButton, sButton, sPanel;

type
  TLayoutAlForm = class(TForm)
    ActionList: TActionList;
    NewKeyAction: TAction;
    PopupMenu: TPopupMenu;
    DeleteKeyAction: TAction;
    PropertiesAction: TAction;
    DeleteKey1: TMenuItem;
    Properties1: TMenuItem;
    SaveLayoutAction: TAction;
    LoadLayoutAction: TAction;
    SaveDialog: TSaveDialog;
    OpenDialog: TOpenDialog;
    ClearAllBtn: TAction;
    ShiftUpAction: TAction;
    ShiftDownAction: TAction;
    ShiftLeftAction: TAction;
    ShiftRightAction: TAction;
    SetFontAction: TAction;
    FontDialog: TFontDialog;
    LayoutPanel: TsPanel;
    TopPanel: TsPanel;
    RightPanel: TsPanel;
    NewKeyBtn: TsButton;
    SaveLayoutBtn: TsButton;
    LoadLayoutBtn: TsButton;
    CloseBtn: TsButton;
    ClearBtn: TsButton;
    KeyFontBtn: TsButton;
    ShiftRightBtn: TsSpeedButton;
    ShiftLeftBtn: TsSpeedButton;
    ShiftUpBtn: TsSpeedButton;
    ShiftDownBtn: TsSpeedButton;
    procedure NewKeyActionExecute(Sender: TObject);
    procedure DeleteKeyActionExecute(Sender: TObject);
    procedure PropertiesActionExecute(Sender: TObject);
    procedure SaveLayoutActionExecute(Sender: TObject);
    procedure LoadLayoutActionExecute(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ClearAllBtnExecute(Sender: TObject);
    procedure ShiftRightActionExecute(Sender: TObject);
    procedure ShiftLeftActionExecute(Sender: TObject);
    procedure ShiftDownActionExecute(Sender: TObject);
    procedure ShiftUpActionExecute(Sender: TObject);
    procedure SetFontActionExecute(Sender: TObject);
  private
    // members
    m_Keys: TKeyCollection;
    m_Drag: Boolean;
    m_MouseDown: Boolean;
    m_X0, m_Y0: Integer;
    m_HGrid, m_VGrid: Integer;
    m_DefaultFont: TFont;

    // key manipulation
    procedure NewKey;

    // helper
    function CreateKeyControl: TKeyControl;
    procedure ShowPropertiesForm(Sender: TObject);
    procedure CreateControls;
    procedure DestroyControls;
    procedure CreateGrid;

    // events assigned to control for drag and drop positioning
    procedure BtnMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure BtnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure BtnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure BtnClick(Sender: TObject);

  public
    constructor Create(AOwner: TComponent; Keys: TKeyCollection); reintroduce;

  end;

implementation

uses KeyAlPropertiesF;

{$R *.dfm}

resourcestring
  TXT_CONFIRM_DELETE = 'All keys will be deleted! Proceed?';
  TXT_CONFIRM_FONT_CHANGE = 'This will set the font for ALL existing keys! Proceed?';

{ TLayoutForm }

//-----------------------------------------------------------------------------
// TLayoutForm.Create
//-----------------------------------------------------------------------------
constructor TLayoutAlForm.Create(AOwner: TComponent; Keys: TKeyCollection);
begin

  inherited Create(AOwner);
  m_Keys := Keys;
  m_DefaultFont := TFont.Create;
  m_DefaultFont.Assign(FontDialog.Font);

  // initialize members
  m_HGrid := HORZ_BASE_UNIT;
  m_VGrid := VERT_BASE_UNIT;

  CreateGrid;
  CreateControls;
end;

//-----------------------------------------------------------------------------
// TLayoutForm.NewKey
//-----------------------------------------------------------------------------
procedure TLayoutAlForm.NewKey;
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
  K.Font.Assign(m_DefaultFont);
  K.ImageIndex:=-1;
  m_Keys.AddKey(K);

  // create key control and assign it to the Key
  Ctrl := CreateKeyControl;
  K.AssignControl(Ctrl);
end;

//-----------------------------------------------------------------------------
// TLayoutForm.NewKeyActionExecute
//-----------------------------------------------------------------------------
procedure TLayoutAlForm.NewKeyActionExecute(Sender: TObject);
begin
  NewKey;
end;

//-----------------------------------------------------------------------------
// TLayoutForm.BtnMouseMove
//-----------------------------------------------------------------------------
procedure TLayoutAlForm.BtnMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  P: TPoint;
  Ctrl: TKeyControl;
begin
  Ctrl := (Sender as TKeyControl);

  if m_MouseDown and ((Abs(X - m_X0) > 2) or (Abs(Y - m_Y0) > 2)) then
    m_Drag := True;

  if not m_Drag then
    exit;

  P := LayoutPanel.ScreenToClient(Mouse.CursorPos);
  Ctrl.Key.Left := ((P.X div m_HGrid) * m_HGrid - Ctrl.Width div 2) div HORZ_BASE_UNIT;
  Ctrl.Key.Top := ((P.Y div m_VGrid) * m_VGrid - Ctrl.Height div 2) div VERT_BASE_UNIT;
end;

//-----------------------------------------------------------------------------
// TLayoutForm.BtnMouseDown
//-----------------------------------------------------------------------------
procedure TLayoutAlForm.BtnMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    m_MouseDown := True;
    m_X0 := X;
    m_Y0 := Y;
  end;
end;

//-----------------------------------------------------------------------------
// TLayoutForm.BtnMouseUp
//-----------------------------------------------------------------------------
procedure TLayoutAlForm.BtnMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
    m_MouseDown := False;
end;

//-----------------------------------------------------------------------------
// TLayoutForm.BtnClick
//
// Show Properties Form on single click of a key
//-----------------------------------------------------------------------------
procedure TLayoutAlForm.BtnClick(Sender: TObject);
begin
  if not m_Drag then
    ShowPropertiesForm(Sender)
  else
    m_Drag := False;
end;

//-----------------------------------------------------------------------------
// TLayoutForm.CreateKeyControl
//
// Creates a new KeyControl, assign it to LayoutPanel and sets events
// for drag and drop positioning.
//-----------------------------------------------------------------------------
function TLayoutAlForm.CreateKeyControl: TKeyControl;
var
  Ctrl: TKeyControl;
begin
  Ctrl := TKeyControl.Create(LayoutPanel);
  Ctrl.Parent := LayoutPanel;

  // set events of control for drag and drop positioning
  Ctrl.OnMouseMove := BtnMouseMove;
  Ctrl.OnMouseDown := BtnMouseDown;
  Ctrl.OnMouseUp := BtnMouseUp;
  Ctrl.PopupMenu := PopupMenu;
  Ctrl.OnClick := BtnClick;
  Result := Ctrl;
end;

//-----------------------------------------------------------------------------
// TLayoutForm.DeleteKeyActionExecute
//-----------------------------------------------------------------------------
procedure TLayoutAlForm.DeleteKeyActionExecute(Sender: TObject);
begin
  // delete key from key collection and free it
  m_Keys.DeleteAndFreeKey(((((Sender as TAction).ActionComponent as TMenuItem).
                      GetParentMenu as TPopupMenu).PopupComponent as TKeyControl).Key);

  // delete Control
  ((((Sender as TAction).ActionComponent as TMenuItem).GetParentMenu as TPopupMenu).PopupComponent as TKeyControl).Free;
end;

//-----------------------------------------------------------------------------
// TLayoutForm.PropertiesActionExecute
//-----------------------------------------------------------------------------
procedure TLayoutAlForm.PropertiesActionExecute(Sender: TObject);
begin
  ShowPropertiesForm(Sender);
end;

//-----------------------------------------------------------------------------
// TLayoutForm.ShowPropertiesForm
//-----------------------------------------------------------------------------
procedure TLayoutAlForm.ShowPropertiesForm(Sender: TObject);
var
  KeyPropertiesForm: TKeyAlPropertiesForm;
  Key: TKey;
begin
  // get reference of Key
  if Sender is TAction then
    Key := ((((Sender as TAction).ActionComponent as TMenuItem).
                      GetParentMenu as TPopupMenu).PopupComponent as TKeyControl).Key
  else
    Key := (Sender as TKeyControl).Key;

  // show Key properties Form
  KeyPropertiesForm := TKeyAlPropertiesForm.Create(Self, Key);
  try
    if KeyPropertiesForm.ShowModal = mrOK then
      Key.UpdateControl;
  finally
    KeyPropertiesForm.Free;
  end;
end;

//-----------------------------------------------------------------------------
// TLayoutForm.SaveLayoutActionExecute
//
// Save current layout to file
//-----------------------------------------------------------------------------
procedure TLayoutAlForm.SaveLayoutActionExecute(Sender: TObject);
var
  FileName: String;
begin
  if not SaveDialog.Execute then
    exit;

  FileName := SaveDialog.FileName;
  m_Keys.SaveToFile(FileName);
end;

//-----------------------------------------------------------------------------
// TLayoutForm.LoadLayoutActionExecute
//
// Load layout from file. Current layout will be discarded
//-----------------------------------------------------------------------------
procedure TLayoutAlForm.LoadLayoutActionExecute(Sender: TObject);
var
  FileName: String;
begin
  if not OpenDialog.Execute then
    exit;

  FileName := OpenDialog.FileName;
  DestroyControls;
  m_Keys.LoadFromFile(FileName);
  CreateControls;
end;

//-----------------------------------------------------------------------------
// TLayoutForm.CreateControls
//
// Creates all key controls needed for keys in of m_Keys
//-----------------------------------------------------------------------------
procedure TLayoutAlForm.CreateControls;
var
  i: Integer;
  Ctrl: TKeyControl;
begin
  for i := 0 to m_Keys.Count - 1 do
  begin
    Ctrl := CreateKeyControl;
    m_Keys[i].AssignControl(Ctrl);
  end;
end;

//-----------------------------------------------------------------------------
// TLayoutForm.DestroyControls
//
// Destroys all key controls
//-----------------------------------------------------------------------------
procedure TLayoutAlForm.DestroyControls;
var
  i: Integer;
  Ctrl: TKeyControl;
begin
  for i := 0 to m_Keys.Count - 1 do
  begin
    Ctrl := m_Keys[i].Control;
    if assigned(Ctrl) then
    begin
      Ctrl.Parent := nil;
      Ctrl.Free;
      m_Keys[i].Control := nil;
    end;
  end;
end;

//-----------------------------------------------------------------------------
// TLayoutForm.FormDestroy
//-----------------------------------------------------------------------------
procedure TLayoutAlForm.FormDestroy(Sender: TObject);
begin
  DestroyControls;
  m_DefaultFont.Free;
end;

//-----------------------------------------------------------------------------
// TLayoutForm.CreateGrid
//
// Creates a grid spanning 2000x1500 pixels with a grid size of 2 x BASE_UNIT
//-----------------------------------------------------------------------------
procedure TLayoutAlForm.CreateGrid;
var
  i: Integer;
  S: TShape;
begin
  // vertical lines
  i := 0;
  while i <= 2000 do
  begin
    S := TShape.Create(LayoutPanel);
    S.Shape := stRectangle;
    S.Pen.Style := psSolid;
    S.Pen.Color := clSilver;
    S.Top := 0;
    S.Height := 1500;
    S.Left := i;
    S.Width := 1;
    i := i + 2 * HORZ_BASE_UNIT;
    S.Parent := LayoutPanel;
  end;

  // horizontal lines
  i := 0;
  while i <= 1500 do
  begin
    S := TShape.Create(LayoutPanel);
    S.Shape := stRectangle;
    S.Pen.Style := psSolid;
    S.Pen.Color := clSilver;
    S.Top := i;
    S.Height := 1;
    S.Left := 0;
    S.Width := 2000;
    i := i + 2 * VERT_BASE_UNIT;
    S.Parent := LayoutPanel;
  end;
end;

//-----------------------------------------------------------------------------
// TLayoutForm.ClearAllBtnExecute
//-----------------------------------------------------------------------------
procedure TLayoutAlForm.ClearAllBtnExecute(Sender: TObject);
begin
  if MessageDlg(TXT_CONFIRM_DELETE, mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    DestroyControls;
    m_Keys.DeleteAllKeys;
  end;
end;

//-----------------------------------------------------------------------------
// TLayoutForm.ShiftRightActionExecute
//
// Shifts all keys 1 BASE_UNIT to the right
//-----------------------------------------------------------------------------
procedure TLayoutAlForm.ShiftRightActionExecute(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to m_Keys.Count - 1 do
    m_Keys[i].Left := m_Keys[i].Left + HORZ_BASE_UNIT;
end;

//-----------------------------------------------------------------------------
// TLayoutForm.ShiftLeftActionExecute
//
// Shifts all keys 1 BASE_UNIT to the left
//-----------------------------------------------------------------------------
procedure TLayoutAlForm.ShiftLeftActionExecute(Sender: TObject);
var
  i: Integer;
begin
  // check left bounds
  for i := 0 to m_Keys.Count - 1 do
    if m_Keys[i].Left < HORZ_BASE_UNIT then
      exit;

  for i := 0 to m_Keys.Count - 1 do
    m_Keys[i].Left := m_Keys[i].Left - HORZ_BASE_UNIT;
end;

//-----------------------------------------------------------------------------
// TLayoutForm.ShiftDownActionExecute
//
// Shifts all keys 1 BASE_UNIT down
//-----------------------------------------------------------------------------
procedure TLayoutAlForm.ShiftDownActionExecute(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to m_Keys.Count - 1 do
    m_Keys[i].Top := m_Keys[i].Top + VERT_BASE_UNIT;
end;

//-----------------------------------------------------------------------------
// TLayoutForm.ShiftUpActionExecute
//
// Shifts all keys 1 BASE_UNIT up
//-----------------------------------------------------------------------------
procedure TLayoutAlForm.ShiftUpActionExecute(Sender: TObject);
var
  i: Integer;
begin
  // check upper bounds
  for i := 0 to m_Keys.Count - 1 do
    if m_Keys[i].Top < VERT_BASE_UNIT then
      exit;

  for i := 0 to m_Keys.Count - 1 do
    m_Keys[i].Top := m_Keys[i].Top - VERT_BASE_UNIT;
end;

//-----------------------------------------------------------------------------
// TLayoutForm.SetFontActionExecute
//-----------------------------------------------------------------------------
procedure TLayoutAlForm.SetFontActionExecute(Sender: TObject);
var
  i: Integer;
begin
  if MessageDlg(TXT_CONFIRM_FONT_CHANGE, mtConfirmation, [mbYes, mbNo], 0) = mrNo then
    exit;

  if FontDialog.Execute then
  begin
    m_DefaultFont.Assign(FontDialog.Font);
    for i := 0 to m_Keys.Count - 1 do
    begin
      m_Keys[i].Font.Assign(m_DefaultFont);
      m_Keys[i].UpdateControl;
    end;
  end;
end;

end.
