//-----------------------------------------------------------------------------
// Project: TKeyboard
// Unit:    Keyboard
// Author:  Martin
//
// Purpose:
// Virtual on screen Keyboard component.
// Width and Height of a key are expressed in multiples of the the base-key-unit.
// Therefore a key width of 0.5 means a key that is half a base-key-width wide.
// The base key unit is defined as a certain amount of pixel. The concept of the
// base key unit helps reducing the rounding errors when stretching a keyboard to
// a custom size. Rounding errors of one or more pixel can happen when recalculating
// the layout and should be minimized because they are visible right away in keys
// overlapping or in gaps between keys.
//
// History: 23.11.2001: created
//-----------------------------------------------------------------------------
unit Keyboard;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, ExtCtrls, StdCtrls, Graphics, Buttons;

const
  // definition of base key width and height
  HORZ_BASE_UNIT = 4;
  VERT_BASE_UNIT = 4;

  // default key width and height. This are the dimensions in base key units
  // of the keys in the layout editor
  DEFAULT_KEY_WIDTH = 8;
  DEFAULT_KEY_HEIGHT = 8;

  // define Wingdings ASCII codes for special keys
  WINGDINGS_PAGEUP = 241;
  WINGDINGS_PAGEDOWN = 242;
  WINGDINGS_LEFT = 223;
  WINGDINGS_RIGHT = 224;
  WINGDINGS_UP = 225;
  WINGDINGS_DOWN = 226;
  WINGDINGS_ENTER = 195;
  WINGDINGS = 'Wingdings';

type
  // different levels a the keyboard, a key can have a different value for each level
  TKeyLevel = (klNormal, klShift, klAltGr);

  // a normal keyboard consists of normal keys and a set of special keys
  TKeyType = (ktNormal, ktShift, ktCapsLock, ktEscape, ktAltGr, ktEnter, KtTab,
                ktBackspace, ktInsert, ktDelete, ktHome, ktEnd, ktPageUp, ktPageDown,
                ktLeft, ktRight, ktUp, ktDown, ktFunction);

  TKeyValue = String;

  // ranges for key width and height in base key units
  TKeyWidth = 1..10 * DEFAULT_KEY_WIDTH;
  TKeyHeight = 1..10 * DEFAULT_KEY_HEIGHT;

  // key caption and value types holding one caption and one value for each level
  TKeyLevelCaption = array[TKeyLevel] of String;
  TKeyLevelValue = array[TKeyLevel] of TKeyValue;

  TKeyControl = class;  // forward declaration

  // TKey represents a key in the keyboard. TKey is not the graphical representation
  // of the key but the collection of a keys properties. A TKeyControl can be
  // assigned to a TKey instance and will so be used as representation of TKey on
  // screen
  TKey = class
  private
    // property member vars
    FKeyCaption: TKeyLevelCaption;
    FKeyType: TKeyType;
    FKeyValue: TKeyLevelValue;
    FControl: TKeyControl;
    FTop: Integer;
    FLeft: Integer;
    FWidth: TKeyWidth;
    FHeight: TKeyHeight;
    FVisible: Boolean;
    FEnabled: Boolean;
    FFont: TFont;

    // property access methods
    procedure SetLeft(const Value: Integer);
    procedure SetTop(const Value: Integer);
    procedure SetWidth(const Value: TKeyWidth);
    procedure SetHeight(const Value: TKeyHeight);
    function GetKeyCaption(Level: TKeyLevel): String;
    procedure SetKeyCaption(Level: TKeyLevel; KeyCaption: String);
    function GetKeyValue(Level: TKeyLevel): TKeyValue;
    procedure SetKeyValue(Level: TKeyLevel; KeyValue: TKeyValue);
    procedure SetKeyType(const Value: TKeyType);
    procedure SetEnabled(const Value: Boolean);
    procedure SetVisible(const Value: Boolean);
    procedure SetFont(const Value: TFont);

  public
    // contruction / destruction
    constructor Create(KeyType: TKeyType);
    destructor Destroy; override;

    // properties
    property KeyCaption[Level: TKeyLevel]: String read GetKeyCaption write SetKeyCaption;
    property KeyType: TKeyType read FKeyType write SetKeyType;
    property KeyValue[Level: TKeyLevel]: TKeyValue read GetKeyValue write SetKeyValue;
    property Top: Integer read FTop write SetTop;
    property Left: Integer read FLeft write SetLeft;
    property Width: TKeyWidth read FWidth write SetWidth;
    property Height: TKeyHeight read FHeight write SetHeight;
    property Enabled: Boolean read FEnabled write SetEnabled;
    property Visible: Boolean read FVisible write SetVisible;
    property Control: TKeyControl read FControl write FControl;
    property Font: TFont read FFont write SetFont;

    // methods
    procedure AssignControl(Ctrl: TKeyControl);
    procedure UpdateControl;
  end;

  // the actual WinControl descendant that represents the key graphically on
  // the screen. The Key member references the TKey instance to which the
  // TKeyControl is assigned
  TKeyControl = class(TSpeedButton)
  public
    Key: TKey;
  end;

  // TKeyCollection is the collection of all keys of a keyboard and
  // through the coordinates of the keys also represents the screen
  // layout of the keyboard
  TKeyCollection = class(TPersistent)
  private
    // property access methods
    function GetItem(n: Integer): TKey;
    function GetCount: Integer;

  protected
    FItems: TList;

    // persistency
    procedure DefineProperties(Filer: TFiler); override;
    procedure LoadKeys(Reader: TReader);
    procedure WriteKeys(Writer: TWriter);

  public
    // properties
    property Items[n: Integer]: TKey read GetItem; default;
    property Count: Integer read GetCount;

    // construction, destruction
    constructor Create;
    destructor Destroy; override;

    // persistency
    procedure SaveToFile(FileName: String);
    procedure LoadFromFile(FileName: String);

    // manipulation
    procedure AddKey(Key: TKey);
    procedure DeleteKey(Key: TKey); overload;
    procedure DeleteKey(i: Integer); overload;
    procedure DeleteAndFreeKey(Key: TKey); overload;
    procedure DeleteAndFreeKey(i: Integer); overload;
    procedure DeleteAllKeys;
  end;

  // event types
	TKeyEvent = procedure(Sender: TObject; Key: TKey; KeyLevel: TKeyLevel; KeyValue: TKeyValue) of object;
	TShiftStateChange = procedure(Sender: TObject; ShiftOn: Boolean) of object;
	TCapsStateChange = procedure(Sender: TObject; CapsOn: Boolean) of object;
	TAltGrStateChange = procedure(Sender: TObject; AltGrOn: Boolean) of object;

  // TKeyboard component
  TKeyboard = class(TCustomPanel)
  private
    // members
    m_bShiftState: Boolean;
    m_bCapsState: Boolean;
    m_bAltGrState: Boolean;

    // property members
    FKeys: TKeyCollection;
    FCharCase: TEditCharCase;
    FBeep: Boolean;
    FLinkedControl: TWinControl;
    FHighlightColor: TColor;
    FOnCapsStateChange: TCapsStateChange;
    FOnShiftStateChange: TShiftStateChange;
    FOnAltGrStateChange: TAltGrStateChange;
    FOnKey: TKeyEvent;

    // property access methods
    procedure SetKeys(const Value: TKeyCollection);
    function GetKeys: TKeyCollection;
    procedure SetLinkedControl(const Value: TWinControl);
    function GetKeyLevel: TKeyLevel;

  protected
    // initialisation
    procedure Loaded; override;

    // screen display
    procedure Resize; override;

    // key control handling
    procedure CreateControls;
    procedure ReCreateControls;
    procedure FreeAllControls;

    // key control events
    procedure KeyClick(Sender: TObject);

    // shift, caps, altgr state handling
    procedure SetShiftState;
    procedure ClearShiftState;
    procedure ToggleCapsState;
    procedure ToggleAltGrState;

    // other
    procedure DoLinkedControlAction(Key: TKey);

  public
    // construction destruction
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // screen display
    procedure ReCreateKeys;
    procedure ResizeKeyboard;

    // layout control
    procedure LoadKeyLayout(FileName: String);
    procedure SaveKeyLayout(FileName: String);

    // key access
    function SearchKey(KeyCaption: String; KeyLevel: TKeyLevel): TKey; overload;
    function SearchKey(KeyLevel: TKeyLevel; KeyValue: TKeyValue): TKey; overload;
    function SearchKey(KeyType: TKeyType; KeyCaption: String; KeyLevel: TKeyLevel): TKey; overload;

  published
    // publish some useful properties of TCustomPanel
    property BevelOuter;
    property BevelInner;
    property BevelWidth;
    property BorderStyle;
    property BorderWidth;
    property Color;
    property Enabled;

    // new properties
		property CharCase: TEditCharCase read FCharCase write FCharCase default ecNormal;
		property BeepOnKeyPressed: Boolean read FBeep write FBeep default False;
		property LinkedControl: TWinControl read FLinkedControl write SetLinkedControl;
		property HighlightColor: TColor read FHighlightColor write FHighlightColor;
    property ShiftState: Boolean read m_bShiftState;
    property CapsState: Boolean read m_bCapsState;
    property AltGrState: Boolean read m_bAltGrState;
    property KeyLevel: TKeyLevel read GetKeyLevel;
    property Keys: TKeyCollection read GetKeys write SetKeys;

    // new event properties
		property OnKey: TKeyEvent read FOnKey write FOnKey;
		property OnShiftStateChange: TShiftStateChange read FOnShiftStateChange write FOnShiftStateChange;
		property OnCapsStateChange: TCapsStateChange read FOnCapsStateChange write FOnCapsStateChange;
		property OnAltGrStateChange: TAltGrStateChange read FOnAltGrStateChange write FOnAltGrStateChange;
  end;

//{$IFDEF Shareware}
  // function to detect if delphi is running
  function IsDelphiRunning: Boolean;
//{$ENDIF}

implementation

uses Math, Dialogs; // {$IFDEF Shareware}, Dialogs {$ENDIF};

resourcestring
  ERR_FILE_NOT_FOUND = 'File %s not found!';
  TXT_SHAREWARE_MSG = 'This program is using an unregistered version of "TKeyboard" component!!!';

{ TKey }

//-----------------------------------------------------------------------------
// TKey.Create
//-----------------------------------------------------------------------------
constructor TKey.Create(KeyType: TKeyType);
begin
  // initialize members
  FKeyType := KeyType;
  FEnabled := True;
  FVisible := True;
  FFont := TFont.Create;
  FTop := 0;
  FLeft := 0;
  FWidth := DEFAULT_KEY_WIDTH;
  FHeight := DEFAULT_KEY_HEIGHT;
end;

//-----------------------------------------------------------------------------
// TKey.Destroy
//-----------------------------------------------------------------------------
destructor TKey.Destroy;
begin
  FFont.Free;
  inherited;
end;

//-----------------------------------------------------------------------------
// TKey.GetKeyCaption
//-----------------------------------------------------------------------------
function TKey.GetKeyCaption(Level: TKeyLevel): String;
begin
  Result := FKeyCaption[Level];
end;

//-----------------------------------------------------------------------------
// TKey.GetKeyValue
//-----------------------------------------------------------------------------
function TKey.GetKeyValue(Level: TKeyLevel): TKeyValue;
begin
  Result := FKeyValue[Level];
end;

//-----------------------------------------------------------------------------
// TKey.SetKeyCaption
//-----------------------------------------------------------------------------
procedure TKey.SetKeyCaption(Level: TKeyLevel; KeyCaption: String);
begin
  FKeyCaption[Level] := KeyCaption;
end;

//-----------------------------------------------------------------------------
// TKey.SetKeyValue
//-----------------------------------------------------------------------------
procedure TKey.SetKeyValue(Level: TKeyLevel; KeyValue: TKeyValue);
begin
  FKeyValue[Level] := KeyValue;
end;

//-----------------------------------------------------------------------------
// TKey.SetKeyType
//-----------------------------------------------------------------------------
procedure TKey.SetKeyType(const Value: TKeyType);
begin
  FKeyType := Value;
  if not (fKeyType in [ktNormal]) then
  begin
    FKeyValue[klNormal] := '';
    FKeyValue[klShift] := '';
    FKeyValue[klAltGr] := '';
  end;
end;

//-----------------------------------------------------------------------------
// TKey.SetLeft
//-----------------------------------------------------------------------------
procedure TKey.SetLeft(const Value: Integer);
begin
  FLeft := Value;
  if assigned(FControl) then
    FControl.Left := FLeft * HORZ_BASE_UNIT;
end;

//-----------------------------------------------------------------------------
// TKey.SetTop
//-----------------------------------------------------------------------------
procedure TKey.SetTop(const Value: Integer);
begin
  FTop := Value;
  if assigned(FControl) then
    FControl.Top := FTop * VERT_BASE_UNIT;
end;

//-----------------------------------------------------------------------------
// TKey.AssignControl
//
// Assigns Ctrl to the key. Ctrl properties will be set to match Key properties
//-----------------------------------------------------------------------------
procedure TKey.AssignControl(Ctrl: TKeyControl);
begin
  FControl := Ctrl;
  Ctrl.Key := Self;
  Ctrl.Top := FTop * HORZ_BASE_UNIT;
  Ctrl.Left := FLeft * VERT_BASE_UNIT;
  Ctrl.Width := FWidth * HORZ_BASE_UNIT;
  Ctrl.Height := FHeight * VERT_BASE_UNIT;
  Ctrl.Enabled := FEnabled;
  Ctrl.Visible := FVisible;
  Ctrl.Font.Assign(FFont);
  Ctrl.Caption := FKeyCaption[klNormal];
end;

//-----------------------------------------------------------------------------
// TKey.SetWidth
//-----------------------------------------------------------------------------
procedure TKey.SetWidth(const Value: TKeyWidth);
begin
  FWidth := Value;
  if assigned(FControl) then
    FControl.Width := FWidth * HORZ_BASE_UNIT;
end;

//-----------------------------------------------------------------------------
// TKey.SetHeight
//-----------------------------------------------------------------------------
procedure TKey.SetHeight(const Value: TKeyHeight);
begin
  FHeight := Value;
  if assigned(FControl) then
    FControl.Height := FHeight * VERT_BASE_UNIT;
end;

//-----------------------------------------------------------------------------
// TKey.SetEnabled
//-----------------------------------------------------------------------------
procedure TKey.SetEnabled(const Value: Boolean);
begin
  FEnabled := Value;
  if assigned(FControl) then
    FControl.Enabled := FEnabled;
end;

//-----------------------------------------------------------------------------
// TKey.SetVisible
//-----------------------------------------------------------------------------
procedure TKey.SetVisible(const Value: Boolean);
begin
  FVisible := Value;
  if assigned(FControl) then
    FControl.Visible := FVisible;
end;


//-----------------------------------------------------------------------------
// TKey.UpdateControl
//
// Updates the control properties with the property values of Key
//-----------------------------------------------------------------------------
procedure TKey.UpdateControl;
begin
  if assigned(FControl) then
  begin
    FControl.Top := FTop * HORZ_BASE_UNIT;
    FControl.Left := FLeft * VERT_BASE_UNIT;
    FControl.Width := FWidth * HORZ_BASE_UNIT;
    FControl.Height := FHeight * VERT_BASE_UNIT;
    FControl.Enabled := FEnabled;
    FControl.Visible := FVisible;
    FControl.Caption := FKeyCaption[klNormal];
    FControl.Font.Assign(FFont);
  end;
end;

//-----------------------------------------------------------------------------
// TKey.SetFont
//-----------------------------------------------------------------------------
procedure TKey.SetFont(const Value: TFont);
begin
  FFont := Value;
  if assigned(FControl) then
    FControl.Font.Assign(Value);
end;


{ TKeyCollection }

//-----------------------------------------------------------------------------
// TKeyCollection.Create
//-----------------------------------------------------------------------------
constructor TKeyCollection.Create;
begin
  inherited;

  // intialize members
  FItems := TList.Create;
end;

//-----------------------------------------------------------------------------
// TKeyCollection.Destroy
//-----------------------------------------------------------------------------
destructor TKeyCollection.Destroy;
begin
  DeleteAllKeys;

  // free FItems
  FItems.Free;

  inherited;
end;

//-----------------------------------------------------------------------------
// TKeyCollection.LoadKeys
//
// Load key information from stream
//-----------------------------------------------------------------------------
procedure TKeyCollection.LoadKeys(Reader: TReader);
var
  K: TKey;
  FontStyle: Integer;
begin
  Reader.ReadListBegin;
  while not Reader.EndOfList do
  begin
    K := TKey.Create(TKeyType(Reader.ReadInteger));
    K.KeyCaption[klNormal] := Reader.ReadString;
    K.KeyCaption[klShift] := Reader.ReadString;
    K.KeyCaption[klAltGr] := Reader.ReadString;
    K.KeyValue[klNormal] := Reader.ReadString;
    K.KeyValue[klShift] := Reader.ReadString;
    K.KeyValue[klAltGr] := Reader.ReadString;
    K.Left := Reader.ReadInteger;
    K.Top := Reader.ReadInteger;
    K.Width := Reader.ReadInteger;
    K.Height := Reader.ReadInteger;
    K.Visible := Reader.ReadBoolean;
    K.Enabled := Reader.ReadBoolean;
    K.Font.Name := Reader.ReadString;
    K.Font.Size := Reader.ReadInteger;
    K.Font.Color := Reader.ReadInteger;
    FontStyle := Reader.ReadInteger;
    if (1 and FontStyle) > 0 then
      K.Font.Style := K.Font.Style + [fsBold];
    if (2 and FontStyle) > 0 then
      K.Font.Style := K.Font.Style + [fsItalic];
    if (4 and FontStyle) > 0 then
      K.Font.Style := K.Font.Style + [fsUnderline];
    if (8 and FontStyle) > 0 then
      K.Font.Style := K.Font.Style + [fsStrikeOut];
    FItems.Add(K);
  end;
  Reader.ReadListEnd;
end;

//-----------------------------------------------------------------------------
// TKeyCollection.WriteKeys
//
// Write key information to stream
//-----------------------------------------------------------------------------
procedure TKeyCollection.WriteKeys(Writer: TWriter);
var
  i: Integer;
  FontStyle: Integer;
begin
  Writer.WriteListBegin;
  for i := 0 to FItems.Count - 1 do
  begin
    Writer.WriteInteger(Ord(TKey(FItems[i]).KeyType));
    Writer.WriteString(TKey(FItems[i]).KeyCaption[klNormal]);
    Writer.WriteString(TKey(FItems[i]).KeyCaption[klShift]);
    Writer.WriteString(TKey(FItems[i]).KeyCaption[klAltGr]);
    Writer.WriteString(TKey(FItems[i]).KeyValue[klNormal]);
    Writer.WriteString(TKey(FItems[i]).KeyValue[klShift]);
    Writer.WriteString(TKey(FItems[i]).KeyValue[klAltGr]);
    Writer.WriteInteger(TKey(FItems[i]).Left);
    Writer.WriteInteger(TKey(FItems[i]).Top);
    Writer.WriteInteger(TKey(FItems[i]).Width);
    Writer.WriteInteger(TKey(FItems[i]).Height);
    Writer.WriteBoolean(TKey(FItems[i]).Visible);
    Writer.WriteBoolean(TKey(FItems[i]).Enabled);
    Writer.WriteString(TKey(FItems[i]).Font.Name);
    Writer.WriteInteger(TKey(FItems[i]).Font.Size);
    Writer.WriteInteger(TKey(FItems[i]).Font.Color);
    FontStyle := Ord(fsBold in TKey(FItems[i]).Font.Style) +
                  2 * Ord(fsItalic in TKey(FItems[i]).Font.Style) +
                  4 * Ord(fsUnderline in TKey(FItems[i]).Font.Style) +
                  8 * Ord(fsStrikeOut in TKey(FItems[i]).Font.Style);
    Writer.WriteInteger(FontStyle);
  end;
  Writer.WriteListEnd;
end;

//-----------------------------------------------------------------------------
// TKeyCollection.DefineProperties
//-----------------------------------------------------------------------------
procedure TKeyCollection.DefineProperties(Filer: TFiler);
begin
  inherited;

  Filer.DefineProperty('Items', LoadKeys, WriteKeys, True);
end;

//-----------------------------------------------------------------------------
// TKeyCollection.AddKey
//-----------------------------------------------------------------------------
procedure TKeyCollection.AddKey(Key: TKey);
begin
  FItems.Add((Key));
end;

//-----------------------------------------------------------------------------
// TKeyCollection.GetItem
//
// Returns Key with Index n
//-----------------------------------------------------------------------------
function TKeyCollection.GetItem(n: Integer): TKey;
begin
  Result := TKey(FItems[n]);
end;

//-----------------------------------------------------------------------------
// TKeyCollection.GetCount
//
// Returns number of Keys in collection
//-----------------------------------------------------------------------------
function TKeyCollection.GetCount: Integer;
begin
  Result := FItems.Count;
end;

//-----------------------------------------------------------------------------
// TKeyCollection.DeleteKey
//
// Deletes Key from the collection. Does not free the key and/or the associated
// control.
//-----------------------------------------------------------------------------
procedure TKeyCollection.DeleteKey(Key: TKey);
var
  i: Integer;
begin
  for i := 0 to FItems.Count - 1 do
    if FItems[i] = Key then
    begin
      DeleteKey(i);
      exit;
    end;
end;

//-----------------------------------------------------------------------------
// TKeyCollection.DeleteAndFreeKey
//
// Deletes Key from the collection and frees it.
//-----------------------------------------------------------------------------
procedure TKeyCollection.DeleteAndFreeKey(Key: TKey);
begin
  DeleteKey(Key);
  Key.Free;
end;

//-----------------------------------------------------------------------------
// TKeyCollection.DeleteAndFreeKey
//
// Deletes Key with index i from the collection and frees it.
//-----------------------------------------------------------------------------
procedure TKeyCollection.DeleteAndFreeKey(i: Integer);
begin
  TKey(FItems[i]).Free;
  DeleteKey(i);
end;

//-----------------------------------------------------------------------------
// TKeyCollection.DeleteKey
//
// Deletes Key with index i from the collection. Does not free the Key
//-----------------------------------------------------------------------------
procedure TKeyCollection.DeleteKey(i: Integer);
begin
  FItems.Delete(i);
end;

//-----------------------------------------------------------------------------
// TKeyCollection.LoadFromFile
//-----------------------------------------------------------------------------
procedure TKeyCollection.LoadFromFile(FileName: String);
var
  FS: TFileStream;
  Reader: TReader;
begin
  // delete all exisiting keys
  DeleteAllKeys;

	// create output stream and Reader object
	FS := TFileStream.Create(FileName, fmOpenRead);
	Reader := TReader.Create(FS, 1024);
  try
    LoadKeys(Reader);
  finally
    Reader.Free;
    FS.Free;
  end;
end;

//-----------------------------------------------------------------------------
// TKeyCollection.SaveToFile
//-----------------------------------------------------------------------------
procedure TKeyCollection.SaveToFile(FileName: String);
var
  FS: TFileStream;
  Writer: TWriter;
begin
	// create output stream and Writer object
	FS := TFileStream.Create(FileName, fmCreate or fmOpenWrite);
	Writer := TWriter.Create(FS, 1024);
  try
    WriteKeys(Writer);
  finally
    Writer.Free;
    FS.Free;
  end;
end;

//-----------------------------------------------------------------------------
// TKeyCollection.DeleteAllKeys
//-----------------------------------------------------------------------------
procedure TKeyCollection.DeleteAllKeys;
var
  i: Integer;
begin
  // delete and free all keys
  for i := 0 to FItems.Count - 1 do
    DeleteAndFreeKey(0);
end;


{ TKeyboard }

//-----------------------------------------------------------------------------
// TKeyboard.Create
//-----------------------------------------------------------------------------
constructor TKeyboard.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  // set defaults
  m_bShiftState := False;
  m_bCapsState := False;
  m_bAltGrState := False;
  FHighlightColor := clRed;
  BevelOuter := bvNone;
  BevelInner := bvNone;
  Height := 80;
  Width := 400;

  // create key collection and key controls
  FKeys := TKeyCollection.Create;
  CreateControls;

//{$IFDEF Shareware}
  // in shareware version check if Delphi is running. If not display message
  if not IsDelphiRunning then
    ShowMessage(TXT_SHAREWARE_MSG);
//{$ENDIF}
end;

//-----------------------------------------------------------------------------
// TKeyboard.Destroy
//-----------------------------------------------------------------------------
destructor TKeyboard.Destroy;
begin
  FKeys.Free;
  inherited;
end;

//-----------------------------------------------------------------------------
// TKeyboard.GetKeys
//-----------------------------------------------------------------------------
function TKeyboard.GetKeys: TKeyCollection;
begin
  Result := FKeys;
end;

//-----------------------------------------------------------------------------
// TKeyboard.SetKeys
//-----------------------------------------------------------------------------
procedure TKeyboard.SetKeys(const Value: TKeyCollection);
begin
  FKeys := Value;

  // recreate all controls and resize keyboard. This needs to be done in
  // SetKeys, because FKeys, and therefore the keyboards itself,
  // could have been changed
  ReCreateControls;
  ResizeKeyboard;
end;

//-----------------------------------------------------------------------------
// TKeyboard.ReCreateControls
//
// Deletes all controls and then recreates them. Used to update the keyboard
// after a layout change
//-----------------------------------------------------------------------------
procedure TKeyboard.ReCreateControls;
begin
  FreeAllControls;
  CreateControls;
end;

//-----------------------------------------------------------------------------
// TKeyboard.CreateControls
//
// Creates all key controls out of FKeys information
//-----------------------------------------------------------------------------
procedure TKeyboard.CreateControls;
var
  K: TKeyControl;
  i: Integer;
begin
  for i := 0 to FKeys.Count - 1 do
  begin
    K := TKeyControl.Create(Self);
    K.Parent := Self;
    K.OnClick := KeyClick;
    FKeys[i].AssignControl(K);
  end;
end;

//-----------------------------------------------------------------------------
// TKeyboard.FreeAllControls
//
// Frees all controls.
//-----------------------------------------------------------------------------
procedure TKeyboard.FreeAllControls;
var
  i: Integer;
  K: TKeyControl;
begin
  i := 0;
  while i < ComponentCount do
    if Components[i] is TKeyControl then
    begin
      K := Components[i] as TKeyControl;
      RemoveComponent(K);
      K.Free;
    end
    else
      Inc(i);
end;

//-----------------------------------------------------------------------------
// TKeyboard.Loaded
//
// Inherited procedure. After loading the component perform custom initialization
//-----------------------------------------------------------------------------
procedure TKeyboard.Loaded;
begin
  inherited;

  Caption := '';
  CreateControls;
  ResizeKeyboard;
end;

//-----------------------------------------------------------------------------
// TKeyboard.ResizeKeyboard
//
// Recalculates top and left coordinates and width and height of every key to
// fit the keyboard into the component area
//-----------------------------------------------------------------------------
procedure TKeyboard.ResizeKeyboard;
var
  MinX, MinY, MaxX, MaxY: Integer;
  KeyAreaWidthBU, KeyAreaHeightBU: Integer;
  CompAreaWidthBU, CompAreaHeightBU: Integer;
  StretchX, StretchY: Integer;
  i: Integer;
begin
  MinX := 10000;
  MinY := 10000;
  MaxX := 0;
  MaxY := 0;

  // get coordinates of key area
  for i := 0 to FKeys.Count - 1 do
  begin
    MinX := Min(MinX, FKeys[i].Left);
    MinY := Min(MinY, FKeys[i].Top);
    MaxX := Max(MaxX, FKeys[i].Left + FKeys[i].Width);
    MaxY := Max(MaxY, FKeys[i].Top + FKeys[i].Height);
  end;

  // calculate how many whole base unit fit into the key area
  KeyAreaWidthBU := (MaxX - MinX);
  KeyAreaHeightBU := (MaxY - MinY);

  // calculate how many whole key base units fit into actual component area
  // leave 5 base units horizontal and 2 base unit vertical of free space to
  // compensate for float to integer projection errors and avoid keys being
  // partially out of the keyboard panel
  CompAreaWidthBU := ClientWidth div HORZ_BASE_UNIT - 4;
  CompAreaHeightBU := ClientHeight div VERT_BASE_UNIT - 2;

  // calculate stretch factor
  StretchX := trunc((CompAreaWidthBU - KeyAreaWidthBU) / (KeyAreaWidthBU / DEFAULT_KEY_WIDTH) * HORZ_BASE_UNIT);
  StretchY := trunc((CompAreaHeightBU - KeyAreaHeightBU) / (KeyAreaHeightBU / DEFAULT_KEY_HEIGHT) * HORZ_BASE_UNIT);

  // reposition and resize keys
  for i := 0 to FKeys.Count - 1 do
  begin
    FKeys[i].Control.Left := (FKeys[i].Left - MinX) * HORZ_BASE_UNIT + StretchX * (FKeys[i].Left - MinX) div DEFAULT_KEY_WIDTH;
    FKeys[i].Control.Width := FKeys[i].Width * HORZ_BASE_UNIT + round((FKeys[i].Width / DEFAULT_KEY_WIDTH) * StretchX);
    FKeys[i].Control.Top := (FKeys[i].Top - MinY) * VERT_BASE_UNIT + StretchY * (FKeys[i].Top - MinY) div DEFAULT_KEY_HEIGHT;
    FKeys[i].Control.Height := FKeys[i].Height * VERT_BASE_UNIT + round((FKeys[i].Height / DEFAULT_KEY_HEIGHT) * StretchY);
  end;
end;

//-----------------------------------------------------------------------------
// TKeyboard.Resize
//-----------------------------------------------------------------------------
procedure TKeyboard.Resize;
begin
  inherited;

  ResizeKeyboard;
end;

//-----------------------------------------------------------------------------
// TKeyboard.SetLinkedControl
//-----------------------------------------------------------------------------
procedure TKeyboard.SetLinkedControl(const Value: TWinControl);
begin
  if Value = Self then
    FLinkedControl := nil
  else
    FLinkedControl := Value;
end;

//-----------------------------------------------------------------------------
// TKeyboard.KeyClick
//
// Triggered when a key is pressed
//-----------------------------------------------------------------------------
procedure TKeyboard.KeyClick(Sender: TObject);
var
  K: TKeyControl;
  V: TKeyValue;
begin
  K := (Sender as TKeyControl);

  // beep
  if FBeep then
    Beep;

  // toggle caps state if caps key pressed
  if K.Key.KeyType = ktCapsLock then
  begin
    ToggleCapsState;
    exit;
  end;

  // set shift state if shift key pressed
  if K.Key.KeyType = ktShift then
  begin
    // if we are in caps state then clear it instead of setting shift state
    if m_bCapsState then
      ToggleCapsState
    else
      SetShiftState;
    exit;
  end;

  // toggle AlrGr state if AltGr key pressed
  if K.Key.KeyType = ktAltGr then
  begin
    ToggleAltGrState;
    exit;
  end;

  // consider CharCase property
  if FCharCase = ecUpperCase then
    V := UpperCase(K.Key.KeyValue[KeyLevel])
  else if FCharCase = ecLowerCase then
    V := LowerCase(K.Key.KeyValue[KeyLevel])
  else
    V := K.Key.KeyValue[KeyLevel];

  // fire OnKey event
  if assigned(FOnKey) then
    FOnKey(Self, K.Key, KeyLevel, V);

  // perform action in linked control
  DoLinkedControlAction(K.Key);

  // if any key is pressed in shift state and not in caps state then clear shift state again
  if m_bShiftState and not m_bCapsState then
    ClearShiftState;

  // if any key is pressed in AltGr state then clear AltGr state again
  if m_bAltGrState then
    ToggleAltGrState;
end;

//-----------------------------------------------------------------------------
// TKeyboard.SetShiftState
//-----------------------------------------------------------------------------
procedure TKeyboard.SetShiftState;
var
  i: Integer;
begin
  // if we are in AltGr state then clear it
  if m_bAltGrState then
    ToggleAltGrState;

  // if we are already in shift state then clear it
  if m_bShiftState then
    ClearShiftState
  else
  begin
    for i := 0 to FKeys.Count - 1 do
    begin
      // if a key is of type Normal, display shift caption
      if FKeys[i].KeyType = ktNormal then
        FKeys[i].Control.Caption := FKeys[i].KeyCaption[klShift];

      // if a key is a shift key highlight it
      if (FKeys[i].KeyType = ktShift) and not m_bCapsState then
        FKeys[i].Control.Font.Color := FHighlightColor;

      // if a key is a caps key highlight it
      if (FKeys[i].KeyType = ktCapsLock) and m_bCapsState then
        FKeys[i].Control.Font.Color := FHighlightColor;
    end;

    m_bShiftState := True;

    // fire ShiftStateChange event
    if assigned(FOnShiftStateChange) then
      FOnShiftStateChange(Self, m_bShiftState);
  end;
end;

//-----------------------------------------------------------------------------
// TKeyboard.ClearShiftState
//-----------------------------------------------------------------------------
procedure TKeyboard.ClearShiftState;
var
  i: Integer;
begin
  for i := 0 to FKeys.Count - 1 do
  begin
    // set key captions back to normal
    if FKeys[i].KeyType = ktNormal then
      FKeys[i].Control.Caption := FKeys[i].KeyCaption[klNormal];

    // if a key is a shift or caps key then clear highlighting
    if FKeys[i].KeyType in [ktShift, ktCapsLock] then
      FKeys[i].Control.Font.Color := FKeys[i].Font.Color;
  end;
  m_bShiftState := False;
  m_bCapsState := False;

  // fire ShiftStateChange event
  if assigned(FOnShiftStateChange) then
    FOnShiftStateChange(Self, m_bShiftState);
end;

//-----------------------------------------------------------------------------
// TKeyboard.ToggleCapsState
//-----------------------------------------------------------------------------
procedure TKeyboard.ToggleCapsState;
begin
  if m_bShiftState then
    ClearShiftState
  else
  begin
    m_bCapsState := True;
    SetShiftState;
  end;

  // fire CapsStateChange event
  if assigned(FOnCapsStateChange) then
    FOnCapsStateChange(Self, m_bCapsState);
end;

//-----------------------------------------------------------------------------
// TKeyboard.ToggleAltGrState
//-----------------------------------------------------------------------------
procedure TKeyboard.ToggleAltGrState;
var
  State: TKeyLevel;
  i: Integer;
begin
  // if we are in Shift state then clear it
  if m_bShiftState then
  begin
    if m_bCapsState and assigned(FOnCapsStateChange) then
      FOnCapsStateChange(Self, m_bCapsState);
    ClearShiftState;
  end;

  if m_bAltGrState then
    State := klNormal
  else
    State := klAltGr;

  for i := 0 to FKeys.Count - 1 do
  begin
    // toggle key captions
    if FKeys[i].KeyType = ktNormal then
      FKeys[i].Control.Caption := FKeys[i].KeyCaption[State];

    // if a key is a AltGr key then toggle highlighting
    if FKeys[i].KeyType = ktAltGr then
      if State = klNormal then
        FKeys[i].Control.Font.Color := FKeys[i].Font.Color
      else
        FKeys[i].Control.Font.Color := FHighlightColor;
  end;

  m_bAltGrState := not m_bAltGrState;

  // fire AltGrStateChange event
  if assigned(FOnAltGrStateChange) then
    FOnAltGrStateChange(Self, m_bAltGrState);
end;

//-----------------------------------------------------------------------------
// TKeyboard.GetKeyLevel
//
// Returns the current level of TKeyboard
//-----------------------------------------------------------------------------
function TKeyboard.GetKeyLevel: TKeyLevel;
var
  Level: TKeyLevel;
begin
  // calculate key level
  if m_bShiftState or m_bCapsState then
    Level := klShift
  else if m_bAltGrState then
    Level := klAltGr
  else
    Level := klNormal;
  Result := Level;
end;

//-----------------------------------------------------------------------------
// TKeyboard.DoLinkedControlAction
//
// Performs an action on the linked control depending on the key pressed
//-----------------------------------------------------------------------------
procedure TKeyboard.DoLinkedControlAction(Key: TKey);
var
  i: Integer;
begin
  if not assigned(FLinkedControl) then
    exit;

  case Key.KeyType of
    ktEscape:     FLinkedControl.Perform(WM_KEYDOWN, VK_ESCAPE, 0);
    ktEnter:      FLinkedControl.Perform(WM_CHAR, VK_RETURN, 0);
    KtTab:        FLinkedControl.Perform(WM_CHAR, VK_TAB, 0);
    ktBackspace:  FLinkedControl.Perform(WM_CHAR, VK_BACK, 0);
    ktInsert:     FLinkedControl.Perform(WM_KEYDOWN, VK_INSERT, 0);
    ktDelete:     FLinkedControl.Perform(WM_KEYDOWN, VK_DELETE, 0);
    ktHome:       FLinkedControl.Perform(WM_KEYDOWN, VK_HOME, 0);
    ktEnd:        FLinkedControl.Perform(WM_KEYDOWN, VK_END, 0);
    ktPageUp:     FLinkedControl.Perform(WM_KEYDOWN, VK_PRIOR, 0);
    ktPageDown:   FLinkedControl.Perform(WM_KEYDOWN, VK_NEXT, 0);
    ktLeft:       FLinkedControl.Perform(WM_KEYDOWN, VK_LEFT, 0);
    ktRight:      FLinkedControl.Perform(WM_KEYDOWN, VK_RIGHT, 0);
    ktUp:         FLinkedControl.Perform(WM_KEYDOWN, VK_UP, 0);
    ktDown:       FLinkedControl.Perform(WM_KEYDOWN, VK_DOWN, 0);
    ktNormal:     begin
                    for i := 1 to Length(Key.KeyValue[KeyLevel]) do
                      FLinkedControl.Perform(WM_CHAR, Ord(Key.KeyValue[KeyLevel][i]), 0);
                  end;
  end;
end;

//-----------------------------------------------------------------------------
// TKeyboard.SearchKey
//
// Search for a key with caption KeyCaption on level KeyLevel. Returns
// a reference to the key if successful, nil otherwhise.
// If more than one key on the keyboard has KeyCaption as caption only
// the first key will be returned.
//-----------------------------------------------------------------------------
function TKeyboard.SearchKey(KeyCaption: String; KeyLevel: TKeyLevel): TKey;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to FKeys.Count - 1 do
    if FKeys[i].KeyCaption[KeyLevel] = KeyCaption then
    begin
      Result := FKeys[i];
      break;
    end;
end;

//-----------------------------------------------------------------------------
// TKeyboard.SearchKey
//
// Search for a key with value KeyValue on level KeyLevel. Returns
// a reference to the key if successful, nil otherwhise.
// If more than one key on the keyboard has KeyValue as value only
// the first key will be returned.
//-----------------------------------------------------------------------------
function TKeyboard.SearchKey(KeyLevel: TKeyLevel; KeyValue: TKeyValue): TKey;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to FKeys.Count - 1 do
    if FKeys[i].KeyValue[KeyLevel] = KeyValue then
    begin
      Result := FKeys[i];
      break;
    end;
end;

//-----------------------------------------------------------------------------
// TKeyboard.SearchKey
//
// Search for a key of type KeyType with caption KeyCaption. Returns
// a reference to the key if successful, nil otherwhise.
// If more than one key of type KeyType on the keyboard has KeyCaption as caption,
// only the first key will be returned. If KeyCaption is empty then the search
// is performed only on the type and the first key with a matching type
// will be returned.
//-----------------------------------------------------------------------------
function TKeyboard.SearchKey(KeyType: TKeyType; KeyCaption: String;  KeyLevel: TKeyLevel): TKey;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to FKeys.Count - 1 do
    if FKeys[i].KeyType = KeyType then
      if KeyCaption = '' then
      begin
        Result := FKeys[i];
        break;
      end
      else if FKeys[i].KeyCaption[KeyLevel] = KeyCaption then
      begin
        Result := FKeys[i];
        break;
      end
end;

//-----------------------------------------------------------------------------
// TKeyboard.ReCreateKeys
//
// Deletes and recreates all keys. Call when a key has been deleted or added
// to the keyboard.
//-----------------------------------------------------------------------------
procedure TKeyboard.ReCreateKeys;
begin
  ReCreateControls;
end;

//-----------------------------------------------------------------------------
// TKeyboard.LoadKeyLayout
//
// Loads a new key layout from a file.
//-----------------------------------------------------------------------------
procedure TKeyboard.LoadKeyLayout(FileName: String);
begin
  if not FileExists(FileName) then
    raise Exception.Create(Format(ERR_FILE_NOT_FOUND, [FileName]));

  FreeAllControls;
  FKeys.DeleteAllKeys;
  FKeys.LoadFromFile(FileName);
  CreateControls;
end;

//-----------------------------------------------------------------------------
// TKeyboard.SaveKeyLayout
//-----------------------------------------------------------------------------
procedure TKeyboard.SaveKeyLayout(FileName: String);
begin
  FKeys.SaveToFile(FileName);
end;


//-----------------------------------------------------------------------------
// IsDelphiRunning
//-----------------------------------------------------------------------------
//{$IFDEF Shareware}
function IsDelphiRunning: Boolean;
begin
  Result := (FindWindow('TApplication', 'Delphi 7') <> 0) or  // gotta think ahead
            (FindWindow('TApplication', 'Delphi 6') <> 0) or
            (FindWindow('TApplication', 'Delphi 5') <> 0) or
            (FindWindow('TApplication', 'Delphi 4') <> 0) or
            (FindWindow('TApplication', 'Delphi 3') <> 0) or
            (FindWindow('TApplication', 'Delphi 2') <> 0);
end;
//{$ENDIF}

end.
