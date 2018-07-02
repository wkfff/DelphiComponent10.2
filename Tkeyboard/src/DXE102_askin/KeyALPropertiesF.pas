//-----------------------------------------------------------------------------
// Project: TKeyboard
// Unit:    KeyPropertiesF
// Author:  Martin
//
// Purpose:
// Dialog to set Key properties
//
// History: 26.11.2001: created
//-----------------------------------------------------------------------------
unit KeyALPropertiesF;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, KeyboardAL, Vcl.Samples.Spin;

type
  TKeyAlPropertiesForm = class(TForm)
    OKBtn: TButton;
    CancelBtn: TButton;
    KeyTypeRadioGroup: TRadioGroup;
    KeyCaptionsGroupBox: TGroupBox;
    NormalCaptionEdit: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    ShiftCaptionEdit: TEdit;
    AltGrCaptionEdit: TEdit;
    KeyValuesGroupBox: TGroupBox;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    NormalValueEdit: TEdit;
    ShiftValueEdit: TEdit;
    AltGrValueEdit: TEdit;
    KeyWidthComboBox: TComboBox;
    Label7: TLabel;
    Label8: TLabel;
    KeyHeightComboBox: TComboBox;
    FontBtn: TButton;
    FontDialog: TFontDialog;
    FontLabel: TLabel;
    SpinEdit1: TSpinEdit;
    Label9: TLabel;
    procedure KeyTypeRadioGroupClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FontBtnClick(Sender: TObject);
  private
    m_Key: TKey;
    m_Load: Boolean;

    procedure UpdateControls;
    procedure LoadProperties;
    procedure SaveProperties;

  public
    constructor Create(AOwner: TComponent; Key: TKey); reintroduce;

  end;


implementation

{$R *.dfm}

{ TKeyPropertiesForm }

//-----------------------------------------------------------------------------
// TKeyPropertiesForm.Create
//-----------------------------------------------------------------------------
constructor TKeyAlPropertiesForm.Create(AOwner: TComponent; Key: TKey);
begin
  inherited Create(AOwner);
  m_Key := Key;
  m_Load := False;

  LoadProperties;
  UpdateControls;

  FontLabel.Caption := m_Key.Control.Font.Name + ' ' + IntToStr(m_Key.Control.Font.Size);
end;

//-----------------------------------------------------------------------------
// TKeyPropertiesForm.UpdateControls
//
// Sets, resets, enables and disables controls
//-----------------------------------------------------------------------------
procedure TKeyAlPropertiesForm.UpdateControls;
begin
  // enable/disable controls
  NormalValueEdit.Enabled := (KeyTypeRadioGroup.ItemIndex = 0);
  ShiftValueEdit.Enabled := (KeyTypeRadioGroup.ItemIndex = 0);
  AltGrValueEdit.Enabled := (KeyTypeRadioGroup.ItemIndex = 0);

  ShiftCaptionEdit.Enabled := (KeyTypeRadioGroup.ItemIndex = 0);
  AltGrCaptionEdit.Enabled := (KeyTypeRadioGroup.ItemIndex = 0);

  // set color of disabled controls to gray
  if NormalValueEdit.Enabled then
    NormalValueEdit.Color := clWindow
  else
    NormalValueEdit.Color := clBtnFace;
  if ShiftValueEdit.Enabled then
    ShiftValueEdit.Color := clWindow
  else
    ShiftValueEdit.Color := clBtnFace;
  if AltGrValueEdit.Enabled then
    AltGrValueEdit.Color := clWindow
  else
    AltGrValueEdit.Color := clBtnFace;
  if ShiftCaptionEdit.Enabled then
    ShiftCaptionEdit.Color := clWindow
  else
    ShiftCaptionEdit.Color := clBtnFace;
  if AltGrCaptionEdit.Enabled then
    AltGrCaptionEdit.Color := clWindow
  else
    AltGrCaptionEdit.Color := clBtnFace;


  // clear not enabled controls
  if (KeyTypeRadioGroup.ItemIndex > 0) and
      (KeyTypeRadioGroup.ItemIndex < Ord(ktFunction)) then
  begin
    ShiftCaptionEdit.Text := '';
    AltGrCaptionEdit.Text := '';
  end;
end;

//-----------------------------------------------------------------------------
// TKeyPropertiesForm.KeyTypeRadioGroupClick
//-----------------------------------------------------------------------------
procedure TKeyAlPropertiesForm.KeyTypeRadioGroupClick(Sender: TObject);
begin
  UpdateControls;

  // don't change anything during load of properties
  if m_Load then
    exit;

  // set special Wingdings arrow characters for navigation keys
  if TKeyType(KeyTypeRadioGroup.ItemIndex) in [ktPageUp, ktPageDown, ktUp, KtDown, ktLeft, ktRight, ktEnter] then
  begin
    NormalCaptionEdit.Font.Name := WINGDINGS;
    m_Key.Font.Name := WINGDINGS;
  end;
  if KeyTypeRadioGroup.ItemIndex = Ord(ktPageUp) then
    NormalCaptionEdit.Text := Char(WINGDINGS_PAGEUP);
  if KeyTypeRadioGroup.ItemIndex = Ord(ktPageDown) then
    NormalCaptionEdit.Text := Char(WINGDINGS_PAGEDOWN);
  if KeyTypeRadioGroup.ItemIndex = Ord(ktUp) then
    NormalCaptionEdit.Text := Char(WINGDINGS_UP);
  if KeyTypeRadioGroup.ItemIndex = Ord(ktDown) then
    NormalCaptionEdit.Text := Char(WINGDINGS_DOWN);
  if KeyTypeRadioGroup.ItemIndex = Ord(ktLeft) then
    NormalCaptionEdit.Text := Char(WINGDINGS_LEFT);
  if KeyTypeRadioGroup.ItemIndex = Ord(ktRight) then
    NormalCaptionEdit.Text := Char(WINGDINGS_RIGHT);
  if KeyTypeRadioGroup.ItemIndex = Ord(ktEnter) then
    NormalCaptionEdit.Text := Char(WINGDINGS_ENTER);
end;

//-----------------------------------------------------------------------------
// TKeyPropertiesForm.FormClose
//-----------------------------------------------------------------------------
procedure TKeyAlPropertiesForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  if ModalResult = mrCancel then
    exit;

  SaveProperties;
end;

//-----------------------------------------------------------------------------
// TKeyPropertiesForm.SaveProperties
//
// Writes values from Controls into m_Key properties
//-----------------------------------------------------------------------------
procedure TKeyAlPropertiesForm.SaveProperties;
begin
  // set type
  m_Key.KeyType := TKeyType(KeyTypeRadioGroup.ItemIndex);

  // set caption
  m_Key.KeyCaption[klNormal] := NormalCaptionEdit.Text;
  m_Key.KeyCaption[klShift] := ShiftCaptionEdit.Text;
  m_Key.KeyCaption[klAltGr] := AltGrCaptionEdit.Text;

  // set value
  m_Key.KeyValue[klNormal] := NormalValueEdit.Text;
  if m_Key.KeyType in [ktNormal, ktFunction] then
  begin
    m_Key.KeyValue[klShift] := ShiftValueEdit.Text;
    m_Key.KeyValue[klAltGr] := AltGrValueEdit.Text;
  end;

  // set width and height
  m_Key.Width := (KeyWidthComboBox.ItemIndex + 1) * DEFAULT_KEY_WIDTH div 2;
  m_Key.Height := (KeyHeightComboBox.ItemIndex + 1) * DEFAULT_KEY_HEIGHT div 2;

  //imagesindex
  m_key.ImageIndex:=SpinEdit1.Value;
end;

//-----------------------------------------------------------------------------
// TKeyPropertiesForm.LoadProperties
//
// Load Key properties into Controls
//-----------------------------------------------------------------------------
procedure TKeyAlPropertiesForm.LoadProperties;
begin
  m_Load := True;

  try
    // load type
    KeyTypeRadioGroup.ItemIndex := Ord(m_Key.KeyType);

    // load caption
    NormalCaptionEdit.Text := m_Key.KeyCaption[klNormal];
    ShiftCaptionEdit.Text := m_Key.KeyCaption[klShift];
    AltGrCaptionEdit.Text := m_Key.KeyCaption[klAltGr];

    // load value
    NormalValueEdit.Text := m_Key.KeyValue[klNormal];
    ShiftValueEdit.Text := m_Key.KeyValue[klShift];
    AltGrValueEdit.Text := m_Key.KeyValue[klAltGr];

    // load width and height
    KeyWidthComboBox.ItemIndex := m_Key.Width * 2 div DEFAULT_KEY_WIDTH - 1;
    KeyHeightComboBox.ItemIndex := m_Key.Height * 2 div DEFAULT_KEY_HEIGHT - 1;

    // set font names
    NormalCaptionEdit.Font.Name := m_Key.Font.Name;
    ShiftCaptionEdit.Font.Name := m_Key.Font.Name;
    AltGrCaptionEdit.Font.Name := m_Key.Font.Name;

  //imagesindex
    SpinEdit1.Value:=m_key.ImageIndex;
  finally
    m_Load := False;
  end;
end;

//-----------------------------------------------------------------------------
// TKeyPropertiesForm.FontBtnClick
//-----------------------------------------------------------------------------
procedure TKeyAlPropertiesForm.FontBtnClick(Sender: TObject);
begin
  FontDialog.Font.Assign(m_Key.Font);
  if FontDialog.Execute then
  begin
    m_Key.Font.Assign(FontDialog.Font);
    NormalCaptionEdit.Font.Name := m_Key.Font.Name;
    ShiftCaptionEdit.Font.Name := m_Key.Font.Name;
    AltGrCaptionEdit.Font.Name := m_Key.Font.Name;
  end;
end;

end.
