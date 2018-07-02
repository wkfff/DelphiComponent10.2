unit acCharListEditor;
{$I sDefs.inc}

interface

uses
  {$IFNDEF DELPHI5} Types, {$ENDIF}
  {$IFDEF  DELPHI_XE2} UITypes,    {$ENDIF}
  {$IFNDEF DELPHI6UP} acD5Ctrls, {$ENDIF}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, ComCtrls, ExtCtrls, StdCtrls, ImgList, Buttons, Mask,
  acAlphaImageList, sListView, sPanel, sPageControl, sButton, sComboBox, sEdit, sComboBoxes,  sGroupBox, sCheckBox, sSkinProvider,
  sConst, sSpeedButton, sFontCtrls, sListBox, sMaskEdit, sCustomComboEdit, sToolEdit, sCheckListBox, sLabel, sStatusBar, sDialogs, sBitBtn,
  sTrackBar;


type
  TFormCharListEditor = class(TForm)
    sCharImageList1: TsCharImageList;
    sPanel1: TsPanel;
    sPanel2: TsPanel;
    sSkinProvider1: TsSkinProvider;
    sPageControl1: TsPageControl;
    sTabSheet1: TsTabSheet;
    sTabSheet2: TsTabSheet;
    sListBox1: TsListBox;
    sComboBox1: TsComboBox;
    sListBox2: TsListBox;
    sStatusBar1: TsStatusBar;
    sOpenDialog1: TsOpenDialog;
    sPanel4: TsPanel;
    sPanel5: TsPanel;
    sSpeedButton5: TsSpeedButton;
    sSpeedButton6: TsSpeedButton;
    sSpeedButton7: TsSpeedButton;
    sPanel3: TsPanel;
    sPanel6: TsPanel;
    sSpeedButton1: TsSpeedButton;
    sSpeedButton2: TsSpeedButton;
    sSpeedButton4: TsSpeedButton;
    sSpeedButton3: TsSpeedButton;
    sListView1: TsListView;
    sPanel7: TsPanel;
    sGroupBox1: TsGroupBox;
    sLabel1: TsLabel;
    sLabel2: TsLabel;
    sLabel3: TsLabel;
    sLabel4: TsLabel;
    sTrackBar1: TsTrackBar;
    sTrackBar2: TsTrackBar;
    sColorBox1: TsColorBox;
    sButton1: TsBitBtn;
    sButton2: TsBitBtn;
    procedure FormShow(Sender: TObject);
    procedure sButton1Click(Sender: TObject);
    procedure sListBox1Click(Sender: TObject);
    procedure sListView1Click(Sender: TObject);
    procedure sComboBox1Change(Sender: TObject);
    procedure sSpeedButton2Click(Sender: TObject);
    procedure sSpeedButton4Click(Sender: TObject);
    procedure sSpeedButton3MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure sSpeedButton1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure sPageControl1Change(Sender: TObject);
    procedure sSpeedButton5Click(Sender: TObject);
    procedure sSpeedButton6Click(Sender: TObject);
    procedure sListBox2Click(Sender: TObject);
    function CanAddChar: boolean;
    procedure CheckScroll(Sender: TObject);
    procedure sListView1DragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure sListView1DragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure sSpeedButton7Click(Sender: TObject);
    procedure sListBox2BeforeItemDraw(aDC: HDC; aIndex: Integer; var aRect: TRect);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure sSpeedButton1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure sTrackBar1Change(Sender: TObject);
    procedure sTrackBar2Change(Sender: TObject);
    procedure sColorBox1Change(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  protected
    ControlsUpdating: boolean;
    SourceList: TsCharImageList;
    procedure InitControls;
    procedure RefillListView;
    function FontIsEmbedded(AIndex: integer): boolean;
    procedure AddNewFont(FileName: string);
    procedure DeleteFont(Ndx: integer);
    procedure UpdateBtnsListBox2;
    procedure AddAwesome(AddList: boolean = True);
    procedure ShowCharMap(Sender: TsSpeedButton);
  public
    procedure AddNewGlyph(aChar: Word; CharsetNdx: integer = -1; FontNdx: integer = -1);
    procedure InitFromImgList(ImgList: TsCharImageList);
  end;


const
  iEMBEDDED = 19;
  arCharsets: array [0 .. iEMBEDDED] of Byte = (ANSI_CHARSET, DEFAULT_CHARSET, SYMBOL_CHARSET, SHIFTJIS_CHARSET, HANGEUL_CHARSET, GB2312_CHARSET, CHINESEBIG5_CHARSET, OEM_CHARSET, JOHAB_CHARSET,
    HEBREW_CHARSET, ARABIC_CHARSET, GREEK_CHARSET, TURKISH_CHARSET, VIETNAMESE_CHARSET, THAI_CHARSET, EASTEUROPE_CHARSET, RUSSIAN_CHARSET, MAC_CHARSET, BALTIC_CHARSET, DEFAULT_CHARSET);

var
  FormCharListEditor: TFormCharListEditor;
  ScrollTimer: TTimer = nil;

function EnumFontCallback(lpelfe: PLogFont; lpntme: PNewTextMetricEX; FontType: DWORD; lp: LPARAM): Integer; stdcall;

implementation

uses math,
  {$IFDEF TNTUNICODE}TntComCtrls, {$ENDIF}
  acntUtils, acCharMap, acPopupController, sVCLUtils, acFontStore, acDesignData, CommCtrl, sGraphUtils;

{$R *.dfm}

procedure StartScroll;
begin
  if ScrollTimer = nil then begin
    ScrollTimer := TTimer.Create(nil);
    ScrollTimer.OnTimer := FormCharListEditor.CheckScroll;
    ScrollTimer.Interval := 10;
  end;
end;


procedure EndScroll;
begin
  if ScrollTimer <> nil then
    FreeAndNil(ScrollTimer);
end;


procedure TFormCharListEditor.AddAwesome(AddList: boolean = True);
begin
{$IFNDEF NOFONTRES}
  if sCharImageList1.EmbeddedFonts.Count = 0 then begin
    if AddList then
      sListBox2.Items.Add(s_FontAwesome);

    with TacEmbeddedFont(sCharImageList1.EmbeddedFonts.Add) do
      FontName := s_FontAwesome;
  end;
{$ENDIF}
end;


procedure TFormCharListEditor.AddNewFont(FileName: string);
var
  s: string;
begin
  s := acGetFontName(FileName);
  if sCharImageList1.AddEmbeddedFont(FileName, s) then
    sListBox2.Items.Add(s);
end;


procedure TFormCharListEditor.AddNewGlyph(aChar: Word; CharsetNdx: integer = -1; FontNdx: integer = -1);
var
  cid: TacCharItemData;
  li: {$IFDEF TNTUNICODE}TTntListItem{$ELSE}TListItem{$ENDIF};
begin
  ControlsUpdating := True;
  if CharsetNdx = iEMBEDDED then begin
    cid.Charset := DEFAULT_CHARSET;
    if FontNdx <> -1 then begin
      sListBox1.ItemIndex := -1;
      sListBox2.ItemIndex := FontNdx;
      cid.FontName := sListBox2.Items[sListBox2.ItemIndex];
    end
    else
      cid.FontName := '';
  end
  else begin
    if (CharsetNdx <> -1) and (sComboBox1.ItemIndex <> CharsetNdx) then begin
      sComboBox1.ItemIndex := CharsetNdx;
      sComboBox1Change(sComboBox1);
    end;
    cid.Charset := arCharSets[sComboBox1.ItemIndex];
    if FontNdx <> -1 then begin
      sListBox2.ItemIndex := -1;
      sListBox1.ItemIndex := FontNdx;
      cid.FontName := sListBox1.Items[FontNdx];
    end
    else
      cid.FontName := '';
  end;

  cid.Orientation := 0;
  cid.Pitch := fpDefault;
  cid.ScalingFactor := GetDefScaling(cid.Charset, cid.FontName);
  cid.OffsetY := 0;
  cid.Style := [];
  cid.Char := aChar;
  cid.Color := clNone;

  li := sListView1.Items.Add;
  li.ImageIndex := sCharImageList1.AddItem(cid);
  li.Caption := IntToStr(li.Index);

  sListView1.Selected := nil;
  sListView1.Selected := li;
  ControlsUpdating := False;
  sListView1Click(sListView1);
end;


function TFormCharListEditor.CanAddChar: boolean;
begin
  Result := (sListBox1.ItemIndex >= 0) or (sListBox2.ItemIndex >= 0)
end;


const
  scBorder = 8;

procedure DoScroll(X, Y: integer);
var
  dx: integer;
begin
  if X > FormCharListEditor.sListView1.Width - scBorder then
    dx := 10
  else
    if X < scBorder then
      dx := -10
    else
      dx := 0;

  if dx <> 0 then begin
    FormCharListEditor.sListView1.Scroll(dx, 0);
    FormCharListEditor.sListView1.Invalidate;
  end;
end;


procedure TFormCharListEditor.CheckScroll(Sender: TObject);
var
  P: TPoint;
begin
  if (Mouse.Capture <> 0) and (ScrollTimer <> nil) and not (csDestroying in ScrollTimer.ComponentState) and (FormCharListEditor <> nil) then begin
    ScrollTimer.Enabled := False;
    P := sListView1.ScreenToClient(acMousePos);
    DoScroll(P.X, P.Y);
    if ScrollTimer <> nil then
      ScrollTimer.Enabled := True;
  end;
end;


procedure EnableCtrls(Ctrl: TControl; Data: Integer);
begin
  if Ctrl.Tag = 1 then
    Ctrl.Enabled := boolean(Data);
end;


function TFormCharListEditor.FontIsEmbedded(AIndex: integer): boolean;
begin
  Result := False;
end;


procedure TFormCharListEditor.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if FormCharMap <> nil then
    FreeAndNil(FormCharMap);
end;


procedure TFormCharListEditor.FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  if (FormCharMap <> nil) and IsWindowVisible(FormCharMap.Handle) then
    if WheelDelta > 0 then
      FormCharMap.Grid.TopRow := max(0, FormCharMap.Grid.TopRow - 1)
    else
      FormCharMap.Grid.TopRow := min(FormCharMap.Grid.RowCount - FormCharMap.Grid.ClientHeight div FormCharMap.Grid.DefaultRowHeight, FormCharMap.Grid.TopRow + 1);
end;


procedure TFormCharListEditor.FormShow(Sender: TObject);
var
  i: integer;
begin
  sListBox2.Items.BeginUpdate;
  AddAwesome(False);

  for i := 0 to sCharImageList1.EmbeddedFonts.Count - 1 do
    sListBox2.Items.Add(sCharImageList1.EmbeddedFonts[i].FontName);

  sListView1.ViewStyle := vsIcon;
  sListBox2.Items.EndUpdate;
  sComboBox1.ItemIndex := arCharsets[SYMBOL_CHARSET];
  sComboBox1Change(sComboBox1);
  sListBox1.ItemIndex := sListBox1.Items.IndexOf(s_WebDings);
{$IFDEF DELPHI6UP}
  if sListView1.Items.Count > 0 then
    sListView1.ItemIndex := 0
  else
    IterateControls(sPanel2, 0, EnableCtrls);
{$ENDIF}

  sListView1Click(sListView1);
  if sListBox1.ItemIndex >= 0 then
    sListBox1Click(sListBox1)
  else
    if sListBox2.ItemIndex >= 0 then
      sPageControl1.ActivePageIndex := 1;

  UpdateBtnsListBox2;
end;


procedure TFormCharListEditor.InitControls;
var
  i: integer;
begin
  Caption := SourceList.Owner.Name + s_Dot + SourceList.Name;
{$IFDEF DELPHI6UP}
  if IsValidIndex(sListView1.ItemIndex, sCharImageList1.Items.Count) then begin
{$ELSE}
  if (sListView1.Selected <> nil) and IsValidIndex(sListView1.Selected.Index, sCharImageList1.Items.Count) then begin
{$ENDIF}
    ControlsUpdating := True;

    i := sCharImageList1.FontIndex(sCharImageList1.Items[sListView1.Selected.Index].FontName);
    if i >= 0 then begin // Embedded
      sListBox1.ItemIndex := -1;
      sListBox2.ItemIndex := i;
      sListBox2Click(sListBox2);
      sPageControl1.ActivePageIndex := 1;
    end
    else begin
      sListBox2.ItemIndex := -1;
      if sCharImageList1.Items[sListView1.Selected.Index].Charset <> arCharSets[sComboBox1.ItemIndex] then begin
        for i := 0 to sComboBox1.Items.Count - 1 do
          if sCharImageList1.Items[sListView1.Selected.Index].Charset = arCharSets[i] then begin
            sComboBox1.ItemIndex := i;
            Break;
          end;

        sComboBox1Change(sComboBox1);
      end;
      sListBox1.ItemIndex := sListBox1.Items.IndexOf(sCharImageList1.Items[sListView1.Selected.Index].FontName);
      sPageControl1.ActivePageIndex := 0;
    end;
    sStatusBar1.Panels[1].Text := IntToStr(sCharImageList1.Width) + 'x' + IntToStr(sCharImageList1.Height);
    sStatusBar1.Panels[3].Text := '0x' + IntToHex(ord(sCharImageList1.Items[sListView1.Selected.Index].Char), 4);
    sStatusBar1.Panels[5].Text := sCharImageList1.Items[sListView1.Selected.Index].FontName;
    sTrackBar1.Position := Round((sCharImageList1.Items[sListView1.Selected.Index].ScalingFactor - 1) * 100);
    sTrackBar2.Position := sCharImageList1.Items[sListView1.Selected.Index].OffsetY;
    sLabel3.Caption := 'X ' + FloatToStr(sCharImageList1.Items[sListView1.Selected.Index].ScalingFactor);
    sLabel4.Caption := FloatToStr(sTrackBar2.Position);
    sColorBox1.Selected := sCharImageList1.Items[sListView1.Selected.Index].Color;
    ControlsUpdating := False;
  end
  else begin
    sTrackBar1.Position := 0;
    sTrackBar2.Position := 0;
    sLabel3.Caption := 'X 1';
    sLabel4.Caption := '0';
    sColorBox1.Selected := clNone;
    sStatusBar1.Panels[3].Text := '';
    sStatusBar1.Panels[5].Text := '';
  end;
end;


procedure TFormCharListEditor.InitFromImgList(ImgList: TsCharImageList);
begin
  BidiMode := Application.BiDiMode;
  if BiDiMode = bdRightToLeft then
    ReflectControls(Self, True);

  SourceList := ImgList;
  sCharImageList1.Assign(ImgList);
  RefillListView;
  sListView1.SkinData.VertScrollData.ButtonsSize := 0;
  sListView1.LargeImages := sCharImageList1;
end;


procedure TFormCharListEditor.RefillListView;
var
  i: Integer;
begin
  sListView1.Items.BeginUpdate;
  sListView1.SkinData.BeginUpdate;
  sListView1.Items.Clear;
  for i := 0 to sCharImageList1.Items.Count - 1 do
    with sListView1.Items.Add do begin
      ImageIndex := i;
      Caption := IntToStr(i);
    end;

  sListView1.SkinData.EndUpdate;
  sListView1.Items.EndUpdate;
end;


procedure TFormCharListEditor.sButton1Click(Sender: TObject);
begin
  SourceList.Assign(sCharImageList1);
  SourceList.Change;
end;


var
  sLastFontName: string = '';

function EnumFontCallback(lpelfe: PLogFont; lpntme: PNewTextMetricEX; FontType: DWORD; lp: LPARAM): Integer; stdcall;
begin
  if (lpelfe^.lfFaceName[0] <> '@') and (lpelfe^.lfFaceName <> sLastFontName) then begin
    sLastFontName := lpelfe^.lfFaceName;
    TStrings(lp).Add(lpelfe^.lfFaceName);
  end;
  Result := 1;
end;


{$IFNDEF NOFONTRES}
procedure TFormCharListEditor.sListBox2BeforeItemDraw(aDC: HDC; aIndex: Integer; var aRect: TRect);
const
  s = WideString(WideChar($F08D));
var
  f: TFont;
  SavedDC: hdc;
begin
  if aIndex = 0 then begin
    SavedDC := SaveDC(aDC);
    f := TFont.Create;
    try
      GetCurrentObject(aDC, f.Handle);
      f.Name := s_FontAwesome;
      f.Size := sListBox2.Canvas.Font.Size + 3;
      inc(aRect.Left, acSpacing);
      SelectObject(aDC, f.Handle);
      DrawTextW(aDC, PWideChar(s), 1, aRect, DT_NOPREFIX or DT_VCENTER);
      inc(aRect.Left, abs(f.Height) + acSpacing);
    finally
      f.Free;
      RestoreDC(aDC, SavedDC);
    end;
  end;
end;
{$ELSE}
procedure TFormCharListEditor.sListBox2BeforeItemDraw(aDC: HDC; aIndex: Integer; var aRect: TRect);
begin
end;
{$ENDIF}


procedure TFormCharListEditor.sListBox2Click(Sender: TObject);
begin
  if sListBox2.ItemIndex >= 0 then begin
    sListBox1.ItemIndex := -1;
{$IFDEF DELPHI6UP}
    if IsValidIndex(sListView1.ItemIndex, sCharImageList1.Items.Count) then begin
{$ELSE}
    if (sListView1.Selected <> nil) and IsValidIndex(sListView1.Selected.Index, sCharImageList1.Items.Count) then begin
{$ENDIF}
      sCharImageList1.Items[sListView1.Selected.Index].Charset := DEFAULT_CHARSET;
      sCharImageList1.Items[sListView1.Selected.Index].FontName := sListBox2.Items[sListBox2.ItemIndex];
      sStatusBar1.Panels[5].Text := sListBox2.Items[sListBox2.ItemIndex];
      if not ControlsUpdating then
        sCharImageList1.GenerateStdList;
    end
    else
      sStatusBar1.Panels[5].Text := '';
  end
  else
    sStatusBar1.Panels[5].Text := '';

  UpdateBtnsListBox2;
  sSpeedButton1.Enabled := CanAddChar;
end;


procedure TFormCharListEditor.sColorBox1Change(Sender: TObject);
var
  lItem: TListItem;
begin
  if not ControlsUpdating then begin
    lItem := sListView1.GetNextItem(nil, sdAll, [isSelected]);
    Perform(WM_SETREDRAW, 0, 0);
    while lItem <> nil do begin
      if sColorBox1.Selected <> sCharImageList1.Items[lItem.Index].Color then begin
        sCharImageList1.Items[lItem.Index].Color := sColorBox1.Selected;
        sCharImageList1.UpdateStd(lItem.Index);
      end;
      lItem := sListView1.GetNextItem(lItem, sdAll, [isSelected]);
    end;
    Application.ProcessMessages;
    Perform(WM_SETREDRAW, 1, 0);
    ListView_RedrawItems(sListView1.Handle, 0, sListView1.Items.Count - 1);
    UpdateWindow(sListView1.Handle);
  end;
end;


procedure TFormCharListEditor.sComboBox1Change(Sender: TObject);
var
  sl: TStringList;
  lf: TLogFont;
  DC: hdc;
begin
  sListBox1.Items.BeginUpdate;
  sListBox1.Clear;
  ZeroMemory(@lf, SizeOf(TLogFont));
  lf.lfCharSet := arCharSets[sComboBox1.ItemIndex];
  DC := GetDC(0);
  try
    EnumFontFamiliesEx(DC, lf, @EnumFontCallback, {$IFDEF DELPHI7UP}NativeInt{$ELSE}LParam{$ENDIF}(sListBox1.Items), 0);
  finally
    ReleaseDC(0, DC);
  end;
  sl := TStringList.Create;
  sl.Assign(sListBox1.Items);
  sl.Sort;
  sListBox1.Items.Assign(sl);
  sl.Free;
  sListBox1.Items.EndUpdate;

{$IFDEF DELPHI6UP}
  if not ControlsUpdating and IsValidIndex(sListView1.ItemIndex, sCharImageList1.Items.Count) then begin
{$ELSE}
  if not ControlsUpdating and (sListView1.Selected <> nil) and IsValidIndex(sListView1.Selected.Index, sCharImageList1.Items.Count) then begin
{$ENDIF}
    if sListBox1.ItemIndex < 0 then
      sListBox1.ItemIndex := 0;

    sCharImageList1.AcBeginUpdate;
    if sListBox1.ItemIndex >= 0 then begin
      sCharImageList1.Items[sListView1.Selected.Index].Charset := arCharSets[sComboBox1.ItemIndex];
      sCharImageList1.Items[sListView1.Selected.Index].FontName := sListBox1.Items[sListBox1.ItemIndex]
    end
    else begin
      sCharImageList1.Items[sListView1.Selected.Index].CharSet := DEFAULT_CHARSET;
      sCharImageList1.Items[sListView1.Selected.Index].FontName := s_FontAwesome;
      FreeAndNil(sCharImageList1.Items[sListView1.Selected.Index].CacheBmp);
      sCharImageList1.Items[sListView1.Selected.Index].Char := $F128;
    end;
    sCharImageList1.AcEndUpdate;
  end;
  sSpeedButton1.Enabled := sListBox1.ItemIndex >= 0;
  sSpeedButton3.Enabled := sListBox1.ItemIndex >= 0;
  sSpeedButton1.Enabled := CanAddChar;
end;


procedure TFormCharListEditor.sListBox1Click(Sender: TObject);
begin
  if sListBox1.ItemIndex >= 0 then begin
    sListBox2.ItemIndex := -1;
{$IFDEF DELPHI6UP}
    if IsValidIndex(sListView1.ItemIndex, sCharImageList1.Items.Count) then begin
{$ELSE}
    if (sListView1.Selected <> nil) and IsValidIndex(sListView1.Selected.Index, sCharImageList1.Items.Count) then begin
{$ENDIF}
      sCharImageList1.Items[sListView1.Selected.Index].FontName := sListBox1.Items[sListBox1.ItemIndex];
      sStatusBar1.Panels[5].Text := sListBox1.Items[sListBox1.ItemIndex];
    end
    else
      sStatusBar1.Panels[5].Text := '';
  end
  else
    sStatusBar1.Panels[5].Text := '';

  sCharImageList1.GenerateStdList;
  sSpeedButton1.Enabled := CanAddChar;
end;


procedure TFormCharListEditor.sListView1Click(Sender: TObject);
begin
  InitControls;
  sSpeedButton2.Enabled := sListView1.Selected <> nil;
  sSpeedButton3.Enabled := sListView1.Selected <> nil;
  sSpeedButton4.Enabled := sListView1.Items.Count > 0;
  sTrackBar1.Enabled := sListView1.Selected <> nil;
  sTrackBar2.Enabled := sListView1.Selected <> nil;
  sColorBox1.Enabled := sListView1.Selected <> nil;
  IterateControls(sPanel2, Integer(sSpeedButton2.Enabled), EnableCtrls);
end;


procedure TFormCharListEditor.sListView1DragDrop(Sender, Source: TObject; X, Y: Integer);
var
  li: TListItem;
begin
  li := sListView1.GetItemAt(X, Y);
  sCharImageList1.Items[sListView1.ItemFocused.Index].Index := li.Index;
  sCharImageList1.Move(sListView1.ItemFocused.Index, li.Index);
  EndScroll;
end;


procedure TFormCharListEditor.sListView1DragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
var
  li: TListItem;
begin
  li := sListView1.GetItemAt(X, Y);
  Accept := (li <> nil) and (li <> sListView1.ItemFocused);
  if (X < scBorder) or (X > sListView1.Width - scBorder) then
    StartScroll
end;


procedure TFormCharListEditor.sPageControl1Change(Sender: TObject);
begin
  if (sPageControl1.ActivePageIndex = 0) and (sComboBox1.ItemIndex = iEMBEDDED) then
    sComboBox1Change(sComboBox1);
end;


procedure TFormCharListEditor.ShowCharMap(Sender: TsSpeedButton);
begin
  if FormCharMap = nil then begin
    GetIntController.MousePressed := True;
    Sender.Down := True;
    FormCharMap := TFormCharMap.Create(Self);
    FormCharMap.PressedButton := Sender;
    FormCharMap.sPanel2.Visible := sSpeedButton1 = Sender;

    if sListBox1.ItemIndex >= 0 then begin // Sys font used
      FormCharMap.sComboBox1.ItemIndex := sComboBox1.ItemIndex;
      FormCharMap.sComboBox1Change(FormCharMap.sComboBox1);
      FormCharMap.sComboBox2.ItemIndex := FormCharMap.sComboBox2.Items.IndexOf(sListBox1.Items[sListBox1.ItemIndex]);
    end
    else begin // Embedded font used
      FormCharMap.sComboBox1.ItemIndex := iEMBEDDED;
      FormCharMap.sComboBox1Change(FormCharMap.sComboBox1);
      if sListBox2.ItemIndex >= 0 then  // Sys font used
        FormCharMap.sComboBox2.ItemIndex := FormCharMap.sComboBox2.Items.IndexOf(sListBox2.Items[sListBox2.ItemIndex]);
    end;
    FormCharMap.Loading := False;
    FormCharMap.sComboBox2Change(FormCharMap.sComboBox2);
    GetIntController.IgnoreCapture := True;
    FormCharMap.BiDiMode := BiDiMode;
    if BiDiMode = bdRightToLeft then
      ReflectControls(FormCharMap, True);

    ShowPopupForm(FormCharMap, FormCharMap.PressedButton.ClientToScreen(Point(0, FormCharMap.PressedButton.Height)), sSkinProvider1.SkinData.Skinned);
    GetIntController.IgnoreCapture := False;
    FormCharMap.PressedButton.Down := False;
  end
  else begin
    FormCharMap.Close;
    FreeAndNil(FormCharMap);
    Sender.Down := True; // Will be changed to False in std MouseDown handler
  end;
end;


procedure TFormCharListEditor.sSpeedButton1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ShowCharMap(sSpeedButton1);
end;


procedure TFormCharListEditor.sSpeedButton1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  GetIntController.MousePressed := False;
end;


procedure TFormCharListEditor.sSpeedButton2Click(Sender: TObject);
{$IFDEF DELPHI6UP}
var
  SavedIndex: integer;
{$ENDIF}
begin
{$IFDEF DELPHI6UP}
  if IsValidIndex(sListView1.ItemIndex, sCharImageList1.Items.Count) then begin
    SavedIndex := sListView1.Selected.Index;
{$ELSE}
  if (sListView1.Selected <> nil) and IsValidIndex(sListView1.Selected.Index, sCharImageList1.Items.Count) then begin
{$ENDIF}
    sCharImageList1.Items.Delete(sListView1.Selected.Index);
    sListView1.Items.Delete(sListView1.Selected.Index);
    sCharImageList1.GenerateStdList;
    RefillListView;
{$IFDEF DELPHI6UP}
    sListView1.ItemIndex := min(SavedIndex, sListView1.Items.Count - 1);
{$ENDIF}
    sListView1Click(sListView1);
  end;
end;


procedure TFormCharListEditor.sSpeedButton3MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
{$IFDEF DELPHI6UP}
  if IsValidIndex(sListView1.ItemIndex, sCharImageList1.Items.Count) then
{$ELSE}
  if (sListView1.Selected <> nil) and IsValidIndex(sListView1.Selected.Index, sCharImageList1.Items.Count) then
{$ENDIF}
    ShowCharMap(sSpeedButton3);
end;


procedure TFormCharListEditor.sSpeedButton4Click(Sender: TObject);
begin
  if CustomRequest('Really delete all items?') then begin
    sCharImageList1.Clear;
    RefillListView;
    sListView1Click(sListView1);
  end;
end;


// Add new font
procedure TFormCharListEditor.sSpeedButton5Click(Sender: TObject);
var
  i: integer;
begin
  if sOpenDialog1.InitialDir = '' then
    sOpenDialog1.InitialDir := GetAppPath;

  if sOpenDialog1.Execute then begin
    sListBox2.Items.BeginUpdate;
    for i := 0 to sOpenDialog1.Files.Count - 1 do
      AddNewFont(sOpenDialog1.Files[i]);

    sListBox2.Items.EndUpdate;
  end;
  UpdateBtnsListBox2;
end;


// Clear all
procedure TFormCharListEditor.sSpeedButton6Click(Sender: TObject);
var
  i: integer;
begin
  sListBox2.Items.BeginUpdate;
  for i := sListBox2.Count - 1 downto 1 do
    DeleteFont(i);

  sListBox2.Items.EndUpdate;
  sListBox2.ItemIndex := 0;
  UpdateBtnsListBox2;
end;


procedure TFormCharListEditor.DeleteFont(Ndx: integer);
var
  i: integer;
  b: boolean;
begin
  b := False;
  for i := 0 to sCharImageList1.Count - 1 do
    if sCharImageList1.Items[i].FontName = sListBox2.Items[Ndx] then begin
      if not b then begin
        b := MessageDlg('Some chars requires this font: ' + sListBox2.Items[Ndx] + #13#10'Delete anyway?', mtConfirmation, [mbYes, mbNo], 0) = mrYes;
        if not b then
          Exit;
      end;
      sCharImageList1.Items[i].FontName := s_FontAwesome;
      sCharImageList1.Items[i].CharSet := DEFAULT_CHARSET;
      FreeAndNil(sCharImageList1.Items[i].CacheBmp);
      sCharImageList1.Items[i].Char := $F128;
    end;

  sCharImageList1.EmbeddedFonts.Delete(Ndx);
  sListBox2.Items.Delete(Ndx);
  if b then
    sCharImageList1.GenerateStdList;
end;


procedure TFormCharListEditor.sSpeedButton7Click(Sender: TObject);
var
  OldNdx: integer;
begin
  if sListBox2.ItemIndex > 0 then begin
    OldNdx := sListBox2.ItemIndex;
    DeleteFont(sListBox2.ItemIndex);
    sListBox2.ItemIndex := min(OldNdx, sListBox2.Items.Count - 1);
    UpdateBtnsListBox2;
  end;
end;


procedure TFormCharListEditor.sTrackBar1Change(Sender: TObject);
var
  lItem: TListItem;
  r: Real;
begin
  if not ControlsUpdating then begin
    r := 1 + (sTrackBar1.Position) * 0.01;
    sLabel3.Caption := 'X ' + FloatToStr(r);
    sLabel3.Repaint;
    lItem := sListView1.GetNextItem(nil, sdAll, [isSelected]);
    Perform(WM_SETREDRAW, 0, 0);
    while lItem <> nil do begin
      sCharImageList1.Items[lItem.Index].ScalingFactor := r;
      sCharImageList1.UpdateStd(lItem.Index);
      lItem := sListView1.GetNextItem(lItem, sdAll, [isSelected]);
    end;
    Perform(WM_SETREDRAW, 1, 0);
    lItem := sListView1.GetNextItem(nil, sdAll, [isSelected]);
    while lItem <> nil do begin
      ListView_RedrawItems(sListView1.Handle, lItem.Index, lItem.Index);
      lItem := sListView1.GetNextItem(lItem, sdAll, [isSelected]);
    end;
  end;
end;


procedure TFormCharListEditor.sTrackBar2Change(Sender: TObject);
var
  lItem: TListItem;
begin
  if not ControlsUpdating then begin
    sLabel4.Caption := FloatToStr(sTrackBar2.Position);
    sLabel4.Repaint;
    lItem := sListView1.GetNextItem(nil, sdAll, [isSelected]);
    Perform(WM_SETREDRAW, 0, 0);
    while lItem <> nil do begin
      sCharImageList1.Items[lItem.Index].OffsetY := sTrackBar2.Position;
      sCharImageList1.UpdateStd(lItem.Index);
      lItem := sListView1.GetNextItem(lItem, sdAll, [isSelected]);
    end;
    Perform(WM_SETREDRAW, 1, 0);
    lItem := sListView1.GetNextItem(nil, sdAll, [isSelected]);
    while lItem <> nil do begin
      ListView_RedrawItems(sListView1.Handle, lItem.Index, lItem.Index);
      lItem := sListView1.GetNextItem(lItem, sdAll, [isSelected]);
    end;
  end;
end;


procedure TFormCharListEditor.UpdateBtnsListBox2;
var
  NdxOffset: integer;
begin
  NdxOffset := {$IFNDEF NOFONTRES}1;{$ELSE}0;{$ENDIF}
  sSpeedButton7.Enabled := sListBox2.ItemIndex >= NdxOffset;
  sSpeedButton6.Enabled := sListBox2.Items.Count > NdxOffset;
end;


{$IFDEF NOFONTRES}
var
  ResStream: TResourceStream;
  FontsCount: integer;
  awFont: tHandle = 0;
{$ENDIF}

initialization

{$IFDEF NOFONTRES}
{$R acFontData.res}
  if awFont = 0 then begin
    ResStream := TResourceStream.Create(hInstance, sawFont, RT_RCDATA);
    awFont := AddFontMemResourceEx(ResStream.Memory, ResStream.Size, nil, @FontsCount);
    ResStream.Free();
  end;
{$ENDIF}

finalization
{$IFDEF NOFONTRES}
  if awFont <> 0 then
    RemoveFontMemResourceEx(awFont);
{$ENDIF}

end.
