unit sPropEditors;
{$I sDefs.inc}

interface

uses
  Windows, Messages, Classes, Graphics, Controls, Forms, Dialogs,
  FileCtrl, ComCtrls, ExtCtrls, ImgList, SysUtils,
  {$IFDEF DELPHI_10}Vcl.Consts{$ELSE}Consts{$ENDIF}, ComStrs, CommCtrl, TypInfo, ColnEdit,
{$IFNDEF ALITE}
  sDialogs, sPageControl,
  {$IFDEF TNTUNICODE}
  TntComCtrls_Design,
  {$ENDIF}
{$ENDIF}
{$IFDEF DELPHI6UP} DesignEditors, DesignIntf, VCLEditors, {$ELSE} dsgnintf, {$ENDIF}
  sVclUtils, sPanel, sGraphUtils, acntUtils, sConst;


type
{$IFNDEF ALITE}
  TacHintProperty = class(TCaptionProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetEditLimit: Integer; override;
    procedure Edit; override;
  end;


  TacGradientProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;


  TacPageControlEditor = class(TDefaultEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;


  TacFrameBarEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;


  TacTabSheetEditor = class(TDefaultEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;


  TacHintsTemplatesProperty = class(TClassProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;


  TacAlphaHintsEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;


  TacFloatCollectionEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;


  TacPathDlgEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;


  TacTemplateNameProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;


  TacCharListItemsProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;


  TacCharListEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;
{$ENDIF}


  TacSkinInfoProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;


  TacImageListEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;


  TacFontStoreEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;


  TacEmbeddedFontsProperty = class(TClassProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;


  TacImageIndexEditor = class(TIntegerProperty{$IFDEF DELPHI7UP}, ICustomPropertyListDrawing{$ENDIF})
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    function GetMyList: TCustomImageList;
    procedure ListMeasureHeight(const Value: string; ACanvas: TCanvas; var AHeight: Integer); {$IFNDEF DELPHI6UP}override; {$ENDIF}
    procedure ListMeasureWidth(const Value: string; ACanvas: TCanvas; var AWidth: Integer); {$IFNDEF DELPHI6UP}override; {$ENDIF}
    procedure ListDrawValue(const Value: string; ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean); {$IFNDEF DELPHI6UP}override; {$ENDIF}
  end;


  TacImgListItemsProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;


  TacSkinSectionProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

{
  TacFontNameProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;
}

  TacSkinNameProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;


  TacDirProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;


  TacInternalSkinsProperty = class(TClassProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;


  TacThirdPartyProperty = class(TClassProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;


  TacSkinManagerEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;


  TacTitleBarEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;


{$IFDEF DELPHI6UP}
  TacListViewEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

procedure EditPropertyWithDialog(Component: TPersistent; const PropName: AnsiString; const Designer: IDesigner);
{$ENDIF}

procedure Register;

implementation

uses
  stdreg, Menus, DsnConst,
{$IFDEF USEFILTEDIT}
  FiltEdit,
{$ENDIF}
{$IFDEF USEHINTMANAGER}
  sHintEditor,
{$ENDIF}
{$IFNDEF ALITE}
  sToolEdit, sComboBoxes, sBitBtn, sLabel, sStrEdit, acShellCtrls, acRootEdit, acPathDialog, sFrameBar, sMemo, acAlphaHintsEdit,
  acNotebook, acAlphaHints, acImage, acSlider, sColorDialog, sGradBuilder, sGroupBox, acFloatCtrls, sListView, acCharListEditor,
  acArcControls, acMeter,
{$ENDIF}
  sCommonData, sDefaults, sSkinManager, sMaskData, sSkinProps, sStoreUtils, sInternalSkins, sSpeedButton, sStyleSimply,
  sGradient, acAlphaImageList, sImgListEditor, ac3rdPartyEditor, acSkinInfo, sSkinProvider, acTitleBar, sCheckBox, acFontStoreEditor,
  acFontStore, sGlyphUtils;


const
  sectName = 'DesignOptions';
  optionName = 'SkinsDirectory';


{$IFNDEF ALITE}
procedure TacPageControlEditor.ExecuteVerb(Index: Integer);
var
  NewPage: TsTabSheet;
begin
  case index of
    0: begin
      NewPage := TsTabSheet.Create(Designer.GetRoot);
      NewPage.Parent := Component as TsPageControl;
      NewPage.PageControl := Component as TsPageControl;
      NewPage.Caption := Designer.UniqueName('sTabSheet');
      NewPage.Name := NewPage.Caption;
    end;

    1: begin
      NewPage := TsTabSheet((Component as TsPageControl).ActivePage);
      NewPage.Free;
    end;

    2: (Component as TsPageControl).SelectNextPage(True);

    3: (Component as TsPageControl).SelectNextPage(False);
  end;
  if Designer <> nil then
    Designer.Modified;
end;


function TacPageControlEditor.GetVerb(Index: Integer): string;
begin
  case index of
    0: Result := 'New Page';
    1: Result := 'Delete Page';
    2: Result := 'Next Page';
    3: Result := 'Previous Page';
  end;
end;


function TacPageControlEditor.GetVerbCount: Integer;
begin
  Result := 4;
end;
{$ENDIF}


procedure Register;
begin
{$IFNDEF ALITE}
  RegisterComponentEditor(TsTabSheet,      TacTabSheetEditor);
  RegisterComponentEditor(TsFrameBar,      TacFrameBarEditor);
  RegisterComponentEditor(TsAlphaHints,    TacAlphaHintsEditor);
  RegisterComponentEditor(TsPageControl,   TacPageControlEditor);
  RegisterComponentEditor(TsFloatButtons,  TacFloatCollectionEditor);
  RegisterComponentEditor(TsCharImageList, TacCharListEditor);
  RegisterComponentEditor(TsFontStore,     TacFontStoreEditor);
  RegisterComponentEditor(TsPathDialog,    TacPathDlgEditor);

  RegisterPropertyEditor(TypeInfo(TacCharListItems), TsCharImageList, 'Items',         TacCharListItemsProperty);
  RegisterPropertyEditor(TypeInfo(TacEmbeddedFonts), TsCharImageList, 'EmbeddedFonts', TacCharListItemsProperty);
  RegisterPropertyEditor(TypeInfo(acFontStore.TacFonts), TsFontStore, 'Fonts',         TacEmbeddedFontsProperty);

  RegisterPropertyEditor(TypeInfo(Integer), TsMeter,       'GlyphIndex',         TacImageIndexEditor);
  RegisterPropertyEditor(TypeInfo(Integer), TsTitleItem,   'ImageIndex',         TacImageIndexEditor);

  RegisterPropertyEditor(TypeInfo(Integer), TacFloatBtn,   'ImageIndex',         TacImageIndexEditor);
  RegisterPropertyEditor(TypeInfo(Integer), TacFloatBtn,   'ImageIndexHot',      TacImageIndexEditor);
  RegisterPropertyEditor(TypeInfo(Integer), TacFloatBtn,   'ImageIndexPressed',  TacImageIndexEditor);

  RegisterPropertyEditor(TypeInfo(Integer), TsSlider,      'GlyphIndexOff',      TacImageIndexEditor);
  RegisterPropertyEditor(TypeInfo(Integer), TsSlider,      'GlyphIndexOn',       TacImageIndexEditor);
  RegisterPropertyEditor(TypeInfo(Integer), TsSlider,      'ImageIndexOff',      TacImageIndexEditor);
  RegisterPropertyEditor(TypeInfo(Integer), TsSlider,      'ImageIndexOn',       TacImageIndexEditor);
  RegisterPropertyEditor(TypeInfo(Integer), TsSlider,      'ThumbIndexOff',      TacImageIndexEditor);
  RegisterPropertyEditor(TypeInfo(Integer), TsSlider,      'ThumbIndexOn',       TacImageIndexEditor);
  RegisterPropertyEditor(TypeInfo(Integer), TsImage,       'ImageIndex',         TacImageIndexEditor);
  RegisterPropertyEditor(TypeInfo(Integer), TsImgLabel,    'ImageIndex',         TacImageIndexEditor);


  RegisterPropertyEditor(TypeInfo(Integer), TsSpeedButton, 'ImageIndex',         TacImageIndexEditor);
  RegisterPropertyEditor(TypeInfo(Integer), TsSpeedButton, 'ImageIndexHot',      TacImageIndexEditor);
  RegisterPropertyEditor(TypeInfo(Integer), TsSpeedButton, 'ImageIndexPressed',  TacImageIndexEditor);
  RegisterPropertyEditor(TypeInfo(Integer), TsSpeedButton, 'ImageIndexDisabled', TacImageIndexEditor);
  RegisterPropertyEditor(TypeInfo(Integer), TsSpeedButton, 'ImageIndexSelected', TacImageIndexEditor);

  RegisterPropertyEditor(TypeInfo(Integer), TsGroupBox,    'ImageIndex',         TacImageIndexEditor);
  RegisterPropertyEditor(TypeInfo(Integer), TsArcControl,  'GlyphIndex',         TacImageIndexEditor);

  RegisterPropertyEditor(TypeInfo(Integer), TacCustomRoundBtn, 'ImageIndex',         TacImageIndexEditor);
  RegisterPropertyEditor(TypeInfo(Integer), TacCustomRoundBtn, 'ImageIndexHot',      TacImageIndexEditor);
  RegisterPropertyEditor(TypeInfo(Integer), TacCustomRoundBtn, 'ImageIndexPressed',  TacImageIndexEditor);
  RegisterPropertyEditor(TypeInfo(Integer), TsBitBtn,          'ImageIndex',         TacImageIndexEditor);

  RegisterPropertyEditor(TypeInfo(Integer), TsRollOutPanel, 'ImageIndexCollapsed', TacImageIndexEditor);
  RegisterPropertyEditor(TypeInfo(Integer), TsRollOutPanel, 'ImageIndexExpanded',  TacImageIndexEditor);

  RegisterPropertyEditor(TypeInfo(TacStrValue),      TsAlphaHints, 'TemplateName', TacTemplateNameProperty);
  RegisterPropertyEditor(TypeInfo(TacHintTemplates), TsAlphaHints, 'Templates',    TacHintsTemplatesProperty);

  RegisterPropertyEditor(TypeInfo(TsSkinSection), TObject, 'TabSkin',       TacSkinSectionProperty);
  RegisterPropertyEditor(TypeInfo(TsSkinSection), TObject, 'PanelSkin',     TacSkinSectionProperty);
  RegisterPropertyEditor(TypeInfo(TsSkinSection), TObject, 'HeaderSkin',    TacSkinSectionProperty);
  RegisterPropertyEditor(TypeInfo(TsSkinSection), TObject, 'SelectionSkin', TacSkinSectionProperty);

//  RegisterPropertyEditor(TypeInfo(string), TacFonts, 'MainFont', TacFontNameProperty);

  RegisterPropertyEditor(TypeInfo(string), TacGradPaintData, 'CustomGradient', TacGradientProperty);
  // Shell ctrls
  RegisterPropertyEditor(TypeInfo(TacRoot), TsShellListView, 'Root', TacRootProperty);
  RegisterPropertyEditor(TypeInfo(TacRoot), TsShellTreeView, 'Root', TacRootProperty);
  RegisterPropertyEditor(TypeInfo(TacRoot), TsShellComboBox, 'Root', TacRootProperty);
  RegisterPropertyEditor(TypeInfo(TacRoot), TsPathDialog,    'Root', TacRootProperty);
  RegisterPropertyEditor(TypeInfo(TacRoot), TsDirectoryEdit, 'Root', TacRootProperty);

  {$IFDEF USEFILTEDIT}
  RegisterPropertyEditor(TypeInfo(string), TsFileNameEdit, 'Filter', TFilterProperty);
  {$ENDIF}

  {$IFNDEF TNTUNICODE}
  RegisterPropertyEditor(TypeInfo(string),   TsMemo,       'Text',    TacHintProperty);
  RegisterPropertyEditor(TypeInfo(string),   TObject,      'Hint',    TacHintProperty);
  RegisterPropertyEditor(TypeInfo(TCaption), TObject,      'Caption', TacHintProperty);
  RegisterPropertyEditor(TypeInfo(string),   TMenuItem,    'Caption', TacHintProperty);
  RegisterPropertyEditor(TypeInfo(string),   TsBoundLabel, 'Caption', TacHintProperty);
  {$IFDEF DELPHI6UP}RegisterComponentEditor(TsListView, TacListViewEditor);{$ENDIF}
  {$ELSE}
  RegisterComponentEditor(TsListView, TTntListViewEditor);
  {$ENDIF} // TNTUNICODE

{$ENDIF} // IFNDEF ALITE

  RegisterComponentEditor(TsTitleBar,       TacTitleBarEditor);
  RegisterComponentEditor(TsAlphaImageList, TacImageListEditor);
  RegisterComponentEditor(TsSkinManager,    TacSkinManagerEditor);

  RegisterPropertyEditor(TypeInfo(Integer), TacAddedGlyph, 'ImageIndex',         TacImageIndexEditor);

  RegisterPropertyEditor(TypeInfo(TsSkinSection), TObject, 'MenuLineSkin', TacSkinSectionProperty);
  RegisterPropertyEditor(TypeInfo(TsSkinSection), TObject, 'IcoLineSkin',  TacSkinSectionProperty);
  RegisterPropertyEditor(TypeInfo(TsSkinSection), TObject, 'SkinSection',  TacSkinSectionProperty);
  RegisterPropertyEditor(TypeInfo(TsSkinSection), TObject, 'TitleSkin',    TacSkinSectionProperty);
  RegisterPropertyEditor(TypeInfo(TsSkinSection), TObject, 'ButtonSkin',   TacSkinSectionProperty);
  RegisterPropertyEditor(TypeInfo(TsSkinSection), TObject, 'CaptionSkin',  TacSkinSectionProperty);
  RegisterPropertyEditor(TypeInfo(TsSkinSection), TObject, 'CloseBtnSkin', TacSkinSectionProperty);
  RegisterPropertyEditor(TypeInfo(TsSkinSection), TObject, 'TabsLineSkin', TacSkinSectionProperty);
  RegisterPropertyEditor(TypeInfo(TsSkinSection), TObject, 'ProgressSkin', TacSkinSectionProperty);

  RegisterPropertyEditor(TypeInfo(TsSkinName),       TsSkinManager, 'SkinName',      TacSkinNameProperty);
  RegisterPropertyEditor(TypeInfo(TsDirectory),      TsSkinManager, 'SkinDirectory', TacDirProperty);
  RegisterPropertyEditor(TypeInfo(TsStoredSkins),    TsSkinManager, 'InternalSkins', TacInternalSkinsProperty);
  RegisterPropertyEditor(TypeInfo(TacSkinInfo),      TsSkinManager, 'SkinInfo',      TacSkinInfoProperty);
  RegisterPropertyEditor(TypeInfo(TsThirdPartyList), TsSkinManager, 'ThirdParty',    TacThirdPartyProperty);
  RegisterPropertyEditor(TypeInfo(TsThirdPartyList), TsSkinProvider,'ThirdParty',    TacThirdPartyProperty);
  RegisterPropertyEditor(TypeInfo(TsImgListItems), TsAlphaImageList, 'Items',        TacImgListItemsProperty);

  RegisterPropertyEditor(TypeInfo(Integer), TacTitleBarItem, 'ImageIndex',         TacImageIndexEditor);
  RegisterPropertyEditor(TypeInfo(Integer), TacTitleBarItem, 'HotImageIndex',      TacImageIndexEditor);
  RegisterPropertyEditor(TypeInfo(Integer), TacTitleBarItem, 'PressedImageIndex',  TacImageIndexEditor);
  RegisterPropertyEditor(TypeInfo(Integer), TacTitleBarItem, 'DisabledImageIndex', TacImageIndexEditor);

  RegisterPropertyEditor(TypeInfo(TsImageIndex), TControl, 'ImgChecked',           TacImageIndexEditor);
  RegisterPropertyEditor(TypeInfo(TsImageIndex), TControl, 'ImgUnchecked',         TacImageIndexEditor);
end;


function TacSkinNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList, paAutoUpdate];
end;


procedure TacSkinNameProperty.GetValues(Proc: TGetStrProc);
var
  i: Integer;
begin
  with TsSkinManager(GetComponent(0)).SkinListController do begin
    if Length(SkinList) = 0 then
      UpdateData(True);

    for i := 0 to Length(SkinList) - 1 do
      Proc(SkinList[i].skName);
  end;
end;


function TacDirProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paAutoUpdate];
end;


procedure TacDirProperty.Edit;
var
  s: string;
begin
  s := TsSkinManager(GetComponent(0)).SkinDirectory;
  if not directoryExists(s) then
    s := 'C:\';

{$IFNDEF ALITE}
  PathDialogForm := TPathDialogForm.Create(Application);
  try
    PathDialogForm.sShellTreeView1.Path := s;
    if PathDialogForm.ShowModal = mrOk then begin
      s := PathDialogForm.sShellTreeView1.Path;
      if (s <> '') and directoryExists(s) then
        TsSkinManager(GetComponent(0)).SkinDirectory := s;
    end;
  finally
    FreeAndNil(PathDialogForm);
  end;
{$ELSE}
  if SelectDirectory('', '', s) then
    if (s <> '') and directoryExists(s) then
      TsSkinManager(GetComponent(0)).SkinDirectory := s;
{$ENDIF}
end;


function TacDirProperty.GetValue: string;
var
  s: string;
begin
  Result := inherited GetValue;
  if Result = DefSkinsDir then begin
    s := sStoreUtils.ReadRegString(HKEY_CURRENT_USER, 'SOFTWARE\' + s_RegName, s_IntSkinsPath);
    if (s <> '') and directoryExists(s) then begin
      Result := s;
      TsSkinManager(GetComponent(0)).SkinDirectory := s;
    end;
  end;
end;


procedure TacDirProperty.SetValue(const Value: string);
begin
  inherited;
  sStoreUtils.WriteRegString(HKEY_CURRENT_USER, 'SOFTWARE\' + s_RegName, s_IntSkinsPath, Value);
end;


procedure TacInternalSkinsProperty.Edit;
var
  i: Integer;
begin
  Application.CreateForm(TFormInternalSkins, FormInternalSkins);
  FormInternalSkins.ListBox1.Clear;
  FormInternalSkins.SkinManager := TsSkinManager(GetComponent(0));
  for i := 0 to FormInternalSkins.SkinManager.InternalSkins.Count - 1 do
    FormInternalSkins.ListBox1.Items.Add(FormInternalSkins.SkinManager.InternalSkins.Items[i].Name);

  FormInternalSkins.ShowModal;
//  if (FormInternalSkins.ShowModal = mrOk) and (Designer <> nil) then
//  Designer.Modified;

  sStoreUtils.WriteRegString(HKEY_CURRENT_USER, 'SOFTWARE\' + s_RegName, s_IntSkinsPath, FormInternalSkins.SkinManager.SkinDirectory);
  if Assigned(FormInternalSkins) then
    FreeAndNil(FormInternalSkins);

  if Designer <> nil then
    Designer.Modified;

  inherited;
end;


function TacInternalSkinsProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paAutoUpdate, paReadOnly];
end;


procedure TacSkinManagerEditor.ExecuteVerb(Index: Integer);
var
  i: Integer;
  sm: TsSkinManager;
begin
  case index of
    0: begin
      sm := TsSkinManager(Component);
      Application.CreateForm(TFormInternalSkins, FormInternalSkins);
      FormInternalSkins.ListBox1.Clear;
      FormInternalSkins.SkinManager := sm;
      FormInternalSkins.SkinManager.SkinListController.UpdateData(False);
      for i := 0 to sm.InternalSkins.Count - 1 do
        FormInternalSkins.ListBox1.Items.Add(sm.InternalSkins.Items[i].Name);

      FormInternalSkins.ShowModal;
      sStoreUtils.WriteRegString(HKEY_CURRENT_USER, 'SOFTWARE\' + s_RegName, s_IntSkinsPath, FormInternalSkins.SkinManager.SkinDirectory);
      FreeAndNil(FormInternalSkins);
      sm.SkinListController.UpdateData;
      if Designer <> nil then
        Designer.Modified;
    end;

    1: begin
      Application.CreateForm(TForm3rdPartyEditor, Form3rdPartyEditor);
      Form3rdPartyEditor.Cmp := Component;

      if Component is TsSkinManager then
        Form3rdPartyEditor.Lists := TsSkinManager(Component).ThirdLists
      else
        Form3rdPartyEditor.Lists := TsSkinProvider(Component).ThirdLists;

      Form3rdPartyEditor.sListView1.Items.Clear;
      Form3rdPartyEditor.Populate;
      Form3rdPartyEditor.ShowModal;

      for i := 0 to Length(Form3rdPartyEditor.Lists) - 1 do
        TsSkinManager(Component).ThirdLists[i].Text := Form3rdPartyEditor.Lists[i].Text;

      UpdateThirdNames(TsSkinManager(Component));
      FreeAndNil(Form3rdPartyEditor);
      if Designer <> nil then
        Designer.Modified;
    end;
  end;
  inherited;
end;


function TacSkinManagerEditor.GetVerb(Index: Integer): string;
begin
  case index of
    0: Result := '&Internal skins...';
    1: Result := '&Third party controls...';
    2: Result := CharMinus;
  end;
end;


function TacSkinManagerEditor.GetVerbCount: Integer;
begin
  Result := 2;
end;


{$IFNDEF ALITE}
procedure TacTabSheetEditor.ExecuteVerb(Index: Integer);
var
  NewPage: TsTabSheet;
begin
  case index of
    0: begin
      NewPage := TsTabSheet.Create(Designer.GetRoot);
      NewPage.Parent := TsTabSheet(Component).PageControl;
      NewPage.PageControl := TsTabSheet(Component).PageControl;
      NewPage.Caption := Designer.UniqueName('sTabSheet');
      NewPage.Name := NewPage.Caption;
    end;

    1: begin
      NewPage := TsTabSheet(TsTabSheet(Component).PageControl.ActivePage);
      NewPage.Free;
    end;

    2: TsTabSheet(Component).PageControl.SelectNextPage(True);

    3: TsTabSheet(Component).PageControl.SelectNextPage(False);
  end;
  if Designer <> nil then
    Designer.Modified;
end;


function TacTabSheetEditor.GetVerb(Index: Integer): string;
begin
  case index of
    0: Result := 'New Page';
    1: Result := 'Delete Page';
    2: Result := 'Next Page';
    3: Result := 'Previous Page';
  end;
end;


function TacTabSheetEditor.GetVerbCount: Integer;
begin
  Result := 4;
end;


procedure TacHintProperty.Edit;
var
  Temp: string;
  Comp: TPersistent;
  sed: TStrEditDlg;
begin
  sed := TStrEditDlg.Create(Application);
  with sed do
    try
      Comp := GetComponent(0);
      if Comp is TComponent then
        Caption := TComponent(Comp).Name + s_Dot + GetName
      else
        Caption := GetName;

      Temp := GetStrValue;
      Memo.Text := Temp;
{$IFNDEF TNTUNICODE}
      Memo.MaxLength := GetEditLimit;
{$ENDIF}
      UpdateStatus(nil);
      if ShowModal = mrOk then begin
        Temp := Memo.Text;
        while (Length(Temp) > 0) and (Temp[Length(Temp)] < s_Space) do
          System.Delete(Temp, Length(Temp), 1);

        SetStrValue(Temp);
        if Designer <> nil then
          Designer.Modified;
      end;
    finally
      Free;
    end;
end;


function TacHintProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog, paAutoUpdate];
end;


function TacHintProperty.GetEditLimit: Integer;
begin
  if GetPropType^.Kind = tkString then
    Result := GetTypeData(GetPropType)^.MaxLength
  else
    Result := 1024;
end;


procedure TacPathDlgEditor.ExecuteVerb(Index: Integer);
begin
  inherited;
  TsPathDialog(Component).Execute;
end;


function TacPathDlgEditor.GetVerb(Index: Integer): string;
begin
  case index of
    0: Result := '&Test dialog...';
    1: Result := CharMinus;
  end;
end;


function TacPathDlgEditor.GetVerbCount: Integer;
begin
  Result := 2;
end;


procedure TacFrameBarEditor.ExecuteVerb(Index: Integer);
begin
  inherited;
  ShowCollectionEditor(Designer, Component, (Component as TsFrameBar).Items, 'Items');
end;


function TacFrameBarEditor.GetVerb(Index: Integer): string;
begin
  case index of
    0: Result := '&Items editor...';
  end;
end;


function TacFrameBarEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;


procedure TacHintsTemplatesProperty.Edit;
begin
  if EditHints(TsAlphaHints(GetComponent(0))) and (Designer <> nil) then
    Designer.Modified;

  inherited;
end;


function TacHintsTemplatesProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paAutoUpdate, paReadOnly];
end;


procedure TacAlphaHintsEditor.ExecuteVerb(Index: Integer);
begin
  case index of
    0: if EditHints(TsAlphaHints(Component)) and (Designer <> nil) then
     Designer.Modified;
  end;
end;


function TacAlphaHintsEditor.GetVerb(Index: Integer): string;
begin
  case index of
    0: Result := '&Hints templates editor...';
  end;
end;


function TacAlphaHintsEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;


function TacTemplateNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList];
end;


procedure TacTemplateNameProperty.GetValues(Proc: TGetStrProc);
var
  i: Integer;
begin
  inherited;
  with TsAlphaHints(GetComponent(0)) do
    for i := 0 to Templates.Count - 1 do
      Proc(Templates[i].Name);
end;


procedure TacGradientProperty.Edit;
var
  GradArray: TsGradArray;
  PaintData: TObject;
begin
  PaintData := GetComponent(0);
  if PaintData is TacGradPaintData then
    with TacGradPaintData(PaintData) do begin
      CreateEditorForm(ColDlg);
      PrepareGradArray(CustomGradient, GradArray);
      GradBuilder.LoadFromArray(GradArray);
      GradBuilder.ShowModal;
      case GradBuilder.ModalResult of
        mrOk: begin
          CustomGradient := GradBuilder.AsString;
          Designer.Modified;
        end;
        mrNone: begin
          CustomGradient := '';
          Designer.Modified;
        end;
      end;
      KillForm;
    end;
end;


function TacGradientProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paAutoUpdate, paReadOnly];
end;


procedure TacFloatCollectionEditor.ExecuteVerb(Index: Integer);
begin
  inherited;
  case index of
    0: ShowCollectionEditor(Designer, Component, acFloatCtrls.TsFloatButtons(Component).Items, 'Items');
  end;
end;


function TacFloatCollectionEditor.GetVerb(Index: Integer): string;
begin
  Result := '';
  case index of
    0: Result := 'Edit list of items...';
    1: Result := CharMinus;
  end;
end;


function TacFloatCollectionEditor.GetVerbCount: Integer;
begin
  Result := 2;
end;


procedure TacCharListItemsProperty.Edit;
var
  Form: TFormCharListEditor;
begin
  Application.CreateForm(TFormCharListEditor, Form);
  Form.InitFromImgList(TsCharImageList(GetComponent(0)));
  Form.ShowModal;
  FreeAndNil(Form);
  if Designer <> nil then
    Designer.Modified;
end;


function TacCharListItemsProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paAutoUpdate, paReadOnly];
end;


procedure TacCharListEditor.ExecuteVerb(Index: Integer);
begin
  case index of
    0: begin
      Application.CreateForm(TFormCharListEditor, FormCharListEditor);
      FormCharListEditor.InitFromImgList(Component as TsCharImageList);
      FormCharListEditor.ShowModal;
      FreeAndNil(FormCharListEditor);
    end;
  end;
  if Designer <> nil then
    Designer.Modified;
end;


function TacCharListEditor.GetVerb(Index: Integer): string;
begin
  case index of
    0: Result := '&ImageList editor...';
  end;
end;


function TacCharListEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;
{$ENDIF}


function TacSkinSectionProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList, paAutoUpdate, paMultiSelect];
end;


procedure TacSkinSectionProperty.GetValues(Proc: TGetStrProc);
var
  i: Integer;
begin
  inherited;
  if Assigned(DefaultManager) then
    with DefaultManager do
      if Length(gd) > 0 then
        for i := 0 to Length(gd) - 1 do
          if gd[i].ClassName <> s_GlobalInfo then
            Proc(gd[i].ClassName);
end;


function TacImageIndexEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paValueList, paRevertable];
end;


function TacImageIndexEditor.GetMyList: TCustomImageList;
var
  Obj: TObject;
begin
  Obj := GetComponent(0);
{$IFDEF DELPHI6UP}
  if Obj is TCollectionItem then // Search a component if property owner is TCollectionItem
    Obj := TCollectionItem(Obj).Collection.Owner;
{$ENDIF}
  Result := TCustomImageList(GetObjectProp(Obj, 'Images', TObject));
end;


procedure TacImageIndexEditor.GetValues(Proc: TGetStrProc);
var
  i: Integer;
  MyList: TCustomImageList;
begin
  MyList := GetMyList;
  if Assigned(MyList) then
    for i := 0 to MyList.Count - 1 do
      Proc(IntToStr(i));
end;


procedure TacImageIndexEditor.ListDrawValue(const Value: string; ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
var
  MyLeft: Integer;
  MyList: TCustomImageList;
begin
  ACanvas.FillRect(ARect);
  MyList := GetMyList;
  MyLeft := ARect.Left + 2;
  if Assigned(MyList) then begin
    MyList.Draw(ACanvas, MyLeft, ARect.Top + 2, StrToInt(Value));
    Inc(MyLeft, MyList.Width);
  end;
  ACanvas.TextOut(MyLeft + 2, ARect.Top + 1, Value);
end;


procedure TacImageIndexEditor.ListMeasureHeight(const Value: string; ACanvas: TCanvas; var AHeight: Integer);
var
  MyList: TCustomImageList;
begin
  MyList := GetMyList;
  AHeight := ACanvas.TextHeight(Value) + 2;
  if Assigned(MyList) and (MyList.Height + 4 > AHeight) then
    AHeight := MyList.Height + 4;
end;


procedure TacImageIndexEditor.ListMeasureWidth(const Value: string; ACanvas: TCanvas; var AWidth: Integer);
var
  MyList: TCustomImageList;
begin
  MyList := GetMyList;
  AWidth := ACanvas.TextWidth(Value) + 4;
  if Assigned(MyList) then
    Inc(AWidth, MyList.Width);
end;


procedure TacThirdPartyProperty.Edit;
var
  i: Integer;
begin
  Application.CreateForm(TForm3rdPartyEditor, Form3rdPartyEditor);
  Form3rdPartyEditor.Cmp := TComponent(GetComponent(0));
  if Form3rdPartyEditor.Cmp is TsSkinManager then
    Form3rdPartyEditor.Lists := TsSkinManager(Form3rdPartyEditor.Cmp).ThirdLists
  else
    Form3rdPartyEditor.Lists := TsSkinProvider(Form3rdPartyEditor.Cmp).ThirdLists;

  Form3rdPartyEditor.sListView1.Items.Clear;
  Form3rdPartyEditor.Populate;
  Form3rdPartyEditor.ShowModal;

  if Form3rdPartyEditor.Cmp is TsSkinManager then begin
    for i := 0 to Length(Form3rdPartyEditor.Lists) - 1 do
      TsSkinManager(Form3rdPartyEditor.Cmp).ThirdLists[i].Text := Form3rdPartyEditor.Lists[i].Text;

    UpdateThirdNames(TsSkinManager(Form3rdPartyEditor.Cmp));
  end
  else begin
    for i := 0 to Length(Form3rdPartyEditor.Lists) - 1 do
      TsSkinProvider(Form3rdPartyEditor.Cmp).ThirdLists[i].Text := Form3rdPartyEditor.Lists[i].Text;

    UpdateProviderThirdNames(TsSkinProvider(Form3rdPartyEditor.Cmp));
  end;

  if Assigned(Form3rdPartyEditor) then
    FreeAndNil(Form3rdPartyEditor);

  if Designer <> nil then
    Designer.Modified;

  inherited;
end;


function TacThirdPartyProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paAutoUpdate, paReadOnly];
end;


procedure TacImgListItemsProperty.Edit;
var
  Form: TFormImgListEditor;
begin
  Application.CreateForm(TFormImgListEditor, Form);
  Form.InitFromImgList(TsAlphaImageList(GetComponent(0)));
  Form.ShowModal;
  FreeAndNil(Form);
  if Designer <> nil then
    Designer.Modified;
end;


function TacImgListItemsProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly];
end;


procedure TacSkinInfoProperty.Edit;
begin
  with TsSkinManager(GetComponent(0)) do
    if CommonSkinData.Active then begin
      SkinInfoForm := TSkinInfoForm.Create(Application);
      SkinInfoForm.sMemo1.Lines.Add('Name: ' + SkinName);
      SkinInfoForm.sMemo1.Lines.Add('Version: ' + SkinInfo);
      SkinInfoForm.sMemo1.Lines.Add('Author: ' + CommonSkinData.Author);
      SkinInfoForm.sMemo1.Lines.Add('Description: ' + CommonSkinData.Description);
      if CommonSkinData.Version < CompatibleSkinVersion then
        SkinInfoForm.Label1.Visible := True;

      try
        SkinInfoForm.ShowModal;
      finally
        FreeAndNil(SkinInfoForm);
      end;
    end
    else
      MessageDlg('Skins are not activated.', mtInformation, [mbOK], 0);
end;


function TacSkinInfoProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly, {$IFDEF DELPHI_XE}{ paDisplayReadOnly, }{$ENDIF}paFullWidthName];
end;


procedure TacImageListEditor.ExecuteVerb(Index: Integer);
begin
  case index of
    0: begin
      Application.CreateForm(TFormImgListEditor, FormImgListEditor);
      FormImgListEditor.InitFromImgList(Component as TsAlphaImageList);
      FormImgListEditor.ShowModal;
      FreeAndNil(FormImgListEditor);
    end;
  end;
  if Designer <> nil then
    Designer.Modified;
end;


function TacImageListEditor.GetVerb(Index: Integer): string;
begin
  case index of
    0: Result := '&ImageList editor...';
  end;
end;


function TacImageListEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;


procedure TacTitleBarEditor.ExecuteVerb(Index: Integer);
begin
  ShowCollectionEditorClass(Designer, TCollectionEditor, TsTitleBar(Component), TsTitleBar(Component).Items, 'Items', [coAdd, coDelete, coMove]);
  inherited;
end;


function TacTitleBarEditor.GetVerb(Index: Integer): string;
begin
  case index of
    0: Result := '&Title bar items...';
    1: Result := CharMinus;
  end;
end;


function TacTitleBarEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;


{$IFDEF DELPHI6UP}
type
  TPropertyEditorWithDialog = class
  private
    FPropName: AnsiString;
    procedure CheckEditProperty(const Prop: IProperty);
    procedure EditProperty(Component: TPersistent; const PropName: AnsiString; const Designer: IDesigner);
  end;


procedure TPropertyEditorWithDialog.CheckEditProperty(const Prop: IProperty);
begin
  if Prop.GetName = FPropName then
    Prop.Edit;
end;


procedure TPropertyEditorWithDialog.EditProperty(Component: TPersistent; const PropName: AnsiString; const Designer: IDesigner);
var
  Components: IDesignerSelections;
begin
  FPropName := PropName;
  Components := TDesignerSelections.Create;
  Components.Add(Component);
  GetComponentProperties(Components, [tkClass], Designer, CheckEditProperty);
end;


procedure EditPropertyWithDialog(Component: TPersistent; const PropName: AnsiString; const Designer: IDesigner);
begin
  with TPropertyEditorWithDialog.Create do
    try
      EditProperty(Component, PropName, Designer);
    finally
      Free;
    end;
end;


procedure TacListViewEditor.ExecuteVerb(Index: Integer);
begin
  case index of
    0: EditPropertyWithDialog(Component, 'Columns', Designer);
    1: EditPropertyWithDialog(Component, 'Items',   Designer);
  end;
end;


function TacListViewEditor.GetVerb(Index: Integer): string;
begin
  case index of
    0: Result := SListColumnsEditor;
    1: Result := SListItemsEditor;
  end;
end;


function TacListViewEditor.GetVerbCount: Integer;
begin
  Result := 2;
end;
{$ENDIF}


procedure TacFontStoreEditor.ExecuteVerb(Index: Integer);
begin
  case index of
    0: begin
      Application.CreateForm(TFormFontStore, FormFontStore);
      FormFontStore.InitEditor(Component as TsFontStore);
      if FormFontStore.ShowModal = mrOk then begin
        TsFontStore(Component).Assign(FormFontStore.sFontStore1);
        if Designer <> nil then
          Designer.Modified;
      end;
      FreeAndNil(FormFontStore);
    end;
  end;
end;


function TacFontStoreEditor.GetVerb(Index: Integer): string;
begin
  case index of
    0: Result := '&Font list editor...';
  end;
end;


function TacFontStoreEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;


procedure TacEmbeddedFontsProperty.Edit;
var
  Cmp: TsFontStore;
begin
  Cmp := TsFontStore(GetComponent(0));
  Application.CreateForm(TFormFontStore, FormFontStore);
  FormFontStore.InitEditor(Cmp);
  if FormFontStore.ShowModal = mrOk then begin
    Cmp.Assign(FormFontStore.sFontStore1);
    if Designer <> nil then
      Designer.Modified;
  end;
  FreeAndNil(FormFontStore);
end;


function TacEmbeddedFontsProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paAutoUpdate, paReadOnly];
end;

end.
