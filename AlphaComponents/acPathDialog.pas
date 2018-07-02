unit acPathDialog;
{$I sDefs.inc}

interface

uses
  Windows, SysUtils, Classes, Graphics, Controls, Forms, Buttons, ComCtrls, ImgList, FileCtrl, StdCtrls,
  sBitBtn, acShellCtrls, sSkinProvider, sTreeView, sScrollBox, sLabel, sConst, acAlphaImageList;


type
  TPathDialogForm = class(TForm)
    sLabel1: TsLabel;
    sBitBtn1: TsBitBtn;
    sBitBtn2: TsBitBtn;
    sBitBtn3: TsBitBtn;
    sScrollBox1: TsScrollBox;
    ImageList1: TsAlphaImageList;
    sSkinProvider1: TsSkinProvider;
    sShellTreeView1: TsShellTreeView;
    procedure sShellTreeView1Change(Sender: TObject; Node: TTreeNode);
    procedure sBitBtn3Click(Sender: TObject);
    procedure sBitBtn2Click(Sender: TObject);
    procedure sBitBtn1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  public
    procedure Loaded; override;
    procedure InitLngCaptions;
    procedure GenerateButtons;
    procedure UpdateAnchors;
    procedure BtnOnClick(Sender: TObject);
  end;


{$IFDEF DELPHI_XE3}[ComponentPlatformsAttribute(pidWin32 or pidWin64)]{$ENDIF}
  TsPathDialog = class(TComponent)
{$IFNDEF NOTFORHELP}
  private
    FRoot: TacRoot;
    FPath: TsDirectory;
    FCaption: acString;
    FNoChangeDir: Boolean;
    FShowRootBtns: Boolean;
    FOptions: TSelectDirOpts;
  public
    constructor Create(AOwner: TComponent); override;
{$ENDIF} // NOTFORHELP
    function Execute: Boolean;
  published
    property Path: TsDirectory read FPath write FPath;
    property Root: TacRoot read FRoot write FRoot;
    property Caption: acString read FCaption write FCaption;
    property NoChangeDir: boolean read FNoChangeDir write FNoChangeDir default False;
    property ShowRootBtns: boolean read FShowRootBtns write FShowRootBtns default False;
{$IFNDEF NOTFORHELP}
    property DialogOptions: TSelectDirOpts read FOptions write FOptions default [sdAllowCreate, sdPerformCreate, sdPrompt];
{$ENDIF} // NOTFORHELP
  end;


var
  PathDialogForm: TPathDialogForm;
  FLargeImages: integer = 0;


implementation

uses
  TypInfo, ShlObj, ShellAPI, CommCtrl,
  sThirdParty, acntUtils, acSBUtils, sSpeedButton, sSkinProps, sGlyphUtils, sSkinManager, sVCLUtils;

{$R *.DFM}

procedure TPathDialogForm.sShellTreeView1Change(Sender: TObject; Node: TTreeNode);
begin
  sBitBtn1.Enabled := acDirExists(sShellTreeView1.Path);
  sBitBtn3.Enabled := sBitBtn1.Enabled;
end;


procedure TPathDialogForm.sBitBtn3Click(Sender: TObject);
var
  NewName: string;
  TreeNode: TTreeNode;
  i: integer;

  function GetUniName: string;
  var
    i: integer;
  begin
    for i := 1 to maxint - 1 do begin
      Result := NormalDir(sShellTreeView1.SelectedFolder.PathName) + s_NewFolder + IntToStr(i);
      if not acDirExists(Result) then
        Exit
    end;
    Result := '';
  end;


begin
  NewName := GetUniName;
  CreateDir(NewName);
  if not acDirExists(NewName) then
    ShowError('Directory ' + NewName + ' can`t be created!')
  else begin
    sShellTreeView1.Refresh(sShellTreeView1.Selected);
    RefreshScrolls(sShellTreeView1.SkinData, sShellTreeView1.ListSW);
    for i := 0 to sShellTreeView1.Selected.Count - 1 do begin
      TreeNode := sShellTreeView1.Selected.Item[i];
      if sShellTreeView1.Folders[TreeNode.AbsoluteIndex].PathName = NewName then begin
        TreeNode.Selected := True;
        sShellTreeView1.SetFocus;
        TreeNode.Expanded := True;
        TreeNode.EditText;
        sShellTreeView1.Path := NewName;
        Break;
      end;
    end;
  end;
end;


procedure TPathDialogForm.sBitBtn2Click(Sender: TObject);
begin
  if sShellTreeView1.IsEditing then
    ModalResult := mrNone
  else
    ModalResult := mrCancel;

  inherited;
end;


procedure TPathDialogForm.sBitBtn1Click(Sender: TObject);
begin
  if sShellTreeView1.IsEditing then begin
    ModalResult := mrNone;
    sShellTreeView1.Selected.EndEdit(False);
    sShellTreeView1.SetFocus;
  end
  else
    ModalResult := mrOk;
end;


procedure TPathDialogForm.InitLngCaptions;
begin
  Caption          := acs_SelectDir;
  sLabel1.Caption  := acs_Root;
  sBitBtn1.Caption := acs_MsgDlgOK;
  sBitBtn2.Caption := acs_MsgDlgCancel;
  sBitBtn3.Caption := acs_Create;
end;


procedure TPathDialogForm.Loaded;
begin
  InitLngCaptions;
  inherited;
end;

function Btn_AutoSize(Btn: TsSpeedButton): Boolean;
var
  iL_BtnHeight: Integer;
  tL_TextSize: TSize;
begin
  if Btn <> nil then begin
    tL_TextSize := Btn.TextRectSize;
    if Btn.Layout in [blGlyphTop, blGlyphBottom] then
      iL_BtnHeight := GetImageHeight(Btn.Images) + tL_TextSize.cy + Btn.Spacing + 2 * Btn.Margin
    else
      iL_BtnHeight := Maxi(GetImageHeight(Btn.Images), tL_TextSize.cy) + (2 * Btn.Margin);

    if iL_BtnHeight <> Btn.Height then
      Btn.Height := iL_BtnHeight;

    Result := True;
  end
  else
    Result := False;
end;


procedure TPathDialogForm.FormCreate(Sender: TObject);
begin
  sBitBtn1.Images := acCharImages;
  sBitBtn2.Images := acCharImages;
  sBitBtn3.Images := acCharImages;
end;


procedure TPathDialogForm.FormShow(Sender: TObject);
begin
  sShellTreeView1.Height := sScrollBox1.Height;
  if BidiMode = bdRightToLeft then
    ReflectControls(Self, True);
end;


procedure TPathDialogForm.GenerateButtons;
var
  FileInfo: TSHFileInfo;

  procedure MakeBtn(Folder: TacRootFolder);
  var
    Btn: TsSpeedButton;
    NewPIDL: PItemIDList;
    Fldr: TacShellFolder;
  begin
    Btn := TsSpeedButton.Create(sScrollBox1);
    Btn.Layout := blGlyphTop;
    Btn.Margin := 10;
    Btn.Flat := True;
    SHGetSpecialFolderLocation(0, nFolder[Folder], NewPIDL);
    Btn.Caption := GetDisplayName(DesktopShellFolder, NewPIDL, SHGDN_NORMAL, seHide);

    Fldr := CreateRootFromPIDL(NewPIDL);
    Btn.ImageIndex := GetShellImage(Fldr.AbsoluteID, False, False);
    FreeAndNil(Fldr);
    Btn.Images := ImageList1;

    Btn.Tag := ord(Folder);
    Btn.Align := alTop;
    Btn.OnClick := BtnOnClick;
    Btn.Parent := sScrollBox1;
    Btn_AutoSize(Btn);
  end;

begin
  sScrollBox1.Color := clAppworkSpace;
  if (FLargeImages = 0) or (ImageList_GetImageCount(FLargeImages) = 0) then
    FLargeImages := SHGetFileInfo('C:\', SFGAO_SHARE, FileInfo, SizeOf(FileInfo), SHGFI_SYSICONINDEX or SHGFI_LARGEICON);

  if FLargeImages <> 0 then begin
    ImageList1.acBeginUpdate;
    ImageList1.Handle := FLargeImages;
    ImageList1.StdMode := True;
    ImageList1.AllowScale := False;
    ImageList1.BkColor := clNone;
    ImageList1.UpdateFromStd;
    ImageList1.acEndUpdate;
  end;
  MakeBtn(rfDesktop);
  MakeBtn(rfMyComputer);
  MakeBtn(rfPersonal);
  MakeBtn(rfNetwork);
end;


procedure TPathDialogForm.BtnOnClick(Sender: TObject);
begin
  sShellTreeView1.Root := GetEnumName(TypeInfo(TacRootFolder), TsSpeedButton(Sender).Tag);
end;


procedure TPathDialogForm.UpdateAnchors;
begin
  DisableAlign;
  sShellTreeView1.Anchors := sShellTreeView1.Anchors + [akRight, akBottom];
  sScrollBox1.Anchors := sScrollBox1.Anchors + [akBottom];
  sBitBtn1.Anchors := [akRight, akBottom];
  sBitBtn2.Anchors := [akRight, akBottom];
  sBitBtn3.Anchors := [akLeft, akBottom];
  RefreshScrolls(sShellTreeView1.SkinData, sShellTreeView1.ListSW);
  sBitBtn3.Left := sShellTreeView1.Left;
  EnableAlign;
end;


constructor TsPathDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRoot := SRFDesktop;
  FNoChangeDir := False;
  FShowRootBtns := False;
  FOptions := [sdAllowCreate, sdPerformCreate, sdPrompt];
end;


function TsPathDialog.Execute: Boolean;
var
  s: acString;
  bw: integer;
begin
  Result := False;
  s := Path;
  if not acDirExists(s) or (s = s_Dot) then
    s := '';

  if not NoChangeDir and (s <> '') then
    acSetCurrentDir(s);

  PathDialogForm := TPathDialogForm.Create(Application);
  PathDialogForm.sBitBtn3.Visible := sdAllowCreate in DialogOptions;
  if ShowRootBtns then begin
    PathDialogForm.sScrollBox1.Visible := True;
    PathDialogForm.sLabel1.Visible := True;
    bw := GetSystemMetrics(SM_CXSIZEFRAME);
    PathDialogForm.sShellTreeView1.Left := PathDialogForm.sShellTreeView1.Left + PathDialogForm.sScrollBox1.Width + 4;
    PathDialogForm.sBitBtn1.Left := PathDialogForm.sBitBtn1.Left + PathDialogForm.sScrollBox1.Width + 4;
    PathDialogForm.sBitBtn2.Left := PathDialogForm.sBitBtn2.Left + PathDialogForm.sScrollBox1.Width + 4;

    PathDialogForm.Width := PathDialogForm.Width + PathDialogForm.sScrollBox1.Width + 4 + bw;
    PathDialogForm.GenerateButtons;
  end
  else begin
    PathDialogForm.sScrollBox1.Visible := False;
    PathDialogForm.sLabel1.Visible := False;
  end;
  PathDialogForm.UpdateAnchors;
  try
    PathDialogForm.sShellTreeView1.BoundLabel.Caption := Caption;
    PathDialogForm.sShellTreeView1.Root := FRoot;
    if (s = '') then begin
      if PathDialogForm.sShellTreeView1.Items.Count > 0 then
        PathDialogForm.sShellTreeView1.Items[0].Selected := True;
    end
    else
      PathDialogForm.sShellTreeView1.Path := s;

    if PathDialogForm.ShowModal = mrOk then begin
      s := PathDialogForm.sShellTreeView1.Path;
      if (s <> '') and acDirExists(s) then
        Path := s;
        
      Result := True
    end;
  finally
    FreeAndNil(PathDialogForm);
  end;
end;

end.
