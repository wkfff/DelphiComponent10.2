unit sGlyphUtils;
{$I sDefs.inc}

interface

uses
  Windows, SysUtils, Classes, Graphics, Controls, buttons, ImgList,
  acAlphaImageList, sSpeedButton, sConst;


type
  TacAddedGlyph = class(TPersistent)
  private
    FBlend,
    FImageIndex: integer;

    FGrayed: boolean;
    FColorTone: TColor;
    FOwner: TWinControl;
    FImages: TCustomImageList;
    FImageChangeLink: TChangeLink;
    procedure ImageListChange(Sender: TObject);
    procedure SetInteger(const Index, Value: integer); virtual;
    procedure SetImages (const Value: TCustomImageList);
    procedure SetGrayed(const Value: boolean);
    procedure SetColorTone(const Value: TColor);
  public
    constructor Create(AOwner: TWinControl); virtual;
    destructor Destroy; override;
    function ImageCount: integer;
    function Height: Integer;
    function Width: Integer;
    procedure Invalidate(UpdateCoords: boolean = False);
  published
    property ColorTone: TColor read FColorTone write SetColorTone default clNone;
    property Grayed: boolean read FGrayed write SetGrayed default False;
    property Images: TCustomImageList read FImages write SetImages;
    property Blend:      integer index 0 read FBlend      write SetInteger default 0;
    property ImageIndex: integer index 1 read FImageIndex write SetInteger default -1;
  end;


{$IFNDEF ALITE}
  TsGlyphMode = class(TacAddedGlyph)
  private
    FImageIndexHot,
    FImageIndexPressed: integer;

    FHint: acString;
    FEnabled: boolean;
    procedure SetInteger(const Index, Value: integer); override;
    procedure SetEnabled(const Value: boolean);
  public
    constructor Create(AOwner: TWinControl); override;
  published
    property Enabled: boolean read FEnabled write SetEnabled default True;
    property Hint: acString read FHint write FHint;
    property ImageIndexHot:     integer index 10 read FImageIndexHot     write SetInteger default -1;
    property ImageIndexPressed: integer index 11 read FImageIndexPressed write SetInteger default -1;
  end;
{$ENDIF}

function acResImgList: TacImageList;


const
  iBTN_ARROW      = -1;
  iBTN_OPENFILE   = 0;
  iBTN_OPENFOLDER = 1;
  iBTN_DATE       = 2;
  iBTN_ELLIPSIS   = 3;
  iBTN_CALC       = 4;

  iServCharCount = 20;
  iServCharMin   = iServCharCount + 1;
  iServCharMax   = iServCharCount + 2;
  iServCharRest  = iServCharCount + 3;
  iServCharClose = iServCharCount + 4;


var
  acCharImages: TsCharImageList = nil;

implementation

uses
  {$IFNDEF ALITE}sCurrencyEdit, sCustomComboEdit, sMaskEdit,{$ENDIF}
  {$IFDEF UNICODE}PngImage, {$ENDIF}
  sComboBox, acntUtils, sThirdParty, sMessages, sVCLUtils, sEdit;


type
  TacCharData = record
    Char: word;
    Scaling: real;
    OffsetY: integer;
  end;

const
  acGlyphsResNames: array [0..4] of string = ('SF', 'SR', 'SD', 'SE', 'SC');

var
  acResImages: TsAlphaImageList = nil;


procedure MakeCharImages;
const
  aSysChars: array [0..3] of TacCharData = (
    (Char: $30; Scaling: 1; OffsetY: 0), // Min
    (Char: $31; Scaling: 1; OffsetY: 0), // Max
    (Char: $32; Scaling: 1; OffsetY: 0), // Restore
    (Char: $72; Scaling: 1; OffsetY: 0)  // Close
  );

  aChars: array [0..iServCharCount] of TacCharData = (
    (Char: $F016; Scaling:   1; OffsetY: 0), // OPENFILE      0
    (Char: $F115; Scaling:   1; OffsetY: 0), // OPENFOLDER    1
    (Char: $F274; Scaling:   1; OffsetY: 0), // DATE          2
    (Char: $F141; Scaling:   1; OffsetY: 4), // ELLIPSIS      3
    (Char: $F1EC; Scaling:   1; OffsetY: 0), // CALC          4
    (Char: $F192; Scaling:1.25; OffsetY: 0), // RADIOCHECKED  5
    (Char: $F00C; Scaling:   1; OffsetY: 0), // CHECKED       6
    (Char: $F047; Scaling: 0.9; OffsetY: 1), // MOVE          7
    (Char: $F1FB; Scaling:   1; OffsetY: 0), // GETPIX        8
    (Char: $F00C; Scaling: 1.1; OffsetY: 0), // OK            9
    (Char: $F00D; Scaling: 1.1; OffsetY: 0), // CANCEL        10
    (Char: $F067; Scaling: 0.8; OffsetY: 2), // PLUS          11
    (Char: $F0D8; Scaling: 1.3; OffsetY: 0), // EDIT          12
    (Char: $F01E; Scaling: 0.9; OffsetY: 1), // REFRESH       13
    (Char: $F048; Scaling: 0.8; OffsetY: 2), // FIRST         14
    (Char: $F053; Scaling: 0.8; OffsetY: 2), // PRIOR         15
    (Char: $F054; Scaling: 0.8; OffsetY: 2), // NEXT          16
    (Char: $F051; Scaling: 0.8; OffsetY: 2), // LAST          17
    (Char: $F068; Scaling: 0.8; OffsetY: 2), // MINUS         18
    (Char: $F00C; Scaling: 0.8; OffsetY: 0),  // OK small     19
    (Char: $F00D; Scaling: 0.8; OffsetY: 0)  // CANCEL small  20
  );

var
  i: integer;
begin
  acCharImages := TsCharImageList.Create(nil);
  acCharImages.BkColor := clWhite;
  acCharImages.Width := 16;
  acCharImages.Height := 14;
  for i := 0 to Length(aChars) - 1 do
    with TacCharListItem(acCharImages.Items.Add) do begin
      Char          := aChars[i].Char;
      ScalingFactor := aChars[i].Scaling;
      OffsetY       := aChars[i].OffsetY;
    end;

  for i := 0 to Length(aSysChars) - 1 do
    with TacCharListItem(acCharImages.Items.Add) do begin
      FontName      := s_Marlett;
      Char          := aSysChars[i].Char;
      ScalingFactor := aSysChars[i].Scaling;
      OffsetY       := aSysChars[i].OffsetY;
    end;

  acCharImages.GenerateStdList;
end;


procedure MakeResImages;
var
  s: TResourceStream;
  it: integer;
begin
  // Load glyphs from resources
  acResImages := TsAlphaImageList.Create(nil);
  acResImages.Width  := 32; // 16 x 2
  acResImages.Height := 16;
  for it := 0 to 4 do begin
    s := TResourceStream.Create(hInstance, acGlyphsResNames[it], RT_RCDATA);
    with TsImgListItem(acResImages.Items.Add) do begin
      ImgData.LoadFromStream(s);
      ImageFormat := ifPNG;
      PixelFormat := pf32bit;
    end;
    s.Free;
  end;
  acResImages.GenerateStdList;
end;


function acResImgList: TacImageList;
begin
  if ac_OldGlyphsMode then begin
    if acResImages = nil then
      MakeResImages;

    Result := acResImages;
  end
  else
    Result := acCharImages;
end;


{$IFNDEF ALITE}
constructor TsGlyphMode.Create(AOwner: TWinControl);
begin
  inherited;
  FOwner := AOwner;
  FEnabled := True;
  FImageIndexHot := -1;
  FImageIndexPressed := -1;
end;


procedure TsGlyphMode.SetEnabled(const Value: boolean);
begin
  if FEnabled <> Value then begin
    FEnabled := Value;
    Invalidate;
  end;
end;


procedure TsGlyphMode.SetInteger(const Index, Value: integer);

  procedure ChangeProp(var Prop: integer; Value: integer);
  begin
    if Prop <> Value then begin
      Prop := Value;
      Invalidate;
    end;
  end;

begin
  case Index of
    10: ChangeProp(FImageIndexHot, Value);
    11: ChangeProp(FImageIndexPressed, Value)
    else inherited;
  end;
end;
{$ENDIF} // IFNDEF ALITE


constructor TacAddedGlyph.Create(AOwner: TWinControl);
begin
  FOwner := AOwner;
  FGrayed := False;
  FBlend := 0;
  FColorTone := clNone;
  FImageIndex := -1;
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImageListChange;
end;


destructor TacAddedGlyph.Destroy;
begin
  FImageChangeLink.Free;
  inherited;
end;


function TacAddedGlyph.Height: Integer;
begin
  if Assigned(FImages) and IsValidIndex(ImageIndex, GetImageCount(FImages)) then
    Result := GetImageHeight(FImages, ImageIndex)
  else
    Result := GetImageHeight(acResImgList, ImageIndex);
end;


function TacAddedGlyph.ImageCount: integer;
var
  w, h: integer;
begin
  if Assigned(FImages) and IsValidIndex(ImageIndex, GetImageCount(FImages)) then begin
    w := GetImageWidth(FImages);
    h := GetImageHeight(FImages);
  end
  else begin
    w := GetImageWidth(acResImgList);
    h := GetImageHeight(acResImgList);
  end;
  if w mod h = 0 then
    Result := w div h
  else
    Result := 1;
end;


procedure TacAddedGlyph.ImageListChange(Sender: TObject);
begin
  Invalidate;
end;


procedure TacAddedGlyph.Invalidate;
begin
{$IFNDEF ALITE}
  if FOwner is TsMaskEdit then
    TsCustomComboEdit(FOwner).SkinData.Invalidate(True)
  else
{$ENDIF}
    if FOwner is TsEdit then
      TsEdit(FOwner).SkinData.Invalidate(True);

  if UpdateCoords then
    SetWindowPos(TWinControl(FOwner).Handle, 0, 0, 0, 0, 0, SWPA_FRAMECHANGED);
end;


procedure TacAddedGlyph.SetColorTone(const Value: TColor);
begin
  if FColorTone <> Value then begin
    FColorTone := Value;
    Invalidate;
  end;
end;


procedure TacAddedGlyph.SetGrayed(const Value: boolean);
begin
  if FGrayed <> Value then begin
    FGrayed := Value;
    Invalidate;
  end;
end;


procedure TacAddedGlyph.SetImages(const Value: TCustomImageList);
begin
  if FImages <> Value then begin
    if Images <> nil then
      Images.UnRegisterChanges(FImageChangeLink);

    FImages := Value;
    if Images <> nil then begin
      Images.RegisterChanges(FImageChangeLink);
      Images.FreeNotification(FOwner);
    end;
    Invalidate(True);
  end;
end;


procedure TacAddedGlyph.SetInteger(const Index, Value: integer);

  procedure ChangeProp(var Prop: integer; Value: integer; UpdateCoords: boolean = False);
  begin
    if Prop <> Value then begin
      Prop := Value;
      Invalidate(UpdateCoords);
    end;
  end;

begin
  case Index of
    0: ChangeProp(FBlend, Value);
    1: ChangeProp(FImageIndex, Value, True);
  end;
end;


function TacAddedGlyph.Width: Integer;
begin
{$IFNDEF ALITE}
  if (FOwner is TsCustomComboEdit) and TsCustomComboEdit(FOwner).ComboBtn and (Self is TsGlyphMode) then
    Result := GetSystemMetrics(SM_CXVSCROLL) - 3
  else
{$ENDIF}
    if Assigned(FImages) and IsValidIndex(ImageIndex, GetImageCount(FImages)) then
      Result := GetImageWidth(FImages) div ImageCount
    else
      Result := GetImageWidth(acResImgList) div ImageCount;
end;


initialization
  if acCharImages = nil then
    MakeCharImages;


finalization
  if acResImages <> nil then
    FreeAndNil(acResImages);

  if acCharImages <> nil then
    FreeAndNil(acCharImages);
end.
