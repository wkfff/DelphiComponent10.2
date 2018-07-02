unit acAlphaImageList;
{$I sDefs.inc}
//{$DEFINE ACDEBUG}

interface

uses
  Windows, Classes, SysUtils, Controls, Graphics, CommCtrl, ImgList,
  {$IFNDEF DELPHI5} Types, {$ENDIF}
  {$IFDEF DELPHI_XE2}UITypes, {$ENDIF}
  sConst, acntUtils, acFontStore;


type
  TsImageFormat = (ifPNG, ifICO, ifBMP32);
{$IFNDEF NOTFORHELP}
  TsAlphaImageList = class;
  TsImgListItems = class;


  TsImgListItem = class(TCollectionItem)
  private
    FText,
    FImageName: string;
    FImageFormat: TsImageFormat;
    FPixelFormat: TPixelFormat;
  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadData(Reader: TStream);
    procedure WriteData(Writer: TStream);
  public
    OrigWidth,
    OrigHeight: integer;
    CacheBmp: TBitmap;
    ImgData: TMemoryStream;
    procedure Assign(Source: TPersistent); override;
    procedure AssignTo(Dest: TPersistent); override;
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
  published
    property ImageFormat: TsImageFormat read FImageFormat write FImageFormat;
    property ImageName: string read FImageName write FImageName stored True;
    property PixelFormat: TPixelFormat read FPixelFormat write FPixelFormat default pf32bit;
    property Text: string read FText write FText;
  end;


  TsImgListItems = class(TCollection)
  protected
    FOwner: TsAlphaImageList;
    function  GetItem(Index: Integer): TsImgListItem;
    procedure SetItem(Index: Integer; Value: TsImgListItem);
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TsAlphaImageList);
    destructor Destroy; override;
    property Items[Index: Integer]: TsImgListItem read GetItem write SetItem; default;
  end;
{$ENDIF}


  TacImageList = class(TImageList)
  protected
{$IFNDEF NOTFORHELP}
  {$IFNDEF ACDEBUG}
    ForeColor: TColor;
  {$ELSE}
    FForeColor: TColor;
    procedure SetForeColor(const Value: TColor);
  {$ENDIF}
  {$IFDEF DELPHI7UP}
    procedure ReadData (Stream: TStream); override;
    procedure WriteData(Stream: TStream); override;
  {$ENDIF}
    procedure AddAsIcon(Bmp: TBitmap; Ndx: integer);
  public
    IgnoreTransparency: boolean;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
{$IFNDEF DELPHI_XE8}
    function Count: integer; virtual; abstract;
{$ENDIF}
{$ENDIF} // NOTFORHELP
    function CreateBitmap32(Index: Integer; aWidth, aHeight: integer; GlyphHeight: integer = 0): TBitmap; virtual; abstract;
    function GetBitmap32(Index: Integer; Image: TBitmap; GlyphHeight: integer = 0): Boolean; virtual; abstract;
  {$IFDEF ACDEBUG}
    property ForeColor: TColor read FForeColor write SetForeColor;
  {$ENDIF}
  end;


{$IFDEF DELPHI_XE3}[ComponentPlatformsAttribute(pidWin32 or pidWin64)]{$ENDIF}
  TsAlphaImageList = class(TacImageList)
{$IFNDEF NOTFORHELP}
  private
    FLoaded,
    FUseCache,
    AcChanging,
    FAllowScale,
    StdListIsGenerated: boolean;

    FItems: TsImgListItems;
    FBkColor: TColor;
    function GetBkColor: TColor;
    procedure SetItems   (const Value: TsImgListItems);
    procedure SetBkColor (const Value: TColor);
    procedure SetUseCache(const Value: boolean);
    function GetDimension(const Index: Integer): integer;
    procedure SetDimension(const Index, Value: integer);
  protected
    FSavedScale,
    SavedHeight,
    SavedWidth: integer;
    procedure ScaleSize;
    procedure SetNewScale(Value: Integer);

    procedure CreateImgList;
    function CanScale: boolean;
    procedure DoDraw(Index: Integer; Canvas: TCanvas; X, Y: Integer; Style: Cardinal; Enabled: Boolean = True); {$IFNDEF FPC}override;{$ENDIF}
    procedure KillImgList;
    function IsDuplicated: boolean;
    procedure ItemsClear;
  public
    StdMode: boolean;
    DoubleData: boolean;
    procedure AcBeginUpdate;
    procedure AcEndUpdate(DoChange: boolean = True);
    procedure Change; {$IFNDEF FPC}override;{$ENDIF}
    procedure Clear; reintroduce;
    procedure Replace(AIndex: integer; ABmp32: TBitmap);
    function Add(Image, Mask: TBitmap): Integer;
    procedure AfterConstruction; override;
    procedure Assign(Source: TPersistent); override;
    procedure AssignTo(Dest: TPersistent); override;
    procedure CopyImages(const ImgList: TsAlphaImageList);
{$IFNDEF DELPHI_XE8}
    function Count: integer; override;
{$ELSE}
    function GetCount: Integer; override;
{$ENDIF}
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateFromStd;
    procedure GenerateStdList;
    function GetBitmap32(Index: Integer; Image: TBitmap; GlyphHeight: integer = 0): Boolean; override;
    function CreateBitmap32(Index: Integer; aWidth, aHeight: integer; GlyphHeight: integer = 0): TBitmap; override;
    function AddImage(Value: TCustomImageList; Index: Integer): Integer; //reintroduce;
    procedure Loaded; override;
    procedure LoadFromFile(const FileName: acString);
    procedure LoadFromPngStream(const Stream: TStream);
    function TryLoadFromFile(const FileName: acString): boolean;
    function TryLoadFromPngStream(Stream: TStream): Boolean;
    procedure MoveItem(CurIndex, NewIndex: integer);
    procedure SetNewDimensions(Value: HImageList);
    property ScaleValue: integer read FSavedScale write SetNewScale default 0;
    property AllowScale: boolean read FAllowScale write FAllowScale default True;
  published
    property Height index 0 read GetDimension write SetDimension;
    property Width  index 1 read GetDimension write SetDimension;

    property BkColor: TColor read GetBkColor write SetBkColor default clNone;
    property Items: TsImgListItems read FItems write SetItems;
    property UseCache: boolean read FUseCache write SetUseCache default True;
{$ENDIF}
  end;


{$IFDEF DELPHI_XE3}[ComponentPlatformsAttribute(pidWin32 or pidWin64)]{$ENDIF}
  TsVirtualImageList = class(TCustomImageList)
{$IFNDEF NOTFORHELP}
  private
    FWidth,
    FHeight: integer;

    FUseCache,
    AcChanging,
    StdListIsGenerated: boolean;

    CachedImages: array of TBitmap;
    FImageChangeLink: TChangeLink;
    FAlphaImageList: TacImageList;
    FForeColor: TColor;
    procedure SetAlphaImageList(const Value: TacImageList);
    procedure SetUseCache      (const Value: boolean);
    procedure SetInteger(const Index, Value: integer);
    procedure SetForeColor(const Value: TColor);
  protected
{$IFDEF DELPHI7UP}
    procedure ReadData (Stream: TStream); override;
    procedure WriteData(Stream: TStream); override;
{$ENDIF}
    procedure CreateImgList;
    procedure DoDraw(Index: Integer; Canvas: TCanvas; X, Y: Integer; Style: Cardinal; Enabled: Boolean = True); {$IFNDEF FPC}override;{$ENDIF}
    procedure KillImgList;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure ImageListChange(Sender: TObject);
    procedure AddAsIcon(Bmp: TBitmap; Ndx: integer);
    property ForeColor: TColor read FForeColor write SetForeColor;
  public
    CurrentScale: integer;
    function ScaleValue: real;
    procedure AcBeginUpdate;
    procedure AcEndUpdate(DoChange: boolean = True);
    procedure Change; {$IFNDEF FPC}override;{$ENDIF}
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
{$IFNDEF DELPHI_XE8}
    function Count: integer;
{$ELSE}
    function GetCount: Integer; override;
{$ENDIF}
    procedure GenerateStdList;
    function GetBitmap32(Index: Integer; Image: TBitmap; GlyphHeight: integer = 0): Boolean;
    function CreateBitmap32(Index: Integer): TBitmap;
    procedure ClearItems;
    procedure Loaded; override;
    procedure RenderCacheNow(ItemIndex: integer = -1);
    procedure UpdateList(IgnoreGenerated: boolean = True);
    procedure SetCharColor(AColor: TColor; AIndex: integer; IgnoreDefault: boolean);
  published
    property Height: integer Index 0 read FHeight write SetInteger default 16;
    property Width:  integer Index 1 read FWidth  write SetInteger default 16;
{$ENDIF}
    property AlphaImageList: TacImageList read FAlphaImageList write SetAlphaImageList;
    property UseCache: boolean read FUseCache write SetUseCache default True;
  end;


{$IFNDEF NOTFORHELP}
  TsCharImageList = class;

  TacCharListItem = class(TCollectionItem)
  private
    FChar: Word;
    FImageName: string;
    FPitch: TFontPitch;
    FColor: TColor;
    FCharset: TFontCharset;
    FFontName: string;
    FStyle: TFontStyles;
    FOrientation: integer;
    FScalingFactor: real;
    FOffsetY: integer;
    procedure SetCharset(const Value: TFontCharset);
    procedure SetColor(const Value: TColor);
    procedure SetFontName(const Value: string);
    procedure SetOrientation(const Value: integer);
    procedure SetPitch(const Value: TFontPitch);
    procedure SetStyle(const Value: TFontStyles);
    function GetImage: TBitmap;
    function ImgList: TsCharImageList;
    procedure SetChar(const Value: Word);
    procedure SetScalingFactor(const Value: real);
    procedure SetOffsetY(const Value: integer);
    function NotDefScaling: boolean;
    function NotDefFont: boolean;
  public
    CacheBmp: TBitmap;
    procedure Assign(Source: TPersistent); override;
    procedure AssignTo(Dest: TPersistent); override;
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    procedure Invalidate;
  published
    property Image: TBitmap read GetImage;
    property FontName: string read FFontName write SetFontName stored NotDefFont;
    property Charset: TFontCharset read FCharset write SetCharset default DEFAULT_CHARSET;
    property Color: TColor read FColor write SetColor default clNone;
    property Orientation: integer read FOrientation write SetOrientation default 0;
    property Pitch: TFontPitch read FPitch write SetPitch default fpDefault;
    property Style: TFontStyles read FStyle write SetStyle default [];
    property Char: Word read FChar write SetChar;
    property ScalingFactor: real read FScalingFactor write SetScalingFactor stored NotDefScaling;
    property OffsetY: integer read FOffsetY write SetOffsetY default 0;
    property ImageName: string read FImageName write FImageName stored True;
  end;


  TacCharListItems = class(TCollection)
  protected
    FOwner: TsCharImageList;
    function  GetItem(Index: Integer): TacCharListItem;
    procedure SetItem(Index: Integer; Value: TacCharListItem);
    function GetOwner: TPersistent; override;
    procedure CheckItems;
  public
    constructor Create(AOwner: TsCharImageList);
    destructor Destroy; override;
    property Items[Index: Integer]: TacCharListItem read GetItem write SetItem; default;
  end;


  TacCharItemData = record
    FontName:    string;
    Charset:     TFontCharset;
    Color:       TColor;
    Orientation: integer;
    Handle:      hFont;
    Pitch:       TFontPitch;
    Style:       TFontStyles;
    Char:        Word;
    ScalingFactor: real;
    OffsetY:     integer;
  end;


  TacEmbeddedFont = class(TCollectionItem)
  protected
    FFileName,
    FFontName: acString;
    Handle: THandle;
    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadData(Reader: TStream);
    procedure WriteData(Writer: TStream);
  public
    FontData: TMemoryStream;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure AssignTo(Dest: TPersistent); override;
    constructor Create(ACollection: TCollection); override;
    procedure UnLoadFont;
    procedure LoadFont;
  published
    property FileName: acString read FFileName write FFileName;
    property FontName: acString read FFontName write FFontName;
  end;


  TacEmbeddedFonts = class(TCollection)
  protected
    FOwner: TsCharImageList;
    function GetItem(Index: Integer): TacEmbeddedFont;
    procedure SetItem(Index: Integer; const Value: TacEmbeddedFont);
    function GetOwner: TPersistent; override;
  public
    procedure Assign(Source: TPersistent); override;
    destructor Destroy; override;
    constructor Create(AOwner: TsCharImageList);
    property Items[Index: Integer]: TacEmbeddedFont read GetItem write SetItem; default;
  end;
{$ENDIF} // NOTFORHELP


{$IFDEF DELPHI_XE3}[ComponentPlatformsAttribute(pidWin32 or pidWin64)]{$ENDIF}
  TsCharImageList = class(TacImageList)
{$IFNDEF NOTFORHELP}
  private
    AcChanging,
    FAllowScale,
    StdListIsGenerated: boolean;

    FBkColor: TColor;
    FItems: TacCharListItems;
    FEmbeddedFonts: TacEmbeddedFonts;
    function GetBkColor: TColor;
    procedure SetItems   (const Value: TacCharListItems);
    procedure SetBkColor (const Value: TColor);
    function GetDimension(const Index: Integer): integer;
    procedure SetDimension(const Index, Value: integer);
    procedure SetEmbeddedFonts(const Value: TacEmbeddedFonts);
  protected
    FLoaded: boolean;
    FSavedScale,
    SavedHeight,
    SavedWidth: integer;
    procedure ScaleSize;
    procedure SetNewScale(Value: Integer);

    procedure CreateImgList;
    function CanScale: boolean;
    procedure DoDraw(Index: Integer; Canvas: TCanvas; X, Y: Integer; Style: Cardinal; Enabled: Boolean = True); override;
    procedure PrepareBmp(Bmp: TBitmap; Index: integer; const aRect: TRect; CharHeight: integer; Offset: integer);
    procedure KillImgList;

    procedure UnLoadAllFonts;
    procedure LoadAllFonts;
  public
    procedure AcBeginUpdate;
    procedure AcEndUpdate(DoChange: boolean = True);
    procedure UpdateStd(i: integer);
    function AddItem(ItemData: TacCharItemData): integer;
    procedure SetCharColor(AColor: TColor; AIndex: integer; IgnoreDefault: boolean);
    procedure Change; override;
    procedure Clear; reintroduce;
    procedure ClearCache;
    procedure AfterConstruction; override;
    procedure Assign(Source: TPersistent); override;
    procedure AssignTo(Dest: TPersistent); override;
    procedure CopyImages(const ImgList: TsCharImageList);
    function FontIndex(const aFontName: acString): integer;
{$IFNDEF DELPHI_XE8}
    function Count: integer; override;
{$ELSE}
    function GetCount: Integer; override;
{$ENDIF}
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure GenerateStdList;
    function GetBitmap32(Index: Integer; Image: TBitmap; GlyphHeight: integer = 0): Boolean; override;
    function CreateBitmap32(Index: Integer; aWidth, aHeight: integer; GlyphHeight: integer = 0): TBitmap; override;
    function CreateBitmap32Color(Index: Integer; aWidth, aHeight: integer; CharColor: TColor; GlyphHeight: integer = 0): TBitmap;
    procedure Loaded; override;
    procedure SetNewDimensions(Value: HImageList);
    function AddEmbeddedFont(FileName, FontName: acString): boolean;
    property ScaleValue: integer read FSavedScale write SetNewScale default 0;
    property AllowScale: boolean read FAllowScale write FAllowScale default True;
  published
    property Height index 0 read GetDimension write SetDimension default 24;
    property Width  index 1 read GetDimension write SetDimension default 24;

    property EmbeddedFonts: TacEmbeddedFonts read FEmbeddedFonts write SetEmbeddedFonts;
    property Items: TacCharListItems read FItems write SetItems;
    property BkColor: TColor read GetBkColor write SetBkColor default clNone;
{$ENDIF}
  end;


{$IFNDEF NOTFORHELP}
TIterImagesProc = procedure(ImgList: TCustomImageList; Data: Longint);

procedure IterateImageLists(CallBack: TIterImagesProc; Data: Longint);
function GetImageFormat(const FileName: acString; var ImageFormat: TsImageFormat): boolean; overload;
function GetImageFormat(const Stream: TStream; var ImageFormat: TsImageFormat): boolean; overload;
function DrawAlphaImgList(const ImgList: TCustomImageList; const DestBmp: TBitmap; const Left: integer; const Top: integer;
                          const ImageIndex: integer; const Blend: integer; const GrayedColor: TColor; State: integer;
                          const NumGlyphs: integer; const Reflected: boolean): TSize;
procedure DrawAlphaImgListDC(const ImgList: TCustomImageList; const DC: hdc; const Left: integer; const Top: integer;
                             const ImageIndex: integer; const Blend: integer; const GrayedColor: TColor; const State: integer;
                             const NumGlyphs: integer; const Reflected: boolean);

function GetDefScaling(CharSet: TFontCharset; FontName: string): real;
{$ENDIF}
function AddImageFromRes(aInstance: LongWord; ImageList: TsAlphaimageList; const ResName: String; aImageFormat: TsImageFormat): Boolean; // Png must be compiled in resource as RcData

implementation

uses
  math, ShellAPI, Dialogs, ActiveX,
  {$IFDEF DELPHI7UP} Themes, {$ENDIF}
  sAlphaGraph, sThirdParty, sSkinManager, sGraphUtils, acgpUtils, sVCLUtils;


var
  acListsArray: TList = nil;


procedure IterateImageLists(CallBack: TIterImagesProc; Data: Longint);
var
  i: integer;
begin
  for i := 0 to acListsArray.Count - 1 do
    CallBack(TCustomImageList(acListsArray[i]), Data);
end;


function GetBpp: integer;
var
  ScreenDC: hdc;
begin
  ScreenDC := GetDC(0);
  try
    Result := GetDeviceCaps(ScreenDC, BITSPIXEL);
  finally
    ReleaseDC(0, ScreenDC)
  end;
end;


function HaveMagic(const Stream: TStream; const Magic: Pointer; const Size: integer): Boolean; overload;
var
  MagicBuf: array [0..7] of Byte;
  len: integer;
begin
  FillChar(MagicBuf, 8, #0);
  len := min(Size, SizeOf(MagicBuf));
  Result := (Stream.Size - Stream.Position) > len;
  if Result then begin
    Stream.ReadBuffer(MagicBuf, len);
    Result := CompareMem(@MagicBuf, Magic, len);
  end;
end;


function HaveMagic(const FileName: string; const Magic: Pointer; const Size: integer): Boolean; overload;
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  Result := HaveMagic(Stream, Magic, Size);
end;


function GetImageFormat(const FileName: acString; var ImageFormat: TsImageFormat): boolean;
const
  IcoMagic: array [0..1] of Byte = (0, 0);
  BmpMagic: array [0..1] of Byte = (66, 77);
var
  s: string;
begin
  Result := False;
  // Check format
  if HaveMagic(FileName, @PNGMagic, 8) then begin // Png
    ImageFormat := ifPNG;
    Result := True;
  end
  else
    if HaveMagic(FileName, @IcoMagic, 2) then begin // Ico
      s := LowerCase(ExtractFileExt(FileName));
      System.Delete(s, 1, 1);
      if s = acIcoExt then begin
        ImageFormat := ifICO;
        Result := True;
      end;
    end
    else
      if HaveMagic(FileName, @BmpMagic, 2) then begin // Bmp32
        ImageFormat := ifBMP32;
        Result := True;
      end;
end;


function GetImageFormat(const Stream: TStream; var ImageFormat: TsImageFormat): boolean;
begin
  ImageFormat := ifPNG;
  Result := True;
end;


function DrawAlphaImgList(const ImgList: TCustomImageList; const DestBmp: TBitmap; const Left: integer; const Top: integer;
  const ImageIndex: integer; const Blend: integer; const GrayedColor: TColor; State: integer; const NumGlyphs: integer; const Reflected: boolean): TSize;
var
  Bmp: TBitmap;
  R1, R2: TRect;
  vil: TsVirtualImageList;
  imgWidth, imgHeight, w, Count: integer;
begin
  if (DestBmp.Width > 0) and ImgList.HandleAllocated and (ImageIndex >= 0) then begin
    imgWidth  := GetImageWidth (ImgList, ImageIndex);
    imgHeight := GetImageHeight(ImgList, ImageIndex);
    Count := max(1, NumGlyphs);
    w := imgWidth div Count;
    if State >= Count then
      State := Count - 1;

    R1.Left := Left + (GetImageWidth(ImgList) - imgWidth) div 2;
    R1 := Rect(R1.Left, Top, R1.Left + w, Top + ImgHeight);
    R2 := MkRect(w, ImgHeight);
    Result := MkSize(w, ImgHeight);
    OffsetRect(R2, w * State, 0);

    if ImgList.BkColor <> clNone then
      FillDC(DestBmp.Canvas.Handle, R1, ColorToRGB(ImgList.BkColor));

    Bmp := nil;
    if ImgList is TacImageList then begin
      if ImgList is TsCharImageList then
        TsCharImageList(ImgList).SetCharColor(DestBmp.Canvas.Font.Color, ImageIndex, DestBmp.Canvas.Font.Size = 0)
      else
        TacImageList(ImgList).ForeColor := DestBmp.Canvas.Font.Color;

      Bmp := TacImageList(ImgList).CreateBitmap32(ImageIndex, ImgWidth, ImgHeight);

      if R2.Left < 0 then begin
        R1.Left := R1.Left + R2.Left;
        R2.Left := 0;
      end;
      if R2.Top < 0 then begin
        R1.Top := R1.Top + R2.Top;
        R2.Top := 0;
      end;
      R2.Right := min(Bmp.Width, R2.Right);
      R2.Bottom := min(Bmp.Height, R2.Bottom);

      TacImageList(ImgList).ForeColor := clNone;
      if Bmp <> nil then
        if TacImageList(ImgList).IgnoreTransparency then
          BitBlt(DestBmp.Canvas.Handle, R1.Left, R1.Top, Bmp.Width, Bmp.Height, Bmp.Canvas.Handle, 0, 0, SRCCOPY)
        else
          CopyBmp32(R1, R2, DestBmp, Bmp, EmptyCI, False, GrayedColor, Blend, Reflected);
    end
    else
      if ImgList is TsVirtualImageList then begin
        vil := TsVirtualImageList(ImgList);
        vil.SetCharColor(DestBmp.Canvas.Font.Color, ImageIndex, DestBmp.Canvas.Font.Size = 0);
        if vil.UseCache then begin
          if Length(vil.CachedImages) > ImageIndex then begin
            if (vil.CachedImages[ImageIndex] <> nil) and
                 ((vil.CachedImages[ImageIndex].Width <> imgWidth) or (vil.CachedImages[ImageIndex].Height <> imgHeight) or (vil.CachedImages[ImageIndex].Canvas.Font.Color <> vil.ForeColor)) then
              FreeAndNil(vil.CachedImages[ImageIndex]);

            if vil.CachedImages[ImageIndex] = nil then
              vil.CachedImages[ImageIndex] := vil.AlphaImageList.CreateBitmap32(ImageIndex, ImgWidth, ImgHeight, GetImageHeight(vil));

            if vil.AlphaImageList.IgnoreTransparency then
              with vil.CachedImages[ImageIndex] do
                BitBlt(DestBmp.Canvas.Handle, R1.Left, R1.Top, Width, Height, vil.CachedImages[ImageIndex].Canvas.Handle, 0, 0, SRCCOPY)
            else
              CopyBmp32(R1, R2, DestBmp, vil.CachedImages[ImageIndex], EmptyCI, False, GrayedColor, Blend, Reflected);
          end;
        end
        else begin
          Bmp := CreateBmp32(ImgWidth, max(0, ImgHeight));
          vil.GetBitmap32(ImageIndex, Bmp, GetImageHeight(ImgList));
          if Bmp <> nil then
            if (vil.AlphaImageList <> nil) and vil.AlphaImageList.IgnoreTransparency then
              BitBlt(DestBmp.Canvas.Handle, R1.Left, R1.Top, Bmp.Width, Bmp.Height, Bmp.Canvas.Handle, 0, 0, SRCCOPY)
            else
              CopyBmp32(R1, R2, DestBmp, Bmp, EmptyCI, False, GrayedColor, Blend, Reflected);
        end;
        vil.ForeColor := clNone;
      end
      else begin
        Bmp := CreateBmp32(ImgWidth, ImgHeight);
        BitBlt(Bmp.Canvas.Handle, R2.Left, R2.Top, WidthOf(R2), HeightOf(R2), DestBmp.Canvas.Handle, R1.Left, R1.Top, SRCCOPY);
        Bmp.Canvas.Font.Color := DestBmp.Canvas.Font.Color;
        ImgList.Draw(Bmp.Canvas, 0, 0, ImageIndex, True);
        BitBlt(DestBmp.Canvas.Handle, R1.Left, R1.Top, WidthOf(R2), HeightOf(R2), Bmp.Canvas.Handle, R2.Left, R2.Top, SRCCOPY);
      end;

    FreeAndNil(Bmp);
  end;
end;


procedure DrawAlphaImgListDC(const ImgList: TCustomImageList; const DC: hdc; const Left: integer; const Top: integer;
  const ImageIndex: integer; const Blend: integer; const GrayedColor: TColor; const State: integer; const NumGlyphs: integer; const Reflected: boolean);
var
  Bmp: TBitmap;
  vil: TsVirtualImageList;
  imgWidth, imgHeight: integer;
begin
  imgWidth  := GetImageWidth (ImgList, ImageIndex);
  imgHeight := GetImageHeight(ImgList, ImageIndex);
  Bmp := CreateBmp32(ImgWidth, ImgHeight);
  if ImgList is TsVirtualImageList then begin
    vil := TsVirtualImageList(ImgList);
    Bmp.Canvas.Font.Color := vil.ForeColor;
    if Reflected and (vil.AlphaImageList is TsCharImageList) then
      Bmp.Height := Bmp.Height + Round(GetImageHeight(ImgList) * TsCharImageList(vil.AlphaImageList).Items[ImageIndex].ScalingFactor) div 2
    else
      Bmp.Height := Bmp.Height + GetImageHeight(ImgList) div 2;

    if (vil.AlphaImageList is TsCharImageList) and (TsCharImageList(vil.AlphaImageList).Items[ImageIndex].Color <> clNone) then
      Bmp.Canvas.Font.Size := 0; // Def color is not allowed
  end
  else
    if ImgList is TsCharImageList then begin
      Bmp.Canvas.Font.Color := TsCharImageList(ImgList).ForeColor;
      if Reflected then
        Bmp.Height := Bmp.Height + Round(GetImageHeight(ImgList) * TsCharImageList(ImgList).Items[ImageIndex].ScalingFactor) div 2;

      if TsCharImageList(ImgList).Items[ImageIndex].Color <> clNone then
        Bmp.Canvas.Font.Size := 0; // Def color is not allowed
    end
    else
      if Reflected then
        Bmp.Height := Bmp.Height + GetImageHeight(ImgList) div 2;

  BitBlt(Bmp.Canvas.Handle, 0, 0, Bmp.Width, Bmp.Height, DC, Left, Top, SRCCOPY);
  DrawAlphaImgList(ImgList, Bmp, 0, 0, ImageIndex, Blend, GrayedColor, State, NumGlyphs, Reflected);
  BitBlt(DC, Left, Top, Bmp.Width, Bmp.Height, Bmp.Canvas.Handle, 0, 0, SRCCOPY);
  FreeAndNil(Bmp);
end;


function GetDefScaling(CharSet: TFontCharset; FontName: string): real;
const
  CharSize = 96;
var
  f: TFont;
  i: integer;
begin
  f := TFont.Create;
  f.Charset := Charset;
  f.Name := FontName;
  f.Height := -CharSize;
  i := GetFontHeight(f.Handle, True);
  Result := i / CharSize;
  f.Free;
end;


function AddImageFromRes(aInstance: LongWord; ImageList: TsAlphaimageList; const ResName: String; aImageFormat: TsImageFormat): Boolean;
var
  hIc: HICON;
  Ico: hIcon;
  Bmp: TBitmap;
  rs: TResourceStream;
begin
  Result := False;
  case aImageFormat of
    ifICO: begin
      hIc := LoadImage(aInstance, PChar(ResName), IMAGE_ICON, ImageList.Width, ImageList.Height, 0);
      if hIc <> 0 then
        try
          if ImageList_AddIcon(ImageList.Handle, hIc) <> -1 then
            Result := True;
        finally
          DestroyIcon(hIc);
        end;
    end;

    ifBMP32: begin
      Bmp := TBitmap.Create;
      Bmp.LoadFromResourceName(aInstance, ResName);
      with TsImgListItem.Create(ImageList.Items) do begin
        ImageFormat := aImageFormat;
        Bmp.SaveToStream(ImgData);
      end;
      Ico := MakeIcon32(Bmp);
      Result := ImageList_AddIcon(ImageList.Handle, Ico) <> -1;
      DestroyIcon(Ico);
      FreeAndNil(Bmp);
    end

    else begin
      rs := TResourceStream.Create(aInstance, ResName, RT_RCDATA);
      rs.Seek(0, 0);
      with TsImgListItem.Create(ImageList.Items) do begin // Add to Items
        ImageFormat := ifPNG;
        ImgData.LoadFromStream(rs);
      end;
      rs.Seek(0, 0);
      Bmp := TBitmap.Create;
      LoadBmpFromPngStream(Bmp, rs);
      FreeAndNil(rs);
      Ico := MakeIcon32(Bmp);
      Result := ImageList_AddIcon(ImageList.Handle, Ico) <> -1;
      DestroyIcon(Ico);
      FreeAndNil(Bmp);
    end;
  end;
end;


procedure TsAlphaImageList.AfterConstruction;
begin
  inherited;
  if not (csLoading in ComponentState) then
    FLoaded := True;
end;


function GetColor(Value: DWORD): TColor;
begin
  case Value of
    CLR_NONE:    Result := clNone;
    CLR_DEFAULT: Result := clDefault
    else         Result := TColor(Value);
  end;
end;


procedure TsAlphaImageList.Assign(Source: TPersistent);
var
  ImageList: TsAlphaImageList;
begin
  if Source = nil then
    KillImgList
  else
    if Source is TsAlphaImageList then begin
      AcBeginUpdate;
      inherited Clear;
      ImageList := TsAlphaImageList(Source);
      Masked := ImageList.Masked;
      ImageType := ImageList.ImageType;
      DrawingStyle := ImageList.DrawingStyle;
      ShareImages := ImageList.ShareImages;
      SetNewDimensions(ImageList.Handle);
      KillImgList;
      if not HandleAllocated then
        CreateImgList
      else
        ImageList_SetIconSize(Handle, Width, Height);

      BkColor := GetColor(ImageList_GetBkColor(ImageList.Handle));
      BlendColor := ImageList.BlendColor;
      CopyImages(ImageList);
      AcEndUpdate(False);
    end
    else
      inherited Assign(Source);
end;


procedure TsAlphaImageList.AssignTo(Dest: TPersistent);
var
  ImageList: TsAlphaImageList;
begin
  if Dest is TsAlphaImageList then begin
    ImageList := TsAlphaImageList(Dest);
    ImageList.AcBeginUpdate;
    ImageList.Masked := Masked;
    ImageList.ImageType := ImageType;
    ImageList.DrawingStyle := DrawingStyle;
    ImageList.ShareImages := ShareImages;
    ImageList.BlendColor := BlendColor;

    ImageList.Clear;
    ImageList.KillImgList;
    ImageList.SetNewDimensions(Self.Handle);
    if not ImageList.HandleAllocated then
      ImageList.CreateImgList
    else
      ImageList_SetIconSize(ImageList.Handle, ImageList.Width, ImageList.Height);

    ImageList.BkColor := GetColor(ImageList_GetBkColor(Self.Handle));
    ImageList.CopyImages(Self);
    ImageList.AcEndUpdate(False);
  end
  else
    inherited AssignTo(Dest);
end;


procedure TsAlphaImageList.CopyImages(const ImgList: TsAlphaImageList);
var
  i: integer;
  Ico: hIcon;
begin
  if HandleAllocated then begin
    ImageList_SetBkColor(ImgList.Handle, CLR_NONE);
    if IsDuplicated then begin
      Items.Clear;
      for i := 0 to ImgList.Items.Count - 1 do
        with TsImgListItem(Items.Add) do begin
          ImageFormat := ImgList.Items[i].ImageFormat;
          ImageName   := ImgList.Items[i].ImageName;
          PixelFormat := ImgList.Items[i].PixelFormat;
          Text        := ImgList.Items[i].Text;
          ImgData.LoadFromStream(ImgList.Items[i].ImgData);
        end;

      GenerateStdList;
    end
    else begin
      inherited Clear;
      ImageList_SetBkColor(Handle, CLR_NONE);
      for i := 0 to ImgList.Count - 1 do begin
        Ico := ImageList_GetIcon(ImgList.Handle, i, ILD_TRANSPARENT);
        ImageList_AddIcon(Handle, Ico);
        DestroyIcon(Ico);
      end;
    end;
  end;
end;


constructor TsAlphaImageList.Create(AOwner: TComponent);
begin
  FLoaded := False;
  FSavedScale := 0;
  inherited;
  FItems := TsImgListItems.Create(Self);
  FAllowScale := True;
  StdMode := False;
  FBkColor := clNone;
  FUseCache := True;
  DoubleData := True;
end;


procedure TsAlphaImageList.CreateImgList;
begin
{$IFNDEF FPC}
  if (SavedWidth = 0) and FLoaded and not (csLoading in ComponentState) then begin
    SavedWidth := Width;
    SavedHeight := Height;
  end;

  if CanScale then
    Handle := ImageList_Create(DefaultManager.ScaleInt(SavedWidth), DefaultManager.ScaleInt(SavedHeight), ILC_COLOR32 or (Integer(Masked) * ILC_MASK), 0, AllocBy)
  else
    Handle := ImageList_Create(SavedWidth, SavedHeight, ILC_COLOR32 or (Integer(Masked) * ILC_MASK), 0, AllocBy);
{$ELSE}
  ReferenceNeeded;
{$ENDIF}
end;


destructor TsAlphaImageList.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;


procedure TsAlphaImageList.DoDraw(Index: Integer; Canvas: TCanvas; X, Y: Integer; Style: Cardinal; Enabled: Boolean);
var
  Ico: hIcon;
  TmpBmp, Bmp: TBitmap;
begin
  if HandleAllocated and (Index >= 0) then
    if IsValidIndex(Index, Items.Count) then
      case Items[Index].ImageFormat of
        ifPng, ifBmp32: begin
          Bmp := CreateBitmap32(Index, Width, Height);
          if Bmp <> nil then
            try
              Bmp.Canvas.Lock;
              TmpBmp := CreateBmp32(Width, Height);
              TmpBmp.Canvas.Lock;
              try
                BitBlt(TmpBmp.Canvas.Handle, 0, 0, Width, Height, Canvas.Handle, X, Y, SRCCOPY);
                CopyBmp32(MkRect(Width, Height), MkRect(Width, Height), TmpBmp, Bmp, MakeCacheInfo(TmpBmp), False, clNone, 0, False);
                BitBlt(Canvas.Handle, X, Y, Width, Height, TmpBmp.Canvas.Handle, 0, 0, SRCCOPY);
              finally
                TmpBmp.Canvas.Unlock;
                FreeAndNil(TmpBmp);
              end;
              Bmp.Canvas.Unlock;
            finally
              FreeAndNil(Bmp);
            end;
        end;

        ifIco: begin
          ImageList_SetBkColor(Handle, CLR_NONE);
          Ico := ImageList_GetIcon(Handle, Index, ILD_TRANSPARENT);
          if Ico > 0 then begin
            DrawIconEx(Canvas.Handle, X, Y, Ico, Width, Height, 0, 0, DI_NORMAL);
            DestroyIcon(Ico);
          end;
        end;
      end
    else begin
      ImageList_SetBkColor(Handle, CLR_NONE);
      Ico := ImageList_GetIcon(Handle, Index, ILD_TRANSPARENT);
      if Ico > 0 then begin
        DrawIconEx(Canvas.Handle, X, Y, Ico, Width, Height, 0, 0, DI_NORMAL);
        DestroyIcon(Ico);
      end;
    end;
end;


procedure TsAlphaImageList.GenerateStdList;
var
  i: integer;
  Bmp: TBitmap;
  Icon: TIcon;
begin
  if not StdMode then begin
    // Check if default size was not initialized yet
    if (SavedHeight = 0) and (inherited Width = 16) and (inherited Height = 16) and not AcChanging then begin
      AcChanging := True;
      KillImgList;
      Width  := inherited Width;
      Height := inherited Height;
    end;

    if SavedHeight <> 0 then begin
      if not HandleAllocated then
        CreateImgList;

      if HandleAllocated then begin
        AcChanging := True;
        inherited Clear;
        for i := 0 to Items.Count - 1 do
          case Items[i].ImageFormat of
            ifPNG: begin
              Items[i].ImgData.Seek(0, 0);
              Bmp := TBitmap.Create;
              LoadBmpFromPngStream(Bmp, Items[i].ImgData);
              Items[i].OrigWidth := Bmp.Width;
              Items[i].OrigHeight := Bmp.Height;
              AddAsIcon(Bmp, -1);
              FreeAndNil(Bmp);
            end;

            ifBMP32: begin
              Bmp := TBitmap.Create;
              Items[i].ImgData.Seek(0, 0);
              Bmp.LoadFromStream(Items[i].ImgData);
              Items[i].OrigWidth := Bmp.Width;
              Items[i].OrigHeight := Bmp.Height;
              Bmp.PixelFormat := pf32bit;
              AddAsIcon(Bmp, -1);
              FreeAndNil(Bmp);
            end;

            ifICO: begin
              Icon := TIcon.Create;
              Items[i].ImgData.Seek(0, 0);
              Icon.LoadFromStream(Items[i].ImgData);
              Items[i].OrigWidth := Icon.Width;
              Items[i].OrigHeight := Icon.Height;
              ImageList_AddIcon(Handle, Icon.Handle);
              FreeAndNil(Icon);
            end;
          end;

        if Items.Count > 0 then begin
          StdListIsGenerated := True;
          if not IsDuplicated then
            Items.Clear;
        end;
        AcChanging := False;
      end;
    end;
  end;
end;


function TsAlphaImageList.GetBitmap32(Index: Integer; Image: TBitmap; GlyphHeight: integer = 0): Boolean;
var
  Ico: hIcon;
  iInfo: TIconInfo;
  TmpBmp, Bmp: TBitmap;
begin
  Result := False;
  if HandleAllocated and (Image <> nil) and IsValidIndex(Index, Count) then
    if IsDuplicated and (Index < Items.Count) and (Items[Index].ImgData.Size > 0) then // Using of original image if exists
      case Items[Index].ImageFormat of
        ifPNG: begin
          if FUseCache and (Items[Index].CacheBmp <> nil) then
            if (Items[Index].CacheBmp.Width = Image.Width) and (Items[Index].CacheBmp.Height = Image.Height) then begin
              Image.Assign(Items[Index].CacheBmp);
              Result := True;
              Exit;
            end
            else
              FreeAndNil(Items[Index].CacheBmp); // Reset cache

          Bmp := TBitmap.Create;
          Bmp.Transparent := Image.Transparent;
          Items[Index].ImgData.Seek(0, 0);
          LoadBmpFromPngStream(Bmp, Items[Index].ImgData);
          if (Items[Index].OrigWidth <> Image.Width) or (Items[Index].OrigHeight <> Image.Height) then begin // If must be scaled
            Image.PixelFormat := pf32bit;
            Stretch(Bmp, Image, Image.Width, Image.Height, ftMitchell);
          end
          else
            Image.Assign(Bmp);

          FreeAndNil(Bmp);
          if Image.PixelFormat <> pf32bit then begin
            Image.PixelFormat := pf32bit;
            CheckEmptyAChannel(Image);
          end;
          // Make Cache
          if FUseCache then
            if (Image.Width = Width) and (Image.Height = Height) then begin
              Items[Index].CacheBmp := CreateBmp32(Image.Width, Image.Height);
              Items[Index].CacheBmp.Assign(Image);
            end;

          Result := True;
        end;

        ifBMP32: begin
          if FUseCache and (Items[Index].CacheBmp <> nil) then
            if (Items[Index].CacheBmp.Width = Image.Width) and (Items[Index].CacheBmp.Height = Image.Height) then begin
              Image.Assign(Items[Index].CacheBmp);
              Result := True;
              Exit;
            end
            else
              FreeAndNil(Items[Index].CacheBmp); // Reset cache

          Items[Index].ImgData.Seek(0, 0);
          if (Items[Index].OrigWidth <> Image.Width) or (Items[Index].OrigHeight <> Image.Height) then begin // If must be scaled
            Bmp := TBitmap.Create;
            Bmp.Transparent := Image.Transparent;
            Bmp.LoadFromStream(Items[Index].ImgData);
            Image.PixelFormat := pf32bit;
            Stretch(Bmp, Image, Image.Width, Image.Height, ftMitchell);
            FreeAndNil(Bmp);
          end
          else
            Image.LoadFromStream(Items[Index].ImgData);
            
          if Image.PixelFormat <> pf32bit then begin
            Image.PixelFormat := pf32bit;
            CheckEmptyAChannel(Image);
          end;
          // Make Cache
          if FUseCache then 
            if (Image.Width = Width) and (Image.Height = Height) then begin
              Items[Index].CacheBmp := CreateBmp32(Image.Width, Image.Height);
              Items[Index].CacheBmp.Assign(Image);
            end;

          Result := True;
        end

        else begin
          Ico := ImageList_GetIcon(Handle, Index, ILD_NORMAL);
          if Ico <> 0 then begin
            TmpBmp := CreateBmp32(Image.Width, Image.Height);
            if GetIconInfo(Ico, iInfo) then begin
              TmpBmp.Handle := iInfo.hbmColor;
              TmpBmp.HandleType := bmDIB;
              TmpBmp.PixelFormat := pf32bit;

              if (Win32MajorVersion < 6) and (GetBpp < 32) then // Update alpha channel
                CheckEmptyAChannel(TmpBmp);

              Image.Assign(TmpBmp);

              DeleteObject(iInfo.hbmMask);
              Result := True;
            end;
            FreeAndNil(TmpBmp);
            DestroyIcon(Ico);
          end;
        end;
      end
    else begin
      Ico := ImageList_GetIcon(Handle, Index, ILD_NORMAL);
      if Ico <> 0 then begin
        TmpBmp := CreateBmp32(Image.Width, Image.Height);
        if GetIconInfo(Ico, iInfo) then begin
          TmpBmp.Handle := iInfo.hbmColor;
          TmpBmp.HandleType := bmDIB;
          TmpBmp.PixelFormat := pf32bit;

          if (Win32MajorVersion < 6) and (GetBpp < 32) then // Update alpha channel
            CheckEmptyAChannel(TmpBmp);

          Image.Assign(TmpBmp);
          DeleteObject(iInfo.hbmMask);
          Result := True;
        end;
        FreeAndNil(TmpBmp);
        DestroyIcon(Ico);
      end;
    end;
end;


function TsAlphaImageList.GetBkColor: TColor;
begin
  if FBkColor = clNone then
    Result := inherited BkColor
  else
    Result := FBkColor;
end;


{$IFNDEF DELPHI_XE8}
function TsAlphaImageList.Count: Integer;
{$ELSE}
function TsAlphaImageList.GetCount: Integer;
{$ENDIF}
begin
  Result := Items.Count;
  if (Result < ImageList_GetImageCount(Handle)) and ((Result = 0) or (Items[0].ImageFormat = ifICO)) then
    UpdateFromStd;
end;


function TsAlphaImageList.GetDimension(const Index: Integer): integer;
var
  b: boolean;
begin
  b := AcChanging;
  AcChanging := True;
  ScaleSize;
  AcChanging := b;
  if Index = 0 then
    Result := inherited Height
  else
    Result := inherited Width;
end;


function TsAlphaImageList.CreateBitmap32(Index: Integer; aWidth, aHeight: integer; GlyphHeight: integer = 0): TBitmap;
var
  iInfo: TIconInfo;
  mBmp, Bmp: TBitmap;
  Ico: hIcon;
  X, Y, DeltaS, DeltaD: integer;
  S0, S: PRGBAArray_;
  D0, D: PByteArray;
begin
  Result := nil;
  if HandleAllocated and IsValidIndex(Index, Count) then
    if IsDuplicated and (Index < Items.Count) and (Items[Index].ImgData.Size > 0) then begin // Using of original image if exists
      case Items[Index].ImageFormat of
        ifPNG: begin
          if FUseCache and (Items[Index].CacheBmp <> nil) then
            if (Items[Index].CacheBmp.Width = aWidth) and (Items[Index].CacheBmp.Height = aHeight) then begin
              Result := CreateBmp32(aWidth, aHeight);
              Result.Assign(Items[Index].CacheBmp);
              Exit;
            end
            else
              FreeAndNil(Items[Index].CacheBmp); // Reset cache

          if (Items[Index].OrigWidth <> aWidth) or (Items[Index].OrigHeight <> aHeight) then begin // If must be scaled
            Bmp := TBitmap.Create;
            Result := CreateBmp32(aWidth, aHeight);
            Items[Index].ImgData.Seek(0, 0);
            LoadBmpFromPngStream(Bmp, Items[Index].ImgData);
            Stretch(Bmp, Result, aWidth, aHeight, ftMitchell);
            FreeAndNil(Bmp);
          end
          else begin
            Result := TBitmap.Create;
            Items[Index].ImgData.Seek(0, 0);
            LoadBmpFromPngStream(Result, Items[Index].ImgData);
            if Result.PixelFormat <> pf32bit then begin
              Result.PixelFormat := pf32bit;
              CheckEmptyAChannel(Result);
            end;
          end;
          // Make Cache
          if FUseCache then begin
            Items[Index].CacheBmp := CreateBmp32(aWidth, aHeight);
            Items[Index].CacheBmp.Assign(Result);
          end;
        end;

        ifBMP32: begin
          if FUseCache and (Items[Index].CacheBmp <> nil) then
            if (Items[Index].CacheBmp.Width = aWidth) and (Items[Index].CacheBmp.Height = aHeight) then begin
              Result := CreateBmp32(aWidth, aHeight);
              Result.Assign(Items[Index].CacheBmp);
              Exit;
            end
            else
              FreeAndNil(Items[Index].CacheBmp); // Reset cache

          if (Items[Index].OrigWidth <> aWidth) or (Items[Index].OrigHeight <> aHeight) then begin // If must be scaled
            Result := CreateBmp32(aWidth, aHeight);
            Bmp := TBitmap.Create;
            Items[Index].ImgData.Seek(0, 0);
            Bmp.LoadFromStream(Items[Index].ImgData);
            if Bmp.PixelFormat <> pf32bit then begin
              Bmp.PixelFormat := pf32bit;
              CheckEmptyAChannel(Bmp);
            end;
            Stretch(Bmp, Result, aWidth, aHeight, ftMitchell);
            FreeAndNil(Bmp);
          end
          else begin
            Result := TBitmap.Create;
            Items[Index].ImgData.Seek(0, 0);
            Result.LoadFromStream(Items[Index].ImgData);
            if Result.PixelFormat <> pf32bit then begin
              Result.PixelFormat := pf32bit;
              CheckEmptyAChannel(Result);
            end;
          end;
          // Make Cache
          if FUseCache then begin
            Items[Index].CacheBmp := CreateBmp32(aWidth, aHeight);
            Items[Index].CacheBmp.Assign(Result);
          end;
        end

        else begin
          Ico := ImageList_GetIcon(Handle, Index, ILD_NORMAL);
          if Ico <> 0 then begin
            Result := CreateBmp32(aWidth, aHeight);
            if GetIconInfo(Ico, iInfo) then begin
              Result.Handle := iInfo.hbmColor;
              Result.HandleType := bmDIB;
              Result.PixelFormat := pf32bit;
              if Win32MajorVersion < 6 then // Update alpha channel
                if (GetBpp < 32) or ((DefaultManager = nil) or DefaultManager.Options.CheckEmptyAlpha) then
                  CheckEmptyAChannel(Result);

              DeleteObject(iInfo.hbmMask);
            end;
            DestroyIcon(Ico);
          end;
        end;
      end;
    end
    else begin
      Ico := ImageList_GetIcon(Handle, Index, ILD_NORMAL);
      if Ico <> 0 then begin
        Result := CreateBmp32(aWidth, aHeight);
        if GetIconInfo(Ico, iInfo) then begin
          Result.Handle := iInfo.hbmColor;
          Result.HandleType := bmDIB;
          Result.PixelFormat := pf32bit;
          if iInfo.hbmMask <> 0 then begin
            mBmp := TBitmap.Create;
            mBmp.Handle := iInfo.hbmMask;
            mBmp.PixelFormat := pf8bit;
            if (mBmp.Width = Result.Width) and (mBmp.Height = Result.Height) then begin
              if InitLine(Result, Pointer(S0), DeltaS) and InitLine(mBmp, Pointer(D0), DeltaD) then
                for Y := 0 to Result.Height - 1 do begin
                  S := Pointer(PAnsiChar(S0) + DeltaS * Y);
                  D := Pointer(PAnsiChar(D0) + DeltaD * Y);
                  for X := 0 to Result.Width - 1 do
                    S[X].A := MaxByte - D[X];
                end;
            end;
            mBmp.Free;
          end
          else
            if Win32MajorVersion < 6 then // Update alpha channel
              if (GetBpp < 32) or ((DefaultManager = nil) or DefaultManager.Options.CheckEmptyAlpha) then
                CheckEmptyAChannel(Result);

          DeleteObject(iInfo.hbmMask);
        end;
        DestroyIcon(Ico);
      end;
    end;
end;


function TsAlphaImageList.IsDuplicated: boolean;
begin
  Result := DoubleData or (csDesigning in ComponentState);
end;


procedure TsAlphaImageList.KillImgList;
begin
  if HandleAllocated and not ShareImages then
    ImageList_Destroy(Handle);

{$IFNDEF FPC}
  Handle := 0;
{$ENDIF}
  if not AcChanging then
    Change;
end;


procedure TsAlphaImageList.Loaded;
var
  w, h: integer;
begin
  inherited;
  FLoaded := True;
  if SavedWidth = 0 then begin
    w := inherited Width;
    h := inherited Height;
    if (w = 16) and (h = 16) then begin // If default size - reinit
      AcChanging := True;
      KillImgList;
      SetDimension(1, w);
      SetDimension(0, h);
    end;
  end
  else
    KillImgList;

  if SavedWidth = 0 then
    SetDimension(1, Width);

  if SavedHeight = 0 then
    SetDimension(0, Height);

  if not StdListIsGenerated then begin
    GenerateStdList;
    StdListIsGenerated := True; // Set the flag even if iconlist is empty
    Change;
  end;
end;


procedure TsAlphaImageList.LoadFromFile(const FileName: acString);
begin
  if not TryLoadFromfile(FileName) then
    MessageDlg('Cannot load ' + FileName + s_0D0A + 'Invalid or unexpected image format.', mtError, [mbOk], 0);
end;


procedure TsAlphaImageList.SetNewScale(Value: Integer);
var
  b: boolean;
begin
  if (FSavedScale <> Value) and CanScale then begin
    b := AcChanging;
    AcChanging := True;
    FSavedScale := Value;
    inherited Clear;
    KillImgList;
    GenerateStdList;
    AcChanging := b;
    if not AcChanging then
      inherited Change;
  end;
end;


procedure TsAlphaImageList.ScaleSize;
begin
  if CanScale and not (csLoading in ComponentState) and (SavedWidth <> 0) then
    ScaleValue := DefaultManager.GetScale;
end;


procedure TsAlphaImageList.SetBkColor(const Value: TColor);
begin
  FBkColor := Value;
  inherited BkColor := Value;
  if HandleAllocated then
    ImageList_SetBkColor(Handle, Value);
end;


procedure TsAlphaImageList.SetDimension(const Index, Value: integer);

  procedure SetScaleValue;
  begin
    if DefaultManager <> nil then
      ScaleValue := DefaultManager.GetScale
    else
      ScaleValue := 0;
  end;

begin
  case Index of
    0: begin
      if FLoaded then begin
        SavedHeight := Value;
        SetScaleValue;
      end;
      inherited Height := Value;
    end;

    1: begin
      if FLoaded then begin
        SavedWidth := Value;
        SetScaleValue;
      end;
      inherited Width := Value;
    end;
  end;
  if not (csLoading in ComponentState) and FLoaded and HandleAllocated and (Count <= Items.Count) and not AcChanging {If can generate new} then begin
    GenerateStdList;
    Change;
  end;
end;


procedure TsAlphaImageList.SetItems(const Value: TsImgListItems);
begin
  FItems.Assign(Value);
end;


procedure TsAlphaImageList.SetNewDimensions(Value: HImageList);
var
  AHeight, AWidth: Integer;
begin
  AWidth := Width;
  AHeight := Height;
  ImageList_GetIconSize(Value, AWidth, AHeight);
  Width := AWidth;
  Height := AHeight;
end;


function TsAlphaImageList.TryLoadFromfile(const FileName: acString): boolean;
var
  Ico: HICON;
  Bmp: TBitmap;
  iInfo: TIconInfo;
  S0, S: PRGBAArray_;
  iFormat: TsImageFormat;
  w, h, X, Y, DeltaS: integer;
begin
  Result := False;
  Ico := 0;
  if not HandleAllocated then
    GenerateStdList;

  if HandleAllocated and GetImageFormat(FileName, iFormat) then begin
    if IsDuplicated then // If double data used
      with TsImgListItem(Items.Add) do begin
        ImgData.LoadFromFile(FileName);
        ImageFormat := iFormat;
        ImageName := ExtractFileName(FileName);
        ImageName := Copy(ImageName, 1, Length(ImageName) - 4);
        case iFormat of
          ifPNG: begin
            Bmp := TBitmap.Create;
            PixelFormat := pf32bit;
            LoadBmpFromPngStream(Bmp, ImgData);
            OrigWidth := Bmp.Width;
            OrigHeight := Bmp.Height;
            Ico := MakeIcon32(Bmp);
            FreeAndNil(Bmp);
          end;

          ifBMP32: begin
            PixelFormat := pf32bit;
            Bmp := TBitmap.Create;
            Bmp.LoadFromStream(ImgData);
            OrigWidth := Bmp.Width;
            OrigHeight := Bmp.Height;
            if Bmp.PixelFormat <> pf32bit then begin // Convert to 32 with alpha channel
              Bmp.PixelFormat := pf32bit;
              ChangeBitmapPixels(Bmp, MakeAlphaPixel, 0, Bmp.Canvas.Pixels[0, Bmp.Height - 1]);
            end;
            Ico := MakeIcon32(Bmp);
            FreeAndNil(Bmp);
          end;

          ifICO: begin
            Ico := {$IFDEF TNTUNICODE}ExtractIconW{$ELSE}ExtractIcon{$ENDIF}(hInstance, PacChar(FileName), 0);
            GetIconInfo(Ico, iInfo);
            Bmp := TBitmap.Create;
            Bmp.Handle := iInfo.hbmColor;
            Bmp.HandleType := bmDIB;
            OrigWidth := Bmp.Width;
            OrigHeight := Bmp.Height;
            PixelFormat := pf24bit;
            w := Bmp.Width - 1;
            h := Bmp.Height - 1;
            if Bmp.PixelFormat = pf32bit then // Check the alpha channel
              if InitLine(Bmp, Pointer(S0), DeltaS) then
                for Y := 0 to h do begin
                  S := Pointer(PAnsiChar(S0) + DeltaS * Y);
                  for X := 0 to w do
                    if S[X].A <> 0 then begin
                      PixelFormat := pf32bit;
                      Break;
                    end;

                  if PixelFormat = pf32bit then
                    Break;
                end;

            FreeAndNil(Bmp);
            DeleteObject(iInfo.hbmColor);
            DeleteObject(iInfo.hbmMask);
          end;
        end;
      end
    else
      case iFormat of
        ifPNG: begin
          Bmp := TBitmap.Create;
          LoadBmpFromPngFile(Bmp, FileName);
          Ico := MakeIcon32(Bmp);
          FreeAndNil(Bmp);
        end;

        ifBMP32: begin
          Bmp := TBitmap.Create;
          Bmp.LoadFromFile(FileName);
          Ico := MakeIcon32(Bmp);
          FreeAndNil(Bmp);
        end;

        ifICO:
          Ico := {$IFDEF TNTUNICODE}ExtractIconW{$ELSE}ExtractIcon{$ENDIF}(hInstance, PacChar(FileName), 0);
      end;

    if Ico <> 0 then begin
      Result := ImageList_AddIcon(Handle, Ico) >= 0;
      DestroyIcon(Ico);
    end;
    Change;
  end;
end;


procedure TsAlphaImageList.ItemsClear;
var
  i: integer;
begin
  for i := 0 to Items.Count - 1 do
    Items[i].ImgData.Clear;

  Items.Clear;
end;


function TsAlphaImageList.CanScale: boolean;
begin
  Result := FAllowScale and FLoaded and (DefaultManager <> nil) and not (csDesigning in ComponentState) and (Items <> nil) and (Items.Count > 0);
end;


procedure TsAlphaImageList.Change;
var
  b: boolean;
  Bmp: TBitmap;
  iInfo: TIconInfo;
  Ico, NewIco: HICON;
  S0, S: PRGBAArray_S;
  X, Y, DeltaS, i, c, h, w: integer;
begin
  if not AcChanging then
    if HandleAllocated and not (csLoading in ComponentState) and StdListIsGenerated then begin
      if not (IsDuplicated and (Count <= Items.Count)) or (csDesigning in ComponentState) then
        inherited;

      if not (csDesigning in ComponentState) then
        if IsDuplicated and (Count <= Items.Count) {If icon was not added using AddIcon or other std. way (not stored in Items)} then begin
          if Count < Items.Count then begin
            AcChanging := True;
            GenerateStdList;
            AcChanging := False;
          end;
          inherited;
        end
        else begin
          c := ImageList_GetImageCount(Handle) - 1;
          if c >= 0 then begin
            Bmp := TBitmap.Create;
            for i := 0 to c do begin
              Ico := ImageList_GetIcon(Handle, i, ILD_NORMAL);
              GetIconInfo(Ico, iInfo);
              DestroyIcon(Ico);
              Bmp.Handle := iInfo.hbmColor;
              Bmp.HandleType := bmDIB;
              b := False;
              h := Bmp.Height - 1;
              w := Bmp.Width - 1;
              Bmp.PixelFormat := pf32bit;
              if InitLine(Bmp, Pointer(S0), DeltaS) then begin
                for Y := 0 to h do begin // Check if AlphaChannel is empty
                  S := Pointer(PAnsiChar(S0) + DeltaS * Y);
                  for X := 0 to w do
                    if S[X].SA <> 0 then begin
                      b := True;
                      Break;
                    end;

                  if b then
                    Break;
                end;
                if not b then begin
                  for Y := 0 to h do begin
                    S := Pointer(PAnsiChar(S0) + DeltaS * Y);
                    for X := 0 to w do
                      with S[X] do
                        if SC <> sFuchsia.C then
                          SA := MaxByte;
                  end;
                  iInfo.hbmColor := Bmp.Handle;
                  NewIco := CreateIconIndirect(iInfo);
                  ImageList_ReplaceIcon(Handle, i, NewIco);
                  DestroyIcon(NewIco);
                end;
              end;
              DeleteObject(iInfo.hbmColor);
              DeleteObject(iInfo.hbmMask);
            end;
            FreeAndNil(Bmp);
          end;
        end;
    end;
end;


procedure TsAlphaImageList.Clear;
begin
  ItemsClear;
  inherited Clear;
end;


procedure TsAlphaImageList.AcBeginUpdate;
begin
  AcChanging := True;
end;


procedure TsAlphaImageList.AcEndUpdate(DoChange: boolean = True);
begin
  AcChanging := False;
  if DoChange then begin
    GenerateStdList;
    Change;
  end;
end;


procedure TsAlphaImageList.SetUseCache(const Value: boolean);
var
  i: integer;
begin
  FUseCache := Value;
  for i := 0 to Items.Count - 1 do
    if Items[i].CacheBmp <> nil then
      FreeAndNil(Items[i].CacheBmp);
end;


procedure TsAlphaImageList.MoveItem(CurIndex, NewIndex: integer);
begin
  Items[CurIndex].Index := NewIndex;
  Move(CurIndex, NewIndex);
end;


procedure TsAlphaImageList.Replace(AIndex: integer; ABmp32: TBitmap);
var
  Ico: HICON;
begin
  if HandleAllocated and (Items.Count > AIndex) then
    try
      if ABmp32.PixelFormat <> pf32bit then begin
        ABmp32.PixelFormat := pf32bit;
        FillAlphaRect(ABmp32, MkRect(ABmp32), MaxByte);
      end;

      if IsDuplicated then // If double data used
        with Items[AIndex] do begin
          FreeAndNil(CacheBmp);
          ImgData.Clear;
          ABmp32.SaveToStream(ImgData);
          ImageFormat := ifBMP32;
        end;

      Ico := MakeIcon32(ABmp32);
      if Ico <> 0 then begin
        ImageList_ReplaceIcon(Handle, AIndex, Ico);
        DestroyIcon(Ico);
      end;
      GenerateStdList;
      Change;
    except
    end;
end;


function TsAlphaImageList.Add(Image, Mask: TBitmap): Integer;
var
  Ico: hIcon;
  C: TsColor;
begin
  if IsDuplicated then // If double data used
    with TsImgListItem(Items.Add) do begin
      if Image.PixelFormat <> pf32bit then begin
        Image.PixelFormat := pf32bit;
        if Image.Transparent then begin
          C.C := Image.TransparentColor;
          C.A := 0;
          FillAlphaRect(Image, MkRect(Image), MaxByte, C.C);
        end
        else
          FillAlphaRect(Image, MkRect(Image), MaxByte, clNone);
      end;
      Image.SaveToStream(ImgData);
      ImageFormat := ifBMP32;
      PixelFormat := pf32bit;
      Ico := MakeIcon32(Image);
    end
  else
    Ico := MakeIcon32(Image);

  if Ico <> 0 then begin
    Result := ImageList_AddIcon(Handle, Ico);
    DestroyIcon(Ico);
  end
  else
    Result := -1;

  Change;
end;


function TsAlphaImageList.AddImage(Value: TCustomImageList; Index: Integer): Integer;
begin
  if Value is TsAlphaImageList then begin
    if IsValidIndex(Index, TsAlphaImageList(Value).Items.Count) then begin
      acBeginUpdate;
      Items.Add.Assign(TsAlphaImageList(Value).Items[Index]);
      acEndUpdate;
      Result := Items.Count;
    end
    else
      Result := -1;
  end
{$IFDEF DELPHI7UP}
  else
    Result := inherited AddImage(Value, Index);
{$ENDIF}    
end;


constructor TsImgListItems.Create(AOwner: TsAlphaImageList);
begin
  inherited Create(TsImgListItem);
  FOwner := AOwner;
end;


destructor TsImgListItems.Destroy;
begin
  inherited Destroy;
  FOwner := nil;
end;


function TsImgListItems.GetItem(Index: Integer): TsImgListItem;
begin
  Result := TsImgListItem(inherited GetItem(Index));
end;


function TsImgListItems.GetOwner: TPersistent;
begin
  Result := FOwner;
end;


procedure TsImgListItems.SetItem(Index: Integer; Value: TsImgListItem);
begin
  inherited SetItem(Index, Value);
end;


procedure TsImgListItem.Assign(Source: TPersistent);
begin
  if Source <> nil then begin
    ImageFormat := TsImgListItem(Source).ImageFormat;
    ImageName   := TsImgListItem(Source).ImageName;
    PixelFormat := TsImgListItem(Source).PixelFormat;
    OrigWidth   := TsImgListItem(Source).OrigWidth;
    OrigHeight  := TsImgListItem(Source).OrigHeight;
    Text        := TsImgListItem(Source).Text;
    ImgData.LoadFromStream(TsImgListItem(Source).ImgData);
  end
  else
    inherited;
end;


procedure TsImgListItem.AssignTo(Dest: TPersistent);
begin
  if Dest <> nil then begin
    TsImgListItem(Dest).ImageFormat := ImageFormat;
    TsImgListItem(Dest).ImageName   := ImageName;
    TsImgListItem(Dest).PixelFormat := PixelFormat;
    TsImgListItem(Dest).OrigWidth   := OrigWidth;
    TsImgListItem(Dest).OrigHeight  := OrigHeight;
    TsImgListItem(Dest).Text        := Text;
    TsImgListItem(Dest).ImgData.LoadFromStream(ImgData);
  end
  else
    inherited;
end;


constructor TsImgListItem.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  CacheBmp := nil;
  ImgData := TMemoryStream.Create;
  FImageName := '';
  FPixelFormat := pf32bit;
end;


procedure TsImgListItem.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineBinaryProperty('ImgData', ReadData, WriteData, True);
end;


destructor TsImgListItem.Destroy;
begin
  FreeAndNil(ImgData);
  if CacheBmp <> nil then
    FreeAndNil(CacheBmp);

  inherited Destroy;
end;


procedure TsImgListItem.ReadData(Reader: TStream);
begin
  ImgData.LoadFromStream(Reader);
end;


procedure TsImgListItem.WriteData(Writer: TStream);
begin
  ImgData.SaveToStream(Writer);
end;


procedure TsVirtualImageList.AcBeginUpdate;
begin
  AcChanging := True;
end;


procedure TsVirtualImageList.AcEndUpdate(DoChange: boolean);
begin
  AcChanging := False;
  HandleNeeded;
  UpdateList;
  if DoChange then
    Change;
end;


procedure TsVirtualImageList.AddAsIcon(Bmp: TBitmap; Ndx: integer);
var
  Ico: hIcon;
  TmpBmp: TBitmap;
begin
  if Bmp <> nil then begin
    if (Bmp.Width <> Width) or (Bmp.Height <> Height) then begin
      TmpBmp := CreateBmp32(Width, Height);
      BitBlt(TmpBmp.Canvas.Handle, 0, 0, TmpBmp.Width, TmpBmp.Height, Bmp.Canvas.Handle, (Bmp.Width - TmpBmp.Width) div 2, (Bmp.Height - TmpBmp.Height) div 2, SRCCOPY);
    end
    else
      TmpBmp := Bmp;

    if IsNTFamily and {$IFDEF DELPHI7UP}acThemesEnabled{$ELSE}False{$ENDIF} then
      Ico := MakeCompIcon(TmpBmp, ColorToRGB(TColor(ImageList_GetBkColor(Handle))))
    else
      Ico := MakeIcon32(TmpBmp);

    if Ndx < 0 then
      ImageList_AddIcon(Handle, Ico)
    else
      ImageList_ReplaceIcon(Handle, Ndx, Ico);

    DestroyIcon(Ico);
    if TmpBmp <> Bmp then
      TmpBmp.Free;
  end;
end;


procedure TsVirtualImageList.Change;
begin
  if not AcChanging then
    inherited;
end;


procedure TsVirtualImageList.ClearItems;
var
  i: integer;
begin
  for i := 0 to Length(CachedImages) - 1 do
    if CachedImages[i] <> nil then
      FreeAndNil(CachedImages[i]);

  SetLength(CachedImages, 0);
end;


constructor TsVirtualImageList.Create(AOwner: TComponent);
begin
  inherited;
  FHeight := 16;
  FWidth := 16;
  CurrentScale := 0;
  FUseCache := True;
  ForeColor := clNone;
  DrawingStyle := dsTransparent;
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImageListChange;
  if acListsArray = nil then
    acListsArray := TList.Create;

  acListsArray.Add(Self);
end;


procedure TsVirtualImageList.CreateImgList;
begin
{$IFNDEF FPC}
  Handle := ImageList_Create(Width, Height, ILC_COLOR32 or (Integer(Masked) * ILC_MASK), 0, AllocBy);
{$ELSE}
  ReferenceNeeded;
{$ENDIF}
end;


destructor TsVirtualImageList.Destroy;
begin
  if acListsArray <> nil then
    acListsArray.Extract(Self);

  FreeAndNil(FImageChangeLink);
  ClearItems;
  inherited;
end;


procedure TsVirtualImageList.DoDraw(Index: Integer; Canvas: TCanvas; X, Y: Integer; Style: Cardinal; Enabled: Boolean);
var
  TmpBmp, IcoBmp: TBitmap;
  imgWidth, imgHeight, deltaX, deltaY: integer;
begin
  if HandleAllocated and IsValidIndex(Index, Count) then begin
    if Canvas.Font.Size = 0 then // If default char color can't be used
      ForeColor := Canvas.Font.Color
    else
      if (FAlphaImageList is TsCharImageList) and (TsCharImageList(FAlphaImageList).Items[Index].Color <> clNone) then
        ForeColor := clNone
      else
        ForeColor := Canvas.Font.Color;

    if FUseCache then begin
      imgWidth := GetImageWidth(Self, Index);
      imgHeight := GetImageHeight(Self, Index);
      if (CachedImages[Index] <> nil) and (CachedImages[Index].Width <> imgWidth) and (CachedImages[Index].Height <> imgHeight) then
        FreeAndNil(CachedImages[Index]);

      if (CachedImages[Index] <> nil) and (CachedImages[Index].Canvas.Font.Color <> ForeColor) then
        FreeAndNil(CachedImages[Index]);

      if CachedImages[Index] = nil then begin
        CachedImages[Index] := CreateBmp32(imgWidth, imgHeight);
        CachedImages[Index].Canvas.Font.Color := ForeColor;
        FAlphaImageList.GetBitmap32(Index, CachedImages[Index], GetImageHeight(Self));
      end;
      IcoBmp := CachedImages[Index];
    end
    else begin
      IcoBmp := CreateBmp32(GetImageWidth(Self, Index), GetImageHeight(Self, Index));
      IcoBmp.Canvas.Font.Color := ForeColor;
      FAlphaImageList.GetBitmap32(Index, IcoBmp, GetImageHeight(Self));
    end;
    TmpBmp := CreateBmpLike(IcoBmp);
    TmpBmp.Canvas.Lock;
    try
      deltaX := 0;
      deltaY := 0;
      if FAlphaImageList is TsCharImageList then begin
        if TsCharImageList(FAlphaImageList).Items[Index].ScalingFactor <> 1 then begin
          deltaX := (IcoBmp.Width  - GetImageWidth(Self)) div 2;
          deltaY := (IcoBmp.Height - GetImageHeight(Self)) div 2;
        end;
      end;
      BitBlt(TmpBmp.Canvas.Handle, 0, 0, TmpBmp.Width, TmpBmp.Height, Canvas.Handle, X - deltaX, Y - deltaY, SRCCOPY);
      CopyBmp32(MkRect(TmpBmp), MkRect(IcoBmp), TmpBmp, IcoBmp, MakeCacheInfo(TmpBmp), False, clNone, 0, False);
      BitBlt(Canvas.Handle, X - deltaX, Y - deltaY, TmpBmp.Width, TmpBmp.Height, TmpBmp.Canvas.Handle, 0, 0, SRCCOPY);
    finally
      TmpBmp.Canvas.Unlock;
      FreeAndNil(TmpBmp);
      if not FUseCache then
        FreeAndNil(IcoBmp);
    end;
    ForeColor := clNone;
  end;
end;


procedure TsVirtualImageList.GenerateStdList;
var
  i: integer;
  Bmp: TBitmap;
begin
  if HandleAllocated then begin
    ForeColor := clNone;
    AcChanging := True;
    ClearItems;
    inherited Clear;
    if FAlphaImageList <> nil then begin
      if FUseCache then
        SetLength(CachedImages, FAlphaImageList.Count);

      for i := 0 to FAlphaImageList.Count - 1 do begin
        Bmp := CreateBmp32(Width, Height);
        if FAlphaImageList.GetBitmap32(i, Bmp, GetImageHeight(Self)) then begin
          AddAsIcon(Bmp, -1);
          if FUseCache then begin
            FreeAndNil(CachedImages[i]);
            CachedImages[i] := Bmp;
          end
          else
            Bmp.Free;
        end;
      end;
    end;
    StdListIsGenerated := True;
    AcChanging := False;
  end;
end;


function TsVirtualImageList.GetBitmap32(Index: Integer; Image: TBitmap; GlyphHeight: integer = 0): Boolean;
begin
  Result := False;
  if HandleAllocated and (Index >= 0) and (FAlphaImageList <> nil) then begin
    if GlyphHeight = 0 then
      GlyphHeight := GetImageHeight(Self);

    if FUseCache then begin
      if Count > Index then begin
        if CachedImages[Index] = nil then begin
          CachedImages[Index] := CreateBmp32(Width, Height);
          FAlphaImageList.GetBitmap32(Index, CachedImages[Index], GlyphHeight);
          CachedImages[Index].Canvas.Font.Color := ForeColor;
        end;
        if CachedImages[Index] <> nil then
          CopyBmp(Image, CachedImages[Index]);
      end;
    end
    else
      if (FAlphaImageList <> nil) and (GetImageCount(FAlphaImageList) > Index) then begin
        FAlphaImageList.GetBitmap32(Index, Image, GlyphHeight);
        Result := True;
      end;
  end;
end;


{$IFNDEF DELPHI_XE8}
function TsVirtualImageList.Count: Integer;
{$ELSE}
function TsVirtualImageList.GetCount: Integer;
{$ENDIF}
begin
  if FAlphaImageList <> nil then
    Result := FAlphaImageList.Count
  else
    Result := 0;
end;


procedure TsVirtualImageList.ImageListChange(Sender: TObject);
begin
  UpdateList;
end;


procedure TsVirtualImageList.KillImgList;
begin
  if HandleAllocated and not ShareImages then
    ImageList_Destroy(Handle);

{$IFNDEF FPC}
  Handle := 0;
{$ENDIF}
  Change;
end;


procedure TsVirtualImageList.Loaded;
var
  i: integer;
begin
  inherited;
  KillImgList;
  CreateImgList;

  UpdateList(False);
  if DefaultManager <> nil then begin
    i := DefaultManager.GetScale;
    if i > 0 then begin
      Width  := MulDiv(Width,  aScalePercents[i], aScalePercents[CurrentScale]);
      Height := MulDiv(Height, aScalePercents[i], aScalePercents[CurrentScale]);
      CurrentScale := i;
    end;
  end
  else
    CurrentScale := 0;
end;


procedure TsVirtualImageList.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FAlphaImageList) then
    AlphaImageList := nil;
end;


{$IFDEF DELPHI7UP}
procedure TsVirtualImageList.ReadData(Stream: TStream);
begin
// Data is virtual
end;
{$ENDIF}


procedure TsVirtualImageList.RenderCacheNow(ItemIndex: integer = -1);
var
  i: integer;

  procedure MakeCache(AIndex: integer);
  begin
    if CachedImages[AIndex] = nil then begin
      CachedImages[AIndex] := CreateBmp32(Width, Height);
      FAlphaImageList.GetBitmap32(AIndex, CachedImages[AIndex], GetImageHeight(Self));
    end;
  end;

begin
  if UseCache then
    if ItemIndex >= 0 then
      for i := 0 to Count - 1 do
        MakeCache(i)
    else
      MakeCache(ItemIndex);
end;


function TsVirtualImageList.ScaleValue: real;
begin
  Result := GetImageWidth(Self) / GetImageWidth(AlphaImageList);
end;


procedure TsVirtualImageList.SetAlphaImageList(const Value: TacImageList);
begin
  if FAlphaImageList <> nil then
    FAlphaImageList.UnRegisterChanges(FImageChangeLink);

  FAlphaImageList := Value;
  if FAlphaImageList <> nil then
    FAlphaImageList.RegisterChanges(FImageChangeLink);

  if not AcChanging then
    HandleNeeded;

  UpdateList;
end;


procedure TsVirtualImageList.SetCharColor(AColor: TColor; AIndex: integer; IgnoreDefault: boolean);
begin
  if AlphaImageList is TsCharImageList then
    if IgnoreDefault then
      ForeColor := AColor
    else
      if (AIndex >= 0) and (TsCharImageList(AlphaImageList).Items[AIndex].Color <> clNone) then
        ForeColor := TsCharImageList(AlphaImageList).Items[AIndex].Color
      else
        ForeColor := AColor;
end;


procedure TsVirtualImageList.SetForeColor(const Value: TColor);
begin
  FForeColor := Value;
  if AlphaImageList is TsCharImageList then
    TsCharImageList(AlphaImageList).ForeColor := FForeColor;
end;


procedure TsVirtualImageList.SetInteger(const Index, Value: integer);
begin
  case Index of
    0: if FHeight <> Value then begin
      FHeight := Value;
      inherited Height := Value;
      UpdateList;
    end;

    1: if FWidth <> Value then begin
      FWidth := Value;
      inherited Width := Value;
      UpdateList;
    end;
  end;
end;


procedure TsVirtualImageList.SetUseCache(const Value: boolean);
begin
  FUseCache := Value;
  UpdateList;
end;


procedure TsVirtualImageList.UpdateList;
begin
  if (IgnoreGenerated or not StdListIsGenerated) and not AcChanging and HandleAllocated then begin
    GenerateStdList;
    StdListIsGenerated := Count > 0;
    Change;
  end;
end;


function TsVirtualImageList.CreateBitmap32(Index: Integer): TBitmap;
begin
  if Count > Index then begin
    if FUseCache then begin
      Result := TBitmap.Create;
      GetBitmap32(Index, Result, GetImageHeight(Self));
    end
    else
      if FAlphaImageList <> nil then
        Result := FAlphaImageList.CreateBitmap32(Index, Width, Height, GetImageHeight(Self))
      else
        Result := nil;
  end
  else
    Result := nil;
end;


function TsAlphaImageList.TryLoadFromPngStream(Stream: TStream): Boolean;
var
  Ico: HICON;
  Bmp: TBitmap;
  iFormat: TsImageFormat;
begin
  Result := False;
  Ico := 0;
  try
    if HandleAllocated and GetImageFormat(Stream, iFormat) then begin
      if IsDuplicated then // If double data used
        with TsImgListItem(Items.Add) do begin
          ImgData.LoadFromStream(Stream);
          ImageFormat := iFormat;
          case iFormat of
            ifPNG: begin
              PixelFormat := pf32bit;
              Bmp := TBitmap.Create;
              LoadBmpFromPngStream(Bmp, ImgData);
              Ico := MakeIcon32(Bmp);
              FreeAndNil(Bmp);
            end;
          end;
        end
      else
        case iFormat of
          ifPNG: begin
            Bmp := TBitmap.Create;

            LoadBmpFromPngStream(Bmp, Stream);
            Ico := MakeIcon32(Bmp);
            FreeAndNil(Bmp);
          end;
        end;

      if Ico <> 0 then begin
        Result := ImageList_AddIcon(Handle, Ico) >= 0;
        DestroyIcon(Ico);
      end;
      Change;
    end;
  except
    Result := False;
  end;
end;


procedure TsAlphaImageList.UpdateFromStd;
var
  ico: hIcon;
  icon: TIcon;
  i, c: integer;
begin
  Items.Clear;
  c := ImageList_GetImageCount(Handle);
  for i := 0 to c - 1 do begin
    ico := ImageList_GetIcon(Handle, i, ILD_NORMAL);
    if ico <> 0 then
      with TsImgListItem(Items.Add) do begin
        Icon := TIcon.Create;
        Icon.Handle := ico;
        OrigWidth := Icon.Width;
        OrigHeight := Icon.Height;

        Icon.SaveToStream(ImgData);
        FreeAndNil(Icon);
        ImageFormat := ifICO;
      end;
  end;
end;


procedure TsAlphaImageList.LoadFromPngStream(const Stream: TStream);
begin
  if not TryLoadFromPngStream(Stream) then
    MessageDlg('Cannot load from stream' + s_0D0A + 'Invalid or unexpected image format.', mtError, [mbOk], 0);
end;


{$IFDEF DELPHI_XE}
type
  TImageListWriteExProc = function(ImageList: HIMAGELIST; Flags: DWORD;
    Stream: IStream): HRESULT; {$IFNDEF CLR}stdcall;{$ENDIF}

var
  ImageListWriteExProc: TImageListWriteExProc;
{$ENDIF} // IFDEF DELPHI_XE


{$IFDEF DELPHI7UP}
procedure TsVirtualImageList.WriteData(Stream: TStream);
{$IFDEF DELPHI_XE}
var
  SA: TStreamAdapter;
  ComCtrlHandle: THandle;
  ImgLst: TImageList;
{$ENDIF}
begin
{$IFDEF DELPHI_XE}
  ComCtrlHandle := GetModuleHandle(comctl32);
  ImageListWriteExProc := GetProcAddress(ComCtrlHandle, 'ImageList_WriteEx');
  SA := TStreamAdapter.Create(Stream);
  try // Save empty bitmap, data is duplicated in Items
    ImgLst := TImageList.Create(Self);
    ImgLst.Width := 1;
    ImgLst.Height := 1;
    ImgLst.Masked := False;
    ImgLst.ColorDepth := cd4bit;
    if Assigned(ImageListWriteExProc) then
      ImageListWriteExProc(ImgLst.Handle, 1 {ILP_DOWNLEVEL}, SA)
    else
      ImageList_Write(ImgLst.Handle, SA);

    ImgLst.Free;
  finally
    SA.Free;
  end;
{$ENDIF}
end;
{$ENDIF} // IFDEF DELPHI7UP


procedure TacCharListItem.Assign(Source: TPersistent);
begin
  if Source <> nil then begin
    FScalingFactor := TacCharListItem(Source).ScalingFactor;
    FFontName    := TacCharListItem(Source).FontName;
    FCharset     := TacCharListItem(Source).Charset;
    FColor       := TacCharListItem(Source).Color;
    FOrientation := TacCharListItem(Source).Orientation;
    FPitch       := TacCharListItem(Source).Pitch;
    FStyle       := TacCharListItem(Source).Style;
    FChar        := TacCharListItem(Source).Char;
    FImageName   := TacCharListItem(Source).ImageName;
    FOffsetY     := TacCharListItem(Source).OffsetY;
  end
  else
    inherited;
end;


procedure TacCharListItem.AssignTo(Dest: TPersistent);
begin
  if Dest <> nil then
    Dest.Assign(Self)
  else
    inherited;
end;


constructor TacCharListItem.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FScalingFactor := 1;
  CacheBmp     := nil;
  FImageName   := '';
  FChar        := 0;
  FFontName    := s_FontAwesome;
  FColor       := clNone;
  FCharset     := DEFAULT_CHARSET;
  FOrientation := 0;
  FPitch       := fpDefault;
  FStyle       := [];
  FOffsetY     := 0;
end;


destructor TacCharListItem.Destroy;
begin
  if CacheBmp <> nil then
    FreeAndNil(CacheBmp);

  inherited Destroy;
end;


function TacCharListItem.GetImage: TBitmap;
var
  actHeight, actWidth: integer;
begin
  with ImgList do begin
    actHeight := GetImageHeight(TacCharListItems(Collection).FOwner, Index);
    actWidth  := GetImageWidth (TacCharListItems(Collection).FOwner, Index);
    if (CacheBmp <> nil) then
      if (CacheBmp.Width <> actWidth) or (CacheBmp.Height <> actHeight) or
           (ImgList.ForeColor <> clNone) and (CacheBmp.Canvas.Font.Color <> ForeColor) or
             (ForeColor = clNone) and (CacheBmp.Canvas.Font.Color <> Color) then
        FreeAndNil(CacheBmp);

    if CacheBmp = nil then begin
      CacheBmp := CreateBmp32(actWidth, actHeight);
      PrepareBmp(CacheBmp, Index, MkRect(CacheBmp), GetImageHeight(TacCharListItems(Collection).FOwner), OffsetY);
    end;
  end;
  Result := CacheBmp;
end;


function TacCharListItem.ImgList: TsCharImageList;
begin
  Result := TacCharListItems(Collection).FOwner;
end;


procedure TacCharListItem.Invalidate;
begin
  FreeAndNil(CacheBmp);
  ImgList.Change;
end;


function TacCharListItem.NotDefFont: boolean;
begin
  Result := FontName <> s_FontAwesome;
end;


function TacCharListItem.NotDefScaling: boolean;
begin
  Result := FScalingFactor <> 1;
end;


procedure TacCharListItem.SetCharset(const Value: TFontCharset);
begin
  if FCharset <> Value then begin
    FCharset := Value;
    Invalidate;
  end;
end;


procedure TacCharListItem.SetColor(const Value: TColor);
begin
  if FColor <> Value then begin
    FColor := Value;
    Invalidate;
  end;
end;


procedure TacCharListItem.SetFontName(const Value: string);
begin
  if FFontName <> Value then begin
    FFontName := Value;
    Invalidate;
  end;
end;


procedure TacCharListItem.SetOffsetY(const Value: integer);
begin
  if FOffsetY <> Value then begin
    FOffsetY := Value;
    Invalidate;
  end;
end;


procedure TacCharListItem.SetOrientation(const Value: integer);
begin
  if FOrientation <> Value then begin
    FOrientation := Value;
    Invalidate;
  end;
end;


procedure TacCharListItem.SetPitch(const Value: TFontPitch);
begin
  if FPitch <> Value then begin
    FPitch := Value;
    Invalidate;
  end;
end;


procedure TacCharListItem.SetScalingFactor(const Value: real);
begin
  if FScalingFactor <> Value then begin
    FScalingFactor := Value;
    Invalidate;
  end;
end;


procedure TacCharListItem.SetStyle(const Value: TFontStyles);
begin
  if FStyle <> Value then begin
    FStyle := Value;
    Invalidate;
  end;
end;


procedure TacCharListItem.SetChar(const Value: Word);
begin
  if FChar <> Value then begin
    FChar := Value;
    Invalidate;
  end;
end;

procedure TsCharImageList.AcBeginUpdate;
begin
  AcChanging := True;
end;


procedure TsCharImageList.AcEndUpdate(DoChange: boolean);
begin
  AcChanging := False;
  if DoChange then
    Change;
end;


function TsCharImageList.AddEmbeddedFont(FileName, FontName: acString): boolean;
var
  ef: TacEmbeddedFont;
begin
  if FileExists(FileName) then begin
    if (FontIndex(FontName) < 0) then begin
      ef := TacEmbeddedFont(EmbeddedFonts.Add);
      ef.FFontName := FontName;
      ef.FFileName := FileName;
      ef.FontData.LoadFromFile(FileName);
      ef.LoadFont;
    end;
    Result := True;
  end
  else
    Result := False;
end;


function TsCharImageList.AddItem(ItemData: TacCharItemData): integer;
var
  cli: TacCharListItem;
begin
  cli := TacCharListItem.Create(Items);
  cli.FChar           := ItemData.Char;
  cli.FFontName       := ItemData.FontName;
  cli.FCharset        := ItemData.Charset;
  cli.FColor          := ItemData.Color;
  cli.FOrientation    := ItemData.Orientation;
  cli.FPitch          := ItemData.Pitch;
  cli.FStyle          := ItemData.Style;
  cli.FScalingFactor  := ItemData.ScalingFactor;
  cli.OffsetY         := ItemData.OffsetY;

  AddAsIcon(cli.Image, -1);
  Result := Items.Count - 1;
end;


procedure TsCharImageList.AfterConstruction;
begin
  inherited;
  if not (csLoading in ComponentState) then
    FLoaded := True;
end;


procedure TsCharImageList.Assign(Source: TPersistent);
var
  ImageList: TsCharImageList;
begin
  if Source = nil then
    KillImgList
  else
    if Source is TsCharImageList then begin
      AcBeginUpdate;
      inherited Clear;
      ImageList := TsCharImageList(Source);
      Masked := ImageList.Masked;
      ImageType := ImageList.ImageType;
      DrawingStyle := ImageList.DrawingStyle;
      ShareImages := ImageList.ShareImages;
      Width := ImageList.Width;
      Height := ImageList.Height;
      KillImgList;
      SetNewDimensions(ImageList.Handle);
      SavedWidth := ImageList.SavedWidth;
      SavedHeight := ImageList.SavedHeight;
      if not HandleAllocated then
        CreateImgList
      else
        ImageList_SetIconSize(Handle, Width, Height);

      BkColor := GetColor(ImageList_GetBkColor(ImageList.Handle));
      BlendColor := ImageList.BlendColor;
      EmbeddedFonts.Assign(ImageList.EmbeddedFonts);
      CopyImages(ImageList);
      GenerateStdList;
      AcEndUpdate(False);
    end
    else
      inherited Assign(Source);
end;


procedure TsCharImageList.AssignTo(Dest: TPersistent);
begin
  if Dest is TsCharImageList then
    Dest.Assign(Self)
  else
    inherited AssignTo(Dest);
end;


function TsCharImageList.CanScale: boolean;
begin
  Result := FAllowScale and FLoaded and (DefaultManager <> nil) and not (csDesigning in ComponentState) and (Items <> nil) and (Items.Count > 0);
end;


procedure TsCharImageList.Change;
begin
  if not AcChanging and StdListIsGenerated and ([csLoading] * ComponentState = []) then
    if HandleAllocated then
      if Count <= Items.Count {If icon was not added using AddIcon or other std. way (not stored in Items)} then begin
        if Count <> Items.Count then begin
          AcChanging := True;
          GenerateStdList;
          AcChanging := False;
        end;
        inherited;
      end;
end;


procedure TsCharImageList.Clear;
begin
  Items.Clear;
  inherited Clear;
end;


procedure TsCharImageList.ClearCache;
var
  i: integer;
begin
  for i := 0 to Items.Count - 1 do
    FreeAndNil(Items[i].CacheBmp);
end;


procedure TsCharImageList.CopyImages(const ImgList: TsCharImageList);
var
  i: integer;
begin
  if HandleAllocated then begin
    ImageList_SetBkColor(ImgList.Handle, CLR_NONE);
    Items.Clear;
    for i := 0 to ImgList.Items.Count - 1 do
      TacCharListItem(Items.Add).Assign(ImgList.Items[i]);

    GenerateStdList;
  end;
end;


constructor TsCharImageList.Create(AOwner: TComponent);
begin
  inherited;
  FLoaded := False;
  FSavedScale := 0;
  Height := 24;
  Width := 24;
  FEmbeddedFonts := TacEmbeddedFonts.Create(Self);
  FItems := TacCharListItems.Create(Self);
  FAllowScale := True;
  FBkColor := clNone;
end;


function TsCharImageList.CreateBitmap32Color(Index, aWidth, aHeight: integer; CharColor: TColor; GlyphHeight: integer = 0): TBitmap;
begin
  Result := nil;
  if HandleAllocated and IsValidIndex(Index, Items.Count) then begin
    ForeColor := CharColor;
    Result := CreateBmp32(aWidth, aHeight);
    if GlyphHeight = 0 then
      GlyphHeight := GetImageHeight(Self);

    if FSavedScale <> 0 then
      PrepareBmp(Result, Index, MkRect(aWidth, aHeight), GlyphHeight, Items[Index].OffsetY * aScalePercents[FSavedScale] div 100)
    else
      PrepareBmp(Result, Index, MkRect(aWidth, aHeight), GlyphHeight, Items[Index].OffsetY);
  end;
end;


function TsCharImageList.CreateBitmap32(Index, aWidth, aHeight: integer; GlyphHeight: integer = 0): TBitmap;
begin
  Result := CreateBitmap32Color(Index, aWidth, aHeight, ForeColor, GlyphHeight);
end;


procedure TsCharImageList.CreateImgList;
var
  Size: TSize;
begin
  if (SavedWidth = 0) and FLoaded and not (csLoading in ComponentState) then begin
    SavedWidth := Width;
    SavedHeight := Height;
  end;
  if CanScale then
    Size := MkSize(DefaultManager.ScaleInt(SavedWidth), DefaultManager.ScaleInt(SavedHeight))
  else
    Size := MkSize(SavedWidth, SavedHeight);

  Handle := ImageList_Create(Size.cx, Size.cy, ILC_COLOR32 or (Integer(Masked) * ILC_MASK), 0, AllocBy);
end;


destructor TsCharImageList.Destroy;
begin
  FEmbeddedFonts.Free;
  FreeAndNil(FItems);
  inherited;
end;


procedure TsCharImageList.DoDraw(Index: Integer; Canvas: TCanvas; X, Y: Integer; Style: Cardinal; Enabled: Boolean);
var
  Bmp: TBitmap;
  imgWidth, imgHeight, deltaX, deltaY: integer;
begin
  imgWidth  := GetImageWidth (Self);
  imgHeight := GetImageHeight(Self);
  if HandleAllocated and IsValidIndex(Index, Items.Count) then begin
    Bmp := CreateBmp32(Width, Height);
    if Bmp <> nil then
      try
        Bmp.Canvas.Lock;
        if (Items[Index].ScalingFactor <> 1) or (Items[Index].OffsetY <> 0) then begin
          Bmp.Width := GetImageWidth(Self, Index);
          Bmp.Height := GetImageHeight(Self, Index);
          deltaX := (Bmp.Width - imgWidth) div 2;
          deltaY := (Bmp.Height - imgHeight) div 2;
        end
        else begin
          deltaX := 0;
          deltaY := 0;
        end;
        BitBlt(Bmp.Canvas.Handle, 0, 0, Bmp.Width, Bmp.Height, Canvas.Handle, X - deltaX, Y - deltaY, SRCCOPY);
        if Items[Index].Color <> clNone then
          ColorizeByMask(Bmp, Items[Index].Image, MkPoint, acColorToRGB(Items[Index].Color), clNone)
        else
          ColorizeByMask(Bmp, Items[Index].Image, MkPoint, acColorToRGB(Canvas.Font.Color), clNone);

        BitBlt(Canvas.Handle, X - deltaX, Y - deltaY, Bmp.Width, Bmp.Height, Bmp.Canvas.Handle, 0, 0, SRCCOPY);
        Bmp.Canvas.Unlock;
      finally
        FreeAndNil(Bmp);
      end;
  end;
end;


procedure TsCharImageList.PrepareBmp(Bmp: TBitmap; Index: integer; const aRect: TRect; CharHeight: integer; Offset: integer);
var
  Item: TacCharListItem;
  Delta, X, Y, dy: integer;
  S0, S: PRGBAArray_;
  ws: WideString;
  R, nR: TRect;
  alpha: byte;
  Col: TColor;
begin
  Bmp.PixelFormat := pf32bit;
  Item := Items[Index];

  Bmp.Canvas.Font.Pitch   := Item.Pitch;
  Bmp.Canvas.Font.Style   := Item.Style;

  Bmp.Canvas.Font.Charset := Item.Charset;
  Bmp.Canvas.Font.Name    := Item.FontName;
  Bmp.Canvas.Font.Color   := clBlack;
  ws := WideChar(Item.Char);

  Bmp.Canvas.Font.Height  := Round(CharHeight * Item.FScalingFactor);

  FillRect32(Bmp, MkRect(Bmp), clWhite, clBlack);
  // Calc
  R := MkRect;
  DrawTextW(Bmp.Canvas.Handle, PWideChar(ws), 1, R, DT_NOPREFIX or DT_CALCRECT or DT_EXTERNALLEADING or DT_NOCLIP);
//  nR := MkRect(Bmp);
  nR.Left := (Bmp.Width - WidthOf(R)) div 2;
  nR.Top := (Bmp.Height - HeightOf(R)) div 2;
  nR.Right := nR.Left + WidthOf(R);
  nR.Bottom := nR.Top + HeightOf(R);
  if Offset <> 0 then begin
    dy := Round(Offset * Item.FScalingFactor);
    OffsetRect(nR, 0, dy);
  end;

  DrawTextW(Bmp.Canvas.Handle, PWideChar(ws), 1, nR, DT_NOPREFIX or DT_VCENTER{ or DT_CENTER} or DT_EXTERNALLEADING or DT_NOCLIP);
  if ForeColor <> clNone then
    Col := SwapRedBlue(acColorToRGB(ForeColor))
  else
    if Item.Color <> clNone then
      Col := SwapRedBlue(acColorToRGB(Item.Color))
    else
      Col := 0;

  if InitLine(Bmp, Pointer(S0), Delta) then
    for Y := 0 to Bmp.Height - 1 do begin
      S := Pointer(PAnsiChar(S0) + Delta * Y);
      for X := 0 to Bmp.Width - 1 do
        with TsColor_(S[X]) do begin
          alpha := MaxByte - (R + G + B) div 3;
          C := Col;
          A := alpha;
        end;
    end;

  Bmp.Canvas.Font.Color := Col;
end;


function TsCharImageList.FontIndex(const aFontName: acString): integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to EmbeddedFonts.Count - 1 do
    if CompareText(EmbeddedFonts[i].FontName, aFontName) = 0 then begin
      Result := i;
      Exit;
    end;
end;


procedure TsCharImageList.GenerateStdList;
var
  i: integer;
  Bmp: TBitmap;
begin
  ForeColor := clNone;
  // Check if default size was not initialized yet
  if (SavedHeight = 0) and (inherited Width = 16) and (inherited Height = 16) and not AcChanging then begin
    AcChanging := True;
    KillImgList;
    Width  := inherited Width;
    Height := inherited Height;
  end;

  if SavedHeight <> 0 then begin
    if not HandleAllocated then
      CreateImgList;

    if HandleAllocated then begin
      AcChanging := True;
      inherited Clear;
      for i := 0 to Items.Count - 1 do begin
        Bmp := CreateBitmap32(i, GetImageWidth(Self, I), GetImageHeight(Self, I), GetImageHeight(Self));
        AddAsIcon(Bmp, -1);
        FreeAndNil(Bmp);
      end;
      if Items.Count > 0 then
        StdListIsGenerated := True;

      AcChanging := False;
    end;
  end;
end;


function TsCharImageList.GetBitmap32(Index: Integer; Image: TBitmap; GlyphHeight: integer = 0): Boolean;
begin
  if HandleAllocated and (Image <> nil) and IsValidIndex(Index, Count) then
    if (GetImageWidth(Self, Index) = Image.Width) and (GetImageHeight(Self, Index) = Image.Height) then
      Image.Assign(Items[Index].Image)
    else begin
      Image.PixelFormat := pf32bit;
      if GlyphHeight = 0 then
        GlyphHeight := Image.Height;

      PrepareBmp(Image, Index, MkRect(Image), GlyphHeight, Items[Index].OffsetY * GlyphHeight div GetImageHeight(Self));
    end;

  Result := True;
end;


function TsCharImageList.GetBkColor: TColor;
begin
  if FBkColor = clNone then
    Result := inherited BkColor
  else
    Result := FBkColor;
end;


{$IFNDEF DELPHI_XE8}
function TsCharImageList.Count: Integer;
{$ELSE}
function TsCharImageList.GetCount: Integer;
{$ENDIF}
begin
  if Items <> nil then
    Result := Items.Count
  else
    Result := 0;
end;


function TsCharImageList.GetDimension(const Index: Integer): integer;
begin
  ScaleSize;
  if Index = 0 then
    Result := inherited Height
  else
    Result := inherited Width;
end;


procedure TsCharImageList.KillImgList;
begin
  if HandleAllocated and not ShareImages then
    ImageList_Destroy(Handle);

  Handle := 0;
  if not AcChanging then
    Change;
end;


procedure TsCharImageList.LoadAllFonts;
var
  i: integer;
begin
  for i := 0 to EmbeddedFonts.Count - 1 do
    EmbeddedFonts[i].LoadFont;
end;


procedure TsCharImageList.Loaded;
var
  w, h: integer;
begin
  inherited;
  LoadAllFonts;
  FLoaded := True;
  if SavedWidth = 0 then begin
    w := inherited Width;
    h := inherited Height;
    if (w = 16) and (h = 16) then begin // If default size - reinit
      AcChanging := True;
      KillImgList;
      SetDimension(1, w);
      SetDimension(0, h);
    end;
  end
  else
    KillImgList;

  if SavedWidth = 0 then
    SetDimension(1, Width);

  if SavedHeight = 0 then
    SetDimension(0, Height);

  Items.CheckItems;
  if not StdListIsGenerated then begin
    GenerateStdList;
    StdListIsGenerated := True; // Set the flag even if iconlist is empty
    Change;
  end;
end;


procedure TsCharImageList.ScaleSize;
begin
  if CanScale and not (csLoading in ComponentState) and (SavedWidth <> 0) then
    ScaleValue := DefaultManager.GetScale;
end;


procedure TsCharImageList.SetBkColor(const Value: TColor);
begin
  FBkColor := Value;
  inherited BkColor := Value;
  if HandleAllocated then
    ImageList_SetBkColor(Handle, Value);
end;


procedure TsCharImageList.SetCharColor(AColor: TColor; AIndex: integer; IgnoreDefault: boolean);
begin
  if IgnoreDefault then
    ForeColor := AColor
  else
    if (AIndex >= 0) and (Items[AIndex].Color <> clNone) then
      ForeColor := Items[AIndex].Color
    else
      ForeColor := AColor;
end;


procedure TsCharImageList.SetDimension(const Index, Value: integer);

  procedure SetScaleValue;
  begin
    if DefaultManager <> nil then
      ScaleValue := DefaultManager.GetScale
    else
      ScaleValue := 0;
  end;

begin
  case Index of
    0: begin
      if FLoaded then begin
        SavedHeight := Value;
        SetScaleValue;
      end;
      inherited Height := Value;
    end;

    1: begin
      if FLoaded then begin
        SavedWidth := Value;
        SetScaleValue;
      end;
      inherited Width := Value;
    end;
  end;
  if not (csLoading in ComponentState) and FLoaded and (Count <= Items.Count) and not AcChanging {If can generate new} then begin
    GenerateStdList;
    Change;
  end;
end;


procedure TsCharImageList.SetEmbeddedFonts(const Value: TacEmbeddedFonts);
begin
  FEmbeddedFonts.Assign(Value);
end;


procedure TsCharImageList.SetItems(const Value: TacCharListItems);
begin
  FItems.Assign(Value);
end;


procedure TsCharImageList.SetNewDimensions(Value: HImageList);
var
  AHeight, AWidth: Integer;
begin
  AWidth := Width;
  AHeight := Height;
  ImageList_GetIconSize(Value, AWidth, AHeight);
  Width := AWidth;
  Height := AHeight;
end;


procedure TsCharImageList.SetNewScale(Value: Integer);
var
  b: boolean;
begin
  if (FSavedScale <> Value) and CanScale then begin
    b := AcChanging;
    AcChanging := True;
    FSavedScale := Value;
    inherited Clear;
    KillImgList;
    GenerateStdList;
    AcChanging := b;
    if not AcChanging then
      inherited Change;
  end;
end;


procedure TsCharImageList.UnLoadAllFonts;
var
  i: integer;
begin
  for i := 0 to EmbeddedFonts.Count - 1 do
    EmbeddedFonts[i].UnloadFont;
end;


procedure TsCharImageList.UpdateStd(i: integer);
var
  Bmp: TBitmap;
begin
  ForeColor := clNone;
  Bmp := Items[i].Image;
  if (Items[i].ScalingFactor <> 1) or (Items[i].OffsetY <> 0) then
    AddAsIcon(Bmp, i)
  else
    ImageList_Replace(Handle, i, Bmp.Handle, Bmp.MaskHandle);
end;


procedure TacCharListItems.CheckItems;
var
  i: integer;
begin
  for i := 0 to Count - 1 do
    if Items[i].FScalingFactor = 0 then begin
      Items[i].FScalingFactor := GetDefScaling(Items[i].Charset, Items[i].FontName);
      Items[i].Color := clNone;
    end;
end;


constructor TacCharListItems.Create(AOwner: TsCharImageList);
begin
  inherited Create(TacCharListItem);
  FOwner := AOwner;
end;


destructor TacCharListItems.Destroy;
begin
  inherited Destroy;
  FOwner := nil;
end;


function TacCharListItems.GetItem(Index: Integer): TacCharListItem;
begin
  Result := TacCharListItem(inherited GetItem(Index));
end;


function TacCharListItems.GetOwner: TPersistent;
begin
  Result := FOwner;
end;


procedure TacCharListItems.SetItem(Index: Integer; Value: TacCharListItem);
begin
  inherited SetItem(Index, Value);
end;


constructor TacImageList.Create(AOwner: TComponent);
begin
  inherited;
  ForeColor := clNone;
  IgnoreTransparency := False;
  if acListsArray = nil then
    acListsArray := TList.Create;

  acListsArray.Add(Self);
end;


procedure TacImageList.AddAsIcon(Bmp: TBitmap; Ndx: integer);
var
  Ico: hIcon;
  TmpBmp: TBitmap;
begin
  if Bmp <> nil then begin
    if ((Bmp.Width <> Width) or (Bmp.Height <> Height)) then begin
      TmpBmp := CreateBmp32(Width, Height);
      FillRect32(TmpBmp, MkRect(TmpBmp), 0, 0);
      if (Bmp.Height > 1) and (Bmp.Width > 1) then
        Stretch(Bmp, TmpBmp, TmpBmp.Width, TmpBmp.Height, ftMitchell)
//        BitBlt(TmpBmp.Canvas.Handle, 0, 0, TmpBmp.Width, TmpBmp.Height, Bmp.Canvas.Handle, (Bmp.Width - TmpBmp.Width) div 2, (Bmp.Height - TmpBmp.Height) div 2, SRCCOPY)
      else
        FillRect32(TmpBmp, MkRect(TmpBmp), 0, 1);
    end
    else
      TmpBmp := Bmp;

    if IsNTFamily and {$IFDEF DELPHI7UP}acThemesEnabled{$ELSE}False{$ENDIF} then
      Ico := MakeCompIcon(TmpBmp, ColorToRGB(TColor(ImageList_GetBkColor(Handle))))
    else
      Ico := MakeIcon32(TmpBmp);

    if Ndx < 0 then
      ImageList_AddIcon(Handle, Ico)
    else
      ImageList_ReplaceIcon(Handle, Ndx, Ico);

    DestroyIcon(Ico);
    if TmpBmp <> Bmp then
      TmpBmp.Free;
  end;
end;


{$IFDEF DELPHI7UP}
procedure TacImageList.ReadData(Stream: TStream);
begin
// Data is duplicated in Items
end;


procedure TacImageList.WriteData(Stream: TStream);
{$IFDEF DELPHI_XE}
var
  SA: TStreamAdapter;
  ComCtrlHandle: THandle;
  ImgLst: TImageList;
{$ENDIF}
begin
{$IFDEF DELPHI_XE}
  ComCtrlHandle := GetModuleHandle(comctl32);
  ImageListWriteExProc := GetProcAddress(ComCtrlHandle, 'ImageList_WriteEx');
  SA := TStreamAdapter.Create(Stream);
  try // Save empty bitmap, data is duplicated in Items
    ImgLst := TImageList.Create(Self);
    ImgLst.Width := 1;
    ImgLst.Height := 1;
    ImgLst.Masked := False;
    ImgLst.ColorDepth := cd4bit;
    if Assigned(ImageListWriteExProc) then
      ImageListWriteExProc(ImgLst.Handle, 1 {ILP_DOWNLEVEL}, SA)
    else
      ImageList_Write(ImgLst.Handle, SA);

    ImgLst.Free;
  finally
    SA.Free;
  end;
{$ENDIF}
end;
{$ENDIF} // IFDEF DELPHI7UP


procedure TacEmbeddedFonts.Assign(Source: TPersistent);
var
  i: integer;
begin
  Clear;

{$IFNDEF NOFONTRES}
  if (TacEmbeddedFonts(Source).Count = 0) or (TacEmbeddedFonts(Source)[0].FontData.Size <> 0) then
    TacEmbeddedFont(Add).FontName := 'FontAwesome';
{$ENDIF}


  for i := 0 to TacEmbeddedFonts(Source).Count - 1 do
    TacEmbeddedFont(Add).Assign(TacEmbeddedFonts(Source)[i]);
end;


constructor TacEmbeddedFonts.Create(AOwner: TsCharImageList);
begin
  inherited Create(TacEmbeddedFont);
  FOwner := AOwner;
end;


destructor TacEmbeddedFonts.Destroy;
begin
  inherited Destroy;
  FOwner := nil;
end;


function TacEmbeddedFonts.GetItem(Index: Integer): TacEmbeddedFont;
begin
  Result := TacEmbeddedFont(inherited GetItem(Index));
end;


function TacEmbeddedFonts.GetOwner: TPersistent;
begin
  Result := FOwner;
end;


procedure TacEmbeddedFonts.SetItem(Index: Integer; const Value: TacEmbeddedFont);
begin
  inherited SetItem(Index, Value);
end;


procedure TacEmbeddedFont.Assign(Source: TPersistent);
begin
  FFileName := TacEmbeddedFont(Source).FileName;
  FFontName := TacEmbeddedFont(Source).FontName;
  TacEmbeddedFont(Source).FontData.SaveToStream(FontData);
end;


procedure TacEmbeddedFont.AssignTo(Dest: TPersistent);
begin
  if Dest <> nil then
    Dest.Assign(Self)
  else
    inherited;
end;


constructor TacEmbeddedFont.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FontData := TMemoryStream.Create;
end;


procedure TacEmbeddedFont.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineBinaryProperty('FontData', ReadData, WriteData, True);
end;


destructor TacEmbeddedFont.Destroy;
begin
  FontData.Free;
  inherited Destroy;
end;


procedure TacEmbeddedFont.LoadFont;
var
  FontCount: DWord;
begin
  if FontData.Size > 0 then begin
    FontCount := 1;
    Handle := AddFontMemResourceEx(FontData.Memory, FontData.Size, nil, @FontCount);
  end
  else
    Handle := 0;
end;


procedure TacEmbeddedFont.ReadData(Reader: TStream);
begin
  FontData.LoadFromStream(Reader);
end;


procedure TacEmbeddedFont.UnLoadFont;
begin
  if Handle <> 0 then
    RemoveFontMemResourceEx(Handle);
end;


procedure TacEmbeddedFont.WriteData(Writer: TStream);
begin
  FontData.SaveToStream(Writer);
end;


destructor TacImageList.Destroy;
begin
  if acListsArray <> nil then
    acListsArray.Extract(Self);

  inherited;
end;


{$IFDEF ACDEBUG}
procedure TacImageList.SetForeColor(const Value: TColor);
begin
  FForeColor := Value;
end;
{$ENDIF}


{$IFNDEF NOFONTRES}
var
  ResStream: TResourceStream;
  FontsCount: DWord;
  awFont: THandle = 0;
{$ENDIF}


initialization

{$IFNDEF NOFONTRES}
{$R acFontData.res}
  if awFont = 0 then begin
    ResStream := TResourceStream.Create(hInstance, 'awFont', RT_RCDATA);
    awFont := AddFontMemResourceEx(ResStream.Memory, ResStream.Size, nil, @FontsCount);
    ResStream.Free;
  end;
{$ENDIF}


finalization
{$IFNDEF NOFONTRES}
  if awFont <> 0 then
    RemoveFontMemResourceEx(awFont);
{$ENDIF}

  FreeAndNil(acListsArray);

end.
