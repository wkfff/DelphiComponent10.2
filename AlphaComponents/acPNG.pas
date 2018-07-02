//  PNG Library
//  Last Update: 14 august 2008
//  Vereshagin Roman Vladimirovich

unit acPNG;
{$I sDefs.inc}

interface

uses
  Windows, Graphics, SysUtils, Classes, Math,
{$IFDEF FPC}
  fpimage,
  ZLibEx, ZLibExAPI,
{$ELSE}
  {$IFNDEF WIN64}acZLibEx, {$ELSE}ZLib, {$ENDIF}
{$ENDIF}
  sConst;


const
  DefaultDisplayGamma = 2.2;

  
type
  TColorScheme = (csUnknown, csIndexed, csG, csGA, csRGB, csRGBA, csBGR, csBGRA, csCMY, csCMYK, csCIELab, csYCbCr, csPhotoYCC );

  TConvertOptions = set of (coAlpha, coApplyGamma, coNeedByteSwap, coLabByteRange, coLabChromaOffset);

  TRawPaletteFormat = (pfInterlaced8Triple, pfInterlaced8Quad, pfPlane8Triple, pfPlane8Quad,
                       pfInterlaced16Triple, pfInterlaced16Quad, pfPlane16Triple, pfPlane16Quad);

  TConversionMethod = procedure(const Source: array of Pointer; Target: Pointer; Count: Cardinal; Mask: Byte) of object;

  TColorManager = class
  private
    FChanged: Boolean;
    FSourceBPS, FTargetBPS, FSourceSPP, FTargetSPP: Byte;
    FMainGamma, FDisplayGamma: Single;
    FGammaTable: array[Byte] of Byte;
    FYCbCrCoefficients: array[0..2] of Single;
    FCrToRedTable, FCbToBlueTable, FCrToGreenTable, FCbToGreenTable: array of Integer;
    FSourceScheme, FTargetScheme: TColorScheme;
    FRowConversion: TConversionMethod;
    FSourceOptions, FTargetOptions: TConvertOptions;
  protected
    function ComponentGammaConvert(Value: Byte): Byte;
    function ComponentNoConvert16(Value: Word): Word;
    function ComponentNoConvert8(Value: Byte): Byte;
    function ComponentScaleConvert(Value: Word): Byte;
    function ComponentScaleGammaConvert(Value: Word): Byte;
    function ComponentSwapScaleGammaConvert(Value: Word): Byte;
    function ComponentSwapScaleConvert(Value: Word): Byte;
    function ComponentSwapConvert(Value: Word): Word;
    procedure RowConvertBGR2BGR         (const Source: array of Pointer; Target: Pointer; Count: Cardinal; Mask: Byte);
    procedure RowConvertBGR2RGB         (const Source: array of Pointer; Target: Pointer; Count: Cardinal; Mask: Byte);
    procedure RowConvertCIELAB2BGR      (const Source: array of Pointer; Target: Pointer; Count: Cardinal; Mask: Byte);
    procedure RowConvertCIELAB2RGB      (const Source: array of Pointer; Target: Pointer; Count: Cardinal; Mask: Byte);
    procedure RowConvertCMYK2BGR        (const Source: array of Pointer; Target: Pointer; Count: Cardinal; Mask: Byte);
    procedure RowConvertCMYK2RGB        (const Source: array of Pointer; Target: Pointer; Count: Cardinal; Mask: Byte);
    procedure RowConvertGray            (const Source: array of Pointer; Target: Pointer; Count: Cardinal; Mask: Byte);
    procedure RowConvertIndexed8        (const Source: array of Pointer; Target: Pointer; Count: Cardinal; Mask: Byte);
    procedure RowConvertIndexedBoth16   (const Source: array of Pointer; Target: Pointer; Count: Cardinal; Mask: Byte);
    procedure RowConvertIndexedSource16 (const Source: array of Pointer; Target: Pointer; Count: Cardinal; Mask: Byte);
    procedure RowConvertIndexedTarget16 (const Source: array of Pointer; Target: Pointer; Count: Cardinal; Mask: Byte);
    procedure RowConvertRGB2BGR         (const Source: array of Pointer; Target: Pointer; Count: Cardinal; Mask: Byte);
    procedure RowConvertRGB2RGB         (const Source: array of Pointer; Target: Pointer; Count: Cardinal; Mask: Byte);
    procedure RowConvertPhotoYCC2BGR    (const Source: array of Pointer; Target: Pointer; Count: Cardinal; Mask: Byte);
    procedure RowConvertPhotoYCC2RGB    (const Source: array of Pointer; Target: Pointer; Count: Cardinal; Mask: Byte);
    procedure RowConvertYCbCr2BGR       (const Source: array of Pointer; Target: Pointer; Count: Cardinal; Mask: Byte);
    procedure RowConvertYCbCr2RGB       (const Source: array of Pointer; Target: Pointer; Count: Cardinal; Mask: Byte);
    procedure CreateYCbCrLookup;
    function GetPixelFormat(Index: Integer): TPixelFormat;
    procedure PrepareConversion;
    procedure SetSourceBitsPerSample(const Value: Byte);
    procedure SetTargetBitsPerSample(const Value: Byte);
    procedure SetSourceColorScheme(const Value: TColorScheme);
    procedure SetTargetColorScheme(const Value: TColorScheme);
    procedure SetSourceSamplesPerPixel(const Value: Byte);
    procedure SetTargetSamplesPerPixel(const Value: Byte);
  public
    constructor Create;
    procedure ConvertRow(const Source: array of Pointer; Target: Pointer; Count: Cardinal; Mask: Byte);
    function CreateColorPalette(const Data: array of Pointer; DataFormat: TRawPaletteFormat; ColorCount: Cardinal; RGB: Boolean): HPALETTE;
    function CreateGrayscalePalette(MinimumIsWhite: Boolean): HPALETTE;
    procedure Error(const Msg: String);
    procedure SetGamma(MainGamma: Single; DisplayGamma: Single = DefaultDisplayGamma);
    property SourceBitsPerSample: Byte read FSourceBPS write SetSourceBitsPerSample;
    property SourceColorScheme: TColorScheme read FSourceScheme write SetSourceColorScheme;
    property SourceOptions: TConvertOptions read FSourceOptions write FSourceOptions;
    property SourcePixelFormat: TPixelFormat index 0 read GetPixelFormat;
    property SourceSamplesPerPixel: Byte read FSourceSPP write SetSourceSamplesPerPixel;
    property TargetBitsPerSample: Byte read FTargetBPS write SetTargetBitsPerSample;
    property TargetColorScheme: TColorScheme read FTargetScheme write SetTargetColorScheme;
    property TargetOptions: TConvertOptions read FTargetOptions write FTargetOptions;
    property TargetPixelFormat: TPixelFormat index 1 read GetPixelFormat;
    property TargetSamplesPerPixel: Byte read FTargetSPP write SetTargetSamplesPerPixel;
  end;


type
  TByteArray     = array of byte;
  TCardinalArray = array of Cardinal;
  TFloatArray    = array of Single;

  TLZ77Decoder = class
  private
    FStream: TZStreamRec;
    FZLibResult,
    FFlushMode: Integer;
    FAutoReset: Boolean;
    function GetAvailableInput: Integer;
    function GetAvailableOutput: Integer;
  public
    constructor Create(FlushMode: Integer; AutoReset: Boolean);
    procedure Decode(var Source, Dest: Pointer; PackedSize, UnpackedSize: Integer);
    procedure DecodeEnd;
    procedure DecodeInit;
    property AvailableInput: Integer read GetAvailableInput;
    property AvailableOutput: Integer read GetAvailableOutput;
    property ZLibResult: Integer read FZLibResult;
  end;

  TImageOptions = set of (ioBigEndian, ioUseGamma);
  TCompressionType = (ctUnknown, ctNone, ctLZ77);


  PImageProperties = ^TImageProperties;
  TImageProperties = record
    Options         : TImageOptions;
    Width           : Cardinal;
    ColorScheme     : TColorScheme;
    BitsPerSample   : Byte;
    SamplesPerPixel : Byte;
    Compression     : TCompressionType;
  end;
  TChunkType = array[0..3] of AnsiChar;
  TPNGChunkHeader = packed record
    Length: Cardinal;
    ChunkType: TChunkType;
  end;

  
  TPNGGraphic = class(TBitmap)
  private
    FColorManager     : TColorManager;
    FProgressRect     : TRect;
    FBasePosition     : Cardinal;
    FStream           : TStream;
    FImageProperties  : TImageProperties;
    FDecoder          : TLZ77Decoder;
    FIDATSize         : Integer;
    FRawBuffer        : Pointer;
    FCurrentSource    : Pointer;
    FHeader           : TPNGChunkHeader;
    FCurrentCRC       : Cardinal;
    FSourceBPP        : Integer;
    FPalette          : HPALETTE;
    FTransparency     : TByteArray;
    FTransparentColor : TColor;
    FBackgroundColor  : TColor;
    procedure ApplyFilter(Filter: Byte; Line, PrevLine, Target: PByte; BPP, BytesPerRow: Integer);
    function IsChunk(const ChunkType: TChunkType): Boolean;
    function LoadAndSwapHeader: Cardinal;
    procedure LoadBackgroundColor(const Description);
    procedure LoadIDAT(const Description);
    procedure LoadTransparency(const Description);
    procedure ReadDataAndCheckCRC;
    procedure ReadRow(RowBuffer: Pointer; BytesPerRow: Integer);
    function SetupColorDepth(ColorType, BitDepth: Integer): Integer;
  public
    Reflected: boolean;
    constructor Create; override;
    destructor Destroy; override;
    procedure Draw(ACanvas: TCanvas; const Rect: TRect); override;
    procedure Assign(Source: TPersistent); override;
    class function CanLoad(const FileName: String): Boolean; overload;
    class function CanLoad(Stream: TStream): Boolean; overload;
{$IFDEF DELPHI_10TOKYO}
    class function CanLoadFromStream(Stream: TStream): Boolean; override;
{$ENDIF}
    procedure LoadFromStream(Stream: TStream); override;
    function ReadImageProperties(Stream: TStream; ImageIndex: Cardinal): Boolean;
    property BackgroundColor: TColor read FBackgroundColor;
    property Transparency: TByteArray read FTransparency;
    property ColorManager: TColorManager read FColorManager;
    property ImageProperties: TImageProperties read FImageProperties write FImageProperties;
  end;


var
  UseACPng: boolean = True;

procedure UpdateTransparency(Bmp: TBitmap; Png: TPNGGraphic);

implementation

uses
{$IFNDEF NOACPNG}
  {$IFDEF D2010} pngimage, {$ENDIF}
{$ENDIF}
  sGraphUtils, acntUtils, sAlphaGraph, sDefaults;


var
  // used to calculate the running CRC of a bunch of bytes,
  // this table is dynamically created in order to save space if never needed
  CRCTable: array of Cardinal;


const
  gesInvalidImage = 'Cannot load image. Invalid or unexpected %s image format.';
  gesInvalidColorFormat = 'Invalid color format in %s file.';
  gesUnsupportedFeature = 'Cannot load image. %s not supported for %s files.';
  gesInvalidCRC = 'Cannot load image. CRC error found in %s file.';
  gesCompression = 'Cannot load image. Compression error found in %s file.';
  gesExtraCompressedData = 'Cannot load image. Extra compressed data found in %s file.';
  gesInvalidPalette = 'Cannot load image. Palette in %s file is invalid.';
  gesUnknownCriticalChunk = 'Cannot load PNG image. Unexpected but critical chunk detected.';
  gesIndexedNotSupported = 'Conversion between indexed and non-indexed pixel formats is not supported.';
  gesConversionUnsupported = 'Color conversion failed. Could not find a proper method.';
  gesInvalidSampleDepth = 'Color depth is invalid. Bits per sample must be 1, 2, 4, 8 or 16.';
  gesInvalidPixelDepth = 'Sample count per pixel does not correspond to the given color scheme.';
  gesInvalidSubSampling = 'Subsampling value is invalid. Allowed are 1, 2 and 4.';
  gesVerticalSubSamplingError = 'Vertical subsampling value must be <= horizontal subsampling value.';
  gesPreparing = 'Preparing...';
  gesLoadingData = 'Loading data...';
  gesUpsampling = 'Upsampling...';
  gesTransfering = 'Transfering...';

  
procedure MakeCRCTable;
// creates the CRC table when it is needed the first time
var
  C: Cardinal;
  N, K: Integer;
  Poly: Cardinal; // polynomial exclusive-or pattern
const
 // terms of polynomial defining this CRC (except x^32)
 P: array [0..13] of Byte = (0, 1, 2, 4, 5, 7, 8, 10, 11, 12, 16, 22, 23, 26);
begin
  // make exclusive-or pattern from polynomial ($EDB88320)
  SetLength(CRCTable, 256);
  Poly := 0;
  for N := 0 to SizeOf(P) - 1 do
    Poly := Poly or (1 shl (31 - P[N]));

  for N := 0 to $FF do begin
    C := N;
    for K := 0 to 7 do
      if (C and 1) <> 0 then
        C := Poly xor (C shr 1)
      else
        C := C shr 1;

    CRCTable[N] := C;
  end;
end;


function CRC32(CRC: Cardinal; Buffer: PByte; Len: Cardinal): Cardinal;
begin
  if Buffer = nil then
    Result := 0
  else begin
    if CRCTable = nil then
      MakeCRCTable;

    CRC := CRC xor $FFFFFFFF;
    while Len >= 8 do begin
      CRC := CRCTable[Byte(CRC) xor Buffer^] xor (CRC shr 8);
      Inc(Buffer);
      CRC := CRCTable[Byte(CRC) xor Buffer^] xor (CRC shr 8);
      Inc(Buffer);
      CRC := CRCTable[Byte(CRC) xor Buffer^] xor (CRC shr 8);
      Inc(Buffer);
      CRC := CRCTable[Byte(CRC) xor Buffer^] xor (CRC shr 8);
      Inc(Buffer);
      CRC := CRCTable[Byte(CRC) xor Buffer^] xor (CRC shr 8);
      Inc(Buffer);
      CRC := CRCTable[Byte(CRC) xor Buffer^] xor (CRC shr 8);
      Inc(Buffer);
      CRC := CRCTable[Byte(CRC) xor Buffer^] xor (CRC shr 8);
      Inc(Buffer);
      CRC := CRCTable[Byte(CRC) xor Buffer^] xor (CRC shr 8);
      Inc(Buffer);

      Dec(Len, 8);
    end;

    while Len > 0 do begin
      CRC := CRCTable[(CRC xor Buffer^) and $FF] xor (CRC shr 8);
      Inc(Buffer);
      Dec(Len);
    end;
    Result := CRC xor $FFFFFFFF;
  end;
end;


type
  EGraphicCompression = class(Exception);


procedure GraphicExError(const ErrorString: String); overload;
begin
  raise EInvalidGraphic.Create(ErrorString);
end;


procedure GraphicExError(const ErrorString: String; const Args: array of const); overload;
begin
  raise EInvalidGraphic.CreateFmt(ErrorString, Args);
end;


procedure CompressionError(const ErrorString: String); overload;
begin
  raise EGraphicCompression.Create(ErrorString);
end;


procedure UpdateTransparency(Bmp: TBitmap; Png: TPNGGraphic);
var
  DeltaS, DeltaD, X, Y: integer;
  D0, D: PRGBAArray_;
  S0, S: PByteArray;
  TransColor: TColor;
  b: boolean;
begin
  b := Bmp.Transparent;
  if (Png.ImageProperties.ColorScheme in [csIndexed]) and (Length(Png.Transparency) > 0) then begin // Adding of indexed transparency
    Bmp.Assign(Png);
    Bmp.PixelFormat := pf32bit;
    if InitLine(Png, Pointer(S0), DeltaS) and InitLine(Bmp, Pointer(D0), DeltaD) then
      for Y := 0 to Png.Height - 1 do begin
        S := Pointer(PAnsiChar(S0) + DeltaS * Y);
        D := Pointer(PAnsiChar(D0) + DeltaD * Y);
        for X := 0 to Png.Width - 1 do
          D[X].A := Png.Transparency[S[X]];
      end;
  end
  else begin
    Bmp.Assign(Png);
    if Bmp.PixelFormat <> pf32bit then begin // If alpha-channell is empty
      Bmp.PixelFormat := pf32bit;
      if InitLine(Bmp, Pointer(D0), DeltaD) then
        if (ioUseGamma in Png.ImageProperties.Options) then begin
          if not (coApplyGamma in Png.ColorManager.TargetOptions) then
            for Y := 0 to Png.Height - 1 do begin
              D := Pointer(PAnsiChar(D0) + DeltaD * Y);
              for X := 0 to Png.Width - 1 do begin
                D[X].C := 0;
                D[X].A := MaxByte - D[X].R;
              end;
            end
          else
            if InitLine(Png, Pointer(S0), DeltaS) then
              if Png.ColorManager.SourcePixelFormat <> pf24Bit then
                for Y := 0 to Png.Height - 1 do begin
                  S := Pointer(PAnsiChar(S0) + DeltaS * Y);
                  D := Pointer(PAnsiChar(D0) + DeltaD * Y);
                  for X := 0 to Png.Width - 1 do begin
                    D[X].C := 0;
                    D[X].A := MaxByte - Png.ColorManager.ComponentGammaConvert(S[X]);
                  end;
                end

              else
                for Y := 0 to Png.Height - 1 do begin
                  D := Pointer(PAnsiChar(D0) + DeltaD * Y);
                  for X := 0 to Png.Width - 1 do
                    D[X].A := MaxByte;
                end

        end
        else
          if not b then begin
            for Y := 0 to Png.Height - 1 do begin
              D := Pointer(PAnsiChar(D0) + DeltaD * Y);
              for X := 0 to Png.Width - 1 do
                D[X].A := MaxByte;
            end
          end
          else begin
            TransColor := Bmp.Canvas.Pixels[0, Bmp.Height - 1];
            for Y := 0 to Png.Height - 1 do begin
              D := Pointer(PAnsiChar(D0) + DeltaD * Y);
              for X := 0 to Png.Width - 1 do
                if D[X].C <> TransColor then
                  D[X].A := MaxByte;
            end;
          end;
    end;
  end;
end;


constructor TLZ77Decoder.Create(FlushMode: Integer; AutoReset: Boolean);
begin
  FillChar(FStream, SizeOf(FStream), 0);
  FFlushMode := FlushMode;
  FAutoReset := AutoReset;
end;


procedure TLZ77Decoder.Decode(var Source, Dest: Pointer; PackedSize, UnpackedSize: Integer);
begin
  FStream.next_in := Source;
  FStream.avail_in := PackedSize;
  if FAutoReset then
    FZLibResult := InflateReset(FStream);

  if FZLibResult = Z_OK then begin
    FStream.next_out := Dest;
    FStream.avail_out := UnpackedSize;
    FZLibResult := Inflate(FStream, FFlushMode);

    Source := FStream.next_in;
    Dest := FStream.next_out;
  end;
end;


procedure TLZ77Decoder.DecodeEnd;
begin
  if InflateEnd(FStream) < 0 then
    CompressionError('LZ77 decompression error.');
end;


procedure TLZ77Decoder.DecodeInit;
begin
  if InflateInit(FStream) < 0 then
    CompressionError('LZ77 decompression error.');
end;


function TLZ77Decoder.GetAvailableInput: Integer;
begin
  Result := FStream.avail_in;
end;


function TLZ77Decoder.GetAvailableOutput: Integer;
begin
  Result := FStream.avail_out;
end;


const
  IHDR = 'IHDR';
  IDAT = 'IDAT';
  IEND = 'IEND';
  PLTE = 'PLTE';
  gAMA = 'gAMA';
  tRNS = 'tRNS';
  bKGD = 'bKGD';
  CHUNKMASK = $20;

type
  PIHDRChunk = ^TIHDRChunk;
  TIHDRChunk = packed record
    Width, Height: Cardinal;
    BitDepth, ColorType, Compression, Filter, Interlaced: Byte;
  end;


class function TPNGGraphic.CanLoad(const FileName: String): Boolean;
var
  Stream: TFileStream;
begin
  if UseACPng then begin
    Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
    try
      Result := CanLoad(Stream);
    finally
      FreeAndNil(Stream);
    end;
  end
  else
    Result := False;
end;


class function TPNGGraphic.CanLoad(Stream: TStream): Boolean;
var
  Magic: array [0..7] of Byte;
  LastPosition: Cardinal;
begin
  if UseACPng then
    with Stream do begin
      LastPosition := Position;
      Result := (Size - Position) > SizeOf(Magic);
      if Result then begin
        ReadBuffer(Magic, SizeOf(Magic));
        Result := CompareMem(@Magic, @PNGMagic, 8);
      end;
      Position := LastPosition;
    end
  else
    Result := False;
end;


{$IFDEF DELPHI_10TOKYO}
class function TPNGGraphic.CanLoadFromStream(Stream: TStream): Boolean;
begin
  Result := CanLoad(Stream);
end;
{$ENDIF}


function TPNGGraphic.IsChunk(const ChunkType: TChunkType): Boolean;
const
  Mask = not $20202020;
begin
  Result := (Cardinal((@FHeader.ChunkType)^) and Mask) = (Cardinal((@ChunkType)^) and Mask);
end;


function TPNGGraphic.LoadAndSwapHeader: Cardinal;
begin
  FStream.ReadBuffer(FHeader, SizeOf(FHeader));
  Result := CRC32(0, PByte(@FHeader.ChunkType), 4);
  FHeader.Length := SwapLong(FHeader.Length);
end;


function PaethPredictor(a, b, c: Byte): Byte;
var
  p, pa, pb, pc: Integer;
begin
  p := a + b - c;
  pa := Abs(p - a);
  pb := Abs(p - b);
  pc := Abs(p - c);
  if (pa <= pb) and (pa <= pc) then
    Result := a
  else
    if pb <= pc then
      Result := b
    else
      Result := c;
end;


procedure TPNGGraphic.ApplyFilter(Filter: Byte; Line, PrevLine, Target: PByte; BPP, BytesPerRow: Integer);
var
  I: Integer;
  Raw, Decoded, Prior, PriorDecoded, TargetRun: PByte;
begin
  case Filter of
    0:
      Move(Line^, Target^, BytesPerRow);

    1: begin
      Raw := Line;
      TargetRun := Target;
      Move(Raw^, TargetRun^, BPP);

      Decoded := TargetRun;
      Inc(Raw, BPP);
      Inc(TargetRun, BPP);
      Dec(BytesPerRow, BPP);
      while BytesPerRow > 0 do begin
        TargetRun^ := Byte(Raw^ + Decoded^);
        Inc(Raw);
        Inc(Decoded);
        Inc(TargetRun);
        Dec(BytesPerRow);
      end;
    end;

    2: begin
      Raw := Line;
      Prior := PrevLine;
      TargetRun := Target;
      while BytesPerRow > 0 do begin
        TargetRun^ := Byte(Raw^ + Prior^);
        Inc(Raw);
        Inc(Prior);
        Inc(TargetRun);
        Dec(BytesPerRow);
      end;
    end;

    3: begin
      Raw := Line;
      Decoded := Line;
      Prior := PrevLine;
      TargetRun := Target;
      for I := 0 to BPP - 1 do begin
        TargetRun^ := Byte(Raw^ + Floor(Prior^ / 2));
        Inc(Raw);
        Inc(Prior);
        Inc(TargetRun);
      end;
      Dec(BytesPerRow, BPP);
      while BytesPerRow > 0 do begin
        TargetRun^ := Byte(Raw^ + Floor((Decoded^ + Prior^) / 2));
        Inc(Raw);
        Inc(Decoded);
        Inc(Prior);
        Inc(TargetRun);
        Dec(BytesPerRow);
      end;
    end;

    4: begin
      Raw := Line;
      Decoded := Target;
      Prior := PrevLine;
      PriorDecoded := PrevLine;
      TargetRun := Target;
      for I := 0 to BPP - 1 do begin
        TargetRun^ := Byte(Raw^ + PaethPredictor(0, Prior^, 0));
        Inc(Raw);
        Inc(Prior);
        Inc(TargetRun);
      end;
      Dec(BytesPerRow, BPP);
      while BytesPerRow > 0 do begin
        TargetRun^ := Byte(Raw^ + PaethPredictor(Decoded^, Prior^, PriorDecoded^));
        Inc(Raw);
        Inc(Decoded);
        Inc(Prior);
        Inc(PriorDecoded);
        Inc(TargetRun);
        Dec(BytesPerRow);
      end;
    end;
  end;
end;


{$IFNDEF DELPHI6UP}
type
  PCardinal = ^Cardinal;
{$ENDIF}


procedure TPNGGraphic.LoadFromStream(Stream: TStream);
var
  Description: TIHDRChunk;
begin
  Handle := 0;
  FBasePosition := Stream.Position;
  FDecoder := nil;
  FStream := Stream;
  if ReadImageProperties(Stream, 0) then
    with Stream, FImageProperties do begin
      Position := FBasePosition + 8;

      FProgressRect := MkRect(Width, 1);
      Progress(Self, psStarting, 0, False, FProgressRect, gesPreparing);

      FPalette := 0;
      FTransparency := nil;
      FBackgroundColor := clWhite;
      FTransparentColor := Graphics.clNone;

      FCurrentCRC := LoadAndSwapHeader;

      FRawBuffer := nil;
      ColorManager.SourceOptions := [coNeedByteSwap];
      try
        ReadDataAndCheckCRC;
        Move(FRawBuffer^, Description, SizeOf(Description));
        SwapLong(PInteger(@Description), 2);

        if Compression = ctLZ77 then begin
          FDecoder := TLZ77Decoder.Create(Z_PARTIAL_FLUSH, False);
          FDecoder.DecodeInit;
        end
        else
          GraphicExError(gesUnsupportedFeature, ['The compression scheme is', 'PNG']);

        repeat
          FCurrentCRC := LoadAndSwapHeader;
          if IsChunk(IDAT) then begin
            Progress(Self, psEnding, 0, False, FProgressRect, '');
            LoadIDAT(Description);
          end
          else
            if IsChunk(PLTE) then begin
              if (FHeader.Length mod 3) <> 0 then
                GraphicExError(gesInvalidPalette, ['PNG']);

              ReadDataAndCheckCRC;
              if Description.ColorType = 3 then begin
                FSourceBPP := SetupColorDepth(Description.ColorType, Description.BitDepth);
                FPalette := ColorManager.CreateColorPalette([FRawBuffer], pfInterlaced8Triple, FHeader.Length div 3, False);
              end;
              Continue;
            end
            else
              if IsChunk(gAMA) then begin
                ReadDataAndCheckCRC;
                ColorManager.SetGamma(SwapLong(PCardinal(FRawBuffer)^) / 100000);
                ColorManager.TargetOptions := ColorManager.TargetOptions + [coApplyGamma];
                Include(Options, ioUseGamma);
                Continue;
              end
              else
                if IsChunk(bKGD) then begin
                  LoadBackgroundColor(Description);
                  Continue;
                end
                else
                  if IsChunk(tRNS) then begin
                    LoadTransparency(Description);
                    Continue;
                  end;

{$IFDEF DELPHI_XE4}
          Seek(NativeInt(FHeader.Length + 4), TSeekOrigin(soFromCurrent));
{$ELSE}
          Seek(FHeader.Length + 4, soFromCurrent);
{$ENDIF}
          if IsChunk(IEND) then
            Break;

          if (Byte(FHeader.ChunkType[0]) and CHUNKMASK) = 0 then
            GraphicExError(gesUnknownCriticalChunk);
        until False;
      finally
        if Assigned(FDecoder) then begin
          FDecoder.DecodeEnd;
          FreeAndNil(FDecoder);
        end;
        if Assigned(FRawBuffer) then begin
          FreeMem(FRawBuffer);
          FRawBuffer := nil;
        end;
        Progress(Self, psEnding, 0, False, FProgressRect, '');
      end;
    end
  else
    GraphicExError(gesInvalidImage, ['PNG']);
end;


function TPNGGraphic.ReadImageProperties(Stream: TStream; ImageIndex: Cardinal): Boolean;
var
  Magic: array[0..7] of Byte;
  Description: TIHDRChunk;
begin
  ZeroMemory(@FImageProperties, SizeOf(FImageProperties));
  Result := False;
  FStream := Stream;
  with Stream, FImageProperties do  begin
    ReadBuffer(Magic, 8);
    if CompareMem(@Magic, @PNGMagic, 8) then begin
      FCurrentCRC := LoadAndSwapHeader;
      if IsChunk(IHDR) then begin
        Include(Options, ioBigEndian);
        ReadDataAndCheckCRC;
        Move(FRawBuffer^, Description, SizeOf(Description));
        SwapLong(PInteger(@Description), 2);

        if (Description.Width = 0) or (Description.Height = 0) then
          Exit;

        Width  := Description.Width;
        Height := Description.Height;

        if Description.Compression = 0 then
          Compression := ctLZ77
        else
          Compression := ctUnknown;

        BitsPerSample := Description.BitDepth;
        SamplesPerPixel := 1;
        case Description.ColorType of
          0: ColorScheme := csG;

          2: begin
            ColorScheme := csRGB;
            SamplesPerPixel := 3;
          end;

          3: ColorScheme := csIndexed;

          4: ColorScheme := csGA;

          6: begin
            ColorScheme := csRGBA;
            SamplesPerPixel := 4;
          end

          else
            ColorScheme := csUnknown;
        end;

        repeat
          FCurrentCRC := LoadAndSwapHeader;
          if IsChunk(gAMA) then begin
            ReadDataAndCheckCRC;
            Break;
          end;

{$IFDEF DELPHI_XE4}
          Seek(NativeInt(FHeader.Length + 4), TSeekOrigin(soFromCurrent));
{$ELSE}
          Seek(FHeader.Length + 4, soFromCurrent);
{$ENDIF}
          if IsChunk(IEND) then
            Break;
        until False;

        FreeMem(FRawBuffer);
        Result := True;
      end;
    end;
  end;
end;


function MulDiv16(Number, Numerator, Denominator: Word): Word;
{$IFNDEF WIN64}
asm
  MUL DX
  DIV CX
{$ELSE}
begin
  Result := Number * Numerator div Denominator;
{$ENDIF}
end;


procedure TPNGGraphic.LoadBackgroundColor(const Description);
var
  Run: PWord;
  R, G, B: Byte;
begin
  ReadDataAndCheckCRC;
  with TIHDRChunk(Description) do
    case ColorType of
      0, 4:
        case BitDepth of
          2:   FBackgroundColor := MulDiv16(Swap(PWord(FRawBuffer)^), 15, 3);
          16:  FBackgroundColor := MulDiv16(Swap(PWord(FRawBuffer)^), $FF, $FFFF);
          else FBackgroundColor := Byte(Swap(PWord(FRawBuffer)^));
        end;

      2, 6: begin
        Run := FRawBuffer;
        if BitDepth = 16 then begin
          R := MulDiv16(Swap(Run^), $FF, $FFFF); Inc(Run);
          G := MulDiv16(Swap(Run^), $FF, $FFFF); Inc(Run);
          B := MulDiv16(Swap(Run^), $FF, $FFFF);
        end
        else begin
          R := Byte(Swap(Run^)); Inc(Run);
          G := Byte(Swap(Run^)); Inc(Run);
          B := Byte(Swap(Run^));
        end;
        FBackgroundColor := RGB(R, G, B);
      end

      else
        FBackgroundColor := PByte(FRawBuffer)^;
    end;
end;


procedure TPNGGraphic.LoadIDAT(const Description);
const
  RowStart:        array [0..6] of Integer = (0, 0, 4, 0, 2, 0, 1);
  ColumnStart:     array [0..6] of Integer = (0, 4, 0, 2, 0, 1, 0);
  RowIncrement:    array [0..6] of Integer = (8, 8, 8, 4, 4, 2, 2);
  ColumnIncrement: array [0..6] of Integer = (8, 8, 4, 4, 2, 2, 1);
  PassMask:        array [0..6] of Byte = ($80, $08, $88, $22, $AA, $55, $FF);
var
  Row0: Pointer;
  EvenRow: Boolean;
  RowBuffer: array[Boolean] of PAnsiChar;
  BytesPerRow, InterlaceRowBytes, Row, Pass, Delta, TargetBPP, InterlaceWidth: Integer;
begin
  Progress(Self, psStarting, 0, False, FProgressRect, gesTransfering);
  RowBuffer[False] := nil;
  RowBuffer[True] := nil;
  try
    if PixelFormat = pfDevice then
      FSourceBPP := SetupColorDepth(TIHDRChunk(Description).ColorType, TIHDRChunk(Description).BitDepth);

    if TIHDRChunk(Description).BitDepth = 16 then
      TargetBPP := FSourceBPP div 2
    else
      TargetBPP := FSourceBPP;

    if FPalette <> 0 then
      Palette := FPalette
    else
      if TargetBPP = 4then
        IgnorePalette := True;

    Width := TIHDRChunk(Description).Width;
    Height := TIHDRChunk(Description).Height;

    Canvas.Lock;
    try
      Canvas.Brush.Color := FBackgroundColor;
      Canvas.FillRect(MkRect(Width, Height));
    finally
      Canvas.Unlock;
    end;
    if FTransparentColor <> Graphics.clNone then begin
      TransparentColor := FTransparentColor;
      Transparent := True;
    end;

    BytesPerRow := TargetBPP * ((Width * TIHDRChunk(Description).BitDepth + 7) div 8) + 1;

    RowBuffer[True] := AllocMem(BytesPerRow);
    RowBuffer[False] := AllocMem(BytesPerRow);

    EvenRow := True;
    if InitLine(Self, Pointer(Row0), Delta) then
      if TIHDRChunk(Description).Interlaced = 1 then
        for Pass := 0 to 6 do begin
          if Width <= ColumnStart[Pass] then
            Continue;

          InterlaceWidth := (Width + ColumnIncrement[Pass] - 1 - ColumnStart[Pass]) div ColumnIncrement[Pass];
          InterlaceRowBytes := TargetBPP * ((InterlaceWidth * TIHDRChunk(Description).BitDepth + 7) div 8) + 1;

          Row := RowStart[Pass];
          while Row < Height do begin
            ReadRow(RowBuffer[EvenRow], InterlaceRowBytes);
            ApplyFilter(Byte(RowBuffer[EvenRow]^), Pointer(RowBuffer[EvenRow] + 1), Pointer(RowBuffer[not EvenRow] + 1), Pointer(RowBuffer[EvenRow] + 1),
                        FSourceBPP, InterlaceRowBytes - 1);

            ColorManager.ConvertRow([Pointer(RowBuffer[EvenRow] + 1)], Pointer(PAnsiChar(Row0) + Delta * Row), Width, PassMask[Pass]);
            EvenRow := not EvenRow;
            Inc(Row, RowIncrement[Pass]);

            if Pass = 6 then
              OffsetRect(FProgressRect, 0, 1);
          end;
        end
      else
        for Row := 0 to Height - 1 do begin
          ReadRow(RowBuffer[EvenRow], BytesPerRow);
          ApplyFilter(Byte(RowBuffer[EvenRow]^), Pointer(RowBuffer[EvenRow] + 1), Pointer(RowBuffer[not EvenRow] + 1), Pointer(RowBuffer[EvenRow] + 1),
                      FSourceBPP, BytesPerRow - 1);

          ColorManager.ConvertRow([Pointer(RowBuffer[EvenRow] + 1)], Pointer(PAnsiChar(Row0) + Delta * Row), Width, $FF);
          EvenRow := not EvenRow;
          OffsetRect(FProgressRect, 0, 1);
        end;

    while IsChunk(IDAT) do begin
      ReadDataAndCheckCRC;
      FCurrentCRC := LoadAndSwapHeader;
    end;
  finally
    if Assigned(RowBuffer[True]) then
      FreeMem(RowBuffer[True]);

    if Assigned(RowBuffer[False]) then
      FreeMem(RowBuffer[False]);
  end;
end;


procedure TPNGGraphic.LoadTransparency(const Description);
var
  Run: PWord;
  R, G, B: Byte;
begin
  ReadDataAndCheckCRC;
  with TIHDRChunk(Description) do
    case ColorType of
      0: begin
        case BitDepth of
          2:   R := MulDiv16(Swap(PWord(FRawBuffer)^), 15, 3);
          16:  R := MulDiv16(Swap(PWord(FRawBuffer)^), $FF, $FFFF);
          else R := Byte(Swap(PWord(FRawBuffer)^));
        end;
        FTransparentColor := RGB(R, R, R);
      end;

      2: begin
        Run := FRawBuffer;
        if BitDepth = 16 then begin
          R := MulDiv16(Swap(Run^), $FF, $FFFF); Inc(Run);
          G := MulDiv16(Swap(Run^), $FF, $FFFF); Inc(Run);
          B := MulDiv16(Swap(Run^), $FF, $FFFF);
        end
        else begin
          R := Byte(Swap(Run^)); Inc(Run);
          G := Byte(Swap(Run^)); Inc(Run);
          B := Byte(Swap(Run^));
        end;
        FTransparentColor := RGB(R, G, B);
      end;

      4, 6:

      else
        SetLength(FTransparency, 256);
        Move(FRawBuffer^,  FTransparency[0], Max(FHeader.Length, 256));
        if FHeader.Length < 256 then
          FillChar(FTransparency[FHeader.Length], 256 - FHeader.Length, $FF);
    end;
end;


procedure TPNGGraphic.ReadDataAndCheckCRC;
var
  FileCRC: Cardinal;
begin
  ReallocMem(FRawBuffer, FHeader.Length);
  FStream.ReadBuffer(FRawBuffer^, FHeader.Length);
  FStream.ReadBuffer(FileCRC, SizeOf(FileCRC));
  FileCRC := SwapLong(FileCRC);
  FCurrentCRC := CRC32(FCurrentCRC, FRawBuffer, FHeader.Length);
  if FCurrentCRC <> FileCRC then
    GraphicExError(gesInvalidCRC, ['PNG']);
end;


procedure TPNGGraphic.ReadRow(RowBuffer: Pointer; BytesPerRow: Integer);
var
  LocalBuffer: Pointer;
  PendingOutput: Integer;
begin
  LocalBuffer := RowBuffer;
  PendingOutput := BytesPerRow;
  repeat
    if FDecoder.AvailableInput = 0 then begin
      FIDATSize := 0;
      while FIDATSize = 0 do begin
        if not IsChunk(IDAT) then
          Exit;

        ReadDataAndCheckCRC;
        FCurrentSource := FRawBuffer;
        FIDATSize := FHeader.Length;
        FCurrentCRC := LoadAndSwapHeader;
      end;
    end;
    FDecoder.Decode(FCurrentSource, LocalBuffer, FIDATSize - (Integer(FCurrentSource) - Integer(FRawBuffer)), PendingOutput);
    if FDecoder.ZLibResult = Z_STREAM_END then begin
      if (FDecoder.AvailableOutput <> 0) or (FDecoder.AvailableInput <> 0) then
        GraphicExError(gesExtraCompressedData, ['PNG']);

      Break;
    end;

    if FDecoder.ZLibResult <> Z_OK then
      GraphicExError(gesCompression, ['PNG']);

    PendingOutput := BytesPerRow - (Integer(LocalBuffer) - Integer(RowBuffer));
  until PendingOutput = 0;
end;


function TPNGGraphic.SetupColorDepth(ColorType, BitDepth: Integer): Integer;
begin
  Result := 0;
  case ColorType of
    0:
      if BitDepth in [1, 2, 4, 8, 16] then
        with ColorManager do begin
          SourceColorScheme := csG;
          TargetColorScheme := csG;

          SourceSamplesPerPixel := 1;
          TargetSamplesPerPixel := 1;
          SourceBitsPerSample := BitDepth;
          case BitDepth of
            2:   TargetBitsPerSample := 4;
            16:  TargetBitsPerSample := 8;
            else TargetBitsPerSample := BitDepth;
          end;   
          PixelFormat := TargetPixelFormat;
          FPalette := CreateGrayscalePalette(False);
          Result := (BitDepth + 7) div 8;
        end
        else
          GraphicExError(gesInvalidColorFormat, ['PNG']);
    2:
      if BitDepth in [8, 16] then
        with ColorManager do begin
          SourceSamplesPerPixel := 3;
          TargetSamplesPerPixel := 3;
          SourceColorScheme := csRGB;
          TargetColorScheme := csBGR;
          SourceBitsPerSample := BitDepth;
          TargetBitsPerSample := 8;
          PixelFormat := pf24Bit;
          Result := BitDepth * 3 div 8;
        end
        else
          GraphicExError(gesInvalidColorFormat, ['PNG']);
    3:
      if BitDepth in [1, 2, 4, 8] then
        with ColorManager do begin
          SourceColorScheme := csIndexed;
          TargetColorScheme := csIndexed;
          SourceSamplesPerPixel := 1;
          TargetSamplesPerPixel := 1;
          SourceBitsPerSample := BitDepth;
          if BitDepth = 2 then
            TargetBitsPerSample := 4
          else
            TargetBitsPerSample := BitDepth;

          PixelFormat := TargetPixelFormat;
          Result := 1;
        end
        else
          GraphicExError(gesInvalidColorFormat, ['PNG']);
    4:
      if BitDepth in [8, 16] then
        with ColorManager do begin
          SourceSamplesPerPixel := 1;
          TargetSamplesPerPixel := 1;
          SourceBitsPerSample := BitDepth;
          TargetBitsPerSample := 8;
          SourceColorScheme := csGA;
          TargetColorScheme := csIndexed;
          PixelFormat := pf8Bit;
          FPalette := CreateGrayScalePalette(False);
          Result := 2 * BitDepth div 8;
        end
        else
          GraphicExError(gesInvalidColorFormat, ['PNG']);
    6:
      if BitDepth in [8, 16] then
        with ColorManager do begin
          SourceSamplesPerPixel := 4;
          TargetSamplesPerPixel := 4;
          SourceColorScheme := csRGBA;
          TargetColorScheme := csBGRA;
          SourceBitsPerSample := BitDepth;
          TargetBitsPerSample := 8;
          PixelFormat := pf32Bit;

          Result := BitDepth * 4 div 8;
        end
      else
        GraphicExError(gesInvalidColorFormat, ['PNG']);

    else
      GraphicExError(gesInvalidColorFormat, ['PNG']);
  end;
end;


constructor TPNGGraphic.Create;
begin
  inherited;
  FColorManager := TColorManager.Create;
  Reflected := False;
end;


destructor TPNGGraphic.Destroy;
begin
  FreeAndNil(FColorManager);
  inherited;
end;


procedure TPNGGraphic.Assign(Source: TPersistent);
begin
  if Source is TPNGGraphic then
    FImageProperties := TPNGGraphic(Source).FImageProperties;

  inherited;
end;


type
  EColorConversionError = class(Exception);

  PCMYK = ^TCMYK;
  TCMYK = packed record
    C, M, Y, K: Byte;
  end;

  PCMYK16 = ^TCMYK16;
  TCMYK16 = packed record
    C, M, Y, K: Word;
  end;

  PCMY = ^TCMY;
  TCMY = packed record
    C, M, Y: Byte;
  end;

  PCMY16 = ^TCMY16;
  TCMY16 = packed record
    C, M, Y: Word;
  end;
  
  PRGB = ^TRGB;
  TRGB = packed record
    R, G, B: Byte;
  end;

  PRGB16 = ^TRGB16;
  TRGB16 = packed record
    R, G, B: Word;
  end;

  PRGBA = ^TRGBA;
  TRGBA = packed record
    R, G, B, A: Byte;
  end;

  PRGBA16 = ^TRGBA16;
  TRGBA16 = packed record
    R, G, B, A: Word;
  end;

  PBGR = ^TBGR;
  TBGR = packed record
    B, G, R: Byte;
  end;

  PBGR16 = ^TBGR16;
  TBGR16 = packed record
    B, G, R: Word;
  end;

  PBGRA = ^TBGRA;
  TBGRA = packed record
    B, G, R, A: Byte;
  end;

  PBGRA16 = ^TBGRA16;
  TBGRA16 = packed record
    B, G, R, A: Word;
  end;


function ClampByte(Value: Integer): Byte;
asm
  OR EAX, EAX
  JNS @@positive
  XOR EAX, EAX
  RET

@@positive:
  CMP EAX, $FF
  JBE @@OK
  MOV EAX, $FF
@@OK:
end;


constructor TColorManager.Create;
begin
  FSourceBPS := 8;
  FTargetBPS := 8;
  FSourceSPP := 3;
  FTargetSPP := 3;
  SetGamma(1, DefaultDisplayGamma);
  FSourceScheme := csRGB;
  FTargetScheme := csBGR;
  FYCbCrCoefficients[0] := 0.299;
  FYCbCrCoefficients[1] := 0.587;
  FYCbCrCoefficients[2] := 0.114;
  FChanged := True;
end;


function TColorManager.ComponentNoConvert8(Value: Byte): Byte;
begin
  Result := Value;
end;


function TColorManager.ComponentNoConvert16(Value: Word): Word;
begin
  Result := Value;
end;


function TColorManager.ComponentGammaConvert(Value: Byte): Byte;
begin
  Result := FGammaTable[Value];
end;


function TColorManager.ComponentScaleConvert(Value: Word): Byte;
begin
  Result := MulDiv16(Value, $FF, $FFFF);
end;


function TColorManager.ComponentScaleGammaConvert(Value: Word): Byte;
begin
  Result := FGammaTable[MulDiv16(Value, $FF, $FFFF)];
end;


function TColorManager.ComponentSwapScaleGammaConvert(Value: Word): Byte;
begin
  Result := FGammaTable[MulDiv16(Swap(Value), $FF, $FFFF)];
end;


function TColorManager.ComponentSwapScaleConvert(Value: Word): Byte;
begin
  Result := MulDiv16(Swap(Value), $FF, $FFFF);
end;


function TColorManager.ComponentSwapConvert(Value: Word): Word;
begin
  Result := Swap(Value);
end;


procedure TColorManager.RowConvertBGR2BGR(const Source: array of Pointer; Target: Pointer; Count: Cardinal; Mask: Byte);
var
  SourceR16,
  SourceG16,
  SourceB16,
  SourceA16: PWord;

  SourceR8,
  SourceG8,
  SourceB8,
  SourceA8: PByte;

  TargetRun16: PBGR16;
  TargetRunA16: PBGRA16;
  TargetRun8: PBGR;
  TargetRunA8: PBGRA;
  BitRun: Byte;

  Convert8_8:       function(Value: Byte): Byte of object;
  Convert16_8:      function(Value: Word): Byte of object;
  Convert16_8Alpha: function(Value: Word): Byte of object;
  Convert16_16:     function(Value: Word): Word of object;

  SourceIncrement,
  TargetIncrement: Cardinal;
  CopyAlpha: Boolean;

begin
  BitRun := $80;
  CopyAlpha := False;
  if coAlpha in FSourceOptions then begin
    SourceIncrement := SizeOf(TRGBA);
    TargetIncrement := SizeOf(TRGB);
    if coAlpha in FTargetOptions then
      CopyAlpha := True;
  end
  else begin
    SourceIncrement := SizeOf(TRGB);
    if coAlpha in FTargetOptions then
      TargetIncrement := SizeOf(TRGBA)
    else
      TargetIncrement := SizeOf(TRGB);
  end;
  if Length(Source) > 1 then
    SourceIncrement := 1;

  case FSourceBPS of
    8: begin
      if Length(Source) = 1 then begin
        SourceB8 := Source[0];
        SourceG8 := SourceB8; Inc(SourceG8);
        SourceR8 := SourceG8; Inc(SourceR8);
        SourceA8 := SourceR8; Inc(SourceA8);
      end
      else begin
        SourceB8 := Source[0];
        SourceG8 := Source[1];
        SourceR8 := Source[2];
        if coAlpha in FSourceOptions then
          SourceA8 := Source[3]
        else
          SourceA8 := nil;
      end;

      case FTargetBPS of
        8: begin
          if coApplyGamma in FTargetOptions then
            Convert8_8 := ComponentGammaConvert
          else
            Convert8_8 := ComponentNoConvert8;

          if CopyAlpha then begin
            TargetRunA8 := Target;
            while Count > 0 do begin
              if Boolean(Mask and BitRun) then begin
                TargetRunA8.R := Convert8_8(SourceR8^);
                TargetRunA8.G := Convert8_8(SourceG8^);
                TargetRunA8.B := Convert8_8(SourceB8^);
                TargetRunA8.A := SourceA8^;

                Inc(SourceB8, SourceIncrement);
                Inc(SourceG8, SourceIncrement);
                Inc(SourceR8, SourceIncrement);
                Inc(SourceA8, SourceIncrement);
              end;
              BitRun := (BitRun shr 1) or byte(BitRun shl 7);
              Dec(Count);
              Inc(TargetRunA8);
            end;
          end
          else begin
            TargetRun8 := Target;
            while Count > 0 do begin
              if Boolean(Mask and BitRun) then begin
                TargetRun8.R := Convert8_8(SourceR8^);
                TargetRun8.G := Convert8_8(SourceG8^);
                TargetRun8.B := Convert8_8(SourceB8^);

                Inc(SourceB8, SourceIncrement);
                Inc(SourceG8, SourceIncrement);
                Inc(SourceR8, SourceIncrement);
              end;
              BitRun := (BitRun shr 1) or byte(BitRun shl 7);
              Dec(Count);
              Inc(PByte(TargetRun8), TargetIncrement);
            end;
          end;
        end;

        16: begin
          if coApplyGamma in FTargetOptions then
            Convert8_8 := ComponentGammaConvert
          else
            Convert8_8 := ComponentNoConvert8;

          if coNeedByteSwap in FSourceOptions then
            Convert16_16 := ComponentSwapConvert
          else
            Convert16_16 := ComponentNoConvert16;

          if Length(Source) = 1 then begin
            SourceB8 := Source[0];
            SourceG8 := SourceB8; Inc(SourceG8);
            SourceR8 := SourceG8; Inc(SourceR8);
            SourceA8 := SourceR8; Inc(SourceA8);
          end
          else begin
            SourceB8 := Source[0];
            SourceG8 := Source[1];
            SourceR8 := Source[2];
            if coAlpha in FSourceOptions then
              SourceA8 := Source[3]
            else
              SourceA8 := nil;
          end;

          if CopyAlpha then begin
            TargetRunA16 := Target;
            while Count > 0 do begin
              if Boolean(Mask and BitRun) then begin
                TargetRunA16.R := Convert16_16(MulDiv16(Convert8_8(SourceR8^), $FFFF, $FF));
                TargetRunA16.G := Convert16_16(MulDiv16(Convert8_8(SourceG8^), $FFFF, $FF));
                TargetRunA16.B := Convert16_16(MulDiv16(Convert8_8(SourceB8^), $FFFF, $FF));
                TargetRunA16.A := Convert16_16(MulDiv16(SourceA8^, $FFFF, $FF));

                Inc(SourceB8, SourceIncrement);
                Inc(SourceG8, SourceIncrement);
                Inc(SourceR8, SourceIncrement);
                Inc(SourceA8, SourceIncrement);
              end;
              BitRun := (BitRun shr 1) or byte(BitRun shl 7);
              Dec(Count);
              Inc(TargetRunA16);
            end;
          end
          else begin
            TargetRun16 := Target;
            while Count > 0 do begin
              if Boolean(Mask and BitRun) then begin
                TargetRun16.R := Convert16_16(MulDiv16(Convert8_8(SourceR8^), $FFFF, $FF));
                TargetRun16.G := Convert16_16(MulDiv16(Convert8_8(SourceG8^), $FFFF, $FF));
                TargetRun16.B := Convert16_16(MulDiv16(Convert8_8(SourceB8^), $FFFF, $FF));

                Inc(SourceB8, SourceIncrement);
                Inc(SourceG8, SourceIncrement);
                Inc(SourceR8, SourceIncrement);
              end;
              BitRun := (BitRun shr 1) or byte(BitRun shl 7);
              Dec(Count);
              Inc(PWord(TargetRun16), TargetIncrement);
            end;
          end;
        end;
      end;
    end;

    16: begin
      if Length(Source) = 1 then begin
        SourceB16 := Source[0];
        SourceG16 := SourceB16; Inc(SourceG16);
        SourceR16 := SourceG16; Inc(SourceR16);
        SourceA16 := SourceR16; Inc(SourceA16);
      end
      else begin
        SourceB16 := Source[0];
        SourceG16 := Source[1];
        SourceR16 := Source[2];
        if coAlpha in FSourceOptions then
          SourceA16 := Source[3]
        else
          SourceA16 := nil;
      end;

      case FTargetBPS of
        8: begin
          if coApplyGamma in FTargetOptions then
            if coNeedByteSwap in FSourceOptions then
              Convert16_8 := ComponentSwapScaleGammaConvert
            else
              Convert16_8 := ComponentScaleGammaConvert
          else
            if coNeedByteSwap in FSourceOptions then
              Convert16_8 := ComponentSwapScaleConvert
            else
              Convert16_8 := ComponentScaleConvert;

          if coNeedByteSwap in FSourceOptions then
            Convert16_8Alpha := ComponentSwapScaleConvert
          else
            Convert16_8Alpha := ComponentScaleConvert;

          if CopyAlpha then begin
            TargetRunA8 := Target;
            while Count > 0 do begin
              if Boolean(Mask and BitRun) then begin
                TargetRunA8.R := Convert16_8(SourceR16^);
                TargetRunA8.G := Convert16_8(SourceG16^);
                TargetRunA8.B := Convert16_8(SourceB16^);
                TargetRunA8.A := Convert16_8Alpha(SourceA16^);

                Inc(SourceB16, SourceIncrement);
                Inc(SourceG16, SourceIncrement);
                Inc(SourceR16, SourceIncrement);
                Inc(SourceA16, SourceIncrement);
              end;
              BitRun := (BitRun shr 1) or byte(BitRun shl 7);
              Dec(Count);
              Inc(TargetRunA8);
            end;
          end
          else begin
            TargetRun8 := Target;
            while Count > 0 do begin
              if Boolean(Mask and BitRun) then begin
                TargetRun8.R := Convert16_8(SourceR16^);
                TargetRun8.G := Convert16_8(SourceG16^);
                TargetRun8.B := Convert16_8(SourceB16^);

                Inc(SourceB16, SourceIncrement);
                Inc(SourceG16, SourceIncrement);
                Inc(SourceR16, SourceIncrement);
              end;
              BitRun := (BitRun shr 1) or byte(BitRun shl 7);
              Dec(Count);
              Inc(PByte(TargetRun8), TargetIncrement);
            end;
          end;
        end;

        16: begin
          if coNeedByteSwap in FSourceOptions then
            Convert16_16 := ComponentSwapConvert
          else
            Convert16_16 := ComponentNoConvert16;

          if Length(Source) = 1 then begin
            SourceB16 := Source[0];
            SourceG16 := SourceB16; Inc(SourceG16);
            SourceR16 := SourceG16; Inc(SourceR16);
            SourceA16 := SourceR16; Inc(SourceA16);
          end
          else begin
            SourceB16 := Source[0];
            SourceG16 := Source[1];
            SourceR16 := Source[2];
            if coAlpha in FSourceOptions then
              SourceA16 := Source[3]
            else
              SourceA16 := nil;
          end;

          if CopyAlpha then begin
            TargetRunA16 := Target;
            while Count > 0 do begin
              if Boolean(Mask and BitRun) then begin
                TargetRunA16.R := Convert16_16(SourceR16^);
                TargetRunA16.G := Convert16_16(SourceG16^);
                TargetRunA16.B := Convert16_16(SourceB16^);
                TargetRunA16.A := Convert16_16(SourceA16^);

                Inc(SourceB16, SourceIncrement);
                Inc(SourceG16, SourceIncrement);
                Inc(SourceR16, SourceIncrement);
                Inc(SourceA16, SourceIncrement);
              end;
              BitRun := (BitRun shr 1) or byte(BitRun shl 7);
              Dec(Count);
              Inc(TargetRunA16);
            end;
          end
          else begin
            TargetRun16 := Target;
            while Count > 0 do begin
              if Boolean(Mask and BitRun) then begin
                TargetRun16.R := Convert16_16(SourceR16^);
                TargetRun16.G := Convert16_16(SourceG16^);
                TargetRun16.B := Convert16_16(SourceB16^);

                Inc(SourceB16, SourceIncrement);
                Inc(SourceG16, SourceIncrement);
                Inc(SourceR16, SourceIncrement);
              end;
              BitRun := (BitRun shr 1) or byte(BitRun shl 7);
              Dec(Count);
              Inc(PWord(TargetRun16), TargetIncrement);
            end;
          end;
        end;
      end;
    end;
  end;
end;


procedure TColorManager.RowConvertBGR2RGB(const Source: array of Pointer; Target: Pointer; Count: Cardinal; Mask: Byte);
var
  SourceR16,
  SourceG16,
  SourceB16,
  SourceA16: PWord;

  SourceR8,
  SourceG8,
  SourceB8,
  SourceA8: PByte;

  TargetRun16: PRGB16;
  TargetRunA16: PRGBA16;
  TargetRun8: PRGB;
  TargetRunA8: PRGBA;
  BitRun: Byte;

  Convert8_8:       function(Value: Byte): Byte of object;
  Convert16_8:      function(Value: Word): Byte of object;
  Convert16_8Alpha: function(Value: Word): Byte of object;
  Convert16_16:     function(Value: Word): Word of object;

  SourceIncrement,
  TargetIncrement: Cardinal;
  CopyAlpha: Boolean;
begin
  BitRun := $80;
  CopyAlpha := False;
  if coAlpha in FSourceOptions then begin
    SourceIncrement := SizeOf(TRGBA);
    TargetIncrement := SizeOf(TRGB);
    if coAlpha in FTargetOptions then
      CopyAlpha := True;
  end
  else begin
    SourceIncrement := SizeOf(TRGB);
    if coAlpha in FTargetOptions then
      TargetIncrement := SizeOf(TRGBA)
    else
      TargetIncrement := SizeOf(TRGB);
  end;
  if Length(Source) > 1 then
    SourceIncrement := 1;

  case FSourceBPS of
    8: begin
      if Length(Source) = 1 then begin
        SourceB8 := Source[0];
        SourceG8 := SourceB8; Inc(SourceG8);
        SourceR8 := SourceG8; Inc(SourceR8);
        SourceA8 := SourceR8; Inc(SourceA8);
      end
      else begin
        SourceB8 := Source[0];
        SourceG8 := Source[1];
        SourceR8 := Source[2];
        if coAlpha in FSourceOptions then
          SourceA8 := Source[3]
        else
          SourceA8 := nil;
      end;

      case FTargetBPS of
        8: begin
          if coApplyGamma in FTargetOptions then
            Convert8_8 := ComponentGammaConvert
          else
            Convert8_8 := ComponentNoConvert8;

          if CopyAlpha then begin
            TargetRunA8 := Target;
            while Count > 0 do begin
              if Boolean(Mask and BitRun) then begin
                TargetRunA8.R := Convert8_8(SourceR8^);
                TargetRunA8.G := Convert8_8(SourceG8^);
                TargetRunA8.B := Convert8_8(SourceB8^);

                TargetRunA8.A := SourceA8^;

                Inc(SourceB8, SourceIncrement);
                Inc(SourceG8, SourceIncrement);
                Inc(SourceR8, SourceIncrement);
                Inc(SourceA8, SourceIncrement);
              end;
              BitRun := (BitRun shr 1) or byte(BitRun shl 7);
              Dec(Count);
              Inc(TargetRunA8);
            end;
          end
          else begin
            TargetRun8 := Target;
            while Count > 0 do begin
              if Boolean(Mask and BitRun) then begin
                TargetRun8.R := Convert8_8(SourceR8^);
                TargetRun8.G := Convert8_8(SourceG8^);
                TargetRun8.B := Convert8_8(SourceB8^);

                Inc(SourceB8, SourceIncrement);
                Inc(SourceG8, SourceIncrement);
                Inc(SourceR8, SourceIncrement);
              end;
              BitRun := (BitRun shr 1) or byte(BitRun shl 7);
              Dec(Count);
              Inc(PByte(TargetRun8), TargetIncrement);
            end;
          end;
        end;

        16: begin
          if coApplyGamma in FTargetOptions then
            Convert8_8 := ComponentGammaConvert
          else
            Convert8_8 := ComponentNoConvert8;

          if coNeedByteSwap in FSourceOptions then
            Convert16_16 := ComponentSwapConvert
          else
            Convert16_16 := ComponentNoConvert16;

          if Length(Source) = 1 then begin
            SourceB8 := Source[0];
            SourceG8 := SourceB8; Inc(SourceG8);
            SourceR8 := SourceG8; Inc(SourceR8);
            SourceA8 := SourceR8; Inc(SourceA8);
          end
          else begin
            SourceB8 := Source[0];
            SourceG8 := Source[1];
            SourceR8 := Source[2];
            if coAlpha in FSourceOptions then
              SourceA8 := Source[3]
            else
              SourceA8 := nil;
          end;

          if CopyAlpha then begin
            TargetRunA16 := Target;
            while Count > 0 do begin
              if Boolean(Mask and BitRun) then begin
                TargetRunA16.R := Convert16_16(MulDiv16(Convert8_8(SourceR8^), $FFFF, $FF));
                TargetRunA16.G := Convert16_16(MulDiv16(Convert8_8(SourceG8^), $FFFF, $FF));
                TargetRunA16.B := Convert16_16(MulDiv16(Convert8_8(SourceB8^), $FFFF, $FF));
                TargetRunA16.A := Convert16_16(MulDiv16(SourceA8^, $FFFF, $FF));

                Inc(SourceB8, SourceIncrement);
                Inc(SourceG8, SourceIncrement);
                Inc(SourceR8, SourceIncrement);
                Inc(SourceA8, SourceIncrement);
              end;
              BitRun := (BitRun shr 1) or byte(BitRun shl 7);
              Dec(Count);
              Inc(TargetRunA16);
            end;
          end
          else begin
            TargetRun16 := Target;
            while Count > 0 do begin
              if Boolean(Mask and BitRun) then begin
                TargetRun16.R := Convert16_16(MulDiv16(Convert8_8(SourceR8^), $FFFF, $FF));
                TargetRun16.G := Convert16_16(MulDiv16(Convert8_8(SourceG8^), $FFFF, $FF));
                TargetRun16.B := Convert16_16(MulDiv16(Convert8_8(SourceB8^), $FFFF, $FF));

                Inc(SourceB8, SourceIncrement);
                Inc(SourceG8, SourceIncrement);
                Inc(SourceR8, SourceIncrement);
              end;
              BitRun := (BitRun shr 1) or byte(BitRun shl 7);
              Dec(Count);
              Inc(PWord(TargetRun16), TargetIncrement);
            end;
          end;
        end;
      end;
    end;

    16: begin
      if Length(Source) = 1 then begin
        SourceB16 := Source[0];
        SourceG16 := SourceB16; Inc(SourceG16);
        SourceR16 := SourceG16; Inc(SourceR16);
        SourceA16 := SourceR16; Inc(SourceA16);
      end
      else begin
        SourceB16 := Source[0];
        SourceG16 := Source[1];
        SourceR16 := Source[2];
        if coAlpha in FSourceOptions then
          SourceA16 := Source[3]
        else
          SourceA16 := nil;
      end;

      case FTargetBPS of
        8: begin
          if coApplyGamma in FTargetOptions then
            if coNeedByteSwap in FSourceOptions then
              Convert16_8 := ComponentSwapScaleGammaConvert
            else
              Convert16_8 := ComponentScaleGammaConvert
          else
            if coNeedByteSwap in FSourceOptions then
              Convert16_8 := ComponentSwapScaleConvert
            else
              Convert16_8 := ComponentScaleConvert;

          if coNeedByteSwap in FSourceOptions then
            Convert16_8Alpha := ComponentSwapScaleConvert
          else
            Convert16_8Alpha := ComponentScaleConvert;

          if CopyAlpha then begin
            TargetRunA8 := Target;
            while Count > 0 do begin
              if Boolean(Mask and BitRun) then begin
                TargetRunA8.R := Convert16_8(SourceR16^);
                TargetRunA8.G := Convert16_8(SourceG16^);
                TargetRunA8.B := Convert16_8(SourceB16^);
                TargetRunA8.A := Convert16_8Alpha(SourceA16^);

                Inc(SourceB16, SourceIncrement);
                Inc(SourceG16, SourceIncrement);
                Inc(SourceR16, SourceIncrement);
                Inc(SourceA16, SourceIncrement);
              end;
              BitRun := (BitRun shr 1) or byte(BitRun shl 7);
              Dec(Count);
              Inc(TargetRunA8);
            end;
          end
          else begin
            TargetRun8 := Target;
            while Count > 0 do begin
              if Boolean(Mask and BitRun) then begin
                TargetRun8.R := Convert16_8(SourceR16^);
                TargetRun8.G := Convert16_8(SourceG16^);
                TargetRun8.B := Convert16_8(SourceB16^);

                Inc(SourceB16, SourceIncrement);
                Inc(SourceG16, SourceIncrement);
                Inc(SourceR16, SourceIncrement);
              end;
              BitRun := (BitRun shr 1) or byte(BitRun shl 7);
              Dec(Count);
              Inc(PByte(TargetRun8), TargetIncrement);
            end;
          end;
        end;
        16: begin
          if coNeedByteSwap in FSourceOptions then
            Convert16_16 := ComponentSwapConvert
          else
            Convert16_16 := ComponentNoConvert16;

          if Length(Source) = 1 then begin
            SourceB16 := Source[0];
            SourceG16 := SourceB16; Inc(SourceG16);
            SourceR16 := SourceG16; Inc(SourceR16);
            SourceA16 := SourceR16; Inc(SourceA16);
          end
          else begin
            SourceB16 := Source[0];
            SourceG16 := Source[1];
            SourceR16 := Source[2];
            if coAlpha in FSourceOptions then
              SourceA16 := Source[3]
            else
              SourceA16 := nil;
          end;

          if CopyAlpha then begin
            TargetRunA16 := Target;
            while Count > 0 do begin
              if Boolean(Mask and BitRun) then begin
                TargetRunA16.R := Convert16_16(SourceR16^);
                TargetRunA16.G := Convert16_16(SourceG16^);
                TargetRunA16.B := Convert16_16(SourceB16^);
                TargetRunA16.A := Convert16_16(SourceA16^);

                Inc(SourceB16, SourceIncrement);
                Inc(SourceG16, SourceIncrement);
                Inc(SourceR16, SourceIncrement);
                Inc(SourceA16, SourceIncrement);
              end;
              BitRun := (BitRun shr 1) or byte(BitRun shl 7);
              Dec(Count);
              Inc(TargetRunA16);
            end;
          end
          else begin
            TargetRun16 := Target;
            while Count > 0 do begin
              if Boolean(Mask and BitRun) then begin
                TargetRun16.R := Convert16_16(SourceR16^);
                TargetRun16.G := Convert16_16(SourceG16^);
                TargetRun16.B := Convert16_16(SourceB16^);

                Inc(SourceB16, SourceIncrement);
                Inc(SourceG16, SourceIncrement);
                Inc(SourceR16, SourceIncrement);
              end;
              BitRun := (BitRun shr 1) or byte(BitRun shl 7);
              Dec(Count);
              Inc(PWord(TargetRun16), TargetIncrement);
            end;
          end;
        end;
      end;
    end;
  end;
end;


procedure TColorManager.RowConvertCIELAB2BGR(const Source: array of Pointer; Target: Pointer; Count: Cardinal; Mask: Byte);
var
  LRun8, aRun8, bRun8: PByte;
  LRun16, aRun16, bRun16: PWord;
  L, a, b, X, Y, Z, T, YYn3: Extended;
  Target8: PByte;
  Target16: PWord;
  Increment: Integer;
  AlphaSkip: Integer;
  BitRun: Byte;
begin
  BitRun := $80;
  AlphaSkip := Ord(coAlpha in FTargetOptions);
  case FSourceBPS of
    8: begin
      if Length(Source) = 1 then begin
        LRun8 := Source[0];
        aRun8 := LRun8; Inc(aRun8);
        bRun8 := aRun8; Inc(bRun8);
        Increment := 3;
      end
      else begin
        LRun8 := Source[0];
        aRun8 := Source[1];
        bRun8 := Source[2];
        Increment := 1;
      end;
      case FTargetBPS of
        8: begin
          Target8 := Target;
          while Count > 0 do begin
            if Boolean(Mask and BitRun) then begin
              if coLabByteRange in FSourceOptions then
                L := LRun8^ / 2.55
              else
                L := LRun8^;

              Inc(LRun8, Increment);
              if coLabChromaOffset in FSourceOptions then begin
                a := aRun8^ - 128;
                Inc(aRun8, Increment);
                b := bRun8^ - 128;
                Inc(bRun8, Increment);
              end
              else begin
                a := ShortInt(aRun8^);
                Inc(aRun8, Increment);
                b := ShortInt(bRun8^);
                Inc(bRun8, Increment);
              end;

              YYn3 := (L + 16) / 116;
              if L < 7.9996 then begin
                Y := L / 903.3;
                X := a / 3893.5 + Y;
                Z := Y - b / 1557.4;
              end
              else begin
                T := YYn3 + a / 500;
                X := T * T * T;
                Y := YYn3 * YYn3 * YYn3;
                T := YYn3 - b / 200;
                Z := T * T * T;
              end;
              Target8^ := ClampByte(Round($FF * ( 0.099 * X - 0.198 * Y + 1.099 * Z)));
              Inc(Target8);
              Target8^ := ClampByte(Round($FF * (-0.952 * X + 1.893 * Y + 0.059 * Z)));
              Inc(Target8);
              Target8^ := ClampByte(Round($FF * ( 2.998 * X - 1.458 * Y - 0.541 * Z)));
              Inc(Target8, 1 + AlphaSkip);
            end
            else
              Inc(Target8, 3 + AlphaSkip);

            BitRun := (BitRun shr 1) or byte(BitRun shl 7);
            Dec(Count);
          end;
        end;

        16: begin
          Target16 := Target;
          while Count > 0 do begin
            if Boolean(Mask and BitRun) then begin
              if coLabByteRange in FSourceOptions then
                L := LRun8^ / 2.55
              else
                L := LRun8^;
                
              Inc(LRun8, Increment);
              if coLabChromaOffset in FSourceOptions then begin
                a := aRun8^ - 128;
                Inc(aRun8, Increment);
                b := bRun8^ - 128;
                Inc(bRun8, Increment);
              end
              else begin
                a := ShortInt(aRun8^);
                Inc(aRun8, Increment);
                b := ShortInt(bRun8^);
                Inc(bRun8, Increment);
              end;

              YYn3 := (L + 16) / 116;
              if L < 7.9996 then begin
                Y := L / 903.3;
                X := a / 3893.5 + Y;
                Z := Y - b / 1557.4;
              end
              else begin
                T := YYn3 + a / 500;
                X := T * T * T;
                Y := YYn3 * YYn3 * YYn3;
                T := YYn3 - b / 200;
                Z := T * T * T;
              end;

              Target16^ := MulDiv16(ClampByte(Round($FF * ( 0.099 * X - 0.198 * Y + 1.099 * Z))), $FFFF, $FF);
              Inc(Target16);
              Target16^ := MulDiv16(ClampByte(Round($FF * (-0.952 * X + 1.893 * Y + 0.059 * Z))), $FFFF, $FF);
              Inc(Target16);
              Target16^ := MulDiv16(ClampByte(Round($FF * ( 2.998 * X - 1.458 * Y - 0.541 * Z))), $FFFF, $FF);
              Inc(Target16, 1 + AlphaSkip);
            end
            else
              Inc(Target16, 3 + AlphaSkip);

            BitRun := (BitRun shr 1) or byte(BitRun shl 7);
            Dec(Count);
          end
        end;
      end;
    end;

    16: begin
      if Length(Source) = 1 then begin
        LRun16 := Source[0];
        aRun16 := LRun16; Inc(aRun16);
        bRun16 := aRun16; Inc(bRun16);
        Increment := 3;
      end
      else begin
        LRun16 := Source[0];
        aRun16 := Source[1];
        bRun16 := Source[2];
        Increment := 1;
      end;
      case FTargetBPS of
        8: begin
          Target8 := Target;
          while Count > 0 do begin
            if Boolean(Mask and BitRun) then begin
              if coLabByteRange in FSourceOptions then
                L := LRun16^ / 2.55
              else
                L := LRun16^;

              Inc(LRun16, Increment);
              if coLabChromaOffset in FSourceOptions then begin
                a := aRun16^ - 128;
                Inc(aRun16, Increment);
                b := bRun16^ - 128;
                Inc(bRun16, Increment);
              end
              else begin
                a := ShortInt(aRun16^);
                Inc(aRun16, Increment);
                b := ShortInt(bRun16^);
                Inc(bRun16, Increment);
              end;

              YYn3 := (L + 16) / 116;
              if L < 7.9996 then begin
                Y := L / 903.3;
                X := a / 3893.5 + Y;
                Z := Y - b / 1557.4;
              end
              else begin
                T := YYn3 + a / 500;
                X := T * T * T;
                Y := YYn3 * YYn3 * YYn3;
                T := YYn3 - b / 200;
                Z := T * T * T;
              end;

              Target8^ := ClampByte(Round($FF * ( 0.099 * X - 0.198 * Y + 1.099 * Z)));
              Inc(Target8);
              Target8^ := ClampByte(Round($FF * (-0.952 * X + 1.893 * Y + 0.059 * Z)));
              Inc(Target8);
              Target8^ := ClampByte(Round($FF * ( 2.998 * X - 1.458 * Y - 0.541 * Z)));
              Inc(Target8, 1 + AlphaSkip);
            end
            else
              Inc(Target8, 3 + AlphaSkip);

            BitRun := (BitRun shr 1) or byte(BitRun shl 7);
            Dec(Count);
          end;
        end;
        16: begin
          Target16 := Target;
          while Count > 0 do begin
            if Boolean(Mask and BitRun) then begin
              if coLabByteRange in FSourceOptions then
                L := LRun16^ / 2.55
              else
                L := LRun16^;

              Inc(LRun16, Increment);
              if coLabChromaOffset in FSourceOptions then begin
                a := aRun16^ - 128;
                Inc(aRun16, Increment);
                b := bRun16^ - 128;
                Inc(bRun16, Increment);
              end
              else begin
                a := ShortInt(aRun16^);
                Inc(aRun16, Increment);
                b := ShortInt(bRun16^);
                Inc(bRun16, Increment);
              end;

              YYn3 := (L + 16) / 116;
              if L < 7.9996 then begin
                Y := L / 903.3;
                X := a / 3893.5 + Y;
                Z := Y - b / 1557.4;
              end
              else begin
                T := YYn3 + a / 500;
                X := T * T * T;
                Y := YYn3 * YYn3 * YYn3;
                T := YYn3 - b / 200;
                Z := T * T * T;
              end;

              Target16^ := ClampByte(Round($FF * ( 0.099 * X - 0.198 * Y + 1.099 * Z)));
              Inc(Target16);
              Target16^ := ClampByte(Round($FF * (-0.952 * X + 1.893 * Y + 0.059 * Z)));
              Inc(Target16);
              Target16^ := ClampByte(Round($FF * ( 2.998 * X - 1.458 * Y - 0.541 * Z)));
              Inc(Target16, 1 + AlphaSkip);
            end
            else
              Inc(Target16, 3 + AlphaSkip);

            BitRun := (BitRun shr 1) or byte(BitRun shl 7);
            Dec(Count);
          end;
        end;
      end;
    end;
  end;
end;


procedure TColorManager.RowConvertCIELAB2RGB(const Source: array of Pointer; Target: Pointer; Count: Cardinal; Mask: Byte);
var
  Target8, LRun8, aRun8, bRun8: PByte;
  L, a, b, X, Y, Z, T, YYn3: Extended;
  LRun16, aRun16, bRun16: PWord;
  Increment, AlphaSkip: Integer;
  Target16: PWord;
  BitRun: Byte;
begin
  BitRun := $80;
  AlphaSkip := Ord(coAlpha in FTargetOptions);
  case FSourceBPS of
    8: begin
      if Length(Source) = 1 then begin
        LRun8 := Source[0];
        aRun8 := LRun8; Inc(aRun8);
        bRun8 := aRun8; Inc(bRun8);
        Increment := 3;
      end
      else begin
        LRun8 := Source[0];
        aRun8 := Source[1];
        bRun8 := Source[2];
        Increment := 1;
      end;

      case FTargetBPS of
        8: begin
          Target8 := Target;
          while Count > 0 do begin
            if Boolean(Mask and BitRun) then begin
              if coLabByteRange in FSourceOptions then
                L := LRun8^ / 2.55
              else
                L := LRun8^;

              Inc(LRun8, Increment);
              if coLabChromaOffset in FSourceOptions then begin
                a := aRun8^ - 128;
                Inc(aRun8, Increment);
                b := bRun8^ - 128;
                Inc(bRun8, Increment);
              end
              else begin
                a := ShortInt(aRun8^);
                Inc(aRun8, Increment);
                b := ShortInt(bRun8^);
                Inc(bRun8, Increment);
              end;

              YYn3 := (L + 16) / 116;
              if L < 7.9996 then begin
                Y := L / 903.3;
                X := a / 3893.5 + Y;
                Z := Y - b / 1557.4;
              end
              else begin
                T := YYn3 + a / 500;
                X := T * T * T;
                Y := YYn3 * YYn3 * YYn3;
                T := YYn3 - b / 200;
                Z := T * T * T;
              end;

              Target8^ := ClampByte(Round($FF * ( 2.998 * X - 1.458 * Y - 0.541 * Z)));
              Inc(Target8);
              Target8^ := ClampByte(Round($FF * (-0.952 * X + 1.893 * Y + 0.059 * Z)));
              Inc(Target8);
              Target8^ := ClampByte(Round($FF * ( 0.099 * X - 0.198 * Y + 1.099 * Z)));
              Inc(Target8, 1 + AlphaSkip);
            end
            else
              Inc(Target8, 3 + AlphaSkip);

            BitRun := (BitRun shr 1) or byte(BitRun shl 7);
            Dec(Count);
          end;
        end;

        16: begin
          Target16 := Target;
          while Count > 0 do begin
            if Boolean(Mask and BitRun) then begin
              if coLabByteRange in FSourceOptions then
                L := LRun8^ / 2.55
              else
                L := LRun8^;

              Inc(LRun8, Increment);
              if coLabChromaOffset in FSourceOptions then begin
                a := aRun8^ - 128;
                Inc(aRun8, Increment);
                b := bRun8^ - 128;
                Inc(bRun8, Increment);
              end
              else begin
                a := ShortInt(aRun8^);
                Inc(aRun8, Increment);
                b := ShortInt(bRun8^);
                Inc(bRun8, Increment);
              end;

              YYn3 := (L + 16) / 116; 
              if L < 7.9996 then begin
                Y := L / 903.3;
                X := a / 3893.5 + Y;
                Z := Y - b / 1557.4;
              end
              else begin
                T := YYn3 + a / 500;
                X := T * T * T;
                Y := YYn3 * YYn3 * YYn3;
                T := YYn3 - b / 200;
                Z := T * T * T;
              end;

              Target16^ := MulDiv16(ClampByte(Round($FF * ( 2.998 * X - 1.458 * Y - 0.541 * Z))), $FFFF, $FF);
              Inc(Target16);
              Target16^ := MulDiv16(ClampByte(Round($FF * (-0.952 * X + 1.893 * Y + 0.059 * Z))), $FFFF, $FF);
              Inc(Target16);
              Target16^ := MulDiv16(ClampByte(Round($FF * ( 0.099 * X - 0.198 * Y + 1.099 * Z))), $FFFF, $FF);
              Inc(Target16, 1 + AlphaSkip);
            end
            else
              Inc(Target16, 3 + AlphaSkip);

            BitRun := (BitRun shr 1) or byte(BitRun shl 7);
            Dec(Count);
          end
        end;
      end;
    end;

    16: begin
      if Length(Source) = 1 then begin
        LRun16 := Source[0];
        aRun16 := LRun16; Inc(aRun16);
        bRun16 := aRun16; Inc(bRun16);
        Increment := 3;
      end
      else begin
        LRun16 := Source[0];
        aRun16 := Source[1];
        bRun16 := Source[2];
        Increment := 1;
      end;

      case FTargetBPS of
        8: begin
          Target8 := Target;
          while Count > 0 do begin
            if Boolean(Mask and BitRun) then begin
              if coLabByteRange in FSourceOptions then
                L := LRun16^ / 2.55
              else
                L := LRun16^;

              Inc(LRun16, Increment);
              if coLabChromaOffset in FSourceOptions then begin
                a := aRun16^ - 128;
                Inc(aRun16, Increment);
                b := bRun16^ - 128;
                Inc(bRun16, Increment);
              end
              else begin
                a := ShortInt(aRun16^);
                Inc(aRun16, Increment);
                b := ShortInt(bRun16^);
                Inc(bRun16, Increment);
              end;

              YYn3 := (L + 16) / 116;
              if L < 7.9996 then begin
                Y := L / 903.3;
                X := a / 3893.5 + Y;
                Z := Y - b / 1557.4;
              end
              else begin
                T := YYn3 + a / 500;
                X := T * T * T;
                Y := YYn3 * YYn3 * YYn3;
                T := YYn3 - b / 200;
                Z := T * T * T;
              end;

              Target8^ := ClampByte(Round($FF * ( 2.998 * X - 1.458 * Y - 0.541 * Z)));
              Inc(Target8);
              Target8^ := ClampByte(Round($FF * (-0.952 * X + 1.893 * Y + 0.059 * Z)));
              Inc(Target8);
              Target8^ := ClampByte(Round($FF * ( 0.099 * X - 0.198 * Y + 1.099 * Z)));
              Inc(Target8, 1 + AlphaSkip);
            end
            else
              Inc(Target8, 3 + AlphaSkip);

            BitRun := (BitRun shr 1) or byte(BitRun shl 7);
            Dec(Count);
          end;
        end;

        16: begin
          Target16 := Target;
          while Count > 0 do begin
            if Boolean(Mask and BitRun) then begin
              if coLabByteRange in FSourceOptions then
                L := LRun16^ / 2.55
              else
                L := LRun16^;

              Inc(LRun16, Increment);
              if coLabChromaOffset in FSourceOptions then begin
                a := aRun16^ - 128;
                Inc(aRun16, Increment);
                b := bRun16^ - 128;
                Inc(bRun16, Increment);
              end
              else begin
                a := ShortInt(aRun16^);
                Inc(aRun16, Increment);
                b := ShortInt(bRun16^);
                Inc(bRun16, Increment);
              end;

              YYn3 := (L + 16) / 116;
              if L < 7.9996 then begin
                Y := L / 903.3;
                X := a / 3893.5 + Y;
                Z := Y - b / 1557.4;
              end
              else begin
                T := YYn3 + a / 500;
                X := T * T * T;
                Y := YYn3 * YYn3 * YYn3;
                T := YYn3 - b / 200;
                Z := T * T * T;
              end;

              Target16^ := ClampByte(Round($FF * ( 2.998 * X - 1.458 * Y - 0.541 * Z)));
              Inc(Target16);
              Target16^ := ClampByte(Round($FF * (-0.952 * X + 1.893 * Y + 0.059 * Z)));
              Inc(Target16);
              Target16^ := ClampByte(Round($FF * ( 0.099 * X - 0.198 * Y + 1.099 * Z)));
              Inc(Target16, 1 + AlphaSkip);
            end
            else
              Inc(Target16, 3 + AlphaSkip);

            BitRun := (BitRun shr 1) or byte(BitRun shl 7);
            Dec(Count);
          end;
        end;
      end;
    end;
  end;
end;


procedure TColorManager.RowConvertCMYK2BGR(const Source: array of Pointer; Target: Pointer; Count: Cardinal; Mask: Byte);
var
  C8, M8, Y8, K8: PByte;
  C16, M16, Y16, K16: PWord;
  Target8: PByte;
  Target16: PWord;
  Increment: Integer;
  AlphaSkip: Integer;
  BitRun: Byte;
begin
  BitRun := $80;
  AlphaSkip := Ord(coAlpha in FTargetOptions);
  case FSourceBPS of
    8: begin
      if Length(Source) = 4 then begin
        C8 := Source[0];
        M8 := Source[1];
        Y8 := Source[2];
        K8 := Source[3];
        Increment := 1;
      end
      else begin
        C8 := Source[0];
        M8 := C8; Inc(M8);
        Y8 := M8; Inc(Y8);
        K8 := Y8; Inc(K8);
        Increment := 4;
      end;

      case FTargetBPS of
        8: begin
          Target8 := Target;
          while Count > 0 do begin
            if Boolean(Mask and BitRun) then begin
              Target8^ := ClampByte($FF - (Y8^ - MulDiv16(Y8^, K8^, $FF) + K8^));
              Inc(Target8);
              Target8^ := ClampByte($FF - (M8^ - MulDiv16(M8^, K8^, $FF) + K8^));
              Inc(Target8);
              Target8^ := ClampByte($FF - (C8^ - MulDiv16(C8^, K8^, $FF) + K8^));
              Inc(Target8, 1 + AlphaSkip);

              Inc(C8, Increment);
              Inc(M8, Increment);
              Inc(Y8, Increment);
              Inc(K8, Increment);
            end
            else
              Inc(Target8, 3 + AlphaSkip);

            BitRun := (BitRun shr 1) or byte(BitRun shl 7);
            Dec(Count);
          end;
        end;
        16: begin
          Target16 := Target;
          while Count > 0 do begin
            if Boolean(Mask and BitRun) then begin
              Target16^ := MulDiv16(ClampByte($FF - (Y8^ - MulDiv16(Y8^, K8^, $FF) + K8^)), $FFFF, $FF);
              Inc(Target16);
              Target16^ := MulDiv16(ClampByte($FF - (M8^ - MulDiv16(M8^, K8^, $FF) + K8^)), $FFFF, $FF);
              Inc(Target16);
              Target16^ := MulDiv16(ClampByte($FF - (C8^ - MulDiv16(C8^, K8^, $FF) + K8^)), $FFFF, $FF);
              Inc(Target16, 1 + AlphaSkip);

              Inc(C8, Increment);
              Inc(M8, Increment);
              Inc(Y8, Increment);
              Inc(K8, Increment);
            end
            else
              Inc(Target16, 3 + AlphaSkip);

            BitRun := (BitRun shr 1) or byte(BitRun shl 7);
            Dec(Count);
          end;
        end;
      end;
    end;

    16: begin
      if Length(Source) = 4 then begin
        C16 := Source[0];
        M16 := Source[1];
        Y16 := Source[2];
        K16 := Source[3];
        Increment := 1;
      end
      else begin
        C16 := Source[0];
        M16 := C16; Inc(M16);
        Y16 := M16; Inc(Y16);
        K16 := Y16; Inc(K16);
        Increment := 4;
      end;

      case FTargetBPS of
        8: begin // 161616 to 888
          Target8 := Target;
          while Count > 0 do begin
            if Boolean(Mask and BitRun) then begin
              // blue
              Target8^ := ClampByte($FF - MulDiv16((Y16^ - MulDiv16(Y16^, K16^, $FFFF) + K16^), $FF, $FFFF));
              Inc(Target8);
              // green
              Target8^ := ClampByte($FF - MulDiv16((M16^ - MulDiv16(M16^, K16^, $FFFF) + K16^), $FF, $FFFF));
              Inc(Target8);
              // blue
              Target8^ := ClampByte($FF - MulDiv16((C16^ - MulDiv16(C16^, K16^, $FFFF) + K16^), $FF, $FFFF));
              Inc(Target8, 1 + AlphaSkip);

              Inc(C16, Increment);
              Inc(M16, Increment);
              Inc(Y16, Increment);
              Inc(K16, Increment);
            end
            else
              Inc(Target8, 3 + AlphaSkip);

            BitRun := (BitRun shr 1) or byte(BitRun shl 7);
            Dec(Count);
          end;
        end;

        16: begin // 161616 to 161616
          Target16 := Target;
          while Count > 0 do begin
            if Boolean(Mask and BitRun) then begin
              // blue
              Target16^ := $FFFF - (Y16^ - MulDiv16(Y16^, K16^, $FFFF) + K16^);
              Inc(Target16);
              // green
              Target16^ := $FFFF - (M16^ - MulDiv16(M16^, K16^, $FFFF) + K16^);
              Inc(Target16);
              // blue
              Target16^ := $FFFF - (C16^ - MulDiv16(C16^, K16^, $FFFF) + K16^);
              Inc(Target16, 1 + AlphaSkip);

              Inc(C16, Increment);
              Inc(M16, Increment);
              Inc(Y16, Increment);
              Inc(K16, Increment);
            end
            else
              Inc(Target16, 3 + AlphaSkip);

            BitRun := (BitRun shr 1) or byte(BitRun shl 7);
            Dec(Count);
          end;
        end;
      end;
    end;
  end;
end;


procedure TColorManager.RowConvertCMYK2RGB(const Source: array of Pointer; Target: Pointer; Count: Cardinal; Mask: Byte);
var
  C8, M8, Y8, K8: PByte;
  C16, M16, Y16, K16: PWord;
  Target8: PByte;
  Target16: PWord;
  Increment: Integer;
  AlphaSkip: Integer;
  BitRun: Byte;
begin
  BitRun := $80;
  AlphaSkip := Ord(coAlpha in FTargetOptions); // 0 if no alpha must be skipped, otherwise 1
  case FSourceBPS of
    8: begin
      if Length(Source) = 4 then begin
        // plane mode
        C8 := Source[0];
        M8 := Source[1];
        Y8 := Source[2];
        K8 := Source[3];
        Increment := 1;
      end
      else begin // interleaved mode
        C8 := Source[0];
        M8 := C8; Inc(M8);
        Y8 := M8; Inc(Y8);
        K8 := Y8; Inc(K8);
        Increment := 4;
      end;
      case FTargetBPS of
        8: begin // 888 to 888
          Target8 := Target;
          while Count > 0 do begin
            if Boolean(Mask and BitRun) then begin
              // red
              Target8^ := ClampByte($FF - (C8^ - MulDiv16(C8^, K8^, $FF) + K8^));
              Inc(Target8);
              // green
              Target8^ := ClampByte($FF - (M8^ - MulDiv16(M8^, K8^, $FF) + K8^));
              Inc(Target8);
              // blue
              Target8^ := ClampByte($FF - (Y8^ - MulDiv16(Y8^, K8^, $FF) + K8^));
              Inc(Target8, 1 + AlphaSkip);
                  
              Inc(C8, Increment);
              Inc(M8, Increment);
              Inc(Y8, Increment);
              Inc(K8, Increment);
            end
            else
              Inc(Target8, 3 + AlphaSkip);

            BitRun := (BitRun shr 1) or byte(BitRun shl 7);
            Dec(Count);
          end;
        end;
        16: begin // 888 to 161616
          Target16 := Target;
          while Count > 0 do begin
            if Boolean(Mask and BitRun) then begin
              // red
              Target16^ := MulDiv16(ClampByte($FF - (C8^ - MulDiv16(C8^, K8^, $FF) + K8^)), $FFFF, $FF);
              Inc(Target16);
              // green
              Target16^ := MulDiv16(ClampByte($FF - (M8^ - MulDiv16(M8^, K8^, $FF) + K8^)), $FFFF, $FF);
              Inc(Target16);
              // blue
              Target16^ := MulDiv16(ClampByte($FF - (Y8^ - MulDiv16(Y8^, K8^, $FF) + K8^)), $FFFF, $FF);
              Inc(Target16, 1 + AlphaSkip);

              Inc(C8, Increment);
              Inc(M8, Increment);
              Inc(Y8, Increment);
              Inc(K8, Increment);
            end
            else
              Inc(Target16, 3 + AlphaSkip);

            BitRun := (BitRun shr 1) or byte(BitRun shl 7);
            Dec(Count);
          end;
        end;
      end;
    end;

    16: begin
      if Length(Source) = 4 then begin
        // plane mode
        C16 := Source[0];
        M16 := Source[1];
        Y16 := Source[2];
        K16 := Source[3];
        Increment := 1;
      end
      else begin
        // interleaved mode
        C16 := Source[0];
        M16 := C16; Inc(M16);
        Y16 := M16; Inc(Y16);
        K16 := Y16; Inc(K16);
        Increment := 4;
      end;

      case FTargetBPS of
        8: begin // 161616 to 888
          Target8 := Target;
          while Count > 0 do begin
            if Boolean(Mask and BitRun) then begin
              // red
              Target8^ := ClampByte($FF - MulDiv16((C16^ - MulDiv16(C16^, K16^, $FFFF) + K16^), $FF, $FFFF));
              Inc(Target8);
              // green
              Target8^ := ClampByte($FF - MulDiv16((M16^ - MulDiv16(M16^, K16^, $FFFF) + K16^), $FF, $FFFF));
              Inc(Target8);
              // blue
              Target8^ := ClampByte($FF - MulDiv16((Y16^ - MulDiv16(Y16^, K16^, $FFFF) + K16^), $FF, $FFFF));
              Inc(Target8, 1 + AlphaSkip);

              Inc(C16, Increment);
              Inc(M16, Increment);
              Inc(Y16, Increment);
              Inc(K16, Increment);
            end
            else
              Inc(Target8, 3 + AlphaSkip);

            BitRun := (BitRun shr 1) or byte(BitRun shl 7);
            Dec(Count);
          end;
        end;
        16: begin // 161616 to 161616
          Target16 := Target;
          while Count > 0 do begin
            if Boolean(Mask and BitRun) then begin
              // red
              Target16^ := $FFFF - (C16^ - MulDiv16(C16^, K16^, $FFFF) + K16^);
              Inc(Target16);
              // green
              Target16^ := $FFFF - (M16^ - MulDiv16(M16^, K16^, $FFFF) + K16^);
              Inc(Target16);
              // blue
              Target16^ := $FFFF - (Y16^ - MulDiv16(Y16^, K16^, $FFFF) + K16^);
              Inc(Target16, 1 + AlphaSkip);

              Inc(C16, Increment);
              Inc(M16, Increment);
              Inc(Y16, Increment);
              Inc(K16, Increment);
            end
            else
              Inc(Target16, 3 + AlphaSkip);

            BitRun := (BitRun shr 1) or byte(BitRun shl 7);
            Dec(Count);
          end;
        end;
      end;
    end;
  end;
end;


procedure TColorManager.RowConvertGray(const Source: array of Pointer; Target: Pointer; Count: Cardinal; Mask: Byte);
var
  Target8: PByte;
  Target16: PWord;
  Source8: PByte;
  Source16: PWord;
  BitRun: Byte;
  AlphaSkip: Integer;
  Convert16: function(Value: Word): Byte of object;
begin
  BitRun := $80;
  AlphaSkip := Ord(coAlpha in FSourceOptions); // 0 if no alpha must be skipped, otherwise 1
  case FSourceBPS of
    8:
      case FTargetBPS of
        8: begin // 888 to 888
          Source8 := Source[0];
          Target8 := Target;
          while Count > 0 do begin
            if Boolean(Mask and BitRun) then begin
              Target8^ := Source8^;
              Inc(Source8, 1 + AlphaSkip);
            end;
            BitRun := (BitRun shr 1) or byte(BitRun shl 7);
            Dec(Count);
            Inc(Target8);
          end;
        end;

        16: begin // 888 to 161616
          Source8 := Source[0];
          Target16 := Target;
          while Count > 0 do begin
            if Boolean(Mask and BitRun) then begin
              Target16^ := MulDiv16(Source8^, $FFFF, $FF);
              Inc(Source8, 1 + AlphaSkip);
            end;
            BitRun := (BitRun shr 1) or byte(BitRun shl 7);
            Dec(Count);
            Inc(Target16);
          end;
        end;
      end;

    16:
      case FTargetBPS of
        8: begin // 161616 to 888
          Source16 := Source[0];
          Target8 := Target;
          if coNeedByteSwap in FSourceOptions then
            Convert16 := ComponentSwapScaleConvert
          else
            Convert16 := ComponentScaleConvert;

          while Count > 0 do begin
            if Boolean(Mask and BitRun) then begin
              Target8^ := Convert16(Source16^);
              Inc(Source16, 1 + AlphaSkip);
            end;
            BitRun := (BitRun shr 1) or byte(BitRun shl 7);
            Dec(Count);
            Inc(Target8);
          end;
        end;

        16: begin // 161616 to 161616
          Source16 := Source[0];
          Target16 := Target;

          if coNeedByteSwap in FSourceOptions then
            while Count > 0 do begin
              if Boolean(Mask and BitRun) then begin
                Target16^ := Swap(Source16^);
                Inc(Source16, 1 + AlphaSkip);
              end;
              BitRun := (BitRun shr 1) or byte(BitRun shl 7);
              Dec(Count);
              Inc(Target16);
            end
          else
            while Count > 0 do begin
              if Boolean(Mask and BitRun) then begin
                Target16^ := Source16^;
                Inc(Source16, 1 + AlphaSkip);
              end;
              BitRun := (BitRun shr 1) or byte(BitRun shl 7);
              Dec(Count);
              Inc(Target16);
            end;
        end;
      end;
  end;
end;


procedure TColorManager.RowConvertIndexed8(const Source: array of Pointer; Target: Pointer; Count: Cardinal; Mask: Byte);
var
  SourceRun, TargetRun: PByte;
  Value, BitRun, TargetMask, SourceMask,
  SourceShift, TargetShift, MaxInSample, MaxOutSample,
  SourceBPS, // local copies to ease assembler access
  TargetBPS: Byte;
  Done: Cardinal;
begin
  SourceRun := Source[0];
  TargetRun := Target;

  if (FSourceBPS = FTargetBPS) and (Mask = $FF) then
    Move(SourceRun^, TargetRun^, (Count * FSourceBPS + 7) div 8)
  else begin
    BitRun := $80;
    // make a copy of these both values from private variables to local variables
    // to ease access during assembler parts in the code
    SourceBPS := FSourceBPS;
    TargetBPS := FTargetBPS;

    // "byte(integer(" - Avoiding of incorrect shifting under WIN64
    SourceMask   := byte(not (integer(1 shl (8 - SourceBPS)) - 1));
    MaxInSample  := byte(integer(1 shl SourceBPS) - 1);
    TargetMask   := byte(integer(1 shl (8 - TargetBPS)) - 1);
    MaxOutSample := byte(integer(1 shl TargetBPS) - 1);

    SourceShift := 8;
    TargetShift := 8 - TargetBPS;
    Done := 0;
    while Done < Count do begin
      if Boolean(Mask and BitRun) then begin
        // adjust shift value by source bit depth
        Dec(SourceShift, SourceBPS);
        Value := (SourceRun^ and SourceMask) shr SourceShift;
        Value := MulDiv16(Value, MaxOutSample, MaxInSample);
        TargetRun^ := (TargetRun^ and TargetMask) or (Value shl TargetShift);
        if SourceShift = 0 then begin
          SourceShift := 8;
          Inc(SourceRun);
        end;
        SourceMask := (SourceMask shr SourceBPS) or byte(SourceMask shl (8 - SourceBPS));
      end;
      BitRun := (BitRun shr 1) or byte(BitRun shl 7);
      TargetMask := (TargetMask shr TargetBPS) or byte(TargetMask shl (8 - TargetBPS));

      if TargetShift = 0 then
        TargetShift := 8 - TargetBPS
      else
        Dec(TargetShift, TargetBPS);

      Inc(Done);
      // advance target pointer every (8 div target bit count)
      if (Done mod (8 div TargetBPS)) = 0 then
        Inc(TargetRun);
    end;
  end;
end;


procedure TColorManager.RowConvertIndexedBoth16(const Source: array of Pointer; Target: Pointer; Count: Cardinal; Mask: Byte);
var
  TargetRun,
  SourceRun: PWord;
  BitRun: Byte;
begin
  SourceRun := Source[0];
  TargetRun := Target;
  BitRun := $80;
  if coNeedByteSwap in FSourceOptions then
    while Count > 0 do begin
      if Boolean(Mask and BitRun) then begin
        TargetRun^ := Swap(SourceRun^);
        Inc(SourceRun);
      end;
      BitRun := (BitRun shr 1) or byte(BitRun shl 7);
      Dec(Count);
      Inc(TargetRun);
    end
  else
    if Mask = $FF then
      Move(SourceRun^, TargetRun^, 2 * Count)
    else
      while Count > 0 do begin
        if Boolean(Mask and BitRun) then begin
          TargetRun^ := SourceRun^;
          Inc(SourceRun);
        end;
        BitRun := (BitRun shr 1) or byte(BitRun shl 7);
        Dec(Count);
        Inc(TargetRun);
      end;
end;


procedure TColorManager.RowConvertIndexedSource16(const Source: array of Pointer; Target: Pointer; Count: Cardinal; Mask: Byte);
var
  TargetRun8: PByte;
  SourceRun16: PWord;
  Value, BitRun, TargetMask, TargetShift, MaxOutSample, TargetBPS: Byte; // local copies to ease assembler access
begin
  SourceRun16 := Source[0];
  TargetRun8 := Target;
  BitRun := $80;
  // make a copy of these both values from private variables to local variables
  // to ease access during assembler parts in the code
  TargetBPS := FTargetBPS;
  TargetMask := (1 shl (8 - TargetBPS)) - 1;
  MaxOutSample := (1 shl TargetBPS) - 1;
  TargetShift := 8 - TargetBPS;
  while Count > 0 do begin
    if Boolean(Mask and BitRun) then begin
      if coNeedByteSwap in FSourceOptions then
        Value := MulDiv16(Swap(SourceRun16^), MaxOutSample, $FFFF)
      else
        Value := MulDiv16(SourceRun16^, MaxOutSample, $FFFF);

      TargetRun8^ := (TargetRun8^ and TargetMask) or (Value shl TargetShift);
      Inc(SourceRun16);
    end;

    BitRun := (BitRun shr 1) or byte(BitRun shl 7);
    TargetMask := (TargetMask shr TargetBPS) or byte(TargetMask shl (8 - TargetBPS));

    if TargetShift = 0 then
      TargetShift := 8 - TargetBPS
    else
      Dec(TargetShift, TargetBPS);

    Dec(Count);
    // advance target pointer every (8 div target bit count)
    if (Count mod (8 div TargetBPS)) = 0 then
      Inc(TargetRun8);
  end;
end;


procedure TColorManager.RowConvertIndexedTarget16(const Source: array of Pointer; Target: Pointer; Count: Cardinal; Mask: Byte);
var
  SourceRun8: PByte;
  TargetRun16: PWord;
  Value: Word;
  BitRun,
  SourceMask,
  SourceShift,
  MaxInSample,
  SourceBPS: Byte;
begin
  SourceRun8 := Source[0];
  TargetRun16 := Target;
  BitRun := $80;
  SourceBPS := FSourceBPS;
  SourceMask := Byte(not ((1 shl (8 - SourceBPS)) - 1));
  MaxInSample := (1 shl SourceBPS) - 1;
  SourceShift := 8;
  while Count > 0 do begin
    if Boolean(Mask and BitRun) then begin
      // adjust shift value by source bit depth
      Dec(SourceShift, SourceBPS);
      Value := (SourceRun8^ and SourceMask) shr SourceShift;
      Value := MulDiv16(Value, $FFFF, MaxInSample);
      if coNeedByteSwap in FSourceOptions then
        TargetRun16^ := Swap(Value)
      else
        TargetRun16^ := Value;

      if SourceShift = 0 then begin
        SourceShift := 8;
        Inc(SourceRun8);
      end;
      SourceMask := (SourceMask shr SourceBPS) or byte(SourceMask shl (8 - SourceBPS));
    end;
    BitRun := (BitRun shr 1) or byte(BitRun shl 7);
    Dec(Count);
    // advance target pointer every (8 div target bit count)
    Inc(TargetRun16);
  end;
end;


procedure TColorManager.RowConvertRGB2BGR(const Source: array of Pointer; Target: Pointer; Count: Cardinal; Mask: Byte);
var
  SourceR16, SourceG16, SourceB16, SourceA16: PWord;
  SourceR8, SourceG8, SourceB8, SourceA8: PByte;
  TargetRun16: PBGR16;
  TargetRunA16: PBGRA16;
  TargetRun8: PBGR;
  TargetRunA8: PBGRA;
  BitRun: Byte;

  Convert8_8: function(Value: Byte): Byte of object;
  Convert16_8: function(Value: Word): Byte of object;
  Convert16_8Alpha: function(Value: Word): Byte of object;
  Convert16_16: function(Value: Word): Word of object;

  SourceIncrement,
  TargetIncrement: Cardinal;
  CopyAlpha: Boolean;
begin
  BitRun := $80;
  // determine alpha handling once
  CopyAlpha := False;
  if coAlpha in FSourceOptions then begin
    // byte size of components doesn't matter as the increments are applied to
    // pointers whose data types determine the final increment
    SourceIncrement := SizeOf(TRGBA);
    TargetIncrement := SizeOf(TRGB);
    if coAlpha in FTargetOptions then
      CopyAlpha := True;
  end
  else begin
    SourceIncrement := SizeOf(TRGB);
    if coAlpha in FTargetOptions then
      TargetIncrement := SizeOf(TRGBA)
    else
      TargetIncrement := SizeOf(TRGB);
  end;
  // in planar mode source increment is always 1
  if Length(Source) > 1 then
    SourceIncrement := 1;

  case FSourceBPS of
    8: begin
      if Length(Source) = 1 then begin
        // interleaved mode
        SourceR8 := Source[0];
        SourceG8 := SourceR8; Inc(SourceG8);
        SourceB8 := SourceG8; Inc(SourceB8);
        SourceA8 := SourceB8; Inc(SourceA8);
      end
      else begin
        SourceR8 := Source[0];
        SourceG8 := Source[1];
        SourceB8 := Source[2];
        if coAlpha in FSourceOptions then
          SourceA8 := Source[3]
        else
          SourceA8 := nil;
      end;
      case FTargetBPS of
        8: begin // 888 to 888
          if coApplyGamma in FTargetOptions then
            Convert8_8 := ComponentGammaConvert
          else
            Convert8_8 := ComponentNoConvert8;

          if CopyAlpha then begin
            TargetRunA8 := Target;
            while Count > 0 do begin
              if Boolean(Mask and BitRun) then begin
                TargetRunA8.R := Convert8_8(SourceR8^);
                TargetRunA8.G := Convert8_8(SourceG8^);
                TargetRunA8.B := Convert8_8(SourceB8^);
                // alpha values are never gamma corrected
                TargetRunA8.A := SourceA8^;
                Inc(SourceB8, SourceIncrement);
                Inc(SourceG8, SourceIncrement);
                Inc(SourceR8, SourceIncrement);
                Inc(SourceA8, SourceIncrement);
              end;
              BitRun := (BitRun shr 1) or byte(BitRun shl 7);
              Dec(Count);
              Inc(TargetRunA8);
            end;
          end
          else begin
            TargetRun8 := Target;
            while Count > 0 do begin
              if Boolean(Mask and BitRun) then begin
                TargetRun8.R := Convert8_8(SourceR8^);
                TargetRun8.G := Convert8_8(SourceG8^);
                TargetRun8.B := Convert8_8(SourceB8^);

                Inc(SourceB8, SourceIncrement);
                Inc(SourceG8, SourceIncrement);
                Inc(SourceR8, SourceIncrement);
              end;
              BitRun := (BitRun shr 1) or byte(BitRun shl 7);
              Dec(Count);
              Inc(PByte(TargetRun8), TargetIncrement);
            end;
          end;
        end;

        16: begin // 888 to 161616
          if coApplyGamma in FTargetOptions then
            Convert8_8 := ComponentGammaConvert
          else
            Convert8_8 := ComponentNoConvert8;

          if coNeedByteSwap in FSourceOptions then
            Convert16_16 := ComponentSwapConvert
          else
            Convert16_16 := ComponentNoConvert16;

          if Length(Source) = 1 then begin
            SourceB8 := Source[0];
            SourceG8 := SourceB8; Inc(SourceG8);
            SourceR8 := SourceG8; Inc(SourceR8);
            SourceA8 := SourceR8; Inc(SourceA8);
          end
          else begin
            SourceB8 := Source[0];
            SourceG8 := Source[1];
            SourceR8 := Source[2];
            if coAlpha in FSourceOptions then
              SourceA8 := Source[3]
            else
              SourceA8 := nil;
          end;
          if CopyAlpha then begin
            TargetRunA16 := Target;
            while Count > 0 do begin
              if Boolean(Mask and BitRun) then begin
                TargetRunA16.R := Convert16_16(MulDiv16(Convert8_8(SourceR8^), $FFFF, $FF));
                TargetRunA16.G := Convert16_16(MulDiv16(Convert8_8(SourceG8^), $FFFF, $FF));
                TargetRunA16.B := Convert16_16(MulDiv16(Convert8_8(SourceB8^), $FFFF, $FF));
                TargetRunA16.A := Convert16_16(MulDiv16(SourceA8^, $FFFF, $FF));

                Inc(SourceB8, SourceIncrement);
                Inc(SourceG8, SourceIncrement);
                Inc(SourceR8, SourceIncrement);
                Inc(SourceA8, SourceIncrement);
              end;
              BitRun := (BitRun shr 1) or byte(BitRun shl 7);
              Dec(Count);
              Inc(TargetRunA16);
            end;
          end
          else begin
            TargetRun16 := Target;
            while Count > 0 do begin
              if Boolean(Mask and BitRun) then begin
                TargetRun16.R := Convert16_16(MulDiv16(Convert8_8(SourceR8^), $FFFF, $FF));
                TargetRun16.G := Convert16_16(MulDiv16(Convert8_8(SourceG8^), $FFFF, $FF));
                TargetRun16.B := Convert16_16(MulDiv16(Convert8_8(SourceB8^), $FFFF, $FF));

                Inc(SourceB8, SourceIncrement);
                Inc(SourceG8, SourceIncrement);
                Inc(SourceR8, SourceIncrement);
              end;
              BitRun := (BitRun shr 1) or byte(BitRun shl 7);
              Dec(Count);
              Inc(PWord(TargetRun16), TargetIncrement);
            end;
          end;
        end;
      end;
    end;

    16: begin
      if Length(Source) = 1 then begin
        SourceR16 := Source[0];
        SourceG16 := SourceR16; Inc(SourceG16);
        SourceB16 := SourceG16; Inc(SourceB16);
        SourceA16 := SourceB16; Inc(SourceA16);
      end
      else begin
        SourceR16 := Source[0];
        SourceG16 := Source[1];
        SourceB16 := Source[2];
        if coAlpha in FSourceOptions then
          SourceA16 := Source[3]
        else
          SourceA16 := nil;
      end;
      case FTargetBPS of
        8: begin // 161616 to 888
          if coApplyGamma in FTargetOptions then
            if coNeedByteSwap in FSourceOptions then
              Convert16_8 := ComponentSwapScaleGammaConvert
            else
              Convert16_8 := ComponentScaleGammaConvert
          else
            if coNeedByteSwap in FSourceOptions then
              Convert16_8 := ComponentSwapScaleConvert
            else
              Convert16_8 := ComponentScaleConvert;
          // since alpha channels are never gamma corrected we need a separate conversion routine
          if coNeedByteSwap in FSourceOptions then
            Convert16_8Alpha := ComponentSwapScaleConvert
          else
            Convert16_8Alpha := ComponentScaleConvert;

          if CopyAlpha then begin
            TargetRunA8 := Target;
            while Count > 0 do begin
              if Boolean(Mask and BitRun) then begin
                TargetRunA8.R := Convert16_8(SourceR16^);
                TargetRunA8.G := Convert16_8(SourceG16^);
                TargetRunA8.B := Convert16_8(SourceB16^);
                TargetRunA8.A := Convert16_8Alpha(SourceA16^);

                Inc(SourceB16, SourceIncrement);
                Inc(SourceG16, SourceIncrement);
                Inc(SourceR16, SourceIncrement);
                Inc(SourceA16, SourceIncrement);
              end;
              BitRun := (BitRun shr 1) or byte(BitRun shl 7);
              Dec(Count);
              Inc(TargetRunA8);
            end;
          end
          else begin
            TargetRun8 := Target;
            while Count > 0 do begin
              if Boolean(Mask and BitRun) then begin
                TargetRun8.R := Convert16_8(SourceR16^);
                TargetRun8.G := Convert16_8(SourceG16^);
                TargetRun8.B := Convert16_8(SourceB16^);

                Inc(SourceB16, SourceIncrement);
                Inc(SourceG16, SourceIncrement);
                Inc(SourceR16, SourceIncrement);
              end;
              BitRun := (BitRun shr 1) or byte(BitRun shl 7);
              Dec(Count);
              Inc(PByte(TargetRun8), TargetIncrement);
            end;
          end;
        end;

        16: begin // 161616 to 161616
          // no gamma correction for 16 bit samples yet
          if coNeedByteSwap in FSourceOptions then
            Convert16_16 := ComponentSwapConvert
          else
            Convert16_16 := ComponentNoConvert16;

          if Length(Source) = 1 then begin
            SourceB16 := Source[0];
            SourceG16 := SourceB16; Inc(SourceG16);
            SourceR16 := SourceG16; Inc(SourceR16);
            SourceA16 := SourceR16; Inc(SourceA16);
          end
          else begin
            SourceB16 := Source[0];
            SourceG16 := Source[1];
            SourceR16 := Source[2];
            if coAlpha in FSourceOptions then
              SourceA16 := Source[3]
            else
              SourceA16 := nil;
          end;
          if CopyAlpha then begin
            TargetRunA16 := Target;
            while Count > 0 do begin
              if Boolean(Mask and BitRun) then begin
                TargetRunA16.R := Convert16_16(SourceR16^);
                TargetRunA16.G := Convert16_16(SourceG16^);
                TargetRunA16.B := Convert16_16(SourceB16^);
                TargetRunA16.A := Convert16_16(SourceA16^);

                Inc(SourceB16, SourceIncrement);
                Inc(SourceG16, SourceIncrement);
                Inc(SourceR16, SourceIncrement);
                Inc(SourceA16, SourceIncrement);
              end;
              BitRun := (BitRun shr 1) or byte(BitRun shl 7);
              Dec(Count);
              Inc(TargetRunA16);
            end;
          end
          else begin
            TargetRun16 := Target;
            while Count > 0 do begin
              if Boolean(Mask and BitRun) then begin
                TargetRun16.R := Convert16_16(SourceR16^);
                TargetRun16.G := Convert16_16(SourceG16^);
                TargetRun16.B := Convert16_16(SourceB16^);

                Inc(SourceB16, SourceIncrement);
                Inc(SourceG16, SourceIncrement);
                Inc(SourceR16, SourceIncrement);
              end;
              BitRun := (BitRun shr 1) or byte(BitRun shl 7);
              Dec(Count);
              Inc(PWord(TargetRun16), TargetIncrement);
            end;
          end;
        end;
      end;
    end;
  end;
end;


procedure TColorManager.RowConvertRGB2RGB(const Source: array of Pointer; Target: Pointer; Count: Cardinal; Mask: Byte);
var
  SourceR16, SourceG16, SourceB16, SourceA16: PWord;
  SourceR8, SourceG8, SourceB8, SourceA8: PByte;
  TargetRun16: PRGB16;
  TargetRunA16: PRGBA16;
  TargetRun8: PRGB;
  TargetRunA8: PRGBA;
  BitRun: Byte;

  Convert8_8: function(Value: Byte): Byte of object;
  Convert16_8: function(Value: Word): Byte of object;
  Convert16_8Alpha: function(Value: Word): Byte of object;
  Convert16_16: function(Value: Word): Word of object;

  SourceIncrement, TargetIncrement: Cardinal;
  CopyAlpha: Boolean;
begin
  BitRun := $80;
  // determine alpha handling once
  CopyAlpha := False;
  if coAlpha in FSourceOptions then begin
    SourceIncrement := SizeOf(TRGBA);
    TargetIncrement := SizeOf(TRGB);
    if coAlpha in FTargetOptions then
      CopyAlpha := True;
  end
  else begin
    SourceIncrement := SizeOf(TRGB);
    if coAlpha in FTargetOptions then
      TargetIncrement := SizeOf(TRGBA)
    else
      TargetIncrement := SizeOf(TRGB);
  end;
  // in planar mode source increment is always 1
  if Length(Source) > 1 then
    SourceIncrement := 1;

  case FSourceBPS of
    8: begin
      if Length(Source) = 1 then begin
        // interleaved mode
        SourceR8 := Source[0];
        SourceG8 := SourceR8; Inc(SourceG8);
        SourceB8 := SourceG8; Inc(SourceB8);
        SourceA8 := SourceB8; Inc(SourceA8);
      end
      else begin
        SourceR8 := Source[0];
        SourceG8 := Source[1];
        SourceB8 := Source[2];
        if coAlpha in FSourceOptions then
          SourceA8 := Source[3]
        else
          SourceA8 := nil;
      end;

      case FTargetBPS of
        8: begin // 888 to 888
          if coApplyGamma in FTargetOptions then
            Convert8_8 := ComponentGammaConvert
          else
            Convert8_8 := ComponentNoConvert8;

          if CopyAlpha then begin
            TargetRunA8 := Target;
            while Count > 0 do begin
              if Boolean(Mask and BitRun) then begin
                TargetRunA8.R := Convert8_8(SourceR8^);
                TargetRunA8.G := Convert8_8(SourceG8^);
                TargetRunA8.B := Convert8_8(SourceB8^);
                // alpha values are never gamma corrected
                TargetRunA8.A := SourceA8^;

                Inc(SourceB8, SourceIncrement);
                Inc(SourceG8, SourceIncrement);
                Inc(SourceR8, SourceIncrement);
                Inc(SourceA8, SourceIncrement);
              end;
              BitRun := (BitRun shr 1) or byte(BitRun shl 7);
              Dec(Count);
              Inc(TargetRunA8);
            end;
          end
          else begin
            TargetRun8 := Target;
            while Count > 0 do begin
              if Boolean(Mask and BitRun) then begin
                TargetRun8.R := Convert8_8(SourceR8^);
                TargetRun8.G := Convert8_8(SourceG8^);
                TargetRun8.B := Convert8_8(SourceB8^);

                Inc(SourceB8, SourceIncrement);
                Inc(SourceG8, SourceIncrement);
                Inc(SourceR8, SourceIncrement);
              end;
              BitRun := (BitRun shr 1) or byte(BitRun shl 7);
              Dec(Count);
              Inc(PByte(TargetRun8), TargetIncrement);
            end;
          end;
        end;

        16: begin // 888 to 161616
          if coApplyGamma in FTargetOptions then
            Convert8_8 := ComponentGammaConvert
          else
            Convert8_8 := ComponentNoConvert8;

          if coNeedByteSwap in FSourceOptions then
            Convert16_16 := ComponentSwapConvert
          else
            Convert16_16 := ComponentNoConvert16;

          if Length(Source) = 1 then begin
            SourceB8 := Source[0];
            SourceG8 := SourceB8; Inc(SourceG8);
            SourceR8 := SourceG8; Inc(SourceR8);
            SourceA8 := SourceR8; Inc(SourceA8);
          end
          else begin
            SourceB8 := Source[0];
            SourceG8 := Source[1];
            SourceR8 := Source[2];
            if coAlpha in FSourceOptions then
              SourceA8 := Source[3]
            else
              SourceA8 := nil;
          end;
          if CopyAlpha then begin
            TargetRunA16 := Target;
            while Count > 0 do begin
              if Boolean(Mask and BitRun) then begin
                TargetRunA16.R := Convert16_16(MulDiv16(Convert8_8(SourceR8^), $FFFF, $FF));
                TargetRunA16.G := Convert16_16(MulDiv16(Convert8_8(SourceG8^), $FFFF, $FF));
                TargetRunA16.B := Convert16_16(MulDiv16(Convert8_8(SourceB8^), $FFFF, $FF));
                TargetRunA16.A := Convert16_16(MulDiv16(SourceA8^, $FFFF, $FF));

                Inc(SourceB8, SourceIncrement);
                Inc(SourceG8, SourceIncrement);
                Inc(SourceR8, SourceIncrement);
                Inc(SourceA8, SourceIncrement);
              end;
              BitRun := (BitRun shr 1) or byte(BitRun shl 7);
              Dec(Count);
              Inc(TargetRunA16);
            end;
          end
          else begin
            TargetRun16 := Target;
            while Count > 0 do begin
              if Boolean(Mask and BitRun) then begin
                TargetRun16.R := Convert16_16(MulDiv16(Convert8_8(SourceR8^), $FFFF, $FF));
                TargetRun16.G := Convert16_16(MulDiv16(Convert8_8(SourceG8^), $FFFF, $FF));
                TargetRun16.B := Convert16_16(MulDiv16(Convert8_8(SourceB8^), $FFFF, $FF));

                Inc(SourceB8, SourceIncrement);
                Inc(SourceG8, SourceIncrement);
                Inc(SourceR8, SourceIncrement);
              end;
              BitRun := (BitRun shr 1) or byte(BitRun shl 7);
              Dec(Count);
              Inc(PWord(TargetRun16), TargetIncrement);
            end;
          end;
        end;
      end;
    end;

    16: begin
      if Length(Source) = 1 then begin
        SourceR16 := Source[0];
        SourceG16 := SourceR16; Inc(SourceG16);
        SourceB16 := SourceG16; Inc(SourceB16);
        SourceA16 := SourceB16; Inc(SourceA16);
      end
      else begin
        SourceR16 := Source[0];
        SourceG16 := Source[1];
        SourceB16 := Source[2];
        if coAlpha in FSourceOptions then
          SourceA16 := Source[3]
        else
          SourceA16 := nil;
      end;
      case FTargetBPS of
        8: begin // 161616 to 888
          if coApplyGamma in FTargetOptions then
            if coNeedByteSwap in FSourceOptions then
              Convert16_8 := ComponentSwapScaleGammaConvert
            else
              Convert16_8 := ComponentScaleGammaConvert
          else
            if coNeedByteSwap in FSourceOptions then
              Convert16_8 := ComponentSwapScaleConvert
            else
              Convert16_8 := ComponentScaleConvert;
          // since alpha channels are never gamma corrected we need a separate conversion routine
          if coNeedByteSwap in FSourceOptions then
            Convert16_8Alpha := ComponentSwapScaleConvert
          else
            Convert16_8Alpha := ComponentScaleConvert;

          if CopyAlpha then begin
            TargetRunA8 := Target;
            while Count > 0 do begin
              if Boolean(Mask and BitRun) then begin
                TargetRunA8.R := Convert16_8(SourceR16^);
                TargetRunA8.G := Convert16_8(SourceG16^);
                TargetRunA8.B := Convert16_8(SourceB16^);
                TargetRunA8.A := Convert16_8Alpha(SourceA16^);

                Inc(SourceB16, SourceIncrement);
                Inc(SourceG16, SourceIncrement);
                Inc(SourceR16, SourceIncrement);
                Inc(SourceA16, SourceIncrement);
              end;
              BitRun := (BitRun shr 1) or byte(BitRun shl 7);
              Dec(Count);
              Inc(TargetRunA8);
            end;
          end
          else begin
            TargetRun8 := Target;
            while Count > 0 do begin
              if Boolean(Mask and BitRun) then begin
                TargetRun8.R := Convert16_8(SourceR16^);
                TargetRun8.G := Convert16_8(SourceG16^);
                TargetRun8.B := Convert16_8(SourceB16^);

                Inc(SourceB16, SourceIncrement);
                Inc(SourceG16, SourceIncrement);
                Inc(SourceR16, SourceIncrement);
              end;
              BitRun := (BitRun shr 1) or byte(BitRun shl 7);
              Dec(Count);
              Inc(PByte(TargetRun8), TargetIncrement);
            end;
          end;
        end;

        16: begin // 161616 to 161616
          // no gamma correction for 16 bit samples yet
          if coNeedByteSwap in FSourceOptions then
            Convert16_16 := ComponentSwapConvert
          else
            Convert16_16 := ComponentNoConvert16;

          if Length(Source) = 1 then begin
            SourceB16 := Source[0];
            SourceG16 := SourceB16; Inc(SourceG16);
            SourceR16 := SourceG16; Inc(SourceR16);
            SourceA16 := SourceR16; Inc(SourceA16);
          end
          else begin
            SourceB16 := Source[0];
            SourceG16 := Source[1];
            SourceR16 := Source[2];
            if coAlpha in FSourceOptions then
              SourceA16 := Source[3]
            else
              SourceA16 := nil;
          end;

          if CopyAlpha then begin
            TargetRunA16 := Target;
            while Count > 0 do begin
              if Boolean(Mask and BitRun) then begin
                TargetRunA16.R := Convert16_16(SourceR16^);
                TargetRunA16.G := Convert16_16(SourceG16^);
                TargetRunA16.B := Convert16_16(SourceB16^);
                TargetRunA16.A := Convert16_16(SourceA16^);

                Inc(SourceB16, SourceIncrement);
                Inc(SourceG16, SourceIncrement);
                Inc(SourceR16, SourceIncrement);
                Inc(SourceA16, SourceIncrement);
              end;
              BitRun := (BitRun shr 1) or byte(BitRun shl 7);
              Dec(Count);
              Inc(TargetRunA16);
            end;
          end
          else begin
            TargetRun16 := Target;
            while Count > 0 do begin
              if Boolean(Mask and BitRun) then begin
                TargetRun16.R := Convert16_16(SourceR16^);
                TargetRun16.G := Convert16_16(SourceG16^);
                TargetRun16.B := Convert16_16(SourceB16^);

                Inc(SourceB16, SourceIncrement);
                Inc(SourceG16, SourceIncrement);
                Inc(SourceR16, SourceIncrement);
              end;
              BitRun := (BitRun shr 1) or byte(BitRun shl 7);
              Dec(Count);
              Inc(PWord(TargetRun16), TargetIncrement);
            end;
          end;
        end;
      end;
    end;
  end;
end;


procedure TColorManager.RowConvertPhotoYCC2BGR(const Source: array of Pointer; Target: Pointer; Count: Cardinal; Mask: Byte);
var
  Y, Cb, Cr: Integer;
  Yf, Cbf, Crf: Single;
  Y8Run, Cb8Run, Cr8Run: PByte;
  Y16Run, Cb16Run, Cr16Run: PWord;
  Target8: PByte;
  Target16: PWord;
  AlphaSkip: Integer;
  BitRun: Byte;
  Increment: Integer;
begin
  BitRun := $80;
  AlphaSkip := Ord(coAlpha in FTargetOptions); // 0 if no alpha must be skipped, otherwise 1
  case FSourceBPS of
    8: begin
      if Length(Source) = 1 then begin
        Y8Run := Source[0];
        Cb8Run := Y8Run; Inc(Cb8Run);
        Cr8Run := Cb8Run; Inc(Cr8Run);
        Increment := 3;
      end
      else begin
        Y8Run := Source[0];
        Cb8Run := Source[1];
        Cr8Run := Source[2];
        Increment := 1;
      end;
      case FTargetBPS of
        8: begin // 888 to 888
          Target8 := Target;
          while Count > 0 do begin
            if Boolean(Mask and BitRun) then begin
              Y := Y8Run^;
              Inc(Y8Run, Increment);
              Cb := Cb8Run^;
              Inc(Cb8Run, Increment);
              Cr := Cr8Run^;
              Inc(Cr8Run, Increment);

              // blue
              Target8^ := ClampByte(Y + FCbToBlueTable[Cb]);
              Inc(Target8);
              // green
              Target8^ := ClampByte(Y + FCbToGreenTable[Cb] + FCrToGreentable[Cr]);
              Inc(Target8);
              // red
              Target8^ := ClampByte(Y + FCrToRedTable[Cr]);
              Inc(Target8, 1 + AlphaSkip);
            end
            else
              Inc(Target8, 3 + AlphaSkip);

            BitRun := (BitRun shr 1) or byte(BitRun shl 7);
            Dec(Count);
          end;
        end;

        16: begin // 888 to 161616
          Target16 := Target;
          while Count > 0 do begin
            if Boolean(Mask and BitRun) then begin
              Y := Y8Run^;
              Inc(Y8Run, Increment);
              Cb := Cb8Run^;
              Inc(Cb8Run, Increment);
              Cr := Cr8Run^;
              Inc(Cr8Run, Increment);

              // blue
              Target16^ := MulDiv16(ClampByte(Y + FCbToBlueTable[Cb]), $FFFF, $FF);
              Inc(Target16);
              // green
              Target16^ := MulDiv16(ClampByte(Y + FCbToGreenTable[Cb] + FCrToGreentable[Cr]), $FFFF, $FF);
              Inc(Target16);
              // red
              Target16^ := MulDiv16(ClampByte(Y + FCrToRedTable[Cr]), $FFFF, $FF);
              Inc(Target16, 1 + AlphaSkip);
            end
            else
              Inc(Target16, 3 + AlphaSkip);

            BitRun := (BitRun shr 1) or byte(BitRun shl 7);
            Dec(Count);
          end;
        end;
      end;
    end;

    16: begin
      if Length(Source) = 1 then begin
        Y16Run := Source[0];
        Cb16Run := Y16Run; Inc(Cb16Run);
        Cr16Run := Cb16Run; Inc(Cr16Run);
        Increment := 3;
      end
      else begin
        Y16Run := Source[0];
        Cb16Run := Source[1];
        Cr16Run := Source[2];
        Increment := 1;
      end;
      case FTargetBPS of
        8: begin // 161616 to 888
          Target8 := Target;
          while Count > 0 do begin
            if Boolean(Mask and BitRun) then begin
              Y := MulDiv16(Y16Run^, $FF, $FFFF);
              Inc(Y16Run, Increment);
              Cb := MulDiv16(Cb16Run^, $FF, $FFFF);
              Inc(Cb16Run, Increment);
              Cr := MulDiv16(Cr16Run^, $FF, $FFFF);
              Inc(Cr16Run, Increment);
              // blue
              Target8^ := ClampByte(Y + FCbToBlueTable[Cb]);
              Inc(Target8);
              // green
              Target8^ := ClampByte(Y + FCbToGreenTable[Cb] + FCrToGreentable[Cr]);
              Inc(Target8);
              // red
              Target8^ := ClampByte(Y + FCrToRedTable[Cr]);
              Inc(Target8, 1 + AlphaSkip);
            end
            else
              Inc(Target8, 3 + AlphaSkip);

            BitRun := (BitRun shr 1) or byte(BitRun shl 7);
            Dec(Count);
          end;
        end;

        16: begin // 161616 to 161616
          Target16 := Target;
          // conversion from 16 to 16 is done with full precision, so there is no
          // loss of information, but the code is slower because the lookup tables
          // cannot be used
          while Count > 0 do begin
            if Boolean(Mask and BitRun) then begin
              Yf := 1.3584 * Y16Run^;
              Inc(Y16Run, Increment);
              Cbf := Cb16Run^ - 40092; // (156 * $FFFF) div $FF
              Inc(Cb16Run, Increment);
              Crf := Cr16Run^ - 35209; // (137 * $FFFF) div $FF
              Inc(Cr16Run, Increment);       
              // blue
              Target16^ := Round(Yf + 2.2179 * Cbf);
              Inc(Target16);
              // green
              Target16^ := Round(Yf - 0.9271435 * Crf - 0.4302726 * Cbf);
              Inc(Target16);
              // red
              Target16^ := Round(Yf + 1.8215 * Crf);
              Inc(Target16, 1 + AlphaSkip);
            end
            else
              Inc(Target16, 3 + AlphaSkip);

            BitRun := (BitRun shr 1) or byte(BitRun shl 7);
            Dec(Count);
          end;
        end;
      end;
    end;
  end;
end;


procedure TColorManager.RowConvertPhotoYCC2RGB(const Source: array of Pointer; Target: Pointer; Count: Cardinal; Mask: Byte);
var
  Y, Cb, Cr: Integer;
  Yf, Cbf, Crf: Single;
  Y8Run, Cb8Run, Cr8Run: PByte;
  Y16Run, Cb16Run, Cr16Run: PWord;
  Target8: PByte;
  Target16: PWord;
  AlphaSkip: Integer;
  BitRun: Byte;
  Increment: Integer;
begin
  BitRun := $80;
  AlphaSkip := Ord(coAlpha in FTargetOptions); // 0 if no alpha must be skipped, otherwise 1
  case FSourceBPS of
    8: begin
      if Length(Source) = 1 then begin
        Y8Run := Source[0];
        Cb8Run := Y8Run; Inc(Cb8Run);
        Cr8Run := Cb8Run; Inc(Cr8Run);
        Increment := 3;
      end
      else begin
        Y8Run := Source[0];
        Cb8Run := Source[1];
        Cr8Run := Source[2];
        Increment := 1;
      end;
      case FTargetBPS of
        8: begin // 888 to 888
          Target8 := Target;
          while Count > 0 do begin
            if Boolean(Mask and BitRun) then begin
              Y := Y8Run^;
              Inc(Y8Run, Increment);
              Cb := Cb8Run^;
              Inc(Cb8Run, Increment);
              Cr := Cr8Run^;
              Inc(Cr8Run, Increment);        
              // red
              Target8^ := ClampByte(Y + FCrToRedTable[Cr]);
              Inc(Target8, 1 + AlphaSkip);
              // green
              Target8^ := ClampByte(Y + FCbToGreenTable[Cb] + FCrToGreentable[Cr]);
              Inc(Target8);
              // blue
              Target8^ := ClampByte(Y + FCbToBlueTable[Cb]);
              Inc(Target8);
            end
            else
              Inc(Target8, 3 + AlphaSkip);

            BitRun := (BitRun shr 1) or byte(BitRun shl 7);
            Dec(Count);
          end;
        end;

        16: begin // 888 to 161616
          Target16 := Target;
          while Count > 0 do begin
            if Boolean(Mask and BitRun) then begin
              Y := Y8Run^;
              Inc(Y8Run, Increment);
              Cb := Cb8Run^;
              Inc(Cb8Run, Increment);
              Cr := Cr8Run^;
              Inc(Cr8Run, Increment);       
              // red
              Target16^ := MulDiv16(ClampByte(Y + FCrToRedTable[Cr]), $FFFF, $FF);
              Inc(Target16, 1 + AlphaSkip);
              // green
              Target16^ := MulDiv16(ClampByte(Y + FCbToGreenTable[Cb] + FCrToGreentable[Cr]), $FFFF, $FF);
              Inc(Target16);
              // blue
              Target16^ := MulDiv16(ClampByte(Y + FCbToBlueTable[Cb]), $FFFF, $FF);
              Inc(Target16);
            end
            else
              Inc(Target16, 3 + AlphaSkip);

            BitRun := (BitRun shr 1) or byte(BitRun shl 7);
            Dec(Count);
          end;
        end;
      end;
    end;

    16: begin
      if Length(Source) = 1 then begin
        Y16Run := Source[0];
        Cb16Run := Y16Run; Inc(Cb16Run);
        Cr16Run := Cb16Run; Inc(Cr16Run);
        Increment := 3;
      end
      else begin
        Y16Run := Source[0];
        Cb16Run := Source[1];
        Cr16Run := Source[2];
        Increment := 1;
      end;
      case FTargetBPS of
        8: begin // 161616 to 888
          Target8 := Target;
          while Count > 0 do begin
            if Boolean(Mask and BitRun) then begin
              Y := MulDiv16(Y16Run^, $FF, $FFFF);
              Inc(Y16Run, Increment);
              Cb := MulDiv16(Cb16Run^, $FF, $FFFF);
              Inc(Cb16Run, Increment);
              Cr := MulDiv16(Cr16Run^, $FF, $FFFF);
              Inc(Cr16Run, Increment);
              // red
              Target8^ := ClampByte(Y + FCrToRedTable[Cr]);
              Inc(Target8, 1 + AlphaSkip);
              // green
              Target8^ := ClampByte(Y + FCbToGreenTable[Cb] + FCrToGreentable[Cr]);
              Inc(Target8);
              // blue
              Target8^ := ClampByte(Y + FCbToBlueTable[Cb]);
              Inc(Target8);
            end
            else
              Inc(Target8, 3 + AlphaSkip);

            BitRun := (BitRun shr 1) or byte(BitRun shl 7);
            Dec(Count);
          end;
        end;

        16: begin // 161616 to 161616
          Target16 := Target;
          // conversion from 16 to 16 is done with full precision, so there is no
          // loss of information, but the code is slower because the lookup tables
          // cannot be used
          while Count > 0 do begin
            if Boolean(Mask and BitRun) then begin
              Yf := 1.3584 * Y16Run^;
              Inc(Y16Run, Increment);
              Cbf := Cb16Run^ - 40092; // (156 * $FFFF) div $FF
              Inc(Cb16Run, Increment);
              Crf := Cr16Run^ - 35209; // (137 * $FFFF) div $FF
              Inc(Cr16Run, Increment);
              // red
              Target16^ := Round(Yf + 1.8215 * Crf);
              Inc(Target16, 1 + AlphaSkip);
              // green
              Target16^ := Round(Yf - 0.9271435 * Crf - 0.4302726 * Cbf);
              Inc(Target16);
              // blue
              Target16^ := Round(Yf + 2.2179 * Cbf);
              Inc(Target16);
            end
            else
              Inc(Target16, 3 + AlphaSkip);

            BitRun := (BitRun shr 1) or byte(BitRun shl 7);
            Dec(Count);
          end;
        end;
      end;
    end;
  end;
end;


procedure TColorManager.RowConvertYCbCr2BGR(const Source: array of Pointer; Target: Pointer; Count: Cardinal; Mask: Byte);
var
  Y, Cb, Cr: Integer;
  Yf, Cbf, Crf: Single;
  Y8Run, Cb8Run, Cr8Run: PByte;
  Y16Run, Cb16Run, Cr16Run: PWord;
  Target8: PByte;
  Target16: PWord;
  AlphaSkip: Integer;
  BitRun: Byte;
  Increment: Integer;
begin
  BitRun := $80;
  AlphaSkip := Ord(coAlpha in FTargetOptions); // 0 if no alpha must be skipped, otherwise 1
  case FSourceBPS of
    8: begin
      if Length(Source) = 1 then begin
        Y8Run := Source[0];
        Cb8Run := Y8Run; Inc(Cb8Run);
        Cr8Run := Cb8Run; Inc(Cr8Run);
        Increment := 3;
      end
      else begin
        Y8Run := Source[0];
        Cb8Run := Source[1];
        Cr8Run := Source[2];
        Increment := 1;
      end;
      case FTargetBPS of
        8: begin // 888 to 888
          Target8 := Target;
          while Count > 0 do begin
            if Boolean(Mask and BitRun) then begin
              Y := Y8Run^;
              Inc(Y8Run, Increment);
              Cb := Cb8Run^;
              Inc(Cb8Run, Increment);
              Cr := Cr8Run^;
              Inc(Cr8Run, Increment);
              // blue
              Target8^ := ClampByte(Y + FCbToBlueTable[Cb]);
              Inc(Target8);
              // green
              Target8^ := ClampByte(Y + FCbToGreenTable[Cb] + FCrToGreentable[Cr]);
              Inc(Target8);
              // red
              Target8^ := ClampByte(Y + FCrToRedTable[Cr]);
              Inc(Target8, 1 + AlphaSkip);
            end
            else
              Inc(Target8, 3 + AlphaSkip);

            BitRun := (BitRun shr 1) or byte(BitRun shl 7);
            Dec(Count);
          end;
        end;

        16: begin // 888 to 161616
          Target16 := Target;
          while Count > 0 do begin
            if Boolean(Mask and BitRun) then begin
              Y := Y8Run^;
              Inc(Y8Run, Increment);
              Cb := Cb8Run^;
              Inc(Cb8Run, Increment);
              Cr := Cr8Run^;
              Inc(Cr8Run, Increment);       
              // blue
              Target16^ := MulDiv16(ClampByte(Y + FCbToBlueTable[Cb]), $FFFF, $FF);
              Inc(Target16);
              // green
              Target16^ := MulDiv16(ClampByte(Y + FCbToGreenTable[Cb] + FCrToGreentable[Cr]), $FFFF, $FF);
              Inc(Target16);
              // red
              Target16^ := MulDiv16(ClampByte(Y + FCrToRedTable[Cr]), $FFFF, $FF);
              Inc(Target16, 1 + AlphaSkip);
            end
            else
              Inc(Target16, 3 + AlphaSkip);

            BitRun := (BitRun shr 1) or byte(BitRun shl 7);
            Dec(Count);
          end;
        end;
      end;
    end;

    16: begin
      if Length(Source) = 1 then begin
        Y16Run := Source[0];
        Cb16Run := Y16Run; Inc(Cb16Run);
        Cr16Run := Cb16Run; Inc(Cr16Run);
        Increment := 3;
      end
      else begin
        Y16Run := Source[0];
        Cb16Run := Source[1];
        Cr16Run := Source[2];
        Increment := 1;
      end;
      case FTargetBPS of
        8: begin // 161616 to 888
          Target8 := Target;
          while Count > 0 do begin
            if Boolean(Mask and BitRun) then begin
              Y := MulDiv16(Y16Run^, $FF, $FFFF);
              Inc(Y16Run, Increment);
              Cb := MulDiv16(Cb16Run^, $FF, $FFFF);
              Inc(Cb16Run, Increment);
              Cr := MulDiv16(Cr16Run^, $FF, $FFFF);
              Inc(Cr16Run, Increment);

              // blue
              Target8^ := ClampByte(Y + FCbToBlueTable[Cb]);
              Inc(Target8);
              // green
              Target8^ := ClampByte(Y + FCbToGreenTable[Cb] + FCrToGreentable[Cr]);
              Inc(Target8);
              // red
              Target8^ := ClampByte(Y + FCrToRedTable[Cr]);
              Inc(Target8, 1 + AlphaSkip);
            end
            else
              Inc(Target8, 3 + AlphaSkip);

            BitRun := (BitRun shr 1) or byte(BitRun shl 7);
            Dec(Count);
          end;
        end;

        16: begin // 161616 to 161616
          Target16 := Target;
          // conversion from 16 to 16 is done with full precision, so there is no
          // loss of information, but the code is slower because the lookup tables
          // cannot be used
          while Count > 0 do begin
            if Boolean(Mask and BitRun) then begin
              Yf := 1.3584 * Y16Run^;
              Inc(Y16Run, Increment);
              Cbf := Cb16Run^ - 40092; // (156 * $FFFF) div $FF
              Inc(Cb16Run, Increment);
              Crf := Cr16Run^ - 35209; // (137 * $FFFF) div $FF
              Inc(Cr16Run, Increment);
              // blue
              Target16^ := Round(Yf + 2.2179 * Cbf);
              Inc(Target16);
              // green
              Target16^ := Round(Yf - 0.9271435 * Crf - 0.4302726 * Cbf);
              Inc(Target16);
              // red
              Target16^ := Round(Yf + 1.8215 * Crf);
              Inc(Target16, 1 + AlphaSkip);
            end
            else
              Inc(Target16, 3 + AlphaSkip);

            BitRun := (BitRun shr 1) or byte(BitRun shl 7);
            Dec(Count);
          end;
        end;
      end;
    end;
  end;
end;


procedure TColorManager.RowConvertYCbCr2RGB(const Source: array of Pointer; Target: Pointer; Count: Cardinal; Mask: Byte);
var
  Y, Cb, Cr: Integer;
  Yf, Cbf, Crf: Single;
  Y8Run, Cb8Run, Cr8Run: PByte;
  Y16Run, Cb16Run, Cr16Run: PWord;
  Target8: PByte;
  Target16: PWord;
  AlphaSkip: Integer;
  BitRun: Byte;
  Increment: Integer;
begin
  BitRun := $80;
  AlphaSkip := Ord(coAlpha in FTargetOptions); // 0 if no alpha must be skipped, otherwise 1
  case FSourceBPS of
    8: begin
      if Length(Source) = 1 then begin
        Y8Run := Source[0];
        Cb8Run := Y8Run; Inc(Cb8Run);
        Cr8Run := Cb8Run; Inc(Cr8Run);
        Increment := 3;
      end
      else begin
        Y8Run := Source[0];
        Cb8Run := Source[1];
        Cr8Run := Source[2];
        Increment := 1;
      end;
      case FTargetBPS of
        8: begin // 888 to 888
          Target8 := Target;
          while Count > 0 do begin
            if Boolean(Mask and BitRun) then begin
              Y := Y8Run^;
              Inc(Y8Run, Increment);
              Cb := Cb8Run^;
              Inc(Cb8Run, Increment);
              Cr := Cr8Run^;
              Inc(Cr8Run, Increment);
              // red
              Target8^ := ClampByte(Y + FCrToRedTable[Cr]);
              Inc(Target8, 1 + AlphaSkip);
              // green
              Target8^ := ClampByte(Y + FCbToGreenTable[Cb] + FCrToGreentable[Cr]);
              Inc(Target8);
              // blue
              Target8^ := ClampByte(Y + FCbToBlueTable[Cb]);
              Inc(Target8);
            end
            else
              Inc(Target8, 3 + AlphaSkip);

            BitRun := (BitRun shr 1) or byte(BitRun shl 7);
            Dec(Count);
          end;
        end;

        16: begin // 888 to 161616
          Target16 := Target;
          while Count > 0 do begin
            if Boolean(Mask and BitRun) then begin
              Y := Y8Run^;
              Inc(Y8Run, Increment);
              Cb := Cb8Run^;
              Inc(Cb8Run, Increment);
              Cr := Cr8Run^;
              Inc(Cr8Run, Increment);
              // red
              Target16^ := MulDiv16(ClampByte(Y + FCrToRedTable[Cr]), $FFFF, $FF);
              Inc(Target16, 1 + AlphaSkip);
              // green
              Target16^ := MulDiv16(ClampByte(Y + FCbToGreenTable[Cb] + FCrToGreentable[Cr]), $FFFF, $FF);
              Inc(Target16);
              // blue
              Target16^ := MulDiv16(ClampByte(Y + FCbToBlueTable[Cb]), $FFFF, $FF);
              Inc(Target16);
            end
            else Inc(Target16, 3 + AlphaSkip);
            BitRun := (BitRun shr 1) or byte(BitRun shl 7);
            Dec(Count);
          end;
        end;
      end;
    end;

    16: begin
      if Length(Source) = 1 then begin
        Y16Run := Source[0];
        Cb16Run := Y16Run; Inc(Cb16Run);
        Cr16Run := Cb16Run; Inc(Cr16Run);
        Increment := 3;
      end
      else begin
        Y16Run := Source[0];
        Cb16Run := Source[1];
        Cr16Run := Source[2];
        Increment := 1;
      end;
      case FTargetBPS of
        8: begin // 161616 to 888
          Target8 := Target;
          while Count > 0 do begin
            if Boolean(Mask and BitRun) then begin
              Y := MulDiv16(Y16Run^, $FF, $FFFF);
              Inc(Y16Run, Increment);
              Cb := MulDiv16(Cb16Run^, $FF, $FFFF);
              Inc(Cb16Run, Increment);
              Cr := MulDiv16(Cr16Run^, $FF, $FFFF);
              Inc(Cr16Run, Increment);
              // red
              Target8^ := ClampByte(Y + FCrToRedTable[Cr]);
              Inc(Target8, 1 + AlphaSkip);
              // green
              Target8^ := ClampByte(Y + FCbToGreenTable[Cb] + FCrToGreentable[Cr]);
              Inc(Target8);
              // blue
              Target8^ := ClampByte(Y + FCbToBlueTable[Cb]);
              Inc(Target8);
            end
            else
              Inc(Target8, 3 + AlphaSkip);

            BitRun := (BitRun shr 1) or byte(BitRun shl 7);
            Dec(Count);
          end;
        end;

        16: begin // 161616 to 161616
          Target16 := Target;
          // conversion from 16 to 16 is done with full precision, so there is no
          // loss of information, but the code is slower because the lookup tables
          // cannot be used
          while Count > 0 do begin
            if Boolean(Mask and BitRun) then begin
              Yf := 1.3584 * Y16Run^;
              Inc(Y16Run, Increment);
              Cbf := Cb16Run^ - 40092; // (156 * $FFFF) div $FF
              Inc(Cb16Run, Increment);
              Crf := Cr16Run^ - 35209; // (137 * $FFFF) div $FF
              Inc(Cr16Run, Increment);
              // red
              Target16^ := Round(Yf + 1.8215 * Crf);
              Inc(Target16, 1 + AlphaSkip);
              // green
              Target16^ := Round(Yf - 0.9271435 * Crf - 0.4302726 * Cbf);
              Inc(Target16);
              // blue
              Target16^ := Round(Yf + 2.2179 * Cbf);
              Inc(Target16);
            end
            else
              Inc(Target16, 3 + AlphaSkip);

            BitRun := (BitRun shr 1) or byte(BitRun shl 7);
            Dec(Count);
          end;
        end;
      end;
    end;
  end;
end;


procedure TColorManager.CreateYCbCrLookup;
var
  LumaRed, LumaGreen, LumaBlue: Single;
  I, Offset1, Offset2: Integer;
  F1, F2, F3, F4: Single;
begin
  LumaRed   := FYCbCrCoefficients[0];
  LumaGreen := FYCbCrCoefficients[1];
  LumaBlue  := FYCbCrCoefficients[2];

  F1 := 2 - 2 * LumaRed;
  F2 := LumaRed * F1 / LumaGreen;
  F3 := 2 - 2 * LumaBlue;
  F4 := LumaBlue * F3 / LumaGreen;

  SetLength(FCrToRedTable, 256);
  SetLength(FCbToBlueTable, 256);
  SetLength(FCrToGreenTable, 256);
  SetLength(FCbToGreenTable, 256);

  if FSourceScheme = csYCbCr then begin
    // I is the actual input pixel value in the range 0..$FF, Cb and Cr values are in the range -128..127.
    // (for TIFF files they are in a range defined by the ReferenceBlackWhite tag).
    Offset1 := -128;
    for I := 0 to $FF do begin
      FCrToRedTable[I]   :=  Round(F1 * Offset1);
      FCbToBlueTable[I]  :=  Round(F3 * Offset1);
      FCrToGreenTable[I] := -Round(F2 * Offset1);
      FCbToGreenTable[I] := -Round(F4 * Offset1);
      Inc(Offset1);
    end;
  end
  else begin
    // PhotoYCC
    // I is the actual input pixel value in the range 0..$FF, Cb values are in the range -156..99,
    // Cr values are in the range -137..118.
    // (for TIFF files they are in a range defined by the ReferenceBlackWhite tag).
    Offset1 := -156;
    Offset2 := -137;
    for I := 0 to $FF do begin
      FCrToRedTable[I]   :=  Round(F1 * Offset2);
      FCbToBlueTable[I]  :=  Round(F3 * Offset1);
      FCrToGreenTable[I] := -Round(F2 * Offset2);
      FCbToGreenTable[I] := -Round(F4 * Offset1);
      Inc(Offset1);
      Inc(Offset2);
    end;
  end;
end;


function TColorManager.GetPixelFormat(Index: Integer): TPixelFormat;
var
  SamplesPerPixel, BitsPerSample: Byte;
begin
  case Index of
    0: begin
      SamplesPerPixel := FSourceSPP;
      BitsPerSample := FSourceBPS;
    end
    
    else
      SamplesPerPixel := FTargetSPP;
      BitsPerSample := FTargetBPS;
  end;

  case SamplesPerPixel of
    1: // one sample per pixel, this is usually a palette format
      case BitsPerSample of
        1:     Result := pf1Bit;
        2..4:  Result := pf4bit; // values < 4 should be upscaled
        8..16: Result := pf8bit  // values > 8 bits must be downscaled to 8 bits
        else   Result := pfCustom;
      end;

    3: // Typical case is RGB or CIE L*a*b* (565 and 555 16 bit color formats would also be possible, but aren't handled by the manager).
      case BitsPerSample of
        1..5: Result := pf15Bit // values < 5 should be upscaled
        else  Result := pf24bit; // values > 8 bits should be downscaled
      end;

    4: // Typical cases: RGBA and CMYK (with 8 bps, other formats like PCX's 4 planes with 1 bit must be handled elsewhere)
      if BitsPerSample >= 8 then
        Result := pf32Bit
      else
        Result := pfCustom

    else
      Result := pfCustom;
  end;
end;


procedure TColorManager.PrepareConversion;
begin
  FRowConversion := nil;
  // Conversion between indexed and non-indexed formats is not supported as well as
  // between source BPS < 8 and target BPS > 8.
  // csGA and csG (grayscale w and w/o alpha) are considered being indexed modes
  if (FSourceScheme in [csIndexed, csG, csGA]) xor (FTargetScheme  in [csIndexed, csG]) then
    Error(gesIndexedNotSupported);

  // set up special conversion options
  if FSourceScheme in [csGA, csRGBA, csBGRA] then
    Include(FSourceOptions, coAlpha)
  else
    Exclude(FSourceOptions, coAlpha);

  if FTargetScheme in [csGA, csRGBA, csBGRA] then
    Include(FTargetOptions, coAlpha)
  else
    Exclude(FTargetOptions, coAlpha);

  case FSourceScheme of
    csG:
      if (FSourceBPS = 16) or (FTargetBPS = 16) then begin
        if (FSourceBPS >= 8) and (FTargetBPS >= 8) then
          FRowConversion := RowConvertGray;
      end
      else
        FRowConversion := RowConvertIndexed8;

    csGA:
      if (FSourceBPS in [8, 16]) and (FTargetBPS in [8, 16]) then
        FRowConversion := RowConvertGray;

    csIndexed:
      // Grayscale is handled like indexed mode.
      // Generally use indexed conversions (with various possible bit operations),
      // assign special methods for source only, target only or source and target being 16 bits per sample
      if (FSourceBPS = 16) and (FTargetBPS = 16) then
        FRowConversion := RowConvertIndexedBoth16
      else
        if FSourceBPS = 16 then
          FRowConversion := RowConvertIndexedSource16
        else
          if FTargetBPS = 16 then
            FRowConversion := RowConvertIndexedTarget16
          else
            FRowConversion := RowConvertIndexed8;

    csRGB, csRGBA:
      case FTargetScheme of
        csRGB:  FRowConversion := RowConvertRGB2RGB;
        csRGBA: FRowConversion := RowConvertRGB2RGB;
        csBGR:  FRowConversion := RowConvertRGB2BGR;
        csBGRA: FRowConversion := RowConvertRGB2BGR;
        csCMY: ;
        csCMYK: ;
        csCIELab: ;
        csYCbCr: ;
      end;

    csBGRA, csBGR:
      case FTargetScheme of
        csRGB,
        csRGBA: FRowConversion := RowConvertBGR2RGB;
        csBGR,
        csBGRA: FRowConversion := RowConvertBGR2BGR;
        csCMY: ;
        csCMYK: ;
        csCIELab: ;
        csYCbCr: ;
      end;

    csCMY:
      case FTargetScheme of
        csRGB: ;
        csRGBA: ;
        csBGR: ;
        csBGRA: ;
        csCMY: ;
        csCMYK: ;
        csCIELab: ;
        csYCbCr: ;
      end;

    csCMYK:
      case FTargetScheme of
        csRGB,
        csRGBA: FRowConversion := RowConvertCMYK2RGB;
        csBGR,
        csBGRA: FRowConversion := RowConvertCMYK2BGR;
        csCMY: ;
        csCMYK: ;
        csCIELab: ;
        csYCbCr: ;
      end;

    csCIELab:
      case FTargetScheme of
        csRGB,
        csRGBA: FRowConversion := RowConvertCIELab2RGB;
        csBGR,
        csBGRA: FRowConversion := RowConvertCIELab2BGR;
        csCMY: ;
        csCMYK: ;
        csCIELab: ;
        csYCbCr: ;
      end;

    csYCbCr: begin
      // create lookup tables to speed up conversion
      CreateYCbCrLookup;
      case FTargetScheme of
        csRGB,
        csRGBA: FRowConversion := RowConvertYCbCr2RGB;
        csBGR,
        csBGRA: FRowConversion := RowConvertYCbCr2BGR;
        csCMY: ;
        csCMYK: ;
        csCIELab: ;
        csYCbCr: ;
      end;
    end;

    csPhotoYCC: begin
      // create lookup tables to speed up conversion
      CreateYCbCrLookup;
      case FTargetScheme of
        csRGB,
        csRGBA: FRowConversion := RowConvertPhotoYCC2RGB;
        csBGR,
        csBGRA: FRowConversion := RowConvertPhotoYCC2BGR;
        csCMY: ;
        csCMYK: ;
        csCIELab: ;
        csYCbCr: ;
      end;
    end;
  end;
  FChanged := False;
end;


procedure TColorManager.SetSourceBitsPerSample(const Value: Byte);
begin
  if not (Value in [1..16]) then
    Error(gesInvalidSampleDepth);

  if FSourceBPS <> Value then begin
    FSourceBPS := Value;
    FChanged := True;
  end;
end;


procedure TColorManager.SetSourceColorScheme(const Value: TColorScheme);
begin
  if FSourceScheme <> Value then begin
    FSourceScheme := Value;
    FChanged := True;
  end;
end;


procedure TColorManager.SetSourceSamplesPerPixel(const Value: Byte);
begin
  if not (Value in [1..4]) then
    Error(gesInvalidPixelDepth);

  if FSourceSPP <> Value then begin
    FSourceSPP := Value;
    FChanged := True;
  end;
end;


procedure TColorManager.SetTargetBitsPerSample(const Value: Byte);
begin
  if not (Value in [1..16]) then
    Error(gesInvalidSampleDepth);

  if FTargetBPS <> Value then begin
    FTargetBPS := Value;
    FChanged := True;
  end;
end;


procedure TColorManager.SetTargetColorScheme(const Value: TColorScheme);
begin
  if FTargetScheme <> Value then begin
    FTargetScheme := Value;
    FChanged := True;
  end;
end;


procedure TColorManager.SetTargetSamplesPerPixel(const Value: Byte);
begin
  if not (Value in [1..4]) then
    Error(gesInvalidPixelDepth);

  if FTargetSPP <> Value then begin
    FTargetSPP := Value;
    FChanged := True;
  end;
end;


procedure TColorManager.ConvertRow(const Source: array of Pointer; Target: Pointer; Count: Cardinal; Mask: Byte);
begin
  // if there are pending changes then apply them
  if FChanged then
    PrepareConversion;
  // check if there's now a conversion method
  if @FRowConversion = nil then
    Error(gesConversionUnsupported)
  else
    FRowConversion(Source, Target, Count, Mask);
end;


function TColorManager.CreateColorPalette(const Data: array of Pointer; DataFormat: TRawPaletteFormat; ColorCount: Cardinal; RGB: Boolean): HPALETTE;
var
  I, MaxIn, MaxOut: Integer;
  LogPalette: TMaxLogPalette;
  RunR8, RunG8, RunB8: PByte;
  RunR16, RunG16, RunB16: PWord;
  Convert8:  function(Value: Byte): Byte of object;
  Convert16: function(Value: Word): Byte of object;
begin
  FillChar(LogPalette, SizeOf(LogPalette), 0);
  LogPalette.palVersion := $300;
  if ColorCount > 256 then
    LogPalette.palNumEntries := 256
  else
    LogPalette.palNumEntries := ColorCount;

  case DataFormat of
    pfInterlaced8Triple, pfInterlaced8Quad: begin
      RunR8 := Data[0];
      if coApplyGamma in FTargetOptions then
        Convert8 := ComponentGammaConvert
      else
        Convert8 := ComponentNoConvert8;

      if RGB then
        if DataFormat = pfInterlaced8Quad then
          for I := 0 to LogPalette.palNumEntries - 1 do begin
            with LogPalette.palPalEntry[I] do begin
              peBlue  := Convert8(RunR8^); Inc(RunR8);
              peGreen := Convert8(RunR8^); Inc(RunR8);
              peRed   := Convert8(RunR8^); Inc(RunR8);
            end;
            Inc(RunR8);
          end
        else
          for I := 0 to LogPalette.palNumEntries - 1 do
            with LogPalette.palPalEntry[I] do begin
              peBlue  := Convert8(RunR8^); Inc(RunR8);
              peGreen := Convert8(RunR8^); Inc(RunR8);
              peRed   := Convert8(RunR8^); Inc(RunR8);
            end
      else
        if DataFormat = pfInterlaced8Quad then
          for I := 0 to LogPalette.palNumEntries - 1 do begin
            with LogPalette.palPalEntry[I] do begin
              peRed   := Convert8(RunR8^); Inc(RunR8);
              peGreen := Convert8(RunR8^); Inc(RunR8);
              peBlue  := Convert8(RunR8^); Inc(RunR8);
            end;
            Inc(RunR8);
          end
        else
          for I := 0 to LogPalette.palNumEntries - 1 do
            with LogPalette.palPalEntry[I] do begin
              peRed   := Convert8(RunR8^); Inc(RunR8);
              peGreen := Convert8(RunR8^); Inc(RunR8);
              peBlue  := Convert8(RunR8^); Inc(RunR8);
            end
    end;

    pfPlane8Triple, pfPlane8Quad: begin
      RunR8 := Data[0];
      RunG8 := Data[1];
      RunB8 := Data[2];
      if coApplyGamma in FTargetOptions then
        Convert8 := ComponentGammaConvert
      else
        Convert8 := ComponentNoConvert8;

      for I := 0 to LogPalette.palNumEntries - 1 do begin
        LogPalette.palPalEntry[I].peRed   := Convert8(RunR8^); Inc(RunR8);
        LogPalette.palPalEntry[I].peGreen := Convert8(RunG8^); Inc(RunG8);
        LogPalette.palPalEntry[I].peBlue  := Convert8(RunB8^); Inc(RunB8);
      end;
    end;

    pfInterlaced16Triple, pfInterlaced16Quad: begin
      RunR16 := Data[0];
      if coApplyGamma in FTargetOptions then
        if coNeedByteSwap in FSourceOptions then
          Convert16 := ComponentSwapScaleGammaConvert
        else
          Convert16 := ComponentScaleGammaConvert
      else
        if coNeedByteSwap in FSourceOptions then
          Convert16 := ComponentSwapScaleConvert
        else
          Convert16 := ComponentScaleConvert;

      if RGB then
        for I := 0 to LogPalette.palNumEntries - 1 do begin
          LogPalette.palPalEntry[I].peRed   := Convert16(RunR16^); Inc(RunR16);
          LogPalette.palPalEntry[I].peGreen := Convert16(RunR16^); Inc(RunR16);
          LogPalette.palPalEntry[I].peBlue  := Convert16(RunR16^); Inc(RunR16);
          if DataFormat = pfInterlaced16Quad then
            Inc(RunR16);
        end
      else
        for I := 0 to LogPalette.palNumEntries - 1 do begin
          LogPalette.palPalEntry[I].peBlue  := Convert16(RunR16^); Inc(RunR16);
          LogPalette.palPalEntry[I].peGreen := Convert16(RunR16^); Inc(RunR16);
          LogPalette.palPalEntry[I].peRed   := Convert16(RunR16^); Inc(RunR16);
          if DataFormat = pfInterlaced16Quad then
            Inc(RunR16);
        end;
    end;

    pfPlane16Triple, pfPlane16Quad: begin
      RunR16 := Data[0];
      RunG16 := Data[1];
      RunB16 := Data[2];
      if coApplyGamma in FTargetOptions then
        if coNeedByteSwap in FSourceOptions then
          Convert16 := ComponentSwapScaleGammaConvert
        else
          Convert16 := ComponentScaleGammaConvert
      else
        if coNeedByteSwap in FSourceOptions then
          Convert16 := ComponentSwapScaleConvert
        else
          Convert16 := ComponentScaleConvert;

      for I := 0 to LogPalette.palNumEntries - 1 do begin
        LogPalette.palPalEntry[I].peRed := Convert16(RunR16^); Inc(RunR16);
        LogPalette.palPalEntry[I].peGreen := Convert16(RunG16^); Inc(RunG16);
        LogPalette.palPalEntry[I].peBlue := Convert16(RunB16^); Inc(RunB16);
      end;
    end;
  end;
  MaxIn := (1 shl FSourceBPS) - 1;
  MaxOut := (1 shl FTargetBPS) - 1;
  if (FTargetBPS <= 8) and (MaxIn <> MaxOut) then begin
    // If target resolution and given color depth differ then the palette needs to be adjusted.
    // Consider the case for 2 bit to 4 bit conversion. Only 4 colors will be given to create
    // the palette but after scaling all values will be up to 15 for which no color is in the palette.
    // This and the reverse case need to be accounted for.
    MaxIn := (1 shl FSourceBPS) - 1;
    MaxOut := (1 shl FTargetBPS) - 1;
    if MaxIn < MaxOut then // palette is too small, enhance it
      for I := MaxOut downto 0 do begin
        LogPalette.palPalEntry[I].peRed := LogPalette.palPalEntry[MulDiv16(I, MaxIn, MaxOut)].peRed;
        LogPalette.palPalEntry[I].peGreen := LogPalette.palPalEntry[MulDiv16(I, MaxIn, MaxOut)].peGreen;
        LogPalette.palPalEntry[I].peBlue := LogPalette.palPalEntry[MulDiv16(I, MaxIn, MaxOut)].peBlue;
      end
    else // palette contains too many entries, shorten it
      for I := 0 to MaxOut do begin
        LogPalette.palPalEntry[I].peRed := LogPalette.palPalEntry[MulDiv16(I, MaxIn, MaxOut)].peRed;
        LogPalette.palPalEntry[I].peGreen := LogPalette.palPalEntry[MulDiv16(I, MaxIn, MaxOut)].peGreen;
        LogPalette.palPalEntry[I].peBlue := LogPalette.palPalEntry[MulDiv16(I, MaxIn, MaxOut)].peBlue;
      end;

    LogPalette.palNumEntries := MaxOut + 1;
  end;                      
  // finally create palette
  Result := CreatePalette(PLogPalette(@LogPalette)^);
end;


function TColorManager.CreateGrayscalePalette(MinimumIsWhite: Boolean): HPALETTE;
var
  LogPalette: TMaxLogPalette;
  I: Integer;
  BPS, Upper, Factor: Byte;
begin
  FillChar(LogPalette, SizeOf(LogPalette), 0);
  LogPalette.palVersion := $300;
  // the product of BPS and SPP considers planar organizatons correctly
  // (e.g. PCX has a format 4 planes with 1 bit resulting to 16 color image)
  BPS := FTargetBPS * FTargetSPP;
  if BPS > 8 then
    BPS := 8;

  LogPalette.palNumEntries := 1 shl BPS;
  Upper := LogPalette.palNumEntries - 1;
  Factor := $FF div Upper;
  if MinimumIsWhite then
    if not (coApplyGamma in FTargetOptions) then
      for I := 0 to Upper do begin
        LogPalette.palPalEntry[Upper - I].peBlue  := I * Factor;
        LogPalette.palPalEntry[Upper - I].peGreen := I * Factor;
        LogPalette.palPalEntry[Upper - I].peRed   := I * Factor;
      end
    else
      for I := 0 to Upper do begin
        LogPalette.palPalEntry[Upper - I].peBlue := FGammaTable[I * Factor];
        LogPalette.palPalEntry[Upper - I].peGreen := FGammaTable[I * Factor];
        LogPalette.palPalEntry[Upper - I].peRed := FGammaTable[I * Factor];
      end
  else
    if not (coApplyGamma in FTargetOptions) then
      for I := 0 to Upper do begin
        LogPalette.palPalEntry[I].peBlue := I * Factor;
        LogPalette.palPalEntry[I].peGreen := I * Factor;
        LogPalette.palPalEntry[I].peRed := I * Factor;
      end
    else
      for I := 0 to Upper do begin
        LogPalette.palPalEntry[I].peBlue := FGammaTable[I * Factor];
        LogPalette.palPalEntry[I].peGreen := FGammaTable[I * Factor];
        LogPalette.palPalEntry[I].peRed := FGammaTable[I * Factor];
      end;

  // finally create palette
  Result := CreatePalette(PLogPalette(@LogPalette)^);
end;


procedure TColorManager.Error(const Msg: String);
begin
  raise EColorConversionError.Create(Msg);
end;


procedure TColorManager.SetGamma(MainGamma, DisplayGamma: Single);
var
  I, SourceHighBound, TargetHighBound: Integer;
  Gamma: Single;
begin
  if MainGamma <= 0 then
    FMainGamma := 1
  else
    FMainGamma := MainGamma;

  if DisplayGamma <= 0 then
    FDisplayGamma := 2.2 // default value for a usual CRT
  else
    FDisplayGamma := DisplayGamma;

  Gamma := 1 / (FMainGamma * FDisplayGamma);
  // source high bound is the maximum possible source value which can appear (0..$FF)
  if FSourceBPS >= 8 then
    SourceHighBound := $FF
  else
    SourceHighBound := (1 shl FTargetBPS) - 1;

  // target high bound is the target value which corresponds to a target sample value of 1 (0..$FF)
  if FTargetBPS >= 8 then
    TargetHighBound := $FF
  else
    TargetHighBound := (1 shl FTargetBPS) - 1;

  for I := 0 to SourceHighBound do
    FGammaTable[I] := Round(Power((I / SourceHighBound), Gamma) * TargetHighBound);
end;


procedure TPNGGraphic.Draw(ACanvas: TCanvas; const Rect: TRect);
var
  DstBmp, SrcBmp, SrcBmp8, Mask: TBitmap;
  w, h: integer;
begin
  if (PixelFormat = pf32bit) then begin
    w := WidthOf(Rect);
    h := HeightOf(Rect);
    DstBmp := CreateBmp32(w, h + Integer(Reflected) * (h div 2));
    BitBlt(DstBmp.Canvas.Handle, 0, 0, w, DstBmp.Height, ACanvas.Handle, Rect.Left, Rect.Top, SRCCOPY);
    if (w = Width) and (h = Height) then
      CopyBmp32(MkRect(w, h), MkRect(w, h), DstBmp, Self, EmptyCI, False, Graphics.clNone, 0, Reflected)
    else begin // If stretched
      SrcBmp := TBitmap.Create;
      SrcBmp.PixelFormat := pfDevice;
      SrcBmp.Width := w;
      SrcBmp.Height := h;

      SetStretchBltMode(SrcBmp.Canvas.Handle, COLORONCOLOR);
      StretchBlt(SrcBmp.Canvas.Handle, 0, 0, w, h, Canvas.Handle, 0, 0, Width, Height, SRCCOPY);
      SrcBmp.PixelFormat := pf32bit;

      Mask := CreateBmpLike(Self);
      Mask.PixelFormat := pf8bit;

      CopyChannel(Self, Mask, 3, True);
      Mask.PixelFormat := pfDevice;
      SrcBmp8 := TBitmap.Create;
      SrcBmp8.PixelFormat := pfDevice;
      SrcBmp8.Width := w;
      SrcBmp8.Height := h;
      SetStretchBltMode(SrcBmp8.Canvas.Handle, COLORONCOLOR);
      StretchBlt(SrcBmp8.Canvas.Handle, 0, 0, w, h, Mask.Canvas.Handle, 0, 0, Width, Height, SRCCOPY);
      SrcBmp8.PixelFormat := pf8bit;
      CopyChannel(SrcBmp, SrcBmp8, 3, False);
      FreeAndNil(Mask);
      FreeAndNil(SrcBmp8);
      CopyBmp32(MkRect(w, h), MkRect(w, h), DstBmp, SrcBmp, EmptyCI, False, Graphics.clNone, 0, False);
      FreeAndNil(SrcBmp);
    end;
    BitBlt(ACanvas.Handle, Rect.Left, Rect.Top, DstBmp.Width, DstBmp.Height, DstBmp.Canvas.Handle, 0, 0, SRCCOPY);
    FreeAndNil(DstBmp);
  end
  else
    inherited
end;


initialization
{$IFNDEF NOACPNG}
  {$IFDEF D2010}
    if not ACPngFirst then
      TPicture.UnregisterGraphicClass(TPngImage);

    TPicture.RegisterFileFormat('png', 'Portable network graphics (AlphaControls)', TPNGGraphic);
    if not ACPngFirst then
      TPicture.RegisterFileFormat('PNG', 'Portable Network Graphics', TPngImage);
  {$ELSE}
    TPicture.RegisterFileFormat('png', 'Portable network graphics (AlphaControls)', TPNGGraphic);
  {$ENDIF}
{$ENDIF}
finalization
  TPicture.UnregisterGraphicClass(TPNGGraphic);

end.
