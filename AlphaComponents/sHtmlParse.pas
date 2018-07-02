unit sHtmlParse;
{$I sDefs.inc}

interface

uses
  Windows, Graphics, SysUtils, Classes, Math, StdCtrls,
  {$IFDEF DELPHI_XE2} UItypes, {$ENDIF}
  {$IFDEF TNTUNICODE} TntGraphics, {$ENDIF}
  acntUtils, sConst;


type
  TacLink = record
    Bounds: TRect;
    URL: string;
  end;

  TacLinks = array of TacLink;
  PacLinks = ^TacLinks;

  TsHtml = class(TObject)
  public
    Origin,
    UppedText: acString;

    Len,
    CurX,
    CurY,
    HotLink,
    WrapWidth,
    LineNumber,
    PressedLink,
    MaxBmpWidth,
    MaxBmpHeight,
    CurWidthValue,
    CurrentRowHeight: integer;

    Area: TRect;
    Links: TacLinks;
    Bitmap: TBitmap;
    Layout: TTextLayout;
    Calculating: boolean;
    Alignment: TAlignment;
    AlphaChannel: boolean;
    BidiMode: TBiDiMode;
    aFonts: array of TFont;
    aLineSizes: array of TSize;
    SkinManager: TComponent;
    DefaultFontColor: TColor;
    procedure BackFont;
    constructor Create;
    destructor Destroy; override;
    procedure ShowCut(inCurPos: integer; inLastPos: integer);
    function ExecTag(const s: acString; CurPos: integer = -1): boolean;

    procedure NewRow;
    procedure InitNewX;
    procedure UpdateRowHeight;
    procedure AddSize(w, h: integer);
    function HTMLText(var ALinks: TacLinks; CalcOnly: boolean = False): TRect;

    function GetSpecialCharacter(const inString: acString; inPos: Integer): acString;
    procedure NewFont(const s: acString; IsLink: boolean = False; State: integer = 0);
    procedure Init(Bmp: TBitmap; const Text: acString; aRect: TRect; AHotLink: integer = -1; APressedLink: integer = -1);
  end;


procedure SetFont(Font: TFont; const Tag: acString);

implementation

uses sVCLUtils, sGraphUtils, sAlphaGraph, sSkinManager;


const
  DisabledChars = [#13, #10];


function GetTag(const s: acString; CurPos: integer): acString;
var
  i, l, j: integer;
begin
  Result := '';
  l := length(s);
  for i := CurPos to l do
    if s[i] = '>' then begin
      for j := CurPos to i do
        Result := Result + s[j];

      Break;
    end;
end;


type
  TacTColorData = record
    Name: string;
    Value: TColor;
  end;


const
  ColorsCount = 16;
  Delims = [s_Space, '=', '"', #13, #10, '<', '>'];
  Colors: array [0..ColorsCount - 1] of TacTColorData = (
    (Name:'AQUA'   ; Value: clAqua),
    (Name:'BLACK'  ; Value: clBlack),
    (Name:'BLUE'   ; Value: clBlue),
    (Name:'FUCHSIA'; Value: clFuchsia),
    (Name:'GRAY'   ; Value: clGray),
    (Name:'GREEN'  ; Value: clGreen),
    (Name:'LIME'   ; Value: clLime),
    (Name:'MAROON' ; Value: clMaroon),
    (Name:'NAVY'   ; Value: clNavy),
    (Name:'OLIVE'  ; Value: clOlive),
    (Name:'PURPLE' ; Value: clPurple),
    (Name:'RED'    ; Value: clRed),
    (Name:'SILVER' ; Value: clSilver),
    (Name:'TEAL'   ; Value: clTeal),
    (Name:'WHITE'  ; Value: clWhite),
    (Name:'YELLOW' ; Value: clYellow)
  );


procedure SetFont(Font: TFont; const Tag: acString);
var
  i, j, count, len, j1, j2: integer;
  Atom, Value: acString;
begin
  count := acWordCount(Tag, Delims);
  len := length(tag);
  for i := 1 to count do begin
    Atom := acExtractWord(i, Tag, Delims);
    if Atom = 'SIZE' then begin
      Value := acExtractWord(i + 1, Tag, Delims);
      if Value <> '' then
        if Value[1] = CharMinus then begin
          Delete(Value, 1, 1);
          if Value <> '' then
            Font.Size := Font.Size - StrToInt(Value);
        end
        else
          if Value[1] = CharPlus then begin
            Delete(Value, 1, 1);
            if Value <> '' then
              Font.Size := Font.Size + StrToInt(Value);
          end
          else
            try
              Font.Size := StrToInt(Value);
            except
              Font.Size := 8;
            end;
    end
    else
      if Atom = 'COLOR' then begin
        Value := acExtractWord(i + 1, Tag, Delims);
        if Value <> '' then
          if Value[1] = CharDiez then begin
            Delete(Value, 1, 1);
            Font.Color := SwapInteger(HexToInt(Value));
          end
          else begin
            for j := 0 to ColorsCount - 1 do 
              if Value = Colors[j].Name then begin
                Font.Color := Colors[j].Value;
                Exit;
              end;

            Font.Color := SwapInteger(HexToInt(Value));
          end;
      end
      else
        if Atom = 'FACE' then begin
          j1 := pos(Atom, Tag);
          while (Tag[j1] <> '"') and (j1 < len) do
            inc(j1);

          j2 := j1 + 1;
          while (Tag[j2] <> '"') and (j2 < len) do
            inc(j2);

          if j2 > j1 then begin
            Value := '';
            inc(j1);
            while j1 < j2 do begin
              Value := Value + Tag[j1];
              inc(j1);
            end;
            if Value <> '' then
              Font.Name := Value;
          end;
        end;
  end;
end;


procedure TsHtml.AddSize(w, h: integer);
var
  l: integer;
begin
  if Calculating then begin
    l := Length(aLineSizes);
    SetLength(aLineSizes, l + 1);
    aLineSizes[l] := MkSize(w, h);
  end;
end;


procedure TsHtml.BackFont;
var
  len: integer;
begin
  len := High(aFonts);
  if len <> -1 then begin
    Bitmap.Canvas.Font.Assign(aFonts[len]);
    if Assigned(aFonts[len]) then
      FreeAndNil(aFonts[len]);

    SetLength(aFonts, len);
  end;
end;


constructor TsHtml.Create;
begin
  inherited;
  SkinManager := nil;
  Alignment := taLeftJustify;
  BidiMode := bdLeftToRight;
  DefaultFontColor := clBlack;
  Layout := tlTop;
end;


destructor TsHtml.Destroy;
var
  len: integer;
begin
  Bitmap := nil;
  len := High(aFonts);
  while Len >= 0 do begin
    if Assigned(aFonts[len]) then
      FreeAndNil(aFonts[len]);

    SetLength(aFonts, len);
    len := High(aFonts);
  end;
  inherited;
end;


function TsHtml.ExecTag(const s: acString; CurPos: integer = -1): boolean;

  function OpenTag(const Tag: acString): boolean;
  var
    p: integer;
  begin
    Result := False;
    p := pos(Tag, s);
    if p > 0 then
      if (p + length(Tag)) <= length(s) then
        if (Tag = 'FONT') or (Tag = 'A') then
          Result := True
        else
          Result := (p = 2) and CharInSet(AnsiChar(s[p + length(Tag)]), ['>', ' ']);
  end;

  function CloseTag(const Tag: acString): boolean;
  begin
    Result := pos(Tag, s) = 3;
  end;

  function ExtractAddress(str: acString): acString;
  var
    hr_pos: integer;
  begin
    hr_pos := pos('HREF', UpperCase(str));
    Delete(str, 1, hr_pos + 3);
    hr_pos := pos('=', UpperCase(str));
    Delete(str, 1, hr_pos + 1);
    Result := str;
    Result := DelChars(Result, '''');
    Result := DelChars(Result, '"');
    Result := DelChars(Result, '>');
  end;

begin
  Result := True;
  with Bitmap.Canvas, Font do
    if (s <> '') and (s[2] = '/') then begin
      if CloseTag('B') then
        Style := Style - [fsBold]
      else
        if CloseTag('I') then
          Style := Style - [fsItalic]
        else
          if CloseTag('U') then
            Style := Style - [fsUnderline]
          else
            if CloseTag('FONT') then
              BackFont
            else
              if CloseTag('A') then
                with Links[Length(Links) - 1] do begin
                  URL := ExtractAddress(URL);
                  Bounds.Right := CurX;
                  Bounds.Bottom := Bounds.Top + CurrentRowHeight;
                  BackFont;
                end
              else
                if CloseTag('P') then begin
                  NewRow;
                  UpdateRowHeight;
                end
                else
                  Result := False;
    end
    else
      if OpenTag('BR') or OpenTag('BR/') then begin
        NewRow;
        UpdateRowHeight;
      end
      else
        if OpenTag('B') then
          Style := Style + [fsBold]
        else
          if OpenTag('I') then
            Style := Style + [fsItalic]
          else
            if OpenTag('U') then
              Style := Style + [fsUnderline]
            else
              if OpenTag('P') then begin
                NewRow;
                UpdateRowHeight;
              end
              else
                if OpenTag('FONT') then
                  NewFont(s)
                else
                  if OpenTag('A') then begin
                    NewFont(s, True, integer(Length(Links) = PressedLink) + integer(Length(Links) = HotLink));
                    SetLength(Links, Length(Links) + 1);
                    with Links[Length(Links) - 1] do begin
                      Bounds.Left := CurX;
                      Bounds.Top := CurY;
                      if CurPos >= 0 then
                        URL := Copy(origin, CurPos, Length(s)) // Save all tag for future processing
                      else
                        URL := s; // Save all tag for future processing
                    end;
                  end
                  else
                    Result := False;
end;


const
  CharsCount = 87;
  SpecialChars: array [1..CharsCount, 1..3] of string = (
    ('ndash',  '#8211', '–'),
    ('mdash',  '#8212', '—'),
    ('nbsp',   '#160',  ' '),
    ('iexcl',  '#161',  '¡'),
    ('iquest', '#191',  '¿'),
    ('quot',   '#34',   '"'),
    ('ldquo',  '#8220', '“'),
    ('rdquo',  '#8221', '”'),
    ('39',     '39',    #39),
    ('lsquo',  '#8216', '‘'),
    ('rsquo',  '#8217', '’'),
    ('laquo',  '#171',  '«'),
    ('raquo',  '#187',  '»'),
    ('amp',    '#38',   '&'),
    ('cent',   '#162',  '¢'),
    ('copy',   '#169',  '©'),
    ('divide', '#247',  '÷'),
    ('gt',     '#62',   '>'),
    ('lt',     '#60',   '<'),
    ('micro',  '#181',  'µ'),
    ('middot', '#183',  '·'),
    ('para',   '#182',  '¶'),
    ('plusmn', '#177',  '±'),
    ('euro',   '#8364', '€'),
    ('pound',  '#163',  '£'),
    ('reg',    '#174',  '®'),
    ('sect',   '#167',  '§'),
    ('trade',  '#153',  '™'),
    ('yen',    '#165',  '¥'),
    ('szlig',  '#223',  'ß'),
    ('yuml',   '#255',  'ÿ'),
    ('aacute', '#225',  'á'),
    ('Aacute', '#193',  'Á'),
    ('agrave', '#224',  'à'),
    ('Agrave', '#192',  'À'),
    ('acirc',  '#226',  'â'),
    ('Acirc',  '#194',  'Â'),
    ('aring',  '#229',  'å'),
    ('Aring',  '#197',  'Å'),
    ('atilde', '#227',  'ã'),
    ('Atilde', '#195',  'Ã'),
    ('auml',   '#228',  'ä'),
    ('Auml',   '#196',  'Ä'),
    ('aelig',  '#230',  'æ'),
    ('AElig',  '#198',  'Æ'),
    ('ccedil', '#231',  'ç'),
    ('Ccedil', '#199',  'Ç'),
    ('eacute', '#233',  'é'),
    ('Eacute', '#201',  'É'),
    ('egrave', '#232',  'è'),
    ('Egrave', '#200',  'È'),
    ('ecirc',  '#234',  'ê'),
    ('Ecirc',  '#202',  'Ê'),
    ('euml',   '#235',  'ë'),
    ('Euml',   '#203',  'Ë'),
    ('iacute', '#237',  'í'),
    ('Iacute', '#205',  'Í'),
    ('igrave', '#236',  'ì'),
    ('Igrave', '#204',  'Ì'),
    ('icirc',  '#238',  'î'),
    ('Icirc',  '#206',  'Î'),
    ('iuml',   '#239',  'ï'),
    ('Iuml',   '#207',  'Ï'),
    ('ntilde', '#241',  'ñ'),
    ('Ntilde', '#209',  'Ñ'),
    ('oacute', '#243',  'ó'),
    ('Oacute', '#211',  'Ó'),
    ('ograve', '#242',  'ò'),
    ('Ograve', '#210',  'Ò'),
    ('ocirc',  '#244',  'ô'),
    ('Ocirc',  '#212',  'Ô'),
    ('oslash', '#248',  'ø'),
    ('Oslash', '#216',  'Ø'),
    ('otilde', '#245',  'õ'),
    ('Otilde', '#213',  'Õ'),
    ('ouml',   '#246',  'ö'),
    ('Ouml',   '#214',  'Ö'),
    ('uacute', '#250',  'ú'),
    ('Uacute', '#218',  'Ú'),
    ('ugrave', '#249',  'ù'),
    ('Ugrave', '#217',  'Ù'),
    ('ucirc',  '#251',  'û'),
    ('Ucirc',  '#219',  'Û'),
    ('uuml',   '#252',  'ü'),
    ('Uuml',   '#220',  'Ü'),
    ('#180',   '#180',  '´'),
    ('#96',    '#96',   '`')
  );


function TsHtml.GetSpecialCharacter(const inString: acString; inPos: Integer): acString;
var
  SemiColonPos, SpacePos, i, w: Integer;
  SpecialChar: acString;
  Flags: Cardinal;
  R: TRect;
begin
  Result := Copy(inString, Succ(inPos), 255);
  SemiColonPos := Pos(';', Result);
  SpacePos := Pos(s_Space, Result);
  if (SemiColonPos = 0) or (SpacePos > 0) and (SpacePos < SemiColonPos) then
    Result := ''
  else begin
    Result := Copy(Result, 1, Pred(SemiColonPos));
    for i := 1 to CharsCount do
      if SameText(Result, SpecialChars[i][1]) or (Result = SpecialChars[i][2]) then begin
        SpecialChar := SpecialChars[i][3];
        break;
      end;

    if SpecialChar = '' then
      Result := ''
    else begin
      w := acTextWidth(Bitmap.Canvas, SpecialChar);
      if (Area.Right > 0) and (CurX + w > Area.Right) then
        NewRow;

      if BidiMode = bdRightToLeft then
        Flags := DT_RIGHT or DT_RTLREADING
      else
        Flags := 0;

      if BidiMode in [bdRightToLeft, bdRightToLeftNoAlign] then
        Flags := Flags or DT_RTLREADING;

      R := Rect(CurX, CurY, CurX + w, CurY + CurrentRowHeight);
      AcDrawText(Bitmap.Canvas.Handle, SpecialChar, R, Flags);

      inc(CurX, + w);
      CurWidthValue := CurX;
      if Calculating then begin
        if Bitmap.Canvas.TextHeight(s_Yy) > CurrentRowHeight then
          CurrentRowHeight := Bitmap.Canvas.TextHeight(s_Yy);
      end
      else
        CurrentRowHeight := aLineSizes[LineNumber].cy;

      Result := '&' + Result + ';';
    end;
  end;
end;


function TsHtml.HTMLText(var ALinks: TacLinks; CalcOnly: boolean = False): TRect;
var
  CurPos, LastPos: integer;
  sCurrentTag, sCurrentChar: acString;
begin
  if not Calculating then begin
    SetLength(aLineSizes, 0);
    Calculating := True;
    LineNumber := 0;
    InitNewX;
    CurY := 0;
    Result := HTMLText(Alinks, True);
    Calculating := False;
  end;
  if Calculating and CalcOnly or (not Calculating and not CalcOnly) then begin
    Bitmap.Canvas.Font.Color := DefaultFontColor;
    CurY := 0;
    LineNumber := 0;
    MaxBmpWidth := 0;
    MaxBmpHeight := 0;
    CurWidthValue := 0;
    InitNewX;
    CurY := Area.Top;
    Links := ALinks;
    SetLength(Links, 0);
    try
      LastPos := 1;
      CurPos := 1;
      while CurPos <= Len do begin
        case UppedText[CurPos] of
          '&': begin
            if CurPos > LastPos then
              ShowCut(CurPos, LastPos);

            sCurrentChar := GetSpecialCharacter(origin, CurPos);
            if sCurrentChar <> '' then begin
              inc(CurPos, Length(sCurrentChar));
              LastPos := CurPos;
              dec(CurPos);
            end
            else
              LastPos := CurPos + integer(UppedText[CurPos + 1] = '&' {Skip second char});
          end;

          #$D: if sConst.acHtmlStdBreak then begin
            if CurPos > LastPos then
              ShowCut(CurPos, LastPos);

            LastPos := CurPos;
            NewRow;
            UpdateRowHeight;
          end;

          '<':
            if UppedText[CurPos + 1] <> '<' then begin
              if CurPos > LastPos then
                ShowCut(CurPos, LastPos);

              sCurrentTag := GetTag(UppedText, CurPos);
              if sCurrentTag = '' then begin
                inc(CurPos);
                LastPos := CurPos;
                Continue;
              end;

              if ExecTag(sCurrentTag, CurPos) then begin
                inc(CurPos, Length(sCurrentTag));
                LastPos := CurPos;
                dec(CurPos);
              end
              else begin
                inc(CurPos);
                ShowCut(CurPos, LastPos);
                LastPos := CurPos;
              end;
            end
            else begin
              if CurPos > LastPos then
                 ShowCut(CurPos, LastPos);

              inc(CurPos);
              LastPos := CurPos;
            end;
        end;
        inc(CurPos);
      end;
      if CurPos > LastPos then
        ShowCut(CurPos, LastPos);

      AddSize(CurWidthValue, CurrentRowHeight);
      if CurWidthValue > MaxBmpWidth then
        MaxBmpWidth := CurWidthValue;

      CurWidthValue := 0;
      MaxBmpHeight := CurY + CurrentRowHeight;
    finally
      Result := MkRect(MaxBmpWidth, MaxBmpHeight);
      Calculating := False;
    end;
    ALinks := Links;
  end;
end;


procedure TsHtml.Init(Bmp: TBitmap; const Text: acString; aRect: TRect; AHotLink: integer = -1; APressedLink: integer = -1);
begin
  Bitmap := Bmp;
  DefaultFontColor := Bitmap.Canvas.Font.Color;
  Origin := Text;
  Area := aRect;
  UppedText := AnsiUpperCase(Origin);
  Len := Length(UppedText);
  MaxBmpWidth := 0;
  MaxBmpHeight := 0;
  CurWidthValue := 0;
  HotLink := AHotLink;
  PressedLink := APressedLink;
end;


procedure TsHtml.InitNewX;
begin
  if Calculating then
    CurX := 0//Area.Left
  else
    case BidiAlign[BidiMode = bdRightToLeft, Alignment] of
      taLeftJustify:  CurX := Area.Left;
      taCenter:       CurX := Area.Left + (WidthOf(Area) - aLineSizes[LineNumber].cx) div 2;
      taRightJustify: CurX := Area.Left + WidthOf(Area) - aLineSizes[LineNumber].cx;
    end;
end;


procedure TsHtml.UpdateRowHeight;
begin
  if CurrentRowHeight = 0 then
    if Calculating then
      CurrentRowHeight := Bitmap.Canvas.TextHeight(s_Yy)
    else
      CurrentRowHeight := aLineSizes[LineNumber].cy;
end;


procedure TsHtml.NewFont(const s: acString; IsLink: boolean = False; State: integer = 0);
var
  len: integer;
begin
  len := High(aFonts);
  inc(len, 2);
  SetLength(aFonts, len);
  // Save prev font
  aFonts[len - 1] := TFont.Create;
  aFonts[len - 1].Assign(Bitmap.Canvas.Font);
  if IsLink then begin
    Bitmap.Canvas.Font.Color := clBlue;
    if (SkinManager <> nil) and TsSkinManager(SkinManager).Active then
      with TsSkinManager(SkinManager) do
        case State of
          2:   Bitmap.Canvas.Font.Color := BlendColors(Palette[pcWebTextHot], Palette[pcLabelText], 127);
          1:   Bitmap.Canvas.Font.Color := Palette[pcWebTextHot]
          else Bitmap.Canvas.Font.Color := Palette[pcWebText]
        end
    else
      case State of
        2:   Bitmap.Canvas.Font.Color := clRed
        else Bitmap.Canvas.Font.Color := clBlue
      end;

    if State <> 0 then
      Bitmap.Canvas.Font.Style := Bitmap.Canvas.Font.Style + [fsUnderline]
  end
  else
    SetFont(Bitmap.Canvas.Font, s);
end;


procedure TsHtml.NewRow;
begin
  inc(CurY, CurrentRowHeight + 2);
  if Calculating then
    AddSize(CurWidthValue, CurrentRowHeight);

  if CurWidthValue > MaxBmpWidth then
    MaxBmpWidth := CurWidthValue;

  CurWidthValue := 0;
  CurrentRowHeight := 0;
  inc(LineNumber);
  InitNewX;
end;


procedure TsHtml.ShowCut(inCurPos: integer; inLastPos: integer);
var
  R: TRect;
  Flags: Cardinal;
  CutString: acString;
  i, SpaceWidth, sw, sh: integer;

  procedure OutLine(sLine: acString);
  var
    j, c, w, h, CharCount: integer;
    s: acString;
  begin
    c := acWordCount(sLine, [s_Space]);
    CharCount := 0;
    for j := 1 to c do begin
      s := acExtractWord(j, sLine, [s_Space]);
      w := acTextWidth(Bitmap.Canvas, s);
      h := acTextHeight(Bitmap.Canvas, s_Yy);
      if w > WidthOf(Area) then begin
        InitNewX;
        if not Calculating then
          if AlphaChannel then
            WriteColor(Bitmap, PacChar(s), True, Rect(CurX, CurY, CurX + w, CurY + h), Flags, Bitmap.Canvas.Font.Color)
          else begin
            R := Rect(CurX, CurY, CurX + w, CurY + CurrentRowHeight);
            AcDrawText(Bitmap.Canvas.Handle, s, R, Flags);
          end;

        CurrentRowHeight := max(CurrentRowHeight, h);
        inc(CurY, CurrentRowHeight);
        CharCount := Length(s);
        if CharCount > 0 then
          Delete(sLine, 1, CharCount);

        OutLine(sLine);
        CurWidthValue := max(CurWidthValue, w);
        Break;
      end
      else
        if CurX + w > Area.Right then begin
          InitNewX;
          CurrentRowHeight := max(CurrentRowHeight, h);
          inc(CurY, CurrentRowHeight);
          if CharCount > 0 then
            Delete(sLine, 1, CharCount);

          OutLine(sLine);
          Break;
        end
        else begin
          if not Calculating then
            if AlphaChannel then
              WriteColor(Bitmap, PacChar(s), True, Rect(CurX, CurY, CurX + w, CurY + h), Flags, Bitmap.Canvas.Font.Color)
            else begin
              R := Rect(CurX, CurY, CurX + w, CurY + CurrentRowHeight);
              AcDrawText(Bitmap.Canvas.Handle, s, R, Flags);
            end;

          inc(CurX, w + SpaceWidth);
          inc(CharCount, Length(s) + 1);
          CurWidthValue := max(CurWidthValue, CurX);
          CurrentRowHeight := max(CurrentRowHeight, h);
        end;
    end;
  end;

begin
  if BidiMode = bdRightToLeft then
    Flags := DT_RIGHT or DT_RTLREADING or DT_VCENTER
  else
    Flags := DT_VCENTER;

  if BidiMode in [bdRightToLeft, bdRightToLeftNoAlign] then
    Flags := Flags or DT_RTLREADING;

  Bitmap.Canvas.TextFlags := Flags;
  for i := inLastPos to inCurPos - 1 do
    if not acCharIn(Origin[i], DisabledChars) then
      CutString := CutString + Origin[i];

  if CutString <> '' then begin
    Bitmap.Canvas.Brush.Style := bsClear;
    sw := acTextWidth(Bitmap.Canvas, CutString);
    sh := acTextHeight(Bitmap.Canvas, CutString);
    CurrentRowHeight := max(CurrentRowHeight, sh);
    if (Area.Right > 0) and (CurX + sw > Area.Right) then begin
      SpaceWidth := acTextWidth(Bitmap.Canvas, s_Space);
      if CutString[1] = s_Space then
        inc(CurX, SpaceWidth);

      OutLine(CutString);
    end
    else begin
      if not Calculating then
        if AlphaChannel then
          WriteColor(Bitmap, PacChar(CutString), True, Rect(CurX, CurY, CurX + sw, CurY + sh), Flags, Bitmap.Canvas.Font.Color)
        else begin
          R := Rect(CurX, CurY, CurX + sw, CurY + CurrentRowHeight);
          AcDrawText(Bitmap.Canvas.Handle, CutString, R, Flags);
        end;

      inc(CurX, sw);
      CurWidthValue := max(CurWidthValue + sw, CurX);
    end;
  end;
end;

end.
