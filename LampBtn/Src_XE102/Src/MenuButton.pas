unit MenuButton;

interface

uses
  SysUtils, Classes, Controls, ExtCtrls, PngImage, Messages, ComCtrls, Types,
  Graphics;

type
  TMenuColor=(mcGreen,mcYellow,mcRed,mcBlue);
  TMenuType=(mtNumber,mtColor,mtPanel);
  TMenuNumber=(mn1,mn2,mn3,mn4,mn5,mn6);

  TMenuButton = class(TCustomPanel)
  private
   {$IF CompilerVersion >= 21} FSfondo:TPngImage; {$ELSE}
   FSfondo:TPngObject; {$IFEND}
   FMenuType: TMenuType;
   FMenuColor: TMenuColor;
   FSelected: Boolean;
   FMenuNumber: TMenuNumber;
   FAllowMultiSelectInGroup: Boolean;
   FGroup: Cardinal;
   FButtIndex: Integer;
   FTabsheet: TTabsheet;
   FPicture:TPicture;
   Procedure LoadPNG;
   procedure SearchOther;
   procedure OnPictureChange(Sender:TObject);
   procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
   procedure SetMenuType(const Value: TMenuType);
   procedure SetMenuColor(const Value: TMenuColor);
   procedure SetSelected(const Value: Boolean);
   procedure SetMenuNumber(const Value: TMenuNumber);
   procedure SetAllowMultiSelectInGroup(const Value: Boolean);
   procedure SetButtIndex(const Value: Integer);
   procedure SetGroup(const Value: Cardinal);
   procedure SetImage(const Value: TPicture);
  protected
   procedure Paint; Override;
   procedure Click; override;
  public
   Constructor Create(AOwner: TComponent); Override;
   Destructor Destroy; override;
  published
   property Align;
   property Caption;
   property Height;
   property Width;
   property Cursor default crHandPoint;
   property Enabled;
   property Font;
   property OnMouseDown;
   property OnMouseMove;
   property OnMouseUp;
   property OnClick;
   property OnDblClick;
   property OnExit;
   property OnEnter;
   property PopUpMenu;
   property DoubleBuffered;
   property Visible;
   property TabOrder;
   property TabStop;
   property MenuType:TMenuType read FMenuType write SetMenuType default mtNumber;
   property MenuColor:TMenuColor read FMenuColor write SetMenuColor default mcYellow;
   property Selected:Boolean read FSelected write SetSelected;
   property MenuNumber:TMenuNumber read FMenuNumber write SetMenuNumber;
   property Group:Cardinal read FGroup write SetGroup default 0;
   property AllowMultiSelectInGroup:Boolean read FAllowMultiSelectInGroup write SetAllowMultiSelectInGroup default False;
   property Tabsheet:TTabsheet read FTabsheet write FTabsheet;
   property ButtIndex:Integer read FButtIndex write SetButtIndex default -1;
   property Picture:TPicture read FPicture write SetImage;
  end;

procedure Register;

{$R ColorButton.res}
{$R MenuButton.res}

implementation


procedure Register;
begin
RegisterComponents('Termo', [TMenuButton]);
end;

{ TMenuButton }

constructor TMenuButton.Create(AOwner: TComponent);
begin
inherited;
{$IF CompilerVersion >= 21} FSfondo:=TPngImage.Create; {$ELSE}
FSfondo:=TPngObject.Create; {$IFEND}
Cursor:=crHandPoint;
FMenuColor:=mcYellow;
FMenuType:=mtNumber;
FSelected:=False;
FAllowMultiSelectInGroup:=False;
FButtIndex:=-1;
FGroup:=0;
FPicture:=TPicture.Create;
FPicture.OnChange:=OnPictureChange;
FPicture.Bitmap.Transparent:=True;
try LoadPNG; finally end;
Width:=FSfondo.Width;
Height:=FSfondo.Height;
end;

destructor TMenuButton.Destroy;
begin
FPicture.Free;
FSfondo.Destroy;
inherited;
end;

procedure TMenuButton.Click;
begin
if FAllowMultiSelectInGroup then Selected:=NOT FSelected
                            else Selected:=True;
if Assigned(OnClick) then OnClick(Self);
if FSelected then if Tabsheet<>NIL then
   begin
    Tabsheet.Hide;
    Tabsheet.Show;
   end;
end;

procedure TMenuButton.Paint;
var sf:Single;
    ts:TSize;
    i:integer;
begin
canvas.font:=Font;
with canvas do
     begin
      lock;
      StretchDraw(Rect(0,0,Width,Height),FSfondo);
      //-----------------------------------------------------------------Picture
      if NOT FPicture.Graphic.Empty then
         begin
          sf:=(Width-30)/FPicture.Graphic.Width;
          if (FPicture.Graphic.Height*sf)>(Height-18) then sf:=(Height-18)/FPicture.Graphic.Height;
          i:=(Width-Round(Picture.Graphic.Width*sf)) DIV 2;
          if FSelected then
             StretchDraw(Rect(8,3,8+Round(Picture.Graphic.Width*sf),3+Round(FPicture.Graphic.Height*sf)),FPicture.Graphic)
          else
             StretchDraw(Rect(i,3,i+Round(Picture.Graphic.Width*sf),3+Round(FPicture.Graphic.Height*sf)),FPicture.Graphic)
         end;
      //-----------------------------------------------------------------Caption
      Brush.Style:=bsClear;
      ts:=canvas.TextExtent(Caption);
      TextOut((Width-ts.cx) DIV 2,Height-3-ts.cy,Caption);
      unlock;
     end;
end;

procedure TMenuButton.LoadPNG;
VAR Stream: TCustomMemoryStream;
    s:String;
begin
case FMenuType of
 mtNumber:if FSelected then s:='MENU'+Inttostr(Ord(FMenuNumber)+1)
                       else s:='MENUSU';
 mtColor:case FMenuColor of
          mcGreen:if FSelected then s:='VERDEGIU'
                               else s:='VERDE';
          mcYellow:if FSelected then s:='GIALLOGIU'
                               else s:='GIALLO';
          mcRed:if FSelected then s:='ROSSOGIU'
                               else s:='ROSSO';
         end;
 mtPanel:case FMenuColor of
          mcGreen:s:='VERDE';
          mcYellow:s:='GIALLO';
          mcRed:s:='ROSSO';
          mcBlue:s:='MENUSU';
         end; 
end;
//if (NOT Enabled) then s:='PDISABLED';
Stream:=TResourceStream.Create(hInstance,PChar(S),PChar('PNG'));
FSfondo.LoadFromStream(Stream); Stream.Destroy;
end;

procedure TMenuButton.CMEnabledChanged(var Message: TMessage);
begin
try LoadPNG; finally end;
end;

procedure TMenuButton.SetMenuType(const Value: TMenuType);
begin
FMenuType:=Value;
try LoadPNG; finally end;
Invalidate;
end;

procedure TMenuButton.SetMenuColor(const Value: TMenuColor);
begin
FMenuColor:=Value;
try LoadPNG; finally end;
Invalidate;
end;

procedure TMenuButton.SetSelected(const Value: Boolean);
begin
FSelected:=Value;
if FSelected then SearchOther;
try LoadPNG; finally end;
Invalidate;
end;

procedure TMenuButton.SetMenuNumber(const Value: TMenuNumber);
begin
FMenuNumber:=Value;
try LoadPNG; finally end;
Invalidate;
end;

procedure TMenuButton.SetAllowMultiSelectInGroup(const Value: Boolean);
begin
FAllowMultiSelectInGroup:=Value;
Invalidate;
end;

procedure TMenuButton.SetButtIndex(const Value: Integer);
begin
FButtIndex:=Value;
Invalidate;
end;

procedure TMenuButton.SetGroup(const Value: Cardinal);
begin
FGroup:=Value;
Invalidate;
end;

procedure TMenuButton.SearchOther;
Var wc:TWinControl;
    i:integer;
begin
if AllowMultiSelectInGroup then exit;
Wc:=Parent;
if Wc=NIL then exit;
while wc.Parent<>NIL do wc:=wc.Parent;
for i:=0 to wc.ComponentCount-1 do
    if (Wc.Components[i] is TMenuButton) and (Wc.Components[i].Name<>Name) then
        if (Wc.Components[i] AS TMenuButton).Group=FGroup then
           (Wc.Components[i] AS TMenuButton).Selected:=False;
end;

procedure TMenuButton.SetImage(const Value: TPicture);
begin
FPicture.Assign(Value);
Invalidate;
end;

procedure TMenuButton.OnPictureChange(Sender: TObject);
begin
Invalidate;
end;

end.
