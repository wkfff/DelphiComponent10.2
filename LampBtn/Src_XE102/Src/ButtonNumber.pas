unit ButtonNumber;

interface

uses
  Messages, SysUtils, Classes, ExtCtrls, Graphics, Controls, ComCtrls;

type
  TNumber=(n1,n2,n3,n4,n5,n6,n7,n8);

  TButtonNumber = class(TCustomPanel)
  private
   FNumber: TNumber;
   //          enable               selected
   FNBmp:array[False..True,TNumber,False..True] of TBitmap;
   FSelected: Boolean;
   FGroup: Cardinal;
   FAllowMultiSelectInGroup: Boolean;
   FTabsheet:TTabsheet;
   FButtIndex: Integer;
   procedure SetNumber(const Value: TNumber);
   Procedure LoadBmp;
   procedure SetSelected(const Value: Boolean);
   procedure SetGroup(const Value: Cardinal);
   procedure SetAllowMultiSelectInGroup(const Value: Boolean);
   procedure SearchOther;
   procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
   procedure SetButtIndex(const Value: Integer);
  protected
   procedure Paint; Override;
   procedure Click; Override;
  public
   Constructor Create(AOwner: TComponent); override;
   Destructor Destroy; override;
  published
   property Align;
   property Height;
   property Width;
   property OnClick;
   property OnMouseDown;
   property OnMouseUp;
   property ShowHint;
   property Hint;
   property Enabled;   
   property Number:TNumber read FNumber write SetNumber default n1;
   property Selected:Boolean read FSelected write SetSelected default False;
   property Group:Cardinal read FGroup write SetGroup default 0;
   property AllowMultiSelectInGroup:Boolean read FAllowMultiSelectInGroup write SetAllowMultiSelectInGroup default False;
   property Tabsheet:TTabsheet read FTabsheet write FTabsheet;
   property ButtIndex:Integer read FButtIndex write SetButtIndex default -1;
  end;

procedure Register;

{$R ButtonNumber.res}

implementation

procedure Register;
begin
  RegisterComponents('Termo', [TButtonNumber]);
end;

{ TButtonNumber }

constructor TButtonNumber.Create(AOwner: TComponent);
var b1,b2:Boolean; F:TNumber;
begin
inherited;
for b1:=False to True do
    for F:=Low(TNumber) to High(TNumber) do
        for b2:=False to True do
            FNBmp[b1,f,b2]:=TBitmap.Create;
Caption:='';
FNumber:=n1;
FSelected:=False;
FAllowMultiSelectInGroup:=False;
FGroup:=0;
LoadBMP;
Height:=64;
Width:=64;
FButtIndex:=-1;
end;

destructor TButtonNumber.Destroy;
var b1,b2:Boolean; F:TNumber;
begin
for b1:=False to True do
    for F:=Low(TNumber) to High(TNumber) do
        for b2:=False to True do
            FNBmp[b1,f,b2].Destroy;
inherited;
end;

procedure TButtonNumber.LoadBmp;
Var b2:Boolean; F:TNumber;
begin
try
 for F:=Low(TNumber) to High(TNumber) do
     for b2:=False to True do
         FNBmp[False,F,b2].LoadFromResourceID(hInstance,51);
// FNBmp[False,n1,false].LoadFromResourceID(hInstance,51);
 FNBmp[True,n1,false].LoadFromResourceID(hInstance,52);
 FNBmp[True,n1,True].LoadFromResourceID(hInstance,53);
 FNBmp[True,n2,false].LoadFromResourceID(hInstance,54);
 FNBmp[True,n2,True].LoadFromResourceID(hInstance,55);
 FNBmp[True,n3,false].LoadFromResourceID(hInstance,56);
 FNBmp[True,n3,True].LoadFromResourceID(hInstance,57);
 FNBmp[True,n4,false].LoadFromResourceID(hInstance,58);
 FNBmp[True,n4,True].LoadFromResourceID(hInstance,59);
 FNBmp[True,n5,false].LoadFromResourceID(hInstance,60);
 FNBmp[True,n5,True].LoadFromResourceID(hInstance,61);
 FNBmp[True,n6,false].LoadFromResourceID(hInstance,62);
 FNBmp[True,n6,True].LoadFromResourceID(hInstance,63);
 FNBmp[True,n7,false].LoadFromResourceID(hInstance,64);
 FNBmp[True,n7,True].LoadFromResourceID(hInstance,65);
 FNBmp[True,n8,false].LoadFromResourceID(hInstance,66);
 FNBmp[True,n8,True].LoadFromResourceID(hInstance,67);
finally
end;
end;

procedure TButtonNumber.Click;
begin
if FAllowMultiSelectInGroup then Selected:=NOT FSelected
                            else Selected:=True;
if Assigned(OnClick) then OnClick(Self);
if FSelected then if Tabsheet<>NIL then
   begin
    //Tabsheet.Hide;
    Tabsheet.Show; //
   end;
end;

procedure TButtonNumber.Paint;
begin
with canvas do
     begin
      Lock;
      StretchDraw(Rect(0,0,Width,Height),FNBMP[Enabled,FNumber,FSelected]);
      Unlock;
     end;
end;

procedure TButtonNumber.SetAllowMultiSelectInGroup(const Value: Boolean);
begin
FAllowMultiSelectInGroup:=Value;
invalidate;
end;

procedure TButtonNumber.SetGroup(const Value: Cardinal);
begin
FGroup:=Value;
invalidate;
end;

procedure TButtonNumber.SetNumber(const Value: TNumber);
begin
FNumber:=Value;
if FButtIndex=-1 then FButtIndex:=ord(FNumber)+1;
invalidate;
end;

procedure TButtonNumber.SetSelected(const Value: Boolean);
begin
FSelected:=Value;
if FSelected then SearchOther;
invalidate;
end;

procedure TButtonNumber.SearchOther;
Var wc:TWinControl;
    i:integer;
begin
if AllowMultiSelectInGroup then exit;
Wc:=Parent;
if Wc=NIL then exit;
while wc.Parent<>NIL do wc:=wc.Parent;
for i:=0 to wc.ComponentCount-1 do
    if (Wc.Components[i] is TButtonNumber) and (Wc.Components[i].Name<>Name) then
        if (Wc.Components[i] AS TButtonNumber).Group=FGroup then
           (Wc.Components[i] AS TButtonNumber).Selected:=False;
end;

procedure TButtonNumber.SetButtIndex(const Value: Integer);
begin
FButtIndex:=Value;
end;

procedure TButtonNumber.CMEnabledChanged(var Message: TMessage);
begin
inherited;
end;

end.
