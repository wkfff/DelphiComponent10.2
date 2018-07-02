unit acDesignData;

interface

uses
  Forms, SysUtils, Classes, ImgList, Controls, acAlphaImageList;

type
  TacDM = class(TDataModule)
    sCharImageList1: TsCharImageList;
  public
  end;

var
  acDM: TacDM;

implementation

{$R *.dfm}


initialization

  acDM := TacDM.Create(nil);

finalization

  FreeAndNil(acDM);

end.
