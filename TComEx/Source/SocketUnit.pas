unit SocketUnit;

interface

uses SysUtils, Classes, IdTCPServer, IdBaseComponent, IdComponent,
     IdTCPConnection, IdTCPClient;

type TSocketModule = class(TObject)
     private
      { Private declarations }
     public
      IdTCPClient1: TIdTCPClient;
      IdTCPServer1: TIdTCPServer;
      Constructor Create(AOwner:TComponent); Reintroduce;
      Destructor Destroy; Reintroduce;
     end;

implementation

{ TSocketModule }

constructor TSocketModule.Create(AOwner: TComponent);
begin
inherited Create;
IdTCPClient1:=TIdTCPClient.Create(AOwner);
IdTCPClient1.Name:=AOwner.Name+'_SC';
IdTCPServer1:=TIdTCPServer.Create(AOwner);
IdTCPServer1.Name:=AOwner.Name+'_SS';
end;

destructor TSocketModule.Destroy;
begin
try
 IdTCPClient1.Destroy;
 IdTCPServer1.Destroy;
finally
end;
end;

end.
