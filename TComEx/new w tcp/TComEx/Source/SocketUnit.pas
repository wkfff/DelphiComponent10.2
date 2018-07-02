unit SocketUnit;

interface

uses SysUtils, Classes, IdBaseComponent, IdComponent, ScktComp, StdCtrls;

type TSocketModule = class(TObject)
     private
      FErrorCode: Integer;
      FOnConnected,FOnDisconnected,FOnSocketError: TNotifyEvent;
      { Private declarations }
      procedure OnClientConnect1(Sender: TObject; Socket: TCustomWinSocket);
      procedure OnClientDisconnect1(Sender: TObject; Socket: TCustomWinSocket);
      procedure OnError1(Sender: TObject; Socket: TCustomWinSocket; ErrorEvent: TErrorEvent; var ErrorCode: Integer);
     public
      ClientSocket1: TClientSocket;
      ServerSocket1: TServerSocket;
      SocketStream1: TWinSocketStream;

      property OnConnected: TNotifyEvent read FOnConnected write FOnConnected;
      property OnDisconnected: TNotifyEvent read FOnDisconnected write FOnDisconnected;
      property OnSocketError: TNotifyEvent read FOnSocketError write FOnSocketError;
      property ErrorCode: Integer read FErrorCode;
      Constructor Create(AOwner:TComponent); Reintroduce;
      Destructor Destroy; Reintroduce;
     end;

implementation

{ TSocketModule }

constructor TSocketModule.Create(AOwner: TComponent);
begin
inherited Create;
ClientSocket1:=TClientSocket.Create(AOwner);
ClientSocket1.Name:=AOwner.Name+'_SC';
ClientSocket1.OnConnect := OnClientConnect1;
ClientSocket1.OnDisconnect := OnClientDisconnect1;
ClientSocket1.OnError := OnError1;
ClientSocket1.ClientType := ctNonBlocking;

ServerSocket1:=TServerSocket.Create(AOwner);
ServerSocket1.Name:=AOwner.Name+'_SS';
ServerSocket1.OnClientConnect := OnClientConnect1;
ServerSocket1.OnClientDisconnect := OnClientDisconnect1;
ServerSocket1.OnClientError := OnError1;
ServerSocket1.ServerType := stNonBlocking;

FOnConnected := NIL;
FOnDisconnected := NIL;
SocketStream1 := NIL;
end;

destructor TSocketModule.Destroy;
begin
try
 ClientSocket1.Destroy;
 ServerSocket1.Destroy;
 SocketStream1.Destroy;
 if SocketStream1 <> NIL then
   SocketStream1.Destroy;
finally
end;
end;

procedure TSocketModule.OnClientConnect1(Sender: TObject; Socket: TCustomWinSocket);
begin
  Socket.AsyncStyles := [];
  SocketStream1 := TWinSocketStream.Create(Socket,6000); //Timout sei secondi di default.
  if Assigned(FOnConnected) then
    FOnConnected(Self);
end;

procedure TSocketModule.OnClientDisconnect1(Sender: TObject; Socket: TCustomWinSocket);
begin
  if SocketStream1 <> NIL then
    begin
      if Assigned(FOnDisconnected) then
        FOnDisconnected(Self);
      SocketStream1.Destroy;
      SocketStream1 := NIL;
    end;
end;

procedure TSocketModule.OnError1(Sender: TObject; Socket: TCustomWinSocket; ErrorEvent: TErrorEvent; var ErrorCode: Integer);
begin
  if Assigned(FOnSocketError) then
    begin
      FErrorCode:=ErrorCode;
      FOnSocketError(Self);
      ErrorCode:=0;
    end;
end;

end.
