unit TCPClientPanel;

{
  TCP klient resici komunikaci s hJOPserverem.

  Kompletni specifikace komunikacnho protkolu je popsana na
  https://github.com/kmzbrnoI/hJOPserver/wiki/panelServer.
}

interface

uses SysUtils, IdTCPClient, ListeningThread, IdTCPConnection, IdGlobal,
  Classes, StrUtils, Graphics, Windows, Forms, Controls, ownStrUtils,
  Generics.Collections, Hash, ExtCtrls;

const
  _DEFAULT_PORT = 5896;
  _PING_TIMER_PERIOD_MS = 5000;

  // tady jsou vyjmenovane vsechny verze protokolu k pripojeni k serveru, ktere klient podporuje
  protocol_version_accept: array [0 .. 1] of string = ('1.0', '1.1');

type
  TPanelConnectionStatus = (closed, opening, handshake, opened);

  TPanelTCPClient = class
  private const
    _PROTOCOL_VERSION_CLIENT = '1.1';

  private
    rthread: TReadingThread;
    tcpClient: TIdTCPClient;
    fstatus: TPanelConnectionStatus;
    parsed: TStrings;
    data: string;
    control_disconnect: boolean; // je true, pokud disconnect plyne ode me
    pingTimer: TTimer;
    mServerVersion: Integer;
    fauthorised: boolean; // true, pokud strojvedouci autorizovan, pouzivat \authorised
    first_connection: boolean; // true, pokud aktualni pripojovani je prvni pripojovani (po startu)

    procedure OnTcpClientConnected(Sender: TObject);
    procedure OnTcpClientDisconnected(Sender: TObject);
    procedure DataReceived(const data: string);
    procedure DataErrorGlobalHandle();
    procedure DataError(); // timeout from socket = broken pipe
    procedure DataTimeout();

    // data se predavaji v Self.Parsed
    procedure ParseLokGlobal(); // parsing dat s prefixem "-;LOK;"
    procedure ParseGlobal(); // parsing dat s prefixem "-;"

    procedure SendPing(Sedner: TObject);
    function GetServerVersionStr(): string;

  public

    constructor Create();
    destructor Destroy(); override;

    function Connect(host: string; port: Word): Integer;
    function Disconnect(): Integer;

    procedure SendLn(str: string);
    procedure LokoPlease(addr: Word; token: string); // zadost o lokomotivu tokenem

    class function VersionToInt(version: string): Integer;
    class function VersionToString(version: Integer): string;
    function IsServerVersionAtLeast(version: string): boolean;

    property status: TPanelConnectionStatus read fstatus;
    property serverVersionStr: string read GetServerVersionStr;
    property serverVersionInt: Integer read mServerVersion;

    property authorised: boolean read fauthorised; // true, pokud je strojvedouci autorizovan

  end; // TPanelTCPClient

var
  PanelTCPClient: TPanelTCPClient;

implementation

uses Main, RegCollector, ORList, GlobalConfig, fAuth, fNewLoko, fDebug,
  fSettings;

/// /////////////////////////////////////////////////////////////////////////////

constructor TPanelTCPClient.Create();
begin
  inherited;

  Self.fauthorised := false;
  Self.parsed := TStringList.Create();
  Self.first_connection := true;
  Self.mServerVersion := 0;

  Self.pingTimer := TTimer.Create(nil);
  Self.pingTimer.Enabled := false;
  Self.pingTimer.Interval := _PING_TIMER_PERIOD_MS;
  Self.pingTimer.OnTimer := Self.SendPing;

  Self.tcpClient := TIdTCPClient.Create(nil);
  Self.tcpClient.OnConnected := Self.OnTcpClientConnected;
  Self.tcpClient.OnDisconnected := Self.OnTcpClientDisconnected;
  Self.tcpClient.ConnectTimeout := 1500;

  Self.fstatus := TPanelConnectionStatus.closed;
end;

destructor TPanelTCPClient.Destroy();
begin
  try
    if (Self.tcpClient.Connected) then
      Self.tcpClient.Disconnect();
  except

  end;

  try
    if (Assigned(Self.tcpClient)) then
      FreeAndNil(Self.tcpClient);

    if (Assigned(Self.parsed)) then
      FreeAndNil(Self.parsed);

    Self.pingTimer.Free();
  finally
    inherited;
  end;
end; // dtor

/// /////////////////////////////////////////////////////////////////////////////

function TPanelTCPClient.Connect(host: string; port: Word): Integer;
begin
  try
    // without .Clear() .Connected() sometimes returns true when actually not connected
    if (Self.tcpClient.IOHandler <> nil) then
      Self.tcpClient.IOHandler.InputBuffer.Clear();
    if (Self.tcpClient.Connected) then
      Exit(1);
  except
    try
      Self.tcpClient.Disconnect(false);
    except
    end;
    if (Self.tcpClient.IOHandler <> nil) then
      Self.tcpClient.IOHandler.InputBuffer.Clear();
  end;

  Self.tcpClient.host := host;
  Self.tcpClient.port := port;

  Self.fstatus := TPanelConnectionStatus.opening;

  try
    Self.tcpClient.Connect();
  except
    Self.fstatus := TPanelConnectionStatus.closed;
    raise;
  end;

  Self.tcpClient.IOHandler.DefStringEncoding := IndyTextEncoding_UTF8();
  Self.tcpClient.IOHandler.MaxLineLength := 16777215;
  Self.control_disconnect := false;

  Result := 0;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TPanelTCPClient.Disconnect(): Integer;
begin
  try
    if (not Self.tcpClient.Connected) then
      Exit(1);
  except

  end;

  Self.control_disconnect := true;
  if Assigned(Self.rthread) then
    Self.rthread.Terminate();
  try
    Self.tcpClient.Disconnect();
  finally
    if Assigned(Self.rthread) then
    begin
      Self.rthread.WaitFor();
      FreeAndNil(Self.rthread);
    end;
  end;

  Result := 0;
end;

/// /////////////////////////////////////////////////////////////////////////////
// eventy z IdTCPClient

procedure TPanelTCPClient.OnTcpClientConnected(Sender: TObject);
begin
  try
    Self.rthread := TReadingThread.Create((Sender as TIdTCPClient));
    Self.rthread.OnData := DataReceived;
    Self.rthread.OnError := DataError;
    Self.rthread.OnTimeout := DataTimeout;
    Self.rthread.Start();
  except
    (Sender as TIdTCPClient).Disconnect;
    raise;
  end;

  F_Main.A_Connect.Enabled := false;
  F_Main.A_Disconnect.Enabled := true;
  F_Main.SB_Main.Panels[0].Text := 'Probíhá handshake...';

  Self.fstatus := TPanelConnectionStatus.handshake;
  Self.pingTimer.Enabled := true;

  // send handshake
  Self.SendLn('-;HELLO;' + Self._PROTOCOL_VERSION_CLIENT + ';');
end;

procedure TPanelTCPClient.OnTcpClientDisconnected(Sender: TObject);
begin
  if Assigned(Self.rthread) then
    Self.rthread.Terminate();

  Self.fstatus := TPanelConnectionStatus.closed;
  Self.pingTimer.Enabled := false;

  F_Main.A_Connect.Enabled := true;
  F_Main.A_Disconnect.Enabled := false;
  F_Main.SB_Main.Panels[0].Text := 'Odpojeno od serveru';

  RegColl.CloseAll();
  if (F_NewLoko.Showing) then
  begin
    F_NewLoko.zadost_probiha := false;
    F_NewLoko.Close();
  end;

  // flag ukoncovani aplikace
  // zavreni aplikace totiz ve skutecnosti nezavre aplikaci, ale nastavi
  // F_Main.close_app na true a TCPclient.DIsconnect
  // po odpojeni od serveru se tak aplikace zavre
  if (F_Main.close_app) then
    F_Main.Close();
end;

/// /////////////////////////////////////////////////////////////////////////////

// parsing prijatych dat
procedure TPanelTCPClient.DataReceived(const data: string);
begin
  Self.parsed.Clear();
  ExtractStringsEx([';'], [#13, #10], data, Self.parsed);

  Self.data := data;

  F_Debug.Log('GET: ' + data);

  try
    if (Self.parsed.Count < 2) then
      Exit();
    Self.parsed[1] := UpperCase(Self.parsed[1]);

    // zakladni rozdeleni podle prefixu
    if (Self.parsed[0] = '-') then
    begin
      if (Self.parsed[1] = 'LOK') then
      begin
        if (Self.parsed[2] = 'G') then
          Self.ParseLokGlobal()
        else
          RegColl.Parse(parsed);
      end
      else
        Self.ParseGlobal();
    end;
  except
    try
      Self.tcpClient.Disconnect();
    except
      try
        Self.OnTcpClientDisconnected(Self);
      except

      end;
    end;
  end;
end;

procedure TPanelTCPClient.DataErrorGlobalHandle();
begin
  F_Main.SB_Main.Panels[1].Text := '';

  try
    if (Self.tcpClient.Connected) then
      Self.tcpClient.Disconnect()
    else
      Self.OnTcpClientDisconnected(Self);
  except
    try
      Self.tcpClient.Disconnect();
    except
      try
        Self.OnTcpClientDisconnected(Self);
      except

      end;
    end;
  end;
end;

procedure TPanelTCPClient.DataError();
begin
  Self.DataErrorGlobalHandle();
  F_Main.SB_Main.Panels[0].Text := 'Výjimka čtení socketu!';
end;

procedure TPanelTCPClient.DataTimeout();
begin
  Self.DataErrorGlobalHandle();
  F_Main.SB_Main.Panels[0].Text := 'Spojení se serverem přerušeno!';
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TPanelTCPClient.ParseGlobal();
begin
  // parse handhake
  if (Self.parsed[1] = 'HELLO') then
  begin
    // kontrola verze protokolu
    var found: Boolean := false;
    for var i := 0 to Length(protocol_version_accept) - 1 do
    begin
      if (Self.parsed[2] = protocol_version_accept[i]) then
      begin
        found := true;
        break;
      end;
    end;

    if (not found) then
      Application.MessageBox(PChar('Verze protokolu, kterou používá server (' +
        Self.parsed[2] + ') není podporována'), 'Upozornění',
        MB_OK OR MB_ICONWARNING);

    Self.mServerVersion := Self.VersionToInt(Self.parsed[2]);
    Self.fstatus := TPanelConnectionStatus.opened;
    F_Main.SB_Main.Panels[0].Text := 'Připojeno k serveru';
    F_Main.SB_Main.Panels[1].Text := 'Probíhá autorizace...';

    // ziskame seznam oblasti rizeni (to muzeme i bez autorizace)
    Self.SendLn('-;OR-LIST;');

    // Autorizace strojvedouciho
    if (GlobConfig.data.args.username <> TGlobConfig._DEFAULT_ARGS.username)
    then
      // nejprve zkontrolujeme, jestli nam nekdo nepredal username a pasword argumenty
      Self.SendLn('-;LOK;G;AUTH;' + GlobConfig.data.args.username + ';' +
        GlobConfig.data.args.password)
    else if (GlobConfig.data.auth.username <> '') then
      // pokud neni predano arguemnty, zkontrolujeme, jestli neni ulozeno
      Self.SendLn('-;LOK;G;AUTH;' + GlobConfig.data.auth.username + ';' +
        GlobConfig.data.auth.password)
    else
    begin
      // pokud nemame odkud ziskat login, zobrazime okynko
      F_Auth.OpenForm('Autorizujte se');
      Self.SendLn('-;LOK;G;AUTH;' + F_Auth.E_username.Text + ';' +
        pwdHash(F_Auth.E_Password.Text));
      F_Main.SB_Main.Panels[1].Text := 'Odeslán požadavek na autorizaci...';
    end;

  end
  else if ((parsed[1] = 'PING') and (parsed.Count > 2) and
    (UpperCase(parsed[2]) = 'REQ-RESP')) then
  begin
    if (parsed.Count >= 4) then
      Self.SendLn('-;PONG;' + parsed[3])
    else
      Self.SendLn('-;PONG');
  end

  else if (parsed[1] = 'OR-LIST') then
  begin
    ORDb.Parse(parsed[2]);
    F_NewLoko.FillStanice();
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TPanelTCPClient.ParseLokGlobal();
begin
  if (parsed.Count < 4) then
    Exit();
  parsed[3] := UpperCase(parsed[3]);

  if (parsed[3] = 'AUTH') then
  begin
    // autorizace konkretni lokomotivy
    Self.fauthorised := (LowerCase(Self.parsed[4]) = 'ok');
    if (Self.fauthorised) then
    begin
      F_Main.SB_Main.Panels[1].Text := 'Autorizováno';

      if (Self.first_connection) then
      begin
        // prvni pripojeni -> otevreme loko z argumentu
        if (GlobConfig.data.args.loks.Count > 0) then
        begin
          for var loko: TLokArgument in GlobConfig.data.args.loks do
            Self.LokoPlease(loko.addr, loko.token);
        end else begin
          F_Settings.Close();
          F_NewLoko.Show();
        end;
      end else begin
        F_Settings.Close();
        F_NewLoko.Show();
      end;
      Self.first_connection := false;
    end else begin
      RegColl.CloseAll();
      F_Main.SB_Main.Panels[1].Text := 'NEAUTORIZOVÁNO : ' + parsed[5];
    end;

  end else begin // if parsed[3] = AUTH
    if (parsed[3] = 'PLEASE-RESP') then
    begin
      if (parsed.Count > 5) then
        F_NewLoko.ServerResponse(parsed[4] = 'ok', parsed[5])
      else
        F_NewLoko.ServerResponse(parsed[4] = 'ok', '');
    end;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TPanelTCPClient.SendLn(str: string);
begin
  try
    if (not Self.tcpClient.Connected) then
      Exit;
  except

  end;

  try
    Self.tcpClient.Socket.WriteLn(str);
  except
    if (Self.fstatus = opened) then
      Self.OnTcpClientDisconnected(Self);
  end;

  F_Debug.Log('SEND: ' + str);
end;

procedure TPanelTCPClient.LokoPlease(addr: Word; token: string);
begin
  Self.SendLn('-;LOK;' + IntToStr(addr) + ';PLEASE;' + token);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TPanelTCPClient.SendPing(Sedner: TObject);
begin
  try
    if (Self.tcpClient.Connected) then
      Self.SendLn('-;PING');
  except

  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

class function TPanelTCPClient.VersionToInt(version: string): Integer;
var strs: TStrings;
begin
  strs := TStringList.Create();
  try
    ExtractStringsEx(['.'], [], version, strs);
    Result := StrToInt(strs[0]) * 1000 + StrToInt(strs[1]);
  finally
    strs.Free();
  end;
end;

function TPanelTCPClient.IsServerVersionAtLeast(version: string): boolean;
begin
  Result := (Self.serverVersionInt >= Self.VersionToInt(version));
end;

class function TPanelTCPClient.VersionToString(version: Integer): string;
begin
  Result := IntToStr(version div 1000) + '.' + IntToStr(version mod 1000);
end;

function TPanelTCPClient.GetServerVersionStr(): string;
begin
  Result := Self.VersionToString(Self.serverVersionInt);
end;

/// /////////////////////////////////////////////////////////////////////////////

initialization

PanelTCPClient := TPanelTCPClient.Create;

finalization

FreeAndNil(PanelTCPClient);

end.// unit
