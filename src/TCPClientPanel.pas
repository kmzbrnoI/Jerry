unit TCPClientPanel;

interface

uses SysUtils, IdTCPClient, ListeningThread, IdTCPConnection, IdGlobal,
     Classes, StrUtils, RPConst, Graphics, Windows, Forms, Controls,
     Generics.Collections;

const
  _DEFAULT_PORT = 5896;

  // tady jsou vyjmenovane vsechny verze protokoluk pripojeni k serveru, ktere klient podporuje
  protocol_version_accept : array[0..0] of string =
    (
      '1.0'
    );

type
  TPanelConnectionStatus = (closed, opening, handshake, opened);

  TPanelTCPClient = class
   private const
    _PROTOCOL_VERSION = '1.0';

   private
    rthread: TReadingThread;
    tcpClient: TIdTCPClient;
    fstatus : TPanelConnectionStatus;
    parsed: TStrings;
    data:string;
    control_disconnect:boolean;       // je true, pokud disconnect plyne ode me
    fauthorised:boolean;
    first_connection:boolean;

     procedure OnTcpClientConnected(Sender: TObject);
     procedure OnTcpClientDisconnected(Sender: TObject);
     procedure DataReceived(const data: string);
     procedure Timeout();   // timeout from socket = broken pipe

     // data se predavaji v Self.Parsed
     procedure ParseLokGlobal();
     procedure ParseGlobal();

   public

     constructor Create();
     destructor Destroy(); override;

     function Connect(host:string; port:Word):Integer;
     function Disconnect():Integer;

     procedure SendLn(str:string);

      property status:TPanelConnectionStatus read fstatus;
      property authorised:boolean read fauthorised;


     procedure LokoPlease(addr:Word; token:string);

  end;//TPanelTCPClient

var
  PanelTCPClient : TPanelTCPClient;

implementation

// specifikace komunikacniho protkolu:
//  jedna se o retezec, ve kterem jsou jednotliva data oddelena strednikem
//  prvni parametr je vzdy id oblasti rizeni, popr. '-' pokud se jedna o rezijni prikaz
// prikazy (pouze pro regulator -- Jerry):

////////////////////////////////////////////////////////////////////////////////
/////////////////////////// KLIENT -> SERVER ///////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
//  -;OR-LIST;                              - pozadavek na ziskani seznamu OR

//
//  -;LOK;G;AUTH;username;passwd            - pozadavek na autorizaci uzivatele
//  -;LOK;G:PLEASE;or_id;comment            - pozadavek na rizeni loko z dane oblasti rizeni
//  -;LOK;G:CANCEL;                         - zruseni pozadavku o loko

//  -:LOK;addr;PLEASE;token                 - zadost o rizeni konkretni lokomotivy; token neni potreba pripojovat v pripade, kdy loko uz mame autorizovane a bylo nam ukradeno napriklad mysi
//  -;LOK;addr;RELEASE                      - uvolneni lokomotivy z rizeni regulatoru
//  -;LOK;addr;SP;sp_km/h                   - nastaveni rychlosti lokomotivy
//  -;LOK;addr;SPD;sp_km/h;dir[0,1]         - nastaveni rychlosti a smeru lokomotivy
//  -;LOK;addr;D;dir[0,1]                   - nastaveni smeru lokomotivy
//  -;LOK;addr;SP-S;sp_stupen[0-28]         - nastaveni rychlosti lokomotivy ve stupnich
//  -;LOK;addr;SPD-S;sp_stupen;dir[0,1]     - nastaveni rychlosti a smeru lokomotivy ve stupnich
//  -;LOK;addr;F;F_left-F_right;states      - nastaveni funkci lokomotivy
//    napr.; or;LOK;F;0-4;00010 nastavi F3 a ostatni F vypne
//  -;LOK;addr;STOP;                        - nouzove zastaveni
//  -;LOK;addr;TOTAL;[0,1]                  - nastaveni totalniho rizeni hnaciho vozidla



////////////////////////////////////////////////////////////////////////////////
/////////////////////////// SERVER -> KLIENT ///////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
//  -;OR-LIST;(or1id,or1name)(or2id, ...    - zaslani seznamu vsech oblasti rizeni

//  -;LOK;G:PLEASE-RESP;[ok, err];info      - odpoved na zadost o lokomotivu z reliefu; v info je pripadna chybova zprava
//  -;LOK;G;AUTH;[ok,not,total];info        - navratove hlaseni o autorizaci
//  -;LOK;addr;AUTH;[ok,not,stolen,release];info;hv_data  - odpoved na pozadavek o autorizaci rizeni hnaciho vozidla (odesilano take jako informace o zruseni ovladani hnacicho vozidla)
//                                        info je string
//                                        hv_data jsou pripojovana k prikazu v pripade, ze doslo uspesne k autorizaci; jedna se o PanelString hnaciho vozdila obsahujici vsechny informace
//  -;LOK;addr;F;F_left-F_right;states      - informace o stavu funkci lokomotivy
//    napr.; or;LOK;0-4;00010 informuje, ze je zaple F3 a F0, F1, F2 a F4 jsou vyple
//  -;LOK;addr;SPD;sp_km/h;sp_stupne;dir    - informace o zmene rychlosti (ci smeru) lokomotivy
//  -;LOK;addr;RESP;[ok, err];info;speed_kmph
//                                          - odpoved na prikaz;  speed_kmph je nepovinny argument; info zpravidla obsahuje rozepsani chyby, pokud je odpoved "ok", info je prazdne
//  -;LOK;addr;TOTAL;[0,1]                  - zmena rucniho rizeni lokomotivy

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

// navazani komunikace:
//  1) klient se pripoji
//  2) klient posle hanshake
//  3) klient vycka na odpoved na handshake
//  4) klient vysila pozadavek na autorizaci oblasti rizeni
//  5) po klientovi muze byt vyzadovano heslo
//  5) klient bud dostane, nebo nedostae pristup k prislusnym OR

// jak funguje rizeni z regulatoru:
//    a) cesta zadosti z regulatoru
//        1) klient se pripoji, autorizuje se vuci serveru prikazem LOK;G;AUTH;
//        2) klient si nacte seznam oblasti rizeni
//        3} klient si vybere oblasti rizeni a do ni posle pozadavek
//        4} oblasti rizeni priradi regulatoru hnaci vozidlo (vozidla)

//    b) cesta primeho prevzeti
//        1) klient se pripoji, autorizuje se vuci serveru prikazem LOK;G;AUTH;
//        2) klient si obstara autorizacni token pro dane hnaci vozidlo (napriklad od dispecera -- resp. z panelu)
//        3) klient pozada o LOKO a prilozi token, loko je mu prirazeno (pozor: token ma omezenou casovou platnost)


uses Main, RegCollector, ORList, GlobalConfig, fAuth, fNewLoko, Debug;

////////////////////////////////////////////////////////////////////////////////

constructor TPanelTCPClient.Create();
begin
 inherited Create();

 Self.fauthorised := false;

 Self.parsed := TStringList.Create;
 Self.first_connection := true;

 Self.tcpClient := TIdTCPClient.Create(nil);
 Self.tcpClient.OnConnected := Self.OnTcpClientConnected;
 Self.tcpClient.OnDisconnected := Self.OnTcpClientDisconnected;
 Self.tcpClient.ConnectTimeout := 1500;

 Self.fstatus := TPanelConnectionStatus.closed;
end;//ctor

destructor TPanelTCPClient.Destroy();
begin
 if (Assigned(Self.tcpClient)) then
   FreeAndNil(Self.tcpClient);

 if (Assigned(Self.parsed)) then
   FreeAndNil(Self.parsed);

 inherited Destroy();
end;//dtor

////////////////////////////////////////////////////////////////////////////////

function TPanelTCPClient.Connect(host:string; port:Word):Integer;
begin
 if (Self.tcpClient.Connected) then Exit(1);

 Self.tcpClient.Host := host;
 Self.tcpClient.Port := port;

 Self.fstatus := TPanelConnectionStatus.opening;

 try
   Self.tcpClient.Connect();
 except
   Self.fstatus := TPanelConnectionStatus.closed;
   raise;
 end;

 Self.tcpClient.IOHandler.DefStringEncoding := TIdEncoding.enUTF8;
 Self.control_disconnect := false;

 Result := 0;
end;//function

////////////////////////////////////////////////////////////////////////////////

function TPanelTCPClient.Disconnect():Integer;
begin
 if (not Self.tcpClient.Connected) then Exit(1);

 Self.control_disconnect := true;
 if Assigned(Self.rthread) then Self.rthread.Terminate;
 try
   Self.tcpClient.Disconnect();
 finally
   if Assigned(Self.rthread) then
   begin
     Self.rthread.WaitFor;
     FreeAndNil(Self.rthread);
   end;
 end;

 Result := 0;
end;//function

////////////////////////////////////////////////////////////////////////////////
// eventy z IdTCPClient

procedure TPanelTCPClient.OnTcpClientConnected(Sender: TObject);
begin
 try
  Self.rthread := TReadingThread.Create((Sender as TIdTCPClient));
  Self.rthread.OnData := DataReceived;
  Self.rthread.OnTimeout := Timeout;
  Self.rthread.Resume;
 except
  (Sender as TIdTCPClient).Disconnect;
  raise;
 end;

 F_Main.A_Connect.Enabled    := false;
 F_Main.A_Disconnect.Enabled := true;
 F_Main.SB_Main.Panels[0].Text := 'Prbíhá handshake...';

 Self.fstatus := TPanelConnectionStatus.handshake;

 // send handshake
 Self.SendLn('-;HELLO;'+Self._PROTOCOL_VERSION+';');
end;//procedure

procedure TPanelTCPClient.OnTcpClientDisconnected(Sender: TObject);
begin
 if Assigned(Self.rthread) then Self.rthread.Terminate;

 Self.fstatus := TPanelConnectionStatus.closed;

 F_Main.A_Connect.Enabled      := true;
 F_Main.A_Disconnect.Enabled   := false;
 F_Main.SB_Main.Panels[0].Text := 'Odpojeno od serveru';

 RegColl.CloseAll();

 if (F_NewLoko.Showing) then
  begin
   F_NewLoko.zadost_probiha := false;
   F_NewLoko.Close();
  end;

 if (F_Main.close_app) then
   F_Main.Close();
end;//procedure

////////////////////////////////////////////////////////////////////////////////

// parsing prijatych dat
procedure TPanelTCPClient.DataReceived(const data: string);
begin
 Self.parsed.Clear();
 ExtractStringsEx([';'], [#13, #10], data, Self.parsed);

 Self.data := data;

 F_Debug.Log('GET: '+data);

 try
   // zakladni rozdeleni parsovani
   if (Self.parsed[0] = '-') then
    begin
     if (Self.parsed[1] = 'LOK') then
      begin
       if (Self.parsed[2] = 'G') then
        Self.ParseLokGlobal()
       else
        RegColl.Parse(parsed);
     end else
       Self.ParseGlobal();
    end;
 except

 end;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TPanelTCPClient.Timeout();
begin
 Self.OnTcpClientDisconnected(Self);
 F_Main.SB_Main.Panels[0].Text := 'Spojení se serverem pøerušeno';
 F_Main.SB_Main.Panels[1].Text := '';
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TPanelTCPClient.ParseGlobal();
var i:Integer;
    found:boolean;
begin
 // parse handhake
 if (Self.parsed[1] = 'HELLO') then
  begin
   // kontrola verze protokolu
   found := false;
   for i := 0 to Length(protocol_version_accept)-1 do
    begin
     if (Self.parsed[2] = protocol_version_accept[i]) then
      begin
       found := true;
       break;
      end;
    end;//for i

   if (not found) then
     Application.MessageBox(PChar('Verze protokolu, kterou požívá server ('+Self.parsed[2]+') není podporována'),
       'Upozornìní', MB_OK OR MB_ICONWARNING);

   Self.fstatus := TPanelConnectionStatus.opened;
   F_Main.SB_Main.Panels[0].Text := 'Pøipojeno k serveru';
   F_Main.SB_Main.Panels[1].Text := 'Probíhá autorizace...';

   Self.SendLn('-;OR-LIST;');

   // Autorizace rizeni.
   if (GlobConfig.data.args.username <> '') then
    // nejprve zkontrolujeme, jestli nam nekdo nepredal username a pasword argumenty
    Self.SendLn('-;LOK;G;AUTH;' + GlobConfig.data.args.username + ';' + GlobConfig.data.args.password)
   else if (GlobConfig.data.auth.username <> '') then
    // pokud neni predano arguemnty, zkontrolujeme, jestli neni ulozeno
    Self.SendLn('-;LOK;G;AUTH;' + GlobConfig.data.auth.username + ';' + GlobConfig.data.auth.password)
   else begin
    // pokud nemame odkud ziskat login, zobrazime okynko
    F_Auth.OpenForm('Autorizujte se');
    Self.SendLn('-;LOK;G;AUTH;' + F_Auth.E_username.Text + ';' + F_Auth.E_Password.Text);
    F_Main.SB_Main.Panels[1].Text := 'Odeslán požadavek na autorizaci...';
   end;
  end

 else if (parsed[1] = 'OR-LIST') then
  begin
   ORDb.Parse(parsed[2]);
   F_NewLoko.FillStanice();
  end;

end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TPanelTCPClient.ParseLokGlobal();
var loko:TLokArgument;
begin
 if (parsed[3] = 'AUTH') then
  begin
   Self.fauthorised := (LowerCase(Self.parsed[4]) = 'ok');
   if (Self.fauthorised) then
    begin
     F_Main.SB_Main.Panels[1].Text := 'Autorizováno';

     if (Self.first_connection) then
      begin
       // prvni pripojeni -> otevreme loko z argumentu
       for loko in GlobConfig.data.args.loks do
         Self.LokoPlease(loko.addr, loko.token);
      end;
     Self.first_connection := false;
    end else begin
     RegColl.CloseAll();
     F_Main.SB_Main.Panels[1].Text := 'NEAUTORIZOVÁNO : '+parsed[5];
    end;
  end else//if parsed[3] = AUTH

 if (parsed[3] = 'PLEASE-RESP') then
  begin
   if (parsed.Count > 5) then
     F_NewLoko.ServerResponse(parsed[4] = 'ok', parsed[5])
   else
     F_NewLoko.ServerResponse(parsed[4] = 'ok', '');
  end;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TPanelTCPClient.SendLn(str:string);
begin
 if (not Self.tcpClient.Connected) then Exit; 

 try
   Self.tcpClient.Socket.WriteLn(str);
 except
   if (Self.fstatus = opened) then
    Self.OnTcpClientDisconnected(Self);
 end;

 F_Debug.Log('SEND: '+str);
end;//procedure

procedure TPanelTCPClient.LokoPlease(addr:Word; token:string);
begin
 Self.SendLn('-;LOK;'+IntToStr(addr)+';PLEASE;'+token);
end;//procedure

////////////////////////////////////////////////////////////////////////////////

initialization
 PanelTCPClient := TPanelTCPClient.Create;

finalization
 FreeAndNil(PanelTCPCLient);

end.//unit
