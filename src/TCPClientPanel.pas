unit TCPClientPanel;

{
  Trida TPanelTCPClient zabezpecuje TCP klient soket, ktery resi komunikaci
  s technologickym serverem.
  Nize je popis komunikacniho protokolu.
}

interface

uses SysUtils, IdTCPClient, ListeningThread, IdTCPConnection, IdGlobal,
     Classes, StrUtils, RPConst, Graphics, Windows, Forms, Controls,
     Generics.Collections, Hash, ExtCtrls;

const
  _DEFAULT_PORT = 5896;
  _PING_TIMER_PERIOD_MS = 20000;

  // tady jsou vyjmenovane vsechny verze protokoluk pripojeni k serveru, ktere klient podporuje
  protocol_version_accept : array[0..1] of string =
    (
      '1.0', '1.1'
    );

type
  TPanelConnectionStatus = (closed, opening, handshake, opened);

  TPanelTCPClient = class
   private const
    _PROTOCOL_VERSION = '1.0';                                                  // verze protokolu od klienta

   private
    rthread: TReadingThread;                                                    // ctecti vlakno (poslouchani probiha v jinem vlakne)
    tcpClient: TIdTCPClient;                                                    // objekt TCP klienta
    fstatus : TPanelConnectionStatus;                                           // aktualni stav klienta, pro pistup pouzivat \status
    parsed: TStrings;                                                           // aktualni naparsovana data, pouze pro vnitrni potrebu objektu pri prijmu dat
    data:string;                                                                // aktualni prijata data v RAW formatu (jeden radek dat)
    control_disconnect:boolean;                                                 // true, pokud se odpojuji od serveru na zaklade pozadavku uzivatele, pri nucenem odpojeni false
    fauthorised:boolean;                                                        // true, pokud strojvedouci autorizovan, pouzivat \authorised
    first_connection:boolean;                                                   // true, pokud aktualni pripojovani je prvni pripojovani (po startu)
    pingTimer:TTimer;

     procedure OnTcpClientConnected(Sender: TObject);                           // event TCP klienta pripojeni k serveru
     procedure OnTcpClientDisconnected(Sender: TObject);                        // event TCP klienta odpojeni od serveru
     procedure DataReceived(const data: string);                                // event prijatych dat od cteciho vlakna
     procedure Timeout();   // timeout from socket = broken pipe                // event timeoutu cteciho vlakna (spojeni se serverem rozvbto)

     // data se predavaji v Self.Parsed
     procedure ParseLokGlobal();                                                // parsing dat s prefixem "-;LOK;"
     procedure ParseGlobal();                                                   // parsing dat s prefixem "-;"

     procedure SendPing(Sedner:TObject);

   public

     constructor Create();
     destructor Destroy(); override;

     function Connect(host:string; port:Word):Integer;                          // pripojit k serveru
     function Disconnect():Integer;                                             // odpojit od serveru

     procedure SendLn(str:string);                                              // poslat zpravu (jeden radek)
     procedure LokoPlease(addr:Word; token:string);                             // zadost o lokomotivu tokenem

      property status:TPanelConnectionStatus read fstatus;                      // aktualni stav pripojeni
      property authorised:boolean read fauthorised;                             //true, pokud strojvedouci autorizovan

  end;//TPanelTCPClient

var
  PanelTCPClient : TPanelTCPClient;

implementation

{
 Specifikace komunikacniho protokolu:
  Jedna se o retezec, ve kterem jsou jednotliva data oddelena strednikem
  prvni parametr je v pripade regulatoru vzdy '-'.
  Komunikace probiha znakovou sadou UTF-8.
  Komunikace JE case-sensitive.


 vynatek z protokolu:
 PRIKAZY PRO REGULATOR:

////////////////////////////////////////////////////////////////////////////////
/////////////////////////// KLIENT -> SERVER ///////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
 -;OR-LIST;                              - pozadavek na ziskani seznamu oblasti rizeni (stanic)


 -;LOK;G;AUTH;username;passwd            - pozadavek na autorizaci uzivatele
 -;LOK;G:PLEASE;or_id;comment            - pozadavek na rizeni loko z dane oblasti rizeni
 -;LOK;G:CANCEL;                         - zruseni pozadavku o loko

 -:LOK;addr;PLEASE;token                 - zadost o rizeni konkretni lokomotivy; token neni potreba pripojovat v pripade, kdy loko uz mame autorizovane a bylo nam ukradeno napriklad mysi
 -;LOK;addr;RELEASE                      - zadost o uvolneni lokomotivy z regulatoru
 -;LOK;addr;SP;sp_km/h                   - nastaveni rychlosti lokomotivy
 -;LOK;addr;SPD;sp_km/h;dir[0,1]         - nastaveni rychlosti a smeru lokomotivy
 -;LOK;addr;D;dir[0,1]                   - nastaveni smeru lokomotivy
 -;LOK;addr;SP-S;sp_stupen[0-28]         - nastaveni rychlosti lokomotivy ve stupnich
 -;LOK;addr;SPD-S;sp_stupen;dir[0,1]     - nastaveni rychlosti a smeru lokomotivy ve stupnich
 -;LOK;addr;F;F_left-F_right;states      - nastaveni funkci lokomotivy
   napr.; or;LOK;F;0-4;00010 nastavi F3 a ostatni F vypne
 -;LOK;addr;STOP;                        - nouzove zastaveni
 -;LOK;addr;TOTAL;[0,1]                  - nastaveni totalniho rucniho rizeni hnaciho vozidla



////////////////////////////////////////////////////////////////////////////////
/////////////////////////// SERVER -> KLIENT ///////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
 -;OR-LIST;[or1id,or1name][or2id, ...    - zaslani seznamu vsech oblasti rizeni; id je unikatni ID, nazev je nazev pro uzivatele
                                           dale v protokolu je pouzivano pouze ID

 -;LOK;G:PLEASE-RESP;[ok, err];info      - odpoved na zadost o lokomotivu z reliefu; v info je pripadna chybova zprava
 -;LOK;G;AUTH;[ok,not,total];info        - navratove hlaseni o autorizaci uzivatele
 -;LOK;addr;AUTH;[ok,not,stolen,release];info;hv_data
                                        - odpoved na pozadavek o autorizaci rizeni hnaciho vozidla (odesilano take jako informace o zruseni ovladani hnacicho vozidla)
                                           info je string
                                           hv_data jsou pripojovana k prikazu v pripade, ze doslo uspesne k autorizaci; jedna se o PanelString hnaciho vozdila obsahujici vsechny informace o nem
 -;LOK;addr;F;F_left-F_right;states      - informace o stavu funkci lokomotivy
   napr.; or;LOK;0-4;00010 informuje, ze je zaple F3 a F0, F1, F2 a F4 jsou vyple
 -;LOK;addr;SPD;sp_km/h;sp_stupne;dir    - informace o zmene rychlosti (ci smeru) lokomotivy
 -;LOK;addr;RESP;[ok, err];info;speed_kmph
                                          - odpoved na prikaz;  speed_kmph je nepovinny argument; info zpravidla obsahuje rozepsani chyby, pokud je odpoved "ok", info je prazdne
 -;LOK;addr;TOTAL;[0,1]                   - zmena rucniho rizeni lokomotivy

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

 navazani komunikace:
  1) klient se pripoji
  2) klient posle hanshake ("-;HELLO;verze_protokolu")
  3) klient vycka na odpoved na handshake
  4) klient posle na server pozadavek o autorizaci obecneho rizeni -- login strojvedouciho a vycka na odpoved
  5) klient nacte seznam oblasti rizeni a nabidne uzivateli vybrat oblast rizeni, do ktere provest zadost o LOKO

 jak funguje rizeni z regulatoru:
    a) cesta zadosti z regulatoru
        1) klient se pripoji, autorizuje se vuci serveru prikazem LOK;G;AUTH;
        2) klient si nacte seznam oblasti rizeni
        3) klient si vybere oblasti rizeni a do ni posle pozadavek
        4) oblasti rizeni priradi regulatoru hnaci vozidlo (vozidla)

    b) cesta primeho prevzeti
        1) klient se pripoji, autorizuje se vuci serveru prikazem LOK;G;AUTH;
        2) klient si obstara autorizacni token pro dane hnaci vozidlo (napriklad od dispecera -- resp. z panelu)
        3) klient pozada o LOKO a prilozi token, loko je mu prirazeno (pozor: token ma omezenou casovou platnost)

}

uses Main, RegCollector, ORList, GlobalConfig, fAuth, fNewLoko, Debug, settings;

////////////////////////////////////////////////////////////////////////////////

constructor TPanelTCPClient.Create();
begin
 inherited;

 // inicializace vlastnosti a objetku
 Self.fauthorised := false;
 Self.parsed := TStringList.Create;
 Self.first_connection := true;

 Self.pingTimer := TTimer.Create(nil);
 Self.pingTimer.Enabled := false;
 Self.pingTimer.Interval := _PING_TIMER_PERIOD_MS;
 Self.pingTimer.OnTimer := Self.SendPing;

 // vytvoreni TCP klienta
 Self.tcpClient := TIdTCPClient.Create(nil);
 Self.tcpClient.OnConnected := Self.OnTcpClientConnected;
 Self.tcpClient.OnDisconnected := Self.OnTcpClientDisconnected;
 Self.tcpClient.ConnectTimeout := 1500;

 // aktualni status = zavrene spojeni
 Self.fstatus := TPanelConnectionStatus.closed;
end;//ctor

destructor TPanelTCPClient.Destroy();
begin
 try
   if (Assigned(Self.tcpClient)) then
     FreeAndNil(Self.tcpClient);

   if (Assigned(Self.parsed)) then
     FreeAndNil(Self.parsed);

   Self.pingTimer.Free();
 finally
   inherited;
 end;
end;//dtor

////////////////////////////////////////////////////////////////////////////////

function TPanelTCPClient.Connect(host:string; port:Word):Integer;
begin
 try
   if (Self.tcpClient.Connected) then Exit(1);
 except
   try
     Self.tcpClient.Disconnect(False);
   except
   end;
   if (Self.tcpClient.IOHandler <> nil) then Self.tcpClient.IOHandler.InputBuffer.Clear;
 end;

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
end;

////////////////////////////////////////////////////////////////////////////////

function TPanelTCPClient.Disconnect():Integer;
begin
 try
   if (not Self.tcpClient.Connected) then Exit(1);
 except

 end;

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
end;

////////////////////////////////////////////////////////////////////////////////
// eventy z IdTCPClient

procedure TPanelTCPClient.OnTcpClientConnected(Sender: TObject);
begin
 // klient pripojen -> vytvorime cteci vlakno
 try
  Self.rthread := TReadingThread.Create(TIdTCPClient(Sender));
  Self.rthread.OnData := DataReceived;
  Self.rthread.OnTimeout := Timeout;
  Self.rthread.Resume;
 except
  (Sender as TIdTCPClient).Disconnect;
  raise;
 end;

 // update okynka
 F_Main.A_Connect.Enabled    := false;
 F_Main.A_Disconnect.Enabled := true;
 F_Main.SB_Main.Panels[0].Text := 'Prob�h� handshake...';

 Self.fstatus := TPanelConnectionStatus.handshake;
 Self.pingTimer.Enabled := true;

 // odeslat handshake
 Self.SendLn('-;HELLO;'+Self._PROTOCOL_VERSION+';');
end;

procedure TPanelTCPClient.OnTcpClientDisconnected(Sender: TObject);
begin
 // klient odpojen -> znicit cteci vlakno
 if Assigned(Self.rthread) then Self.rthread.Terminate;

 // status klienta na odpojen
 Self.fstatus := TPanelConnectionStatus.closed;
 Self.pingTimer.Enabled := false;

 // aktualizace okynka
 F_Main.A_Connect.Enabled      := true;
 F_Main.A_Disconnect.Enabled   := false;
 F_Main.SB_Main.Panels[0].Text := 'Odpojeno od serveru';

 // zavrit vsechny regulatory
 RegColl.CloseAll();

 // zavrit okynko zadosti
 if (F_NewLoko.Showing) then
  begin
   F_NewLoko.zadost_probiha := false;
   F_NewLoko.Close();
  end;

 // flag ukoncovani aplikace
 // zavreni aplikace totiz ve skutecnosti nezavre aplikaci, ale nastavi
 //   F_Main.close_app na true a TCPclient.DIsconnect
 // po odpojeni od serveru se tak aplikace zavre
 if (F_Main.close_app) then
   F_Main.Close();
end;

////////////////////////////////////////////////////////////////////////////////

// parsing prijatych dat
procedure TPanelTCPClient.DataReceived(const data: string);
begin
 Self.parsed.Clear();

 // vlastni parsovaci funkce
 ExtractStringsEx([';'], [#13, #10], data, Self.parsed);

 Self.data := data;

 // logovani dat
 F_Debug.Log('GET: '+data);

 try
   // zakladni rozdeleni podle prefixu
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
end;

////////////////////////////////////////////////////////////////////////////////

procedure TPanelTCPClient.Timeout();
begin
 Self.OnTcpClientDisconnected(Self);
 F_Main.SB_Main.Panels[0].Text := 'Spojen� se serverem p�eru�eno';
 F_Main.SB_Main.Panels[1].Text := '';
end;

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
     Application.MessageBox(PChar('Verze protokolu, kterou po��v� server ('+Self.parsed[2]+') nen� podporov�na'),
       'Upozorn�n�', MB_OK OR MB_ICONWARNING);

   Self.fstatus := TPanelConnectionStatus.opened;
   F_Main.SB_Main.Panels[0].Text := 'P�ipojeno k serveru';
   F_Main.SB_Main.Panels[1].Text := 'Prob�h� autorizace...';

   // ziskame seznam oblasti rizeni (to muzeme i bez autorizace)
   Self.SendLn('-;OR-LIST;');

   // Autorizace strojvedouciho
   if (GlobConfig.data.args.username <> TGlobConfig._DEFAULT_ARGS.username) then
    // nejprve zkontrolujeme, jestli nam nekdo nepredal username a pasword argumenty
    Self.SendLn('-;LOK;G;AUTH;' + GlobConfig.data.args.username + ';' + GlobConfig.data.args.password)
   else if (GlobConfig.data.auth.username <> '') then
    // pokud neni predano arguemnty, zkontrolujeme, jestli neni ulozeno
    Self.SendLn('-;LOK;G;AUTH;' + GlobConfig.data.auth.username + ';' + GlobConfig.data.auth.password)
   else begin
    // pokud nemame odkud ziskat login, zobrazime okynko
    F_Auth.OpenForm('Autorizujte se');
    Self.SendLn('-;LOK;G;AUTH;' + F_Auth.E_username.Text + ';' + GenerateHash(AnsiString(F_Auth.E_Password.Text)));
    F_Main.SB_Main.Panels[1].Text := 'Odesl�n po�adavek na autorizaci...';
   end;
  end

 else if (parsed[1] = 'OR-LIST') then
  begin
   ORDb.Parse(parsed[2]);
   F_NewLoko.FillStanice();
  end;

end;

////////////////////////////////////////////////////////////////////////////////

procedure TPanelTCPClient.ParseLokGlobal();
var loko:TLokArgument;
begin
 if (parsed[3] = 'AUTH') then
  begin
   // autorizace konkretni lokomotivy
   Self.fauthorised := (LowerCase(Self.parsed[4]) = 'ok');
   if (Self.fauthorised) then
    begin
     F_Main.SB_Main.Panels[1].Text := 'Autorizov�no';

     if (Self.first_connection) then
      begin
       // prvni pripojeni -> otevreme loko z argumentu
       if (GlobConfig.data.args.loks.Count > 0) then
        begin
         for loko in GlobConfig.data.args.loks do
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
     F_Main.SB_Main.Panels[1].Text := 'NEAUTORIZOV�NO : '+parsed[5];
    end;
  end else//if parsed[3] = AUTH

 if (parsed[3] = 'PLEASE-RESP') then
  begin
   if (parsed.Count > 5) then
     F_NewLoko.ServerResponse(parsed[4] = 'ok', parsed[5])
   else
     F_NewLoko.ServerResponse(parsed[4] = 'ok', '');
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TPanelTCPClient.SendLn(str:string);
begin
 try
   if (not Self.tcpClient.Connected) then Exit;
 except

 end;

 try
   Self.tcpClient.Socket.WriteLn(str);
 except
   if (Self.fstatus = opened) then
    Self.OnTcpClientDisconnected(Self);
 end;

 F_Debug.Log('SEND: '+str);
end;

procedure TPanelTCPClient.LokoPlease(addr:Word; token:string);
begin
 Self.SendLn('-;LOK;'+IntToStr(addr)+';PLEASE;'+token);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TPanelTCPClient.SendPing(Sedner:TObject);
begin
 try
   if (Self.tcpClient.Connected) then
     Self.SendLn('-;PING');
 except

 end;
end;

////////////////////////////////////////////////////////////////////////////////

initialization
 PanelTCPClient := TPanelTCPClient.Create;

finalization
 FreeAndNil(PanelTCPCLient);

end.//unit
