// JCL_DEBUG_EXPERT_INSERTJDBG OFF
program Jerry;

{
  --------------------------------- JERRY --------------------------------------

  Tento program slouzi jako klientska aplikace k JOP technologii vyvinute
  Janem Horackem, ktera umoznuje rizeni hnacivh vozidel.

  Funkce:
    - rizeni jizdniho stupne, smeru
    - moznost nouzoveho zastaveni, moznost plynuleho zastaveni
    - multitrakce
    - rozliseni mezi totalnim rucnim rizenim (vhodne napr. pro posun) a
      polorucnim rizenim (vhodne napr. pro rizeni funkci HV v trati)
    - kontrola odpovedi serveru, resp. cetraly, na prikaz
    - moznost prevzit hnaci vozidla na Rocomouse a zpet do regulatoru
    - zadost o lokomotivu(y) do oblasti rizeni
    - zmena serveru, portu, uzivatelskeho jmena a hesla
    - prevzeti hnaciho vozidla za pomoci autorizacniho tokenu
    - kompatibilita s vnejsimi programy akceptovanim argumentu
}

{
 Format argumentu:
    "-u" username
    "-p" password
    "-s" server (ip/dns)
    "-pt" port
    "-a" autoconnect
    addr:token

 napr.
   jerry.exe -a -u root -p heslo 1521:8afff1s86fs4sf86hy16j 2341:f4w64fe5f4wefew4fryh4

 Pokud nnektery z argumentu uzivatelske jmeno, heslo, server, nebo port neni
 vyplnen, je pouzit udaj z nastaveni, popripade je uzivatel vyzvan k zadani
 loginu. Pokud neni program volan s zadnymi hnacimi vozidly, pouze se pripoji
 k serveru a autorizuje.
}

uses
  Forms,
  main in 'main.pas' {F_Main},
  TCPClientPanel in 'TCPClientPanel.pas',
  ListeningThread in 'ListeningThread.pas',
  RPConst in 'RPConst.pas',
  GlobalConfig in 'GlobalConfig.pas',
  RegCollector in 'RegCollector.pas',
  Regulator in 'Regulator.pas' {F_DigiReg},
  ORList in 'ORList.pas',
  settings in 'settings.pas' {F_Settings},
  HVDb in 'HVDb.pas',
  CloseTabSheet in 'CloseTabSheet.pas',
  fAuth in 'fAuth.pas' {F_Auth},
  fNewLoko in 'fNewLoko.pas' {F_NewLoko},
  Debug in 'Debug.pas' {F_Debug},
  Verze in 'Verze.pas',
  Hash in 'Hash.pas',
  Windows;

{$R *.res}

begin
  SetPriorityClass(GetCurrentProcess(), HIGH_PRIORITY_CLASS);

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TF_Main, F_Main);
  Application.CreateForm(TF_Settings, F_Settings);
  Application.CreateForm(TF_Auth, F_Auth);
  Application.CreateForm(TF_NewLoko, F_NewLoko);
  Application.CreateForm(TF_Debug, F_Debug);

  // automaticke pripojeni k serveru
  if ((GlobConfig.data.server.autoconnect) or (GlobConfig.data.args.autoconnect)) then
    F_Main.A_ConnectExecute(nil);

  Application.Run;
end.
