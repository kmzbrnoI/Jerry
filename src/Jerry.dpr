program Jerry;

// Format argumentu:
//    "-u" username
//    "-p" password
//    "-s" server (ip/nd)
//    "-pt" port
//    addr:token


// Napr.
//   jerry.exe -u root -p heslo 1521:8afff1s86fs4sf86hy16j 2341:f4w64fe5f4wefew4fryh4

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
  Verze in 'Verze.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TF_Main, F_Main);
  Application.CreateForm(TF_Settings, F_Settings);
  Application.CreateForm(TF_Auth, F_Auth);
  Application.CreateForm(TF_NewLoko, F_NewLoko);
  Application.CreateForm(TF_Debug, F_Debug);
  // automaticke pripojeni k serveru
  if (GlobConfig.data.server.autoconnect) then F_Main.A_ConnectExecute(nil);

  Application.Run;
end.
