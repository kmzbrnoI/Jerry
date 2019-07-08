// JCL_DEBUG_EXPERT_INSERTJDBG OFF
program Jerry;

uses
  Forms,
  main in 'main.pas' {F_Main},
  TCPClientPanel in 'TCPClientPanel.pas',
  ListeningThread in 'ListeningThread.pas',
  GlobalConfig in 'GlobalConfig.pas',
  RegCollector in 'RegCollector.pas',
  Regulator in 'Regulator.pas' {F_DigiReg},
  ORList in 'ORList.pas',
  fSettings in 'fSettings.pas' {F_Settings},
  HVDb in 'HVDb.pas',
  CloseTabSheet in 'CloseTabSheet.pas',
  fAuth in 'fAuth.pas' {F_Auth},
  fNewLoko in 'fNewLoko.pas' {F_NewLoko},
  fDebug in 'fDebug.pas' {F_Debug},
  Verze in 'Verze.pas',
  Hash in 'Hash.pas',
  Windows,
  ownStrUtils in 'ownStrUtils.pas';

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
