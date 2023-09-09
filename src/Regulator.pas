unit Regulator;

{
  Okynko regulatoru jednoho hnaciho vozidla
}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, Generics.Collections, HVDb,
  CloseTabSheet;

const
  _TIMEOUT_SEC = 5;
  _MAX_FORM_FUNC = 28;
  _MOM_KEEP_ON_MS = 750;

type
  TRegCaptionType = (ctShort, ctLong);

  TLogCommand = record
    time: TDateTime;
  end;

  TMomRelease = record
    f: Integer;
    shutdownTime: TDateTime;
  end;

  TF_DigiReg = class(TForm)
    RG_Smer: TRadioGroup;
    B_PrevzitLoko: TButton;
    B_STOP: TButton;
    CHB_Total: TCheckBox;
    L_stupen: TLabel;
    B_Idle: TButton;
    S_Status: TShape;
    T_Speed: TTimer;
    TB_reg: TTrackBar;
    L_speed: TLabel;
    CHB_Multitrack: TCheckBox;
    Label2: TLabel;
    PC_Funkce: TPageControl;
    TS_func_0_13: TTabSheet;
    TS_func_14_28: TTabSheet;
    T_Mom_Release: TTimer;
    procedure CHB_FuncClick(Sender: TObject);
    procedure B_PrevzitLokoClick(Sender: TObject);
    procedure B_STOPClick(Sender: TObject);
    procedure RG_SmerClick(Sender: TObject);
    procedure B_IdleClick(Sender: TObject);
    procedure S_StatusMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure T_SpeedTimer(Sender: TObject);
    procedure CHB_TotalClick(Sender: TObject);
    procedure T_Mom_ReleaseTimer(Sender: TObject);
  private

    sent: TQueue<TLogCommand>; // vystupni fronta odeslanych prikazu na server
    speed: Integer; // posledni poslana rychlost, pouziva se pri aktualizaci rychlosti v T_Speed
    HV: THV; // hnaciho vozidlo, ktere ridim
    updating: boolean; // je true, pokud se nastavuji elementy zvnejsi
    TS: TCloseTabSheet; // odkaz na zalozku PageControlleru
    CHB_funkce: array [0 .. _MAX_FORM_FUNC] of TCheckBox; // CheckBoxy funkci
    com_err: string; // posledni komunikacni chyba
    q_mom_release: TQueue<TMomRelease>; // fronta momentary funkci k vypnuti

    // odesli prikaz konkretniho hnaciho vozidla na server
    procedure SendCmd(cmd: string);

    // nastav Enabled elementu na formulari na hodnotu \state
    procedure SetElementsState(state: boolean);

    // kontroluje, jestli server odpovedel na vsechny pozadavky a v priapde potreby je posila znovu
    procedure UpdateSent();

    function GetMultitrack(): boolean; // je okynko v multitrakci?

    procedure CreateCHBFunkce(); // vytvori CheckBoxy funkci
    procedure MomRelease(mr: TMomRelease);
    function CreateMomRelease(f: Integer; shutdownTime: TDateTime): TMomRelease; overload;
    function CreateMomRelease(f: Integer): TMomRelease; overload;

    class procedure SimulateClick(var chb: TCheckBox);

  public
    addr: Cardinal; // adresa rizeneho HV (hnaciho vozidla)

    constructor Create(addr: Cardinal; lok_data: string; multitrack: boolean;
      total: boolean; tab: TCloseTabSheet; caption_type: TRegCaptionType);
      reintroduce;
    destructor Destroy(); override;

    procedure Parse(data: TStrings); // parse dat pro tento regulator
    function MyKeyPress(key: Integer): boolean; // keyPress
    // aktualizace rychlosti z vedlejsiho regulatoru pri multitrakci
    procedure UpdateSpeed(from_multitrack: boolean = false);
    procedure DirChanged(from_multitrack: boolean = false);
    procedure ChangeDirFromMultitrack();
    procedure EmergencyStopFromMultitrack();
    procedure IdleRuc();

    procedure Total();
    procedure TotalRelease();

    procedure LongCaption();
    procedure ShortCaption();

    procedure EmergencyStop();
    function TabBgColor(): TColor;

    property multitrack: boolean read GetMultitrack;
  end;

  /// /////////////////////////////////////////////////////////////////////////////

implementation

{$R *.dfm}

uses TCPClientPanel, ORList, Main, RegCollector, ownStrUtils, System.UITypes;

/// /////////////////////////////////////////////////////////////////////////////

constructor TF_DigiReg.Create(addr: Cardinal; lok_data: string; multitrack: boolean;
  total: boolean; tab: TCloseTabSheet; caption_type: TRegCaptionType);
begin
  inherited Create(nil);

  Self.updating := true;
  try
    Self.addr := addr;
    Self.TS := tab;
    Self.Parent := tab;

    Self.sent := TQueue<TLogCommand>.Create();
    Self.speed := -2;
    Self.CHB_Multitrack.Checked := multitrack;
    Self.CHB_Total.Checked := total;
    Self.q_mom_release := TQueue<TMomRelease>.Create();

    Self.HV := THV.Create(lok_data);

    Self.CreateCHBFunkce();

    if (caption_type = ctLong) then
      Self.LongCaption()
    else
      Self.ShortCaption();

    Self.S_Status.Brush.Color := clGreen;

    Self.PC_Funkce.ActivePageIndex := 0;

    Self.L_stupen.Caption := IntToStr(Self.HV.speed_steps) + ' / 28';
    Self.L_speed.Caption := IntToStr(Self.HV.speed_kmph);

    Self.updating := true;
    try
      Self.RG_Smer.ItemIndex := Self.HV.dir;
      Self.speed := Self.HV.speed_steps;
      Self.TB_reg.Position := Self.HV.speed_steps;

      for var i := 0 to _MAX_FORM_FUNC do
      begin
        Self.CHB_funkce[i].AllowGrayed :=
          (Self.HV.funcType[i] = THVFuncType.momentary);
        Self.CHB_funkce[i].Checked := Self.HV.functions[i];
      end;
    finally
      Self.updating := false;
    end;

    Self.SetElementsState(true);

    Self.TB_reg.Enabled := Self.CHB_Total.Checked;
    Self.RG_Smer.Enabled := Self.CHB_Total.Checked;
    Self.B_Idle.Enabled := Self.CHB_Total.Checked;

    // zobrazeni nazvu funkci
    for var i := 0 to _MAX_FORM_FUNC do
    begin
      Self.CHB_funkce[i].ShowHint := (Self.HV.funcVyznam[i] <> '');
      if (Self.HV.funcVyznam[i] <> '') then
      begin
        Self.CHB_funkce[i].Caption := 'F' + IntToStr(i) + ': ' +
          Self.HV.funcVyznam[i];
        Self.CHB_funkce[i].Hint := Self.CHB_funkce[i].Caption;
      end
      else
        Self.CHB_funkce[i].Caption := 'F' + IntToStr(i);
    end;

    Self.B_PrevzitLoko.Enabled := false;

    Self.Show();    
  finally
    Self.updating := false;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

destructor TF_DigiReg.Destroy();
begin
  try
    Self.T_Mom_Release.Enabled := false;
    Self.T_Speed.Enabled := false;
    Self.q_mom_release.Free();
    Self.SendCmd('RELEASE;');
    Self.sent.Free();
    Self.HV.Free();

    for var i := 0 to _MAX_FORM_FUNC do
      Self.CHB_funkce[i].Free();
  finally

  end;

  inherited;
end;

/// /////////////////////////////////////////////////////////////////////////////
// Zapnuti/vypnuti totalniho rucniho rizeni

procedure TF_DigiReg.CHB_TotalClick(Sender: TObject);
begin
  F_Main.PC_Main.Repaint();
  if (Self.updating) then
    Exit();

  if (Self.CHB_Total.Checked) then
  begin
    Self.SendCmd('TOTAL;1');

    if ((RegColl.tabs.Count > 1) and (not RegColl.AreAllTotal())) then
      if (Application.MessageBox('Aktivovat ruèní øízení i pro ostatní otevøená HV?', 'Dotaz', MB_YESNO OR MB_ICONQUESTION OR MB_DEFBUTTON1) = mrYes) then
        RegColl.Total(Self);

  end else begin
    Self.SendCmd('TOTAL;0');

    if ((RegColl.tabs.Count > 1) and (not RegColl.AreAllNotTotal())) then
      if (Application.MessageBox('Deaktivovat ruèní øízení i pro ostatní otevøená HV?', 'Dotaz', MB_YESNO OR MB_ICONQUESTION OR MB_DEFBUTTON1) = mrYes) then
        RegColl.TotalRelease(Self);
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////
// Zapnuti/vypnuti libovolne funkce

procedure TF_DigiReg.CHB_FuncClick(Sender: TObject);
var
  f: Integer;
begin
  if (Self.updating) then
    Exit();
  f := (Sender as TCheckBox).Tag;

  if ((Sender as TCheckBox).state = cbChecked) then
    Self.SendCmd('F;' + IntToStr(f) + ';1')
  else if ((Sender as TCheckBox).state = cbGrayed) then
  begin
    Self.q_mom_release.Enqueue(Self.CreateMomRelease(f));
    Self.CHB_funkce[f].Enabled := false;
    Self.SendCmd('F;' + IntToStr(f) + ';1');
  end
  else
    Self.SendCmd('F;' + IntToStr(f) + ';0');
end;

/// /////////////////////////////////////////////////////////////////////////////
// Prevzit loko do rucniho rizeni

procedure TF_DigiReg.B_PrevzitLokoClick(Sender: TObject);
begin
  PanelTCPClient.SendLn('-;LOK;' + IntToStr(Self.addr) + ';PLEASE;');
end;

/// /////////////////////////////////////////////////////////////////////////////
// Nastavit rychlost loko na 0 stupnu

procedure TF_DigiReg.B_IdleClick(Sender: TObject);
begin
  Self.TB_reg.Position := 0;
  Self.T_SpeedTimer(Self);
end;

/// /////////////////////////////////////////////////////////////////////////////
// Nouzove zastaveni loko

procedure TF_DigiReg.EmergencyStop();
begin
  Self.SendCmd('STOP');

  Self.speed := 0;
  Self.TB_reg.Position := 0;
end;

procedure TF_DigiReg.B_STOPClick(Sender: TObject);
begin
  Self.EmergencyStop();
  if (Self.CHB_Multitrack.Checked) then
    RegColl.MultitrackEmergencyStop(Self.TS);
end;

/// /////////////////////////////////////////////////////////////////////////////
// Zmena smeru loko

procedure TF_DigiReg.RG_SmerClick(Sender: TObject);
begin
  if ((Self.speed > -2) and (not Self.updating)) then
    Self.DirChanged();
end;

/// /////////////////////////////////////////////////////////////////////////////
// Parsign dat ze serveru

procedure TF_DigiReg.Parse(data: TStrings);
begin
  data[3] := UpperCase(data[3]);

  if (data[3] = 'F') then
  begin
    var left, right: Integer;
    var func: TStrings := TStringList.Create();
    try
      ExtractStringsEx(['-'], [], data[4], func);
      left := StrToInt(func[0]);
      if (data.Count > 1) then
        right := StrToInt(func[1])
      else
        right := left;
    finally
      func.Free();
    end;

    Self.updating := true;
    try
      for var i := left to right do
        if (i < _MAX_FORM_FUNC) then
          Self.CHB_funkce[i].Checked := (data[5][i - left + 1] = '1');
    finally
      Self.updating := false;
    end;

    /// ///////////////////////////////////////////////
  end
  else if (data[3] = 'SPD') then
  begin
    Self.updating := true;
    try
      Self.RG_Smer.ItemIndex := StrToInt(data[6]);
      Self.speed := StrToInt(data[5]);
      Self.TB_reg.Position := Self.speed;
      Self.L_speed.Caption := data[4];
      Self.L_stupen.Caption := data[5] + ' / 28';
    finally
      Self.updating := false;
    end;

    var pom: Boolean := not Self.TB_reg.Enabled;
    Self.B_STOP.Enabled := true;

    if ((pom) and (Self.TB_reg.Enabled)) then
      Self.TB_reg.SetFocus();

    /// ///////////////////////////////////////////////
  end
  else if (data[3] = 'RESP') then
  begin
    if (data[4] = 'ok') then
    begin
      Self.S_Status.Brush.Color := clGreen;
      Self.com_err := '';
      if (data.Count > 6) then
        Self.L_speed.Caption := data[6];
    end
    else
    begin
      Self.S_Status.Brush.Color := clRed;
      if (data.Count > 5) then
        Self.com_err := data[5]
      else
        Self.com_err := 'loko NEKOMUNIKUJE';
    end;
    
    F_Main.PC_Main.Repaint();
    if (Self.sent.Count > 0) then
      Self.sent.Dequeue();

    /// ///////////////////////////////////////////////
  end
  else if (data[3] = 'AUTH') then
  begin
    if ((data[4] = 'ok') or (data[4] = 'total')) then
    begin
      updating := true;
      try
        Self.CHB_Total.Checked := (data[4] = 'total');
        Self.TB_reg.Enabled := Self.CHB_Total.Checked;
        Self.RG_Smer.Enabled := Self.CHB_Total.Checked;
        Self.B_Idle.Enabled := Self.CHB_Total.Checked;
      finally
        updating := false;
      end;

      Self.HV.ParseData(data[5]);
      Self.OnShow(Self);
      F_Main.PC_Main.Repaint();
    end
    else
    begin
      Self.SetElementsState(false);
      if (data[4] = 'stolen') then
      begin
        Self.S_Status.Brush.Color := clYellow;
      end else begin
        Self.S_Status.Brush.Color := clRed;
      end;
      Self.B_PrevzitLoko.Enabled := true;
    end;

    F_Main.PC_Main.Repaint();

  end
  else if (data[3] = 'TOTAL') then
  begin
    if (Self.sent.Count > 0) then
      Self.sent.Dequeue(); // TOTAL je odpovedi na TOTAL a RESP neni odesilano

    updating := true;
    try
      Self.CHB_Total.Checked := (data[4] = '1');
      Self.TB_reg.Enabled := Self.CHB_Total.Checked;
      Self.RG_Smer.Enabled := Self.CHB_Total.Checked;
      Self.B_Idle.Enabled := Self.CHB_Total.Checked;
    finally
      updating := false;
    end;

    F_Main.PC_Main.Repaint();
    
    if ((F_Main.PC_Main.ActivePage = Self.TS) and
      (Self.TB_reg.Enabled)) then
      Self.TB_reg.SetFocus();
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////
// Kliknuti na Share S_Status zpusobi zadost o prevzeti hnaciho vozidla,
// pokud neni HV prevzato (tj. shape je zluty)

procedure TF_DigiReg.S_StatusMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (Self.com_err <> '') then
    Application.MessageBox(PChar('Loko nekomunikuje:' + #13#10 + Self.com_err),
      'Loko nekomunikuje', MB_OK OR MB_ICONWARNING)
  else if (Self.B_PrevzitLoko.Enabled) then
    Self.B_PrevzitLokoClick(Self);
end;

/// /////////////////////////////////////////////////////////////////////////////
// T_Speed odesila pri kazdem ticku aktualni rychlost hnaciho vozidla
// na server, pokud se tato rychlost zmenila od posledni rychlosti.
// Pri pohybu posuvnikem tak neni rychlost poslana rovnou, ale az pri ticku
// timeru. To je dobre k tomu, abychom server moc nezatezovali. Pri vhodne
// zvolene periode timeru neni rozdil od primeho posilani patrny.

procedure TF_DigiReg.T_SpeedTimer(Sender: TObject);
begin
  if (Self.B_PrevzitLoko.Enabled) then
    Exit();

  Self.UpdateSpeed();
  Self.UpdateSent();
end;

procedure TF_DigiReg.UpdateSent();
begin
  if (Self.sent.Count > 0) then
  begin
    if (Self.sent.Peek.time + EncodeTime(0, 0, _TIMEOUT_SEC, 0) < Now) then
    begin
      // timeout
      Self.sent.Dequeue();
      Self.S_Status.Brush.Color := clRed;
      Self.com_err := 'loko NEKOMUNIKUJE';
      F_Main.PC_Main.Repaint();
    end;
  end;
end;

procedure TF_DigiReg.UpdateSpeed(from_multitrack: boolean);
begin
  if ((Self.speed <> Self.TB_reg.Position) and (Self.speed > -2)) then
  begin
    Self.SendCmd('SPD-S;' + IntToStr(Self.TB_reg.Position) + ';' +
      IntToStr(Self.RG_Smer.ItemIndex));
    Self.L_stupen.Caption := IntToStr(TB_reg.Position) + ' / 28';
    Self.speed := Self.TB_reg.Position;
    if (not from_multitrack) and (Self.CHB_Multitrack.Checked) then
      RegColl.MultitrackSpeedChanged(Self.TS);
  end;
end;

procedure TF_DigiReg.DirChanged(from_multitrack: boolean);
begin
  Self.SendCmd('D;' + IntToStr(Self.RG_Smer.ItemIndex));
  if (not from_multitrack) and (Self.CHB_Multitrack.Checked) then
    RegColl.MultitrackDirChanged(Self.TS);
end;

/// /////////////////////////////////////////////////////////////////////////////
// vyvola se, pokud je me okynko aktivni a je nad nim stiskla klavesa
// (resi se AppcationEvent)

function TF_DigiReg.MyKeyPress(key: Integer): boolean;
begin
  if (Self.B_PrevzitLoko.Enabled) then
  begin
    if ((key = VK_RETURN) and (Self.ActiveControl <> Self.B_PrevzitLoko)) then
    begin
      Self.B_PrevzitLokoClick(Self);
      Exit(true);
    end;
    Exit(false);
  end;

  Result := true;

  case (key) of
    VK_NUMPAD0 .. VK_NUMPAD9:
      if (Self.CHB_funkce[key - VK_NUMPAD0].Enabled) then
        Self.SimulateClick(Self.CHB_funkce[key - VK_NUMPAD0]);
    VK_F1 .. VK_F24:
      if (Self.CHB_funkce[key - VK_F1 + 1].Enabled) then
        Self.SimulateClick(Self.CHB_funkce[key - VK_F1 + 1]);

    VK_ADD:
      if (Self.RG_Smer.Enabled) then
        Self.RG_Smer.ItemIndex := 0;
    VK_SUBTRACT:
      if (Self.RG_Smer.Enabled) then
        Self.RG_Smer.ItemIndex := 1;

    ord('S'):
      if (Self.TB_reg.Enabled) then
        Self.B_STOPClick(Self);
    ord('I'):
      if (Self.TB_reg.Enabled) then
        Self.B_IdleClick(Self);
    ord('R'):
      if (Self.CHB_Total.Enabled) then
        Self.CHB_Total.Checked := not Self.CHB_Total.Checked;
    ord('M'):
      if (Self.CHB_Multitrack.Enabled) then
        Self.CHB_Multitrack.Checked := not Self.CHB_Multitrack.Checked;
  else
    Result := false;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////
// Odeslani prikazu serveru

procedure TF_DigiReg.SendCmd(cmd: string);
var
  lc: TLogCommand;
begin
  PanelTCPClient.SendLn('-;LOK;' + IntToStr(Self.addr) + ';' + cmd);

  lc.time := Now;
  Self.sent.Enqueue(lc);
end;

/// /////////////////////////////////////////////////////////////////////////////
// Zmena enabled objektu na formulari

procedure TF_DigiReg.SetElementsState(state: boolean);
begin
  TB_reg.Enabled := state;
  RG_Smer.Enabled := state;
  B_STOP.Enabled := state;
  B_Idle.Enabled := state;

  for var i := 0 to _MAX_FORM_FUNC do
    Self.CHB_funkce[i].Enabled := state;

  CHB_Total.Enabled := state;
  CHB_Multitrack.Enabled := state;
end;

/// /////////////////////////////////////////////////////////////////////////////

function TF_DigiReg.GetMultitrack(): boolean;
begin
  Result := Self.CHB_Multitrack.Checked;
end;

/// /////////////////////////////////////////////////////////////////////////////
// Vytvoreni vsech CHB_funkce

procedure TF_DigiReg.CreateCHBFunkce();
const
  _COLS = 2;
  _ROWS = 7;
  _CLEAR_PX = 5;
begin
  Self.CHB_funkce[0] := TCheckBox.Create(Self);
  with (Self.CHB_funkce[0]) do
  begin
    Parent := Self.PC_Funkce;
    Left := (Self.TS_func_0_13.ClientWidth div _COLS) + ((Self.PC_Funkce.Width - Self.TS_func_0_13.Width) div 2);
    Top := 3;
    Caption := 'F0';
    Tag := 0;
    AutoSize := False;
    Width := (Self.TS_func_0_13.ClientWidth div _COLS) - _CLEAR_PX;
    OnClick := Self.CHB_FuncClick;
  end; // with

  var myTop := 0;
  for var i := 1 to _MAX_FORM_FUNC do
  begin
    if (((i-1) mod _ROWS) = 0) then
      myTop := 0;

    Self.CHB_funkce[i] := TCheckBox.Create(Self);
    with (Self.CHB_funkce[i]) do
    begin
      if (i <= (_ROWS*_COLS)) then
        Parent := Self.TS_func_0_13
      else
        Parent := Self.TS_func_14_28;

      Left := (((i-1) div _ROWS) mod _COLS) * (Self.TS_func_0_13.ClientWidth div _COLS);
      Top := myTop;
      Caption := 'F' + IntToStr(i);
      Tag := i;
      AutoSize := false;
      Width := (Self.TS_func_0_13.ClientWidth div _COLS) - _CLEAR_PX;
      OnClick := Self.CHB_FuncClick;
      Inc(myTop, Height-1);
    end; // with
  end; // for i
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TF_DigiReg.T_Mom_ReleaseTimer(Sender: TObject);
begin
  if (Self.q_mom_release.Count > 0) then
    if (Self.q_mom_release.Peek().shutdownTime <= Now) then
      Self.MomRelease(Self.q_mom_release.Dequeue());
end;

procedure TF_DigiReg.MomRelease(mr: TMomRelease);
begin
  Self.CHB_funkce[mr.f].state := cbUnchecked; // odesle prikaz k vypnuti funkce
  Self.CHB_funkce[mr.f].Enabled := true;
end;

function TF_DigiReg.CreateMomRelease(f: Integer; shutdownTime: TDateTime)
  : TMomRelease;
begin
  Result.f := f;
  Result.shutdownTime := shutdownTime;
end;

function TF_DigiReg.CreateMomRelease(f: Integer): TMomRelease;
begin
  Result := Self.CreateMomRelease(f, Now + EncodeTime(0, 0,
    _MOM_KEEP_ON_MS div 1000, _MOM_KEEP_ON_MS mod 1000));
end;

/// /////////////////////////////////////////////////////////////////////////////

class procedure TF_DigiReg.SimulateClick(var chb: TCheckBox);
begin
  if (chb.AllowGrayed) then
  begin
    if (chb.state = cbUnchecked) then
      chb.state := cbGrayed
    else if (chb.state = cbGrayed) then
      chb.state := cbChecked
    else
      chb.state := cbUnchecked;
  end
  else
  begin
    chb.Checked := not chb.Checked;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TF_DigiReg.LongCaption();
begin
  if (Self.HV.designation <> '') then
    Self.TS.Caption := Self.HV.name + ' (' + Self.HV.designation + ') : ' +
      IntToStr(Self.HV.addr) + '      '
  else
    Self.TS.Caption := Self.HV.name + ' : ' + IntToStr(Self.HV.addr) + '      ';
end;

procedure TF_DigiReg.ShortCaption();
begin
  Self.TS.Caption := IntToStr(Self.HV.addr) + '      ';
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TF_DigiReg.ChangeDirFromMultitrack();
begin
  Self.updating := true;
  try
    case (Self.RG_Smer.ItemIndex) of
      0: Self.RG_Smer.ItemIndex := 1;
      1: Self.RG_Smer.ItemIndex := 0;
    end;
  finally
    Self.updating := false;
  end;
  Self.DirChanged(true);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TF_DigiReg.EmergencyStopFromMultitrack();
begin
 Self.EmergencyStop();
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TF_DigiReg.IdleRuc();
begin
  if (Self.CHB_Total.Checked) then
    Self.B_IdleClick(Self);
end;

procedure TF_DigiReg.Total();
begin
  Self.SendCmd('TOTAL;1');
end;

procedure TF_DigiReg.TotalRelease();
begin
  Self.SendCmd('TOTAL;0');
end;

/// /////////////////////////////////////////////////////////////////////////////

function TF_DigiReg.TabBgColor(): TColor;
begin
  if ((not Assigned(Self)) or (not Assigned(Self.S_Status))) then
    Exit(clBtnFace);
  Result := Self.S_Status.Brush.Color;
  if (Result = clGreen) then
  begin
    if (Self.CHB_Total.Checked) then
    begin
      if (Self.speed > 0) then
        Result := clMoneyGreen
      else
        Result := clActiveCaption
    end else
      Result := clBtnFace;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

end.
