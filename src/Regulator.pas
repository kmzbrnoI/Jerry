unit Regulator;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, Generics.Collections, HVDb,
  CloseTabSheet;

const
  _TIMEOUT_SEC = 5;

type
  TLogCommand = record
    time:TDateTime;
  end;

  TF_DigiReg = class(TForm)
    CHB_svetla: TCheckBox;
    CHB_f1: TCheckBox;
    CHB_f2: TCheckBox;
    CHB_f4: TCheckBox;
    CHB_f3: TCheckBox;
    CHB_f5: TCheckBox;
    CHB_f6: TCheckBox;
    CHB_f8: TCheckBox;
    CHB_f7: TCheckBox;
    Label5: TLabel;
    RG_Smer: TRadioGroup;
    Label6: TLabel;
    B_PrevzitLoko: TButton;
    B_STOP: TButton;
    Label7: TLabel;
    CHB_Total: TCheckBox;
    CHB_f9: TCheckBox;
    CHB_f10: TCheckBox;
    CHB_f12: TCheckBox;
    CHB_f11: TCheckBox;
    L_address: TLabel;
    L_mine: TLabel;
    L_stupen: TLabel;
    B_Idle: TButton;
    S_Status: TShape;
    T_Speed: TTimer;
    L_ComStatus: TLabel;
    TB_reg: TTrackBar;
    Label1: TLabel;
    L_speed: TLabel;
    CHB_Multitrack: TCheckBox;
    Label2: TLabel;
    procedure CHB_svetlaClick(Sender: TObject);
    procedure B_PrevzitLokoClick(Sender: TObject);
    procedure B_STOPClick(Sender: TObject);
    procedure RG_SmerClick(Sender: TObject);
    procedure B_IdleClick(Sender: TObject);
    procedure S_StatusMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure T_SpeedTimer(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure CHB_TotalClick(Sender: TObject);
  private

   sent:TQueue<TLogCommand>;
   speed:Integer;
   HV:THV;
   updating:boolean;        // je true, pokud se nastavuji elementy zvnejsi
   TS:TCloseTabSheet;

   procedure SendCmd(cmd:string);
   procedure UpdateFunc(f:Integer; state:boolean);

   procedure SetElementsState(state:boolean);

   procedure UpdateSent();

   function GetMultitrack:boolean;

  public
   addr:Word;


   constructor Create(Addr:word; lok_data:string; multitrack:boolean; total:boolean; tab:TCloseTabSheet); reintroduce;
   destructor Destroy(); override;

   procedure Parse(data:TStrings);

   function MyKeyPress(key:Integer):boolean;  // returns handled

   procedure UpdateRych(multitrack:boolean = true);

   property multitrack:boolean read GetMultitrack;
  end;

////////////////////////////////////////////////////////////////////////////////

implementation

{$R *.dfm}

uses TCPClientPanel, ORList, Main, RegCollector, RPConst;

////////////////////////////////////////////////////////////////////////////////

constructor TF_DigiReg.Create(Addr:word; lok_data:string; multitrack:boolean; total:boolean; tab:TCloseTabSheet);
begin
 inherited Create(nil);

 Self.updating := true;

 Self.addr := Addr;
 Self.TS   := tab;

 Self.sent  := TQueue<TLogCommand>.Create();
 Self.speed := -2;
 Self.CHB_Multitrack.Checked := multitrack;
 Self.CHB_Total.Checked := total;

 Self.HV := THV.Create(lok_data);

 Self.Parent := tab;
 Self.Show();
 Self.updating := false;
end;//ctor

destructor TF_DigiReg.Destroy();
begin
 Self.SendCmd('RELEASE;');
 Self.sent.Free();
 Self.HV.Free();

 inherited Destroy();
end;//dtor

procedure TF_DigiReg.FormShow(Sender: TObject);
var i:Integer;
begin
  Self.L_mine.Caption       := 'ano';
  Self.S_Status.Brush.Color := clGreen;
  Self.TS.Caption           := Self.HV.Nazev+' ('+Self.HV.Oznaceni+') : '+IntToStr(Self.HV.Adresa)+'      ';
  Self.L_address.Caption    := IntToStr(Self.addr);
  Self.L_stupen.Caption     := IntToStr(Self.HV.rychlost_stupne)+' / 28';
  Self.L_speed.Caption      := IntToStr(Self.HV.rychlost_kmph);

  Self.updating := true;
  Self.RG_Smer.ItemIndex := Self.HV.smer;
  Self.speed             := Self.HV.rychlost_stupne;
  Self.TB_reg.Position   := Self.HV.rychlost_stupne;

  for i := 0 to _MAX_FUNC do
    Self.UpdateFunc(i, Self.HV.funkce[i]);

  Self.updating := false;
  Self.L_ComStatus.Font.Color := clGreen;
  Self.L_ComStatus.Caption    := 'loko KOMUNIKUJE';

  Self.SetElementsState(true);

  Self.TB_reg.Enabled  := Self.CHB_Total.Checked;
  Self.RG_Smer.Enabled := Self.CHB_Total.Checked;
  Self.B_Idle.Enabled  := Self.CHB_Total.Checked;

  Self.B_PrevzitLoko.Enabled := false;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TF_DigiReg.CHB_TotalClick(Sender: TObject);
begin
 if (Self.updating) then Exit(); 

 if (Self.CHB_Total.Checked) then
  begin
   Self.SendCmd('TOTAL;1');
  end else
   Self.SendCmd('TOTAL;0');
end;

procedure TF_DigiReg.CHB_svetlaClick(Sender: TObject);
 begin
  if (Self.updating) then Exit();
  
  if ((Sender as TCheckBox).Checked) then
    Self.SendCmd('F;'+IntToStr((Sender as TCheckBox).Tag)+';1')
  else
    Self.SendCmd('F;'+IntToStr((Sender as TCheckBox).Tag)+';0');
 end;//procedure

procedure TF_DigiReg.B_PrevzitLokoClick(Sender: TObject);
begin
 PanelTCPClient.SendLn('-;LOK;'+IntToStr(Self.addr)+';PLEASE;');
end;//procedure

procedure TF_DigiReg.B_IdleClick(Sender: TObject);
begin
 Self.TB_reg.Position := 0;
 Self.T_SpeedTimer(Self);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TF_DigiReg.B_STOPClick(Sender: TObject);
 begin
  Self.SendCmd('STOP');

  Self.speed := 0;
  Self.TB_reg.Position := 0;
 end;//procedure

procedure TF_DigiReg.RG_SmerClick(Sender: TObject);
 begin
  if (Self.speed > -2) then
   begin
    Self.speed := -1;
    Self.T_SpeedTimer(Self);
   end;
 end;//procedure

////////////////////////////////////////////////////////////////////////////////
// prijmuta data ze serveru

//  or;LOK;ADDR;AUTH;[ok,not,stolen,release]; info  - odpoved na pozadavek o autorizaci rizeni hnaciho vozidla (odesilano take jako informace o zruseni ovladani hnacicho vozidla)
//  or;LOK;ADDR;F;F_left-F_right;states          - informace o stavu funkci lokomotivy
//    napr.; or;LOK;0-4;00010 informuje, ze je zaple F3 a F0, F1, F2 a F4 jsou vyple
//  or;LOK;ADDR;SPD;sp_km/h;sp_stupne;dir        - informace o zmene rychlosti (ci smeru) lokomotivy
//  or;LOK;ADDR;RESP;[ok, err]; info
//  -;LOK;TOTAL;[0,1]                       - zmena rucniho rizeni lokomotivy

procedure TF_DIgiReg.Parse(data:TStrings);
var func:TStrings;
    left, right, i:Integer;
    pom:boolean;
begin
 if (data[3] = 'F') then begin
   func := TStringList.Create();
   ExtractStringsEx(['-'], [], data[4], func);
   left := StrToInt(func[0]);
   if (data.Count > 1) then
    right := StrToInt(func[1])
   else
    right := left;
   func.Free();

   Self.updating := true;
   for i := left to right do
    if (data[5][i-left+1] = '1') then
      Self.UpdateFunc(i, true)
     else
      Self.UpdateFunc(i, false);
  Self.updating := false;

 //////////////////////////////////////////////////
 end else if (data[3] = 'SPD') then begin
  Self.RG_Smer.ItemIndex := StrToInt(data[6]);

  Self.updating := true;
  Self.speed            := StrToInt(data[5]);
  Self.TB_reg.Position  := Self.speed;
  Self.L_speed.Caption  := data[4];
  Self.L_stupen.Caption := data[5] + ' / 28';
  Self.updating := false;

  pom := not Self.TB_reg.Enabled;
  Self.B_STOP.Enabled  := true;

  if ((pom) and (Self.TB_reg.Enabled)) then Self.TB_reg.SetFocus();

 //////////////////////////////////////////////////
 end else if (data[3] = 'RESP') then begin
  if (data[4] = 'ok') then
   begin
    Self.L_ComStatus.Font.Color := clGreen;
    Self.L_ComStatus.Caption    := 'loko KOMUNIKUJE';
    if (data.Count > 5) then
       Self.L_speed.Caption := data[5];
   end else begin
    Self.L_ComStatus.Font.Color := clRed;
    if (data.Count > 5) then
      Self.L_ComStatus.Caption  := data[5]
    else
      Self.L_ComStatus.Caption  := 'loko NEKOMUNIKUJE';
   end;

  Self.sent.Dequeue();

 //////////////////////////////////////////////////
 end else if (data[3] = 'AUTH') then begin
   if ((data[4] = 'ok') or (data[4] = 'total')) then
    begin
     updating := true;
     Self.CHB_Total.Checked := (data[4] = 'total');
     Self.TB_reg.Enabled  := Self.CHB_Total.Checked;
     Self.RG_Smer.Enabled := Self.CHB_Total.Checked;
     Self.B_idle.Enabled  := Self.CHB_Total.Checked;
     updating := false;

     Self.HV.ParseData(data[5]);
     Self.OnShow(Self);
    end else begin
     Self.SetElementsState(false);
     Self.L_mine.Caption := 'ne';
     if (data[4] = 'stolen') then
      begin
       Self.S_Status.Brush.Color := clYellow;
      end else begin
       Self.S_Status.Brush.Color := clRed;
      end;
     Self.B_PrevzitLoko.Enabled := true;

     if (data.Count > 5) then
      begin
       Self.L_ComStatus.Font.Color := clGray;
       Self.L_ComStatus.Caption    := data[5];
      end;
    end;

   F_Main.PC_Main.Repaint();

 end else if (data[3] = 'TOTAL') then begin
   if (Self.sent.Count > 0) then
     Self.sent.Dequeue();     // TOTAL je odpovedi na TOTAL a RESP neni odesilano

   updating := true;
   Self.CHB_Total.Checked := (data[4] = '1');
   Self.TB_reg.Enabled  := Self.CHB_Total.Checked;
   Self.RG_Smer.Enabled := Self.CHB_Total.Checked;
   Self.B_Idle.Enabled  := Self.CHB_Total.Checked;
   updating := false;

   if (F_Main.PC_Main.ActivePage = (Self.Parent as TCloseTabSheet)) then Self.TB_reg.SetFocus();
 end;
end;//procedure

procedure TF_DigiReg.S_StatusMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 if (Self.S_Status.Brush.Color <> clGreen) then
  Self.B_PrevzitLokoClick(self);
end;

procedure TF_DigiReg.T_SpeedTimer(Sender: TObject);
begin
 if (Self.S_Status.Brush.Color <> clGreen) then Exit();

 Self.UpdateRych();
 Self.UpdateSent();
end;//procedure

procedure TF_DigiReg.UpdateSent();
begin
 if (Self.sent.Count > 0) then
  begin
   if (Self.sent.Peek.time + EncodeTime(0, 0, _TIMEOUT_SEC, 0) < Now) then
    begin
     // timeout
     Self.sent.Dequeue();
     Self.L_ComStatus.Font.Color := clRed;
     Self.L_ComStatus.Caption    := 'loko NEKOMUNIKUJE';
    end;
  end;
end;//procedure

procedure TF_DigiReg.UpdateRych(multitrack:boolean = true);
begin
 if ((Self.speed <> Self.TB_reg.Position) and (Self.speed > -2)) then
  begin
   Self.SendCmd('SPD-S;'+IntToStr(Self.TB_reg.Position)+';'+IntToStr(Self.RG_Smer.ItemIndex));
   Self.L_stupen.Caption := IntToStr(TB_Reg.Position)+' / 28';
   Self.speed := Self.TB_reg.Position;
   if (multitrack) then RegColl.UpdateMultitrack(Self.Parent as TCloseTabSheet);
  end;
end;//procedure

// vyvola se, pokud je me a=okynko aktivni a je nad nim stiskla klavesa
function TF_DigiReg.MyKeyPress(key:Integer):boolean;
begin
 if (Self.S_Status.Brush.Color <> clGreen) then
  begin
   if ((key = VK_RETURN) and (Self.ActiveControl <> Self.B_PrevzitLoko)) then begin
    Self.B_PrevzitLokoClick(Self);
    Exit(true);
   end;
   Exit(false);
  end;

 Result := true;

 case (key) of
  VK_NUMPAD0 : Self.CHB_svetla.Checked := not Self.CHB_svetla.Checked;
  VK_NUMPAD1 : Self.CHB_f1.Checked := not Self.CHB_f1.Checked;
  VK_NUMPAD2 : Self.CHB_f2.Checked := not Self.CHB_f2.Checked;
  VK_NUMPAD3 : Self.CHB_f3.Checked := not Self.CHB_f3.Checked;
  VK_NUMPAD4 : Self.CHB_f4.Checked := not Self.CHB_f4.Checked;
  VK_NUMPAD5 : Self.CHB_f5.Checked := not Self.CHB_f5.Checked;
  VK_NUMPAD6 : Self.CHB_f6.Checked := not Self.CHB_f6.Checked;
  VK_NUMPAD7 : Self.CHB_f7.Checked := not Self.CHB_f7.Checked;
  VK_NUMPAD8 : Self.CHB_f8.Checked := not Self.CHB_f8.Checked;
  VK_NUMPAD9 : Self.CHB_f9.Checked := not Self.CHB_f9.Checked;

  VK_ADD      : if (Self.RG_Smer.Enabled) then Self.RG_Smer.ItemIndex := 0;
  VK_SUBTRACT : if (Self.RG_Smer.Enabled) then Self.RG_Smer.ItemIndex := 1;

  83: if (Self.TB_reg.Enabled) then Self.B_STOPClick(Self);   // 's'
  73: if (Self.TB_reg.Enabled) then Self.B_IdleClick(Self);   // 'i'
 else
   Result := false;
 end;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TF_DigiReg.SendCmd(cmd:string);
var lc:TLogCommand;
begin
 PanelTCPClient.SendLn('-;LOK;'+IntToStr(Self.addr)+';'+cmd);

 lc.time := Now;
 Self.sent.Enqueue(lc);
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TF_DigiReg.UpdateFunc(f:Integer; state:boolean);
begin
 case (f) of
  0 : Self.CHB_svetla.Checked := state;
  1 : Self.CHB_f1.Checked  := state;
  2 : Self.CHB_f2.Checked  := state;
  3 : Self.CHB_f3.Checked  := state;
  4 : Self.CHB_f4.Checked  := state;
  5 : Self.CHB_f5.Checked  := state;
  6 : Self.CHB_f6.Checked  := state;
  7 : Self.CHB_f7.Checked  := state;
  8 : Self.CHB_f8.Checked  := state;
  9 : Self.CHB_f9.Checked  := state;
  10: Self.CHB_f10.Checked := state;
  11: Self.CHB_f11.Checked := state;
  12: Self.CHB_f12.Checked := state;
 end;//case
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TF_DigiReg.SetElementsState(state:boolean);
begin
  TB_reg.Enabled  := state;
  RG_Smer.Enabled := state;
  B_STOP.Enabled  := state;
  B_idle.Enabled  := state;
  CHB_svetla.Enabled := state;
  CHB_f1.Enabled  := state;
  CHB_f2.Enabled  := state;
  CHB_f3.Enabled  := state;
  CHB_f4.Enabled  := state;
  CHB_f5.Enabled  := state;
  CHB_f6.Enabled  := state;
  CHB_f7.Enabled  := state;
  CHB_f8.Enabled  := state;
  CHB_f9.Enabled  := state;
  CHB_f10.Enabled := state;
  CHB_f11.Enabled := state;
  CHB_f12.Enabled := state;
  CHB_Total.Enabled := state;
  CHB_Multitrack.Enabled := state;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

function TF_DigiReg.GetMultitrack():boolean;
begin
 Result := Self.CHB_Multitrack.Checked;
end;//function

end.//unit
