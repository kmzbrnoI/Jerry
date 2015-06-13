unit RegCollector;

interface

uses Regulator, Generics.Collections, SysUtils, Classes, Forms, Dialogs, Windows,
    CloseTabSheet;

type

 TRegulatorCollector = class
   private const
    _MAX_FORMS = 4;

   private
    tabs:TList<TCloseTabSheet>;

     function GetTab(OblR:string; addr:Word):TCloseTabSheet;
     procedure NewLoko(OblR:string; addr:Word; lok_data:string; total:boolean);

     procedure OnTSClose(Sender:TObject);

   public
     constructor Create();
     destructor Destroy(); override;

     function KeyPress(key:Integer):boolean;     // returns handled
     procedure Parse(data:TStrings);
     procedure CloseAll();
     procedure UpdateMultitrack(Sender:TCloseTabSheet);

 end;

var
  RegColl:TRegulatorCollector;

implementation

uses HVDb, fNewLoko, Main;

////////////////////////////////////////////////////////////////////////////////

constructor TRegulatorCollector.Create();
begin
 inherited Create();

 Self.tabs := TList<TCloseTabSheet>.Create();
end;//ctor

destructor TRegulatorCollector.Destroy();
var i:Integer;
begin
 for i := 0 to Self.tabs.Count-1 do
   Self.tabs[i].Free();
 Self.tabs.Free();
 inherited Destroy();
end;//dtor

procedure TRegulatorCollector.NewLoko(OblR:string; addr:Word; lok_data:string; total:boolean);
var tab:TCloseTabSheet;
begin
 for tab in Self.tabs do
   if ((tab.form as TF_DigiReg).addr = addr) then
    begin
     F_Main.PC_Main.ActivePage := tab;
     Exit;
    end;

 tab := TCloseTabSheet.Create(F_Main.PC_Main);
 tab.PageControl := F_Main.PC_Main;
 tab.OnClose     := Self.OnTSClose;
 tab.form        := TF_DigiReg.Create(addr, lok_data, Self.tabs.Count > 0, total, tab);
 Self.tabs.Add(tab);

 (Self.tabs[0].form as TF_DigiReg).CHB_Multitrack.Checked := ((Self.tabs.Count > 1) and (total));

 if ((F_NewLoko.Showing) and (F_NewLoko.zadost_probiha)) then
  begin
   F_NewLoko.zadost_probiha := false;
   F_NewLoko.Close();
  end;

 F_Main.PC_Main.ActivePage := tab;
 if ((tab.form as TF_DigiReg).TB_reg.Enabled) then
   (tab.form as TF_DigiReg).TB_reg.SetFocus()
 else
   (tab.form as TF_DigiReg).CHB_Total.SetFocus();
end;//procedure

function TRegulatorCollector.GetTab(OblR:string; addr:Word):TCloseTabSheet;
var tab:TCloseTabSheet;
begin
 Result := nil;
 for tab in Self.tabs do
   if ((tab.form as TF_DigiReg).addr = addr) then
     Exit(tab);
end;//function

procedure TRegulatorCollector.CloseAll();
var i:Integer;
begin
 for i := Self.tabs.Count-1 downto 0 do
  begin
   (Self.tabs[i].form as TF_DigiReg).Free;
   Self.tabs[i].Free;
  end;
 Self.tabs.Clear();
end;//procedure

function TRegulatorCollector.KeyPress(key:Integer):boolean;
var i:Integer;
begin
 for i := 0 to Self.tabs.Count-1 do
   if ((Self.tabs[i] = F_Main.PC_Main.ActivePage) and ((Self.tabs[i].form as TF_DigiReg).MyKeyPress(key))) then
     Exit(true);

 Result := false;
end;//procedure

////////////////////////////////////////////////////////////////////////////////
// parse dat ze serveru

//  or;LOK;ADDR;AUTH;[ok,not,stolen,release]; info  - odpoved na pozadavek o autorizaci rizeni hnaciho vozidla (odesilano take jako informace o zruseni ovladani hnacicho vozidla)
//  or;LOK;ADDR;F;F_left-F_right;states          - informace o stavu funkci lokomotivy
//    napr.; or;LOK;0-4;00010 informuje, ze je zaple F3 a F0, F1, F2 a F4 jsou vyple
//  or;LOK;ADDR;SPD;sp_km/h;sp_stupne;dir        - informace o zmene rychlosti (ci smeru) lokomotivy
//  or;LOK;ADDR;RESP;[ok, err]; info
//  -;LOK;TOTAL;[0,1]                       - zmena rucniho rizeni lokomotivy

procedure TRegulatorCollector.Parse(data:TStrings);
var tab:TCloseTabSheet;
begin
 tab := Self.GetTab(data[0], StrToInt(data[2]));

 if (data[3] = 'AUTH') then
  begin
   if (tab = nil) then
    begin
     if ((data[4] = 'ok') or (data[4] = 'total')) then
      Self.NewLoko(data[0], StrToInt(data[2]), data[5], data[4]='total')
     else if (data[4] = 'not') then
      begin
       if (data.Count > 5) then
         Application.MessageBox(PChar('Loko '+data[2]+' nepøevzato'+#13#10+data[5]), 'Nepøevzato', MB_OK OR MB_ICONWARNING)
       else
         Application.MessageBox(PChar('Loko '+data[2]+' nepøevzato'), 'Nepøevzato', MB_OK OR MB_ICONWARNING);
      end;
    end else begin
     // tab <> nil
     (tab.form as TF_DigiReg).Parse(data);
    end;
  end else begin
   if (tab <> nil) then (tab.form as TF_DigiReg).Parse(data);
  end;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TRegulatorCollector.OnTSClose(Sender:TObject);
var i:Integer;
begin
 for i := Self.tabs.Count-1 downto 0 do
   if (Self.tabs[i] = Sender) then
    begin
     Self.tabs.Delete(i);
     break;
    end;

 (Sender as TCloseTabSheet).form.Free();
 (Sender as TCloseTabSheet).Free();

 if (Self.tabs.Count = 1) then (Self.tabs[0].form as TF_DigiReg).CHB_Multitrack.Checked := false;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TRegulatorCollector.UpdateMultitrack(Sender:TCloseTabSheet);
var tab:TClosetabSheet;
begin
 if ((Sender.form as TF_DigiReg).CHB_Multitrack.Checked) then
   for tab in Self.tabs do
     if ((tab <> Sender) and ((tab.form as TF_DigiReg).TB_reg.Enabled)) then
      begin
       (tab.form as TF_DigiReg).TB_reg.Position := (Sender.form as TF_DigiReg).TB_reg.Position;
       (tab.form as TF_DigiReg).UpdateRych(false);
      end;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

initialization
  RegColl := TRegulatorCollector.Create();

finalization
  RegColl.Free;

end.//unit
