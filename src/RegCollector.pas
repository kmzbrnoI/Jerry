unit RegCollector;

{
  Collector regulatoru hnacich vozidel
  Trida TRegulatorCollector sdruzuje vsechny regulatory, resp. zalozky.
}

interface

uses Regulator, Generics.Collections, SysUtils, Classes, Forms, Dialogs, Windows,
    CloseTabSheet;

type

 TRegulatorCollector = class
   private const
    _MAX_FORMS = 4;                                                             // maximum otevrenych regulatoru

   private
    tabs:TList<TCloseTabSheet>;                                                 // senzam vsech zalozek

     function GetTab(addr:Word):TCloseTabSheet;                                 // vrati zalozku s lokomotivou adresy \addr, jinak nil
     procedure NewLoko(addr:Word; lok_data:string; total:boolean);              // pozadavek na novou zalozku (novy regulator)

     procedure OnTSClose(Sender:TObject);                                       // event zavreni zalozky zaviracim tlacitkem vpravo nahore zalozky

   public
     constructor Create();
     destructor Destroy(); override;

     function KeyPress(key:Integer):boolean;                                    // keyPress; vraci, jestli byla klavesa zpracovana (zpracovana = true)
     procedure Parse(data:TStrings);                                            // parse zprav pro regulatory (typicky prefix "LOK")
     procedure CloseAll();                                                      // zavrit vsechny regulatory
     procedure UpdateMultitrack(Sender:TCloseTabSheet);                         // aktualizace multitrakce (zmenu vyvolal regulator \Sender), zpropaguj do ostatnich regulatoru

 end;

var
  RegColl:TRegulatorCollector;

implementation

uses HVDb, fNewLoko, Main;

////////////////////////////////////////////////////////////////////////////////

constructor TRegulatorCollector.Create();
begin
 inherited;
 Self.tabs := TList<TCloseTabSheet>.Create();
end;//ctor

destructor TRegulatorCollector.Destroy();
var i:Integer;
begin
 for i := 0 to Self.tabs.Count-1 do
   Self.tabs[i].Free();
 Self.tabs.Free();
 inherited;
end;//dtor

////////////////////////////////////////////////////////////////////////////////

procedure TRegulatorCollector.NewLoko(addr:Word; lok_data:string; total:boolean);
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

 (tab.form as TF_DigiReg).CHB_Multitrack.Checked := true;

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

 F_Main.PC_MainChange(F_Main);
end;

function TRegulatorCollector.GetTab(addr:Word):TCloseTabSheet;
var tab:TCloseTabSheet;
begin
 Result := nil;
 for tab in Self.tabs do
   if ((tab.form as TF_DigiReg).addr = addr) then
     Exit(tab);
end;

procedure TRegulatorCollector.CloseAll();
var i:Integer;
begin
 for i := Self.tabs.Count-1 downto 0 do
  begin
   (Self.tabs[i].form as TF_DigiReg).Free;
   Self.tabs[i].Free;
  end;
 Self.tabs.Clear();
end;

function TRegulatorCollector.KeyPress(key:Integer):boolean;
var i:Integer;
begin
 for i := 0 to Self.tabs.Count-1 do
   if ((Self.tabs[i] = F_Main.PC_Main.ActivePage) and ((Self.tabs[i].form as TF_DigiReg).MyKeyPress(key))) then
     Exit(true);

 Result := false;
end;

////////////////////////////////////////////////////////////////////////////////
// parse dat ze serveru

procedure TRegulatorCollector.Parse(data:TStrings);
var tab:TCloseTabSheet;
begin
 if (data.Count < 4) then
   Exit();
 tab := Self.GetTab(StrToInt(data[2]));

 if (UpperCase(data[3]) = 'AUTH') then
  begin
   if (tab = nil) then
    begin
     if ((data[4] = 'ok') or (data[4] = 'total')) then
      Self.NewLoko(StrToInt(data[2]), data[5], data[4]='total')
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
end;

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

 (Sender as TCloseTabSheet).Visible := false;
 (Sender as TCloseTabSheet).form.Free();
 (Sender as TCloseTabSheet).Free();
end;

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
end;

////////////////////////////////////////////////////////////////////////////////

initialization
  RegColl := TRegulatorCollector.Create();

finalization
  RegColl.Free;

end.//unit
