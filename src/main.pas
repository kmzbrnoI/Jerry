﻿unit main;

{
  Unit hlavniho okynka, tady se resi predevsim interakce s GUI.
}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ActnList, CloseTabSheet, Themes, StdCtrls, Buttons,
  AppEvnts, StrUtils, Menus, System.Actions;

type
  TF_Main = class(TForm)
    SB_Main: TStatusBar;
    AL_Main: TActionList;
    A_Connect: TAction;
    A_Disconnect: TAction;
    PC_Main: TPageControl;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    AE_Main: TApplicationEvents;
    A_Debug: TAction;
    A_About: TAction;
    procedure A_ConnectExecute(Sender: TObject);
    procedure A_DisconnectExecute(Sender: TObject);

    procedure PageControlCloseButtonDrawTab(Control: TCustomTabControl;
      TabIndex: Integer; const Rect: TRect; Active: Boolean);
    procedure PageControlCloseButtonMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure PageControlCloseButtonMouseLeave(Sender: TObject);
    procedure PageControlCloseButtonMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer);
    procedure PageControlCloseButtonMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure PageControlDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure PageControlDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);

    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure BitBtn2Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure AE_MainMessage(var Msg: tagMSG; var Handled: Boolean);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure PC_MainChange(Sender: TObject);
    procedure A_AboutExecute(Sender: TObject);
    procedure A_DebugExecute(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure UpdateFormStyle();

  private
    FCloseButtonMouseDownTab: TCloseTabSheet;
    FCloseButtonShowPushed: Boolean;

  public
    close_app: Boolean;

     procedure OnAppException(Sender: TObject; E: Exception);

  end;

var
  F_Main: TF_Main;

implementation

{$R *.dfm}

uses TCPClientPanel, GlobalConfig, fSettings, fNewLoko, fDebug, RegCollector,
  Verze, Regulator, System.Types, System.UITypes;

/// /////////////////////////////////////////////////////////////////////////////
// Systemove zpravy, pouzito pro odchyceni stisku klavesy

procedure TF_Main.AE_MainMessage(var Msg: tagMSG; var Handled: Boolean);
begin
  if (Msg.message = WM_KeyDown) then
  begin
    if ((Self.Active) and (RegColl.KeyPress(Msg.wParam))) then
      Handled := true;

    // sem muze prijit zpracovani dalsich klaves
  end; // WM_KeyDown
end;

/// /////////////////////////////////////////////////////////////////////////////
// Pripojit se k serveru

procedure TF_Main.A_AboutExecute(Sender: TObject);
begin
  Application.MessageBox(PChar('Jerry v' + VersionStr(Application.ExeName) +
    #13#10 + 'build ' + FormatDateTime('dd.mm.yyyy hh:nn:ss', BuildDateTime()) + #13#10 +
    'Vytvořil Jan Malina (Horáček) 2015-2025 v KMŽ Brno I.'), 'Info',
    MB_OK OR MB_ICONINFORMATION);
end;

procedure TF_Main.A_ConnectExecute(Sender: TObject);
var
  host: string;
  port: Word;
begin
  F_Main.SB_Main.Panels[0].Text := 'Připojování ...';
  Screen.Cursor := crHourGlass;
  Application.ProcessMessages();

  // byl hostname predan jako argument?
  if (GlobConfig.data.args.server <> TGlobConfig._DEFAULT_ARGS.server) then
    host := GlobConfig.data.args.server
  else
    host := GlobConfig.data.server.host;

  // byl port predan jako argument?
  if (GlobConfig.data.args.port <> TGlobConfig._DEFAULT_ARGS.port) then
    port := GlobConfig.data.args.port
  else
    port := GlobConfig.data.server.port;

  try
    PanelTCPClient.Connect(host, port);
  except
    on E: Exception do
    begin
      Application.MessageBox(PChar('Připojení se nezdařilo' + #13#10 +
        E.message), 'Nepřipojeno', MB_OK OR MB_ICONWARNING);
      F_Main.SB_Main.Panels[0].Text := 'Odpojeno od serveru';
    end;
  end;

  Screen.Cursor := crDefault;
end;

/// /////////////////////////////////////////////////////////////////////////////
// Odpojit se od serveru

procedure TF_Main.A_DebugExecute(Sender: TObject);
begin
  F_Debug.Show();
end;

procedure TF_Main.A_DisconnectExecute(Sender: TObject);
begin
  F_Main.SB_Main.Panels[0].Text := 'Odpojování ...';
  Screen.Cursor := crHourGlass;
  Application.ProcessMessages();
  PanelTCPClient.Disconnect();
  Screen.Cursor := crDefault;
end;

/// /////////////////////////////////////////////////////////////////////////////
// Zadost o lokomotivu do oblasti rizeni

procedure TF_Main.BitBtn1Click(Sender: TObject);
begin
  if (PanelTCPClient.status <> TPanelConnectionStatus.opened) then
  begin
    Application.MessageBox
      (PChar('Program není připojen k serveru, nelze žádat o lokomotivu.' +
      #13#10 + 'Připojit k serveru se můžete v okně nastavení.'),
      'Nelze žádat o lokomotivu', MB_OK OR MB_ICONWARNING);
    Exit();
  end;

  F_NewLoko.Show();
end;

/// /////////////////////////////////////////////////////////////////////////////
// Zobrazeni okna nastaveni

procedure TF_Main.BitBtn2Click(Sender: TObject);
begin
  F_Settings.OpenForm();
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TF_Main.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  F_NewLoko.zadost_probiha := false;
end;

procedure TF_Main.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if ((PanelTCPClient.status <> TPanelConnectionStatus.closed) and
    (not Self.close_app)) then
  begin
    Self.close_app := true; // informujeme OnDisconnect, ze ma zavrit okno
    PanelTCPClient.Disconnect();
    CanClose := false; // okno zatim nezavirame, zavre se az pri OnDisconnect
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TF_Main.FormCreate(Sender: TObject);
begin
  try
    GlobConfig.LoadFile();
    Self.UpdateFormStyle();
  except

  end;
end;

procedure TF_Main.FormDestroy(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;

  if (Assigned(GlobConfig)) then
  begin
    try
      GlobConfig.SaveFile();
    except

    end;
  end;
end;

procedure TF_Main.FormResize(Sender: TObject);
begin
  if (Self.WindowState = TWindowState.wsMinimized) then
    RegColl.IdleAllRuc();
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TF_Main.FormShow(Sender: TObject);
begin
  Self.Caption := 'Jerry v' + VersionStr(Application.ExeName) + ' (build ' +
    FormatDateTime('dd.mm.yyyy', BuildDateTime()) + ')';
end;

/// /////////////////////////////////////////////////////////////////////////////
/// ///////// PAGE CONTROL CLOSE BUTTON DRAWING AND HANDLING ////////////////////
/// /////////////////////////////////////////////////////////////////////////////

procedure TF_Main.PageControlCloseButtonDrawTab(Control: TCustomTabControl;
  TabIndex: Integer; const Rect: TRect; Active: Boolean);
var
  CloseBtnSize: Integer;
  PageControl: TPageControl;
  TabSheet: TCloseTabSheet;
  TabCaption: TPoint;
  CloseBtnRect: TRect;
  CloseBtnDrawDetails: TThemedElementDetails;
begin
  PageControl := Control as TPageControl;
  TabCaption.Y := Rect.Top + 3;

  Control.Canvas.Brush.Color := ((PageControl.Pages[TabIndex] as TCloseTabSheet).form as TF_DigiReg).TabBgColor();
  PageControl.Pages[TabIndex].Brush.Color := Control.Canvas.Brush.Color;

  if Active then
  begin
    CloseBtnRect.Top := Rect.Top + 4;
    CloseBtnRect.Right := Rect.Right - 5;
    TabCaption.X := Rect.Left + 6;
  end else begin
    CloseBtnRect.Top := Rect.Top + 3;
    CloseBtnRect.Right := Rect.Right - 5;
    TabCaption.X := Rect.Left + 3;
  end;

  if (PageControl.Pages[TabIndex] is TCloseTabSheet) then
  begin
    TabSheet := PageControl.Pages[TabIndex] as TCloseTabSheet;
    CloseBtnSize := 14;

    CloseBtnRect.Bottom := CloseBtnRect.Top + CloseBtnSize;
    CloseBtnRect.Left := CloseBtnRect.Right - CloseBtnSize;
    TabSheet.FCloseButtonRect := CloseBtnRect;

    PageControl.Canvas.FillRect(Rect);
    PageControl.Canvas.TextOut(TabCaption.X, TabCaption.Y,
      PageControl.Pages[TabIndex].Caption);

    Dec(TabSheet.FCloseButtonRect.Left);

    if (FCloseButtonMouseDownTab = TabSheet) and FCloseButtonShowPushed then
      CloseBtnDrawDetails := StyleServices.GetElementDetails
        (twCloseButtonPushed)
    else
      CloseBtnDrawDetails := StyleServices.GetElementDetails
        (twCloseButtonNormal);

    StyleServices.DrawElement(PageControl.Canvas.Handle, CloseBtnDrawDetails,
      TabSheet.FCloseButtonRect);
  end else begin
    PageControl.Canvas.FillRect(Rect);
    PageControl.Canvas.TextOut(TabCaption.X, TabCaption.Y,
      PageControl.Pages[TabIndex].Caption);
  end;
end;

procedure TF_Main.PageControlCloseButtonMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  PageControl: TPageControl;
begin
  PageControl := Sender as TPageControl;

  if Button = mbLeft then
  begin
    for var I := 0 to PageControl.PageCount - 1 do
    begin
      if not(PageControl.Pages[I] is TCloseTabSheet) then
        Continue;
      var TabSheet := PageControl.Pages[I] as TCloseTabSheet;
      if PtInRect(TabSheet.FCloseButtonRect, Point(X, Y)) then
      begin
        FCloseButtonMouseDownTab := TabSheet;
        FCloseButtonShowPushed := true;
        PageControl.Repaint;
      end;
    end;
  end;

  Self.PC_Main.BeginDrag(false);
end;

procedure TF_Main.PageControlCloseButtonMouseLeave(Sender: TObject);
var
  PageControl: TPageControl;
begin
  PageControl := Sender as TPageControl;
  FCloseButtonShowPushed := false;
  PageControl.Repaint;
end;

procedure TF_Main.PageControlCloseButtonMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  PageControl: TPageControl;
begin
  PageControl := Sender as TPageControl;

  if (ssLeft in Shift) and Assigned(FCloseButtonMouseDownTab) then
  begin
    var Inside := PtInRect(FCloseButtonMouseDownTab.FCloseButtonRect, Point(X, Y));

    if FCloseButtonShowPushed <> Inside then
    begin
      FCloseButtonShowPushed := Inside;
      PageControl.Repaint;
    end;
  end;
end;

procedure TF_Main.PageControlCloseButtonMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  PageControl: TPageControl;
begin
  PageControl := Sender as TPageControl;

  if (Button = mbLeft) and Assigned(FCloseButtonMouseDownTab) then
  begin
    if PtInRect(FCloseButtonMouseDownTab.FCloseButtonRect, Point(X, Y)) then
    begin
      FCloseButtonMouseDownTab.DoClose;
      FCloseButtonMouseDownTab := nil;
      PageControl.Repaint();
      if (Assigned(PageControl.OnChange)) then
        PageControl.OnChange(PageControl);
    end;
  end;
end;

procedure TF_Main.PageControlDragDrop(Sender, Source: TObject; X, Y: Integer);
const
  TCM_GETITEMRECT = $130A;
var
  TabRect: TRect;
begin
  if (Sender is TPageControl) then
    for var j := 0 to Self.PC_Main.PageCount - 1 do
    begin
      Self.PC_Main.Perform(TCM_GETITEMRECT, j, LParam(@TabRect));
      if PtInRect(TabRect, Point(X, Y)) then
      begin
        if Self.PC_Main.ActivePage.PageIndex <> j then
          Self.PC_Main.ActivePage.PageIndex := j;
        Exit;
      end;
    end;
end;

procedure TF_Main.PageControlDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  if (Sender is TPageControl) then
    Accept := true;
end;

procedure TF_Main.PC_MainChange(Sender: TObject);
begin
  if (Self.PC_Main.ActivePage <> nil) then
    Self.Caption := LeftStr(Self.PC_Main.ActivePage.Caption, Length(Self.PC_Main.ActivePage.Caption) - 5) + '– '
  else
    Self.Caption := '';

  Self.Caption := Self.Caption + 'Jerry v' + VersionStr(Application.ExeName) + ' (build ' + FormatDateTime('dd.mm.yyyy', BuildDateTime()) + ')';
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TF_Main.UpdateFormStyle();
begin
  if (GlobConfig.data.stayOnTop) then
    Self.FormStyle := fsStayOnTop
  else
    Self.FormStyle := fsNormal;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TF_Main.OnAppException(Sender: TObject; E: Exception);
begin
  try
    if ((Assigned(PanelTCPClient)) and (PanelTCPClient.status <> TPanelConnectionStatus.closed)) then
      PanelTCPClient.Disconnect();
    if (Assigned(Self)) then
      Self.SB_Main.Panels.Items[1].Text := 'AppException: ' + E.Message;
  except

  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

end.// unit
