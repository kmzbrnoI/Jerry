unit fNewLoko;

{
  Okynko zadosti o loko do oblasti rizeni
}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TF_NewLoko = class(TForm)
    LB_Stanice: TListBox;
    StaticText1: TStaticText;
    Label1: TLabel;
    E_Note: TEdit;
    B_Apply: TButton;
    B_Cancel: TButton;
    Label2: TLabel;
    ST_Stav: TStaticText;
    procedure FormShow(Sender: TObject);
    procedure B_ApplyClick(Sender: TObject);
    procedure B_CancelClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure LB_StaniceDblClick(Sender: TObject);
  private
    { Private declarations }
  public
   zadost_probiha:boolean;

    procedure FillStanice();
    procedure ServerResponse(ok:boolean; msg:string);
  end;

var
  F_NewLoko: TF_NewLoko;

implementation

{$R *.dfm}

uses ORList, TCPClientPanel;

procedure TF_NewLoko.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 // pokud probiha zadost, zrusime ji
 if (Self.zadost_probiha) then Self.B_CancelClick(Self);
end;

procedure TF_NewLoko.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
 if ((Self.zadost_probiha) and
  (Application.MessageBox('Opravdu zru�it ��dost o lokomotivu?', 'Opravdu?', MB_YESNO OR MB_ICONQUESTION) = mrNo)) then
   CanClose := false;
end;

procedure TF_NewLoko.FormShow(Sender: TObject);
begin
 Self.E_Note.Text := '';
 Self.LB_Stanice.ItemIndex := -1;
 Self.B_Apply.Enabled  := true;
 Self.B_Cancel.Enabled := false;
 Self.LB_Stanice.Enabled := true;
 Self.E_Note.Enabled := true;
 Self.ST_Stav.Font.Color := clBlack;
 Self.ST_Stav.Caption := '��dost neprob�hla';
 Self.ActiveControl := Self.LB_Stanice;
 Self.zadost_probiha := false;
end;

procedure TF_NewLoko.LB_StaniceDblClick(Sender: TObject);
begin
 B_ApplyClick(B_Apply);
end;

procedure TF_NewLoko.B_ApplyClick(Sender: TObject);
begin
 if (Self.LB_Stanice.ItemIndex = -1) then
  begin
   Application.MessageBox('Vyberte stanici!', 'Nelze pokra�ovat', MB_OK OR MB_ICONWARNING);
   Exit();
  end;

 PanelTCPClient.SendLn('-;LOK;G;PLEASE;'+ORDb.db_reverse[Self.LB_Stanice.Items[Self.LB_Stanice.ItemIndex]]+';'+Self.E_Note.Text);

 Self.LB_Stanice.Enabled := false;
 Self.E_Note.Enabled     := false;
 Self.B_Apply.Enabled    := false;
 Self.B_Cancel.Enabled   := true;
 Self.zadost_probiha     := true;
 Self.ST_Stav.Caption    := 'Odesl�na ��dost ...';
end;

procedure TF_NewLoko.B_CancelClick(Sender: TObject);
begin
 PanelTCPClient.SendLn('-;LOK;G;CANCEL;');

 Self.B_Cancel.Enabled   := false;
 Self.B_Apply.Enabled    := true;
 Self.LB_Stanice.Enabled := true;
 Self.E_Note.Enabled     := true;
 Self.zadost_probiha     := false;
 Self.ST_Stav.Caption    := '��dost zru�ena';
 Self.ST_Stav.Font.Color := clBlack;
end;

procedure TF_NewLoko.FillStanice();
var name:string;
begin
 Self.LB_Stanice.Clear();
 for name in ORDb.names_sorted do
   Self.LB_Stanice.Items.Add(name);
end;

procedure TF_NewLoko.ServerResponse(ok:boolean; msg:string);
begin
 if (Self.ST_Stav.Caption = '��dost zru�ena') then Exit();

 if (ok) then
  begin
   Self.ST_Stav.Font.Color := clGreen;
   Self.ST_Stav.Caption    := 'Server potvrdil ��dost';
  end else begin
   Self.ST_Stav.Font.Color := clRed;
   Self.ST_Stav.Caption    := msg;

   Self.B_Cancel.Enabled   := false;
   Self.B_Apply.Enabled    := true;
   Self.LB_Stanice.Enabled := true;
   Self.E_Note.Enabled     := true;
   Self.zadost_probiha     := false;
  end;
end;

end.//unit
