unit fSettings;

{
  Okynko nastaveni
}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, ComCtrls, Spin, Hash;

type
  TF_Settings = class(TForm)
    B_Apply: TButton;
    B_Storno: TButton;
    Label4: TLabel;
    PC_Main: TPageControl;
    TS_Server: TTabSheet;
    E_Host: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    CHB_Autoconnect: TCheckBox;
    SE_Port: TSpinEdit;
    TS_Rights: TTabSheet;
    GB_Auth: TGroupBox;
    CHB_RememberAuth: TCheckBox;
    Label14: TLabel;
    E_username: TEdit;
    Label15: TLabel;
    E_Password: TEdit;
    CHB_ShowPassword: TCheckBox;
    Button1: TButton;
    Button2: TButton;
    TS_display: TTabSheet;
    CHB_StayOnTop: TCheckBox;
    procedure B_StornoClick(Sender: TObject);
    procedure B_ApplyClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CHB_RememberAuthClick(Sender: TObject);
    procedure CHB_ShowPasswordClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure E_usernameKeyPress(Sender: TObject; var Key: Char);
    procedure E_PasswordChange(Sender: TObject);
  private
    { Private declarations }
  public
    procedure OpenForm();
  end;

var
  F_Settings: TF_Settings;

implementation

uses GlobalConfig, Main, ownStrUtils;

/// /////////////////////////////////////////////////////////////////////////////

{$R *.dfm}

procedure TF_Settings.B_ApplyClick(Sender: TObject);
begin
  GlobConfig.data.server.host := Self.E_Host.Text;
  GlobConfig.data.server.port := Self.SE_Port.Value;
  GlobConfig.data.server.autoconnect := Self.CHB_Autoconnect.Checked;

  GlobConfig.data.auth.autoauth := Self.CHB_RememberAuth.Checked;
  if (GlobConfig.data.auth.autoauth) then
  begin
    GlobConfig.data.auth.username := Self.E_username.Text;
    if (Self.E_Password.Text <> 'heslo') then
      GlobConfig.data.auth.password := ownStrUtils.pwdHash(Self.E_Password.Text);
  end
  else
  begin
    GlobConfig.data.auth.username := '';
    GlobConfig.data.auth.password := '';
  end;

  GlobConfig.data.stayOnTop := Self.CHB_StayOnTop.Checked;
  F_Main.UpdateFormStyle();

  Self.Close();
end;

procedure TF_Settings.B_StornoClick(Sender: TObject);
begin
  Self.Close();
end;

procedure TF_Settings.CHB_RememberAuthClick(Sender: TObject);
begin
  Self.E_username.Enabled := Self.CHB_RememberAuth.Checked;
  Self.E_Password.Enabled := Self.CHB_RememberAuth.Checked;

  if (not Self.CHB_RememberAuth.Checked) then
  begin
    Self.E_username.Text := '';
    Self.E_Password.Text := '';
  end;
end;

procedure TF_Settings.CHB_ShowPasswordClick(Sender: TObject);
begin
  if (Self.CHB_ShowPassword.Checked) then
    Self.E_Password.PasswordChar := #0
  else
    Self.E_Password.PasswordChar := '*';
end;

procedure TF_Settings.E_PasswordChange(Sender: TObject);
begin
  if (Self.E_Password.Text = '') then
    Self.CHB_ShowPassword.Enabled := true;
end;

procedure TF_Settings.E_usernameKeyPress(Sender: TObject; var Key: Char);
begin
  if ((Key = ';') or (Key = ',')) then
    Key := #0;
end;

procedure TF_Settings.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Self.CHB_ShowPassword.Checked := false;
  Self.E_Password.PasswordChar := '*';
end;

procedure TF_Settings.FormCreate(Sender: TObject);
begin
  Self.PC_Main.ActivePageIndex := 0;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TF_Settings.OpenForm();
var
  data: TGlobConfigData;
begin
  data := GlobConfig.data;

  Self.E_Host.Text := data.server.host;
  Self.SE_Port.Value := data.server.port;
  Self.CHB_Autoconnect.Checked := data.server.autoconnect;

  Self.CHB_RememberAuth.Checked := data.auth.autoauth;
  Self.CHB_RememberAuthClick(Self.CHB_RememberAuth);
  Self.E_username.Text := data.auth.username;
  if (data.auth.password = '') then
    Self.E_Password.Text := ''
  else
    Self.E_Password.Text := 'heslo';

  Self.CHB_ShowPassword.Enabled := (data.auth.password = '');

  Self.CHB_StayOnTop.Checked := data.stayOnTop;

  Self.Show();
end;

/// /////////////////////////////////////////////////////////////////////////////

end.// unit
