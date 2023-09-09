unit GlobalConfig;

{
  Globalni konfigurace SW
}

interface

uses IniFiles, SysUtils, Types, Generics.Collections, Classes, ownStrUtils;

type
  TServerConfig = record
    host: string;
    port: Word;
    autoconnect: boolean;
  end;

  TAuthConfig = record
    autoauth: boolean;
    username, password: string;
  end;

  TLokArgument = record
    addr: Cardinal;
    token: string;
  end;

  TArguments = record
    username, password, server: string;
    autoconnect: boolean;
    port: Word;
    loks: TList<TLokArgument>;
  end;

  TGlobConfigData = record
    server: TServerConfig;
    auth: TAuthConfig;
    args: TArguments;
    stayOnTop: Boolean;
  end;

  TGlobConfig = class
  public const
    _DEFAULT_FN = 'config_jerry.ini';
    _DEFAULT_ARGS: TArguments = (username: ''; password: ''; server: '';
      autoconnect: false; port: 0;);

  private
    filename: string;

    procedure ParseArguments();

  public

    data: TGlobConfigData;

    constructor Create();
    destructor Destroy(); override;

    procedure LoadFile(const filename: string = _DEFAULT_FN);
    procedure SaveFile(const filename: string); overload;
    procedure SaveFile(); overload;

    property fn: string read filename;
  end;

var
  GlobConfig: TGlobConfig;

implementation

uses TCPClientPanel, main;

/// /////////////////////////////////////////////////////////////////////////////

constructor TGlobConfig.Create();
begin
  inherited;
  Self.data.args := _DEFAULT_ARGS;
  Self.data.args.loks := TList<TLokArgument>.Create();
  Self.ParseArguments();
end;

destructor TGlobConfig.Destroy();
begin
  Self.data.args.loks.Free();
  inherited;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TGlobConfig.LoadFile(const filename: string = _DEFAULT_FN);
var
  ini: TMemIniFile;
begin
  ini := TMemIniFile.Create(filename);
  try
    Self.filename := filename;

    Self.data.server.host := ini.ReadString('server', 'host', 'localhost');
    Self.data.server.port := ini.ReadInteger('server', 'port', _DEFAULT_PORT);
    Self.data.server.autoconnect := ini.ReadBool('server', 'autoconnect', false);

    Self.data.auth.autoauth := ini.ReadBool('auth', 'autoauth', false);
    Self.data.auth.username := ini.ReadString('auth', 'username', '');
    Self.data.auth.password := ini.ReadString('auth', 'password', '');

    Self.data.stayOnTop := ini.ReadBool('form', 'stayOnTop', false);
  finally
    ini.Free();
  end;
end;

procedure TGlobConfig.SaveFile(const filename: string);
var
  ini: TMemIniFile;
begin
  ini := TMemIniFile.Create(filename);
  try
    ini.WriteString('server', 'host', Self.data.server.host);
    ini.WriteInteger('server', 'port', Self.data.server.port);
    ini.WriteBool('server', 'autoconnect', Self.data.server.autoconnect);

    ini.WriteBool('auth', 'autoauth', Self.data.auth.autoauth);
    ini.WriteString('auth', 'username', Self.data.auth.username);
    ini.WriteString('auth', 'password', Self.data.auth.password);

    ini.WriteBool('form', 'stayOnTop', Self.data.stayOnTop);

    ini.UpdateFile();
  finally
    ini.Free();
  end;
end;

procedure TGlobConfig.SaveFile();
begin
  Self.SaveFile(Self.filename);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TGlobConfig.ParseArguments();
var
  data: TStrings;
begin
  data := TStringList.Create();
  try
    var i := 1;
    while (i <= ParamCount) do
    begin
      var arg: string := ParamStr(i);
      if (arg = '-a') then
      begin // autoconnect
        Self.data.args.autoconnect := true;
      end
      else if ((arg = '-u') and (i < ParamCount - 1)) then
      begin // username
        Inc(i);
        Self.data.args.username := ParamStr(i);
      end
      else if ((arg = '-p') and (i < ParamCount - 1)) then
      begin // password
        Inc(i);
        Self.data.args.password := ParamStr(i);
      end
      else if ((arg = '-s') and (i < ParamCount - 1)) then
      begin // server hostname
        Inc(i);
        Self.data.args.server := ParamStr(i);
      end
      else if ((arg = '-pt') and (i < ParamCount - 1)) then
      begin // server port
        Inc(i);
        Self.data.args.port := StrToIntDef(ParamStr(i), _DEFAULT_PORT);
      end
      else begin // lokos
        try
          data.Clear();
          ExtractStringsEx([':'], [], ParamStr(i), data);
          var lok: TLokArgument;
          lok.addr := StrToInt(data[0]);
          if (data.Count > 1) then
            lok.token := data[1]
          else
            lok.token := '';
          Self.data.args.loks.Add(lok);
        except
          // chyby jsou ignorovany
        end;
      end;

      Inc(i);
    end; // while
  finally
    data.Free();
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

initialization

GlobConfig := TGlobConfig.Create();

finalization

FreeAndNil(GlobConfig);

end.// unit
