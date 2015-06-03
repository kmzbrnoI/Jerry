unit GlobalConfig;

// globalni konfigurace SW

interface

uses IniFiles, SysUtils, RPConst, Types, Generics.Collections, Classes;

type
  TServerConfig = record
    host:string;
    port:Word;
    autoconnect:boolean;
  end;

  TAuthConfig = record
    autoauth:boolean;
    username,password:string;
  end;

  TLokArgument = record
    addr:Word;
    token:string;
  end;

  TArguments = record
    username,password, server:string;
    port:Word;
    loks:TList<TLokArgument>;
  end;

  TGlobConfigData = record
    server:TServerConfig;
    auth:TAuthConfig;
    frmPos:TPoint;
    args:TArguments;
  end;

  TGlobConfig = class
    public const
      _DEFAULT_FN = 'config_jerry.ini';

    private
      filename:string;

      procedure ParseArguments();

    public

      data:TGlobConfigData;

      constructor Create();
      destructor Destroy(); override;

      function LoadFile(const filename:string = _DEFAULT_FN):Integer;
      function SaveFile(const filename:string):Integer; overload;
      function SaveFile():Integer; overload;

      property fn:string read filename;
  end;

var
  GlobConfig:TGlobConfig;

implementation

uses TCPClientPanel, main;

////////////////////////////////////////////////////////////////////////////////

constructor TGlobConfig.Create();
begin
 inherited Create();
 Self.data.args.loks := TList<TLokArgument>.Create();
 Self.ParseArguments();
end;//ctor

destructor TGlobConfig.Destroy();
begin
 Self.data.args.loks.Free();
 inherited Destroy();
end;//dtor

////////////////////////////////////////////////////////////////////////////////

function TGlobConfig.LoadFile(const filename:string = _DEFAULT_FN):Integer;
var ini:TMemIniFile;
begin
 try
   ini := TMemIniFile.Create(filename);
 except
   Exit(1);
 end;

 Self.filename := filename;

 Self.data.server.host        := ini.ReadString('server', 'host', 'localhost');
 Self.data.server.port        := ini.ReadInteger('server', 'port', _DEFAULT_PORT);
 Self.data.server.autoconnect := ini.ReadBool('server', 'autoconnect', false);

 Self.data.auth.autoauth      := ini.ReadBool('auth', 'autoauth', false);
 Self.data.auth.username      := ini.ReadString('auth', 'username', '');
 Self.data.auth.password      := ini.ReadString('auth', 'password', '');

 Self.data.frmPos.X := ini.ReadInteger('F_Main', 'X', 0);
 Self.data.frmPos.Y := ini.ReadInteger('F_Main', 'Y', 0);

 Result := 1;
end;//function

function TGlobConfig.SaveFile(const filename:string):Integer;
var ini:TMemIniFile;
begin
 try
   ini := TMemIniFile.Create(filename);
 except
   Exit(1);
 end;


 ini.WriteString('server', 'host', Self.data.server.host);
 ini.WriteInteger('server', 'port', Self.data.server.port);
 ini.WriteBool('server', 'autoconnect', Self.data.server.autoconnect);

 ini.WriteBool('auth', 'autoauth', Self.data.auth.autoauth);
 ini.WriteString('auth', 'username', Self.data.auth.username);
 ini.WriteString('auth', 'password', Self.data.auth.password);

 ini.WriteInteger('F_Main', 'X', Self.data.frmPos.X);
 ini.WriteInteger('F_Main', 'Y', Self.data.frmPos.Y);

 ini.UpdateFile();

 Result := 0;
end;//function

function TGlobConfig.SaveFile():Integer;
begin
 Result := Self.SaveFile(Self.filename);
end;//function

////////////////////////////////////////////////////////////////////////////////

procedure TGlobConfig.ParseArguments();
var i:Integer;
    arg:string;
    data:TStrings;
    lok:TLokArgument;
begin
 data := TStringList.Create();

 i := 1;
 while (i <= ParamCount) do
  begin
   arg := ParamStr(i);
   if ((arg = '-u') and (i < ParamCount-1)) then
    begin
     // parse username
     Inc(i);
     Self.data.args.username := ParamStr(i);
    end else
   if ((arg = '-p') and (i < ParamCount-1)) then
    begin
     // parse password
     Inc(i);
     Self.data.args.password := ParamStr(i);
    end else
   if ((arg = '-s') and (i < ParamCount-1)) then
    begin
     Inc(i);
     Self.data.args.server := ParamStr(i);
    end else
   if ((arg = '-pt') and (i < ParamCount-1)) then
    begin
     Inc(i);
     Self.data.args.port := StrToIntDef(ParamStr(i), _DEFAULT_PORT);
    end else begin
     // parse loko
     try
       data.Clear();
       ExtractStringsEx(':', ParamStr(i), data);
       lok.addr  := StrToInt(data[0]);
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
  end;//while

 data.Free();
end;//procedure

////////////////////////////////////////////////////////////////////////////////

initialization
  GlobConfig := TGlobConfig.Create();

finalization
  FreeAndNil(GlobConfig);

end.//unit
