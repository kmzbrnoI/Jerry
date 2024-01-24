unit HVDb;

{
  Databaze hnacich vozidel
}

interface

uses Classes, SysUtils, StdCtrls, ownStrUtils, Generics.Collections;

const
  _MAX_HV = 128;
  _MAX_FUNC = 28;

type
  THVType = (other = -1, steam = 0, diesel = 1, motor = 2, electro = 3,
    car = 4);
  TFunkce = array [0 .. _MAX_FUNC] of boolean;
  THVStanoviste = (lichy = 0, sudy = 1);
  THVFuncType = (permanent = 0, momentary = 1);

  THV = class
  private
    procedure DefaultData();

  public
    name: string;
    owner: string;
    designation: string;
    note: String;
    addr: Word;
    typ: THVType;
    train: string;
    sta: THVStanoviste;
    functions: TFunkce;
    speed_steps: Word;
    speed_kmph: Word;
    dir: Integer;
    station: string;

    funcVyznam: array [0 .. _MAX_FUNC] of string;
    funcType: array [0 .. _MAX_FUNC] of THVFuncType;

    procedure ParseData(data: string);
    constructor Create(data: string); overload;

    class function CharToHVFuncType(c: Char): THVFuncType;
    class function HVFuncTypeToChar(t: THVFuncType): Char;
  end;

implementation

/// /////////////////////////////////////////////////////////////////////////////

constructor THV.Create(data: string);
begin
  inherited Create();
  Self.ParseData(data);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure THV.ParseData(data: string);
var
  str: TStrings;
begin
  str := TStringList.Create();
  ExtractStringsEx(['|'], [], data, str);

  Self.DefaultData();

  try
    Self.name := str[0];
    Self.owner := str[1];
    Self.designation := str[2];
    Self.note := str[3];
    Self.addr := StrToInt(str[4]);
    Self.typ := THVType(StrToInt(str[5]));
    Self.train := str[6];
    Self.sta := THVStanoviste(StrToInt(str[7]));

    for var i := 0 to _MAX_FUNC do
      if (i < Length(str[8])) then
        Self.functions[i] := (str[8][i + 1] = '1');

    Self.speed_steps := StrToInt(str[9]);
    Self.speed_kmph := StrToInt(str[10]);
    Self.dir := StrToInt(str[11]);
    Self.station := str[12];

    // pom-take, pom-release omitted

    // function description
    if (str.Count > 15) then
    begin
      var fdesc: TStrings := TStringList.Create();
      try
        ExtractStringsEx([';'], [], str[15], fdesc);
        for var i := 0 to _MAX_FUNC do
        begin
          if (i < fdesc.Count) then
            Self.funcVyznam[i] := fdesc[i]
          else
            Self.funcVyznam[i] := '';
        end;
      finally
        fdesc.Free();
      end;
    end
    else
    begin
      for var i := 0 to _MAX_FUNC do
        Self.funcVyznam[i] := '';
    end;

    // function types
    if (str.Count > 16) then
    begin
      for var i := 0 to _MAX_FUNC do
      begin
        if (i < Length(str[16])) then
          Self.funcType[i] := CharToHVFuncType(str[16][i + 1])
        else
          Self.funcType[i] := THVFuncType.permanent;
      end;
    end
    else
    begin
      for var i := 0 to _MAX_FUNC do
        Self.funcType[i] := THVFuncType.permanent;
    end;
  except

  end;

  str.Free();
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure THV.DefaultData();
begin
  Self.name := '';
  Self.owner := '';
  Self.designation := '';
  Self.note := '';
  Self.addr := 0;
  Self.typ := THVType.other;
  Self.train := '-';

  for var i := 0 to _MAX_FUNC do
    Self.functions[i] := false;
end;

/// /////////////////////////////////////////////////////////////////////////////

class function THV.CharToHVFuncType(c: Char): THVFuncType;
begin
  if (UpperCase(c) = 'M') then
    Result := THVFuncType.momentary
  else
    Result := THVFuncType.permanent;
end;

class function THV.HVFuncTypeToChar(t: THVFuncType): Char;
begin
  if (t = THVFuncType.momentary) then
    Result := 'M'
  else
    Result := 'P';
end;

/// /////////////////////////////////////////////////////////////////////////////
///
end.// unit
