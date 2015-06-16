unit HVDb;

interface

uses Classes, SysUtils, StdCtrls, RPConst, Generics.Collections;

const
  _MAX_HV = 128;
  _MAX_FUNC = 12;

type
  THVClass = (parni = 0, diesel = 1, motor = 2, elektro = 3);
  TFunkce = array[0.._MAX_FUNC] of boolean;
  THVStanoviste = (lichy = 0, sudy = 1);              // v jakem smeru se nachazi stanoviste A

  THVPomCV = record                                 // jeden zaznam POM se sklada z
    cv:Word;                                           // oznaceni CV a
    data:Byte;                                         // dat, ktera se maji do CV zapsat.
  end;

  THV = class
   private
     procedure DefaultData();

   public
     Nazev:string;                                       // nazev HV
     Majitel:string;                                     // majitel HV
     Oznaceni:string;                                    // oznaceni HV
     Poznamka:String;                                    // poznamka k HV
     Adresa:Word;
     Trida:THVClass;                                     // trida hnaciho vozidla - parni, diesel, motor, elektro
     Souprava:string;
     StanovisteA:THVStanoviste;                          //0 = lichy; 1 = sudy
     funkce:TFunkce;                                     // stav funkci
     rychlost_stupne:Word;
     rychlost_kmph:Word;
     smer:Integer;

     POMtake : TList<THVPomCV>;                          // seznam POM pri prevzeti do automatu
     POMrelease : TList<THVPomCV>;                       // seznam POM pri uvolneni to rucniho rizeni

     procedure ParseData(data:string);
     constructor Create(data:string); overload;
     constructor Create(); overload;

     function GetPanelLokString():string;
  end;

  THVDb = class
   public
    HVs:array [0.._MAX_HV] of THV;
    count:Integer;

    constructor Create();
    destructor Destroy(); override;

    procedure ParseHVs(data:string);
    procedure ClearList();

    procedure FillHVs(var CB:TComboBox; var Indexes:TWordAr; addr:Integer = -1; special:THV = nil; with_spr:boolean = false);

  end;

implementation

////////////////////////////////////////////////////////////////////////////////

constructor THVDb.Create();
var i:Integer;
begin
 inherited Create();

 for i := 0 to _MAX_HV-1 do
  Self.HVs[i] := nil; 
end;//ctor

destructor THVDb.Destroy();
begin
 Self.ClearList();
 inherited Destroy();
end;//dtor

////////////////////////////////////////////////////////////////////////////////

procedure THVDb.ClearList();
var i:Integer;
begin
 for i := 0 to Self.count-1 do
  if (Assigned(Self.HVs[i])) then
    FreeAndNil(Self.HVs[i]);
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure THVDb.ParseHVs(data:string);
var str:TStrings;
    i:Integer;
begin
 str := TStringList.Create();
 ExtractStringsEx([']'], ['['], data, str);

 Self.ClearList();

 Self.count := str.Count;

 for i := 0 to str.Count-1 do
   Self.HVs[i] := THV.Create(str[i]);

 str.Free();
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure THVDb.FillHVs(var CB:TComboBox; var Indexes:TWordAr; addr:Integer = -1; special:THV = nil; with_spr:boolean = false);
var i,index:Integer;
begin
 CB.Clear();

 if (Assigned(special)) then
  begin
   SetLength(Indexes, Self.count+1);
   CB.Items.Add(IntToStr(special.Adresa) + ' : ' + special.Nazev + ' (' + special.Oznaceni + ')');
   Indexes[0] := special.Adresa;
   if (special.Adresa = addr) then CB.ItemIndex := 0;
   index := 1;
  end else begin
   SetLength(Indexes, Self.count);
   index := 0;
  end;

 for i := 0 to Self.count-1 do
  begin
   if ((Self.HVs[i].Souprava = '-') or (with_spr)) then
    begin
     CB.Items.Add(IntToStr(Self.HVs[i].Adresa) + ' : ' + Self.HVs[i].Nazev + ' (' + Self.HVs[i].Oznaceni + ')');
     Indexes[index] := Self.HVs[i].Adresa;
     if (Self.HVs[i].Adresa = addr) then CB.ItemIndex := i;
     index := index + 1;
    end;
  end;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

constructor THV.Create(data:string);
begin
 inherited Create();
 Self.POMtake    := TList<THVPomCv>.Create();
 Self.POMrelease := TList<THVPomCv>.Create();
 Self.ParseData(data);
end;//ctor

constructor THV.Create();
begin
 Self.POMtake.Free();
 Self.POMrelease.Free();
 inherited Create();
end;//ctor

////////////////////////////////////////////////////////////////////////////////

procedure THV.ParseData(data:string);
var str, str2, str3:TStrings;
    i:Integer;
    pomCv:THVPomCv;
    tmp:string;
begin
 // format zapisu: nazev|majitel|oznaceni|poznamka|adresa|trida|souprava|stanovisteA|funkce|rychlost_stupne|rychlost_kmph|smer|{[{cv1take|cv1take-value}][{...}]...}|{[{cv1release|cv1release-value}][{...}]...}|
 // souprava je bud cislo soupravy, nebo znak '-'
 str  := TStringList.Create();
 str2 := TStringList.Create();
 str3 := TStringList.Create();
 ExtractStringsEx(['|'] , [], data, str);

 Self.DefaultData();

 try
  Self.Nazev        := str[0];
  Self.Majitel      := str[1];
  Self.Oznaceni     := str[2];
  Self.Poznamka     := str[3];
  Self.Adresa       := StrToInt(str[4]);
  Self.Trida        := THvClass(StrToInt(str[5]));
  Self.Souprava     := str[6];
  Self.StanovisteA  := THVStanoviste(StrToInt(str[7]));

  for i := 0 to _MAX_FUNC do
   begin
    if (i < Length(str[8])) then
      if (str[8][i+1] = '1') then
        Self.funkce[i] := true
      else
        Self.funkce[i] := false;
   end;

   Self.rychlost_stupne := StrToInt(str[9]);
   Self.rychlost_kmph   := StrToInt(str[10]);
   Self.smer            := StrToInt(str[11]);

   if (str.Count > 11) then
    begin
     // pom-take
     ExtractStringsEx([']'] , ['['], str[12], str2);
     for tmp in str2 do
      begin
       ExtractStringsEx(['|'] , [], tmp, str3);
       pomCV.cv := StrToInt(str3[0]);
       pomCV.cv := StrToInt(str3[1]);
       Self.POMtake.Add(pomCV);
      end;

     // pom-release
     ExtractStringsEx([']'] , ['['], str[13], str2);
     for tmp in str2 do
      begin
       ExtractStringsEx(['|'] , [], tmp, str3);
       pomCV.cv := StrToInt(str3[0]);
       pomCV.cv := StrToInt(str3[1]);
       Self.POMrelease.Add(pomCV);
      end;
    end;//if str.Count > 11

 except

 end;

 str.Free();
 str2.Free();
 str3.Free();
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure THV.DefaultData();
var i:Integer;
begin
 Self.Nazev     := '';
 Self.Majitel   := '';
 Self.Oznaceni  := '';
 Self.Poznamka  := '';
 Self.Adresa    := 0;
 Self.Trida     := THvClass.diesel;
 Self.Souprava  := '-';

 Self.POMtake.Clear();
 self.POMrelease.Clear();

 for i := 0 to _MAX_FUNC do
   Self.funkce[i] := false;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

function THV.GetPanelLokString():string;
var i:Integer;
begin
 // format zapisu: nazev|majitel|oznaceni|poznamka|adresa|trida|souprava|stanovisteA|funkce
 // souprava je bud cislo soupravy, nebo znak '-'
 Result := Self.Nazev + '|' + Self.Majitel + '|' + Self.Oznaceni + '|' + Self.Poznamka + '|' +
           IntToStr(Self.adresa) + '|' + IntToStr(Integer(Self.Trida)) + '|' + Self.souprava + '|' +
           IntToStr(Integer(Self.StanovisteA)) + '|';

 for i := 0 to _MAX_FUNC do
  begin
   if (Self.funkce[i]) then
     Result := Result + '1'
   else
     Result := Result + '0';
  end;
end;//function

////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////

end.//unit
