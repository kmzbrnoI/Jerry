unit RPConst;
//deklarace konstant programu
interface

uses Classes;


type

  TWordAr = array of Word;

  procedure ExtractStringsEx(Separator: Char; Content: string; Strings: TStrings);

implementation


function StrToBool(str:string):boolean;
begin
 if (str = '1') then Result := true
 else Result := false;
end;//function

procedure ExtractStringsEx(Separator: Char; Content: string; Strings: TStrings);
var i: word;
    s: string;
 begin
  s := '';
  if (Length(Content) = 0) then
   begin
    Exit;
   end;
  for i := 1 to Length(Content) do
   begin
    if (Content[i] = Separator) then
     begin
      Strings.Add(s);
      s := '';
     end else begin
      s := s + Content[i];
     end;
   end;
  Strings.Add(s);
end;

end.//unit
