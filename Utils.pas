unit Utils;


interface

 uses System.SysUtils;

function desencriptar(aStr: String; aKey: Integer): String;
function encriptar(aStr: String; aKey: Integer): String;

implementation

function desencriptar(aStr: String; aKey: Integer): String;
begin
  Result:='';
  RandSeed:=aKey;
  for aKey:=0 to Length(aStr)-1 do
     Result:=Result+Chr(Byte(aStr[aKey]) xor random(256));
end;

function encriptar(aStr: String; aKey: Integer): String;
begin
  Result:='';
  RandSeed:=aKey;
  for aKey:= 0 to Length(aStr)-1 do
     Result:=Result+Chr(Strtoint(Copy(aStr[aKey], 0, pos(#39,aStr[aKey]))){Byte(aStr[aKey])} xor random(256));
end;

end.
