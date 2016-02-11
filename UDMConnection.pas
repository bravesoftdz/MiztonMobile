unit UDMConnection;

interface

uses
  System.SysUtils, System.Classes, Data.DB, DBAccess, Uni, Dateutils;

type
  TDMConnection = class(TDataModule)
    UniConnection1: TUniConnection;
    procedure DataModuleDestroy(Sender: TObject);
  private
    { Private declarations }
  public
    IdUSuario: integer;
    { Public declarations }
  end;

var
  DMConnection: TDMConnection;
  GlobalUsuario: integer;

function fechaSQL(Fecha: TDate): string;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

procedure TDMConnection.DataModuleDestroy(Sender: TObject);
begin
  UniConnection1.Disconnect;
end;

function fechaSQL(Fecha: TDate): string;
var
  mes, dia: String;
begin
  if (MonthOf(Fecha)) < 10 then
    mes := '0' + IntToStr(MonthOf(Fecha))
  else
    mes := IntToStr(MonthOf(Fecha));

  if DayOf(Fecha) < 10 then
    dia := '0' +  IntToStr(DayOf(Fecha))
  else
    dia := IntToStr(DayOf(Fecha));

  Result := IntToStr(YearOf(Fecha)) + '-' + mes + '-' + Dia;
end;

end.
