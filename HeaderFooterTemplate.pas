unit HeaderFooterTemplate;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Edit, FMX.Layouts, FMX.Controls.Presentation, Data.DB, MemDS, DBAccess,
  Uni, UniProvider, MySQLUniProvider, Utils, FMX.Objects, UDMConnection;

type
  TULogin = class(TForm)
    Layout1: TLayout;
    txtUsuario: TEdit;
    TxtContrasena: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    MySQLUniProvider1: TMySQLUniProvider;
    uUsuario: TUniQuery;
    Button1: TButton;
    Label3: TLabel;
    Label4: TLabel;
    Image1: TImage;
    ScrollBox1: TScrollBox;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ULogin: TULogin;

implementation

uses MainFormUnit;
{$R *.fmx}

procedure TULogin.Button1Click(Sender: TObject);
var
  IdUsuario, IdPersonal: Integer;
  usuario: String;
begin
  uUsuario.parambyName('Usuario').asString := txtUsuario.text;

  if uUsuario.Active then
    uUsuario.Refresh
  else
    uUsuario.Open;

  Usuario := uUsuario.FieldByName('Usuario').AsString;
  IdUsuario := uUsuario.FieldByName('IdUsuario').AsInteger;
  IdPersonal := uUsuario.FieldByName('IdPersonal').AsInteger;

  if (UUsuario.recordcount = 1) and (uUsuario.FieldByName('Contrasena').AsString = (TxtContrasena.Text)) then
  begin
    if uUsuario.FieldByName('IdPersonal').AsInteger = -9 then
      ShowMessage('El usuario ingresado no tiene permiso de acceder a esta aplicación.')
    else
    begin
      ShowMessage('Autenticación exitosa');
      globalUsuario := uUsuario.FieldByName('IdPersonal').AsInteger;
      application.createForm(TmainForm,MainForm);
      MainForm.Show;
    end;
  end
  else
    ShowMessage('El usuario o contraseña son inválidos.')
end;

end.
