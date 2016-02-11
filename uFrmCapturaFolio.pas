unit uFrmCapturaFolio;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects,
  FMX.Layouts, FMX.Effects, FMX.Ani, Data.DB, MemDS, DBAccess, Uni, UniProvider,
  MySQLUniProvider, FMX.StdCtrls, FMX.Controls.Presentation, FMX.Edit,udmconnection,
  System.Actions, FMX.ActnList, Data.Bind.EngExt, Fmx.Bind.DBEngExt,
  System.Rtti, System.Bindings.Outputs, Fmx.Bind.Editors, Data.Bind.Components,
  Data.Bind.DBScope;

type
  TFrmCapturaFolio = class(TForm)
    Layout1: TLayout;
    txtFolio: TEdit;
    TxtTelefono: TEdit;
    Label2: TLabel;
    Label4: TLabel;
    MySQLUniProvider1: TMySQLUniProvider;
    uFolio: TUniQuery;
    DetailTopLayout: TLayout;
    DetailTopBackground: TRectangle;
    DetailTopAnimation: TFloatAnimation;
    ShadowEffect1: TShadowEffect;
    Label1: TLabel;
    TxtPrincipal: TEdit;
    ActionBottomLayout: TFlowLayout;
    acCancel: TCircle;
    Image2: TImage;
    ActionBottomAnimation: TFloatAnimation;
    acSave: TCircle;
    Image3: TImage;
    Label3: TLabel;
    TxtSecundario: TEdit;
    MainTopLabel: TLabel;
    ActionList1: TActionList;
    aSave: TAction;
    BindingsList1: TBindingsList;
    DataSource1: TDataSource;
    LinkControlToField1: TLinkControlToField;
    BindSourceDB1: TBindSourceDB;
    LinkControlToField2: TLinkControlToField;
    LinkControlToField3: TLinkControlToField;
    LinkControlToField4: TLinkControlToField;
    aClose: TAction;
    procedure FormShow(Sender: TObject);
    procedure aSaveExecute(Sender: TObject);
    procedure aCloseExecute(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FrmCapturaFolio: TFrmCapturaFolio;

implementation

{$R *.fmx}

procedure TFrmCapturaFolio.aCloseExecute(Sender: TObject);
begin
  Close;
end;

procedure TFrmCapturaFolio.aSaveExecute(Sender: TObject);
begin
  uFolio.FieldByName('IdFolio').AsInteger := 0;
  uFolio.Post;
  ShowMessage('Folio añadido correctamente.');
  Close;
end;

procedure TFrmCapturaFolio.FormShow(Sender: TObject);
begin
  if uFolio.Active then
    uFolio.Refresh
  else
    uFolio.Open;
  uFolio.Append;
end;

end.
