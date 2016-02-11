unit MainFormUnit;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.ListView.Types, Data.Bind.GenData, System.Rtti, System.Bindings.Outputs,
  Fmx.Bind.Editors, Data.Bind.EngExt, Fmx.Bind.DBEngExt, Data.Bind.Components,
  Data.Bind.ObjectScope, FMX.ListView, FMX.Objects, FMX.Layouts, FMX.StdCtrls,
  FMX.Ani, FMX.ListBox, System.Actions, FMX.ActnList
  , Generics.Collections, FMX.Effects, FMX.ListView.Appearances,
  FMX.ListView.Adapters.Base, FMX.Controls.Presentation, Data.Bind.DBScope,
  Data.DB, DBAccess, Uni, MemDS, UniProvider, MySQLUniProvider,
  System.ImageList, FMX.ImgList, udmConnection, Fmx.Bind.GenData,
  FMX.DateTimeCtrls, Dateutils, FMX.MultiView, FMX.TabControl;

type
  TOverlayInfo=record
    Control: TControl;
    Animation: TFloatAnimation;
    Trigger: Integer;
    InitialMargin: Integer;
    Top: Boolean;
    LastPos: Single;
  end;


  TMainForm = class(TForm)
    lvFolios: TListView;
    MainTopLayout: TLayout;
    MainTopBackground: TRectangle;
    MainTopAnimation: TFloatAnimation;
    MainTopLabel: TLabel;
    DetailTopLayout: TLayout;
    DetailTopAnimation: TFloatAnimation;
    ActionBottomLayout: TFlowLayout;
    Action1Circle: TCircle;
    Action1Icon: TImage;
    Action2Circle: TCircle;
    ActionBottomAnimation: TFloatAnimation;
    ActionList1: TActionList;
    ShadowEffect1: TShadowEffect;
    Image1: TImage;
    BindingsList1: TBindingsList;
    LinkListFolio: TLinkListControlToField;
    DataSource1: TDataSource;
    MySQLUniProvider1: TMySQLUniProvider;
    uFoliosxTecnicos: TUniQuery;
    ImageList1: TImageList;
    Action4: TAction;
    aAdd: TAction;
    Panel1: TPanel;
    DateDesde: TDateEdit;
    Label1: TLabel;
    Label2: TLabel;
    DateHasta: TDateEdit;
    Action3Circle: TCircle;
    Image2: TImage;
    TabControl1: TTabControl;
    TabQuejas: TTabItem;
    TabLiquidadas: TTabItem;
    TabObjetadas: TTabItem;
    OverlayLayout: TLayout;
    PrototypeBindSource1: TPrototypeBindSource;
    BindSourceDB1: TBindSourceDB;
    acRefresh: TCircle;
    Image3: TImage;
    acSearch: TCircle;
    Image4: TImage;
    procedure Action1Execute(Sender: TObject);
    procedure Action2Execute(Sender: TObject);
    procedure Action3Execute(Sender: TObject);
    procedure lvFoliosPaint(Sender: TObject; Canvas: TCanvas;
      const ARect: TRectF);
    procedure Action4Execute(Sender: TObject);
    procedure aAddExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure DateHastaChange(Sender: TObject);
    procedure DateDesdeChange(Sender: TObject);
    procedure acSearchClick(Sender: TObject);
  private
    { Private declarations }
    FOverlayInitialized: Boolean;
    FMainTopInfo, FDetailTopInfo, FActionBottomInfo: TOverlayInfo;
    FLastScrollViewPos: Single;
    procedure InitOverlay;
    procedure OverlayAnimation(AInfo: TOverlayInfo);

  public
    constructor Create(AOwner: TComponent); override;

    { Public declarations }
  end;

var
  MainForm: TMainForm;
  GlobalLastPos: Single = 0;

implementation

uses
  UfrmcapturaFolio;

{$R *.fmx}

procedure TMainForm.aAddExecute(Sender: TObject);
begin
  application.CreateForm(TFrmcapturaFolio, FrmCapturaFolio);
  FrmCapturaFolio.show;
end;

procedure TMainForm.acSearchClick(Sender: TObject);
begin
  lvFolios.SearchVisible := Not lvFolios.SearchVisible;
end;

procedure TMainForm.Action1Execute(Sender: TObject);
begin
  ShowMessage('Action 1');
end;

procedure TMainForm.Action2Execute(Sender: TObject);
begin
  ShowMessage('Action 2');
end;

procedure TMainForm.Action3Execute(Sender: TObject);
begin
  ShowMessage('Action 3');
end;

procedure TMainForm.Action4Execute(Sender: TObject);
begin
  if uFoliosxTecnicos.Active then
    uFoliosxTecnicos.Refresh
  else
    uFoliosxTecnicos.Open;
end;

constructor TMainForm.Create(AOwner: TComponent);
begin
  FOverlayInitialized := False;

  inherited;
end;

procedure TMainForm.DateDesdeChange(Sender: TObject);
var
  Inicio, Termino: String;
begin
  DateDesde.Date := StartOfTheWeek(DateDesde.Date);
  DateHasta.Date := EndOfTheWeek(DateDesde.Date);

  Inicio := FechaSQL(DateDesde.Date);
  Termino := FechaSQL(DateHasta.Date);

  uFoliosxTecnicos.ParamByName('IdPersonal').AsInteger := Globalusuario;
  uFoliosxTecnicos.ParamByName('Desde').AsString := Inicio;
  uFoliosxTecnicos.ParamByName('Hasta').AsString := Termino;

  if uFoliosxTecnicos.Active then
    uFoliosxTecnicos.Refresh
  else
    uFoliosxTecnicos.Open;
end;

procedure TMainForm.DateHastaChange(Sender: TObject);
var
  inicio, Termino: String;
begin
  DateDesde.Date := StartOfTheWeek(DateHasta.date);
  DateHasta.Date := EndOfTheWeek(DateHasta.Date);

  Inicio := FechaSQL(DateDesde.Date);
  Termino := FechaSQL(DateHasta.Date);

  uFoliosxTecnicos.ParamByName('IdPersonal').AsInteger := Globalusuario;
  uFoliosxTecnicos.ParamByName('Desde').AsString := Inicio;
  uFoliosxTecnicos.ParamByName('Hasta').AsString := Termino;

  if uFoliosxTecnicos.Active then
    uFoliosxTecnicos.Refresh
  else
    uFoliosxTecnicos.Open;
end;

procedure TMainForm.FormShow(Sender: TObject);
var
  Cursor: TCursor;
  inicio, Termino: string;
  IdPersonal: Integer;
begin
  try
    try
      DateDesde.Date := StartOfTheWeek(Now);
      DateHasta.Date := EndOfTheWeek(Now);

      Inicio := FechaSQL(DateDesde.Date);
      Termino := FechaSQL(DateHasta.Date);

      IdPersonal := GlobalUsuario;
      uFoliosxTecnicos.ParamByName('IdPersonal').AsInteger := IdPersonal;
      uFoliosxTecnicos.ParamByName('Desde').AsString := Inicio;
      uFoliosxTecnicos.ParamByName('Hasta').AsString := Termino;

      if uFoliosxTecnicos.Active then
        uFoliosxTecnicos.Refresh
      else
        uFoliosxTecnicos.Open;
      ActionBottomLayout.Visible := False;
    finally
      ;
    end;
  Except
    on e: Exception do
      ShowMessage('Ha ocurrido el siguiente error: ' + e.Message);
  end;
end;

procedure TMainForm.InitOverlay;
begin
//  FMainTopInfo.Control := MainTopLayout;
//  FMainTopInfo.Animation := MainTopAnimation;
//  FMainTopInfo.Trigger := 32;
//  FMainTopInfo.InitialMargin := 0;
//  FMainTopInfo.Top := True;
//  FMainTopInfo.LastPos := 0;
//
//  FDetailTopInfo.Control := DetailTopLayout;
//  FDetailTopInfo.Animation := DetailTopAnimation;
//  FDetailTopInfo.Trigger := 64;
//  FDetailTopInfo.InitialMargin := 0;
//  FDetailTopInfo.Top := True;
//  FDetailTopInfo.LastPos := 0;
//
//  FActionBottomInfo.Control := ActionBottomLayout;
//  FActionBottomInfo.Animation := ActionBottomAnimation;
//  FActionBottomInfo.Trigger := 32;
//  FActionBottomInfo.InitialMargin := 10;
//  FActionBottomInfo.Top := False;
//  FActionBottomInfo.LastPos := 0;
//
//  FOverlayInitialized := True;
end;

procedure TMainForm.lvFoliosPaint(Sender: TObject; Canvas: TCanvas;
  const ARect: TRectF);
begin
//  if not FOverlayInitialized then
//    InitOverlay;
//
//  OverlayAnimation(FMainTopInfo);
//  OverlayAnimation(FDetailTopInfo);
//  OverlayAnimation(FActionBottomInfo);
end;

procedure TMainForm.OverlayAnimation(AInfo: TOverlayInfo);
var
  LCurrentValue: Single;
  LNewValue: Single;
begin
//  if AInfo.Top then
//    LCurrentValue := AInfo.Control.Margins.Top
//  else
//    LCurrentValue := AInfo.Control.Margins.Bottom;
//
//  if AInfo.Animation.Running then
//    Exit;
//
////  MainForm.Caption := (ListView1.ScrollViewPos - GlobalLastPos).toString;
//
//  if ListView1.ScrollViewPos > (FLastScrollViewPos + 1) then // DOWNWARD
//  begin
//    if GlobalLastPos > ListView1.ScrollViewPos then
//      GlobalLastPos := ListView1.ScrollViewPos;
//
//    if ListView1.ScrollViewPos - GlobalLastPos > AInfo.Trigger then
//    begin
//      LNewValue := -(AInfo.Control.Height + AInfo.InitialMargin);
//      if (LCurrentValue <> LNewValue) then
//      begin
//        AInfo.Animation.StopValue := LNewValue;
//        AInfo.Animation.Start;
//        GlobalLastPos := ListView1.ScrollViewPos;
//      end;
//    end;
//  end
//  else if ListView1.ScrollViewPos < (FLastScrollViewPos - 1) then
//  begin // UPWARD
//    LNewValue := AInfo.InitialMargin;
//    if (LCurrentValue <> LNewValue) then
//    begin
//      AInfo.Animation.StopValue := LNewValue;
//      AInfo.Animation.Start;
//      GlobalLastPos := ListView1.ScrollViewPos;
//    end;
//  end;
//
//  FLastScrollViewPos := ListView1.ScrollViewPos; // store current value for next step
end;

end.
