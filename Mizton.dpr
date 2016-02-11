program Mizton;

uses
  System.StartUpCopy,
  FMX.Forms,
  HeaderFooterTemplate in 'HeaderFooterTemplate.pas' {ULogin},
  Utils in 'Utils.pas',
  MainFormUnit in 'MainFormUnit.pas' {MainForm},
  uFrmCapturaFolio in 'uFrmCapturaFolio.pas' {FrmCapturaFolio},
  UDMConnection in 'UDMConnection.pas' {DMConnection: TDataModule};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TULogin, ULogin);
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TFrmCapturaFolio, FrmCapturaFolio);
  Application.CreateForm(TDMConnection, DMConnection);
  Application.Run;
end.
