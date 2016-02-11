unit UFrmFolios;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.ListView.Types, FMX.ListView.Appearances, FMX.ListView.Adapters.Base,
  FMX.Objects, FMX.Layouts, FMX.Controls.Presentation, FMX.StdCtrls,
  FMX.Effects, FMX.ListBox, FMX.Ani, FMX.ListView;

type
  TFrmFolios = class(TForm)
    ListView1: TListView;
    OverlayLayout: TLayout;
    DetailTopLayout: TLayout;
    DetailTopBackground: TRectangle;
    DetailTopAnimation: TFloatAnimation;
    DetailComboBox: TComboBox;
    ShadowEffect1: TShadowEffect;
    MainTopLayout: TLayout;
    MainTopBackground: TRectangle;
    MainTopLabel: TLabel;
    MainTopAnimation: TFloatAnimation;
    ActionBottomLayout: TFlowLayout;
    Action1Circle: TCircle;
    Action2Circle: TCircle;
    Action3Circle: TCircle;
    ActionBottomAnimation: TFloatAnimation;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FrmFolios: TFrmFolios;

implementation

{$R *.fmx}
{$R *.iPhone55in.fmx IOS}
{$R *.LgXhdpiPh.fmx ANDROID}
{$R *.NmXhdpiPh.fmx ANDROID}

end.
