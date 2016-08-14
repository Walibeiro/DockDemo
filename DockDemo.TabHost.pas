unit DockDemo.TabHost;

interface

uses
  System.SysUtils, System.Classes, System.Types, WinApi.Windows,
  WinApi.Messages, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.ComCtrls, DockDemo.Form, Vcl.StdCtrls;

type
  TTabDockHost = class(TDockableForm)
    PageControl: TPageControl;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure PageControlUnDock(Sender: TObject; Client: TControl;
      NewTarget: TWinControl; var Allow: Boolean);
    procedure PageControlGetSiteInfo(Sender: TObject; DockClient: TControl;
      var InfluenceRect: TRect; MousePos: TPoint; var CanDock: Boolean);
    procedure PageControlDockOver(Sender: TObject; Source: TDragDockObject;
      X, Y: Integer; State: TDragState; var Accept: Boolean);
  end;

implementation

{$R *.dfm}

{ TTabDockHost }

procedure TTabDockHost.FormClose(Sender: TObject; var Action: TCloseAction);
var
  ARect: TRect;
begin
  if PageControl.DockClientCount = 1 then
  begin
    with PageControl.DockClients[0] do
    begin
      ARect.TopLeft := ClientToScreen(Point(0, 0));
      ARect.BottomRight := ClientToScreen(Point(UndockWidth, UndockHeight));
      ManualFloat(ARect);
    end;
    Action := caFree;
  end
  else
    Action := caHide;
end;

procedure TTabDockHost.PageControlUnDock(Sender: TObject;
  Client: TControl; NewTarget: TWinControl; var Allow: Boolean);
begin
  // only 2 dock clients means the host must be destroyed and
  // the remaining window undocked to its old position and size.
  if (PageControl.DockClientCount = 2) and (NewTarget <> PageControl) then
    PostMessage(Self.Handle, WM_CLOSE, 0, 0);
end;

procedure TTabDockHost.PageControlGetSiteInfo(Sender: TObject;
  DockClient: TControl; var InfluenceRect: TRect; MousePos: TPoint;
  var CanDock: Boolean);
begin
  CanDock := DockClient is TDockableForm;
end;

procedure TTabDockHost.PageControlDockOver(Sender: TObject;
  Source: TDragDockObject; X, Y: Integer; State: TDragState;
  var Accept: Boolean);
begin
  Accept := Source.Control is TDockableForm;
end;

end.
