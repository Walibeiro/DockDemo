unit DockDemo.Host;

interface

uses
  System.SysUtils, System.Classes, System.Types, WinApi.Windows,
  WinApi.Messages, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  DockDemo.Form;

type
  TFormDockHost = class(TForm)
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDockDrop(Sender: TObject; Source: TDragDockObject;
      X, Y: Integer);
    procedure FormUnDock(Sender: TObject; Client: TControl;
      NewTarget: TWinControl; var Allow: Boolean);
    procedure FormDockOver(Sender: TObject; Source: TDragDockObject;
      X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure FormGetSiteInfo(Sender: TObject; DockClient: TControl;
      var InfluenceRect: TRect; MousePos: TPoint; var CanDock: Boolean);
  private
    procedure DoFloat(AControl: TControl);
  public
    procedure UpdateCaption(Exclude: TControl);
  end;

implementation

{$R *.dfm}

{ TFormDockHost }

procedure TFormDockHost.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if DockClientCount = 1 then
  begin
    DoFloat(DockClients[0]);
    Action := caFree;
  end
  else
    Action := caHide;
end;

procedure TFormDockHost.DoFloat(AControl: TControl);
var
  ARect: TRect;
begin
  // float the control with its original size.
  ARect.TopLeft := AControl.ClientToScreen(Point(0, 0));
  ARect.BottomRight := AControl.ClientToScreen(Point(AControl.UndockWidth,
    AControl.UndockHeight));
  AControl.ManualFloat(ARect);
end;

procedure TFormDockHost.UpdateCaption(Exclude: TControl);
var
  Index: Integer;
begin
  // if a dockable form is undocking, it will pass itself in as Exclude
  // because even it hasn't actually been taken out of the DockClient array
  // at this point.
  Caption := '';
  for Index := 0 to DockClientCount - 1 do
    if DockClients[Index].Visible and (DockClients[Index] <> Exclude) then
      Caption := Caption + TCustomForm(DockClients[Index]).Caption + ' ';
end;

procedure TFormDockHost.FormDockDrop(Sender: TObject;
  Source: TDragDockObject; X, Y: Integer);
begin
  UpdateCaption(nil);

  // Force DockManager to redraw it's clients.
  DockManager.ResetBounds(True);
end;

{
  The following example is taken from the docking demo. It shows how the
  OnUnDock event handler of the conjoinment docking site re-enables docking in
  the control that is undocked (if it is a dockable form). In addition, when
  the next-to-last docked control is undocked, the conjoinment docking site
  sends itself a close message so that the last docked control is undocked to
  its old position and size.
}

procedure TFormDockHost.FormUnDock(Sender: TObject; Client: TControl;
  NewTarget: TWinControl; var Allow: Boolean);
begin
  // only 2 dock clients means the host must be destroyed and
  // the remaining window undocked to its old position and size.
  // (Recall that OnUnDock gets called before the undocking actually occurs)
  if Client is TFormDockable then
    TFormDockable(Client).DockSite := True;
  if (DockClientCount = 2) and (NewTarget <> Self) then
    PostMessage(Self.Handle, WM_CLOSE, 0, 0);

  UpdateCaption(Client);
end;

procedure TFormDockHost.FormDockOver(Sender: TObject;
  Source: TDragDockObject; X, Y: Integer; State: TDragState;
  var Accept: Boolean);
begin
  Accept := Source.Control is TFormDockable;
end;

procedure TFormDockHost.FormGetSiteInfo(Sender: TObject;
  DockClient: TControl; var InfluenceRect: TRect; MousePos: TPoint;
  var CanDock: Boolean);
begin
  CanDock := DockClient is TFormDockable;
end;

end.
