unit DockDemo.Main;

interface

uses
  System.SysUtils, System.Classes, System.Types, WinApi.Windows,
  WinApi.Messages, Vcl.Forms, Vcl.Menus, Vcl.StdCtrls, Vcl.ComCtrls,
  Vcl.ToolWin, Vcl.ExtCtrls, Vcl.Tabs, Vcl.DockTabSet, Vcl.Controls,
  DockDemo.Form;

type
  TMainForm = class(TForm)
    MainMenu: TMainMenu;
    MenuItemBlue: TMenuItem;
    MenuItemFile: TMenuItem;
    MenuItemFileExit: TMenuItem;
    MenuItemFloatOnCloseDocked: TMenuItem;
    MenuItemGreen: TMenuItem;
    MenuItemLime: TMenuItem;
    MenuItemPurple: TMenuItem;
    MenuItemRed: TMenuItem;
    MenuItemTeal: TMenuItem;
    MenuItemView: TMenuItem;
    MenuItemWhite: TMenuItem;
    N1: TMenuItem;
    PanelBottom: TPanel;
    PanelLeft: TPanel;
    PanelRight: TPanel;
    SplitterBottom: TSplitter;
    SplitterLeft: TSplitter;
    SplitterRight: TSplitter;
    DelayedStartTimer: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure MenuItemFileExitClick(Sender: TObject);
    procedure MenuItemFloatOnCloseDockedClick(Sender: TObject);
    procedure MenuItemViewFormClick(Sender: TObject);
    procedure FormGetSiteInfo(Sender: TObject; DockClient: TControl;
      var InfluenceRect: TRect; MousePos: TPoint; var CanDock: Boolean);
    procedure PanelUnDock(Sender: TObject; Client: TControl;
      NewTarget: TWinControl; var Allow: Boolean);
    procedure FormShow(Sender: TObject);
    procedure FormDockOver(Sender: TObject; Source: TDragDockObject; X,
      Y: Integer; State: TDragState; var Accept: Boolean);
    procedure DelayedStartTimerTimer(Sender: TObject);
  private
    procedure CreateDockedWindows;
    function ComputeDockingRect(var DockRect: TRect; MousePos: TPoint): TAlign;
    procedure CMDockClient(var Message: TCMDockClient); message CM_DOCKCLIENT;
  public
    procedure ShowDockPanel(APanel: TPanel; MakeVisible: Boolean; Client: TControl);
  end;

var
  MainForm: TMainForm;

implementation

uses
  Vcl.Graphics, DockDemo.Host;

{$R *.dfm}

var
  DockWindows: array [0..6] of TFormDockable;

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Left := 400;

  CreateDockedWindows;
end;

procedure TMainForm.FormDockOver(Sender: TObject; Source: TDragDockObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
var
  ARect: TRect;
begin
  Accept := (Source.Control is TFormDockable);

  // Draw dock preview depending on where the cursor is relative to our client area
  if Accept and (ComputeDockingRect(ARect, Point(X, Y)) <> alNone) then
  begin
    ComputeDockingRect(ARect, Point(X, Y));
    Source.DockRect := ARect;
  end;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  DelayedStartTimer.Enabled := True;
end;

procedure TMainForm.MenuItemFileExitClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.MenuItemFloatOnCloseDockedClick(Sender: TObject);
var
  Index: Integer;
begin
  MenuItemFloatOnCloseDocked.Checked := not MenuItemFloatOnCloseDocked.Checked;

  for Index := Low(DockWindows) to High(DockWindows) do
    DockWindows[Index].FloatOnCloseDock := MenuItemFloatOnCloseDocked.Checked;
end;

procedure TMainForm.MenuItemViewFormClick(Sender: TObject);
var
  DockWindow: TFormDockable;
begin
  DockWindow := DockWindows[(Sender as TComponent).Tag];

  if (DockWindow.HostDockSite is TPanel) and (DockWindow.HostDockSite.Owner is TFormDockHost) then
    TFormDockHost(DockWindow.HostDockSite.Owner).Show
  else
    DockWindow.Show;
end;

procedure TMainForm.PanelUnDock(Sender: TObject; Client: TControl;
  NewTarget: TWinControl; var Allow: Boolean);
begin
  // OnUnDock gets called BEFORE the client is undocked, in order to optionally
  // disallow the undock. DockClientCount is never 0 when called from this event.
  if (Sender as TPanel).DockClientCount = 1 then
    ShowDockPanel(Sender as TPanel, False, nil);
end;

procedure TMainForm.FormGetSiteInfo(Sender: TObject;
  DockClient: TControl; var InfluenceRect: TRect; MousePos: TPoint;
  var CanDock: Boolean);
begin
  // if CanDock is true, the panel will not automatically draw the preview rect.
  CanDock := (DockClient is TFormDockable);
end;

procedure TMainForm.CMDockClient(var Message: TCMDockClient);
var
  ARect: TRect;
  DockType: TAlign;
  Pt: TPoint;
begin
  if Message.DockSource.Control is TFormDockable then
  begin
    Pt.X := Message.MousePos.X;
    Pt.Y := Message.MousePos.Y;
    DockType := ComputeDockingRect(ARect, Pt);

    case DockType of
      alLeft:
        begin
          ShowDockPanel(PanelLeft, True, Message.DockSource.Control);
          Message.DockSource.Control.ManualDock(PanelLeft, nil, alClient);
        end;
      alRight:
        begin
          ShowDockPanel(PanelRight, True, Message.DockSource.Control);
          Message.DockSource.Control.ManualDock(PanelRight, nil, alClient);
        end;
      alBottom:
        begin
          ShowDockPanel(PanelBottom, True, Message.DockSource.Control);
          Message.DockSource.Control.ManualDock(PanelBottom, nil, alClient);
        end;
    end;
  end;
end;

function TMainForm.ComputeDockingRect(var DockRect: TRect; MousePos: TPoint): TAlign;
var
  DockLeftRect, DockRightRect, DockBottomRect: TRect;
begin
  Result := alNone;

  // divide form up into docking zones
  DockLeftRect.TopLeft := Point(0, 0);
  DockLeftRect.BottomRight := Point(ClientWidth div 5, ClientHeight);

  DockRightRect.TopLeft := Point(4 * ClientWidth div 5, 0);
  DockRightRect.BottomRight := Point(ClientWidth, ClientHeight);

  DockBottomRect.TopLeft := Point(ClientWidth div 5, 4 * ClientHeight div 5);
  DockBottomRect.BottomRight := Point(4 * ClientWidth div 5, ClientHeight);

  // locate the mouse position in the docking zone
  if PtInRect(DockLeftRect, MousePos) then
  begin
    Result := alLeft;
    DockRect := DockLeftRect;
    DockRect.Right := ClientWidth div 3;
  end
  else
  if PtInRect(DockRightRect, MousePos) then
  begin
    Result := alRight;
    DockRect := DockRightRect;
    DockRect.Left := 2 * ClientWidth div 3;
  end
  else
  if PtInRect(DockBottomRect, MousePos) then
  begin
    Result := alBottom;
    DockRect := DockBottomRect;
    DockRect.Left := 0;
    DockRect.Right := ClientWidth;
    DockRect.Top := 2 * ClientHeight div 3;
  end;

  // check if a docking area has been determined
  if Result = alNone then Exit;

  // DockRect is in screen coordinates.
  DockRect.TopLeft := ClientToScreen(DockRect.TopLeft);
  DockRect.BottomRight := ClientToScreen(DockRect.BottomRight);
end;

procedure TMainForm.CreateDockedWindows;
var
  Index: Integer;
  ColorText: string;
const
  CColors: array [0..6] of TColor = (clWhite, clBlue, clGreen, clRed, clTeal,
    clPurple, clLime);
begin
  for Index := 0 to High(DockWindows) do
  begin
    DockWindows[Index] := TFormDockable.Create(Application);
    ColorText := ColorToString(CColors[Index]);
    Delete(ColorText, 1, 2);
    DockWindows[Index].Caption := ColorText;

    with TMemo.Create(DockWindows[Index]) do
    begin
      Parent := DockWindows[Index];
      Left := 0;
      Top := 0;
      Width := 391;
      Height := 217;
      Align := alClient;
      Lines.Clear;
      Lines.Add('Memo1');
      TabOrder := 0;
      Color := CColors[Index];
      Font.Color := CColors[Index] xor $00FFFFFF;
      Text := ColorText + ' window';
    end;
  end;
end;

procedure TMainForm.DelayedStartTimerTimer(Sender: TObject);
var
  Index: Integer;
begin
  DelayedStartTimer.Enabled := False;

  for Index := 0 to 3 do
  begin
    DockWindows[Index].Left := Left - DockWindows[Index].Width;
    DockWindows[Index].Top := Top + 64 * Index;
    DockWindows[Index].Show;
  end;
end;

procedure TMainForm.ShowDockPanel(APanel: TPanel; MakeVisible: Boolean;
  Client: TControl);
begin
  if not MakeVisible and (APanel.VisibleDockClientCount > 1) then
    Exit;

  if APanel = PanelLeft then
  begin
    SplitterLeft.Visible := MakeVisible;
    APanel.Visible := MakeVisible;
    if MakeVisible then
    begin
      APanel.Width := ClientWidth div 3;
      SplitterLeft.Left := APanel.Width + SplitterLeft.Width;
    end;
  end
  else
  if APanel = PanelRight then
  begin
    SplitterRight.Visible := MakeVisible;
    APanel.Visible := MakeVisible;
    if MakeVisible then
    begin
      APanel.Width := ClientWidth div 3;
      SplitterRight.Left := APanel.Left - SplitterRight.Width;
    end;
  end
  else
  if APanel = PanelBottom then
  begin
    SplitterBottom.Visible := MakeVisible;
    APanel.Visible := MakeVisible;
    if MakeVisible then
    begin
      APanel.Height := ClientWidth div 3;
      SplitterRight.Top := ClientHeight - APanel.Height - SplitterBottom.Width;
    end;
  end;

  if MakeVisible and (Client <> nil) then
    Client.Show;
end;

end.
