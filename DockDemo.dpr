program DockDemo;

uses
  Forms,
  DockDemo.Form in 'DockDemo.Form.pas' {FormDockable},
  DockDemo.Host in 'DockDemo.Host.pas' {FormDockHost},
  DockDemo.ConjoinHost in 'DockDemo.ConjoinHost.pas',
  DockDemo.Main in 'DockDemo.Main.pas' {MainForm},
  DockDemo.TabHost in 'DockDemo.TabHost.pas' {FormDockHostTabs},
  DockDemo.Utilities in 'DockDemo.Utilities.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
