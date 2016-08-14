program DockDemo;

uses
  Forms,
  DockDemo.Form in 'DockDemo.Form.pas' {DockableForm},
  DockDemo.Host in 'DockDemo.Host.pas' {ConjoinDockHost},
  DockDemo.Main in 'DockDemo.Main.pas' {MainForm},
  DockDemo.TabHost in 'DockDemo.TabHost.pas' {TabDockHost},
  DockDemo.Utilities in 'DockDemo.Utilities.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
