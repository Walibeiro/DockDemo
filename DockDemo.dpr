program DockDemo;

uses
  Forms,
  DockDemo.Main in 'DockDemo.Main.pas' {MainForm},
  DockDemo.Form in 'DockDemo.Form.pas',
  DockDemo.ConjoinHost in 'DockDemo.ConjoinHost.pas',
  DockDemo.TabHost in 'DockDemo.TabHost.pas',
  DockDemo.Host in 'DockDemo.Host.pas',
  DockDemo.Utilities in 'DockDemo.Utilities.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
