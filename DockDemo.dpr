program DockDemo;

uses
  Forms,
  DockDemo.Main in 'DockDemo.Main.pas' {MainForm},
  DockDemo.Form in 'DockDemo.Form.pas' {FormDockable},
  DockDemo.Host in 'DockDemo.Host.pas' {FormDockHost},
  DockDemo.Utilities in 'DockDemo.Utilities.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
