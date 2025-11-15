program ADSSQLCommander;

uses
  Forms,
  MainForm in 'MainForm.pas' {ADSform};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'ADS SQL Commander';
  Application.CreateForm(TADSform, ADSform);
  Application.Run;
end.
