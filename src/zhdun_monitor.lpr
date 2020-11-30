program zhdun_monitor;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, lnetvisual, MonitorForm, ZhdunItems, ZhdunTicketManager
  { you can add units after this };

{$R *.res}

begin
  {$if declared(useHeapTrace)}
  globalSkipIfNoLeaks := true; // supported as of debugger version 3.2.0
  setHeapTraceOutput('monitor_trace.log'); // supported as of debugger version 3.2.0
  {$endIf}
  RequireDerivedFormResource := True;
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TFormMonitor, FormMonitor);
  Application.Run;
end.

