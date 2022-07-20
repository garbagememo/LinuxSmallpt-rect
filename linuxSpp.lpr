program linuxSpp;

{$mode objfpc}{$H+}

uses
  // for multi threading the cthreads unit must be used on unix systems:
  // for example: Linux, MacOSX, FreeBSD, Solaris
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, RenderUnit;

begin
  Application.Initialize;
  Application.CreateForm(TRenderForm, RenderForm);
  Application.Run;
end.

