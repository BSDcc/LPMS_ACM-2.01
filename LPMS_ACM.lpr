program LPMS_ACM;

{$mode objfpc}{$H+}

uses
   {$IFDEF UNIX}{$IFDEF UseCThreads}
   cthreads,
   {$ENDIF}{$ENDIF}
   Interfaces, // this includes the LCL widgetset
   Forms, datetimectrls, LPMS_Login, LPMS_Main, LPMS_InputQuery, LPMS_Show
   { you can add units after this };

{$R *.res}

begin
   RequireDerivedFormResource := True;
   Application. Scaled := True;
   Application. Initialize;
   Application. CreateForm( TFLPMS_Login, FLPMS_Login);
   Application. CreateForm( TFLPMS_Main, FLPMS_Main);
   Application. CreateForm( TFLPMS_InputQuery, FLPMS_InputQuery);
   Application. CreateForm( TFLPMS_Show, FLPMS_Show);
   Application. Run;
end.

