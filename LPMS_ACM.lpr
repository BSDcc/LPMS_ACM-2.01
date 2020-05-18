program LPMS_ACM;

{$mode objfpc}{$H+}

uses
   {$IFDEF UNIX}{$IFDEF UseCThreads}
   cthreads,
   {$ENDIF}{$ENDIF}
   Interfaces, // this includes the LCL widgetset
   Forms, datetimectrls, LPMS_Login, LPMS_Main, LPMS_InputQuery, LPMS_Show,
   LPMS_Excel
   { you can add units after this };

{$R *.res}

begin
   RequireDerivedFormResource := True;
   Application. Scaled := True;
   Application. Initialize;
   Application. CreateForm( TFLPMS_Login, FLPMS_Login);
   Application. Run;
end.

