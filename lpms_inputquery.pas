//------------------------------------------------------------------------------
// Date.......: 10 May 2020
// System.....: LPMS Access Control Manager
// Program ID.: LPMS_Inputquery
// Platform...: Lazarus (Winblows, Linux, Raspbian & macOS)
// Author.....: Francois De Bruin Meyer (BlueCrane Software Development CC)
//------------------------------------------------------------------------------
// History....: 10 May 2020 - Adapt from LPMS C++ version
//------------------------------------------------------------------------------

unit LPMS_InputQuery;

{$mode objfpc}{$H+}

interface

//------------------------------------------------------------------------------
// Uses clause
//------------------------------------------------------------------------------
uses
   Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons;

//------------------------------------------------------------------------------
// Declarations
//------------------------------------------------------------------------------
type

   { TFLPMS_InputQuery }

   TFLPMS_InputQuery = class( TForm)
      btnCancel: TButton;
      btnOk: TButton;
      edtInput: TEdit;
      lblCaption: TLabel;
      btnView: TSpeedButton;
      procedure btnCancelClick( Sender: TObject);
      procedure btnOkClick( Sender: TObject);
      procedure btnViewClick( Sender: TObject);
      procedure FormShow( Sender: TObject);

private   { Private Declarations }

public    { Public Declarations }

end;

//------------------------------------------------------------------------------
// Global variables
//------------------------------------------------------------------------------
var
   FLPMS_InputQuery: TFLPMS_InputQuery;

implementation

   uses LPMS_Main;

{$R *.lfm}

{ TFLPMS_InputQuery }

//------------------------------------------------------------------------------
// Executed when the Form is show
//------------------------------------------------------------------------------
procedure TFLPMS_InputQuery. FormShow( Sender: TObject);
begin

   edtInput.Clear;
   edtInput.SetFocus;

end;

//------------------------------------------------------------------------------
// User clicked on the OK button
//------------------------------------------------------------------------------
procedure TFLPMS_InputQuery. btnOkClick( Sender: TObject);
begin

  FLPMS_Main.ThisRes := edtInput.Text;
  Close;

end;

//------------------------------------------------------------------------------
// User clicked on the Cancel button
//------------------------------------------------------------------------------
procedure TFLPMS_InputQuery. btnCancelClick( Sender: TObject);
begin

    FLPMS_Main.ThisRes := '';
    Close;

end;

//------------------------------------------------------------------------------
// User clicked on the button to Show/Hide the password
//------------------------------------------------------------------------------
procedure TFLPMS_InputQuery. btnViewClick( Sender: TObject);
var
   Location : string;

begin

   Location := ExtractFilePath(Application.ExeName);

   if edtInput.PasswordChar = '*' then begin

      edtInput.PasswordChar := #00;
      btnView.Glyph.LoadFromFile(Location + 'eye_Off_16.bmp');

   end else begin

      edtInput.PasswordChar := '*';
      btnView.Glyph.LoadFromFile(Location + 'eye_16.bmp');

   end;

end;

//------------------------------------------------------------------------------
end.

