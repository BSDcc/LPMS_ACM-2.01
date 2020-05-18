//------------------------------------------------------------------------------
// Date.......: 17 May 2020
// System.....: LPMS Access Control Manager
// Program ID.: LPMS_Excel
// Platform...: Lazarus (Winblows, Linux, Raspbian & macOS)
// Author.....: Francois De Bruin Meyer (BlueCrane Software Development CC)
//------------------------------------------------------------------------------
// Description: This module generates an Excel spreadsheet containing all the
// ...........: detail about every Key definition in the LPMS_ACM database
//------------------------------------------------------------------------------

unit LPMS_Excel;

{$mode objfpc}{$H+}

interface

//------------------------------------------------------------------------------
// Uses clause
//------------------------------------------------------------------------------
uses
   Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
   ActnList, StdActns, EditBtn, Buttons, LCLIntf, LCLType, fpspreadsheetgrid,
   fpspreadsheetctrls, fpstypes{, fpspreadsheet, fpsActions}, fpsallformats;

//------------------------------------------------------------------------------
// Declarations
//------------------------------------------------------------------------------
type

   { TFLPMS_Excel }

   TFLPMS_Excel = class( TForm)
      alList: TActionList;
      Bevel1: TBevel;
      Bevel2: TBevel;
      Bevel3: TBevel;
      Bevel4: TBevel;
      Bevel5: TBevel;
      btnExport: TBitBtn;
      btnReturn: TBitBtn;
      cbOpen: TCheckBox;
      cbxFormat: TComboBox;
      FileSaveAs: TFileSaveAs;
      imgSmall: TImageList;
      Label1: TLabel;
      Panel1: TPanel;
      Panel2: TPanel;
      Panel3: TPanel;
      Panel4: TPanel;
      Panel5: TPanel;
      Panel6: TPanel;
      wbsSource: TsWorkbookSource;
      wsgGrid: TsWorksheetGrid;
      procedure btnExportClick( Sender: TObject);
      procedure btnReturnClick( Sender: TObject);
      procedure FileSaveAsAccept( Sender: TObject);
      procedure FileSaveAsBeforeExecute( Sender: TObject);
      procedure FormCreate( Sender: TObject);
      procedure FormShow( Sender: TObject);

private   {Private Declarations}

   FileSaved : boolean;

public    {Public Declarations}

end;

var
   FLPMS_Excel: TFLPMS_Excel;

implementation

{$R *.lfm}

{ TFLPMS_Excel }

//------------------------------------------------------------------------------
// Executed when the Form is created
//------------------------------------------------------------------------------
procedure TFLPMS_Excel.FormCreate( Sender: TObject);
begin

   FileSaved := False;

end;

//------------------------------------------------------------------------------
// Executed when the Form is shown
//------------------------------------------------------------------------------
procedure TFLPMS_Excel.FormShow( Sender: TObject);
begin

   btnExport.SetFocus;
   cbxFormat.ItemIndex := 0;

end;

//------------------------------------------------------------------------------
// User clicked on the Return button
//------------------------------------------------------------------------------
procedure TFLPMS_Excel.btnReturnClick( Sender: TObject);
begin

      Close();

end;

//------------------------------------------------------------------------------
// Executed before the 'Save As' dialog is displayed
//------------------------------------------------------------------------------
procedure TFLPMS_Excel. FileSaveAsBeforeExecute( Sender: TObject);
begin

   case cbxFormat.ItemIndex of

      0: FileSaveAs.Dialog.Filter := 'Excel 2007+ Format (*.xlsx)|*.xlsx';
      1: FileSaveAs.Dialog.Filter := 'Excel 95-2003 format (*.xls)|*.xls';
      2: FileSaveAs.Dialog.Filter := 'Open/LibreOffice format (*.ods)|*.ods';
      3: FileSaveAs.Dialog.Filter := 'CSV (text) format (*.csv)|*.csv';

   end;

   FileSaved := False;

end;

//------------------------------------------------------------------------------
// Executed when user clicks on the 'Save' button on Dialog that opens when the
// wsgGrid is exported
//------------------------------------------------------------------------------
procedure TFLPMS_Excel.FileSaveAsAccept(Sender: TObject);
var
  Format : TsSpreadsheetFormat;

begin

  Screen.Cursor := crHourglass;

  try

    case cbxFormat.ItemIndex of

       0: Format := sfOOXML;           // Excel 2007+ Format (*.xlsx)
       1: Format := sfExcel8;          // Excel 95-2003 Format (*.xls)
       2: Format := sfOpenDocument;    // Open/LibreOffice Format (*.ods)
       3: Format := sfCSV;             // CSV (Text) Format (*.csv)

    end;

    wbsSource.SaveToSpreadsheetFile(FileSaveAs.Dialog.FileName, Format);
    FileSaved := FileSaveAs.ExecuteResult;

  finally

    Screen.Cursor := crDefault;

  end;

end;

//------------------------------------------------------------------------------
// User clicked on the Export button
//------------------------------------------------------------------------------
procedure TFLPMS_Excel.btnExportClick(Sender: TObject);
begin

  FileSaveAs.Execute;

  if FileSaved = False then begin

     Application.MessageBox('File not saved','LPMS ACM',MB_OK);
     Exit;

  end;

  if cbOpen.Checked = True then begin

     OpenDocument(FileSaveAs.Dialog.FileName);

  end;

end;

//------------------------------------------------------------------------------

end.

