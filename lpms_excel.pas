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
   Classes, SysUtils, sqldb, Forms, Controls, Graphics, Dialogs, ExtCtrls,
   StdCtrls, ActnList{, StdActns}, EditBtn, Buttons, LCLIntf, LCLType,
   fpspreadsheet, fpsallformats, fpspreadsheetgrid, fpspreadsheetctrls,
   {fpsActions, }fpstypes,

{$IFDEF WINDOWS}                     // Target is Winblows
   mysql56conn;
{$ENDIF}

{$IFDEF LINUX}                       // Target is Linux
   {$IFDEF CPUARMHF}                 // Running on ARM (Raspbian) architecture
      mysql55conn;
   {$ELSE}                           // Running on Intel architecture
      mysql57conn;
   {$ENDIF}
{$ENDIF}

{$IFDEF DARWIN}                      // Target is macOS
   {$IFDEF CPUI386}                  // Running on a version below Catalina
      mysql55conn;
   {$ELSE}                           // Running on Catalina
      mysql57conn;
   {$ENDIF}
{$ENDIF}


//------------------------------------------------------------------------------
// Declarations
//------------------------------------------------------------------------------
type

   { TFLPMS_Excel }

   TFLPMS_Excel = class( TForm)
      Bevel1: TBevel;
      Bevel2: TBevel;
      Bevel3: TBevel;
      Bevel4: TBevel;
      Bevel5: TBevel;
      btnExport: TBitBtn;
      btnOpen: TBitBtn;
      btnReturn: TBitBtn;
      cbOpen: TCheckBox;
      cbClose: TCheckBox;
      cbxFormat: TComboBox;
      Label1: TLabel;
      Label2: TLabel;
      Panel1: TPanel;
      Panel2: TPanel;
      Panel3: TPanel;
      Panel4: TPanel;
      Panel5: TPanel;
      Panel6: TPanel;
      cbxSheets: TComboBox;
      dlgSelDir: TSelectDirectoryDialog;
      sqlQry1: TSQLQuery;
      sqlQry2: TSQLQuery;
      sqlTran: TSQLTransaction;
      StaticText1: TStaticText;
      wsgGrid: TsWorksheetGrid;
      procedure btnExportClick( Sender: TObject);
      procedure btnOpenClick( Sender: TObject);
      procedure btnReturnClick( Sender: TObject);
      procedure cbxSheetsSelect( Sender: TObject);
      procedure FormClose( Sender: TObject; var CloseAction: TCloseAction);
      procedure FormCreate( Sender: TObject);
      procedure FormShow( Sender: TObject);

private   {Private Declarations}

   FileName  : string;             // Holds the Path and Name (without the Ext) of the Generated File
   ErrMsg    : string;             // Holds last MySQL error message
   OSDelim   : string;             // Platform specific delimiter

{$IFDEF WINDOWS}                   // Target is Winblows
   sqlCon  : TMySQL56Connection;
{$ENDIF}

{$IFDEF LINUX}                     // Target is Linux
   {$IFDEF CPUARMHF}               // Running on ARM (Raspbian) architecture
      sqlCon : TMySQL55Connection;
   {$ELSE}                         // Running on Intel architecture
      sqlCon : TMySQL57Connection;
   {$ENDIF}
{$ENDIF}

{$IFDEF DARWIN}                    // Target is macOS
   {$IFDEF CPUI386}                // Running on a version below Catalina
      sqlCon : TMySQL55Connection;
   {$ELSE}                         // Running on Catalina
      sqlCon : TMySQL57Connection;
   {$ENDIF}
{$ENDIF}

   function  CreateSheet(): boolean;
   function  OpenDB(): boolean;
   procedure CloseDB();
   function  ReadCpy(): boolean;
   function  ReadUser(Prefix : string): boolean;
   function  ReadUnique(): boolean;

public    {Public Declarations}

   Host     : string;       // Passed from calling Form
   UserID   : string;       // Passed from calling Form
   Password : string;       // Passed from calling Form

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

   DefaultFormatSettings.ShortDateFormat := 'yyyy/MM/dd';
   DefaultFormatSettings.DateSeparator   := '/';

   OSDelim := '/';

{$IFDEF WINDOWS}                    // Target is Winblows
   OSdelim := '\';
   sqlCon  := TMySQL56Connection.Create(nil);
{$ENDIF}

{$IFDEF LINUX}                      // Target is Linux
   {$IFDEF CPUARMHF}                // Running on ARM (Raspbian) architecture
      sqlCon := TMySQL55Connection.Create(nil);
   {$ELSE}                          // Running on Intel architecture
      sqlCon := TMySQL57Connection.Create(nil);
   {$ENDIF}
{$ENDIF}

{$IFDEF DARWIN}                     // Target is macOS
   {$IFDEF CPUI386}                 // Running on a version below Catalina
      sqlCon := TMySQL55Connection.Create(nil);
   {$ELSE}
      sqlCon := TMySQL57Connection.Create(nil);
   {$ENDIF}
{$ENDIF}

   sqlTran.DataBase    := sqlCon;
   sqlQry1.Transaction := sqlTran;
   sqlQry2.Transaction := sqlTran;

end;

//------------------------------------------------------------------------------
// Executed before the Form is shown
//------------------------------------------------------------------------------
procedure TFLPMS_Excel.FormShow( Sender: TObject);
begin

   dlgSelDir.Title      := 'LPMS_ACM - Select location for the generated output file';

   if dlgSelDir.Execute = False then begin

      Close();
      Exit;

   end;

   FileName := dlgSelDir.FileName + OSDelim + FormatDateTime('yyyyMMdd',Date()) + ' - LPMS_ACM Company and User Definitions';

   FLPMS_Excel.Caption := 'LPMS_ACM Export Manager';
   btnExport.Enabled   := False;
   btnOpen.Enabled     := False;
   cbxFormat.ItemIndex := 0;

   if CreateSheet() = True then begin

      btnExport.Enabled := True;
      btnOpen.Enabled   := True;
      cbClose.Checked   := True;

      btnOpen.SetFocus;

   end;

end;

//------------------------------------------------------------------------------
// Executed before the Form is finally closed
//------------------------------------------------------------------------------
procedure TFLPMS_Excel.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin

   CloseDB();

end;

//------------------------------------------------------------------------------
// User clicked on the Return button
//------------------------------------------------------------------------------
procedure TFLPMS_Excel.btnReturnClick(Sender: TObject);
begin

      Close();

end;

//------------------------------------------------------------------------------
// Function to generate the Excel spreadsheet
//------------------------------------------------------------------------------
function TFLPMS_Excel.CreateSheet() : boolean;
var
   Row, Col       : integer;          // Row and Column counter
   ThisCase, idx1 : integer;          // Used for case statements
   ThisPref       : string;           // Current Company Prefix

   xlsBook        : TsWorkbook;       // The Excel Workbook;
   xlsSheet       : TsWorksheet;      // The active Sheet in the Workbook

begin

//--- Open the database connection and read the Companies table

   if (OpenDB() = False) then begin

      Application.MessageBox(PChar('Unexpected error while opening database connection: ' + ErrMsg + '. LPMS_Exel will now terminate'),'LPMS Export Manager', (MB_OK + MB_ICONWARNING));
      Close;
      Exit;

   end;

   if (ReadCpy() = False) then begin

      Application.MessageBox(PChar('Unexpected database error:' + ErrMsg + '. LPMS_Exel will now terminate'),'LPMS Export Manager',(MB_OK + MB_ICONWARNING));
      Close;
      Exit;

   end;

   xlsBook := TsWorkbook.Create;
   xlsBook.SetDefaultFont('Ariel', 10);
   xlsBook.Options := xlsBook.Options + [boCalcBeforeSaving];

   while (sqlQry1.Eof = false) do begin

      ThisPref := sqlQry1.FieldByName('LPMSKey_Prefix').AsString;

      Row := 0;
      Col := 0;

      xlsSheet := xlsBook.AddWorksheet(ThisPref);

//--- Create the heading information

      xlsSheet.WriteText(Row,Col,'LPMS Access Control Manager - Company and User definition export');
      xlsSheet.WriteBorders(Row,Col,[cbWest]);
      xlsSheet.WriteBorders(Row,Col+14,[cbEast]);

      for idx1 := Col to Col+13 do begin

         xlsSheet.WriteBorders(Row,idx1,[cbNorth, cbSouth]);
         xlsSheet.WriteBackgroundColor(Row,idx1,clNavy);
         xlsSheet.WriteFontColor(Row,idx1,clWhite);
         xlsSheet.WriteFontStyle(Row,idx1,[fssBold]);

      end;

      Row := Row + 2;

      xlsSheet.WriteText(Row,Col,'Company:');
      xlsSheet.WriteText(Row+1,Col,'Prefix:');
      xlsSheet.WriteText(Row+2,Col,'Type:');
      xlsSheet.WriteText(Row+3,Col,'License Count:');
      xlsSheet.WriteText(Row+4,Col,'Interval:');
      xlsSheet.WriteText(Row+5,Col,'Blocked:');

      for idx1 := Row to Row + 5 do begin

         xlsSheet.WriteBorders(idx1,Col,[cbWest,cbSouth,cbNorth]);
         xlsSheet.WriteBorders(idx1,Col+1,[cbSouth,cbNorth,cbEast]);
         xlsSheet.WriteBorders(idx1,Col+2,[cbSouth,cbNorth,cbEast]);

      end;

      Row := Row + 7;

//--- Generate the table headings

      xlsSheet.WriteText(Row,Col,'Key');
      xlsSheet.WriteColWidth(Col,5);

      xlsSheet.WriteText(Row,Col+1,'User');
      xlsSheet.WriteColWidth(Col+1,30);

      xlsSheet.WriteText(Row,Col+2,'Company');
      xlsSheet.WriteColWidth(Col+2,35);

      xlsSheet.WriteText(Row,Col+3,'Email');
      xlsSheet.WriteColWidth(Col+3,30);

      xlsSheet.WriteText(Row,Col+4,'Contact');
      xlsSheet.WriteColWidth(Col+4,15);

      xlsSheet.WriteText(Row,Col+5,'Unique');
      xlsSheet.WriteColWidth(Col+5,15);

      xlsSheet.WriteText(Row,Col+6,'License');
      xlsSheet.WriteColWidth(Col+6,20);

      xlsSheet.WriteText(Row,Col+7,'Expire');
      xlsSheet.WriteColWidth(Col+7,12);

      xlsSheet.WriteText(Row,Col+8,'Activation Key');
      xlsSheet.WriteColWidth(Col+8,45);

      xlsSheet.WriteText(Row,Col+9,'Blocked');
      xlsSheet.WriteColWidth(Col+9,12);

      xlsSheet.WriteText(Row,Col+10,'Renewals');
      xlsSheet.WriteColWidth(Col+10,12);
      xlsSheet.WriteHorAlignment(Row,Col+10,haRight);

      xlsSheet.WriteText(Row,Col+11,'Transfer');
      xlsSheet.WriteColWidth(Col+11,12);

      xlsSheet.WriteText(Row,Col+12,'New Prefix');
      xlsSheet.WriteColWidth(Col+12,12);

      xlsSheet.WriteText(Row,Col+13,'New License');
      xlsSheet.WriteColWidth(Col+13,20);

      for idx1 := Col to Col + 13 do begin

         xlsSheet.WriteBorders(Row,idx1,[cbEast,cbNorth,cbSouth,cbWest]);
         xlsSheet.WriteBackgroundColor(Row,idx1,clNavy);
         xlsSheet.WriteFontColor(Row,idx1,clWhite);
         xlsSheet.WriteFontStyle(Row,idx1,[fssBold]);

      end;

//--- Insert the Company Information

      Row := 2;
      Col := 2;

      xlsSheet.WriteText(Row,Col,sqlQry1.FieldByName('LPMSKey_Desc').AsString);
      xlsSheet.WriteText(Row+1,Col,sqlQry1.FieldByName('LPMSKey_Prefix').AsString);
      xlsSheet.WriteText(Row+2,Col,sqlQry1.FieldByName('LPMSKey_Name').AsString);
      xlsSheet.WriteText(Row+3,Col,sqlQry1.FieldByName('LPMSKey_LicCount').AsString);
      xlsSheet.WriteText(Row+4,Col,sqlQry1.FieldByName('LPMSKey_Interval').AsString);

      if (sqlQry1.FieldByName('LPMSKey_Blocked').AsInteger = 0) then
         xlsSheet.WriteText(Row+5,Col,'No')
      else
         xlsSheet.WriteText(Row+5,Col,'Yes');

      for idx1 := Row to Row + 5 do
         xlsSheet.WriteBackgroundColor(idx1,Col,clSilver);

      Row := Row + 8;
      Col := 0;

//--- Insert the User Information for this Company

      ReadUser(ThisPref);

      while (sqlQry2.Eof = false) do begin

         xlsSheet.WriteText(Row,Col,sqlQry2.FieldByName('LPMSKey_Key').AsString);
         xlsSheet.WriteText(Row,Col+1,sqlQry2.FieldByName('LPMSKey_Name').AsString);
         xlsSheet.WriteText(Row,Col+2,sqlQry2.FieldByName('LPMSKey_Company').AsString);
         xlsSheet.WriteText(Row,Col+3,sqlQry2.FieldByName('LPMSKey_Email').AsString);
         xlsSheet.WriteText(Row,Col+4,sqlQry2.FieldByName('LPMSKey_Contact').AsString);
         xlsSheet.WriteText(Row,Col+5,sqlQry2.FieldByName('LPMSKey_Unique').AsString);

         ThisCase := sqlQry2.FieldByName('LPMSKey_LicType').AsInteger;

         case ThisCase of

            1: xlsSheet.WriteText(Row,Col+6,'Trial');
            2: xlsSheet.WriteText(Row,Col+6,'Personal');
            3: xlsSheet.WriteText(Row,Col+6,'Browse');
            4: xlsSheet.WriteText(Row,Col+6,'Generic');

         end;

         xlsSheet.WriteText(Row,Col+7,sqlQry2.FieldByName('LPMSKey_ExpiryDate').AsString);
         xlsSheet.WriteText(Row,Col+8,sqlQry2.FieldByName('LPMSKey_Activation').AsString);

         if (sqlQry2.FieldByName('LPMSKey_Blocked').AsInteger = 0) then
            xlsSheet.WriteText(Row,Col+9,'No')
         else
            xlsSheet.WriteText(Row,Col+9,'Yes');

         xlsSheet.WriteText(Row,Col+10,sqlQry2.FieldByName('LPMSKey_Renewals').AsString);
         xlsSheet.WriteHorAlignment(Row,Col+10,haRight);


         if (sqlQry2.FieldByName('LPMSKey_Transfer').AsInteger = 0) then
            xlsSheet.WriteText(Row,Col+11,'No')
         else
            xlsSheet.WriteText(Row,Col+11,'Yes');

         xlsSheet.WriteText(Row,Col+12,sqlQry2.FieldByName('LPMSKey_NewPrefix').AsString);

         ThisCase := sqlQry2.FieldByName('LPMSKey_NewLicense').AsInteger;

         case ThisCase of

            0: xlsSheet.WriteText(Row,Col+13,'Not Transferred');
            1: xlsSheet.WriteText(Row,Col+13,'Trial');
            2: xlsSheet.WriteText(Row,Col+13,'Personal');
            3: xlsSheet.WriteText(Row,Col+13,'Browse');
            4: xlsSheet.WriteText(Row,Col+13,'Generic');

         end;

         for idx1 := Col to Col + 13 do begin
            xlsSheet.WriteBorders(Row,idx1,[cbWest,cbNorth,cbSouth,cbEast]);
         end;

         Row := Row + 1;
         sqlQry2.Next;

      end;

      sqlQry1.Next;

   end;

//--- Save the spreadsheet to a file

   xlsBook.WriteToFile(FileName + '.xlsx', True);
   xlsBook.Free;

{
var
   row, col  : integer;          // Row and Column counter
   ThisCase  : integer;          // Used for case statements
   ThisPref  : string;           // Current Company Prefix

   xlsBook   : IXLSWorkbook;
   xlsSheet  : IXLSWorksheet;

begin
   ShortDateFormat := 'yyyy/MM/dd';
   DateSeparator   := '/';

//--- Create the Excel workbook that will contain the exported data

   xlsBook := TXLSWorkbook.Create;

//--- Open the database connection and read the Companies table

   if (OpenDB = false) then begin
      MessageDlg('Unexpected error while opening database connection: ' + ErrMsg + '. LPMS_Export will now terminate', mtWarning, [mbOK], 0);
      Close;
      Exit;
   end;

   if (ReadCpy = false) then begin
      MessageDlg('Unexpected database error:' + ErrMsg + '. LPMS_Export will now terminate', mtWarning, [mbOK], 0);
      Close;
      Exit;
   end;

   while (sqlQry1.Eof = false) do begin
      ThisPref := sqlQry1.FieldByName('LPMSKey_Prefix').AsString;

      row := 1;
      col := 1;

      xlsSheet := xlsBook.Sheets.Add;
      xlsSheet.Name := sqlQry1.FieldByName('LPMSKey_Prefix').AsString;

//--- Create the heading information

      xlsSheet.Cells.Item[row,col].Value := 'LPMS Access Control Manager - Company and User definition export';
      with xlsSheet.Range['A1', 'N1'] do begin
         Borders[xlAround].Weight := xlThin;
         Interior.Color           := clNavy;
         Font.Color               := clWhite;
         Font.Bold                := true;
      end;

      row := row + 2;

      xlsSheet.Cells.Item[row,col].Value := 'Company:';
      xlsSheet.Cells.Item[row,col].Borders[xlEdgeBottom].Weight := xlThin;
      xlsSheet.Cells.Item[row,col + 1].Borders[xlEdgeBottom].Weight := xlThin;

      xlsSheet.Cells.Item[row + 1,col].Value := 'Prefix:';
      xlsSheet.Cells.Item[row + 1,col].Borders[xlEdgeBottom].Weight := xlThin;
      xlsSheet.Cells.Item[row + 1,col + 1].Borders[xlEdgeBottom].Weight := xlThin;

      xlsSheet.Cells.Item[row + 2,col].Value := 'Type:';
      xlsSheet.Cells.Item[row + 2,col].Borders[xlEdgeBottom].Weight := xlThin;
      xlsSheet.Cells.Item[row + 2,col + 1].Borders[xlEdgeBottom].Weight := xlThin;

      xlsSheet.Cells.Item[row + 3,col].Value := 'License Count:';
      xlsSheet.Cells.Item[row + 3,col].Borders[xlEdgeBottom].Weight := xlThin;
      xlsSheet.Cells.Item[row + 3,col + 1].Borders[xlEdgeBottom].Weight := xlThin;

      xlsSheet.Cells.Item[row + 4,col].Value := 'Interval:';
      xlsSheet.Cells.Item[row + 4,col].Borders[xlEdgeBottom].Weight := xlThin;
      xlsSheet.Cells.Item[row + 4,col + 1].Borders[xlEdgeBottom].Weight := xlThin;

      xlsSheet.Cells.Item[row + 5,col].Value := 'Blocked:';
      xlsSheet.Cells.Item[row + 5,col].Borders[xlEdgeBottom].Weight := xlThin;
      xlsSheet.Cells.Item[row + 5,col + 1].Borders[xlEdgeBottom].Weight := xlThin;

      with xlsSheet.Range['A3', 'B8'] do begin
         Borders[xlAround].Weight := xlThin;
         Interior.Color           := clSilver;
         Font.Bold                := true;
      end;

      row := row + 7;

//--- Print the table headings

      xlsSheet.Cells.Item[row,col].Value := 'Key';
      xlsSheet.Cells.Item[row,col].ColumnWidth := 5;
      xlsSheet.Cells.Item[row,col].Borders[xlAround].Weight := xlThin;
      xlsSheet.Cells.Item[row,col].Interior.Color := clNavy;
      xlsSheet.Cells.Item[row,col].Font.Color := clWhite;
      xlsSheet.Cells.Item[row,col].Font.Bold := true;

      xlsSheet.Cells.Item[row,col +  1].Value := 'User';
      xlsSheet.Cells.Item[row,col +  1].ColumnWidth := 30;
      xlsSheet.Cells.Item[row,col +  1].Borders[xlAround].Weight := xlThin;
      xlsSheet.Cells.Item[row,col +  1].Interior.Color := clNavy;
      xlsSheet.Cells.Item[row,col +  1].Font.Color := clWhite;
      xlsSheet.Cells.Item[row,col +  1].Font.Bold := true;

      xlsSheet.Cells.Item[row,col +  2].Value := 'Company';
      xlsSheet.Cells.Item[row,col +  2].ColumnWidth := 35;
      xlsSheet.Cells.Item[row,col +  2].Borders[xlAround].Weight := xlThin;
      xlsSheet.Cells.Item[row,col +  2].Interior.Color := clNavy;
      xlsSheet.Cells.Item[row,col +  2].Font.Color := clWhite;
      xlsSheet.Cells.Item[row,col +  2].Font.Bold := true;

      xlsSheet.Cells.Item[row,col +  3].Value := 'Email';
      xlsSheet.Cells.Item[row,col +  3].ColumnWidth := 30;
      xlsSheet.Cells.Item[row,col +  3].Borders[xlAround].Weight := xlThin;
      xlsSheet.Cells.Item[row,col +  3].Interior.Color := clNavy;
      xlsSheet.Cells.Item[row,col +  3].Font.Color := clWhite;
      xlsSheet.Cells.Item[row,col +  3].Font.Bold := true;

      xlsSheet.Cells.Item[row,col +  4].Value := 'Contact';
      xlsSheet.Cells.Item[row,col +  4].ColumnWidth := 15;
      xlsSheet.Cells.Item[row,col +  4].Borders[xlAround].Weight := xlThin;
      xlsSheet.Cells.Item[row,col +  4].Interior.Color := clNavy;
      xlsSheet.Cells.Item[row,col +  4].Font.Color := clWhite;
      xlsSheet.Cells.Item[row,col +  4].Font.Bold := true;

      xlsSheet.Cells.Item[row,col +  5].Value := 'Unique';
      xlsSheet.Cells.Item[row,col +  5].ColumnWidth := 15;
      xlsSheet.Cells.Item[row,col +  5].Borders[xlAround].Weight := xlThin;
      xlsSheet.Cells.Item[row,col +  5].Interior.Color := clNavy;
      xlsSheet.Cells.Item[row,col +  5].Font.Color := clWhite;
      xlsSheet.Cells.Item[row,col +  5].Font.Bold := true;

      xlsSheet.Cells.Item[row,col +  6].Value := 'License';
      xlsSheet.Cells.Item[row,col +  6].ColumnWidth := 20;
      xlsSheet.Cells.Item[row,col +  6].Borders[xlAround].Weight := xlThin;
      xlsSheet.Cells.Item[row,col +  6].Interior.Color := clNavy;
      xlsSheet.Cells.Item[row,col +  6].Font.Color := clWhite;
      xlsSheet.Cells.Item[row,col +  6].Font.Bold := true;

      xlsSheet.Cells.Item[row,col +  7].Value := 'Expire';
      xlsSheet.Cells.Item[row,col +  7].ColumnWidth := 12;
      xlsSheet.Cells.Item[row,col +  7].Borders[xlAround].Weight := xlThin;
      xlsSheet.Cells.Item[row,col +  7].Interior.Color := clNavy;
      xlsSheet.Cells.Item[row,col +  7].Font.Color := clWhite;
      xlsSheet.Cells.Item[row,col +  7].Font.Bold := true;

      xlsSheet.Cells.Item[row,col +  8].Value := 'Activation Key';
      xlsSheet.Cells.Item[row,col +  8].ColumnWidth := 45;
      xlsSheet.Cells.Item[row,col +  8].Borders[xlAround].Weight := xlThin;
      xlsSheet.Cells.Item[row,col +  8].Interior.Color := clNavy;
      xlsSheet.Cells.Item[row,col +  8].Font.Color := clWhite;
      xlsSheet.Cells.Item[row,col +  8].Font.Bold := true;

      xlsSheet.Cells.Item[row,col +  9].Value := 'Blocked';
      xlsSheet.Cells.Item[row,col +  9].ColumnWidth := 12;
      xlsSheet.Cells.Item[row,col +  9].Borders[xlAround].Weight := xlThin;
      xlsSheet.Cells.Item[row,col +  9].Interior.Color := clNavy;
      xlsSheet.Cells.Item[row,col +  9].Font.Color := clWhite;
      xlsSheet.Cells.Item[row,col +  9].Font.Bold := true;

      xlsSheet.Cells.Item[row,col + 10].Value := 'Renewals';
      xlsSheet.Cells.Item[row,col + 10].ColumnWidth := 12;
      xlsSheet.Cells.Item[row,col + 10].Borders[xlAround].Weight := xlThin;
      xlsSheet.Cells.Item[row,col + 10].HorizontalAlignment := xlHalignRight;
      xlsSheet.Cells.Item[row,col + 10].Interior.Color := clNavy;
      xlsSheet.Cells.Item[row,col + 10].Font.Color := clWhite;
      xlsSheet.Cells.Item[row,col + 10].Font.Bold := true;

      xlsSheet.Cells.Item[row,col + 11].Value := 'Transfer';
      xlsSheet.Cells.Item[row,col + 11].ColumnWidth := 12;
      xlsSheet.Cells.Item[row,col + 11].Borders[xlAround].Weight := xlThin;
      xlsSheet.Cells.Item[row,col + 11].Interior.Color := clNavy;
      xlsSheet.Cells.Item[row,col + 11].Font.Color := clWhite;
      xlsSheet.Cells.Item[row,col + 11].Font.Bold := true;

      xlsSheet.Cells.Item[row,col + 12].Value := 'New Prefix';
      xlsSheet.Cells.Item[row,col + 12].ColumnWidth := 12;
      xlsSheet.Cells.Item[row,col + 12].Borders[xlAround].Weight := xlThin;
      xlsSheet.Cells.Item[row,col + 12].Interior.Color := clNavy;
      xlsSheet.Cells.Item[row,col + 12].Font.Color := clWhite;
      xlsSheet.Cells.Item[row,col + 12].Font.Bold := true;

      xlsSheet.Cells.Item[row,col + 13].Value := 'New License';
      xlsSheet.Cells.Item[row,col + 13].ColumnWidth := 20;
      xlsSheet.Cells.Item[row,col + 13].Borders[xlAround].Weight := xlThin;
      xlsSheet.Cells.Item[row,col + 13].Interior.Color := clNavy;
      xlsSheet.Cells.Item[row,col + 13].Font.Color := clWhite;
      xlsSheet.Cells.Item[row,col + 13].Font.Bold := true;

//--- Insert the Company Information

      row := 3;
      col := 3;

      xlsSheet.Cells.Item[row,col].Value := sqlQry1.FieldByName('LPMSKey_Desc').AsString;
      xlsSheet.Cells.Item[row,col].Borders[xlAround].Weight     := xlThin;
      xlsSheet.Cells.Item[row,col].Interior.Color := clSilver;

      xlsSheet.Cells.Item[row + 1,col].Value := sqlQry1.FieldByName('LPMSKey_Prefix').AsString;
      xlsSheet.Cells.Item[row + 1,col].Borders[xlAround].Weight := xlThin;
      xlsSheet.Cells.Item[row + 1,col].Interior.Color := clSilver;

      xlsSheet.Cells.Item[row + 2,col].Value := sqlQry1.FieldByName('LPMSKey_Name').AsString;
      xlsSheet.Cells.Item[row + 2,col].Borders[xlAround].Weight := xlThin;
      xlsSheet.Cells.Item[row + 2,col].Interior.Color := clSilver;

      xlsSheet.Cells.Item[row + 3,col].Value := sqlQry1.FieldByName('LPMSKey_LicCount').AsString;
      xlsSheet.Cells.Item[row + 3,col].Borders[xlAround].Weight := xlThin;
      xlsSheet.Cells.Item[row + 3,col].Interior.Color := clSilver;

      xlsSheet.Cells.Item[row + 4,col].Value := sqlQry1.FieldByName('LPMSKey_Interval').AsString;
      xlsSheet.Cells.Item[row + 4,col].Borders[xlAround].Weight := xlThin;
      xlsSheet.Cells.Item[row + 4,col].Interior.Color := clSilver;

      if (sqlQry1.FieldByName('LPMSKey_Blocked').AsInteger = 0) then
         xlsSheet.Cells.Item[row + 5,col].Value := 'No'
      else
         xlsSheet.Cells.Item[row + 5,col].Value := 'Yes';

      xlsSheet.Cells.Item[row + 5,col].Borders[xlAround].Weight := xlThin;
      xlsSheet.Cells.Item[row + 5,col].Interior.Color := clSilver;

      row := row + 8;
      col := 1;

//--- Insert the User Information for this Company

      ReadUser(ThisPref);

      while (sqlQry2.Eof = false) do begin
         xlsSheet.Cells.Item[row,col].Value := sqlQry2.FieldByName('LPMSKey_Key').AsString;
         xlsSheet.Cells.Item[row,col].Borders[xlAround].Weight := xlThin;

         xlsSheet.Cells.Item[row,col +  1].Value := sqlQry2.FieldByName('LPMSKey_Name').AsString;
         xlsSheet.Cells.Item[row,col +  1].Borders[xlAround].Weight := xlThin;

         xlsSheet.Cells.Item[row,col +  2].Value := sqlQry2.FieldByName('LPMSKey_Company').AsString;
         xlsSheet.Cells.Item[row,col +  2].Borders[xlAround].Weight := xlThin;

         xlsSheet.Cells.Item[row,col +  3].Value := sqlQry2.FieldByName('LPMSKey_Email').AsString;
         xlsSheet.Cells.Item[row,col +  3].Borders[xlAround].Weight := xlThin;

         xlsSheet.Cells.Item[row,col +  4].Value := sqlQry2.FieldByName('LPMSKey_Contact').AsString;
         xlsSheet.Cells.Item[row,col +  4].Borders[xlAround].Weight := xlThin;

         xlsSheet.Cells.Item[row,col +  5].Value := sqlQry2.FieldByName('LPMSKey_Unique').AsString;
         xlsSheet.Cells.Item[row,col +  5].Borders[xlAround].Weight := xlThin;

         ThisCase := sqlQry2.FieldByName('LPMSKey_LicType').AsInteger;
         case ThisCase of
             1: xlsSheet.Cells.Item[row,col +  6].Value := 'Trial';
             2: xlsSheet.Cells.Item[row,col +  6].Value := 'Personal';
             3: xlsSheet.Cells.Item[row,col +  6].Value := 'Browse';
             4: xlsSheet.Cells.Item[row,col +  6].Value := 'Generic';
         end;
         xlsSheet.Cells.Item[row,col +  6].Borders[xlAround].Weight := xlThin;

         xlsSheet.Cells.Item[row,col +  7].Value := sqlQry2.FieldByName('LPMSKey_ExpiryDate').AsString;
         xlsSheet.Cells.Item[row,col +  7].Borders[xlAround].Weight := xlThin;

         xlsSheet.Cells.Item[row,col +  8].Value := sqlQry2.FieldByName('LPMSKey_Activation').AsString;
         xlsSheet.Cells.Item[row,col +  8].Borders[xlAround].Weight := xlThin;

         if (sqlQry2.FieldByName('LPMSKey_Blocked').AsInteger = 0) then
            xlsSheet.Cells.Item[row,col +  9].Value := 'No'
         else
            xlsSheet.Cells.Item[row,col +  9].Value := 'Yes';
         xlsSheet.Cells.Item[row,col +  9].Borders[xlAround].Weight := xlThin;

         xlsSheet.Cells.Item[row,col + 10].Value := sqlQry2.FieldByName('LPMSKey_Renewals').AsString;
         xlsSheet.Cells.Item[row,col + 10].Borders[xlAround].Weight := xlThin;
         xlsSheet.Cells.Item[row,col + 10].HorizontalAlignment := xlHalignRight;

         if (sqlQry2.FieldByName('LPMSKey_Transfer').AsInteger = 0) then
            xlsSheet.Cells.Item[row,col + 11].Value := 'No'
         else
            xlsSheet.Cells.Item[row,col + 11].Value := 'Yes';
         xlsSheet.Cells.Item[row,col + 11].Borders[xlAround].Weight := xlThin;

         xlsSheet.Cells.Item[row,col + 12].Value := sqlQry2.FieldByName('LPMSKey_NewPrefix').AsString;
         xlsSheet.Cells.Item[row,col + 12].Borders[xlAround].Weight := xlThin;

         ThisCase := sqlQry2.FieldByName('LPMSKey_NewLicense').AsInteger;
         case ThisCase of
             1: xlsSheet.Cells.Item[row,col + 13].Value := 'Trial';
             2: xlsSheet.Cells.Item[row,col + 13].Value := 'Personal';
             3: xlsSheet.Cells.Item[row,col + 13].Value := 'Browse';
             4: xlsSheet.Cells.Item[row,col + 13].Value := 'Generic';
         end;
         xlsSheet.Cells.Item[row,col + 13].Borders[xlAround].Weight := xlThin;

         row := row + 1;
         sqlQry2.Next;
      end;

      sqlQry1.Next;
   end;

//--- Insert a sheet with all the Unique Identifiers

   if (ReadUnique = false) then begin
      MessageDlg('Unexpected database error:' + ErrMsg + '. LPMS_Export will now terminate', mtWarning, [mbOK], 0);
      Close;
      Exit;
   end;

   row := 1;
   col := 1;

   xlsSheet := xlsBook.Sheets.Add;
   xlsSheet.Name := 'Unique Identifiers';

//--- Create the heading information

   xlsSheet.Cells.Item[row,col].Value := 'LPMS Access Control Manager - Company and User definition export';
   with xlsSheet.Range['A1', 'C1'] do begin
      Borders[xlAround].Weight := xlThin;
      Interior.Color           := clNavy;
      Font.Color               := clWhite;
      Font.Bold                := true;
   end;

   row := row + 2;

//--- Print the table headings

   xlsSheet.Cells.Item[row,col].Value := 'Unique';
   xlsSheet.Cells.Item[row,col].ColumnWidth := 15;
   xlsSheet.Cells.Item[row,col].Borders[xlAround].Weight := xlThin;
   xlsSheet.Cells.Item[row,col].Interior.Color := clNavy;
   xlsSheet.Cells.Item[row,col].Font.Color := clWhite;
   xlsSheet.Cells.Item[row,col].Font.Bold := true;

   xlsSheet.Cells.Item[row,col +  1].Value := 'User';
   xlsSheet.Cells.Item[row,col +  1].ColumnWidth := 30;
   xlsSheet.Cells.Item[row,col +  1].Borders[xlAround].Weight := xlThin;
   xlsSheet.Cells.Item[row,col +  1].Interior.Color := clNavy;
   xlsSheet.Cells.Item[row,col +  1].Font.Color := clWhite;
   xlsSheet.Cells.Item[row,col +  1].Font.Bold := true;

   xlsSheet.Cells.Item[row,col +  2].Value := 'Company';
   xlsSheet.Cells.Item[row,col +  2].ColumnWidth := 100;
   xlsSheet.Cells.Item[row,col +  2].Borders[xlAround].Weight := xlThin;
   xlsSheet.Cells.Item[row,col +  2].Interior.Color := clNavy;
   xlsSheet.Cells.Item[row,col +  2].Font.Color := clWhite;
   xlsSheet.Cells.Item[row,col +  2].Font.Bold := true;

   row := row + 1;

//--- Insert the Usernique Identifier details

   while (sqlQry2.Eof = false) do begin
      xlsSheet.Cells.Item[row,col].Value := sqlQry2.FieldByName('LPMSKey_Unique').AsString;
      xlsSheet.Cells.Item[row,col].Borders[xlAround].Weight := xlThin;
      if (sqlQry2.FieldByName('LPMSKey_Unique').AsString = '123456789ABC') then
         xlsSheet.Cells.Item[row,col].Font.Bold := true;

      xlsSheet.Cells.Item[row,col +  1].Value := sqlQry2.FieldByName('LPMSKey_Name').AsString;
      xlsSheet.Cells.Item[row,col +  1].Borders[xlAround].Weight := xlThin;

      xlsSheet.Cells.Item[row,col +  2].Value := sqlQry2.FieldByName('LPMSKey_Company').AsString;
      xlsSheet.Cells.Item[row,col +  2].Borders[xlAround].Weight := xlThin;

      row := row + 1;
      sqlQry2.Next;
   end;

//--- Write the Excel file to disk

   xlsBook.SaveAs(edtOutFile.Text);

//--- Now open the Excel workbook

   ShellExecute(Handle,'open',PChar(edtOutFile.Text),nil,nil,SW_SHOWNORMAL);

}

   wsgGrid.LoadFromSpreadsheetFile(FileName + '.xlsx');
   wsgGrid.GetSheets(cbxSheets.Items);

   if cbxSheets.Items.Count <> -1 then begin

      cbxSheets.ItemIndex := cbxSheets.Items.Count - 1;
      cbxSheetsSelect(Application);

   end;

   Result := True;

end;

//------------------------------------------------------------------------------
// Procedure to load the sheet selected from the Sheets combobox
//------------------------------------------------------------------------------
procedure TFLPMS_Excel.cbxSheetsSelect( Sender: TObject);
begin

   wsgGrid.SelectSheetByIndex(cbxSheets.ItemIndex);

end;

//------------------------------------------------------------------------------
// User clicked on the Export button
//------------------------------------------------------------------------------
procedure TFLPMS_Excel.btnExportClick(Sender: TObject);
var
   Ext    : string;
   Format : TsSpreadsheetFormat;

begin

   Screen.Cursor := crHourglass;

   try

     case cbxFormat.ItemIndex of

        0: begin                // Excel 2007+ Format (*.xlsx)

           Format := sfOOXML;
           Ext    := '.xlxs';

        end;

        1: begin                // Excel 95-2003 Format (*.xls)

           Format := sfExcel8;
           Ext    := '.xls';

        end;

        2: begin                // Open/LibreOffice Format (*.ods)

           Format := sfOpenDocument;
           Ext    := '.ods';

        end;

        3: begin                // CSV (Text) Format (*.csv)

           Format := sfCSV;
           Ext    := '.csv';

        end;

   end;

   wsgGrid.SaveToSpreadsheetFile(FileName + Ext,Format,True);

   finally

      Screen.Cursor := crDefault;

   end;

   if cbOpen.Checked = True then begin

      OpenDocument(FileName + Ext);

   end;

end;

//------------------------------------------------------------------------------
// User clicked on the button to open the generated spreadsheet
//------------------------------------------------------------------------------
procedure TFLPMS_Excel. btnOpenClick( Sender: TObject);
begin

   OpenDocument(FileName + '.xlsx');

   if cbClose.Checked = True then
      Close();

end;

//===========================================================================
//===                                                                     ===
//=== Database Functions and Procedures                                   ===
//===                                                                     ===
//===========================================================================

//---------------------------------------------------------------------------
// Function to open a connection to the datastore
//---------------------------------------------------------------------------
function TFLPMS_Excel.OpenDB(): boolean;
begin

   try

      sqlQry1.Close;
      sqlQry2.Close;
      sqlCon.Close;

      sqlCon.HostName     := Host;
      sqlCon.UserName     := UserID;
      sqlCon.Password     := Password;
      sqlCon.DatabaseName := 'lpmsdefault';
      sqlQry1.DataBase    := sqlCon;
      sqlQry2.DataBase    := sqlCon;

      sqlCon.Open;

   except

      ErrMsg := '''Unable to connect to ' + Host + '''';
      Result := False;
      Exit;

   end;

   Result := True;

end;

//---------------------------------------------------------------------------
// Procedure to close the datastore connection
//---------------------------------------------------------------------------
procedure TFLPMS_Excel.CloseDB();
begin

   sqlQry1.Close;
   sqlQry2.Close;

   sqlCon.Close;

end;

//---------------------------------------------------------------------------
// Function to retrieve the Company records
//---------------------------------------------------------------------------
function TFLPMS_Excel.ReadCpy(): boolean;
var
   S1 : string;

begin

   S1 := 'SELECT * FROM companies ORDER BY LPMSKey_Prefix';

   try

     sqlQry1.Close;
      sqlQry1.SQL.Text := S1;
      sqlQry1.Open;

   except

      ErrMsg := '''Unable to read from ' + Host + ''' (companies)';
      Result := False;
      Exit;

   end;

   Result := True;

end;

//---------------------------------------------------------------------------
// Function to retrieve the User records
//---------------------------------------------------------------------------
function TFLPMS_Excel.ReadUser(Prefix : string): boolean;
var
   S1 : string;

begin

   S1 := 'SELECT * FROM users WHERE LPMSKey_Prefix = ''' + Prefix + '''ORDER BY LPMSKey_Name';

   try

      sqlQry2.Close;
      sqlQry2.SQL.Text := S1;
      sqlQry2.Open;

   except

      ErrMsg := '''Unable to read from ' + Host + ''' (users)';
      Result := False;
      Exit;

   end;

   Result := True;

end;

//---------------------------------------------------------------------------
// Function to retrieve all the Unique identifier records
//---------------------------------------------------------------------------
function TFLPMS_Excel.ReadUnique(): boolean;
var
   S1 : string;

begin

   S1 := 'SELECT LPMSKey_Unique, LPMSKey_Name, LPMSKey_Company FROM users ORDER BY LPMSKey_Unique';

   try

      sqlQry2.Close;
      sqlQry2.SQL.Text := S1;
      sqlQry2.Open;

   except

      ErrMsg := '''Unable to read from ' + Host + ''' (users)';
      Result := False;
      Exit;

   end;

   Result := True;

end;

//------------------------------------------------------------------------------

end.

