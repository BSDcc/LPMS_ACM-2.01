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
   StdCtrls, ActnList, EditBtn, Buttons, LCLIntf, LCLType, StdActns,
   fpspreadsheet, fpspreadsheetgrid, fpspreadsheetctrls, fpstypes, xlsbiff8,
   fpsopendocument, fpsCSV, xlsxOOXML,

{$IFDEF DARWIN}                      // Target is macOS
   Zipper, StrUtils, DateUtils, SMTPSend, MimeMess, MimePart, SynaUtil,
   ComCtrls,
 {$IFDEF CPUI386}                    // Running on older hardware - Widget set must be Carbon
      mysql55conn, Interfaces;
  {$ELSE}                            // Running on new hardware - Widget set must be Cocoa
      mysql57conn, Interfaces;
  {$ENDIF}
{$ENDIF}

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

//------------------------------------------------------------------------------
// Declarations
//------------------------------------------------------------------------------
type

   { TFLPMS_Excel }

   TFLPMS_Excel = class( TForm)
   alActions: TActionList;
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
      FileSaveAs: TFileSaveAs;
      Label1: TLabel;
      Panel1: TPanel;
      Panel2: TPanel;
      Panel3: TPanel;
      Panel4: TPanel;
      Panel5: TPanel;
      Panel6: TPanel;
      dlgSelDir: TSelectDirectoryDialog;
      sCellEdit1: TsCellEdit;
      sCellIndicator1: TsCellIndicator;
      wtcTabControl: TsWorkbookTabControl;
      wbsSource: TsWorkbookSource;
      wsgGrid: TsWorksheetGrid;
      procedure btnExportClick( Sender: TObject);
      procedure btnOpenClick( Sender: TObject);
      procedure btnReturnClick( Sender: TObject);
      procedure FileSaveAsAccept( Sender: TObject);
      procedure FileSaveAsBeforeExecute( Sender: TObject);
      procedure FormClose( Sender: TObject; var CloseAction: TCloseAction);
      procedure FormCreate( Sender: TObject);
      procedure FormShow( Sender: TObject);
private   {Private Declarations}

   SpreadSheetName : string;        // Holds the Path and Name (without the Ext) of the Generated File
   ErrMsg          : string;        // Holds last MySQL error message
   InitDir         : string;        // Remembers the Initial Directory from the first File Save when the form is shown

{$IFDEF WINDOWS}                   // Target is Winblows
   sqlCon  : TMySQL56Connection;
   sqlTran : TSQLTransaction;
   sqlQry1 : TSQLQuery;
   sqlQry2 : TSQLQuery;
{$ENDIF}

{$IFDEF LINUX}                     // Target is Linux
   {$IFDEF CPUARMHF}               // Running on ARM (Raspbian) architecture
      sqlCon : TMySQL55Connection;
   {$ELSE}                         // Running on Intel architecture
      sqlCon : TMySQL57Connection;
   {$ENDIF}
   sqlTran : TSQLTransaction;
   sqlQry1 : TSQLQuery;
   sqlQry2 : TSQLQuery;
{$ENDIF}

{$IFDEF DARWIN}                    // Target is macOS
   {$IFDEF CPUARMHF}               // Running on older hardware
      sqlCon : TMySQL55Connection;
   {$ELSE}                         // Running on new hardware
      sqlCon : TMySQL57Connection;
   {$ENDIF}
   sqlTran : TSQLTransaction;
   sqlQry1 : TSQLQuery;
   sqlQry2 : TSQLQuery;
{$ENDIF}

   function  CreateSheet(): boolean;
   function  OpenDB(): boolean;
   procedure CloseDB();
   function  ReadCpy(): boolean;
   function  ReadUser(Prefix : string): boolean;
   function  ReadUnique(): boolean;

type

   RE_RSLT =  (ERR_INVALID,         // The content of the Key is invalid
               ERR_LENGTH,          // The length of the Key is wrong
               ERR_EXPIRED);        // The Key has expired

public    {Public Declarations}

   Host     : string;       // Passed from calling Form
   UserID   : string;       // Passed from calling Form
   Password : string;       // Passed from calling Form

end;

//------------------------------------------------------------------------------
// Global variables
//------------------------------------------------------------------------------

type

   REC_Key_Priv = record
      Key              : string;
      DaysLeft         : integer;
      LPMS_Collections : boolean;
      LPMS_DocGen      : boolean;
      LPMS_Floating    : boolean;
      LPMS_Options4    : boolean;
      License          : integer;
      DBPrefix         : string;
      Unique           : string;
      KeyDate          : string;
   end;


var
   FLPMS_Excel: TFLPMS_Excel;

   This_Key_Priv   : REC_Key_Priv;

//--- Utilities contained in BSD_Utilities.dll

{$IFDEF LINUX}
   function DoDecode(var Decode_Key_Priv: REC_Key_Priv): integer; StdCall; external 'libbsd_utilities';
{$ENDIF}
{$IFDEF WINDOWS}
   function DoDecode(var Decode_Key_Priv: REC_Key_Priv): integer; StdCall; external 'BSD_Utilities';
{$ENDIF}

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

{$IFDEF WINDOWS}                    // Target is Winblows
   sqlCon  := TMySQL56Connection.Create(nil);
   sqlTran := TSQLTransaction.Create(nil);
   sqlQry1 := TSQLQuery.Create(nil);
   sqlQry2 := TSQLQuery.Create(nil);
{$ENDIF}

{$IFDEF LINUX}                      // Target is Linux
   {$IFDEF CPUARMHF}                // Running on ARM (Raspbian) architecture
      sqlCon := TMySQL55Connection.Create(nil);
   {$ELSE}                          // Running on Intel architecture
      sqlCon := TMySQL57Connection.Create(nil);
   {$ENDIF}
   sqlTran := TSQLTransaction.Create(nil);
   sqlQry1 := TSQLQuery.Create(nil);
   sqlQry2 := TSQLQuery.Create(nil);
{$ENDIF}

{$IFDEF DARWIN}                     // Target is macOS
   {$IFDEF CPUI386}                 // Running on older hardware
      sqlCon := TMySQL55Connection.Create(nil);
   {$ELSE}                          // Running on new hardware
      sqlCon := TMySQL57Connection.Create(nil);
   {$ENDIF}
   sqlTran := TSQLTransaction.Create(nil);
   sqlQry1 := TSQLQuery.Create(nil);
   sqlQry2 := TSQLQuery.Create(nil);
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

//--- Request a location for saving the output file from the user

  FileSaveAs.Dialog.FileName   := FormatDateTime('yyyyMMdd',Date()) + ' - LPMS_ACM Company and User Definitions.xlsx';

   if FileSaveAs.Dialog.Execute = False then begin

      Close();
      Exit;

   end;

   SpreadSheetName := ChangeFileExt(ExtractFileName(FileSaveAs.Dialog.FileName),'');
   InitDir         := ExtractFilePath(FileSaveAs.Dialog.FileName);

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
   DaysLeft       : integer;          // Days left on a User's key
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

      for idx1 := Col to Col+18 do begin

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
         xlsSheet.WriteBackgroundColor(idx1,Col,clSilver);
         xlsSheet.WriteBackgroundColor(idx1,Col+1,clSilver);
         xlsSheet.WriteFontStyle(idx1,Col,[fssBold]);

      end;

      Row := Row + 7;

//--- Generate the table headings

      xlsSheet.WriteText(Row,Col,'Key');
      xlsSheet.WriteColWidth(Col,5,suChars);

      xlsSheet.WriteText(Row,Col+1,'User');
      xlsSheet.WriteColWidth(Col+1,30,suChars);

      xlsSheet.WriteText(Row,Col+2,'Company');
      xlsSheet.WriteColWidth(Col+2,35,suChars);

      xlsSheet.WriteText(Row,Col+3,'Email');
      xlsSheet.WriteColWidth(Col+3,30,suChars);

      xlsSheet.WriteText(Row,Col+4,'Contact');
      xlsSheet.WriteColWidth(Col+4,15,suChars);

      xlsSheet.WriteText(Row,Col+5,'Unique');
      xlsSheet.WriteColWidth(Col+5,15,suChars);

      xlsSheet.WriteText(Row,Col+6,'License');
      xlsSheet.WriteColWidth(Col+6,20,suChars);

      xlsSheet.WriteText(Row,Col+7,'Expire');
      xlsSheet.WriteColWidth(Col+7,12,suChars);

      xlsSheet.WriteText(Row,Col+8,'Days Left');
      xlsSheet.WriteColWidth(Col+8,22,suChars);

      xlsSheet.WriteText(Row,Col+9,'Activation Key');
      xlsSheet.WriteColWidth(Col+9,45,suChars);

      xlsSheet.WriteText(Row,Col+10,'Collections');
      xlsSheet.WriteColWidth(Col+10,12,suChars);

      xlsSheet.WriteText(Row,Col+11,'Doc Gen');
      xlsSheet.WriteColWidth(Col+11,12,suChars);

      xlsSheet.WriteText(Row,Col+12,'Floating');
      xlsSheet.WriteColWidth(Col+12,12,suChars);

      xlsSheet.WriteText(Row,Col+13,'Reserved');
      xlsSheet.WriteColWidth(Col+13,12,suChars);

      xlsSheet.WriteText(Row,Col+14,'Blocked');
      xlsSheet.WriteColWidth(Col+14,12,suChars);

      xlsSheet.WriteText(Row,Col+15,'Renewals');
      xlsSheet.WriteColWidth(Col+15,12,suChars);
      xlsSheet.WriteHorAlignment(Row,Col+15,haRight);

      xlsSheet.WriteText(Row,Col+16,'Transfer');
      xlsSheet.WriteColWidth(Col+16,12,suChars);

      xlsSheet.WriteText(Row,Col+17,'New Prefix');
      xlsSheet.WriteColWidth(Col+17,12,suChars);

      xlsSheet.WriteText(Row,Col+18,'New License');
      xlsSheet.WriteColWidth(Col+18,20,suChars);

      for idx1 := Col to Col + 18 do begin

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

//--- Decode the Key and populate the fields contained in the key

         This_Key_Priv.Key := sqlQry2.FieldByName('LPMSKey_Activation').AsString;
         DaysLeft := DoDeCode(This_Key_Priv);

         if DaysLeft < 0 then begin

            if DaysLeft = ord(ERR_EXPIRED) - 3 then
               xlsSheet.WriteText(Row,Col+8,'Key has Expired!')
            else
               xlsSheet.WriteText(Row,Col+8,'Key is Invalid!');

         end else
            xlsSheet.WriteText(Row,Col+8,IntToStr(DaysLeft) + ' Days remaining');

         xlsSheet.WriteText(Row,Col+9,sqlQry2.FieldByName('LPMSKey_Activation').AsString);

         if This_Key_Priv.LPMS_Collections = True then
            xlsSheet.WriteText(Row,Col+10,'Yes')
         else
            xlsSheet.WriteText(Row,Col+10,'No');

         if This_Key_Priv.LPMS_DocGen = True then
            xlsSheet.WriteText(Row,Col+11,'Yes')
         else
            xlsSheet.WriteText(Row,Col+11,'No');

         if This_Key_Priv.LPMS_Floating = True then
            xlsSheet.WriteText(Row,Col+12,'Yes')
         else
            xlsSheet.WriteText(Row,Col+12,'No');

         if This_Key_Priv.LPMS_Options4 = True then
            xlsSheet.WriteText(Row,Col+13,'Yes')
         else
            xlsSheet.WriteText(Row,Col+13,'No');

         if (sqlQry2.FieldByName('LPMSKey_Blocked').AsInteger = 0) then
            xlsSheet.WriteText(Row,Col+14,'No')
         else
            xlsSheet.WriteText(Row,Col+14,'Yes');

         xlsSheet.WriteText(Row,Col+15,sqlQry2.FieldByName('LPMSKey_Renewals').AsString);
         xlsSheet.WriteHorAlignment(Row,Col+15,haRight);


         if (sqlQry2.FieldByName('LPMSKey_Transfer').AsInteger = 0) then
            xlsSheet.WriteText(Row,Col+16,'No')
         else
            xlsSheet.WriteText(Row,Col+16,'Yes');

         xlsSheet.WriteText(Row,Col+17,sqlQry2.FieldByName('LPMSKey_NewPrefix').AsString);

         ThisCase := sqlQry2.FieldByName('LPMSKey_NewLicense').AsInteger;

         case ThisCase of

            0: xlsSheet.WriteText(Row,Col+18,'Not Transferred');
            1: xlsSheet.WriteText(Row,Col+18,'Trial');
            2: xlsSheet.WriteText(Row,Col+18,'Personal');
            3: xlsSheet.WriteText(Row,Col+18,'Browse');
            4: xlsSheet.WriteText(Row,Col+18,'Generic');

         end;

         for idx1 := Col to Col + 18 do begin
            xlsSheet.WriteBorders(Row,idx1,[cbWest,cbNorth,cbSouth,cbEast]);
         end;

         Row := Row + 1;
         sqlQry2.Next;

      end;

      sqlQry1.Next;

   end;

//--- Insert a sheet with all the Unique Identifiers

   if (ReadUnique = false) then begin

      Application.MessageBox(PChar('Unexpected database error:''' + ErrMsg + '''. LPMS_Export will now terminate'),'LPMS Export Manager', (MB_OK + MB_ICONWARNING));
      Close;
      Exit;

   end;

   Row := 0;
   Col := 0;

   xlsSheet := xlsBook.AddWorksheet('Unique Identifiers');

//--- Create the heading information

   xlsSheet.WriteText(Row,Col,'LPMS Access Control Manager - Company and User definition export');
   xlsSheet.WriteBorders(Row,Col,[cbWest]);
   xlsSheet.WriteBorders(Row,Col+2,[cbEast]);

   for idx1 := Col to Col+2 do begin

      xlsSheet.WriteBorders(Row,idx1,[cbNorth, cbSouth]);
      xlsSheet.WriteBackgroundColor(Row,idx1,clNavy);
      xlsSheet.WriteFontColor(Row,idx1,clWhite);
      xlsSheet.WriteFontStyle(Row,idx1,[fssBold]);

   end;

   Row := Row + 2;

//--- Print the table headings

   xlsSheet.WriteText(Row,Col,'Unique');
   xlsSheet.WriteColWidth(Col,20,suChars);

   xlsSheet.WriteText(Row,Col+1,'User');
   xlsSheet.WriteColWidth(Col+1,40,suChars);

   xlsSheet.WriteText(Row,Col+2,'Company');
   xlsSheet.WriteColWidth(Col+2,100,suChars);

   for idx1 := Col to Col + 2 do begin

      xlsSheet.WriteBorders(Row,idx1,[cbEast,cbNorth,cbSouth,cbWest]);
      xlsSheet.WriteBackgroundColor(Row,idx1,clNavy);
      xlsSheet.WriteFontColor(Row,idx1,clWhite);
      xlsSheet.WriteFontStyle(Row,idx1,[fssBold]);

   end;

   Row := Row + 1;

//--- Insert the Usernique Identifier details

   while (sqlQry2.Eof = false) do begin

      xlsSheet.WriteText(Row,Col,sqlQry2.FieldByName('LPMSKey_Unique').AsString);

      if (sqlQry2.FieldByName('LPMSKey_Unique').AsString = '123456789ABC') then
         xlsSheet.WriteFontStyle(Row,Col,[fssBold]);

      xlsSheet.WriteText(Row,Col+1,sqlQry2.FieldByName('LPMSKey_Name').AsString);
      xlsSheet.WriteText(Row,Col+2,sqlQry2.FieldByName('LPMSKey_Company').AsString);

      for idx1 := Col to Col + 2 do
         xlsSheet.WriteBorders(Row,idx1,[cbWest,cbNorth,cbSouth,cbEast]);

      Row := Row + 1;
      sqlQry2.Next;

   end;

//--- Save the spreadsheet to a file

   xlsBook.WriteToFile(InitDir + SpreadSheetName + '.xlsx', True);
   xlsBook.Free;

   wbsSource.LoadFromSpreadsheetFile(InitDir + SpreadSheetName + '.xlsx',sfOOXML);

   Result := True;

end;

//------------------------------------------------------------------------------
// User clicked on the Export button
//------------------------------------------------------------------------------
procedure TFLPMS_Excel.btnExportClick(Sender: TObject);
begin

   FileSaveAs.Execute

end;

//------------------------------------------------------------------------------
// Executed before the SaveAs dialog is shown
//------------------------------------------------------------------------------
procedure TFLPMS_Excel. FileSaveAsBeforeExecute( Sender: TObject);
var
   Ext    : string;

begin

   With FileSaveAs.Dialog do begin

      case cbxFormat.ItemIndex of

         0: begin                // Excel 2007+ Format (*.xlsx)

            Ext := '.xlsx';

            DefaultExt := '.xlsx';
            Filter     := 'Excel 2007+ File (*.xlsx)|*.xlsx';

         end;

         1: begin                // Excel 95-2003 Format (*.xls)

            Ext := '.xls';

            DefaultExt := '.xls';
            Filter     := 'Excel 95-2003 File (*.xls)|*.xls';

         end;

         2: begin                // Open/LibreOffice Format (*.ods)

            Ext := '.ods';

            DefaultExt := '.ods';
            Filter     := 'Open/Libre Office Document (*.ods)|*.ods';

         end;

         3: begin                // CSV (Text) Format (*.csv)

            Ext := '.csv';

            DefaultExt := '.csv';
            Filter     := 'CSV (Text) File (*.csv)|*.csv';

         end;

      end;

      FileName   := SpreadSheetName + Ext;
      InitialDir := InitDir;

   end;

end;

//------------------------------------------------------------------------------
// User Selected a Folder to store the exported spreadhseet and clicked on the
// Save button
//------------------------------------------------------------------------------
procedure TFLPMS_Excel.FileSaveAsAccept(Sender: TObject);
var
   Ext    : string;
   Format : TsSpreadsheetFormat;

begin

  case cbxFormat.ItemIndex of

     0: begin                // Excel 2007+ Format (*.xlsx)

        Format := sfOOXML;
        Ext    := '.xlsx';

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

   wbsSource.SaveToSpreadsheetFile(InitDir + SpreadSheetName + Ext,Format,True);

   if cbOpen.Checked = True then
      OpenDocument(InitDir + SpreadSheetName + Ext);

end;

//------------------------------------------------------------------------------
// User clicked on the button to open the generated spreadsheet
//------------------------------------------------------------------------------
procedure TFLPMS_Excel. btnOpenClick( Sender: TObject);
begin

   OpenDocument(InitDir + SpreadSheetName + '.xlsx');

   if cbClose.Checked = True then
      Close();

end;

//==============================================================================
//===                                                                        ===
//=== Database Functions and Procedures                                      ===
//===                                                                        ===
//==============================================================================

//------------------------------------------------------------------------------
// Function to open a connection to the datastore
//------------------------------------------------------------------------------
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

//------------------------------------------------------------------------------
// Procedure to close the datastore connection
//------------------------------------------------------------------------------
procedure TFLPMS_Excel.CloseDB();
begin

   sqlQry1.Close;
   sqlQry2.Close;

   sqlCon.Close;

end;

//------------------------------------------------------------------------------
// Function to retrieve the Company records
//------------------------------------------------------------------------------
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

//------------------------------------------------------------------------------
// Function to retrieve the User records
//------------------------------------------------------------------------------
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

//------------------------------------------------------------------------------
// Function to retrieve all the Unique identifier records
//------------------------------------------------------------------------------
function TFLPMS_Excel.ReadUnique(): boolean;
var
   S1 : string;

begin

   S1 := 'SELECT LPMSKey_Unique, LPMSKey_Name, LPMSKey_Company FROM users ORDER BY LPMSKey_Company ASC, LPMSKey_Name ASC';

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

