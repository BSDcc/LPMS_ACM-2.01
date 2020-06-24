//------------------------------------------------------------------------------
// Date.......: 10 May 2020
// System.....: LPMS Access Control Manager
// Program ID.: LPMS_Show
// Platform...: Lazarus (Winblows, Linux, Raspbian & macOS)
// Author.....: Francois De Bruin Meyer (BlueCrane Software Development CC)
//------------------------------------------------------------------------------
// History....: 10 May 2020 - Adapt from LPMS C++ version
//------------------------------------------------------------------------------

unit LPMS_Show;

{$mode objfpc}{$H+}

interface

//------------------------------------------------------------------------------
// Uses clause
//------------------------------------------------------------------------------
uses
   Classes, SysUtils, sqldb, Forms, Controls, Graphics, Dialogs, StdCtrls,
   ComCtrls, LCLType, StrUtils,

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
   {$IFDEF CPUI386}                  // Running on older hardware - Widget set must be Carbon
      mysql55conn;
   {$ELSE}                           // Running on new hardware - Widget set must be Cocoa
      mysql57conn;
   {$ENDIF}
{$ENDIF}

//------------------------------------------------------------------------------
// Declarations
//------------------------------------------------------------------------------
type

   { TFLPMS_Show }

   TFLPMS_Show = class( TForm)
      btnReturn: TButton;
      btnSelect: TButton;
      edtSelected: TEdit;
      lvRecords: TListView;
      sqlQry1: TSQLQuery;
      sqlTran: TSQLTransaction;
      stUnique: TStaticText;
      procedure btnReturnClick( Sender: TObject);
      procedure btnSelectClick( Sender: TObject);
      procedure FormCreate( Sender: TObject);
      procedure FormShow( Sender: TObject);
      procedure lvRecordsSelectItem( Sender: TObject; Item: TListItem; Selected: Boolean);

private   { Private Declarations }

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
   {$IFDEF CPUI386}                // Running on older hardware
      sqlCon : TMySQL55Connection;
   {$ELSE}                         // Running on new hardware
      sqlCon : TMySQL57Connection;
   {$ENDIF}
{$ENDIF}

   function  MySQLAccess(S1: string) : boolean;
   function  ReplaceQuote(S1: string) : string;
   procedure SetPlatform();

public    { Public Declarations }

   ThisUnique : string;            // The Unique identifier to show

end;

//------------------------------------------------------------------------------
// Global variables
//------------------------------------------------------------------------------
var
   FLPMS_Show: TFLPMS_Show;

implementation

   uses LPMS_Main;

{$R *.lfm}

   { TFLPMS_Show }

//------------------------------------------------------------------------------
// Executed when the Form is created
//------------------------------------------------------------------------------
procedure TFLPMS_Show.FormCreate( Sender: TObject);
begin

{$IFDEF WINDOWS}                    // Target is Winblows
   sqlCon  := TMySQL56Connection.Create(nil);
{$ENDIF}

{$IFDEF LINUX}                      // Target is Linux
   {$IFDEF CPUARMHF}                // Running on ARM (Raspbian) architecture
      sqlCon  := TMySQL55Connection.Create(nil);
   {$ELSE}                          // Running on Intel architecture
     sqlCon  := TMySQL57Connection.Create(nil);
   {$ENDIF}
{$ENDIF}

{$IFDEF DARWIN}                     // Target is macOS
   {$IFDEF CPUI386}                 // Running on older hardwarre
      sqlCon := TMySQL55Connection.Create(nil);
   {$ELSE}                          // Running on new hardware
      sqlCon := TMySQL57Connection.Create(nil);
   {$ENDIF}
{$ENDIF}

   sqlTran.DataBase    := sqlCon;
   sqlQry1.Transaction := sqlTran;

   SetPlatform();

end;

//------------------------------------------------------------------------------
// Excuted when the Form is shown
//------------------------------------------------------------------------------
procedure TFLPMS_Show.FormShow( Sender: TObject);
var
   idx1     : integer;
   S1       : string;
   ThisList : TListItem;

const
   Types    : array[1..5] of string = ('None','Trial','Personal','Browse','Generic');

begin

   stUnique.Caption := 'LPMS_ACM Definitions containing ''' + ThisUnique + ''' as a Unique Identifier';

//--- Read all the User records containing this Unique Identifier

   S1 := 'SELECT * FROM users WHERE LPMSKey_Unique = ''' + ThisUnique + ''' ORDER BY LPMSKey_Company';
   MySQLAccess(S1);

   sqlQry1.First();
   lvRecords.Clear();

   for idx1 := 0 to sqlQry1.RecordCount - 1 do begin

      ThisList := lvRecords.Items.Add();

      ThisList.Caption := ReplaceQuote(sqlQry1.FieldByName('LPMSKey_Company').AsString);
      ThisList.SubItems.Add(ReplaceQuote(sqlQry1.FieldByName('LPMSKey_Name').AsString));
      ThisList.SubItems.Add(ReplaceQuote(sqlQry1.FieldByName('LPMSKey_ExpiryDate').AsString));
      ThisList.SubItems.Add(Types[sqlQry1.FieldByName('LPMSKey_LicType').AsInteger + 1]);

      if sqlQry1.FieldByName('LPMSKey_Blocked').AsInteger = 0 then
         ThisList.SubItems.Add('No')
      else
         ThisList.SubItems.Add('Yes');

      ThisList.SubItems.Add(ReplaceQuote(sqlQry1.FieldByName('LPMSKey_Key').AsString));

      sqlQry1.Next();
   end;

   sqlQry1.Close();
   sqlCon.Close();

end;

//------------------------------------------------------------------------------
// User selected a record from the ListView
//------------------------------------------------------------------------------
procedure TFLPMS_Show.lvRecordsSelectItem( Sender: TObject; Item: TListItem; Selected: Boolean);
begin

   btnSelect.Enabled := True;
   edtSelected.Text  := Item.Caption + ', ' + Item.SubItems.Strings[0] + ', ' + ThisUnique;
   btnSelect.Caption := 'Select';

end;

//------------------------------------------------------------------------------
// User clicked on the Return button
//------------------------------------------------------------------------------
procedure TFLPMS_Show. btnReturnClick( Sender: TObject);
begin

   FLPMS_Main.ThisName := '';
   Close();

end;

//------------------------------------------------------------------------------
// User clicked on the Select button
//------------------------------------------------------------------------------
procedure TFLPMS_Show. btnSelectClick( Sender: TObject);
begin

   FLPMS_Main.ThisCompany := lvRecords.Selected.Caption;
   FLPMS_Main.ThisName    := lvRecords.Selected.SubItems.Strings[0];
   FLPMS_Main.ThisUnique  := ThisUnique;

   Close();

end;

//------------------------------------------------------------------------------
// MySQL database access function
//------------------------------------------------------------------------------
function TFLPMS_Show.MySQLAccess(S1: string) : boolean;
begin

   sqlQry1.Close();
   sqlCon.Close();

   sqlCon.HostName     := FLPMS_Main.HostName;
   sqlCon.UserName     := FLPMS_Main.UserName;
   sqlCon.Password     := FLPMS_Main.Password;
   sqlCon.DatabaseName := 'lpmsdefault';
   sqlQry1.DataBase    := sqlCon;

   try

      sqlQry1.Close();
      sqlQry1.SQL.Text := S1;
      sqlQry1.Open();

      except on E : Exception do begin

         Application.MessageBox(Pchar('FATAL: Unexpected database error: ' + #10 + #10 + '''' + E.Message + ''''),'LPMS Access Control Manager - Login',(MB_OK + MB_ICONSTOP));
         Result := False;
         Exit;

      end;

   end;

   Result := True;

end;

//------------------------------------------------------------------------------
// Function to replace &quote with "'" and &slash with "\"
//------------------------------------------------------------------------------
function TFLPMS_Show.ReplaceQuote(S1: string) : string;
begin

   S1 := AnsiReplaceStr(S1,'&quot','''');
   S1 := AnsiReplaceStr(S1,'&slash','\');

   Result := S1;

end;

//------------------------------------------------------------------------------
// Adjust some display artifacts for optimum presentation on the execution
// platform
//------------------------------------------------------------------------------
procedure TFLPMS_Show.SetPlatform();
begin

{$IFDEF WINDOWS}

   btnReturn.Top :=  324;
   btnSelect.Top :=  324;

{$ENDIF}

{$IFDEF LINUX}

   btnReturn.Top :=  321;
   btnSelect.Top :=  321;

{$ENDIF}

{$IFDEF DARWIN}

   btnReturn.Top :=  321;
   btnSelect.Top :=  321;

{$ENDIF}

end;

//------------------------------------------------------------------------------

end.

