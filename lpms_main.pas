//------------------------------------------------------------------------------
// Date.......: 10 May 2020
// System.....: LPMS Access Control Manager
// Program ID.: LPMS_Main
// Platform...: Lazarus (Winblows, Linux, Raspbian & macOS)
// Author.....: Francois De Bruin Meyer (BlueCrane Software Development CC)
//------------------------------------------------------------------------------
// Description: This is the main module for the LPMS_ACM utility
//------------------------------------------------------------------------------
// History....: 10 May 2020 - Adapt from LPMS C++ version
//------------------------------------------------------------------------------

unit LPMS_Main;

{$mode objfpc}{$H+}

interface

//------------------------------------------------------------------------------
// Uses clause
//------------------------------------------------------------------------------
uses
   Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
   Menus, ActnList, StdCtrls, Buttons, Spin, DateTimePicker, StrUtils, LCLType,
   EditBtn, Process, sqldb, LazFileUtils,

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
   macOSAll,
  {$IFDEF CPUI386}                   // Running on older hardware - Widget set must be Carbon
      CarbonProc, mysql55conn;
   {$ELSE}                           // Running on new hardware - Widget set must be Cocoa
      CocoaUtils, mysql57conns;
   {$ENDIF}
{$ENDIF}

//------------------------------------------------------------------------------
// Declarations
//------------------------------------------------------------------------------
type

   { TFLPMS_Main }

   TFLPMS_Main = class( TForm)
   About1: TMenuItem;
   Actions1: TMenuItem;
   ActionsBackup: TAction;
   ActionsRefresh: TAction;
   ActionsRestore: TAction;
   actList: TActionList;
   Backup1: TMenuItem;
   edtPortB: TEdit;
   edtPortRe: TEdit;
   Label39: TLabel;
   Label41: TLabel;
   lblIPAddress: TLabel;
   RestoreLogRe1: TListView;
   Bevel1: TBevel;
   Bevel10: TBevel;
   Bevel2: TBevel;
   Bevel3: TBevel;
   Bevel4: TBevel;
   Bevel5: TBevel;
   Bevel6: TBevel;
   Bevel8: TBevel;
   Bevel9: TBevel;
   btnCancel: TButton;
   btnOpenRe: TButton;
   btnPaste: TSpeedButton;
   btnDecodeR: TButton;
   btnDelete: TButton;
   btnEncodeR: TButton;
   btnExit: TButton;
   btnFindR: TButton;
   btnLock: TBitBtn;
   btnLockB: TBitBtn;
   btnLockR: TBitBtn;
   btnNew: TButton;
   btnCopy: TSpeedButton;
   btnUnlock: TBitBtn;
   btnUnlockB: TBitBtn;
   btnUnlockR: TBitBtn;
   btnUpdate: TButton;
   cbAllowDuplicates: TCheckBox;
   cbBlockedC: TCheckBox;
   cbBlockedU: TCheckBox;
   cbCollectR: TCheckBox;
   cbCollectU: TCheckBox;
   cbDocGenR: TCheckBox;
   cbDocGenU: TCheckBox;
   cbEditEmail: TCheckBox;
   cbFloatingR: TCheckBox;
   cbFloatingU: TCheckBox;
   cbOption4R: TCheckBox;
   cbOption4U: TCheckBox;
   cbTransferU: TCheckBox;
   cbxKeyTypeC: TComboBox;
   cbxLicTypeR: TComboBox;
   cbxLicTypeU: TComboBox;
   cbxNewLicU: TComboBox;
   cbxNewPrefixU: TComboBox;
   edtBackupRe: TEditButton;
   edtHostNameRe: TEdit;
   edtLocationB: TDirectoryEdit;
   dlgOpen: TOpenDialog;
   dlgSave: TSaveDialog;
   dtpExpiryDateU: TDateTimePicker;
   dtpExpiryR: TDateTimePicker;
   edtPasswordRe: TEdit;
   edtUserIDB: TEdit;
   edtPasswordB: TEdit;
   edtTemplateB: TEdit;
   edtHostNameB: TEdit;
   edtCompanyC: TEdit;
   edtCompanyU: TEdit;
   edtContactU: TEdit;
   edtDBKeyU: TEdit;
   edtEmailU: TEdit;
   edtKeyU: TEdit;
   edtPrefixC: TEdit;
   edtPrefixR: TEdit;
   edtPrefixU: TEdit;
   edtRenewalsU: TEdit;
   edtUniqueF: TEdit;
   edtKeyR: TEdit;
   edtServerR: TEdit;
   edtUniqueR: TEdit;
   edtPasswordR: TEdit;
   edtUniqueU: TEdit;
   edtUserIDRe: TEdit;
   edtUserNameU: TEdit;
   Email1: TMenuItem;
   Exit1: TMenuItem;
   Export1: TMenuItem;
   File1: TMenuItem;
   FileExit: TAction;
   Generate1: TMenuItem;
   Help1: TMenuItem;
   Help2: TMenuItem;
   Image1: TImage;
   imgDis: TImageList;
   imgHot: TImageList;
   imgList: TImageList;
   imgNorm: TImageList;
   Label1: TLabel;
   Label10: TLabel;
   Label11: TLabel;
   Label12: TLabel;
   Label13: TLabel;
   Label14: TLabel;
   Label15: TLabel;
   Label16: TLabel;
   Label17: TLabel;
   Label18: TLabel;
   Label19: TLabel;
   Label2: TLabel;
   Label20: TLabel;
   Label21: TLabel;
   Label22: TLabel;
   Label23: TLabel;
   Label24: TLabel;
   Label25: TLabel;
   Label29: TLabel;
   Label3: TLabel;
   Label30: TLabel;
   Label31: TLabel;
   Label32: TLabel;
   Label33: TLabel;
   Label34: TLabel;
   Label35: TLabel;
   Label36: TLabel;
   Label37: TLabel;
   Label38: TLabel;
   Label4: TLabel;
   Label40: TLabel;
   Label42: TLabel;
   Label5: TLabel;
   Label6: TLabel;
   Label7: TLabel;
   Label8: TLabel;
   Label9: TLabel;
   BackupLog: TListView;
   mnuMain: TMainMenu;
   N1: TMenuItem;
   ools1: TMenuItem;
   Panel1: TPanel;
   Panel2: TPanel;
   Panel3: TPanel;
   Panel4: TPanel;
   pnlBackup: TPanel;
   pnlCompany: TPanel;
   pnlRestore: TPanel;
   pnlRoot: TPanel;
   pnlUser: TPanel;
   Refresh1: TMenuItem;
   Restore1: TMenuItem;
   adoQry1: TSQLQuery;
   adoQry2: TSQLQuery;
   btnClear: TSpeedButton;
   RestoreLogRe2: TListView;
   speRenewalC: TSpinEdit;
   speLicCountC: TSpinEdit;
   speReadBlockB: TSpinEdit;
   Splitter1: TSplitter;
   StaticText1: TStaticText;
   StaticText2: TStaticText;
   StaticText3: TStaticText;
   StatusBar1: TStatusBar;
   ToolBar1: TToolBar;
   ToolButton1: TToolButton;
   ToolButton10: TToolButton;
   ToolButton11: TToolButton;
   ToolButton2: TToolButton;
   ToolButton3: TToolButton;
   ToolButton4: TToolButton;
   ToolButton5: TToolButton;
   ToolButton6: TToolButton;
   ToolButton7: TToolButton;
   ToolButton8: TToolButton;
   ToolButton9: TToolButton;
   ToolsEmail: TAction;
   ToolsExport: TAction;
   ToolsGenerate: TAction;
   tvTree: TTreeView;
   txtExpired: TStaticText;
   txtInvalid: TStaticText;
   procedure ActionsBackupExecute( Sender: TObject);
   procedure ActionsRefreshExecute( Sender: TObject);
   procedure ActionsRestoreExecute( Sender: TObject);
   procedure btnCancelClick(Sender: TObject);
   procedure btnClearClick( Sender: TObject);
   procedure btnCopyClick( Sender: TObject);
   procedure btnDecodeRClick( Sender: TObject);
   procedure btnDeleteClick(Sender: TObject);
   procedure btnEncodeRClick( Sender: TObject);
   procedure btnFindRClick( Sender: TObject);
   procedure btnLockBClick( Sender: TObject);
   procedure btnLockClick( Sender: TObject);
   procedure btnLockRClick( Sender: TObject);
   procedure btnNewClick(Sender: TObject);
   procedure btnOpenReClick(Sender: TObject);
   procedure btnPasteClick( Sender: TObject);
   procedure btnUnlockBClick( Sender: TObject);
   procedure btnUnlockClick( Sender: TObject);
   procedure btnUnlockRClick( Sender: TObject);
   procedure btnUpdateClick(Sender: TObject);
   procedure cbBlockedUClick(Sender: TObject);
   procedure cbTransferUClick(Sender: TObject);
   procedure edtBackupReButtonClick(Sender: TObject);
   procedure edtBackupReChange(Sender: TObject);
   procedure edtKeyRChange( Sender: TObject);
   procedure edtKeyRKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
   procedure edtLocationBAcceptDirectory( Sender: TObject; var Value: String);
   procedure edtPrefixCChange( Sender: TObject);
   procedure edtUniqueFChange( Sender: TObject);
   procedure edtUniqueUChange(Sender: TObject);
   procedure edtUserNameUChange(Sender: TObject);
   procedure FileExitExecute(Sender: TObject);
   procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
   procedure FormCreate(Sender: TObject);
   procedure FormShow(Sender: TObject);
   procedure ToolsEmailExecute(Sender: TObject);
   procedure ToolsExportExecute(Sender: TObject);
   procedure ToolsGenerateExecute(Sender: TObject);
   procedure tvTreeClick(Sender: TObject);

private  { Private Declarations }

{$IFDEF WINDOWS}                   // Target is Winblows
   sqlCon  : TMySQL56Connection;
   sqlTran : TSQLTransaction;
   sqlQry1 : TSQLQuery;
{$ENDIF}

{$IFDEF LINUX}                     // Target is Linux
   {$IFDEF CPUARMHF}               // Running on ARM (Raspbian) architecture
      sqlCon : TMySQL55Connection;
   {$ELSE}                         // Running on Intel architecture
      sqlCon : TMySQL57Connection;
   {$ENDIF}
   sqlTran : TSQLTransaction;
   sqlQry1 : TSQLQuery;
{$ENDIF}

{$IFDEF DARWIN}
   {$IFDEF CPUI386}               // Running on older hardware
      sqlCon : TMySQL55Connection;
   {$ELSE}                        // Running on new harsdware
      sqlCon : TMySQL57Connection;
   {$ENDIF}
   sqlTran : TSQLTransaction;
   sqlQry1 : TSQLQuery;
{$ENDIF}

   PlaceHolder                                        : integer;
   DoSave, CanUpdate, DoGen, FirstRun, RestoreSuccess : boolean;
   ClipKey, RestoreFile                               : string;
   Root                                               : TTreeNode;

   procedure RunBackup();
   procedure RunRestore();
   procedure OpenDB(ThisType: integer);
   function  GetData(ThisType: integer; Company, User, Unique: string) : boolean;
   function  InputQueryM(ThisCap, Question : string; DispType: integer) : string;
   function  MySQLAccess(ThisType: integer; S1 : string; adoQry: TSQLQuery) : boolean;
   function  DelData(ThisType: integer; Company, User: string) : boolean;
   function  UpdateCpy() : boolean;
   function  CpyExists(Prefix: string) : boolean;
   function  UserExists(User, Prefix: string) : boolean;
   function  UpdateUser() : boolean;
   function  GetUnique(Unique: string; ThisType: integer; var UniqueCount: integer) : string;
   procedure LPMS_ACM_Abort(Msg: string);
   function  ReplaceQuote(S1: string; ThisType: integer) : string;
   procedure SetPlatform();

type
   DB_TYPE =  (DB_OPEN,             // Open the Database
               DB_CLOSE,            // Close the Database
               DB_ALL,              // Get all prefix records from the database
               DB_COMPANY,          // Get all Company records
               DB_USER,             // Get all User records
               DB_UNIQUE,           // Get all the records for a sepcific Unique
               DB_SELECT,           // 'SELECT' statement
               DB_OTHER);           // All other SQL statements

   TY_TYPE =  (TYPE_PASSWORD,       // Used when calling LPMS_InputQuery
               TYPE_TEST,           //
               TYPE_XFER,           //
               TYPE_READ,           // Reading from the database - &quote to '''
               TYPE_WRITE);         // Writing to the database - ''' to &quote

   RE_RSLT =  (ERR_INVALID,         // The content of the Key is invalid
               ERR_LENGTH,          // The length of the Key is wrong
               ERR_EXPIRED);        // The Key has expired

   RES_TYPE = (RT_OPEN,             // Extract Restore information from the BackupFile
               RT_RESTORE);         // Restore from the Backup File

public   { Public Declarations }

   UserName     : string;      // UserName passed from Login
   Password     : string;      // Password passed from Login
   HostName     : string;      // Host name passed from Login
   Version      : string;      // Version string passed from Login
   CopyRight    : string;      // Copyright notice passed from Login
   ThisRes      : string;      // Holds result from InoutQuery
   ThisName     : string;      // Used by LPMS_Show
   ThisCompany  : string;      // Used by LPMS_Show
   ThisUnique   : string;      // Used by LPMS_Show
   ThisSMTPHost : string;      // SMTP Host for sending emails
   ThisSMTPPass : string;      // SMTP Password for sending emails
   PassPhrase   : string;      // Contains the Phass Phrase to unlock restricted activities
   AutoKey      : string;      // Set by Login. If autoKey contains a key then itis automatically displayed and decoded
   ACMPort      : string;      // Port number on which we are accessing the MySQL Server

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

   REC_Key_Values = record
      Unique           : string;
      ExpDate          : string;
      DBPrefix         : string;
      LPMS_Collections : boolean;
      LPMS_DocGen      : boolean;
      LPMS_Floating    : boolean;
      LPMS_Options4    : boolean;
      License          : integer;
   end;

var
   FLPMS_Main: TFLPMS_Main;

   This_Key_Values : REC_Key_Values;
   This_Key_Priv   : REC_Key_Priv;

//--- Utilities contained in BSD_Utilities.dll

{$IFDEF DARWIN}
   function DoDecode(var Decode_Key_Priv: REC_Key_Priv): integer; stdcall; external 'libbsd_utilities.dylib';
   function DoEncode(var Encode_Key_Values: REC_Key_Values): boolean; stdcall; external 'libbsd_utilities.dylib';
   function SendMimeMail(From, ToStr, CcStr, BccStr, Subject, Body, Attach, SMTPStr : string): boolean; stdcall; external 'libbsd_utilities.dylib';
   function DoBackup(BackupType, BackupLocation, DBName, HostName, UserID, Password, Port, BackupName, Template, ThisVersion: string; BackupBlog: integer; ShowLog: TListView; ShowStatus: TStaticText; DoCompress: boolean) : boolean; stdcall; external 'libbsd_utilities.dylib';
   function DoRestore(BackupLocation,DBName,HostName,UserID,Password,Port,ThisVersion: string; ShowLog: TListView; ShowStatus: TStaticText; ThisType: integer) : string; stdcall; external 'libbsd_utilities.dylib';
{$ENDIF}
{$IFDEF LINUX}
   function DoDecode(var Decode_Key_Priv: REC_Key_Priv): integer; stdcall; external 'libbsd_utilities.so';
   function DoEncode(var Encode_Key_Values: REC_Key_Values): boolean; stdcall; external 'libbsd_utilities.so';
   function SendMimeMail(From, ToStr, CcStr, BccStr, Subject, Body, Attach, SMTPStr : string): boolean; stdcall; external 'libbsd_utilities.so';
   function DoBackup(BackupType, BackupLocation, DBName, HostName, UserID, Password, Port, BackupName, Template, ThisVersion: string; BackupBlog: integer; ShowLog: TListView; ShowStatus: TStaticText; DoCompress: boolean) : boolean; stdcall; external 'libbsd_utilities.so';
   function DoRestore(BackupLocation,DBName,HostName,UserID,Password,Port,ThisVersion: string; ShowLog: TListView; ShowStatus: TStaticText; ThisType: integer) : string; stdcall; external 'libbsd_utilities.so';
{$ENDIF}
{$IFDEF WINDOWS}
   function DoDecode(var Decode_Key_Priv: REC_Key_Priv): integer; stdcall; external 'BSD_Utilities.dll';
   function DoEncode(var Encode_Key_Values: REC_Key_Values): boolean; stdcall; external 'BSD_Utilities.dll';
   function SendMimeMail(From, ToStr, CcStr, BccStr, Subject, Body, Attach, SMTPStr : string): boolean; stdcall; external 'BSD_Utilities.dll';
   function DoBackup(BackupType, BackupLocation, DBName, HostName, UserID, Password, Port, BackupName, Template, ThisVersion: string; BackupBlog: integer; ShowLog: TListView; ShowStatus: TStaticText; DoCompress: boolean) : boolean; stdcall; external 'BSD_Utilities.dll';
   function DoRestore(BackupLocation,DBName,HostName,UserID,Password,Port,ThisVersion: string; ShowLog: TListView; ShowStatus: TStaticText; ThisType: integer) : string; stdcall; external 'BSD_Utilities.dll';
{$ENDIF}

implementation

   uses LPMS_Login, LPMS_InputQuery, LPMS_Show, LPMS_Excel;

{$R *.lfm}

{ TFLPMS_Main }

//------------------------------------------------------------------------------
// Executed when the Form is created
//------------------------------------------------------------------------------
procedure TFLPMS_Main.FormCreate(Sender: TObject);
begin

   DoSave     := False;
   CanUpdate  := False;
   DoGen      := False;
   FirstRun   := True;

   ClipKey := '';

   DefaultFormatSettings.ShortDateFormat := 'yyyy/MM/dd';
   DefaultFormatSettings.DateSeparator   := '/';

{$IFDEF WINDOWS}                    // Target is Winblows
   sqlCon  := TMySQL56Connection.Create(nil);
   sqlTran := TSQLTransaction.Create(nil);
   sqlQry1 := TSQLQuery.Create(nil);
{$ENDIF}

{$IFDEF LINUX}                      // Target is Linux
   {$IFDEF CPUARMHF}                // Running on ARM (Raspbian) architecture
      sqlCon  := TMySQL55Connection.Create(nil);
   {$ELSE}                          // Running on Intel architecture
      sqlCon  := TMySQL57Connection.Create(nil);
   {$ENDIF}
   sqlTran := TSQLTransaction.Create(nil);
   sqlQry1 := TSQLQuery.Create(nil);
{$ENDIF}

{$IFDEF DARWIN}                     // Target is macOS
   {$IFDEF CPUI386}                 // Running on older hardware
      sqlCon := TMySQL55Connection.Create(nil);
   {$ELSE}                          // Running on new hardware
      sqlCon := TMySQL57Connection.Create(nil);
   {$ENDIF}
   sqlTran := TSQLTransaction.Create(nil);
   sqlQry1 := TSQLQuery.Create(nil);
{$ENDIF}

   sqlTran.DataBase    := sqlCon;
   adoQry1.Transaction := sqlTran;
   adoQry2.Transaction := sqlTran;

   PassPhrase := 'BlueCrane Software Development CC';


//--- Adjust a few key display artifacts for optimum presentation on the
//--- execution platform

   SetPlatform();

end;

//------------------------------------------------------------------------------
// Executed when the Form is shown
//------------------------------------------------------------------------------
procedure TFLPMS_Main.FormShow(Sender: TObject);
var
   idx1, idx2 : integer;
   ThisNode   : TTreeNode;

begin

   if FirstRun = False then
      Exit
   else
      FirstRun := False;

   btnPaste.Enabled := False;

   FLPMS_Main.Caption := 'LPMS Access Control Manager';

//--- Display the information in the StatusBar

   StatusBar1.Panels.Items[0].Text := ' ' + CopyRight;
   StatusBar1.Panels.Items[1].Text := ' ' + Version;
   StatusBar1.Panels.Items[2].Text := ' Browse';
   StatusBar1.Panels.Items[3].Text := ' ' + UserName;

//--- Display the Root Panel by default

   pnlRoot.Visible    := True;
   pnlCompany.Visible := False;
   pnlUser.Visible    := False;
   pnlRestore.Visible := False;
   pnlBackup.Visible  := False;

//--- Populate the TreeView with all available information

   tvTree.Items.Clear();
   Root := tvTree.Items.AddFirst(nil,'LPMS_ACM (' + HostName + ':' + ACMPort + ')');


   OpenDB(ord(DB_OPEN));
   GetData(ord(DB_ALL),'','','');

   for idx1 := 0 to adoQry1.RecordCount - 1 do begin

      ThisNode := tvTree.Items.AddChild(Root,adoQry1.FieldByName('LPMSKey_Prefix').AsString);
      GetData(ord(DB_USER),adoQry1.FieldByName('LPMSKey_Prefix').AsString,'%','');

      for idx2 := 0 to adoQry2.RecordCount - 1 do begin

         tvTree.Items.AddChild(ThisNode,adoQry2.FieldByName('LPMSKey_Name').AsString);
         adoQry2.Next();

      end;

      adoQry1.Next();

   end;

   OpenDB(ord(DB_CLOSE));

//--- Select the Root node by default then fully expand the entire tree

   tvTree.Selected := Root;
   tvTree.Selected.Expand(True);

   btnUnlock.Visible := False;
   btnLock.Visible   := True;
   btnNew.Enabled    := True;
   btnCancel.Enabled := False;
   btnDelete.Enabled := False;
   btnUpdate.Enabled := False;

   btnDecodeR.Enabled := False;
   btnEncodeR.Enabled := False;
   btnFindR.Enabled   := False;

   ToolsGenerate.Enabled := False;
   ToolsEmail.Enabled    := False;

   dtpExpiryR.Date   := Now();
   edtServerR.Text   := ThisSMTPHost;
   edtPasswordR.Text := ThisSMTPPass;
   edtKeyR.SetFocus();

   if Trim(AutoKey) <> '' then begin

      edtKeyR.Text := AutoKey;
      btnDecodeRClick(Sender);

   end;

end;

//------------------------------------------------------------------------------
// User clicked on the Exit button
//------------------------------------------------------------------------------
procedure TFLPMS_Main.FileExitExecute(Sender: TObject);
begin
   Close();
end;

//------------------------------------------------------------------------------
// Executed when the Form is fianally closed
//------------------------------------------------------------------------------
procedure TFLPMS_Main.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin

   if DoSave = True then begin

      if Application.MessageBox('There are unsaved changes. You can:' + #10 + #10 + #10 + 'Click [OK] to proceed and ignore the changes; or' + #10 + #10 + 'Click [Cancel] to return and attend to the changes.','LPMS Access Control Management',(MB_OKCANCEL + MB_ICONSTOP)) = ID_CANCEL then begin

         CloseAction := Forms.caNone;
         Exit;

      end;

   end;

   OpenDB(ord(DB_CLOSE));

   FLPMS_Login.ThisSMTPHost := edtServerR.Text;
   FLPMS_Login.ThisSMTPPass := edtPasswordR.Text;

end;

//------------------------------------------------------------------------------
// User clicked on the Refresh/Reload button
//------------------------------------------------------------------------------
procedure TFLPMS_Main.ActionsRefreshExecute(Sender: TObject);
var
   idx1, idx2     : integer;
   ThisRoot, ThisNode : TTreeNode;

begin

   if ((pnlBackup.Visible = True) or (pnlRestore.Visible = True)) then
      Exit;

   if DoSave = True then begin

      if Application.MessageBox('There are unsaved changes. You can:' + #10 + #10 + #10 + 'Click [OK] to proceed and ignore the changes; or' + #10 + #10 + 'Click [Cancel] to return and attend to the changes.','LPMS Access Control Management',(MB_OKCANCEL + MB_ICONSTOP)) = ID_CANCEL then
         Exit;

   end;

   DoSave    := False;
   CanUpdate := False;
   DoGen     := false;

//--- Display the information in the StatusBar

   StatusBar1.Panels.Items[0].Text := ' ' + CopyRight;
   StatusBar1.Panels.Items[1].Text := ' ' + Version;
   StatusBar1.Panels.Items[2].Text := ' Browse';
   StatusBar1.Panels.Items[3].Text := ' ' + UserName;

//--- Display the Root Panel by default

   pnlRoot.Visible    := True;
   pnlCompany.Visible := False;
   pnlUser.Visible    := False;

//--- Populate the TreeView with all available information

   tvTree.Items.Clear();
//   ThisRoot := tvTree.Items.AddFirst(nil,'LPMS');
   ThisRoot := tvTree.Items.AddFirst(nil,'LPMS_ACM (' + HostName + ':' + ACMPort + ')');

   OpenDB(ord(DB_OPEN));
   GetData(ord(DB_ALL),'','','');

   for idx1 := 0 to adoQry1.RecordCount - 1 do begin

      ThisNode := tvTree.Items.AddChild(ThisRoot,adoQry1.FieldByName('LPMSKey_Prefix').AsString);
      GetData(ord(DB_USER),adoQry1.FieldByName('LPMSKey_Prefix').AsString,'%','');

      for idx2 := 0 to adoQry2.RecordCount - 1 do begin

         tvTree.Items.AddChild(ThisNode,adoQry2.FieldByName('LPMSKey_Name').AsString);
         adoQry2.Next();

      end;

      adoQry1.Next();

   end;

//--- Select the Root node by default then fully expand the entire tree

   OpenDB(ord(DB_CLOSE));
   tvTree.Selected := ThisRoot;
   tvTree.FullExpand;

   btnUnlock.Visible := False;
   btnLock.Visible   := True;
   btnNew.Enabled    := True;
   btnCancel.Enabled := False;
   btnDelete.Enabled := False;
   btnUpdate.Enabled := False;

   btnDecodeR.Enabled := False;
   btnEncodeR.Enabled := False;
   btnFindR.Enabled   := False;

   ToolsGenerate.Enabled := False;
   ToolsEmail.Enabled    := False;

   edtKeyR.SetFocus();

end;

//------------------------------------------------------------------------------
// Check whether the User pressed the backspace key while editing the Key on
// the Root display in order to preserve the positionof the '-' characters
//------------------------------------------------------------------------------
procedure TFLPMS_Main.edtKeyRKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
   KeepSelStart, KeepLength : integer;
   ThisField                : string;
begin

//--- Trap and Process the Backspace key

   if Key = VK_BACK then begin

//--- If we are at the start of the field then we consume the key and do nothing

      if (edtKeyR.SelStart = 0) and (edtKeyR.SelLength = 0) then begin

         Key := 0;
         Exit;

      end;

//--- If the whole field is selected then simply delete the contents

      if edtKeyR.SelLength = Length(edtKeyR.Text) then begin

         edtKeyR.Text := '';
         KeepSelStart := 0;

      end else begin

//--- Otherwise delete what is selected

         ThisField    := '';
         KeepLength   := edtKeyR.SelLength;
         KeepSelStart := edtKeyR.SelStart;

         if KeepSelStart = 1 then

            ThisField := Copy(edtKeyR.Text,2,Length(edtKeyR.Text) - 1)

         else begin

            if KeepLength = 0 then
               ThisField := Copy(edtKeyR.Text,1,KeepSelStart - 1)
            else
               ThisField := Copy(edtKeyR.Text,1,KeepSelStart);

            ThisField := Thisfield + Copy(edtKeyR.Text,KeepSelStart + KeepLength + 1,Length(edtKeyR.Text) - 1);

         end;

//--- Update the field without invoking the edtKeyRChange routine

         CanUpdate := True;
         edtKeyR.Text := ThisField;
         CanUpdate := False;

      end;

      if KeepLength = 0 then
         edtKeyR.SelStart := KeepSelStart - 1
      else
         edtKeyR.SelStart := KeepSelStart;

      Key := 0;

   end;

end;

//------------------------------------------------------------------------------
// User clicked on the Copy to Clipboard button on the User screen
//------------------------------------------------------------------------------
procedure TFLPMS_Main. btnCopyClick( Sender: TObject);
begin

   ClipKey := edtKeyU.Text;

end;

//------------------------------------------------------------------------------
// User clicked on the Paste from Clipboard button on the Root screen
//------------------------------------------------------------------------------
procedure TFLPMS_Main. btnPasteClick( Sender: TObject);
//------------------------------------------------------------------------------
// User clicked on the Paste from Clipboard key on the Root screen
//------------------------------------------------------------------------------
begin

   edtKeyR.Text := ClipKey;

end;

//------------------------------------------------------------------------------
// User clicked on the TreeView
//------------------------------------------------------------------------------
procedure TFLPMS_Main.tvTreeClick(Sender: TObject);
var
   DaysLeft : integer;

begin

//--- Ignore if we are doing a Backup or a Restore

   if ((pnlBackup.Visible = True) or (pnlRestore.Visible = True)) then
      Exit;

//--- Warn the user if there are unsaved changes

   if DoSave = True then begin

      if (Application.MessageBox('WARNING: There are unsaved changes. You can:' + #10 + #10 + #10 + 'Click [Ok] to proceed and ignore the changes; or' + #10 + #10 + 'Click [Cancel] to return and attend to the changes.','LPMS Access Control Management',(MB_OKCANCEL + MB_ICONSTOP)) = ID_CANCEL) then
         exit;

      DoSave := False;

   end;

//--- Activate the correct display for the level selected

   CanUpdate := True;
   OpenDB(ord(DB_OPEN));

   if tvTree.Selected.Level = 0 then begin

      pnlRoot.Visible    := True;
      pnlCompany.Visible := False;
      pnlUser.Visible    := False;

      edtKeyR.Clear();
      edtPrefixR.Clear();
      edtUniqueR.Clear();
      dtpExpiryR.Date       := Now();
      cbCollectR.Checked    := False;
      cbDocGenR.Checked     := False;
      cbFloatingR.Checked   := False;
      cbOption4R.Checked    := False;
      edtUniqueF.Clear();
      cbxLicTypeR.ItemIndex := 0;

      btnNew.Enabled    := True;
      btnCancel.Enabled := False;
      btnDelete.Enabled := False;
      btnUpdate.Enabled := False;

      btnDecodeR.Enabled := False;
      btnEncodeR.Enabled := False;
      btnFindR.Enabled   := False;

      ToolsGenerate.Enabled := False;
      ToolsEmail.Enabled    := False;

      if Trim(ClipKey) = '' then
         btnPaste.Enabled := False
      else
         btnPaste.Enabled := True;

      edtKeyR.SetFocus();

   end else if tvTree.Selected.Level = 1 then begin

      pnlRoot.Visible    := False;
      pnlCompany.Visible := True;
      pnlUser.Visible    := False;

      GetData(ord(DB_COMPANY),tvTree.Selected.Text,'%','');

      edtPrefixC.Text       := ReplaceQuote(adoQry1.FieldByName('LPMSKey_Prefix').AsString,ord(TYPE_READ));
      cbxKeyTypeC.ItemIndex := cbxKeyTypeC.Items.IndexOf(adoQry1.FieldByName('LPMSKey_Name').AsString);
      edtCompanyC.Text      := ReplaceQuote(adoQry1.FieldByName('LPMSKey_Desc').AsString,ord(TYPE_READ));
      speLicCountC.Value    := adoQry1.FieldByName('LPMSKey_LicCount').AsInteger;
      speRenewalC.Value     := adoQry1.FieldByName('LPMSKey_Interval').AsInteger;

      if adoQry1.FieldByName('LPMSKey_Blocked').AsInteger = 0 then
         cbBlockedC.Checked := False
      else
         cbBlockedC.Checked := True;

      btnNew.Enabled    := True;
      btnCancel.Enabled := False;
      btnDelete.Enabled := True;
      btnUpdate.Enabled := False;

      ToolsGenerate.Enabled := False;
      ToolsEmail.Enabled    := False;

      cbxKeyTypeC.SetFocus();

   end else if tvTree.Selected.Level = 2 then begin

      pnlRoot.Visible    := False;
      pnlCompany.Visible := False;
      pnlUser.Visible    := True;

      GetData(ord(DB_USER),tvTree.Selected.Parent.Text,tvTree.Selected.Text,'');

      edtDBKeyU.Text      := adoQry2.FieldByName('LPMSKey_Key').AsString;
      edtUserNameU.Text   := ReplaceQuote(adoQry2.FieldByName('LPMSKey_Name').AsString,ord(TYPE_READ));
      edtCompanyU.Text    := ReplaceQuote(adoQry2.FieldByName('LPMSKey_Company').AsString,ord(TYPE_READ));
      edtEmailU.Text      := ReplaceQuote(adoQry2.FieldByName('LPMSKey_Email').AsString,ord(TYPE_READ));
      edtContactU.Text    := ReplaceQuote(adoQry2.FieldByName('LPMSKey_Contact').AsString,ord(TYPE_READ));
      edtRenewalsU.Text   := adoQry2.FieldByName('LPMSKey_Renewals').AsString;

      if adoQry2.FieldByName('LPMSKey_Blocked').AsInteger = 0 then
         cbBlockedU.Checked := False
      else
         cbBlockedU.Checked := True;

      edtKeyU.Text := adoQry2.FieldByName('LPMSKey_Activation').AsString;

      if adoQry2.FieldByName('LPMSKey_Transfer').AsInteger = 0 then
         cbTransferU.Checked := False
      else
         cbTransferU.Checked := True;

      cbTransferUClick(Sender);

//--- Extract and set the Transfer details

      if cbTransferU.Checked = True then begin

         cbxNewPrefixU.ItemIndex := cbxNewPrefixU.Items.IndexOf(ReplaceQuote(adoQry2.FieldByName('LPMSKey_NewPrefix').AsString,ord(TYPE_READ)));

         if cbxNewPrefixU.ItemIndex = -1 then
            cbxNewPrefixU.ItemIndex := 0;

         if adoQry2.FieldByName('LPMSKey_NewLicense').AsInteger > cbxNewLicU.Items.Count - 1 then
            cbxNewLicU.ItemIndex := 0
         else
            cbxNewLicU.ItemIndex := adoQry2.FieldByName('LPMSKey_NewLicense').AsInteger;

      end;

//--- Decode the Key and populate the fields contained in the key

      This_Key_Priv.Key := edtKeyU.Text;
      DaysLeft := DoDeCode(This_Key_Priv);

      if DaysLeft < 0 then begin

         edtUniqueU.Text       := adoQry2.FieldByName('LPMSKey_Unique').AsString;
         cbxLicTypeU.ItemIndex := adoQry2.FieldByName('LPMSKey_LicType').AsInteger;

         if Trim(This_Key_Priv.KeyDate) <> '' then
            dtpExpiryDateU.Date := StrToDate(This_Key_Priv.KeyDate)
         else
            dtpExpiryDateU.Date := Now();

         if DaysLeft = ord(ERR_EXPIRED) - 3 then begin

            txtExpired.Caption := ' ** Key has Expired!';
            txtInvalid.Caption := '';

         end else begin

            txtExpired.Caption := '';
            txtInvalid.Caption := ' ** Key is Invalid';

         end;

         edtPrefixU.Text       := ReplaceQuote(adoQry2.FieldByName('LPMSKey_Prefix').AsString,ord(TYPE_READ));

      end else begin

         edtUniqueU.Text       := This_Key_Priv.Unique;
         cbxLicTypeU.ItemIndex := This_Key_Priv.License;
         dtpExpiryDateU.Date   := StrToDate(This_Key_Priv.KeyDate);
         txtExpired.Caption    := '';
         txtInvalid.Caption    := '';
         edtPrefixU.Text       := This_Key_Priv.DBPrefix;

      end;

{
      if adoQry2.FieldByName('LPMSKEy_AllowDuplicates').AsString = '1' then
         cbAllowDuplicates.Checked := True
      else
         cbAllowDuplicates.Checked := False;
}

      cbAllowDuplicates.Checked := StrToBool(adoQry2.FieldByName('LPMSKEy_AllowDuplicates').AsString);

      cbCollectU.Checked  := This_Key_Priv.LPMS_Collections;
      cbDocGenU.Checked   := This_Key_Priv.LPMS_DocGen;
      cbFloatingU.Checked := This_Key_Priv.LPMS_Floating;
      cbOption4U.Checked  := This_Key_Priv.LPMS_Options4;

      btnNew.Enabled    := False;
      btnCancel.Enabled := False;
      btnDelete.Enabled := True;
      btnUpdate.Enabled := False;

      ToolsGenerate.Enabled := True;

      if ((Trim(edtServerR.Text) = '') or (Trim(edtPasswordR.Text) = '') or (DaysLeft < 1)) then
         ToolsEmail.Enabled := False
      else
         ToolsEmail.Enabled := True;

      edtUserNameU.SetFocus();

   end;

   OpenDB(ord(DB_CLOSE));
   CanUpdate := False;

end;

//------------------------------------------------------------------------------
// User clicked on the New button
//------------------------------------------------------------------------------
procedure TFLPMS_Main.btnNewClick(Sender: TObject);
var
   ThisNode : TTreeNode;

begin

   if DoSave = True then begin

      if Application.MessageBox('There are unsaved changes. You can:' +#10 + #10 + #10 + 'Click [Ok] to proceed and ignore the changes; or' +#10 + #10 + 'Click [Cancel] to return and attend to the changes.','LPMS Access Control Management',(MB_OKCANCEL + MB_ICONSTOP)) = ID_CANCEL then
         Exit;
   end;

   if pnlRoot.Visible = True then begin

      ThisNode := tvTree.Selected;
      ThisNode := tvTree.Items.AddChild(ThisNode,'New Company');
      tvTree.Selected := ThisNode;

      StatusBar1.Panels.Items[2].Text := ' New';

      ToolsEmail.Enabled     := False;
      ToolsExport.Enabled    := False;
      ToolsGenerate.Enabled  := False;
      ActionsRefresh.Enabled := False;
      ActionsBackup.Enabled  := False;
      ActionsRestore.Enabled := False;

      btnNew.Enabled    := False;
      btnDelete.Enabled := False;
      btnUpdate.Enabled := False;
      btnCancel.Enabled := True;

      pnlCompany.Visible  := True;
      pnlUser.Visible     := False;
      edtPrefixC.ReadOnly := False;

      CanUpdate := True;

      edtPrefixC.Text       := 'XXX000';
      cbxKeyTypeC.ItemIndex := 0;
      edtCompanyC.Clear();
      speLicCountC.Value    := 0;
      speRenewalC.Value     := 30;
      cbBlockedC.Checked    := False;

      CanUpdate := False;

      edtPrefixC.SetFocus();

   end else if pnlCompany.Visible = True then begin

      ThisNode := tvTree.Selected;

//--- First check to see whether the maximum number of users has been reached

      if speLicCountC.Value <= ThisNode.Count then begin

         Application.MessageBox('Maximum number of Licenses reached - Unable to add a New User.','LPMS Access Control Management',(MB_OK + MB_ICONSTOP));
         Exit;

      end;

//--- If not then we can go ahead and add the new user

      ThisNode := tvTree.Items.AddChild(ThisNode,'New User');
      tvTree.Selected := ThisNode;

      StatusBar1.Panels.Items[2].Text := ' New';

      ToolsEmail.Enabled     := False;
      ToolsExport.Enabled    := False;
      ToolsGenerate.Enabled  := True;
      ActionsRefresh.Enabled := False;
      ActionsBackup.Enabled  := False;
      ActionsRestore.Enabled := False;

      btnNew.Enabled    := False;
      btnDelete.Enabled := False;
      btnUpdate.Enabled := False;
      btnCancel.Enabled := True;

      pnlCompany.Visible := False;
      pnlUser.Visible    := True;

      CanUpdate := True;

      edtPrefixU.Text           := edtPrefixC.Text;
      edtUserNameU.Text         := 'New User';
      edtCompanyU.Text          := edtCompanyC.Text;
      edtEmailU.Clear();
      edtContactU.Clear();
      cbxLicTypeU.ItemIndex     := 2;
      edtUniqueU.Text           := '000000000000';
      cbAllowDuplicates.Checked := False;
      dtpExpiryDateU.Date       := (Now() + speRenewalC.Value);
      txtExpired.Caption        := '';
      edtRenewalsU.Text         := '0';
      cbBlockedU.Checked        := False;
      cbCollectU.Checked        := True;
      cbDocGenU.Checked         := True;
      cbFloatingU.Checked       := False;
      cbOption4U.Checked        := False;
      edtKeyU.Clear();
      cbTransferU.Checked       := False;
      cbxNewLicU.ItemIndex      := 0;
      cbxNewLicU.Enabled        := False;
      cbxNewPrefixU.ItemIndex   := 0;
      cbxNewPrefixU.Enabled     := False;

      CanUpdate := False;

      edtUserNameU.SetFocus();

   end;

end;

//-----------------------------------------------------------------------------
// User clicked on the Cancel button
//------------------------------------------------------------------------------
procedure TFLPMS_Main.btnCancelClick(Sender: TObject);
var
   SaveNode : TTreeNode;

begin

   if pnlCompany.Visible = True then begin

      if StatusBar1.Panels.Items[2].Text = ' New' then begin

         SaveNode := tvTree.Selected.Parent;
         tvTree.Selected.Delete();
         tvTree.Selected := SaveNode;

      end;

      btnDelete.Enabled := True;
      btnUpdate.Enabled := False;
      btnCancel.Enabled := False;
      cbxKeyTypeC.SetFocus();

      ToolsEmail.Enabled     := True;
      ToolsExport.Enabled    := True;
      ActionsRefresh.Enabled := True;
      ActionsBackup.Enabled  := True;
      ActionsRestore.Enabled := True;

      edtPrefixC.ReadOnly := True;

   end else if pnlUser.Visible = True then begin

      if StatusBar1.Panels.Items[2].Text = ' New' then begin

         SaveNode := tvTree.Selected.Parent;
         tvTree.Selected.Delete();
         tvTree.Selected := SaveNode;

      end;

      btnDelete.Enabled := True;
      btnUpdate.Enabled := False;
      btnCancel.Enabled := False;
      edtUserNameU.SetFocus();

      ToolsEmail.Enabled     := True;
      ToolsExport.Enabled    := True;
      ActionsRefresh.Enabled := True;
      ActionsBackup.Enabled  := True;
      ActionsRestore.Enabled := True;

   end else if pnlBackup.Visible = True then begin

      btnCancel.Caption := 'Cancel';
      btnUpdate.Caption := 'Update';

      pnlBackup.Visible := False;

      case PlaceHolder of

         1: pnlRoot.Visible    := True;
         2: pnlCompany.Visible := True;
         3: pnlUser.Visible    := True;

      end;

   end else if pnlRestore.Visible = True then begin

      btnCancel.Caption := 'Cancel';
      btnUpdate.Caption := 'Update';

      pnlRestore.Visible := False;

      case PlaceHolder of

         1: pnlRoot.Visible    := True;
         2: pnlCompany.Visible := True;
         3: pnlUser.Visible    := True;

      end;

      if RestoreSuccess = True then
         ActionsRefreshExecute(Sender);

   end;

   tvTree.Enabled := True;

   StatusBar1.Panels.Items[2].Text := ' Browse';
   DoSave := False;
   tvTreeClick(Sender);

end;

//------------------------------------------------------------------------------
// User clicked on the Delete button
//------------------------------------------------------------------------------
procedure TFLPMS_Main.btnDeleteClick(Sender: TObject);
var
   NumUsers : integer;
   SaveNode : TTreeNode;

begin

   if DoSave = True then begin

      if Application.MessageBox('There are unsaved changes. You can:' + #10 + #10 + #10 + 'Click [Ok] to proceed and ignore the changes; or' + #10 + #10 + 'Click [Cancel] to return and attend to the changes.','LPMS Access Control Management',(MB_OKCANCEL + MB_ICONSTOP)) = ID_CANCEL then
         Exit;

   end;

   tvTree.Enabled  := False;
   btnExit.Enabled := False;
   ToolBar1.Enabled := False;


   if pnlCompany.Visible = True then begin

      btnNew.Enabled := False;
      OpenDB(ord(DB_OPEN));

      if GetData(ord(DB_USER),tvTree.Selected.Text,'%','') = False then begin

         OpenDB(ord(DB_CLOSE));

         tvTree.Enabled   := True;
         btnNew.Enabled   := True;
         btnExit.Enabled  := True;
         ToolBar1.Enabled := True;

         Exit;

      end;

      NumUsers := adoQry2.RecordCount;

      if Application.MessageBox(PChar('Delete ''' + tvTree.Selected.Text + ''' and ' + IntToStr(NumUsers) + ' associated Users?' + #10 + #10 + 'The Company and its associated Users will be permanently deleted and this action cannot be undone. You can:' + #10 + #10 + #10 + 'Click [Yes] to proceed with the delete action; or' + #10 + #10 + 'Click [No] to cancel the delete request.'),'LPMS Access Control Management',(MB_YESNO + MB_ICONSTOP)) = ID_NO then begin

         OpenDB(ord(DB_CLOSE));

         tvTree.Enabled   := True;
         btnNew.Enabled   := True;
         btnExit.Enabled  := True;
         ToolBar1.Enabled := True;

         Exit;

      end;

      if DelData(ord(DB_COMPANY),tvTree.Selected.Text,'%') = True then begin

         Application.MessageBox(PChar('Company ''' + tvTree.Selected.Text + ''' and ' + IntToStr(NumUsers) + ' associated Users deleted.'),'LPMS Access Control Management',(MB_OK + MB_ICONINFORMATION));

         SaveNode := tvTree.Selected.Parent;
         tvTree.Selected.Delete();
         tvTree.Selected := SaveNode;

      end;

      OpenDB(ord(DB_CLOSE));

   end else if (pnlUser.Visible = True) then begin

      if Application.MessageBox(PChar('Delete ''' + tvTree.Selected.Text + '''?' + #10 + #10 + 'The User will be permanently deleted and this action cannot be undone. You can:' + #10 + #10 + 'Click [Yes] to proceed with the delete action; or' + #10 + #10 + 'Click [No] to cancel the delete request.'),'LPMS Access Control Management',(MB_YESNO + MB_ICONSTOP)) = ID_NO then begin

         tvTree.Enabled   := True;
         btnExit.Enabled  := True;
         ToolBar1.Enabled := True;
         edtUserNameU.SetFocus();

         Exit;

      end;

      OpenDB(ord(DB_OPEN));

      if DelData(ord(DB_USER),tvTree.Selected.Parent.Text,tvTree.Selected.Text) = True then begin

         Application.MessageBox(PChar('User ''' + tvTree.Selected.Text + ''' deleted.'),'LPMS Access Control Management',(MB_OK + MB_ICONINFORMATION));

         SaveNode := tvTree.Selected.Parent;
         tvTree.Selected.Delete();
         tvTree.Selected := SaveNode;

      end;


      OpenDB(ord(DB_CLOSE));

   end;

   tvTree.Enabled   := True;
   btnExit.Enabled  := True;
   ToolBar1.Enabled := True;
   tvTreeClick(Sender);

end;

//------------------------------------------------------------------------------
// User clicked on the Update button
//------------------------------------------------------------------------------
procedure TFLPMS_Main.btnUpdateClick(Sender: TObject);
var
   Dummy   : integer = 0;
   MsgPart : string;

begin

   if pnlCompany.Visible = True then begin

//--- Make sure that the Company is valid and does not exist

      if edtPrefixC.Text = 'XXX000' then begin

         Application.MessageBox(PChar('Prefix ''' + edtPrefixC.Text + ''' is reserved and cannot be used.'),'LPMS Access Control Management',(MB_OK + MB_ICONSTOP));
         edtPrefixC.SetFocus();
         Exit;

      end;

      if StatusBar1.Panels.Items[2].Text = ' New' then begin

         if CpyExists(edtPrefixC.Text) = True then begin

            Application.MessageBox('''Prefix'' already exists.','LPMS Access Control Management',(MB_OK + MB_ICONSTOP));
            edtPrefixC.SetFocus();
            Exit;

         end;

      end;

//--- Make sure that the required information is provided

      if Trim(edtPrefixC.Text) = '' then begin

         Application.MessageBox('''Prefix'' is a required field and must be provided.','LPMS Access Control Management',(MB_OK + MB_ICONSTOP));
         edtPrefixC.SetFocus();
         Exit;

      end;

      if cbxKeyTypeC.ItemIndex < 1 then begin

         Application.MessageBox(PChar('Key Type ''' + cbxKeyTypeC.Text + ''' is not a valid selection.'),'LPMS Access Control Management',(MB_OK + MB_ICONSTOP));
         cbxKeyTypeC.SetFocus();
         Exit;

      end;

      if Trim(edtCompanyC.Text) = '' then begin

         Application.MessageBox('''Company'' is a require field and must be provided.','LPMS Access Control Management',(MB_OK + MB_ICONSTOP));
         edtCompanyC.SetFocus();
         Exit;

      end;


//--- If we get here then all information is there - do the Update

      if UpdateCpy() = True then
         Application.MessageBox('Update succesfully completed.','LPMS Access Control Management',(MB_OK + MB_ICONINFORMATION));

      tvTree.Selected.Text := edtPrefixC.Text;

   end else if pnlUser.Visible = True then begin

//--- Check whether a field that is contained in the Key changed

      if DoGen = True then begin

         if Application.MessageBox('A field that is contained in the Activation Key (''Unique'', ''License Type'', ''Expiry Date'' or ''Options'') changed.' + #10 + #10 + 'This will cause the values contained in the current key to be displayed and not the updated value(s). You can:' + #10 + #10 + #10 + 'Click [Ok] to proceed with the Update; or' + #10 + #10 + 'Click [Cancel] to return.' + #10 + #10 + 'HINT: Click [Cancel] and then on the ''Generate'' icon to generate a new key containing the new values.','LPMS Access Control Management',(MB_OKCANCEL + MB_ICONSTOP)) = ID_CANCEL then begin

            edtUserNameU.SetFocus();
            Exit;

         end;

      end;

//--- Make sure that the User or the Unique Identifier does not exist

      if StatusBar1.Panels.Items[2].Text = ' New' then begin

          if UserExists(edtUserNameU.Text, edtPrefixU.Text) = True then begin

            Application.MessageBox('''Username'' already exists for this Company.','LPMS Access Control Management',(MB_OK + MB_ICONSTOP));
            edtUserNameU.SetFocus();
            Exit;

         end;

      end;

//--- A Unique = '123456789ABC' is not allowed for Trial licenses

      if ((edtUniqueU.Text = '123456789ABC') and (cbxLicTypeU.Text = 'Trial')) then begin

         Application.MessageBox(PChar('''Unique'' with value ''123456789ABC'' is not allowed for license type ''' + cbxLicTypeU.Text + '''.'),'LPMS Access Control Management',(MB_OK + MB_ICONSTOP));

            if btnUnlock.Visible = True then
               edtUniqueU.SetFocus()
            else
               btnLock.SetFocus();

         Exit;

      end;

//--- If it is not a Trail license and the Unique = '123456789ABC' then we have
//--- a floating license.

      if edtUniqueU.Text <> '123456789ABC' then begin

         MsgPart := GetUnique(edtUniqueU.Text,ord(TYPE_TEST),Dummy);

         if ((MsgPart <> '') and (cbAllowDuplicates.Checked = False)) then begin

            Application.MessageBox(PChar('''Unique'' already exists for ' + MsgPart),'LPMS Access Control Management',(MB_OK + MB_ICONSTOP));

            if btnUnlock.Visible = True then
               edtUniqueU.SetFocus()
            else
               btnLock.SetFocus();

            Exit;

         end;

      end;

//--- Make sure that the required information is provided

      if Trim(edtUserNameU.Text) = '' then begin

         Application.MessageBox('''Username'' is a required field and must be provided.','LPMS Access Control Management',(MB_OK + MB_ICONSTOP));
         edtUserNameU.SetFocus();
         Exit;

      end;

      if Trim(edtCompanyU.Text) = '' then begin

         Application.MessageBox('''Company'' is a required field and must be provided.','LPMS Access Control Management',(MB_OK + MB_ICONSTOP));
         edtCompanyU.SetFocus();
         Exit;

      end;

      if Trim(edtEmailU.Text) = '' then begin

         Application.MessageBox('''Email'' is a required field and must be provided.','LPMS Access Control Management',(MB_OK + MB_ICONSTOP));
         edtEmailU.SetFocus();
         Exit;

      end;

      if Trim(edtContactU.Text) = '' then begin

         Application.MessageBox('''Contact No'' is a required field and must be provided.','LPMS Access Control Management',(MB_OK + MB_ICONSTOP));
         edtContactU.SetFocus();
         Exit;

      end;

      if Length(edtUniqueU.Text) <> 12 then begin

         Application.MessageBox(PChar('' + edtUniqueU.Text + ''' is not a valid value for ''Unique''.'),'LPMS Access Control Management',(MB_OK + MB_ICONSTOP));
         edtUniqueU.SetFocus();
         Exit;

      end;

      if cbxLicTypeU.ItemIndex < 1 then begin

         Application.MessageBox(PChar('License Type ''' + cbxLicTypeU.Text + ''' is not a valid selection.'),'LPMS Access Control Management',(MB_OK + MB_ICONSTOP));
         cbxLicTypeU.SetFocus();
         Exit;

      end;

      if Trim(edtKeyU.Text) = '' then begin

         Application.MessageBox('''Key'' is a required field and must be provided.' + #10 + #10 + #10 + 'HINT: Click on ''Generate'' to generate a Key.','LPMS Access Control Management',(MB_OK + MB_ICONSTOP));
         edtKeyU.SetFocus();
         Exit;

      end;

      if Length(edtKeyU.Text) <> 38 then begin

         Application.MessageBox(PChar('' + edtKeyU.Text + ''' is not a valid value for ''Key''.'),'LPMS Access Control Management',(MB_OK + MB_ICONSTOP));
         edtKeyU.SetFocus();
         Exit;

      end;

      if cbTransferU.Checked = True then begin

         if cbxNewLicU.ItemIndex = 0 then begin

            Application.MessageBox(PChar('New Lic Type ''' + cbxNewLicU.Text + ''' is not a valid selection.'),'LPMS Access Control Management',(MB_OK + MB_ICONSTOP));
            cbxNewLicU.SetFocus();
            Exit;

         end;

         if cbxNewPrefixU.ItemIndex = 0 then begin

            Application.MessageBox(PChar('New Prefix ''' + cbxNewPrefixU.Text + ''' is not a valid selection.'),'LPMS Access Control Management',(MB_OK + MB_ICONSTOP));
            cbxNewPrefixU.SetFocus();
            Exit;

         end;

      end;

//--- If we get here then all information is there - do the Update

      if UpdateUser() = True then
         Application.MessageBox('Update successfully completed.','LPMS Access Control Management',(MB_OK + MB_ICONINFORMATION));

      tvTree.Selected.Text := edtUserNameU.Text;

   end else if pnlBackup.Visible = True then begin

      RunBackup();

   end else if pnlRestore.Visible = True then begin

      RunRestore();

   end;

   if ((pnlBackup.Visible = False) and (pnlRestore.Visible = False)) then begin

      DoGen := False;
      StatusBar1.Panels.Items[2].Text := ' Updated';
      btnCancelClick(Sender);

   end;

end;

//------------------------------------------------------------------------------
// Procedure to perform a backup
//------------------------------------------------------------------------------
procedure TFLPMS_Main.RunBackup();
begin

   DoBackup('Ad-Hoc',edtLocationB.Text,'lpmsdefault',edtHostNameB.Text,edtUserIDB.Text,edtPasswordB.Text,edtPortB.Text,'LPMS_ACM',edtTemplateB.Text,FLPMS_Login.Version,speReadBlockB.Value,BackupLog,nil,True);

end;

//------------------------------------------------------------------------------
// Procedure to perform a Restore
//------------------------------------------------------------------------------
procedure TFLPMS_Main.RunRestore();
var
   ThisResult : string;

begin

   if Trim(edtUserIDRe.Text) = '' then begin

      Application.MessageBox('''UserID'' is a required field - please provide then try again.','LPMS Access Control Management',(MB_OK + MB_ICONSTOP));
      edtUserIDRe.SetFocus();
      Exit;

   end;

   if Trim(edtPasswordRe.Text) = '' then begin

      Application.MessageBox('''Password'' is a required field and must be provided.','LPMS Access Control Management',(MB_OK + MB_ICONSTOP));
      edtPasswordRe.SetFocus();
      Exit;

   end;

   if Trim(edtHostNameRe.Text) = '' then begin

      Application.MessageBox('''Host Name'' is a required field and must be provided.','LPMS Access Control Management',(MB_OK + MB_ICONSTOP));
      edtHostNameRe.SetFocus();
      Exit;

   end;

   RestoreLogRe2.Visible := False;
   RestoreLogRe1.Visible := True;

   if Application.MessageBox(PChar('WARNING: Restoring database ''lpmsdefault'' from ''' + RestoreFile + ''' will DESTROY all information in the database. This operation cannot be undone once started. You can:' + #10 + #10 + #10 + 'Click [Ok] to proceed with the Restore; or' + #10 + #10 + 'Click [Cancel to abandon the Restore and return.'),'LPMS Access Control Management',(MB_OKCANCEL + MB_ICONSTOP)) = ID_NO then begin
      Exit;
   end;

   RestoreSuccess := False;
   ThisResult := DoRestore(RestoreFile,'lpmsdefault',edtHostNameRe.Text,edtUserIDRe.Text,edtPasswordRe.Text,edtPortRe.Text,'',RestoreLogRe1,nil,ord(RT_RESTORE));

   if ThisResult <> '' then
      RestoreSuccess := True;

end;

//------------------------------------------------------------------------------
// A field on the Company screen changed
//------------------------------------------------------------------------------
procedure TFLPMS_Main.edtPrefixCChange(Sender: TObject);
begin

   if CanUpdate = True then
      Exit;

   btnDelete.Enabled := False;
   btnUpdate.Enabled := True;
   btnCancel.Enabled := True;

   tvTree.Enabled         := False;
   ToolsEmail.Enabled     := False;
   ToolsExport.Enabled    := False;
   ToolsGenerate.Enabled  := False;
   ActionsRefresh.Enabled := False;
   ActionsBackup.Enabled  := False;
   ActionsRestore.Enabled := False;

   DoSave := True;

   if StatusBar1.Panels.Items[2].Text = ' Browse' then
      StatusBar1.Panels.Items[2].Text := ' Modified';

end;

//------------------------------------------------------------------------------
// A field on the Users screen changed
//------------------------------------------------------------------------------
procedure TFLPMS_Main.edtUserNameUChange(Sender: TObject);
begin

   if CanUpdate = True then
      Exit;

   btnDelete.Enabled := False;
   btnUpdate.Enabled := True;
   btnCancel.Enabled := True;

   tvTree.Enabled         := False;
   ToolsEmail.Enabled     := False;
   ToolsExport.Enabled    := False;
   ActionsRefresh.Enabled := False;
   ActionsBackup.Enabled  := False;
   ActionsRestore.Enabled := False;

   DoSave := True;

   if StatusBar1.Panels.Items[2].Text = ' Browse' then
      StatusBar1.Panels.Items[2].Text := ' Modified';

end;

//------------------------------------------------------------------------------
// A field contained in the Key Changed
//------------------------------------------------------------------------------
procedure TFLPMS_Main.edtUniqueUChange(Sender: TObject);
begin

   if CanUpdate = True then
      Exit;

   DoGen := True;
   edtUserNameUChange(Sender);

end;

//------------------------------------------------------------------------------
// User clicked on a checkbox on the User panel
//------------------------------------------------------------------------------
procedure TFLPMS_Main.cbBlockedUClick(Sender: TObject);
begin
   edtUserNameUChange(Sender);
end;

//------------------------------------------------------------------------------
// User clicked on the Transfer checkbox
//------------------------------------------------------------------------------
procedure TFLPMS_Main.cbTransferUClick(Sender: TObject);
var
   ThisNode : TTreeNode;
begin

   if cbTransferU.Checked = False then begin

      cbxNewLicU.Enabled    := False;
      cbxNewPrefixU.Enabled := False;

   end else begin

      cbxNewLicU.Enabled    := True;
      cbxNewPrefixU.Enabled := True;

      cbxNewPrefixU.Clear();
      cbxNewPrefixU.Items.Add('--- Select ---');
      ThisNode := tvTree.Items.GetFirstNode();
      ThisNode := ThisNode.getFirstChild();

      while ThisNode <> nil do begin

         cbxNewPrefixU.Items.Add(ThisNode.Text);
         ThisNode := ThisNode.getNextSibling();

      end;

   end;

   cbxNewLicU.ItemIndex    := 0;
   cbxNewPrefixU.ItemIndex := 0;

end;

//------------------------------------------------------------------------------
// User clicked on the button to clear the 'Key:' field on the Root Screen
//------------------------------------------------------------------------------
procedure TFLPMS_Main. btnClearClick( Sender: TObject);
begin

   edtKeyR.Clear();
   edtKeyR.SetFocus();

end;

//------------------------------------------------------------------------------
// User clicked on the Decode button
//------------------------------------------------------------------------------
procedure TFLPMS_Main. btnDecodeRClick( Sender: TObject);
var
   DaysLeft : integer;

begin

   This_Key_Priv.Key := edtKeyR.Text;
   DaysLeft          := DoDeCode(This_Key_Priv);

   if (DaysLeft = ord(ERR_INVALID) - 3) or (DaysLeft = ord(ERR_LENGTH) - 3) then begin

      Application.MessageBox('The supplied Unlock Key is invalid.','LPMS Access Control Management',(MB_OK + MB_ICONSTOP));
      Exit;

   end else if DaysLeft = ord(ERR_EXPIRED) - 3 then
      Application.MessageBox('Unlock Key has expired.','LPMS Access Control Management',(MB_OK + MB_ICONSTOP));

   edtPrefixR.Text       := This_Key_Priv.DBPrefix;
   edtUniqueR.Text       := This_Key_Priv.Unique;
   dtpExpiryR.Date       := StrToDate(This_Key_Priv.KeyDate);
   cbxLicTypeR.ItemIndex := This_Key_Priv.License;
   cbCollectR.Checked    := This_Key_Priv.LPMS_Collections;
   cbDocGenR.Checked     := This_Key_Priv.LPMS_DocGen;
   cbFloatingR.Checked   := This_Key_Priv.LPMS_Floating;
   cbOption4R.Checked    := This_Key_Priv.LPMS_Options4;
   edtUniqueF.Text       := This_Key_Priv.Unique;

   edtKeyR.SetFocus();

end;

//------------------------------------------------------------------------------
// User clicked on the Encode button
//------------------------------------------------------------------------------
procedure TFLPMS_Main. btnEncodeRClick( Sender: TObject);
var
   ThisDate, ThisPass : string;
begin

   if UpperCase(edtUniqueR.Text) = '123456789ABC' then begin

      Application.MessageBox('Dynamic encoding not allowed when ''Unique:'' is ''123456789ABC''.','LPMS Access Control Management',(MB_OK + MB_ICONSTOP));
      edtUniqueR.SetFocus();
      Exit;

   end;

   if CpyExists(edtPrefixR.Text) = False then begin

      Application.MessageBox(PChar('' + edtPrefixR.Text + ''' is not a valid Prefix.'),'LPMS Access Control Management',(MB_OK + MB_ICONSTOP));
      edtPrefixR.SetFocus();
      Exit;

   end;

   ThisPass := InputQueryM('LPMS Access Control Management','Pass phrase:',ord(TYPE_PASSWORD));

   if ThisPass <> PassPhrase then
      Exit;

   ThisDate := FormatDateTime('yyyy/MM/dd',Now());

   if DateToStr(dtpExpiryR.Date) <= ThisDate then begin

      Application.MessageBox('Invalid date - Please provide a future date.','LPMS Access Control Management',(MB_OK + MB_ICONSTOP));
      dtpExpiryR.SetFocus();
      Exit;

   end;

   This_Key_Values.ExpDate          := DateToStr(dtpExpiryR.Date);
   This_Key_Values.DBPrefix         := edtPrefixR.Text;
   This_Key_Values.Unique           := edtUniqueR.Text;
   This_Key_Values.License          := cbxLicTypeR.ItemIndex;
   This_Key_Values.LPMS_Collections := cbCollectR.Checked;
   This_Key_Values.LPMS_DocGen      := cbDocGenR.Checked;
   This_Key_Values.LPMS_Floating    := cbFloatingR.Checked;
   This_Key_Values.LPMS_Options4    := cbOption4R.Checked;

   if DoEnCode(This_Key_Values) = True then begin

      edtKeyR.Text := This_Key_Values.Unique;
      edtKeyR.SetFocus();

   end else
      Application.MessageBox('Unlock Key generation failed.','LPMS Access Control Management',(MB_OK + MB_ICONSTOP));

   ToolsEmail.Enabled := True;
   Exit;

end;

//------------------------------------------------------------------------------
// User clicked on the Find button on the Root panel
//------------------------------------------------------------------------------
procedure TFLPMS_Main. btnFindRClick( Sender: TObject);
var
   UniqueCount                          : integer = 0;
   Loop, Found                          : boolean;
   FoundName, FoundCompany, FoundUnique : string;
   ThisCpy, ThisUser                    : TTreeNode;

begin

//--- Check whether the provided Unique ID is valid

   if Length(edtUniqueF.Text) <> 12 then begin

      Application.MessageBox('Unique Identifier is invalid.','LPMS Access Control Management',(MB_OK + MB_ICONSTOP));
      edtUniqueF.SetFocus();
      Exit;

   end;

   GetUnique(edtUniqueF.Text,ord(TYPE_XFER),UniqueCount);

   if UniqueCount = 0 then begin

      Application.MessageBox(PChar('Unique Identifier ''' + edtUniqueF.Text + ''' not found.'),'LPMS Access Control Management',(MB_OK + MB_ICONWARNING));
      edtUniqueF.SetFocus();
      Exit;

   end else if UniqueCount > 1 then begin

      ThisName    := '';
      ThisCompany := '';
      ThisUnique  := '';

      FLPMS_Main.Hide();

      FLPMS_Show := TFLPMS_Show.Create(Application);
      FLPMS_Show.ThisUnique := edtUniqueF.Text;
      FLPMS_Show.ShowModal();
      FLPMS_Show.Destroy;

      FLPMS_Main.Show();

      if ThisName = '' then
         Exit;

   end else begin

      GetData(ord(DB_UNIQUE),'','',edtUniqueF.Text);
      ThisName    := ReplaceQuote(adoQry2.FieldByName('LPMSKey_Name').AsString,ord(TYPE_READ));
      ThisCompany := ReplaceQuote(adoQry2.FieldByName('LPMSKey_Company').AsString,ord(TYPE_READ));
      ThisUnique  := ReplaceQuote(adoQry2.FieldByName('LPMSKey_Unique').AsString,ord(TYPE_READ));

   end;

   tvTree.Selected := Root;
   ThisCpy := tvTree.Selected.getFirstChild();
   Found := False;

   while ThisCpy <> nil do begin

      Loop := True;
      ThisUser := ThisCpy.getFirstChild();

      while Loop = True do begin

         if ThisUser = nil then
            Loop := False
         else begin

            GetData(ord(DB_USER),ThisCpy.Text,ThisUser.Text,'');
            FoundName    := ReplaceQuote(adoQry2.FieldByName('LPMSKey_Name').AsString,ord(TYPE_READ));
            FoundCompany := ReplaceQuote(adoQry2.FieldByName('LPMSKey_Company').AsString,ord(TYPE_READ));
            FoundUnique  := ReplaceQuote(adoQry2.FieldByName('LPMSKey_Unique').AsString,ord(TYPE_READ));

            if (ThisName = FoundName) and (ThisCompany = FoundCompany) and (ThisUnique = FoundUnique) then begin

               tvTree.Selected := ThisUser;
               Loop  := False;
               Found := True;
               break;

            end else
               ThisUser := ThisUser.getNextSibling();

         end;

      end;

      if Found = True then
         break
      else
         ThisCpy := ThisCpy.getNextSibling();

   end;

   tvTreeClick(Sender);

end;

//------------------------------------------------------------------------------
// A field related to a key on the Root panel changed
//------------------------------------------------------------------------------
procedure TFLPMS_Main. edtKeyRChange( Sender: TObject);
var
   idx1, idx2, SelStrt : integer;
   DoIns               : boolean;
   ThisKey, ThisStr    : string;
   Parts               : TStringList;

begin

   if CanUpdate = True then
      Exit;

   btnDecodeR.Enabled := False;
   btnEncodeR.Enabled := False;

   if Trim(edtKeyR.Text) = '' then
      Exit;

   if edtKeyR.Focused() = True then begin

      try
         Parts := TStringList.Create;

//--- Determine where the cursor is and whether we are at the end of the field.
//--- If we are not at the end then we are doing an insert

         SelStrt := edtKeyR.SelStart;

         if SelStrt < Length(edtKeyR.Text) then
            DoIns := True
         else
            DoIns := False;

         ThisKey  := '';
         ThisStr  := '';

//--- Remove the '-' characters from the key as these will shift due to new
//--- characters being added or eisting characters being deleted

         ExtractStrings(['-'], [' '], PChar(edtKeyR.Text), Parts);

         for idx1 := 0 to Parts.Count - 1 do
            ThisStr := ThisStr + Parts[idx1];

//--- Rebuild the structure of the key inserting '-' at the appropriate places

         for idx2 := 1 to Length(ThisStr) do begin

            ThisKey := ThisKey + ThisStr[idx2];

            if idx2 in [4,7,11,15,19,23,27] then
               ThisKey := ThisKey + '-';

         end;

      finally

         Parts.Free;

      end;

      CanUpdate    := True;
      edtKeyR.Text := ThisKey;

//--- If the cursor is positioned at a '-' then we need to move it forward by
//--- 1 position, however this test will fail if we are at the beginning of the
//--- field

      if SelStrt > 0 then begin

         if ThisKey[SelStrt] = '-' then
            Inc(SelStrt);

      end;

//--- Reposition the cursor depending on wheter we are doing an insert or not

      if DoIns = True then
         edtKeyR.SelStart := SelStrt
      else
         edtKeyR.SelStart := Length(edtKeyR.Text);

      CanUpdate := False;

   end;


//--- Set the disposition of the Encode and Decode buttons

   if Length(edtKeyR.Text) = 38 then
      btnDecodeR.Enabled := True;

   if (Length(edtPrefixR.Text) = 6) and (Length(edtUniqueR.Text) = 12) and (cbxLicTypeR.ItemIndex > 0) then
      btnEncodeR.Enabled := True;

end;

//------------------------------------------------------------------------------
// A field related to Unique has changed on the Root screen
//------------------------------------------------------------------------------
procedure TFLPMS_Main. edtUniqueFChange( Sender: TObject);
begin

   if CanUpdate = True then
      Exit;

   btnFindR.Enabled := False;

   if Length(edtUniqueF.Text) = 12 then
      btnFindR.Enabled := True;

end;

//------------------------------------------------------------------------------
// User clicked on the Backup button
//------------------------------------------------------------------------------
procedure TFLPMS_Main.ActionsBackupExecute(Sender: TObject);
begin

   if ((pnlBackup.Visible = True) or (pnlRestore.Visible = True)) then
      Exit;

//--- Remember where we were when the Backup was initiated

   if pnlRoot.Visible = True then
      PlaceHolder := 1
   else if pnlCompany.Visible = True then
      PlaceHolder := 2
   else if pnlUser.Visible = True then
      PlaceHolder := 3;

   pnlRoot.Visible    := False;
   pnlCompany.Visible := False;
   pnlUser.Visible    := False;
   pnlRestore.Visible := False;
   pnlBackup.Visible  := True;

   btnUpdate.Caption := 'Backup';
   btnCancel.Caption := 'Return';
   btnNew.Enabled    := False;
   btnCancel.Enabled := True;
   btnDelete.Enabled := False;
   btnUpdate.Enabled := False;

   edtPortB.Text := ACMPort;

   btnLockB.Visible   := True;
   btnUnlockB.Visible := False;

   BackupLog.Clear;
   btnCancel.SetFocus();

end;

//------------------------------------------------------------------------------
// User clicked on the Unlock button on the Backup screen
//------------------------------------------------------------------------------
procedure TFLPMS_Main.btnUnlockBClick(Sender: TObject);
begin

   btnUnlockB.Visible := False;
   btnLockB.Visible   := True;
   btnUpdate.Enabled  := False;
   btnLockB.SetFocus();

end;

//------------------------------------------------------------------------------
// User clicked on the Lock button on the Backup screen
//------------------------------------------------------------------------------
procedure TFLPMS_Main.btnLockBClick(Sender: TObject);
var
   ThisPass : string;

begin

   ThisPass := InputQueryM('LPMS Access Control Management','Pass phrase:',ord(TYPE_PASSWORD));

   if ThisPass = PassPhrase then begin

      btnUnlockB.Visible := True;
      btnLockB.Visible   := False;
      btnUpdate.Enabled  := True;
      btnUpdate.SetFocus();

   end;

end;

//------------------------------------------------------------------------------
// User clicked on the button to accept the output location for backups
//------------------------------------------------------------------------------
procedure TFLPMS_Main.edtLocationBAcceptDirectory(Sender: TObject; var Value: String);
begin

   Value := AppendPathDelim(Value);

end;

//------------------------------------------------------------------------------
// User click on the Unlock button on the User screen
//------------------------------------------------------------------------------
procedure TFLPMS_Main.btnUnlockClick(Sender: TObject);
begin

   btnUnlock.Visible         := False;
   btnLock.Visible           := True;
   edtUniqueU.Enabled        := False;
   cbAllowDuplicates.Enabled := False;

   edtUserNameU.SetFocus();

end;

//------------------------------------------------------------------------------
// User click on the Lock button on the User screen
//------------------------------------------------------------------------------
procedure TFLPMS_Main.btnLockClick(Sender: TObject);
var
   ThisPass : string;
begin

   ThisPass := InputQueryM('LPMS Access Control Management','Pass phrase:',ord(TYPE_PASSWORD));

   if ThisPass = PassPhrase then begin

      btnUnlock.Visible         := True;
      btnLock.Visible           := False;
      edtUniqueU.Enabled        := True;
      cbAllowDuplicates.Enabled := True;

      edtUniqueU.SetFocus();

   end;

end;

//------------------------------------------------------------------------------
// User clicked on the Restore button
//------------------------------------------------------------------------------
procedure TFLPMS_Main.ActionsRestoreExecute(Sender: TObject);
begin

   if ((pnlBackup.Visible = True) or (pnlRestore.Visible = True)) then
      Exit;

//--- Remember where we were when the Backup was initiated

   if pnlRoot.Visible = True then
      PlaceHolder := 1
   else if pnlCompany.Visible = True then
      PlaceHolder := 2
   else if pnlUser.Visible = True then
      PlaceHolder := 3;

   pnlRoot.Visible    := False;
   pnlCompany.Visible := False;
   pnlUser.Visible    := False;
   pnlRestore.Visible := True;
   pnlBackup.Visible  := False;

   btnUpdate.Caption := 'Restore';
   btnCancel.Caption := 'Return';
   btnNew.Enabled    := False;
   btnCancel.Enabled := True;
   btnDelete.Enabled := False;
   btnUpdate.Enabled := False;

   edtPortRe.Text := ACMPort;

   btnLockR.Visible   := False;
   btnUnlockR.Visible := False;

   RestoreLogRe2.Visible := True;
   RestoreLogRe1.Visible := False;
   btnOpenRe.Enabled     := False;

end;

//------------------------------------------------------------------------------
// User clicked on the Unlock button on the Restore screen
//------------------------------------------------------------------------------
procedure TFLPMS_Main.btnUnlockRClick(Sender: TObject);
begin

   btnUnlockR.Visible  := False;
   btnLockR.Visible    := True;
   btnUpdate.Enabled   := False;

end;

//------------------------------------------------------------------------------
// User clicked on the Lock button on the Restore screen
//------------------------------------------------------------------------------
procedure TFLPMS_Main.btnLockRClick(Sender: TObject);
var
   ThisPass : string;

begin

   ThisPass := InputQueryM('LPMS Access Control Management','Pass phrase:',ord(TYPE_PASSWORD));

   if ThisPass = PassPhrase then begin

      btnUnlockR.Visible  := True;
      btnLockR.Visible    := False;
      btnUpdate.Enabled   := True;

   end;

end;

//------------------------------------------------------------------------------
// User clicked on the button to open the Backup File on the Restore screen
//------------------------------------------------------------------------------
procedure TFLPMS_Main.edtBackupReButtonClick(Sender: TObject);
begin

   btnUnlockR.Visible := False;
   btnLockR.Visible   := False;

   RestoreLogRe2.Clear;

   if dlgOpen.Execute = True then
      edtBackupRe.Text := dlgOpen.FileName;

end;

//------------------------------------------------------------------------------
// The information in the Backup File field changed on the Restore screen
//------------------------------------------------------------------------------
procedure TFLPMS_Main.edtBackupReChange(Sender: TObject);
begin

   btnUnlockRClick(Sender);
   btnOpenRe.Enabled  := False;
   btnUnlockR.Visible := False;
   btnLockR.Visible   := False;

   RestoreLogRe1.Visible := False;
   RestoreLogRe2.Visible := True;
   RestoreLogRe2.Clear;

   if FileExists(edtBackupRe.Text) = True then
      btnOpenRe.Enabled := True;

end;

//------------------------------------------------------------------------------
// User clicked on the button to get the Backup information from the selected
// Backup File on the Restore screen
//------------------------------------------------------------------------------
procedure TFLPMS_Main.btnOpenReClick(Sender: TObject);
var
   ThisResult : string;

begin

   RestoreLogRe1.Visible := False;
   RestoreLogRe2.Visible := True;
   RestoreLogRe2.Clear;

   ThisResult := DoRestore(edtBackupRe.Text,'lpmsdefault',edtHostNameRe.Text,edtUserIDRe.Text,edtPasswordRe.Text,edtPortRe.Text,'',RestoreLogRe2,nil,ord(RT_OPEN));

   if ThisResult <> '' then begin

      RestoreFile := ThisResult;

      btnUnlockR.Visible := False;
      btnLockR.Visible   := True;

   end else begin

      RestoreFile := '';

      btnUnlockRClick(Sender);
      btnUnlockR.Visible := False;
      btnLockR.Visible   := False;

   end;

end;

//------------------------------------------------------------------------------
// User clicked on the Generate a new Key button
//------------------------------------------------------------------------------
procedure TFLPMS_Main.ToolsGenerateExecute(Sender: TObject);
var
   ThisDate, Pass : string;

begin

   if ((pnlBackup.Visible = True) or (pnlRestore.Visible = True)) then
      Exit;

   Pass := InputQueryM('LPMS Access Control Management','Pass phrase:',ord(TYPE_PASSWORD));

   if Pass <> PassPhrase then begin

      Application.MessageBox('Invalid Pass phrase - Key generation cancelled.','LPMS Access Control Management',(MB_OK + MB_ICONSTOP));
      Exit;

   end;

   ThisDate := FormatDateTime('yyyy/MM/dd',Now());

   if DateToStr(dtpExpiryDateU.Date) <= ThisDate then begin

      Application.MessageBox('Invalid date - Please provide a future date.','LPMS Access Control Management',(MB_OK + MB_ICONSTOP));
      dtpExpiryDateU.SetFocus();
      Exit;

   end;

   if Length(edtUniqueU.Text) <> 12 then begin

      Application.MessageBox('Unique Identifier is invalid.','LPMS Access Control Management',(MB_OK + MB_ICONSTOP));
      edtUniqueU.SetFocus();
      Exit;

   end;

   if cbxLicTypeU.ItemIndex < 1 then begin

      Application.MessageBox('License Type is invlaid.','LPMS Access Control Management',(MB_OK + MB_ICONSTOP));
      cbxLicTypeU.SetFocus();
      Exit;

   end;

   This_Key_Values.ExpDate          := DateToStr(dtpExpiryDateU.Date);
   This_Key_Values.DBPrefix         := edtPrefixU.Text;
   This_Key_Values.Unique           := edtUniqueU.Text;
   This_Key_Values.License          := cbxLicTypeU.ItemIndex;
   This_Key_Values.LPMS_Collections := cbCollectU.Checked;
   This_Key_Values.LPMS_DocGen      := cbDocGenU.Checked ;
   This_Key_Values.LPMS_Floating    := cbFloatingU.Checked;
   This_Key_Values.LPMS_Options4    := cbOption4U.Checked;

   if DoEnCode(This_Key_Values) = True then begin

      edtKeyU.Text := This_Key_Values.Unique;
      Application.MessageBox('Unlock Key generation successful.','LPMS Access Control Management',(MB_OK + MB_ICONINFORMATION));
      edtUserNameUChange(Sender);
      edtUserNameU.SetFocus();
      DoGen := False;

   end else
      Application.MessageBox('Unlock Key generation failed.','LPMS Access Control Management',(MB_OK + MB_ICONSTOP));

end;

//------------------------------------------------------------------------------
// User clicked on the Export button
//------------------------------------------------------------------------------
procedure TFLPMS_Main.ToolsExportExecute(Sender: TObject);
begin

   FLPMS_Main.Hide();

   FLPMS_Excel := TFLPMS_Excel.Create(Application);

   FLPMS_Excel.Host     := HostName;
   FLPMS_Excel.UserID   := UserName;
   FLPMS_Excel.Password := Password;

   FLPMS_Excel.ShowModal();
   FLPMS_Excel.Destroy;

   FLPMS_Main.Show();

end;

//------------------------------------------------------------------------------
// User clicked on the Send Email button
//------------------------------------------------------------------------------
procedure TFLPMS_Main.ToolsEmailExecute(Sender: TObject);
var
   idx          : integer;     // Loop counter
   EditEmail    : boolean;     // Used to determine whether the standalone emil viewer should be called
   From         : string;      // Email address of the Sender
   ToStr        : string;      // To addresses, comma delimited
   CcStr        : string;      // Cc addresses, comma delimited
   BccStr       : string;      // Bcc addresses, comma delimited
   Subject      : string;      // Email Subject
   Body         : string;      // Body of the email
   Attach       : string;      // '|' delimited string containing files to be attached
   SMTPStr      : string;      // '|' delimited string containing the SMTP parameters
   SMUtil       : string;      // Name of external email viewer - assumed to be in the execution directory
   Addressee    : string;      // Name of he addressee
   ExpiryDate   : string;      // Key expiry date
   Key          : string;      // The Key
   Prefix       : string;      // The Prefix
   Process      : TProcess;    // Used for calling the standalone email utility if 'Edit Email' is checked

begin

   if (pnlBackup.Visible = True) or (pnlRestore.Visible = True) then
      Exit;

//--- We can only send email from the Root and User panels. If the Root panel
//--- is visible then the ToStr is still undetermined.

   if pnlRoot.Visible = True then begin

      EditEmail  := True;
      ToStr      := 'Please specify';
      Addressee  := '[Insert]';
      ExpiryDate := DateToStr(dtpExpiryR.Date);
      Key        := edtKeyR.Text;
      Prefix     := edtPrefixR.Text;

   end else if pnlUser.Visible = True then begin

      EditEmail  := cbEditemail.Checked;
      ToStr      := edtEmailU.Text;
      Addressee  := edtUserNameU.Text;
      ExpiryDate := DateToStr(dtpExpiryDateU.Date);
      Key        := edtKeyU.Text;
      Prefix     := edtPrefixU.Text;

   end else begin

      Exit;

   end;

//--- Construct the email
   From    := 'registration@bluecrane.cc';
   CcStr   := '';
   BccStr  := 'registration@bluecrane.cc';
   Subject := 'LPMS Activation Key';
   Attach  := '';
   SMTPStr := edtServerR.Text + '|registration@bluecrane.cc|' + edtPasswordR.Text + '|';

   Body    := 'Dear ' + Addressee + ',|' +
              ' |' +
              'Attached below is your new/updated LPMS Activation Key which expires on ' + ExpiryDate + ':|' +
              ' |' +
              '   ' + Key + '|' +
              ' |' +
              'To activate LPMS with this key please do the following:|' +
              ' |' +
              '1.  Run LPMS_FirstRun.|' +
              ' |' +
              '    1.1. Click on Start;|' +
              '    1.2. Click on Programs;|' +
              '    1.3. Click on BlueCrane Software; and|' +
              '    1.4. Click on LPMS_FirstRun;|' +
              ' |' +
              '2.  If your release is a multi-company release then:|' +
              ' |' +
              '    2.1. Enter ''' + Prefix + ''' in the field next to ''DBPrefix:'';|' +
              '    2.2. Select ''Multi Company Support''; and|' +
              '    2.3. Click on ''Ok'' button to proceed;|' +
              ' |' +
              '3.  If your release is not a multi-company release simply click on the ''Ok'' button;|' +
              '4.  Click on the ''Maintenance'' tab;|' +
              '5.  Enter ''' + Prefix + ''' in the field next to ''Prefix:'';|' +
              '6.  Copy the key above and paste in the field next to ''Key:'';|' +
              '7.  Click on the ''Update'' button, then on the ''OK'' button and then on the ''Close'' button; and|' +
              '8.  Start LPMS.|' +
              ' |' +
              'Please contact BlueCrane Software Development should you have any queries or experience any problems.|' +
              ' |' +
              'Sincerely|' +
              'LPMS Support|' +
              ' |' +
              ' |' +
              'BlueCrane Software Development CC|' +
              ' |' +
              '17 Church Street, Lamberts Bay, Western Cape, 8130|' +
              'PO Box 204, Lamberts Bay, 8130|' +
              'Tel: 027-432-2561|' +
              'Fax: 086-585-9545|' +
              'www.bluecrane.cc|' +
              ' |' +
              'Sent from LPMS ACM (c) 2009-' + FormatDateTime('yyyy',Now()) + ' BlueCrane Software Development CC|';

//--- Call and execute the SendMimeMail function in the DLL

   if EditEmail = True then begin

//--- User wants to see/edit the email before it is sent. Make sure the
//--- external program exists and can be called

      SMUtil := ExtractFilePath(Application.ExeName) + 'BSD_SendEmail';
{$IFDEF WINDOWS}
      SMUtil := SMUtil + '.exe';
{$ENDIF}

      if FileExists(SMUtil) = False then begin

         Application.MessageBox('Unable to load external utility to view/edit the email content.','LPMS Access Control Management',(MB_OK + MB_ICONSTOP));
         Exit;

      end;

      Process := TProcess.Create(nil);

      try

         Process.InheritHandles := False;
         Process.Options        := [poWaitOnExit];
         Process.ShowWindow     := swoShow;

   //--- Copy default environment variables including DISPLAY variable for GUI
   //--- application to work

         for idx := 1 to GetEnvironmentVariableCount do
            Process.Environment.Add(GetEnvironmentString(idx));

         Process.Executable := SMUtil;
         Process.Parameters.Add('--args');
         Process.Parameters.Add('-FBSD SEND EMAIL');
         Process.Parameters.Add('-P' + SMTPStr);
         Process.Parameters.Add('-B' + BccStr);
         Process.Parameters.Add('-E' + Body);
         Process.Parameters.Add('-T' + ToStr);
         Process.Parameters.Add('-C' + CcStr);
         Process.Parameters.Add('-S' + Subject);
         Process.Parameters.Add('-O' + From);

         FLPMS_Main.Hide();
         Process.Execute;
         FLPMS_Main.Show();

      finally
         Process.Free;
      end;

   end else begin

      if SendMimeMail(From, ToStr, CcStr, BccStr, Subject, Body, Attach, SMTPStr) = False then
         Application.MessageBox('Sending Email failed! Please check Email set-up details.','LPMS Access Control Management',(MB_OK + MB_ICONSTOP))
      else
         Application.MessageBox('Send Email completed.','LPMS Access Control Management',(MB_OK + MB_ICONINFORMATION));

   end;

end;

//==============================================================================
//===
//=== Database functions
//===
//==============================================================================

//------------------------------------------------------------------------------
// Function to connect to the Database
//------------------------------------------------------------------------------
procedure TFLPMS_Main.OpenDB(ThisType: integer);
begin

   case ThisType of

      ord(DB_OPEN): begin

         OpenDB(ord(DB_CLOSE));

         sqlCon.HostName     := HostName;
         sqlCon.UserName     := UserName;
         sqlCon.Password     := Password;
         sqlCon.DatabaseName := 'lpmsdefault';
         sqlCon.Port         := StrToInt(ACMPort);
         adoQry1.DataBase    := sqlCon;
         adoQry2.Database    := sqlCon;

      end;

      ord(DB_CLOSE): begin

         adoQry1.Close();
         adoQry2.Close();
         sqlCon.Close();

      end;

   end;

end;

//------------------------------------------------------------------------------
// Function to read Company and User Records from the Database
//------------------------------------------------------------------------------
function TFLPMS_Main.GetData(ThisType: integer; Company, User, Unique: string) : boolean;
var
   S1 : string;

begin

   case ThisType of

      ord(DB_ALL): begin

         S1 := 'SELECT LPMSKey_Prefix FROM companies ORDER BY LPMSKey_Prefix';
         MySQLAccess(ord(DB_SELECT),S1,adoQry1);

      end;

      ord(DB_COMPANY): begin

         S1 := 'SELECT * FROM companies WHERE LPMSKey_Prefix = ''' + Company + '''';
         MySQLAccess(ord(DB_SELECT),S1,adoQry1);

      end;

      ord(DB_USER): begin

         S1 := 'SELECT * FROM users WHERE LPMSKey_Name LIKE ''' + User + ''' AND LPMSKey_Prefix = ''' + Company + ''' ORDER BY LPMSKey_Name';
         MySQLAccess(ord(DB_SELECT),S1,adoQry2);

      end;

      ord(DB_UNIQUE): begin

         S1 := 'SELECT * FROM users WHERE LPMSKey_Unique = ''' + Unique + '''';
         MySQLAccess(ord(DB_SELECT),S1,adoQry2);

      end;

   end;


   Result := True;

end;

//------------------------------------------------------------------------------
// Function to delete Company and User Records from the Database
//------------------------------------------------------------------------------
function TFLPMS_Main.DelData(ThisType: integer; Company, User: string) : boolean;
var
   S1 : string;

begin

   case ThisType of

      ord(DB_COMPANY): begin

         S1 := 'DELETE FROM users WHERE LPMSKey_Prefix = ''' + Company + '''';
         MySQLAccess(ord(DB_OTHER),S1,adoQry1);

         S1 := 'DELETE FROM companies WHERE LPMSKey_Prefix = ''' + Company + '''';
         MySQLAccess(ord(DB_OTHER),S1,adoQry1);

      end;

      ord (DB_USER): begin

         S1 := 'DELETE FROM users WHERE LPMSKey_Name = ''' + User + ''' AND LPMSKey_Prefix = ''' + Company + '''';
         MySQLAccess(ord(DB_OTHER),S1,adoQry2);

      end;

   end;

   Result := True;

end;

//------------------------------------------------------------------------------
// Function to check whether a company already exists
//------------------------------------------------------------------------------
function TFLPMS_Main.CpyExists(Prefix: string) : boolean;
var

   RetCode : boolean;
   S1      : string;

begin

   S1 := 'SELECT LPMSKey_Prefix FROM companies WHERE LPMSKey_Prefix = ''' + Prefix + '''';

   MySQLAccess(ord(DB_SELECT),S1,adoQry1);

   if adoQry1.RecordCount = 0 then
      RetCode := False
   else
      RetCode := True;

   Result := RetCode;

end;

//------------------------------------------------------------------------------
// Function to insert/update a Company record
//------------------------------------------------------------------------------
function TFLPMS_Main.UpdateCpy() : boolean;
var
   ThisResult    : boolean;
   S1, TimeStamp : string;

begin

   OpenDB(ord(DB_OPEN));

   if StatusBar1.Panels.Items[2].Text = ' New' then begin

      TimeStamp := FormatDateTime('yyyy/MM/dd',Now()) + '+' + FormatDateTime('HH:nn:ss:zzz',Now()) + '+' + UserName;

      S1 := 'INSERT INTO companies (LPMSKey_Prefix, LPMSKey_Name, LPMSKey_Desc, LPMSKey_LicCount, LPMSKey_Interval, LPMSKey_Blocked, LPMSKey_CreatedBy, LPMSKey_CreatedOn, LPMSKey_CreatedAt, LPMSKey_TimeStamp) Values(''' +
            ReplaceQuote(edtPrefixC.Text,ord(TYPE_WRITE))  + ''', ''' +
            cbxKeyTypeC.Text                               + ''', ''' +
            ReplaceQuote(edtCompanyC.Text,ord(TYPE_WRITE)) + ''', '   +
            speLicCountC.Text                              + ', '     +
            speRenewalC.Text                               + ', '     +
            BoolToStr(cbBlockedC.Checked)                  + ', '''   +
            UserName                                       + ''', ''' +
            FormatDateTime('yyyy/MM/dd',Now())             + ''', ''' +
            FormatDateTime('HH:nn:ss',Now())               + ''', ''' +
            ReplaceQuote(TimeStamp,ord(TYPE_WRITE))        + ''')';

      ThisResult := MySQLAccess(ord(DB_OTHER),S1,adoQry1);

   end else begin

      S1 := 'UPDATE companies SET LPMSKey_Name = ''' + ReplaceQuote(cbxKeyTypeC.Text,ord(TYPE_WRITE)) +
            ''', LPMSKey_Desc = '''        + ReplaceQuote(edtCompanyC.Text,ord(TYPE_WRITE)) +
            ''', LPMSKey_LicCount = '      + speLicCountC.Text                              +
            ', LPMSKey_Interval = '        + speRenewalC.Text                               +
            ', LPMSKey_Blocked = '         + BoolToStr(cbBlockedC.Checked)                  +
            ',  LPMSKey_ModBy = '''        + UserName                                       +
            ''', LPMSKey_ModOn = '''       + FormatDateTime('yyyy/MM/dd',Now())             +
            ''', LPMSKey_ModAt = '''       + FormatDateTime('HH:nn:ss',Now())               +
            ''' WHERE LPMSKey_Prefix = ''' + ReplaceQuote(edtPrefixC.Text,ord(TYPE_WRITE))  + '''';

      Result := MySQLAccess(ord(DB_OTHER),S1,adoQry1);

   end;

   OpenDB(ord(DB_CLOSE));

   Result := ThisResult;

end;

//------------------------------------------------------------------------------
// Function to check whether a User exists
//------------------------------------------------------------------------------
function TFLPMS_Main.UserExists(User, Prefix: string) : boolean;
var
   S1      : string;

begin

   S1 := 'SELECT LPMSKey_Key FROM users WHERE LPMSKey_Name = ''' + User + ''' AND LPMSKey_Prefix = ''' + Prefix + '''';

   OpenDB(ord(DB_OPEN));
   MySQLAccess(ord(DB_SELECT),S1,adoQry1);

   if adoQry1.RecordCount = 0 then
      Result := False
   else
      Result := True;

   OpenDB(ord(DB_CLOSE));

end;

//------------------------------------------------------------------------------
// Function to find a Unique identifier in the database
//------------------------------------------------------------------------------
function TFLPMS_Main.GetUnique(Unique: string; ThisType: integer; var UniqueCount: integer) : string;
var
   Count                : integer;
   S1, NameStr, ThisCpy : string;

begin

   S1 := 'SELECT LPMSKey_Key, LPMSKey_Name, LPMSKey_Company FROM users WHERE LPMSKey_Unique = ''' + Unique + '''';

   OpenDB(ord(DB_OPEN));
   MySQLAccess(ord(DB_SELECT),S1,adoQry1);

   UniqueCount := adoQry1.RecordCount;
   Count       := adoQry1.RecordCount;

//--- Check whether the Unique exists. If this is a normal Update operation
//--- i.e. Type = TYPE_TEST then we must determine whether the user changed
//--- the Unique or not.

   if Count = 0 then
      NameStr := ''
   else begin

      if ThisType = ord(TYPE_TEST) then begin

         if edtDBKeyU.Text = adoQry1.FieldByName('LPMSKey_Key').AsString then
            NameStr := ''
         else begin

            NameStr := adoQry1.FieldByName('LPMSKey_Name').AsString;
            ThisCpy := adoQry1.FieldByName('LPMSKey_Company').AsString;

         end;

      end else begin

         NameStr := adoQry1.FieldByName('LPMSKey_Name').AsString;
         ThisCpy := adoQry1.FieldByName('LPMSKey_Company').AsString;

      end;

   end;

   OpenDB(ord(DB_CLOSE));

   if NameStr = '' then
      Result := NameStr
   else
      Result := ('''' + ThisName + ''' in ''' + ThisCpy + '''');

end;

//------------------------------------------------------------------------------
// Function to insert/update a User record
//------------------------------------------------------------------------------
function TFLPMS_Main.UpdateUser() : boolean;
var
   Renewals                 : integer;
   S1, TimeStamp, ThisPref  : string;

begin

   OpenDB(ord(DB_OPEN));

//--- Increase the value of Renewals

   Renewals := StrToInt(edtRenewalsU.Text) + 1;

//--- Now do the insert or Update

   if StatusBar1.Panels.Items[2].Text = ' New' then begin

      if cbxNewPrefixU.ItemIndex = 0 then
         ThisPref := edtPrefixU.Text
      else
         ThisPref := cbxNewPrefixU.Text;

      TimeStamp := FormatDateTime('yyyy/MM/dd',Now()) + '+' + FormatDateTime('HH:nn:ss:zzz',Now()) + '+' + UserName;

      S1 := 'INSERT INTO users (LPMSKey_Name, LPMSKey_Company, LPMSKey_Email, LPMSKey_Contact, LPMSKey_Prefix, LPMSKey_Unique, LPMSKey_LicType, LPMSKey_AllowDuplicates, LPMSKey_ExpiryDate, LPMSKey_Activation, LPMSKey_Blocked, LPMSKey_Renewals, LPMSKey_CreatedBy, LPMSKey_CreatedOn, LPMSKey_CreatedAt, LPMSKey_TimeStamp, LPMSKey_Transfer, LPMSKey_NewPrefix, LPMSKey_NewLicense) Values(''' +
            ReplaceQuote(edtUserNameU.Text,ord(TYPE_WRITE))  + ''', ''' +
            ReplaceQuote(edtCompanyU.Text,ord(TYPE_WRITE))   + ''', ''' +
            ReplaceQuote(edtEmailU.Text,ord(TYPE_WRITE))     + ''', ''' +
            ReplaceQuote(edtContactU.Text,ord(TYPE_WRITE))   + ''', ''' +
            ReplaceQuote(edtPrefixU.Text,ord(TYPE_WRITE))    + ''', ''' +
            ReplaceQuote(edtUniqueU.Text,ord(TYPE_WRITE))    + ''', '   +
            IntToStr(cbxLicTypeU.ItemIndex)                  + ', '     +
            BoolToStr(cbAllowDuplicates.Checked)             + ', '''   +
            FormatDateTime('yyyy/MM/dd',dtpExpiryDateU.Date) + ''', ''' +
            ReplaceQuote(edtKeyU.Text,ord(TYPE_WRITE))       + ''', '   +
            BoolToStr(cbBlockedU.Checked)                    + ', '     +
            IntToStr(Renewals)                               + ', '''   +
            UserName                                         + ''', ''' +
            FormatDateTime('yyyy/MM/dd',Now())               + ''', ''' +
            FormatDateTime('HH:mm:ss',Now())                 + ''', ''' +
            ReplaceQuote(TimeStamp,ord(TYPE_WRITE))          + ''', '   +
            BoolToStr(cbTransferU.Checked)                   + ', '''   +
            ReplaceQuote(ThisPref,ord(TYPE_WRITE))           + ''', '   +
            IntToStr(cbxNewLicU.ItemIndex)                   + ')';

      Result := MySQLAccess(ord(DB_OTHER),S1,adoQry1);

   end else begin

      if cbxNewPrefixU.ItemIndex = 0 then
         ThisPref := edtPrefixU.Text
      else
         ThisPref := cbxNewPrefixU.Text;

      S1 := 'UPDATE users SET LPMSKey_Name = ''' + ReplaceQuote(edtUserNameU.Text,ord(TYPE_WRITE))  +
            ''', LPMSKey_Company = '''           + ReplaceQuote(edtCompanyU.Text,ord(TYPE_WRITE))   +
            ''', LPMSKey_Email = '''             + ReplaceQuote(edtEmailU.Text,ord(TYPE_WRITE))     +
            ''', LPMSKey_Contact = '''           + ReplaceQuote(edtContactU.Text,ord(TYPE_WRITE))   +
            ''', LPMSKey_Prefix = '''            + ReplaceQuote(edtPrefixU.Text,ord(TYPE_WRITE))    +
            ''', LPMSKey_Unique = '''            + ReplaceQuote(edtUniqueU.Text,ord(TYPE_WRITE))    +
            ''', LPMSKey_LicType = '             + IntToStr(cbxLicTypeU.ItemIndex)                  +
            ', LPMSKey_AllowDuplicates = '       + BoolToStr(cbAllowDuplicates.Checked)             +
            ', LPMSKey_ExpiryDate = '''          + FormatDateTime('yyyy/MM/dd',dtpExpiryDateU.Date) +
            ''', LPMSKey_Activation = '''        + ReplaceQuote(edtKeyU.Text,ord(TYPE_WRITE))       +
            ''', LPMSKey_Blocked = '             + BoolToStr(cbBlockedU.Checked)                    +
            ', LPMSKey_Renewals = '              + IntToStr(Renewals)                               +
            ',  LPMSKey_ModBy = '''              + UserName                                         +
            ''', LPMSKey_ModOn = '''             + FormatDateTime('YYYY/MM/DD',Now())               +
            ''', LPMSKey_ModAt = '''             + FormatDateTime('HH:mm:ss',Now())                 +
            ''', LPMSKey_Transfer = '            + BoolToStr(cbTransferU.Checked)                   +
            ', LPMSKey_NewPrefix = '''           + ReplaceQuote(ThisPref,ord(TYPE_WRITE))           +
            ''', LPMSKey_NewLicense = '          + IntToStr(cbxNewLicU.ItemIndex)                   +
            ' WHERE LPMSKey_Key = '              + edtDBKeyU.Text;

      Result := MySQLAccess(ord(DB_OTHER),S1,adoQry1);

   end;

   OpenDB(ord(DB_CLOSE));

end;

//==============================================================================
//===
//=== Support Functions
//===
//==============================================================================

//------------------------------------------------------------------------------
// Function to Request a PassPhrase from the User
//------------------------------------------------------------------------------
function TFLPMS_Main.InputQueryM(ThisCap, Question : string; DispType: integer) : string;
begin

   FLPMS_InputQuery := TFLPMS_InputQuery.Create(Application);

   FLPMS_InputQuery.Caption := ThisCap;
   FLPMS_InputQuery.edtInput.Clear();
   FLPMS_InputQuery.lblCaption.Caption := Question;

   if DispType = ord(TYPE_PASSWORD) then
      FLPMS_InputQuery.edtInput.PasswordChar := '*';

   FLPMS_Main.Hide();
   FLPMS_InputQuery.ShowModal();
   FLPMS_Main.Show();

   FLPMS_InputQuery.Destroy;

   Result := ThisRes;

end;

//------------------------------------------------------------------------------
// Function to access the MySQL database
//------------------------------------------------------------------------------
function TFLPMS_Main.MySQLAccess(ThisType: integer; S1 : string; adoQry: TSQLQuery) : boolean;
begin

   case ThisType of

      ord(DB_SELECT): begin

         try

            adoQry.Close();
            adoQry.SQL.Text := S1;
            adoQry.Open();

            Except on Err : Exception do begin

               if ((AnsiContainsStr(Err.Message,'MySQL server has gone away') = True) or (AnsiContainsStr(Err.Message,'Lost connection to MySQL server during query') = True)) then begin

                  LPMS_ACM_Abort(Err.Message);
                  Result := False;
                  Exit

               end else begin

                  Application.MessageBox(PChar('Unexpected Data Base error: ''' + Err.Message + ''' - Operation cancelled'),'LPMS Access Control Management',(MB_OK + MB_ICONSTOP));
                  Result := False;
                  Exit;

               end;

            end;

         end;

      end;

      ord(DB_OTHER): begin

         try

            adoQry.Close();
            adoQry.SQL.Text := S1;
            adoQry.ExecSQL();

            Except on Err : Exception do begin

               if ((AnsiContainsStr(Err.Message,'MySQL server has gone away') = True) or (AnsiContainsStr(Err.Message,'Lost connection to MySQL server during query') = True)) then begin

                  LPMS_ACM_Abort(Err.Message);
                  Result := False;
                  Exit

               end else begin

                  Application.MessageBox(PChar('Unexpected Data Base error: ''' + Err.Message + ''' - Operation cancelled'),'LPMS Access Control Management',(MB_OK + MB_ICONSTOP));
                  Result := False;
                  Exit;

               end;

            end;

         end;

      end;

   end;

   Result := True;

end;

//------------------------------------------------------------------------------
// Function to handle MySQL connection problems
//------------------------------------------------------------------------------
procedure TFLPMS_Main.LPMS_ACM_Abort(Msg: string);
var
   idx     : integer;
   Process : TProcess;

begin

   if Application.MessageBox(PChar('FATAL: Unexpected/Unhandled error: ''' + Msg + ''' - LPMS_ACM cannot continue. You can: ' + #10 + #10 + #10 + 'Click [Ok] to terminate and restart LPMS_ACM; or' + #10 + #10 + 'Click [Cancel] to terminate LPMS_ACM'),'LPMS Access Control Management',(MB_OKCANCEL + MB_ICONSTOP)) = ID_CANCEL then begin

      Application.Terminate;
      Exit;

   end;

   Process := TProcess.Create(nil);

   try

      Process.InheritHandles := False;
      Process.Options        := [];
      Process.ShowWindow     := swoShow;

//--- Copy default environment variables including DISPLAY variable for GUI
//--- application to work

      for idx := 1 to GetEnvironmentVariableCount do
         Process.Environment.Add(GetEnvironmentString(idx));

      Process.Executable := Application.ExeName;
      Process.Parameters.Add('--args');
      Process.Parameters.Add('-H' + HostName);
      Process.Parameters.Add('-u' + UserName);
      Process.Parameters.Add('-p' + Password);

      Process.Execute;

   finally
      Process.Free;
   end;

   Application.Terminate;
   Exit;

end;

//------------------------------------------------------------------------------
// Function to manage replacement of single quotes and back slashes to avoid
// SQL errors
//------------------------------------------------------------------------------
function TFLPMS_Main.ReplaceQuote(S1 : string; ThisType: integer) : string;
begin

   if ThisType = ord(TYPE_READ) then begin

      S1 := AnsiReplaceStr(S1,'&quot','''');
      S1 := AnsiReplaceStr(S1,'&slash','\');

   end else begin

      S1 := AnsiReplaceStr(S1,'''','&quot');
      S1 := AnsiReplaceStr(S1,'\','&slash');

   end;

   Result := S1;

end;

//------------------------------------------------------------------------------
// Adjust some display artifacts for optimum presentation on the execution
// platform
//------------------------------------------------------------------------------
procedure TFLPMS_Main.SetPlatform();
begin

{$IFDEF WINDOWS}

//--- Root Panel

   btnClear.Height       :=  23;
   btnClear.Width        :=  23;
   btnPaste.Height       :=  23;
   btnPaste.Width        :=  23;
   btnCopy.Height        :=  23;
   btnCopy.Width         :=  23;
   btnDecodeR.Height     :=  23;
   btnEncodeR.Height     :=  23;
   btnFindR.Height       :=  23;
   dtpExpiryR.Left       := 192;
   dtpExpiryR.Width      := 127;
   dtpExpiryR.Height     :=  23;
   dtpExpiryDateU.Height :=  23;
   cbxLicTypeR.Height    :=  23;

//--- User Panel

   btnUnlock.Height   := 23;
   btnUnlock.Width    := 23;
   btnLock.Height     := 23;
   btnLock.Width      := 23;
   cbxLicTypeU.Height := 23;
   btnCopy.Height     := 23;

//--- Restore Panel

   btnOpenRe.Top := 160;

{$ENDIF}

{$IFDEF LINUX}

   {$IFDEF CPUARM}

//--- Root Panel

      btnClear.Height       :=  24;
      btnClear.Width        :=  23;
      btnPaste.Height       :=  24;
      btnPaste.Width        :=  23;
      btnCopy.Height        :=  23;
      btnCopy.Width         :=  23;
      btnDecodeR.Height     :=  23;
      btnEncodeR.Height     :=  23;
      btnFindR.Height       :=  23;
      dtpExpiryR.Left       := 192;
      dtpExpiryR.Width      := 127;
      dtpExpiryR.Height     :=  24;
      dtpExpiryDateU.Height :=  23;
      cbxLicTypeR.Height    :=  23;

//--- User Panel

      btnUnlock.Height   := 23;
      btnUnlock.Width    := 23;
      btnLock.Height     := 23;
      btnLock.Width      := 23;
      cbxLicTypeU.Height := 23;
      btnCopy.Height     := 24;

//--- Restore Panel

      btnOpenRe.Top := 155;

   {$ELSE}

//--- Root Panel

      btnClear.Height       :=  26;
      btnClear.Width        :=  26;
      btnPaste.Height       :=  26;
      btnPaste.Width        :=  26;
      btnCopy.Height        :=  26;
      btnCopy.Width         :=  26;
      btnDecodeR.Height     :=  26;
      btnEncodeR.Height     :=  26;
      btnFindR.Height       :=  26;
      dtpExpiryR.Left       := 193;
      dtpExpiryR.Width      := 126;
      dtpExpiryR.Height     :=  26;
      dtpExpiryDateU.Height :=  26;
      cbxLicTypeR.Height    :=  26;

//--- User Panel

      btnUnlock.Height   := 26;
      btnUnlock.Width    := 26;
      btnLock.Height     := 26;
      btnLock.Width      := 26;
      cbxLicTypeU.Height := 26;
      btnCopy.Height     := 26;

//--- Restore Panel

      btnOpenRe.Top := 156;

   {$ENDIF}

{$ENDIF}

{$IFDEF DARWIN}

//--- Root Panel

   btnClear.Height       :=  23;
   btnClear.Width        :=  23;
   btnPaste.Height       :=  23;
   btnPaste.Width        :=  23;
   btnCopy.Height        :=  23;
   btnCopy.Width         :=  23;
   btnDecodeR.Height     :=  23;
   btnEncodeR.Height     :=  23;
   btnFindR.Height       :=  23;
   dtpExpiryR.Left       := 192;
   dtpExpiryR.Width      := 127;
   dtpExpiryR.Height     :=  23;
   dtpExpiryDateU.Height :=  23;
   cbxLicTypeR.Height    :=  23;

//--- User Panel

   btnUnlock.Height   := 23;
   btnUnlock.Width    := 23;
   btnLock.Height     := 23;
   btnLock.Width      := 23;
   cbxLicTypeU.Height := 23;
   btnCopy.Height     := 23;

//--- Restore Panel

     btnOpenRe.Top := 160;

{$ENDIF}

end;

//------------------------------------------------------------------------------

end.

