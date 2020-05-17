//------------------------------------------------------------------------------
// Date.......: 10 May 2020
// System.....: LPMS Access Control Manager
// Program ID.: LPMS_Main
// Platform...: Lazarus (Winblows, Linux, Raspbian & macOS)
// Author.....: Francois De Bruin Meyer (BlueCrane Software Development CC)
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
   Process, DynLibs,

{$IFDEF WINDOWS}                     // Target is Winblows
   mysql56conn, sqldb;
{$ENDIF}

{$IFDEF LINUX}                       // Target is Linux
   {$IFDEF CPUARMHF}                 // Running on ARM (Raspbian) architecture
      mysql55conn, sqldb;
   {$ELSE}                           // Running on Intel architecture
      mysql57conn, sqldb;
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

   { TFLPMS_Main }

   TFLPMS_Main = class( TForm)
   About1: TMenuItem;
   Actions1: TMenuItem;
   ActionsBackup: TAction;
   ActionsRefresh: TAction;
   ActionsRestore: TAction;
   actList: TActionList;
   Backup1: TMenuItem;
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
   btnDecodeR: TButton;
   btnDelete: TButton;
   btnEncodeR: TButton;
   btnExit: TButton;
   btnFindR: TButton;
   btnLock: TBitBtn;
   btnLockB: TBitBtn;
   btnLockR: TBitBtn;
   btnNew: TButton;
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
   dlgOpen: TOpenDialog;
   dlgSave: TSaveDialog;
   dtpExpiryDateU: TDateTimePicker;
   dtpExpiryR: TDateTimePicker;
   edtCompanyC: TEdit;
   edtCompanyU: TEdit;
   edtContactU: TEdit;
   edtDBKeyU: TEdit;
   edtEmailU: TEdit;
   edtHost: TEdit;
   edtKeyU: TEdit;
   edtPassword: TEdit;
   edtPrefixC: TEdit;
   edtPrefixR: TEdit;
   edtPrefixU: TEdit;
   edtRecord: TEdit;
   edtRenewalsU: TEdit;
   edtTable: TEdit;
   edtUniqueF: TEdit;
   edtKeyR: TEdit;
   edtServerR: TEdit;
   edtUniqueR: TEdit;
   edtPasswordR: TEdit;
   edtUniqueU: TEdit;
   edtUserID: TEdit;
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
   Label26: TLabel;
   Label27: TLabel;
   Label28: TLabel;
   Label29: TLabel;
   Label3: TLabel;
   Label30: TLabel;
   Label31: TLabel;
   Label32: TLabel;
   Label33: TLabel;
   Label4: TLabel;
   Label5: TLabel;
   Label6: TLabel;
   Label7: TLabel;
   Label8: TLabel;
   Label9: TLabel;
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
   speRenewalC: TSpinEdit;
   speLicCountC: TSpinEdit;
   Splitter1: TSplitter;
   sqlTran: TSQLTransaction;
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
   procedure btnDecodeRClick( Sender: TObject);
   procedure btnDeleteClick(Sender: TObject);
   procedure btnEncodeRClick( Sender: TObject);
   procedure btnFindRClick( Sender: TObject);
   procedure btnLockBClick( Sender: TObject);
   procedure btnLockClick( Sender: TObject);
   procedure btnLockRClick( Sender: TObject);
   procedure btnNewClick(Sender: TObject);
   procedure btnUnlockBClick( Sender: TObject);
   procedure btnUnlockClick( Sender: TObject);
   procedure btnUnlockRClick( Sender: TObject);
   procedure btnUpdateClick(Sender: TObject);
   procedure cbBlockedUClick(Sender: TObject);
   procedure cbTransferUClick(Sender: TObject);
   procedure edtKeyRChange( Sender: TObject);
   procedure edtKeyRKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
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

type

   REC_Key_Priv = record
      Key              : string;
      DaysLeft         : integer;
      LPMS_Collections : boolean;
      LPMS_DocGen      : boolean;
      LPMS_Floating    : boolean;
      LPMS_Option4     : boolean;
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

private  { Private Declarations }

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

   PlaceHolder                        : integer;
   DoSave, CanUpdate, DoGen, FirstRun : boolean;
   Root                               : TTreeNode;
   This_Key_Values                    : REC_Key_Values;
   This_Key_Priv                      : REC_Key_Priv;


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
   function  DeCode() : integer;
   function  EnCode() : boolean;

type
   DB_TYPE = (DB_OPEN,             // Open the Database
              DB_CLOSE,            // Close the Database
              DB_ALL,              // Get all prefix records from the database
              DB_COMPANY,          // Get all Company records
              DB_USER,             // Get all User records
              DB_UNIQUE,           // Get all the records for a sepcific Unique
              DB_SELECT,           // 'SELECT' statement
              DB_OTHER);           // All other SQL statements

   TY_TYPE = (TYPE_PASSWORD,       // Used when calling LPMS_InoputQuery
              TYPE_TEST,           //
              TYPE_XFER,           //
              TYPE_READ,           // Reading from the database - &quote to '''
              TYPE_WRITE);         // Writing to the database - ''' to &quote

   RE_RSLT = (ERR_INVALID,         // The content of the Key is invalid
              ERR_LENGTH,          // The length of the Key is wrong
              ERR_EXPIRED);        // The Key has expired

public   { Public Declarations }

   UserName    : string;      // UserName passed from Login
   Password    : string;      // Password passed from Login
   HostName    : string;      // Host name passed from Login
   Version     : string;      // Version string passed from Login
   CopyRight   : string;      // Copyright notice passed from Login
   ThisRes     : string;      // Holds result from InoutQuery
   ThisName    : string;      // Used by LPMS_Show
   ThisCompany : string;      // Used by LPMS_Show
   ThisUnique  : string;      // Used by LPMS_Show
   SMTPHost    : string;      // SMTP Host for sending emails
   SMTPPass    : string;      // SMTP Password for sending emails

end;

//------------------------------------------------------------------------------
// Global variables
//------------------------------------------------------------------------------
var
   FLPMS_Main: TFLPMS_Main;

implementation

   uses LPMS_Login, LPMS_InputQuery, LPMS_Show;

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

   DefaultFormatSettings.ShortDateFormat := 'yyyy/MM/dd';
   DefaultFormatSettings.DateSeparator   := '/';

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
   {$IFDEF CPUI386}                 // Running on a version below Catalina
      sqlCon := TMySQL55Connection.Create(nil);
   {$ELSE}
     sqlCon := TMySQL57Connection.Create(nil);
   {$ENDIF}
{$ENDIF}

   sqlTran.DataBase    := sqlCon;
   adoQry1.Transaction := sqlTran;
   adoQry2.Transaction := sqlTran;

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
   Root := tvTree.Items.AddFirst(nil,'LPMS');


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
   edtServerR.Text   := SMTPHost;
   edtPasswordR.Text := SMTPPass;
   edtKeyR.SetFocus();

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

   FLPMS_Login.SMTPHost := edtServerR.Text;
   FLPMS_Login.SMTPPass := edtPasswordR.Text;

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
   ThisRoot := tvTree.Items.AddFirst(nil,'LPMS');

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
begin

   if Key = VK_BACK then begin

{------------------------------------------------------------------------------}
{---- Add Code to reformat the Input field when Backspace is pressed       ----}
{------------------------------------------------------------------------------}

{
      if edtKeyR.SelLength = Length(edtKeyR.Text) then
         edtKeyR.Text := ''
      else
         edtKeyR.Text := Copy(edtKeyR.Text,1,Length(edtKeyR.Text) - 1);

      edtKeyR.SelStart := Length(edtKeyR.Text);

      Key := 0;
}

   end;

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
      DaysLeft := DeCode();

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

      if adoQry2.FieldByName('LPMSKEy_AllowDuplicates').AsString = '1' then
         cbAllowDuplicates.Checked := True
      else
         cbAllowDuplicates.Checked := False;

      cbCollectU.Checked  := This_Key_Priv.LPMS_Collections;
      cbDocGenU.Checked   := This_Key_Priv.LPMS_DocGen;
      cbFloatingU.Checked := This_Key_Priv.LPMS_Floating;
      cbOption4U.Checked  := This_Key_Priv.LPMS_Option4;

      btnNew.Enabled    := False;
      btnCancel.Enabled := False;
      btnDelete.Enabled := True;
      btnUpdate.Enabled := False;

      ToolsGenerate.Enabled := True;
      ToolsEmail.Enabled    := True;

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

         Application.MessageBox('''Prefix'' is invalid.','LPMS Access Control Management',(MB_OK + MB_ICONSTOP));
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

         Application.MessageBox('''Prefix'' must be provided.','LPMS Access Control Management',(MB_OK + MB_ICONSTOP));
         edtPrefixC.SetFocus();
         Exit;

      end;

      if cbxKeyTypeC.ItemIndex < 1 then begin

         Application.MessageBox('''Key Type'' is invalid.','LPMS Access Control Management',(MB_OK + MB_ICONSTOP));
         cbxKeyTypeC.SetFocus();
         Exit;

      end;

      if Trim(edtCompanyC.Text) = '' then begin

         Application.MessageBox('''Company'' must be provided.','LPMS Access Control Management',(MB_OK + MB_ICONSTOP));
         edtCompanyC.SetFocus();
         Exit;

      end;


//--- If we get here then all information is there - do the Update

      if UpdateCpy() = True then
         Application.MessageBox('Update completed.','LPMS Access Control Management',(MB_OK + MB_ICONINFORMATION));

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

//--- If the Unique = '123456789ABC' then we have a floating license

      if edtUniqueU.Text <> '123456789ABC' then begin

        MsgPart := GetUnique(edtUniqueU.Text,ord(TYPE_TEST),Dummy);

         if MsgPart <> '' then begin

            if cbAllowDuplicates.Checked = False then begin

               Application.MessageBox(PChar('''Unique'' already exists for ' + MsgPart),'LPMS Access Control Management',(MB_OK + MB_ICONSTOP));

               if btnUnlock.Visible = True then
                  edtUniqueU.SetFocus()
               else
                  btnLock.SetFocus();

               Exit;

            end;

         end;

      end;

//--- Make sure that the required information is provided

      if Trim(edtUserNameU.Text) = '' then begin

         Application.MessageBox('''Username'' must be provided.','LPMS Access Control Management',(MB_OK + MB_ICONSTOP));
         edtUserNameU.SetFocus();
         Exit;

      end;

      if Trim(edtCompanyU.Text) = '' then begin

         Application.MessageBox('''Company'' must be provided.','LPMS Access Control Management',(MB_OK + MB_ICONSTOP));
         edtCompanyU.SetFocus();
         Exit;

      end;

      if Trim(edtEmailU.Text) = '' then begin

         Application.MessageBox('''Email'' must be provided.','LPMS Access Control Management',(MB_OK + MB_ICONSTOP));
         edtEmailU.SetFocus();
         Exit;

      end;

      if Trim(edtContactU.Text) = '' then begin

         Application.MessageBox('''Contact No'' must be provided.','LPMS Access Control Management',(MB_OK + MB_ICONSTOP));
         edtContactU.SetFocus();
         Exit;

      end;

      if Length(edtUniqueU.Text) <> 12 then begin

         Application.MessageBox('''Unique'' is invalid.','LPMS Access Control Management',(MB_OK + MB_ICONSTOP));
         edtUniqueU.SetFocus();
         Exit;

      end;

      if cbxLicTypeU.ItemIndex < 1 then begin

         Application.MessageBox('''License Type'' is invalid.','LPMS Access Control Management',(MB_OK + MB_ICONSTOP));
         cbxLicTypeU.SetFocus();
         Exit;

      end;

      if Length(edtKeyU.Text) <> 38 then begin

         Application.MessageBox('''Key'' is invalid.','LPMS Access Control Management',(MB_OK + MB_ICONSTOP));
         edtKeyU.SetFocus();
         Exit;

      end;

      if cbTransferU.Checked = True then begin

         if cbxNewLicU.ItemIndex = 0 then begin

            Application.MessageBox('''New Lic Type'' is invalid.','LPMS Access Control Management',(MB_OK + MB_ICONSTOP));
            cbxNewLicU.SetFocus();
            Exit;

         end;

         if cbxNewPrefixU.ItemIndex = 0 then begin

            Application.MessageBox('''New Prefix'' is invalid.','LPMS Access Control Management',(MB_OK + MB_ICONSTOP));
            cbxNewPrefixU.SetFocus();
            Exit;

         end;

      end;

//--- If we get here then all information is there - do the Update

      if UpdateUser() = True then
         Application.MessageBox('Update completed.','LPMS Access Control Management',(MB_OK + MB_ICONINFORMATION));

      tvTree.Selected.Text := edtUserNameU.Text;

   end else if pnlBackup.Visible = True then begin

//      DoBackup();

   end else if pnlRestore.Visible = True then begin

      if Trim(edtUserID.Text) = '' then begin

         Application.MessageBox('''UserID'' is a required field - please provide then try again.','LPMS Access Control Management',(MB_OK + MB_ICONSTOP));
         edtUserID.SetFocus();
         Exit;

      end;

      if Trim(edtPassword.Text) = '' then begin

         Application.MessageBox('''Password'' is a required field - please provide then try again.','LPMS Access Control Management',(MB_OK + MB_ICONSTOP));
         edtPassword.SetFocus();
         Exit;

      end;

      if Trim(edtHost.Text) = '' then begin

         Application.MessageBox('''Host'' is a required field - please provide then try again.','LPMS Access Control Management',(MB_OK + MB_ICONSTOP));
         edtHost.SetFocus();
         Exit;

      end;

//      DoRestore();

   end;

   if ((pnlBackup.Visible = False) and (pnlRestore.Visible = False)) then begin

      DoGen := False;
      StatusBar1.Panels.Items[2].Text := ' Updated';
      btnCancelClick(Sender);

   end;

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
   DaysLeft          := DeCode();

   if (DaysLeft = ord(ERR_INVALID) - 3) or (DaysLeft = ord(ERR_LENGTH) - 3) then begin

      Application.MessageBox('Unlock Key is invalid.','LPMS Access Control Management',(MB_OK + MB_ICONSTOP));
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
   cbOption4R.Checked    := This_Key_Priv.LPMS_Option4;
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

   ThisPass := InputQueryM('LPMS Access Control Management','Pass phrase:',ord(TYPE_PASSWORD));

   if ThisPass <> 'BlueCrane Software Development CC' then
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

   if EnCode() = True then begin

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
   ThisCount : integer;
   ThisKey   : string;

begin

   if CanUpdate = True then
      Exit;

   btnDecodeR.Enabled := False;
   btnEncodeR.Enabled := False;


   if edtKeyR.Focused() = True then begin

//--- If the backspace key was pressed then we don't do any processing here

{
      if BackSpace = True then begin

         BackSpace := False;
         Exit;

      end;
}

      ThisCount := Length(edtKeyR.Text);
      ThisKey   := edtKeyR.Text;

      if ThisCount in [4,8,13,18,23,28,33] then
         ThisKey := ThisKey + '-';

      if ThisCount in [6,10,15,20,25,30] then begin

         if Copy(edtKeyR.Text,ThisCount,1) = '-' then
            ThisKey := Copy(edtKeyR.Text, 1, ThisCount - 1);

      end;

      edtKeyR.Text := ThisKey;
      edtKeyR.SelStart := Length(edtKeyR.Text);

   end;


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
// Function to load the LPMS_EncDec DLL and decode a key contained in
// Decode_Key_Priv
//------------------------------------------------------------------------------
function TFLPMS_Main.DeCode() : integer;
type
  TMyFunc = function (Decode_Key_Priv : REC_Key_Priv) : REC_Key_Priv; stdcall;

var
   MyFunc           : TMyFunc;

begin

//--- Call and execute the DoDecode function in the DLL

   MyFunc := TMyFunc(GetProcedureAddress(FLPMS_Login.MyLibC, 'DoDecode'));

   This_Key_Priv := MyFunc(This_Key_Priv);

//--- Return the Result

   Result := This_Key_Priv.DaysLeft;

end;

//------------------------------------------------------------------------------
// Function to load the LPMS_EncDec DLL and encode a key with the information
// contained in Encode_Key_Values
//------------------------------------------------------------------------------
function TFLPMS_Main.EnCode() : boolean;
type
  TMyFunc = function (Encode_Key_Values : REC_Key_Values) : REC_Key_Values; stdcall;

var
   MyFunc : TMyFunc;

begin

//--- Call and execute the DoDecode function in the DLL

   MyFunc := TMyFunc(GetProcedureAddress(FLPMS_Login.MyLibC, 'DoEncode'));

   This_Key_Values := MyFunc(This_Key_Values);

//--- If the encoding was successful Unique will contain the Key

   if This_Key_Values.Unique = '000000000000' then
      Result := False
   else
      Result := True;

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

   btnLockB.Visible   := True;
   btnUnlockB.Visible := False;

   edtTable.Clear();
   edtRecord.Clear();
   btnCancel.SetFocus();

end;

//------------------------------------------------------------------------------
// User click on the Unlock button on the Backup screen
//------------------------------------------------------------------------------
procedure TFLPMS_Main.btnUnlockBClick(Sender: TObject);
begin

   btnUnlockB.Visible := False;
   btnLockB.Visible   := True;
   edtTable.Enabled   := False;
   edtRecord.Enabled  := False;
   btnUpdate.Enabled  := False;
   btnLockB.SetFocus();

end;

//------------------------------------------------------------------------------
// User click on the Lock button on the Backup screen
//------------------------------------------------------------------------------
procedure TFLPMS_Main.btnLockBClick(Sender: TObject);
var
   ThisPass : string;

begin

   ThisPass := InputQueryM('LPMS Access Control Management','Pass phrase:',ord(TYPE_PASSWORD));

   if ThisPass = 'BlueCrane Software Development CC' then begin

      btnUnlockB.Visible := True;
      btnLockB.Visible   := False;
      edtTable.Enabled   := True;
      edtRecord.Enabled  := True;
      btnUpdate.Enabled  := True;
      btnUpdate.SetFocus();

   end;

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

   if ThisPass = 'BlueCrane Software Development CC' then begin

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

   btnLockR.Visible   := True;
   btnUnlockR.Visible := False;

   edtUserID.Clear();
   edtPassword.Clear();
   edtHost.Clear();
   edtUserID.SetFocus();

end;

//------------------------------------------------------------------------------
// User clicked on the Unlock button on the Restore screen
//------------------------------------------------------------------------------
procedure TFLPMS_Main.btnUnlockRClick(Sender: TObject);
begin

   btnUnlockR.Visible  := False;
   btnLockR.Visible    := True;
   edtUserID.Enabled   := False;
   edtPassword.Enabled := False;
   edtHost.Enabled     := False;
   btnUpdate.Enabled   := False;
   edtUserID.SetFocus();

end;

//------------------------------------------------------------------------------
// User clicked on the Lock button on the Restore screen
//------------------------------------------------------------------------------
procedure TFLPMS_Main.btnLockRClick(Sender: TObject);
var
   ThisPass : string;

begin

   ThisPass := InputQueryM('LPMS Access Control Management','Pass phrase:',ord(TYPE_PASSWORD));

   if ThisPass = 'BlueCrane Software Development CC' then begin

      btnUnlockR.Visible  := True;
      btnLockR.Visible    := False;
      edtUserID.Enabled   := True;
      edtPassword.Enabled := True;
      edtHost.Enabled     := True;
      btnUpdate.Enabled   := True;
      edtUserID.SetFocus();

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

   if Pass <> 'BlueCrane Software Development CC' then begin

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

   if EnCode() = True then begin

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

end;

//------------------------------------------------------------------------------
// User clicked on the Send Email button
//------------------------------------------------------------------------------
procedure TFLPMS_Main.ToolsEmailExecute(Sender: TObject);
type
  TMyFunc = function (From, ToStr, CcStr, BccStr, Subject, Body, Attach, SMTPStr : string) : boolean; stdcall;

var
   MyFunc       : TMyFunc;

   From         : string;      // Email address of the Sender
   ToStr        : string;      // To addresses, comma delimited
   CcStr        : string;      // Cc addresses, comma delimited
   BccStr       : string;      // Bcc addresses, comma delimited
   Subject      : string;      // Email Subject
   Body         : string;      // Body of the email
   Attach       : string;      // '|' delimited string containing files to be attached
   SMTPStr      : string;      // '|' delimited string containing the SMTP parameters

begin

   if (pnlBackup.Visible = True) or (pnlRestore.Visible = True) then
      Exit;

   From    := 'registration@bluecrane.cc';
   ToStr   := edtEmailU.Text;
   CcStr   := '';
   BccStr  := 'registration@bluecrane.cc';
   Subject := 'LPMS Activation Key';
   Attach  := '';
   SMTPStr := edtServerR.Text + '|registration@bluecrane.cc|' + edtPasswordR.Text + '|';

   Body    := 'Dear ' + edtUserNameU.Text + ',|' +
              ' |' +
              'Attached below is your new/updated LPMS Activation Key which expires on ' + DateToStr(dtpExpiryDateU.Date) + ':|' +
              ' |' +
              '   ' + edtKeyU.Text + '|' +
              ' |' +
              'To activate LPMS with this key please do the following:|' +
              ' |' +
              '1.  Run LPMS_FirstRun.|' +
              '    1.1. Click on Start;|' +
              '    1.2. Click on Programs;|' +
              '    1.3. Click on BlueCrane Software; and|' +
              '    1.4. Click on LPMS_FirstRun|' +
              '2.  If your release is a multi-company release then:|' +
              '    2.1. Enter ''' + edtPrefixU.Text + ''' in the field next to ''DBPrefix:'';|' +
              '    2.2. Select ''Multi Company Support''; and|' +
              '    2.3. Click on ''Ok'' button to proceed|' +
              '3.  If your release is not a multi-company release simply click on the ''Ok'' button|' +
              '4.  Click on the ''Maintenance'' tab|' +
              '5.  Enter ''' + edtPrefixU.Text + ''' in the field next to ''Prefix:''|' +
              '6.  Copy the key above and paste in the field next to ''Key:''|' +
              '7.  Click on the ''Update'' button, then on the ''OK'' button and then on the ''Close'' button |' +
              '8.  Start LPMS. |' +
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

   MyFunc := TMyFunc(GetProcedureAddress(FLPMS_Login.MyLibC, 'SendMimeMail'));

   if MyFunc(From, ToStr, CcStr, BccStr, Subject, Body, Attach, SMTPStr) = False then
      Application.MessageBox('Sending Email failed! Please check Email set-up details.','LPMS Access Control Management',(MB_OK + MB_ICONSTOP))
   else
      Application.MessageBox('Send Email completed.','LPMS Access Control Management',(MB_OK + MB_ICONINFORMATION));

{

   if cbEditEmail.Checked = True then
      emlSend.Flags << sfDialog
   else
      emlSend.Flags >> sfDialog;

}

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

{
var
   Process: TProcess;
   I: Integer;

begin

   Process := TProcess.Create(nil);

   try
      Process.InheritHandles := False;
      Process.Options := [];
      Process.ShowWindow := swoShow;

// Copy default environment variables including DISPLAY variable for GUI application to work

      for I := 1 to GetEnvironmentVariableCount do
         Process.Environment.Add(GetEnvironmentString(I));

      Process.Executable := '/usr/bin/gedit';
      Process.Execute;
   finally
      Process.Free;
   end;

end;
}
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

end.

