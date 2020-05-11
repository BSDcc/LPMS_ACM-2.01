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
   Process,

{$IFDEF WINDOWS}                     // Target is Winblows
   mysql56conn;
{$ENDIF}

{$IFDEF LINUX}                       // Target is Linux
   {$IFDEF CPUARMHF}                 // Running on ARM (Raspbian) architecture
      mysql55conn;
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
   edtUniqueR: TEdit;
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
   procedure ActionsBackupExecute( Sender: TObject);
   procedure ActionsRefreshExecute( Sender: TObject);
   procedure ActionsRestoreExecute( Sender: TObject);
   procedure btnLockBClick( Sender: TObject);
   procedure btnLockRClick( Sender: TObject);
   procedure btnUnlockBClick( Sender: TObject);
   procedure btnUnlockRClick( Sender: TObject);
   procedure edtKeyRKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
   procedure FileExitExecute( Sender: TObject);
   procedure FormClose( Sender: TObject; var CloseAction: TCloseAction);
   procedure FormCreate( Sender: TObject);
   procedure FormShow( Sender: TObject);
   procedure ToolsEmailExecute( Sender: TObject);
   procedure ToolsExportExecute( Sender: TObject);
   procedure ToolsGenerateExecute( Sender: TObject);

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
   ConStr1, ConStr2                   : string;
   Root                               : TTreeNode;

   Registered, BackSpace : boolean;

   procedure OpenDB(ThisType: integer);
   function  GetData(ThisType: integer; Company, User, Unique: string) : boolean;
   function  InputQueryM(ThisCap, Question : string; DispType: integer) : string;
   function  MySQLAccess(ThisType: integer; S1 : string; adoQry: TSQLQuery) : boolean;
   procedure MySQLAbort(Msg: string);

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
              TYPE_XFER);          //

{
//--- Used for trapping the BackSpace Key

   function WMHotKey(Message : TWMHotKey) : MESSAGE;
}
{
   uses
     Messages, Windows;
   ...
     // Your main form's class
     protected
       procedure WMHotKey(var Message: TMessage); message WM_HOTKEY;
   ...
   implementation
   ...
   procedure TMainForm.WMHotKey(var Message: TMessage);
   begin
     // This example brings the application up front. Put your code here
     Application.BringToFront;
   end;

---

   RegisterHotKey(Handle, 100000 { Any unused number}, MOD_CONTROL, VK_F7);
   // When closing program
   UnregisterHotKey(Handle, 100000);

--- C++

BEGIN_MESSAGE_MAP
VCL_MESSAGE_HANDLER(WM_HOTKEY, TWMHotKey, WMHotKey)
END_MESSAGE_MAP(TForm)

}

public   { Public Declarations }

   UserName    : string;      // UserName passed from Login
   Password    : string;      // Password passed from Login
   HostName    : string;      // Host name passed from Login
   Version     : string;      // Version string passed from Login
   CopyRight   : string;      // Copyright notice passed from Login
   ThisRes     : string;      // Holds result from InoutQuery
   ThisName    : string;      //
   ThisCompany : string;      //
   ThisUnique  : string;      //

end;

//------------------------------------------------------------------------------
// Global variables
//------------------------------------------------------------------------------
var
   FLPMS_Main: TFLPMS_Main;

implementation

   uses LPMS_InputQuery;

{$R *.lfm}

   { TFLPMS_Main }

//------------------------------------------------------------------------------
// Executed when the Form is created
//------------------------------------------------------------------------------
procedure TFLPMS_Main. FormCreate( Sender: TObject);
begin

  DoSave     := False;
  CanUpdate  := False;
  DoGen      := False;
  BackSpace  := False;
  Registered := False;
  FirstRun   := True;

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
procedure TFLPMS_Main. FormShow( Sender: TObject);
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

   dtpExpiryR.Date := Now();
   edtKeyR.SetFocus();

end;

//------------------------------------------------------------------------------
// User clicked on the Exit button
//------------------------------------------------------------------------------
procedure TFLPMS_Main. FileExitExecute( Sender: TObject);
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

//---------------------------------------------------------------------------
// User click on the Unlock button on the Backup screen
//---------------------------------------------------------------------------
procedure TFLPMS_Main.btnUnlockBClick(Sender: TObject);
begin

   btnUnlockB.Visible := False;
   btnLockB.Visible   := True;
   edtTable.Enabled   := False;
   edtRecord.Enabled  := False;
   btnUpdate.Enabled  := False;
   btnLockB.SetFocus();

end;

//---------------------------------------------------------------------------
// User click on the Lock button on the Backup screen
//---------------------------------------------------------------------------
procedure TFLPMS_Main.btnLockBClick(Sender: TObject);
var
   ThisPass : UnicodeString;

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
   ThisPass : UnicodeString;

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
begin

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
begin

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

                  MySQLAbort(Err.Message);
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

                  MySQLAbort(Err.Message);
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

//---------------------------------------------------------------------------
// Function to handle MySQL connection problems
//---------------------------------------------------------------------------
procedure TFLPMS_Main.MySQLAbort(Msg: string);
var
   idx     : integer;
   Process : TProcess;

begin

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
   Close;

end;

{
var
   ThisParmStr, ThisExe : string;
//   ReturnValue          : HINSTANCE;

begin

   Application.MessageBox(PChar('Unexpected Data Base error: ''' + Msg + ''' - LPMS_ACM cannot continue and will be restarted'),'LPMS Access Control Management',(MB_OK + MB_ICONSTOP));

   ThisExe := Application.ExeName;

//--- Build the parm string then terminate this instance and restart LPMS

   ThisParmStr := '-u"' + UserName + '" -p"' + Password + '" -H"' + HostName + '"';

   if ShellExecute(Application, 'open', ThisExe, ThisParmStr, nil, SW_SHOWNORMAL) < 33 then
      Application.MessageBox(PChar('Failed to invoke ''' + ThisExe + ''),'LPMS Access Control Manager',(MB_OK + MB_ICONSTOP));

   Application.Terminate();

   Exit;
}

//------------------------------------------------------------------------------

end.

