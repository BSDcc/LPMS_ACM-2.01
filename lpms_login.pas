//------------------------------------------------------------------------------
// Date.......: 10 May 2020
// System.....: LPMS Access Control Manager
// Program ID.: LPMS_Login
// Platform...: Lazarus (Winblows, Linux, Raspbian & macOS)
// Author.....: Francois De Bruin Meyer (BlueCrane Software Development CC)
//------------------------------------------------------------------------------
// History....: 10 May 2020 - Adapt from LPMS C++ version
//------------------------------------------------------------------------------

unit LPMS_Login;

{$mode objfpc}{$H+}

interface

//------------------------------------------------------------------------------
// Uses clause
//------------------------------------------------------------------------------
uses
{$IFDEF UNIX}
   {$IFDEF UseCThreads}
      cthreads,
   {$ENDIF}
{$ENDIF}

   Classes, SysUtils, sqldb, Forms, Controls, Graphics, Dialogs, StdCtrls,
   ExtCtrls, ComCtrls, LCLType, FileInfo, INIFiles, LazFileUtils,

{$IFDEF DARWIN}                      // Target is macOS
   Zipper, StrUtils, DateUtils, SMTPSend, MimeMess, MimePart, SynaUtil,
   macOSAll,
  {$IFDEF CPUI386}                   // Running on old hardware i.e. i386 - Widget set must be Carbon
      CarbonProc, mysql55conn, Interfaces;
   {$ELSE}                           // Running on X86_64 hardware - Widget set must be Cocoa
      CocoaUtils, mysql57conn, Interfaces;
   {$ENDIF}
{$ENDIF}

{$IFDEF WINDOWS}                     // Target is Winblows
   winpeimagereader, mysql56conn;
{$ENDIF}

{$IFDEF LINUX}                       // Target is Linux
   elfreader,
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

   { TFLPMS_Login }

   TFLPMS_Login = class( TForm)
   btnLogin: TButton;
   edtHostName: TEdit;
   edtPassword: TEdit;
   edtUserID: TEdit;
   Image1: TImage;
   jvBrowse: TSelectDirectoryDialog;
   Label1: TLabel;
   Label2: TLabel;
   Label3: TLabel;
   StaticText1: TStaticText;
   StatusBar1: TStatusBar;
   timTimer: TTimer;
   procedure btnLoginClick( Sender: TObject);
   procedure FormClose( Sender: TObject; var CloseAction: TCloseAction);
   procedure FormCreate( Sender: TObject);
   procedure FormShow( Sender: TObject);
   procedure timTimerTimer( Sender: TObject);

{$IFDEF DARWIN}
{$INCLUDE '../BSD_Utilities/BSD_Utilities_01.inc'}
{$ENDIF}

private  { Private Declarations }

   Major      : string;     // Major Version component of the Version info
   Minor      : string;     // Minor Version component of the Version info
   VerRelease : string;     // Release component of the Version info
   Build      : string;     // Build Number component of the Version info
   INILoc     : string;     // Location of the INI file
   LocalPath  : string;     // Path to location of the INI file



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

{$IFDEF DARWIN}
{$INCLUDE '../BSD_Utilities/BSD_Utilities_02.inc'}
{$ENDIF}

   function  DoLogin() : boolean;
   procedure GetVersion();

public   { Public Declarations }

   LoginCount, DBBlock                                 : integer;
   AutoLogin                                           : boolean;
   CopyRight, UserName, Password, Version, DTDLocation : string;
   AutoUser, AutoPass, AutoHost, Path, ThisSMTPHost    : string;
   ThisSMTPPass, DBUser, DBPass, DBHost, DBLocation    : string;
   DBTemplate                                          : string;

type

{$IFNDEF DARWIN}

   MASK_TYPES   = (MA_MASK,          // Encode the input field
                   MA_UNMASK);       // Decode the input field

{$ENDIF}

end;

{$IFNDEF DARWIN}

//------------------------------------------------------------------------------
// Global variables
//------------------------------------------------------------------------------
var
   FLPMS_Login: TFLPMS_Login;

{$IFDEF WINDOWS}
   function  cmdlOptions(OptList : string; CmdLine, ParmStr : TStringList): integer; cdecl; external 'BSD_Utilities';
   function  MaskField(InputField: string; MaskType: integer): string; cdecl; external 'BSD_Utilities';
{$ELSE}
   function  cmdlOptions(OptList : string; CmdLine, ParmStr : TStringList): integer; cdecl; external 'libbsd_utilities';
   function  MaskField(InputField: string; MaskType: integer): string; cdecl; external 'libbsd_utilities';
{$ENDIF}

implementation

   uses LPMS_Main;

{$R *.lfm}

{$ElSE}

//------------------------------------------------------------------------------
// Global variables
//------------------------------------------------------------------------------
var
   FLPMS_Login: TFLPMS_Login;

implementation

   uses LPMS_Main;

{$R *.lfm}

{$DEFINE LPMS_ACM_Login}
{$INCLUDE '../BSD_Utilities/BSD_Utilities.lpr'}
{$UNDEF LPMMS_ACM_Login}

{$ENDIF}

{ TFLPMS_Login }

//------------------------------------------------------------------------------
// Executed after the form is created
//------------------------------------------------------------------------------
procedure TFLPMS_Login.FormCreate(Sender: TObject);
var
   idx, NumParms  : integer;
   Params, Args   : TStringList;
   IniFile        : TINIFile;

begin

   AutoLogin := False;

{$IFDEF WINDOWS}                    // Target is Winblows
   sqlCon  := TMySQL56Connection.Create(nil);
   sqlTran := TSQLTransaction.Create(nil);
   sqlQry1 := TSQLQuery.Create(nil);
   sqlQry2 := TSQLQuery.Create(nil);
{$ENDIF}

{$IFDEF LINUX}                      // Target is Linux
   {$IFDEF CPUARMHF}                // Running on ARM (Raspbian) architecture
      sqlCon  := TMySQL55Connection.Create(nil);
   {$ELSE}                          // Running on Intel architecture
      sqlCon  := TMySQL57Connection.Create(nil);
   {$ENDIF}
   sqlTran := TSQLTransaction.Create(nil);
   sqlQry1 := TSQLQuery.Create(nil);
   sqlQry2 := TSQLQuery.Create(nil);
{$ENDIF}

{$IFDEF DARWIN}                     // Target is macOS
   {$IFDEF CPUI386}                 // Running on a version below Catalina
      sqlCon := TMySQL55Connection.Create(nil);
   {$ELSE}
      sqlCon := TMySQL57Connection.Create(nil);
   {$ENDIF}
   sqlTran := TSQLTransaction.Create(nil);
   sqlQry1 := TSQLQuery.Create(nil);
   sqlQry2 := TSQLQuery.Create(nil);
{$ENDIF}

   sqlTran.DataBase    := sqlCon;
   sqlQry1.Transaction := sqlTran;

   LoginCount := 0;
   CopyRight  := '© 2008-' + FormatDateTime('YYYY',Now()) + ' BlueCrane Software Development CC';

   GetVersion();

{$IFOPT D+}
   Version := 'Version ' + Major + '.' + Minor + '.' + VerRelease + ' [DEBUG]';
{$ELSE}
   Version := 'Version ' + Major + '.' + Minor + '.' + VerRelease + ' [' + Build + ']';
{$ENDIF}

//--- Check whether any paramters were passed and retrieve if so

   try

      Params  := TStringList.Create;
      Args    := TStringList.Create;

      for idx := 1 to ParamCount do
         Args.Add(ParamStr(idx));

//--- Call and execute the cmdlOptions function in the BSD_Utilities DLL

      NumParms := cmdlOptions('u:p:H:', Args, Params);

      if NumParms > 0 then begin

         idx      := 0;
         NumParms := NumParms * 2;

         while idx < Params.Count do begin

            if Params.Strings[idx] = 'u' then begin

               AutoUser  := Params.Strings[idx + 1];
               AutoLogin := true;

            end;

            if Params.Strings[idx] = 'p' then begin

               AutoPass  := Params.Strings[idx + 1];
               AutoLogin := true;

            end;

            if Params.Strings[idx] = 'H' then begin

               AutoHost  := Params.Strings[idx + 1];
               AutoLogin := true;

            end;

            idx := idx + 2;

         end;

      end;

   finally

      Params.Destroy;
      Args.Free;

   end;

//--- Set the location of the INI file. We get the path to the user's home
//--- directory (this is platform independent). Winblows is a problem due to a
//--- lack of naming conventions across versions of Winblows. If it is not
//--- 'Documents' or 'My Documents' then we give the User a change to select
//--- the home directory.

{$IFDEF WINDOWS}

   LocalPath := AppendPathDelim(GetUserDir + 'Documents');

   if DirectoryExists(LocalPath) = False then begin

      LocalPath := AppendPathDelim(GetUserDir + 'My Documents');

      if DirectoryExists(LocalPath) = False then begin

         if (MessageDlg('LPMS Access Control Management','WARNING: Unable to locate home directory. You can:' + #10 + #10 + #10 + 'Click [Yes] to locate the home directory; or ' + #10 +#10 + 'Click [No] to terminate.', mtWarning, [mbYes,mbNo], '') = mrNo) then begin;

            Application.Terminate;
            Exit;

         end;


         if jvBrowse.Execute = False then begin

            Application.Terminate;
            Exit;

         end;

      end;

   end;

   LocalPath := AppendPathDelim(LocalPath + 'LPMS_ACM');

{$ELSE}

   LocalPath := AppendPathDelim(GetUSerDir);
   LocalPath := AppendPathDelim(LocalPath + '.lpms_acm');

{$ENDIF}

//--- We now have what passes for a home directory with the working directory
//--- 'LPMS_ACM' (Winblows) or '.lpms_acm' (*nix) added to it and tests whether
//--- this exists. If it does not then we ask the User whether we should create
//--- it and do so if the User agrees otherwise we terminate the Application

   if DirectoryExists(LocalPath) = False then begin

      if (MessageDlg('LPMS Access Control Management','WARNING: LPMS_ACM directory does not exist. You can:' + #10 + #10 + #10 + 'Click [Yes] to create the directory; or' +#10 + #10 + 'Click [No] to terminate.', mtWarning, [mbYes,mbNo], '') = mrNo) then begin;

         Application.Terminate;
         Exit;

      end;

      if CreateDir(LocalPath) = False then begin

         MessageDlg('LPMS Access Control Management','FATAL: Unable to create LPMS_ACM directory.' + #10 + #10 + 'LPMS_ACM cannot continue and will be terminated.', mtError, [mbOk], '');
         Application.Terminate;
         Exit;

      end;

   end;


//--- Get the SMTP parameters from the INI file and store for later use

   INILoc := LocalPath + 'LPMS_ACM.ini';

   if FileExists(INILoc) = True then begin

      IniFile := TINIFile.Create(INILoc);

      ThisSMTPHost := IniFile.ReadString('Config','SMTPHost','');
      ThisSMTPPass := IniFile.ReadString('Config','SMTPPass','');
      DBUser       := IniFile.ReadString('Config','DBuser','');
      DBPass       := IniFile.ReadString('Config','DBPass','');
      DBHost       := IniFile.ReadString('Config','DBHost','');
      DBLocation   := IniFile.ReadString('Config','DBLocation','');
      DBTemplate   := IniFile.ReadString('Config','DBTemplate','&Date@&Time - &BackupType Backup for &BackupName (&DBName on &HostName) {&OSShort}');
      DBBlock      := IniFile.ReadInteger('Config','DBBlock',20000);

      IniFile.Destroy;

   end;

//--- Unmask the Password

   ThisSMTPPass := MaskField(ThisSMTPPass,ord(MA_UNMASK));

end;

//------------------------------------------------------------------------------
// Executed when the form becomes visible
//------------------------------------------------------------------------------
procedure TFLPMS_Login.FormShow(Sender: TObject);
begin

   FLPMS_Login.Caption := 'LPMS_ACM Login';
   StatusBar1.Panels.Items[0].Text := '  ' + Version;

   if AutoLogin = True then begin

      edtUserID.Text   := AutoUser;
      edtPassword.Text := AutoPass;
      edtHostName.Text := AutoHost;
      timTimer.Enabled := True;

   end else
      edtUserID.SetFocus();

end;

//------------------------------------------------------------------------------
// Executed when the Form is finally closed
//------------------------------------------------------------------------------
procedure TFLPMS_Login. FormClose( Sender: TObject; var CloseAction: TCloseAction);
var
   IniFile   : TINIFile;

begin

//--- Mask the SMTP Password before it is written to the INI File

   ThisSMTPPass := MaskField(ThisSMTPPass,ord(MA_MASK));

//--- Write the current SMTP parameters to the INI file

   INILoc := LocalPath + 'LPMS_ACM.ini';

   IniFile := TINIFile.Create(INILoc);

   IniFile.WriteString('Config','SMTPHost',ThisSMTPHost);
   IniFile.WriteString('Config','SMTPPass',ThisSMTPPass);
   IniFile.WriteString('Config','DBuser',DBUser);
   IniFile.WriteString('Config','DBPass',DBPass);
   IniFile.WriteString('Config','DBHost',DBHost);
   IniFile.WriteString('Config','DBLocation',DBLocation);
   IniFile.WriteString('Config','DBTemplate',DBTemplate);
   IniFile.WriteInteger('Config','DBBlock',DBBlock);

   IniFile.Destroy;

end;

//------------------------------------------------------------------------------
// Executed when timTimer pops
//------------------------------------------------------------------------------
procedure TFLPMS_Login. timTimerTimer( Sender: TObject);
begin

   timTimer.Enabled := False;
   btnLoginClick(Sender);

end;

//------------------------------------------------------------------------------
// Executed when the User clicks on the Login button
//------------------------------------------------------------------------------
procedure TFLPMS_Login.btnLoginClick(Sender: TObject);
begin

   Inc(LoginCount);

   if LoginCount > 3 then begin

      StatusBar1.Panels.Items[0].Text := ' Login count exceeded...';
      btnLogin.Enabled := False;
      Exit;
   end;

   if Trim(edtUserID.Text) = '' then begin

      StatusBar1.Panels.Items[0].Text := ' User ID is a required input field...';
      edtUserID.SetFocus();
      Exit;

   end;

   if Trim(edtPassword.Text) = '' then begin

      StatusBar1.Panels.Items[0].Text := ' Password is a required input field...';
      edtPassword.SetFocus();
      Exit;

   end;

   if Trim(edtHostName.Text) = '' then begin

      StatusBar1.Panels.Items[0].Text := ' Host is a required input field...';
      edtHostName.SetFocus();
      Exit;

   end;

   if DoLogin() = False then begin

      StatusBar1.Panels.Items[0].Text := ' Login attempt failed (' + IntToStr(LoginCount) + ') ...';
      edtUserID.SetFocus();
      Exit;

   end else begin

      FLPMS_Login.Hide();
      FLPMS_Main := TFLPMS_Main.Create(Application);

//--- Set up some values on the Main form

      FLPMS_Main.UserName     := edtUserID.Text;
      FLPMS_Main.Password     := edtPassword.Text;
      FLPMS_Main.HostName     := edtHostName.Text;
      FLPMS_Main.Version      := Version;
      FLPMS_Main.CopyRight    := CopyRight;
      FLPMS_Main.ThisSMTPHost := ThisSMTPHost;
      FLPMS_Main.ThisSMTPPass := ThisSMTPPass;

      FLPMS_Main.edtUserIDB.Text     := DBUSer;
      FLPMS_Main.edtPasswordB.Text   := DBPass;
      FLPMS_Main.speReadBlockB.Value := DBBlock;
      FLPMS_Main.edtHostNameB.Text   := DBHost;
      FLPMS_Main.edtLocationB.Text   := DBLocation;
      FLPMS_Main.edtTemplateB.Text   := DBTemplate;

//--- Call the Main form

      FLPMS_Main.ShowModal();

//--- Retrieve the Backup value from the Main Form in case they were changed

      DBUser     := FLPMS_Main.edtUserIDB.Text;
      DBPass     := FLPMS_Main.edtPasswordB.Text;
      DBBlock    := FLPMS_Main.speReadBlockB.Value;
      DBHost     := FLPMS_Main.edtHostNameB.Text;
      DBLocation := FLPMS_Main.edtLocationB.Text;
      DBTemplate := FLPMS_Main.edtTemplateB.Text;

//--- Terminate

      FLPMS_Main.Destroy;
      FLPMS_Login.Show();

      Close();

   end;

end;

//------------------------------------------------------------------------------
// Function to connect to the database. If the connection succeeds then the
// supplied UserID, Password and HostName is valid
//------------------------------------------------------------------------------
function TFLPMS_Login.DoLogin() : boolean;
var
   S1 : string;

begin

   S1 := 'SELECT LPMSKey_Prefix FROM companies';

   sqlQry1.Close();
   sqlCon.Close();

   sqlCon.HostName     := edtHostName.Text;
   sqlCon.UserName     := edtUserID.Text;
   sqlCon.Password     := edtPassword.Text;
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

   sqlQry1.Close();
   sqlCon.Close();

   Result := True;

end;

//------------------------------------------------------------------------------
// Procedure to extract the version info from the Application
//------------------------------------------------------------------------------
procedure TFLPMS_Login.GetVersion();
var

{$IFDEF DARWIN}
   BundleRef     : CFBundleRef;
   KeyRef        : CFStringRef;
   ValueRef      : CFTypeRef;
{$ENDIF}

   VersionString : string;
   VersionTokens : TStringList;
   FileVerInfo   : TFileVersionInfo;

begin

{$IFDEF DARWIN}

   try

      BundleRef := CFBundleGetMainBundle;

      if BundleRef = nil then
         Exit;

      KeyRef   := CFStringCreateWithPascalString(nil,'CFBundleVersion',kCFStringEncodingUTF8);
      ValueRef := CFBundleGetValueForInfoDictionaryKey(BundleRef, KeyRef);

      if ValueRef = nil then
         Exit;

      if CFGetTypeID(ValueRef) <> CFStringGetTypeID then
         Exit;

      VersionString := CFStringToStr(ValueRef);

   except on E : Exception do

      ShowMessage(E.Message);

   end;

   FreeCFString(KeyRef);

   VersionTokens := TStringList.Create;
   FileVerInfo   := TFileVersionInfo.Create(nil);

   ExtractStrings(['.'], [], PChar(VersionString), VersionTokens);

   Major      := Copy(VersionTokens[0],13,99);
   Minor      := VersionTokens[1];
   VerRelease := VersionTokens[2];
   Build      := VersionTokens[3];

   VersionTokens.Free;
   FileVerInfo.Free;

{$ELSE}

   VersionTokens := TStringList.Create;
   FileVerInfo   := TFileVersionInfo.Create(nil);

   try

     FileVerInfo.ReadFileInfo;

     VersionString := FileVerInfo.VersionStrings[3];
     ExtractStrings(['.'], [], PChar(VersionString), VersionTokens);

     Major      := Copy(VersionTokens[0],13,99);
     Minor      := VersionTokens[1];
     VerRelease := VersionTokens[2];
     Build      := VersionTokens[3];

   finally

     VersionTokens.Free;
     FileVerInfo.Free;

   end;

{$ENDIF}

end;

//------------------------------------------------------------------------------

end.

