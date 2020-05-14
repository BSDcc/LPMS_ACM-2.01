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
   ExtCtrls, ComCtrls, LCLType, FileInfo, DynLibs,

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

{$IFDEF DARWIN}                      // Target is macOS
   macOSAll, CarbonProc,
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

   { TFLPMS_Login }

   TFLPMS_Login = class( TForm)
   btnLogin: TButton;
   edtHostName: TEdit;
   edtPassword: TEdit;
   edtUserID: TEdit;
   Image1: TImage;
   Label1: TLabel;
   Label2: TLabel;
   Label3: TLabel;
   sqlQry1: TSQLQuery;
   sqlTran: TSQLTransaction;
   StaticText1: TStaticText;
   StatusBar1: TStatusBar;
   timTimer: TTimer;
   procedure btnLoginClick( Sender: TObject);
   procedure FormClose( Sender: TObject; var CloseAction: TCloseAction);
   procedure FormCreate( Sender: TObject);
   procedure FormShow( Sender: TObject);
   procedure timTimerTimer( Sender: TObject);

private  { Private Declarations }

   Major      : string;     // Major Version component of the Version info
   Minor      : string;     // Minor Version component of the Version info
   VerRelease : string;     // Release component of the Version info
   Build      : string;     // Build Number component of the Version info

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

   function  cmdlOpt(OptList : string; Options, Parms : TStringList) : integer;
   function  DoLogin() : boolean;
   procedure GetVersion();

public   { Public Declarations }

   LoginCount                                           : integer;
   AutoLogin                                            : boolean;
   CopyRight, UserName, Password, Version, DTDLocation  : string;
   AutoUser, AutoPass, AutoHost, Path                   : string;
   MyLibC                                               : TLibHandle;

end;

//------------------------------------------------------------------------------
// Global variables
//------------------------------------------------------------------------------
var
   FLPMS_Login: TFLPMS_Login;

implementation

   uses LPMS_Main;

{$R *.lfm}

{ TFLPMS_Login }

//------------------------------------------------------------------------------
// Executed after the form is created
//------------------------------------------------------------------------------
procedure TFLPMS_Login.FormCreate(Sender: TObject);
var
   idx             : integer;
   DLLName         : string;
   Options, Params : TStringList;

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
   {$IFDEF CPUI386}                 // Running on a version below Catalina
      sqlCon := TMySQL55Connection.Create(nil);
   {$ELSE}
      sqlCon := TMySQL57Connection.Create(nil);
   {$ENDIF}
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

   AutoLogin := False;

//--- Check whether any paramters were passed and retrieve if so

   Options := TStringList.Create;
   Params  := TStringList.Create;

   if cmdlOpt('u:p:H:',Options,Params) > 0 then begin

      for idx := 0 to Options.Count - 1 do begin

         if Options.Strings[idx] = 'u' then begin

            AutoUser  := Params.Strings[idx];
            AutoLogin := true;

         end;

         if Options.Strings[idx] = 'p' then begin

            AutoPass  := Params.Strings[idx];
            AutoLogin := true;

         end;

         if Options.Strings[idx] = 'H' then begin

            AutoHost  := Params.Strings[idx];
            AutoLogin := true;

         end;

      end;

   end;

   Options.Free;
   Params.Free;

//--- Load the DLL used to do Encoding and Decoding of Keys

   MyLibC := DynLibs.NilHandle;
   Path   := ExtractFilePath(Application.ExeName);

{$IFDEF WINDOWS}
   DLLName := 'LPMS_EncDec.';
{$ELSE}
   DLLName := 'liblpms_encdec.';
{$ENDIF}

   MyLibC := LoadLibrary(Path + DLLName + SharedSuffix);

//--- Check whether the DLL was loaded successfully

   if MyLibC = DynLibs.NilHandle then begin

      Application.MessageBox('FATAL: Unexpected/Unhandled error: ''Unable to load required Dynamic Load Library'' - LPMS_ACM cannot continue.','LPMS Access Control Management',(MB_OK + MB_ICONSTOP));
      Application.Terminate;
      Exit;

   end;

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
begin

//--- Unload the Encode/Decode DLL if it was loaded successfully

      if MyLibC <>  DynLibs.NilHandle then
         if FreeLibrary(MyLibC) then
            MyLibC := DynLibs.NilHandle;

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
procedure TFLPMS_Login. btnLoginClick( Sender: TObject);
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

      FLPMS_Main.UserName  := edtUserID.Text;
      FLPMS_Main.Password  := edtPassword.Text;
      FLPMS_Main.HostName  := edtHostName.Text;
      FLPMS_Main.Version   := Version;
      FLPMS_Main.CopyRight := CopyRight;

      FLPMS_Main.ShowModal();
      FLPMS_Main.Destroy;
      FLPMS_Login.Show();

      Close();

   end;

end;

//---------------------------------------------------------------------------
// Function to connect to the database. If the connection succeeds then the
// supplied UserID, Password and HostName is valid
//---------------------------------------------------------------------------
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
// Function to extract and return the parameters that were passed on the
// command line when the application was invoked.
//
// An Option list of "H:L:mu:" would expect a command line similar to:
//   -H followed by a parameter e.g. -Hwww.sourcingmethods.com
//   -L followed by a parameter e.g. -L1
//   -m
//   -u followed by a parameter e.g. -uFrancois
//
// The calling function must create the following TStringList variables:
//
//   Options
//   Parms
//
// The function returns:
//
//    0 if no command line parameters were passed
//    $ in the Parms string if a value was expected but not found
//    # in the Parms string if an unknown parameter was found
//   -1 if the switch '-' could not be found
//   The number of parameters found if no error were found
//---------------------------------------------------------------------------
function TFLPMS_Login.cmdlOpt(OptList : string; Options, Parms : TStringList) : integer;
var
   idx1, idx2           : integer;
   Found                : boolean;
   ThisParm, ThisOption : string;

begin

   if ParamCount < 1 then begin

      Result := 0;
      Exit;

   end;

//--- Extract the parameters that were passed from ParamStr

   for idx1 := 1 to ParamCount do begin

      ThisParm := ParamStr(idx1);

//--- First character of the argument must be the switch character ('-')

      if ThisParm.SubString(0,1) <> '-' then begin

         Result := -1;
         Exit;

      end;

//--- Extract the second character and search for it in OptList

      ThisOption := ThisParm.SubString(1,1);
      Found := False;

      for idx2 := 0 to OptList.Length do begin

         if OptList.SubString(idx2,1) = ThisOption then begin

            Found := True;
            Options.Add(ThisOption);

//--- If this Option is followed by ":" in the OptList then a parameter is
//    expected. Extract the parameter if it is expected

            if OptList.SubString(idx2 + 1,1) = ':' then begin

               if ThisParm.Length < 3 then
                  Parms.Add('$')
               else
                  Parms.Add(ThisParm.SubString(2, ThisParm.Length - 1));

            end else
               Parms.Add('$');

         end;

      end;

      if Found = False then begin
         Options.Add(ThisOption);
         Parms.Add('#');
      end;

   end;

   Result := Parms.Count;

end;

//------------------------------------------------------------------------------

end.

