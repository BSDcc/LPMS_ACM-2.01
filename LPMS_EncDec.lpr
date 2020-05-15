//------------------------------------------------------------------------------
// Date.......: 12 May 2020
// System.....: LPMS Access Control Manager
// Program ID.: LPMS_EncDec - Platform independent DLL
// Platform...: Lazarus (Winblows, Linux, Raspbian & macOS)
// Author.....: Francois De Bruin Meyer (BlueCrane Software Development CC)
//------------------------------------------------------------------------------

library LPMS_EncDec;

{$mode objfpc}{$H+}

//------------------------------------------------------------------------------
// Uses clause
//------------------------------------------------------------------------------
uses
   Classes, DateUtils, SysUtils, SMTPSend, MimeMess, MimePart, SynaUtil;

//------------------------------------------------------------------------------
// Define the ENUM Types and Record structures
//------------------------------------------------------------------------------
type

   LIC_LICENSE  = (LIC_INVALID,      // Used as a place holder
                   LIC_TRIAL,        // Generate a Trial License
                   LIC_PERSONAL,     // Generate a normal production license
                   LIC_BROWSE,       // Generate a Browse only license
                   LIC_GENERIC);     // Generate a non Legal Firm license

   RES_RESULTS  = (ERR_INVALID,      // The supplied license key is invalid
                   ERR_LENGTH,       // The length of the supplied license key is wrong
                   ERR_EXPIRED);     // The supplied license key has expired

   REC_Key_Priv = record             // DoDeCode - Populated with data from a supplied license key
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

   REC_Key_Values = record           // DoEncode - Required fields to generate a license key
      Unique           : string;     // If successful the License Key is stored in Unique
      ExpDate          : string;
      DBPrefix         : string;
      LPMS_Collections : boolean;
      LPMS_DocGen      : boolean;
      LPMS_Floating    : boolean;
      LPMS_Options4    : boolean;
      License          : integer;
   end;

   REC_Key_Chars = record            // Overlay giving access to individual characters in the License Key
      ExpDateYL   : char;
      ExpDateYR   : char;
      ExpDateM    : char;
      ExpDateDL   : char;
      ExpDateDR   : char;
      DBPrefix01L : char;
      DBPrefix02L : char;
      DBPrefix03L : char;
      Switch01    : char;
      DBPrefix03R : char;
      DBPrefix05  : char;
      DBPrefix06  : char;
      Unique01    : char;
      Unique02    : char;
      Unique03    : char;
      Unique04    : char;
      Unique05    : char;
      Unique06    : char;
      Unique07    : char;
      Unique08    : char;
      Unique09    : char;
      Unique10    : char;
      Unique11    : char;
      Unique12    : char;
      Range       : char;
      CheckSum1   : char;
      CheckSum2   : char;
      DBPrefix01R : char;
      DBPrefix02R : char;
      Switch02    : char;
      DBPrefix04  : char;
   end;

   REC_Key_Fields = record           // Overlay giving access to individual fields in the License Key
      ExpDate     : array[1..5] of char;
      DBPrefix01L : char;
      DBPrefix02L : char;
      DBPrefix03L : char;
      Switch01    : char;
      DBPrefix03R : char;
      DBPrefix05  : char;
      DBPrefix06  : char;
      Unique      : array[1..12] of char;
      Range       : char;
      CheckSum1   : char;
      CheckSum2   : char;
      DBPrefix02R : char;
      Switch02    : char;
      DBPrefix04  : char;
   end;

   REC_Key_String = record           // Overlay giving access to the License Key as a whole string
      Key : array[1..31] of char;
   end;

   REC_Key_Overlay = record          // Defines the above three overlays to contain the same storage space
      case integer of
         0: (Strings : REC_Key_String);
         1: (Chars   : REC_Key_Chars);
         2: (Fields  : REC_Key_Fields);
   end;

//==============================================================================
// Various Support functions
//==============================================================================

//==============================================================================
// End of Support Functions
// Start of callable DLL functions
//==============================================================================

//------------------------------------------------------------------------------
// Function to decode a key contained in the parameter passed and return a
// fully populated REC_Key_Priv structure
//------------------------------------------------------------------------------
function DoDecode(Decode_Key_Priv: REC_Key_Priv): REC_Key_Priv; stdcall;
const
   KeyLength : integer = 38;
   KeyShort  : integer = 31;
   UniqueLen : integer = 12;

   KeySet1 : array[1..31] of integer = (5,9,1,14,31,29,27,20,22,10,26,15,11,21,16,6,12,17,3,7,4,18,23,19,25,8,13,24,2,30,28);
   KeySet2 : array[1..31] of integer = (13,14,4,24,30,28,29,15,11,2,16,9,17,7,10,8,18,26,23,19,5,12,20,6,25,21,22,3,1,27,31);
   KeySet3 : array[1..31] of integer = (8,2,26,11,28,30,31,15,5,24,16,13,17,6,22,3,19,18,1,9,20,7,23,4,25,10,21,12,14,29,27);

var
   idx, RandomRange, Save1, Save2, Hash1, Hash2 : integer;
   Mod1, Mod2, ThisIdx, Month                   : integer;
   ThisVal                                      : shortint;
   WorkingDate, WorkingMonth, UnlockCode        : string;
   KeySet                                       : array[1..31] of integer;
   UniqueID                                     : array[1..12] of char;
   DBPrefix                                     : array[1..6]  of char;
   This_Key_Priv                                : REC_Key_Priv;
   CodedKey, ScrambledKey                       : REC_Key_Overlay;

begin

   DefaultFormatSettings.ShortDateFormat := 'yyyy/MM/dd';
   DefaultFormatSettings.DateSeparator   := '/';

//--- Set all fields to 0 or false as a precaution

   This_Key_Priv.DaysLeft         := 0;
   This_Key_Priv.LPMS_Collections := False;
   This_Key_Priv.LPMS_DocGen      := False;
   This_Key_Priv.LPMS_Floating    := False;
   This_Key_Priv.LPMS_Options4    := False;
   This_Key_Priv.License          := ord(LIC_INVALID);
   This_Key_Priv.DBPrefix         := '';
   This_Key_Priv.Unique           := '000000000000';

//--- Remove the "-" characters from the supplied key and copy to Key in Strings

   if Length(Decode_Key_Priv.Key) <> KeyLength then begin

      This_Key_Priv.DaysLeft := ord(ERR_LENGTH) - 3;
      Result := This_Key_Priv;
      Exit;

   end;

   UnlockCode := Copy(Decode_Key_Priv.Key, 1, 4) + Copy(Decode_Key_Priv.Key, 6, 3) +
                 Copy(Decode_Key_Priv.Key,10, 4) + Copy(Decode_Key_Priv.Key,15, 4) +
                 Copy(Decode_Key_Priv.Key,20, 4) + Copy(Decode_Key_Priv.Key,25, 4) +
                 Copy(Decode_Key_Priv.Key,30, 4) + Copy(Decode_Key_Priv.Key,35, 4);

   for idx := 1 to KeyShort do
      ScrambledKey.Strings.Key[idx] := char(UnlockCode[idx]);

//--- Replace all '#' with '0' and '?' with '@'

   for idx := 1 to KeyShort do begin

      if ScrambledKey.Strings.Key[idx] = '#' then
         ScrambledKey.Strings.Key[idx] := '0';

      if ScrambledKey.Strings.Key[idx] = '?' then
         ScrambledKey.Strings.Key[idx] := '@';

   end;

//--- Start off by extracting the range that was used to create the key

   RandomRange := integer(ScrambledKey.Chars.Range) and $0F;

   if (RandomRange < 0) or (RandomRange > 2) then begin

      This_Key_Priv.DaysLeft := ord(ERR_INVALID) - 3;
      Result := This_Key_Priv;
      Exit;

   end;

//--- Initialise the key set using the RandomRange

   case RandomRange of
      0: begin
         for idx := 1 to KeyShort do
            KeySet[idx] := KeySet1[idx];
      end;

      1: begin
         for idx := 1 to KeyShort do
            KeySet[idx] := KeySet2[idx];
      end;

      2: begin
         for idx := 1 to KeyShort do
            KeySet[idx] := KeySet3[idx];
      end;

   end;

//--- Unscramble the supplied key into Coded form

   for idx := 1 to KeyShort do begin

      ThisVal := shortint(ScrambledKey.Strings.Key[idx]) and $0F;
      ThisIdx := KeySet[idx];
      CodedKey.Strings.Key[ThisIdx] := char(ThisVal);

   end;

//--- Check the HashCodes

   Save1 := shortint(CodedKey.Chars.CheckSum1);
   Save2 := shortint(CodedKey.Chars.CheckSum2);

   CodedKey.Chars.CheckSum1 := char(0);
   CodedKey.Chars.CheckSum2 := char(0);

   Hash1 := 0;
   Hash2 := 0;

   for idx := 1 to UniqueLen do begin

      ThisIdx := idx - 1;
      Hash1 := Hash1 + (integer(CodedKey.Fields.Unique[idx]) * ThisIdx);

   end;

   Mod1 := Hash1 mod 11;

   for idx := 1 to KeyShort do begin

      ThisIdx := idx - 1;
      Hash2 := Hash2 + (integer(CodedKey.Strings.Key[idx]) * ThisIdx);

   end;

   Mod2 := Hash2 mod 11;

   if Save1 <> Mod1 then begin

      This_Key_Priv.DaysLeft := ord(ERR_INVALID) - 3;
      Result := This_Key_Priv;
      Exit;

   end;

   if Save2 <> Mod2 then begin

      This_Key_Priv.DaysLeft := ord(ERR_INVALID) - 3;
      Result := This_Key_Priv;
      Exit;

   end;

//--- Decode the unscrambled key - Only Fields that are required to be ASCII
//--- display fields are transformed

//--- Start with the Date

   if shortint(CodedKey.Chars.ExpDateM) = $0A then
      WorkingMonth := '10'
   else if shortint(CodedKey.Chars.ExpDateM) = $0B then
      WorkingMonth := '11'
   else if shortint(CodedKey.Chars.ExpDateM) = $0C then
      WorkingMonth := '12'
   else begin

      Month := integer(CodedKey.Chars.ExpDateM) or $30;
      WorkingMonth := '0' + char(Month);

   end;

   CodedKey.Chars.ExpDateYL := char(integer(CodedKey.Chars.ExpDateYL) or $30);
   CodedKey.Chars.ExpDateYR := char(integer(CodedKey.Chars.ExpDateYR) or $30);
   CodedKey.Chars.ExpDateDL := char(integer(CodedKey.Chars.ExpDateDL) or $30);
   CodedKey.Chars.ExpDateDR := char(integer(CodedKey.Chars.ExpDateDR) or $30);

   WorkingDate := '20' + string(CodedKey.Chars.ExpDateYL) +
                  string(CodedKey.Chars.ExpDateYR) + '/' + WorkingMonth +
                  '/' + string(CodedKey.Chars.ExpDateDL) +
                  string(CodedKey.Chars.ExpDateDR);

//--- Extract the switches
//--- Start with Switch 1 containing the Options

   if (integer(CodedKey.Chars.Switch01) and $08) = $08 then
      This_Key_Priv.LPMS_Collections := True
   else
      This_Key_Priv.LPMS_Collections := False;

   if (integer(CodedKey.Chars.Switch01) and $04) = $04 then
      This_Key_Priv.LPMS_DocGen := True
   else
      This_Key_Priv.LPMS_DocGen := False;

   if (integer(CodedKey.Chars.Switch01) and $02) = $02 then
      This_Key_Priv.LPMS_Floating := True
   else
      This_Key_Priv.LPMS_Floating := False;

   if (integer(CodedKey.Chars.Switch01) and $01) = $01 then
      This_Key_Priv.LPMS_Options4 := True
   else
      This_Key_Priv.LPMS_Options4 := False;

//--- Now Switch 2 containing the license type

   if (integer(CodedKey.Chars.Switch02) and $08) = $08 then
      This_Key_Priv.License := ord(LIC_TRIAL)
   else if (integer(CodedKey.Chars.Switch02) and $04) = $04 then
      This_Key_Priv.License := ord(LIC_BROWSE)
   else if (integer(CodedKey.Chars.Switch02) and $02) = $02 then
      This_Key_Priv.License := ord(LIC_PERSONAL)
   else if (integer(CodedKey.Chars.Switch02) and $01) = $01 then
      This_Key_Priv.License := ord(LIC_GENERIC)
   else
      This_Key_Priv.License := ord(LIC_INVALID);

//--- Extract the licensed MacAddress

   for idx := 1 to UniqueLen do begin

      if integer(CodedKey.Fields.Unique[idx]) > $09 then begin

         CodedKey.Fields.Unique[idx] := char(integer(CodedKey.Fields.Unique[idx]) or $40);
         CodedKey.Fields.Unique[idx] := char(integer(CodedKey.Fields.Unique[idx]) - 9);

      end else
         CodedKey.Fields.Unique[idx] := char(integer(CodedKey.Fields.Unique[idx]) or $30);

      UniqueID[idx] := CodedKey.Fields.Unique[idx];

   end;

   This_Key_Priv.Unique := UniqueID;

//--- Extract the DBPrefix

   DBPrefix[1] := char((integer(CodedKey.Chars.DBPrefix01L) shl 4) + integer(CodedKey.Chars.DBPrefix01R));
   DBPrefix[2] := char((integer(CodedKey.Chars.DBPrefix02L) shl 4) + integer(CodedKey.Chars.DBPrefix02R));
   DBPrefix[3] := char((integer(CodedKey.Chars.DBPrefix03L) shl 4) + integer(CodedKey.Chars.DBPrefix03R));
   DBPrefix[4] := char(integer(CodedKey.Chars.DBPrefix04) or $30);
   DBPrefix[5] := char(integer(CodedKey.Chars.DBPrefix05) or $30);
   DBPrefix[6] := char(integer(CodedKey.Chars.DBPrefix06) or $30);

   This_Key_Priv.DBPrefix := DBPrefix;

//--- Calculate the number of days remaining

   This_Key_Priv.KeyDate := WorkingDate;

   if WorkingDate < FormatDateTime(DefaultFormatSettings.ShortDateFormat,Date()) then begin

      This_Key_Priv.DaysLeft := ord(ERR_EXPIRED) - 3;
      Result := This_Key_Priv;
      Exit;

   end;

   try
      This_Key_Priv.DaysLeft := DaysBetween(Now(),(StrToDate(WorkingDate)));
   except

      This_Key_Priv.DaysLeft := ord(ERR_INVALID) - 3;
      Result := This_Key_Priv;
      Exit;

   end;

   Result := This_Key_Priv;

end;

exports
   DoDecode;

//------------------------------------------------------------------------------
// Function to encode a key with the values contained in This_Key_Values and
// return a fully porpulated REC_Key_Values structure with the encoded Key
// inthe 'Unique' field
//------------------------------------------------------------------------------
function DoEncode(Decode_Key_Values: REC_Key_Values): REC_Key_Values; stdcall;
var
   This_Key_Values : REC_Key_Values;
begin

   This_Key_Values.Unique := '8HRH-?#?-431A-#?KP-DQBR-U27K-JPTP-KARM';

   Result := This_Key_Values;

end;

exports
   DoEncode;


//------------------------------------------------------------------------------
// Function to extract swithces '-?' and values from a stringlist of passed
// command line parameters
//------------------------------------------------------------------------------
function cmdlOptions(): integer; stdcall;
begin

   Result := 0;

end;

exports
   cmdlOptions;

//------------------------------------------------------------------------------
// Function to use MIME to send an email via SMTP
//------------------------------------------------------------------------------
function SendMimeMail(From, ToStr, CcStr, BccStr, Subject, Body, Attach, SMTPStr : string): boolean; stdcall;
var
   idx          : integer;     // Used for adding attachments
   NumErrors    : integer;     // Keesp track whether there were errors during send

   SendResult   : boolean;     // Result returned by the SMTP send function

   SMTPHost     : string;      // SMTP Host name
   SMTPUser     : string;      // SMTP User name
   SMTPPasswd   : string;      // SMTP Password
   AddrId       : string;      // Holds an extracted email address
   ThisStr      : string;      // Used in the transformation of the address lists

   SMTPParms    : TStringList; // Final list of SMTP parameters
   Content      : TStringList; // Final string list containing the body of the email
   AttachList   : TStringList; // Final list of attahcments
   Mime         : TMimeMess;   // Mail message in MIME format
   MimePtr      : TMimePart;   // Pointer to the MIME messsage being constructed

begin

//--- Extract the SMTP Parameters

   SMTPParms := TStringList.Create;
   ExtractStrings(['|'],[' '],PChar(SMTPStr),SMTPParms);

   SMTPHost   := SMTPParms.Strings[0];
   SMTPUser   := SMTPParms.Strings[1];
   SMTPPasswd := SMTPParms.Strings[2];

//--- Extract the List of files to be attached

   AttachList := TStringList.Create;
   ExtractStrings(['|'],[' '],PChar(Attach),AttachList);

//--- Extract the body of the email

   Content := TStringList.Create;
   ExtractStrings(['|'],['*'],PChar(Body),Content);

   Mime := TMimeMess.Create;

   try

//--- Set the headers. The various address lists (To, Cc and Bcc) can contain
//--- more than 1 address but these must be seperted by commas and there should
//--- be no spaces between addresses. The Bcc address list is not added to the
//--- Header as the data in the Header is not used to determine who to send the
//--- email to.

      ThisStr := ToStr;

      repeat

         AddrId := Trim(FetchEx(ThisStr, ',', '"'));

         if AddrId <> '' then
            Mime.Header.ToList.Append(AddrId);

      until ThisStr = '';

      ThisStr := CcStr;

      repeat

         AddrId := Trim(FetchEx(ThisStr, ',', '"'));

         if AddrId <> '' then
            Mime.Header.CCList.Append(AddrId);

      until ThisStr = '';

      Mime.Header.Subject := Subject;
      Mime.Header.From    := From;
      Mime.Header.ReplyTo := From;

//--- Create a MultiPart part

      MimePtr := Mime.AddPartMultipart('mixed',Nil);

//--- Add the mail text as the first part

      Mime.AddPartText(Content,MimePtr);

//--- Add all atachments

      if AttachList.Count > 0 then begin

         for idx := 0 to AttachList.Count - 1 do
            Mime.AddPartBinaryFromFile(AttachList[idx],MimePtr);

      end;

//--- Compose the message to comply with MIME requirements

      Mime.EncodeMessage;

//--- Now the messsage can be sent to each of the recipients (To, Cc and Bcc)
//--- using SendToRaw

      NumErrors := 0;
      ThisStr  := ToStr + ',' + CcStr + ',' + BccStr;

      repeat

         AddrId := Trim(FetchEx( ThisStr, ',', '"'));

         if AddrId <> '' then begin
            SendResult := SendToRaw(From, AddrId, SMTPHost, Mime.Lines, SMTPUser, SMTPPasswd);

            if SendResult = False then
               Inc(NumErrors);

         end;

      until ThisStr = '';

   finally
      Mime.Free;
   end;


//--- Delete the lists we used

   AttachList.Free;
   SMTPParms.Free;
   Content.Free;

   if NumErrors > 0 then
      Result := False
   else
      Result := True;

end;

exports
   SendMimeMail;

end.
