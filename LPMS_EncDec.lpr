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
   Classes, DateUtils, SysUtils;

//------------------------------------------------------------------------------
// Define the ENUM Types and Record structures
//------------------------------------------------------------------------------
type

   LIC_LICENSE  = (LIC_INVALID, LIC_TRIAL, LIC_PERSONAL, LIC_BROWSE, LIC_GENERIC);
   RES_RESULTS  = (ERR_INVALID, ERR_LENGTH, ERR_EXPIRED);

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

   REC_Key_Chars = record
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

   REC_Key_Fields = record
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

   REC_Key_String = record
      Key : array[1..31] of char;
   end;

   REC_Key_Overlay = record
      case integer of
         0: (Strings : REC_Key_String);
         1: (Chars   : REC_Key_Chars);
         2: (Fields  : REC_Key_Fields);
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

//------------------------------------------------------------------------------
// Function to decode a key contained in the parameter passed and return a
// string containing DaysLeft , Unique, License and DBPrefix
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
   DBPrefix                                     : array[ 1..6] of char;
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

   if (Length(Decode_Key_Priv.Key) <> KeyLength) then begin

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

//--- Replace all '#' with '0'

   for idx := 1 to KeyShort do begin

      if (ScrambledKey.Strings.Key[idx] = '#') then
         ScrambledKey.Strings.Key[idx] := '0';

   end;

//--- Replace all '?' with '@'

   for idx := 1 to KeyShort do begin

      if (ScrambledKey.Strings.Key[idx] = '?') then
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

   if (Save1 <> Mod1) then begin

      This_Key_Priv.DaysLeft := ord(ERR_INVALID) - 3;
      Result := This_Key_Priv;
      Exit;

   end;

   if (Save2 <> Mod2) then begin

      This_Key_Priv.DaysLeft := ord(ERR_INVALID) - 3;
      Result := This_Key_Priv;
      Exit;

   end;

//--- Decode the unscrambled key - Only Fields that are required to be ASCII
//--- display fields are transformed

//--- Start with the Date

   if (shortint(CodedKey.Chars.ExpDateM) = $0A) then begin

      WorkingMonth := '10';

   end else if (shortint(CodedKey.Chars.ExpDateM) = $0B) then begin

      WorkingMonth := '11';

   end else if (shortint(CodedKey.Chars.ExpDateM) = $0C) then begin

      WorkingMonth := '12';

   end else begin

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

   if ((integer(CodedKey.Chars.Switch01) and $08) = $08) then
      This_Key_Priv.LPMS_Collections := True
   else
      This_Key_Priv.LPMS_Collections := False;

   if ((integer(CodedKey.Chars.Switch01) and $04) = $04) then
      This_Key_Priv.LPMS_DocGen := True
   else
      This_Key_Priv.LPMS_DocGen := False;

   if ((integer(CodedKey.Chars.Switch01) and $02) = $02) then
      This_Key_Priv.LPMS_Floating := True
   else
      This_Key_Priv.LPMS_Floating := False;

   if ((integer(CodedKey.Chars.Switch01) and $01) = $01) then
      This_Key_Priv.LPMS_Options4 := True
   else
      This_Key_Priv.LPMS_Options4 := False;

//--- Now Switch 2 containing the license type

   if ((integer(CodedKey.Chars.Switch02) and $08) = $08) then
      This_Key_Priv.License := ord(LIC_TRIAL)
   else if ((integer(CodedKey.Chars.Switch02) and $04) = $04) then
      This_Key_Priv.License := ord(LIC_BROWSE)
   else if ((integer(CodedKey.Chars.Switch02) and $02) = $02) then
      This_Key_Priv.License := ord(LIC_PERSONAL)
   else if ((integer(CodedKey.Chars.Switch02) and $01) = $01) then
      This_Key_Priv.License := ord(LIC_GENERIC)
   else
      This_Key_Priv.License := ord(LIC_INVALID);

//--- Extract the licensed MacAddress

   for idx := 1 to UniqueLen do begin

      if (integer(CodedKey.Fields.Unique[idx]) > $09) then begin

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

   if (WorkingDate < FormatDateTime(DefaultFormatSettings.ShortDateFormat,Date())) then begin

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
// return the encoded Key
//------------------------------------------------------------------------------
function DoEncode(Decode_Key_Values: REC_Key_Values): REC_Key_Values; stdcall;
var
   This_Key_Values : REC_Key_Values;
begin

   This_Key_Values.Unique := 'ABCD-EFG-HIJK-LMNO-PQRS-TUVW-XUZ1-2345';

   Result := This_Key_Values;

end;

exports
  DoEncode;

end.

