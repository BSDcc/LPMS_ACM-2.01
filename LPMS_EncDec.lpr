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
      Dummy       : char;
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
      Dummy       : char;
   end;

   REC_Key_String = record
      Key : array[1..32] of char;
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
function DoDecode(var This_Key_Priv: REC_Key_Priv): integer; cdecl;
const
   KeyLength : integer = 38;
   KeyShort  : integer = 31;
   UniqueLen : integer = 12;

   KeySet1 : array[1..31] of integer = (4,8,0,13,30,28,26,19,21,9,25,14,10,20,15,5,11,16,2,6,3,17,22,18,24,7,12,23,1,29,27);
   KeySet2 : array[1..31] of integer = (12,13,3,23,29,27,28,14,10,1,15,8,16,6,9,7,17,25,22,18,4,11,19,5,24,20,21,2,0,26,30);
   KeySet3 : array[1..31] of integer = (7,1,25,10,27,29,30,14,4,23,15,12,16,5,21,2,18,17,0,8,19,6,22,3,24,9,20,11,13,28,26);

var
   idx, RandomRange, Save1, Save2, Hash1, Hash2 : integer;
   Mod1, Mod2, ThisVal, ThisIdx, Month          : integer;
   WorkingDate, WorkingMonth, UnlockCode        : string;
   KeySet                                       : array[1..32] of integer;
   UniqueID                                     : array[1..13] of char;
   DBPrefix                                     : array[ 1..7] of char;
   CodedKey, ScrambledKey                       : REC_Key_Overlay;

begin

   ShortDateFormat := 'yyyy/MM/dd';
   DateSeparator   := '/';

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

   if (Length(This_Key_Priv.Key) <> KeyLength) then begin

      Result := ord(ERR_LENGTH) - 3;
      Exit;

   end;

   UnlockCode := Copy(This_Key_Priv.Key, 1, 4) + Copy(This_Key_Priv.Key, 6, 3) +
                 Copy(This_Key_Priv.Key,10, 4) + Copy(This_Key_Priv.Key,15, 4) +
                 Copy(This_Key_Priv.Key,20, 4) + Copy(This_Key_Priv.Key,25, 4) +
                 Copy(This_Key_Priv.Key,30, 4) + Copy(This_Key_Priv.Key,35, 4);

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

      Result := ord(ERR_INVALID) - 3;
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

      ThisVal := integer(ScrambledKey.Strings.Key[idx]) and $0F;
      ThisIdx := KeySet[idx] + 1;
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

      Result := ord(ERR_INVALID) - 3;
      Exit;

   end;

   if (Save2 <> Mod2) then begin

      Result := ord(ERR_INVALID) - 3;
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

   if (WorkingDate < FormatDateTime('yyyy/MM/dd',Date())) then begin

      Result := ord(ERR_EXPIRED) - 3;
      Exit;

   end;

   try
      This_Key_Priv.DaysLeft := DaysBetween(Now(),(StrToDate(WorkingDate)));
   except

      Result := ord(ERR_INVALID) - 3;
      Exit;

   end;

//--- Check the days left then return an appropriate result

   if (This_Key_Priv.DaysLeft <= 0) then
      Result := ord(ERR_EXPIRED) - 3
   else
      Result := This_Key_Priv.DaysLeft;

end;

exports
   DoDecode;

//------------------------------------------------------------------------------
// Function to encode a key with the values contained in This_Key_Values and
// return the encoded Key
//------------------------------------------------------------------------------
function DoEncode(var This_Key_Values: REC_Key_Values): boolean; cdecl;
begin

   This_Key_Values.Unique := 'ABCD-EFG-HIJK-LMNO-PQRS-TUVW-XUZ1-2345';

   Result := True;
end;

exports
  DoEncode;

end.

