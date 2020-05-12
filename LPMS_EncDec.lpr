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
   Classes
   { you can add units after this };

{
//------------------------------------------------------------------------------
// Define the Record structures
//------------------------------------------------------------------------------
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

//   LPMS_Key_Values = REC_Key_Values;
//   LPMS_Key_Priv   = REC_Key_Priv;
}

//------------------------------------------------------------------------------
// Function to decode a key contained in the parameter passed and return a
// string containing DaysLeft , Unique, License and DBPrefix
//------------------------------------------------------------------------------
function DoDecode(This_Key: string): string; stdcall;
var
   KeyInfo : string;

begin

   KeyInfo := '5|0123456789ABC|2|MPA001';

   Result := KeyInfo;

end;

exports
   DoDecode;

//------------------------------------------------------------------------------
// Function to encode a key with the values contained in This_Key_Values and
// return the encoded Key
//------------------------------------------------------------------------------
function DoEncode(Unique: string): string; stdcall;
begin
  DoEncode := 'ABCD-EFG-HIJK-LMNO-PQRS-TUVW-XYZ1-2345';
end;

exports
  DoEncode;

end.

