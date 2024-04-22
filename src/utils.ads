
with Ada.Text_IO;           use Ada.Text_IO;

with Interfaces.C;          use Interfaces.C;

package Utils is

   type CharArray      is array (Integer range <>) of char;
   type CharacterArray is array (Integer range <>) of Character;

   LC_A_ORD : constant := 96; --  'a'

   function is_in (ch : char;      rg : CharArray)      return Boolean;
   function is_in (ch : Character; rg : CharacterArray) return Boolean;

   function Round (x : Long_Float) return Integer;

   procedure PutFloat (Item : Long_Float; Fore, Aft, Exp : Integer);

   function SeekEoln (File : File_Type) return Boolean;
   procedure Skip_Line (File : File_Type);

   Program_halted : exception;

end Utils;
