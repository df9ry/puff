
with Interfaces.C;          use Interfaces.C;

package Utils is

   type CharArray      is array (Integer range <>) of char;
   type CharacterArray is array (Integer range <>) of Character;

   LC_A_ORD : constant := 96; --  'a'

   function is_in (ch : char;      rg : CharArray)      return Boolean;
   function is_in (ch : Character; rg : CharacterArray) return Boolean;

   function Round (x : Long_Float) return Integer;

   Program_halted : exception;

end Utils;
