
with Interfaces.C; use Interfaces.C;

package Utils is

   type CharArray      is array (Integer range <>) of char;
   type CharacterArray is array (Integer range <>) of Character;

   function is_in (ch : char;      rg : CharArray)      return Boolean;
   function is_in (ch : Character; rg : CharacterArray) return Boolean;

   Program_halted : exception;

end Utils;
