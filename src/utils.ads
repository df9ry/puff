
with Interfaces.C; use Interfaces.C;

package utils is

   type CharArray is array (Integer range <>) of char;

   function is_in (ch : char; rg : CharArray) return Boolean;

   Program_halted : exception;

end utils;
