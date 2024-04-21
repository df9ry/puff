
--  with Interfaces.C;

with xgraph;                use xgraph;

package body Utils is

   function is_in (ch : char; rg : CharArray) return Boolean is
   begin
      for x in rg'Range loop
         if rg (x) = ch
         then
            return True;
         end if;
      end loop;
      return False;
   end is_in;

   function is_in (ch : Character; rg : CharacterArray) return Boolean is
   begin
      for x in rg'Range loop
         if rg (x) = ch
         then
            return True;
         end if;
      end loop;
      return False;
   end is_in;

   function Round (x : Long_Float) return Integer is
   begin
      return Integer (Long_Float'Rounding (x));
   end Round;

   procedure PutFloat (Item : Long_Float; Fore, Aft, Exp : Integer) is
      pragma Unreferenced (Fore, Aft, Exp);
   begin
      --  s := Long_Float'Image (Item, Fore, Aft, Exp);
      PutStr (Interfaces.C.To_C (Long_Float'Image (Item)));
   end PutFloat;

end Utils;
