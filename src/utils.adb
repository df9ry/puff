package body utils is

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

end utils;
