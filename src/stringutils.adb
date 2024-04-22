package body StringUtils is

   function Copy (Item : Unbounded_String;
                  Start : Integer; Len : Integer) return Unbounded_String
   is
      s : Unbounded_String := To_Unbounded_String ("");
   begin
      for i in Start .. Start + Len - 1 loop
         s := s & Element (Item, i);
      end loop;
      return s;
   end Copy;

end StringUtils;
