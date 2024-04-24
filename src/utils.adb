
with xgraph;                  use xgraph;

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

   function SeekEoln (File : File_Type) return Boolean is
      Item : Character;
      End_Of_Line : Boolean;
      Whitespace : constant CharacterArray :=
         (' ', ASCII.HT, ASCII.CR, ASCII.LF);
   begin
      loop
         Look_Ahead (File, Item, End_Of_Line);
         if End_Of_Line
         then
            return True;
         end if;
         if not is_in (Item, Whitespace)
         then
            return False;
         end if;
         Get (File, Item);
      end loop;
   end SeekEoln;

   procedure Skip_Line (File : File_Type) is
      Item : Character;
      End_Of_Line : Boolean;
      Ignores : constant CharacterArray :=
         (' ', ASCII.HT, ASCII.CR, ASCII.LF);
   begin
      loop
         Look_Ahead (File, Item, End_Of_Line);
         exit when End_Of_File (File) or else not is_in (Item, Ignores);
         Get (File, Item);
      end loop;
   end Skip_Line;

   procedure Get (File : File_Type; Item : out Unbounded_String) is
      ch : Character;
   begin
      Item := To_Unbounded_String ("");
      while not (End_Of_File (File) or else End_Of_Line (File)) loop
         Get (File, ch);
         Item := Item & ch;
      end loop;
   end Get;

end Utils;
