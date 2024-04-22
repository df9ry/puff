
with Ada.Text_IO;            use Ada.Text_IO;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;

with Utils;                  use Utils;

package body Scanner is

   procedure GetCh (file : Ada.Text_IO.File_Type) is
   begin
      cur_ch := next_ch;
      if not End_Of_File (file)
      then
         Get (file, next_ch);
      else
         next_ch := Character'Val (0);
      end if;
   end GetCh;

   procedure SkipToEndOfLine (file : Ada.Text_IO.File_Type) is
   begin
      Utils.Skip_Line (file);
      Get (file, cur_ch);
      Get (file, next_ch);
   end SkipToEndOfLine;

   Whitespace : constant CharacterArray := (Space, HT, CR, LF);

   procedure SkipWhitespace (file : Ada.Text_IO.File_Type) is
   begin
      loop
         exit when End_Of_File (file) or else not is_in (cur_ch, Whitespace);
         GetCh (file);
      end loop;
   end SkipWhitespace;

end Scanner;
