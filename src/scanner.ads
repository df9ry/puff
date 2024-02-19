
with Ada.Text_IO;

package Scanner is

   cur_ch : Character := ' ';
   next_ch : Character := ' ';

   procedure GetCh (file : Ada.Text_IO.File_Type);

   procedure SkipToEndOfLine (file : Ada.Text_IO.File_Type);

   procedure SkipWhitespace (file : Ada.Text_IO.File_Type);

end Scanner;
