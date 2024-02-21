
with pfun1; use pfun1;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package pfun3 is

   procedure Draw_Circuit;

   procedure Draw_Port (mnet : net; col : Integer);

   procedure Write_File_Name (fname : Unbounded_String);

end pfun3;
