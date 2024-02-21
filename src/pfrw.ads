
with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package pfrw is

   procedure Read_Net (net_file : File_Type; init_graphics : Boolean);

   procedure Post_Read_Net (file_name : Unbounded_String);

   procedure Read_Board (net_file : File_Type; read_graphics : Boolean);

end pfrw;
