
with pfun1; use pfun1;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package pfun3 is

   procedure Draw_Circuit;

   procedure Draw_Port (mnet : net; col : Integer);

   procedure Draw_Net (tnet : net);

   procedure Write_File_Name (fname : Unbounded_String);

   procedure draw_ports (tnet : net);

   procedure goto_port (port_number : Integer);

   procedure draw_to_port (tnet : net; port_number : Integer);

   procedure node_look;

end pfun3;
