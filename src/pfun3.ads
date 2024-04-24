
with pfun1; use pfun1;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package pfun3 is

   procedure Draw_Circuit;

   procedure Draw_Port (mnet : net; col : Integer);

   procedure Draw_Net (tnet : net);

   procedure Write_File_Name (fname : Unbounded_String);

   procedure Write_Expanded_Parts;

   procedure HighLight_Window;

   procedure Write_Parts_ListO;

   procedure Write_Board_Parameters;

   procedure Plot_Manager (do_analysis, clear_plot, do_time, boxes,
                           b_out : Boolean);

   procedure Draw_Graph (x1, y1, x2, y2 : Integer; time : Boolean);

   procedure Get_Coords;

   procedure Pick_Smith (smith_type : Boolean);

   procedure Smith_and_Magplot (lighten, dash, boxes : Boolean);

   procedure Analysis (do_time, b_out : Boolean);

   procedure Write_Coordinates (time : Boolean);

   procedure Write_BigSmith_Coordinates;

   procedure draw_ports (tnet : net);

   procedure goto_port (port_number : Integer);

   procedure draw_to_port (tnet : net; port_number : Integer);

   procedure node_look;

   procedure move_marker (xi : Integer);

   procedure draw_ticksO (x1, y1, x2, y2 : Integer; incx, incy : Long_Float);

   procedure calc_posO (x, y, theta, scf : Long_Float; sfreq : Integer;
                        dash : Boolean);

end pfun3;
