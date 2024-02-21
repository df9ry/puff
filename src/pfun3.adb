
with Ada.Text_IO;  use Ada.Text_IO;
with Interfaces;   use Interfaces;
with Interfaces.C; use Interfaces.C;
with Utils;        use Utils;

with xgraph;       use xgraph;

package body pfun3 is

   --  *
   --  Make a new external port.
   --  *
   procedure new_port (x, y : Long_Float; port_number : Integer) is
   begin

      --  New_n (portnet (port_number));
      declare P2Ada_Var_17 : net_record renames portnet (port_number).all;
      begin
         --  not_connected yet
         P2Ada_Var_17.next_net := null;
         P2Ada_Var_17.number_of_con := 0;
         P2Ada_Var_17.con_start := null;
         P2Ada_Var_17.xr := x;
         P2Ada_Var_17.yr := y;
         P2Ada_Var_17.ports_connected := port_number;
         P2Ada_Var_17.node := False;
      end;
   end new_port;

   --  *
   --  Write file name above Parts.
   --  Remove any subdirectory information and .puf
   --  *
   procedure Write_File_Name (fname : Unbounded_String) is
      --  Erase old file name
      temp_str : Unbounded_String;
   begin
      GotoXY (Integer_32 (filename_position (1)),
              Integer_32 (filename_position (2)));
      for i in filename_position (1) .. filename_position (3)
      loop
         Put (' ');
      end loop;
      --  Remove dir from filename
      --  loop
      --     i := Index (fname, "/");
      --     if i > 0
      --     then
      --        Delete (Source => fname, 1, Before => i);
      --     end if;
      --     exit when i = 0;
      --  end loop;
      --  Write filename on screen
      temp_str := To_Unbounded_String ("file : ") & fname;
      TextCol (Unsigned_16 (col_window (1)));
      GotoXY (Integer_32 (filename_position (1) + (filename_position (3) -
                filename_position (1) - Length (temp_str)) / 2),
                Integer_32 (filename_position (2)));
      Put (To_String (temp_str));
   end Write_File_Name;

   --  *
   --  Draws a small box and number for an external port.
   --  *
   procedure Draw_Port (mnet : net; col : Integer) is
      x, y, i, yj : Integer;
   begin
      SetCol (Unsigned_16 (col));
      x := xmin (1) + Round (mnet.all.xr / csx);
      y := ymin (1) + Round (mnet.all.yr / csy);
      i := 0;
      --  yj := 0;
      case mnet.all.ports_connected is
         when 1 | 3 =>
            --  i:= 0;
            SetTextJustify (righttext, centertext);
            i := -3;
         when 2 | 4 =>
            --  i:= 2;
            SetTextJustify (lefttext, centertext);
            i := 5;
         when others =>
            --  [P2Ada]: no otherwise / else in Pascal
            null;
      end case;
      --  write number
      yj := y;
      OutTextXY (x + i, yj, To_C ("" & mnet.ports_connected'Image));
      fill_box (x - 2, y - 2, x + 2, y + 2, col);
   end Draw_Port;

   procedure Draw_Circuit is
      --  * Clear Layout screen *
      --  * Draw double box to look like text border *
      --  * write "LAYOUT" header *
      tnet : net;
      port_number, xb, yb, xo, yo : Integer;
   begin
      xo := xmin (1);
      xb := Round (bmax / csx) + xo;
      yo := ymin (1);
      yb := Round (bmax / csy) + yo;
      Clear_Window_gfx (xmin (9), ymin (9), xmax (9), ymax (9));
      Draw_Box (xo, yo - 7, xb, yb + 2, col_window (1));
      Draw_Box (xo, yo - 5, xb, yb, col_window (1));
      write_compt (col_window (1), window_f (1));
      if net_start = null
      then
         new_port (bmax / 2.0, bmax / 2.0, 0);
         new_port (0.0, (bmax - con_sep) / 2.0, 1);
         new_port (bmax, (bmax - con_sep) / 2.0, 2);
         min_ports := 2;
         if con_sep /= 0.0
         then
            new_port (0.0, (bmax + con_sep) / 2.0, 3);
            new_port (bmax, (bmax + con_sep) / 2.0, 4);
            min_ports := 4;
         end if;
      end if;
      --  if net_start
      for port_number in 1 .. min_ports
      loop
         Draw_Port (portnet (port_number), Brown);
      end loop;
      if net_start /= null
      then
         tnet := null;
         iv := 1;
         loop
            if tnet = null
            then
               tnet := net_start;
            else
               tnet := tnet.all.next_net;
            end if;
            dirn := tnet.all.con_start.all.dir;
            if tnet.all.node
            then
               if tnet.grounded
               then
                  draw_groundO (tnet.xr, tnet.yr);
               end if;
            else
               Draw_Net (tnet);
            end if;
            exit when tnet.all.next_net = null;
         end loop;
         tnet := null;
         loop
            if tnet = null
            then
               tnet := net_start;
            else
               tnet := tnet.all.next_net;
            end if;
            if tnet.all.ports_connected > 0
            then
               Draw_Port (tnet);
            end if;
            exit when tnet.all.next_net = null;
         end loop;
         --  if net_start=nil
         xi := Round (xm / csx);
         yi := Round (ym / csy);
      else
         goto_port (0);
      end if;
   end Draw_Circuit;

end pfun3;
