
with Ada.Text_IO;  use Ada.Text_IO;
with Interfaces;   use Interfaces;
with Interfaces.C; use Interfaces.C;
with Utils;        use Utils;

with xgraph;       use xgraph;
with pfun2;        use pfun2;

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
      xb, yb, xo, yo : Integer;
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
               draw_ports (tnet);
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

   procedure Draw_Net (tnet : net) is
   begin
      lengthxy (tnet);
      if read_kbd or else demo_mode
      then
         case tnet.all.com.all.typ is
            when 't' =>
               Draw_tline (tnet, True, False);
            when 'q' =>
               Draw_tline (tnet, True, False);
            when 'l' =>
               Draw_tline (tnet, False, False);
            when 'x' =>
               --  transformer
               Draw_xformer (tnet);
            when 'a' =>
               --  attenuator
               Draw_tline (tnet, False, False);
            when 'd' | 'i' =>
               Draw_device (tnet);
            when 'c' =>
               Draw_tline (tnet.all.other_net, True, False);
               Draw_tline (tnet, True, True);
            when others =>
               --  [P2Ada]: no otherwise / else in Pascal
               null;
         end case;
      end if;
      --  case
   end Draw_Net;

   procedure draw_ports (tnet : net) is
      tcon : conn;
   begin
      tcon := null;
      loop
         if tcon = null
         then
            tcon := tnet.all.con_start;
         else
            tcon := tcon.all.next_con;
         end if;
         if ext_port (tcon)
         then
            draw_to_port (tnet, tcon.all.port_type);
         end if;
         exit when tcon.all.next_con = null;
      end loop;
   end draw_ports;

   procedure goto_port (port_number : Integer) is
   begin
      xm := portnet (port_number).all.xr;
      ym := portnet (port_number).all.yr;
      if port_number = 0
      then
         xrold := xm;
         yrold := ym;
      end if;
      xi := Round (xm / csx);
      yi := Round (ym / csy);
      node_look;
   end goto_port;

   procedure draw_to_port (tnet : net; port_number : Integer) is
      xp, yp, offset, xli, yli, xsn, ysn : Integer;
   begin
      portnet (port_number).all.node := True;
      xsn := cwidthxZ02;
      ysn := cwidthyZ02;
      case port_number is
         when 1 | 3 =>
            offset := 2;
         when 2 | 4 =>
            offset := -2;
            xsn := -xsn;
         when others =>
            null;
      end case;
      --  case
      xli := Round (tnet.all.xr / csx) + xmin (1);
      yli := Round (tnet.all.yr / csy) + ymin (1);
      xp := Round (portnet (port_number).all.xr / csx) + offset + xmin (1);
      yp := Round (portnet (port_number).all.yr / csy) + ymin (1);
      if yli < yp
      then
         ysn := -ysn;
      end if;
      if abs (tnet.all.yr - portnet (port_number).all.yr) < widthZ0 / 2.0
      then
         puff_draw (xp, yp + ysn, xli, yp + ysn, LightGray);
         puff_draw (xli, yp - ysn, xli, yp + ysn, LightGray);
         puff_draw (xp, yp - ysn, xli, yp - ysn, LightGray);
      else
         puff_draw (xli - xsn, yli, xli + xsn, yli, LightGray);
         puff_draw (xli + xsn, yli, xli + xsn, yp, LightGray);
         puff_draw (xli + xsn, yp, xli, yp - ysn, LightGray);
         puff_draw (xli, yp - ysn, xp, yp - ysn, LightGray);
         puff_draw (xp, yp + ysn, xli - xsn, yp + ysn, LightGray);
         puff_draw (xli - xsn, yp + ysn, xli - xsn, yli, LightGray);
      end if;
      Draw_Port (portnet (port_number), LightRed);
   end draw_to_port;

   procedure node_look is
      tnet : net;
   begin
      cnet := null;
      tnet := null;
      if net_start /= null
      then
         loop
            if tnet = null
            then
               tnet := net_start;
            else
               tnet := tnet.all.next_net;
            end if;
            declare P2Ada_Var_16 : net_record renames tnet.all;
            begin
               if P2Ada_Var_16.con_start /= null
               then
                  if (abs (P2Ada_Var_16.con_start.all.cxr - xm) < resln)
                    and then P2Ada_Var_16.node
                    and then
                      (abs (P2Ada_Var_16.con_start.all.cyr - ym) < resln)
                  then
                     cnet := tnet;
                     return;
                  end if;
               end if;
            end;
            --  [P2Ada]: end of WITH
            exit when tnet.all.next_net = null;
         end loop;
      end if;
   end node_look;

end pfun3;
