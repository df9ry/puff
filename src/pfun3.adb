with Ada.Numerics.Long_Elementary_Functions;
use  Ada.Numerics.Long_Elementary_Functions;

with Ada.Text_IO;            use Ada.Text_IO;
with Ada.Long_Float_Text_IO; use Ada.Long_Float_Text_IO;
with Interfaces;             use Interfaces;
with Interfaces.C;           use Interfaces.C;
with Utils;                  use Utils;

with xgraph;                 use xgraph;
with pfun2;                  use pfun2;
with pfmsc;                  use pfmsc;

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

   procedure Write_Expanded_Parts is
         tcompt : compt;
   begin
      Make_Text_Border (xmin (3) - 1, ymin (3) - 1, xmax (5) + 1, ymax (5) + 1,
                        col_window (3), True);
      write_compt (col_window (3), window_f (3));
      if window_number = 3
      then
         HighLight_Window;
      end if;
      if part_start /= null
      then
         tcompt := null;
         loop
            if tcompt = null
            then
               tcompt := part_start;
            else
               tcompt := tcompt.all.next_compt;
            end if;
            write_compt (LightGray, tcompt);
            exit when tcompt.all.next_compt = null;
         end loop;
      end if;
   end Write_Expanded_Parts;

   procedure Write_Board_Parameters is
      --  erase and write border in board color
      --  write BOARD Header
      tcompt : compt;
   begin
      Make_Text_Border (xmin (4) - 1, ymin (4) - 1, xmax (4) + 1, ymax (4) + 1,
                        col_window (4), True);
      write_compt (col_window (4), window_f (4));
      if board_start /= null
      then
         tcompt := null;
         loop
            if tcompt = null
            then
               tcompt := board_start;
            else
               tcompt := tcompt.all.next_compt;
            end if;
            write_compt (LightGray, tcompt);
            exit when tcompt.all.next_compt = null;
         end loop;
         TextCol (LightGray);
         GotoXY (Integer_32 (xmin (4)), Integer_32 (ymin (4) + 6));
         Put ("Tab  ");
         if Manhattan_Board
         then
            Put ("Manhattan ");
         else
            if stripline
            then
               Put ("stripline ");
            else
               Put ("microstrip");
            end if;
         end if;
      end if;
   end Write_Board_Parameters;

   procedure HighLight_Window is
      --  highlight selected window
   begin
      GotoXY (Integer_32 (window_f (window_number).all.xp + 1),
              Integer_32 (window_f (window_number).all.yp));
      TextCol (White);
      Put ('F');
      Put (window_number'Image);
   end HighLight_Window;

   procedure Write_Parts_ListO is
      tcompt : compt;
      y : Integer;
   begin
      Make_Text_Border (xmin (3) - 1, ymin (3) - 1, xmax (3) + 1, ymax (3) + 1,
                        col_window (3), True);
      write_compt (col_window (3), window_f (3));
      if part_start /= null
      then
         tcompt := null;
         y := ymin (3);
         loop
            if tcompt = null
            then
               tcompt := part_start;
            else
               tcompt := tcompt.all.next_compt;
            end if;
            write_compt (LightGray, tcompt);
            y := y + 1;
            exit when y = ymax (3) + 1;
         end loop;
      end if;
   end Write_Parts_ListO;

   procedure Plot_Manager (do_analysis, clear_plot, do_time, boxes,
                           b_out : Boolean) is
      --  used to erase any residual S's
      ticko : Integer_64;
   begin
      ticko := GetTimerTicks;
      Erase_Message;
      move_cursor (0, 0);
      Get_Coords;
      if bad_compt
      then
         Write_Message;
         cx := ccompt.all.x_block;
         filled_OK := False;
      else
         Pick_Smith (admit_chart);
         if not (Large_Smith)
         then
            Draw_Graph (xmin (8), ymin (8), xmax (8), ymax (8), False);
         end if;
         cx := ccompt.all.x_block;
         if not (clear_plot) and then filled_OK
         then
            Smith_and_Magplot (True, True, True);
            move_marker (0);
         end if;
         if do_analysis
         then
            Analysis (do_time, b_out);
         end if;
         --  Start Analysis
         if filled_OK
         then
            --  plot spline points after analysis
            --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
            marker_OK := True;
            Smith_and_Magplot (False, False, boxes);
            ticko := GetTimerTicks - ticko;
            if do_analysis and then not (key = 'h' or else key = 'H')
            then
               --  center text in message box
               --  was ticko/18.2:8:1
               Erase_Message;
               TextCol (LightGray);
               GotoXY (Integer_32 ((xmin (6) + xmax (6) - 16) / 2),
                       Integer_32 (ymin (6) + 1));
               Put ("Time ");
               Put (Long_Float (ticko) / 18.2, 6, 1, 0);
               Put (" secs");
               beep;
            end if;
            move_marker (0);
            if demo_mode
            then
               rcdelay (300);
            end if;
         else
            marker_OK := False;
         end if;
      end if;
      --  if bad_compt
   end Plot_Manager;

   procedure Draw_Graph (x1, y1, x2, y2 : Integer; time : Boolean) is
      --  erase previous graph
   begin
      Clear_Window_gfx (xmin (11), ymin (11), xmax (11), ymax (11));
      if not (time)
      then
         Clear_Window (xmin (2), ymin (2), xmax (2), ymax (2));
      end if;
      --  clear key area
      TextCol (LightGray);
      Write_Coordinates (time);
      Draw_Box (x1, y1, x2, y2, LightGreen);
      draw_ticksO (x1, y1, x2, y2,
                   Long_Float (x2 - x1) / 4.0, Long_Float (y2 - y1) / 4.0);
   end Draw_Graph;

   procedure Get_Coords is
      --  point to dBmax value
      tcoord : compt;
   begin
      bad_compt := False;
      tcoord := dBmax_ptr;
      symax := Get_Real (tcoord, 1);
      if bad_compt
      then
         return;
      end if;
      --  point to dBmin value
      tcoord := tcoord.all.next_compt;
      symin := Get_Real (tcoord, 1);
      if bad_compt
      then
         return;
      end if;
      if symin >= symax
      then
         bad_compt := True;
         message (1) := To_Unbounded_String ("Must have");
         message (2) := To_Unbounded_String ("dB(max) > dB(min)");
         ccompt := tcoord;
         return;
      end if;
      --  point to fmin
      tcoord := tcoord.all.next_compt;
      sxmin := Get_Real (tcoord, 1);
      if bad_compt
      then
         return;
      end if;
      if (sxmin < 0.0) and then not (Alt_Sweep)
      then
         bad_compt := True;
         message (1) := To_Unbounded_String ("Must have");
         message (2) := To_Unbounded_String ("frequency >= 0");
         ccompt := tcoord;
         return;
      end if;
      --  point to fmax
      tcoord := tcoord.all.next_compt;
      sxmax := Get_Real (tcoord, 1);
      if bad_compt
      then
         return;
      end if;
      if (sxmax < 0.0) and then not (Alt_Sweep)
      then
         bad_compt := True;
         message (1) := To_Unbounded_String ("Must have");
         message (2) := To_Unbounded_String ("frequency >= 0");
         ccompt := tcoord;
         return;
      end if;
      if sxmin >= sxmax
      then
         bad_compt := True;
         message (1) := To_Unbounded_String ("Plot must have");
         message (2) := To_Unbounded_String ("x_max > x_min");
         ccompt := tcoord;
         return;
      end if;
      sfx1 := Long_Float (xmax (8) - xmin (8)) / (sxmax - sxmin);
      sfy1 := Long_Float (ymax (8) - ymin (8)) / (symax - symin);
      sigma := (symax - symin) / 100.0;
      rho_fac := Get_Real (rho_fac_compt, 1);
      if (rho_fac <= 0.0) or else bad_compt
      then
         bad_compt := True;
         message (1) := To_Unbounded_String ("The Smith chart");
         message (2) := To_Unbounded_String ("radius must be >0");
         ccompt := rho_fac_compt;
      end if;
   end Get_Coords;

   procedure Pick_Smith (smith_type : Boolean) is
      --  Erase Smith Chart region
   begin
      Clear_Window_gfx (xmin (10), ymin (10), xmax (10), ymax (10));
      Draw_EGA_Smith (not (smith_type));
      if Large_Smith
      then
         Write_BigSmith_Coordinates;
      end if;
   end Pick_Smith;

   procedure Smith_and_Magplot (lighten, dash, boxes : Boolean) is
      --  ****************************************************************
      jxsdif, scf, cx1, cx2, cx3, cx4, cy1, cy2, cy3, cy4, sqfmfj, sqfjmf,
      fmfj, fjmf, spar1, spar2 : Long_Float;
      sfreq, jfreq, nopts, col : Integer;
      line_s, line_r : Boolean;
      cplt : plot_param;
      cspc : spline_param;
      --  *
      --  Calculate spline coefficients.
      --  Johnson and Riess Numerical Analysis p41,241.
      --  *

      procedure splineO (ij : Integer) is
         --  SplineO
         zx, zy, u : array (0 .. 1000) of Long_Float;
         m : Integer;
         li : Long_Float;
         cplt : plot_param;
         cspc : spline_param;
      begin
         m := npts - 1;
         for i in 0 .. m
         loop
            if i = 0
            then
               cplt := plot_start (ij);
               cspc := spline_start;
            else
               cplt := cplt.all.next_p;
               cspc := cspc.all.next_c;
            end if;
            --  [P2Ada]: WITH instruction
            --  [P2Ada]: !Help! No type found -> add 'P2Ada_Var_3.' to fields
            declare P2Ada_Var_3 : spline_record renames cspc.all;
            begin
               declare P2Ada_Var_4 : plot_record renames cplt.all;
               begin
                  P2Ada_Var_3.h := Sqrt (((yf * (P2Ada_Var_4.next_p.all.y -
                                           P2Ada_Var_4.y)) ** 2) +
                                             ((P2Ada_Var_4.next_p.all.x -
                                                    P2Ada_Var_4.x) ** 2));
                  if P2Ada_Var_3.h < 0.000001
                  then
                     P2Ada_Var_3.h := 0.000001;
                  end if;
               end;
               --  [P2Ada]: end of WITH
            end;
            --  [P2Ada]: end of WITH
         end loop;
         --  for i:=0 to m
         --  u_11=a_11
         --  y_1=b_1
         --  y_1=b_1
         spline_end := cspc.all.next_c;
         cplt := plot_start (ij);
         cspc := spline_start;
         u (1) := 2.0 * (cspc.all.next_c.all.h + cspc.all.h);
         zx (1) := 6.0 * ((cplt.all.next_p.all.next_p.all.x -
                          cplt.all.next_p.all.x) / cspc.all.next_c.all.h -
                            (cplt.all.next_p.all.x - cplt.all.x) / cspc.all.h);
         zy (1) := 6.0 * ((cplt.all.next_p.all.next_p.all.y -
                          cplt.all.next_p.all.y) / cspc.all.next_c.all.h -
                            (cplt.all.next_p.all.y - cplt.all.y) / cspc.all.h);
         for i in 2 .. m
         loop
            --  [P2Ada]: WITH instruction
            --  [P2Ada]: !Help! No type found -> add 'P2Ada_Var_5.' to fields
            cplt := cplt.all.next_p;
            cspc := cspc.all.next_c;
            declare P2Ada_Var_5 : spline_record renames cspc.all;
            begin
               declare P2Ada_Var_6 : plot_record renames cplt.all;
               begin
                  --  a_i-1i=a_ii-1=h_i-1,a_ii=2(h_i+h_i-1)
                  --  u_ii=a_ii-L_i.i-1a_i-1,i
                  --  2.33
                  li := P2Ada_Var_5.h / u (i - 1);
                  u (i) := 2.0 * (P2Ada_Var_5.next_c.all.h + P2Ada_Var_5.h)
                    - P2Ada_Var_5.h * li;
                  zx (i) := 6.0 * ((P2Ada_Var_6.next_p.all.next_p.all.x -
                                     P2Ada_Var_6.next_p.all.x) /
                                       P2Ada_Var_5.next_c.all.h -
                                     (P2Ada_Var_6.next_p.all.x -
                                        P2Ada_Var_6.x) / P2Ada_Var_5.h) -
                      li * zx (i - 1);
                  zy (i) := 6.0 * ((P2Ada_Var_6.next_p.all.next_p.all.y -
                                     P2Ada_Var_6.next_p.all.y) /
                                       P2Ada_Var_5.next_c.all.h -
                                     (P2Ada_Var_6.next_p.all.y -
                                        P2Ada_Var_6.y) / P2Ada_Var_5.h) -
                    li * zy (i - 1);
               end;
               --  [P2Ada]: end of WITH
            end;
            --  [P2Ada]: end of WITH
            --  with
         end loop;
         --  for i
         cspc := spline_end;
         cspc.all.sx := 0.0;
         cspc.all.sy := 0.0;
         cspc := cspc.all.prev_c;
         cspc.all.sx := zx (m) / u (m);
         cspc.all.sy := zy (m) / u (m);
         for i in 1 .. m - 1
         loop
            --  [P2Ada]: WITH instruction
            --  [P2Ada]: !Help! No type found -> add 'P2Ada_Var_7.' to fields
            cspc := cspc.all.prev_c;
            declare P2Ada_Var_7 : spline_record renames cspc.all;
            begin
               P2Ada_Var_7.sx := (zx (m - i) - P2Ada_Var_7.h *
                                    P2Ada_Var_7.next_c.all.sx) / u (m - i);
               P2Ada_Var_7.sy := (zy (m - i) - P2Ada_Var_7.h *
                                    P2Ada_Var_7.next_c.all.sy) / u (m - i);
            end;
            --  [P2Ada]: end of WITH
         end loop;
         cspc := cspc.all.prev_c;
         cspc.all.sx := 0.0;
         cspc.all.sy := 0.0;
      end splineO;
      --  splineO
      --  **************************************************************
      --  *
      --  Plot curve on Smith plot
      --  *

      procedure smith_plotO (x1, y1, col : Integer; linex : in out Boolean) is
         --  Smith_PlotO
      begin
         if spline_in_smith
         then
            if linex
            then
               SetCol (Unsigned_16 (col));
               Line (Integer_32 (xvalo (1)), Integer_32 (yvalo (1)),
                     Integer_32 (x1), Integer_32 (y1));
            end if;
            --  if linex
            --  if spline_
            linex := True;
         else
            linex := False;
         end if;
         xvalo (1) := x1;
         yvalo (1) := y1;
      end smith_plotO;
      --  smith_plotO
      --  ************************************************************
      --  *
      --  Line plotting routine for making curves in
      --  the rectangular plot. y1 is usually equal to spp.
      --  *

      procedure rect_plotO (x1, y1, col : Integer; linex : in out Boolean) is
         --  Rect_PlotO
         --  clip rectangular plot, plot with relative positioning,
         --  and reset the graphics pointer
      begin
         SetViewPort (Integer_32 (xmin (8)),
                      Integer_32 (ymin (8) - 1),
                      Integer_32 (xmax (8)),
                      Integer_32 (ymax (8)), Integer_8 (1));
         if linex
         then
            SetCol (Unsigned_16 (col));
            Line (Integer_32 (xvalo (2) - xmin (8)),
                  Integer_32 (yvalo (2) - ymin (8) + 1),
                  Integer_32 (x1 - xmin (8)),
                  Integer_32 (y1 - ymin (8) + 1));
         end if;
         --  Remove clipping
         linex := True;
         SetViewPort (Integer_32 (xmin (12)), Integer_32 (ymin (12)),
                      Integer_32 (xmax (12)), Integer_32 (ymax (12)),
                      Integer_8 (0));
         xvalo (2) := x1;
         yvalo (2) := y1;
      end rect_plotO;
      --  rect_plotO
      --  **********************************************************
      --  * Smith_and_Magplot *
      --  difference in x between plot points
   begin
      jxsdif := sfx1 * finc;
      scf := 1.0;
      if npts > 1
      then
         for ij in 1 .. max_params
         loop
            if s_param_table (ij).all.calc
            then
               splineO (ij);
               col := s_color (ij);
               if lighten
               then
                  col := col - 8;
               end if;
               line_s := False;
               line_r := False;
               for txpt in 0 .. npts
               loop
                  --  if keypressed then begin
                  --  key := ReadKey;
                  --  if key in['h','H'] then begin
                  --  message[2]:='      HALT       ';
                  --  write_message;
                  --  exit;
                  --  end; {if key in}
                  --  beep;
                  --  end;{if key_pressed}
                  if txpt = 0
                  then
                     cplt := plot_start (ij);
                     cspc := spline_start;
                  else
                     cplt := cplt.all.next_p;
                     cspc := cspc.all.next_c;
                  end if;
                  freq := fmin + Long_Float (txpt) * finc;
                  sfreq := xmin (8) + Round ((freq - sxmin) * sfx1);
                  if cplt.all.filled
                  then
                     calc_posO (cplt.all.x, cplt.all.y, 0.0, scf, sfreq, dash);
                     if not (Large_Smith)
                     then
                        rect_plotO (sfreq, spp, col, line_r);
                        if spline_in_rect and then boxes
                        then
                           box (sfreq, Round (Long_Float (spp) / scf), ij);
                        end if;
                     end if;
                     smith_plotO (spx, spy, col, line_s);
                     if spline_in_smith and then boxes
                     then
                        box (spx, Round (Long_Float (spy) / scf), ij);
                     end if;
                     if txpt < npts
                     then
                        declare P2Ada_Var_8 : spline_record
                           renames cspc.all;
                        begin
                           declare P2Ada_Var_9 : plot_record
                              renames cplt.all;
                           begin
                              cx1 := P2Ada_Var_8.sx / (6.0 * P2Ada_Var_8.h);
                              cx2 := P2Ada_Var_8.next_c.all.sx /
                                (6.0 * P2Ada_Var_8.h);
                              cy1 := P2Ada_Var_8.sy / (6.0 * P2Ada_Var_8.h);
                              cy2 := P2Ada_Var_8.next_c.all.sy /
                                (6.0 * P2Ada_Var_8.h);
                              cx3 := P2Ada_Var_9.next_p.all.x /
                                P2Ada_Var_8.h - P2Ada_Var_8.next_c.all.sx *
                                  P2Ada_Var_8.h / 6.0;
                              cx4 := P2Ada_Var_9.x /
                                P2Ada_Var_8.h - P2Ada_Var_8.sx *
                                  P2Ada_Var_8.h / 6.0;
                              cy3 := P2Ada_Var_9.next_p.all.y /
                                P2Ada_Var_8.h -
                                  P2Ada_Var_8.next_c.all.sy *
                                    P2Ada_Var_8.h / 6.0;
                              cy4 := P2Ada_Var_9.y / P2Ada_Var_8.h -
                                P2Ada_Var_8.sy * P2Ada_Var_8.h / 6.0;
                              if P2Ada_Var_8.h * Long_Float (rad) / rho_fac
                                > 40.0
                              then
                                 nopts := 10;
                              else
                                 nopts := Round (P2Ada_Var_8.h *
                                                   Long_Float (rad) /
                                                 (rho_fac * 4.0)) + 1;
                              end if;
                              for j in 1 .. nopts - 1
                              loop
                                 fmfj := Long_Float (j) *
                                   P2Ada_Var_8.h / Long_Float (nopts);
                                 fjmf := P2Ada_Var_8.h - fmfj;
                                 sqfmfj := ((fmfj) ** 2);
                                 sqfjmf := ((fjmf) ** 2);
                                 spar1 := (cx1 * sqfjmf + cx4) * fjmf +
                                   (cx2 * sqfmfj + cx3) * fmfj;
                                 spar2 := (cy1 * sqfjmf + cy4) * fjmf +
                                   (cy2 * sqfmfj + cy3) * fmfj;
                                 jfreq := Round (Long_Float (j) * jxsdif /
                                                     Long_Float (nopts));
                                 calc_posO (spar1, spar2, 0.0, scf,
                                            sfreq + jfreq, dash);
                                 if not (Large_Smith)
                                 then
                                    rect_plotO (sfreq + jfreq, spp, col,
                                                line_r);
                                 end if;
                                 smith_plotO (spx, spy, col, line_s);
                              end loop;
                              --  j:=1 to nopts-1
                           end;
                           --  [P2Ada]: end of WITH
                        end;
                        --  [P2Ada]: end of WITH
                     end if;
                     --  if txpt
                  end if;
                  --  if cpt^ filled
               end loop;
               --  for txpt
            end if;
         end loop;
      end if;
      --  ij
   end Smith_and_Magplot;

   procedure move_marker (xi : Integer) is
      i, ij, k, kk, nb, sfreq : Integer;
   begin
      if marker_OK
      then
         for ij in 1 .. max_params
         loop
            if s_param_table (ij).all.calc
            then
               case xi is
                  when 0 =>
                     if plot_des (ij) = null
                     then
                        --  point for cursor placement
                        xpt := 0;
                     else
                        c_plot (ij) := plot_des (ij);
                        xpt := Round ((design_freq - fmin) / finc);
                        if not (betweeni (0, xpt, npts))
                        then
                           xpt := 0;
                        end if;
                     end if;
                     if xpt = 0
                     then
                        c_plot (ij) := plot_start (ij);
                     end if;
                     --  0:
                  when 1 =>
                     if c_plot (ij) = plot_end (ij)
                     then
                        c_plot (ij) := plot_start (ij);
                     else
                        c_plot (ij) := c_plot (ij).all.next_p;
                     end if;
                  when -1 =>
                     if c_plot (ij) = plot_start (ij)
                     then
                        c_plot (ij) := plot_end (ij);
                     else
                        c_plot (ij) := c_plot (ij).all.prev_p;
                     end if;
                  when others =>
                     --  [P2Ada]: no otherwise / else in Pascal
                     null;
               end case;
            end if;
         end loop;
         --  case xi
         if xi = 0
         then
            for i in 1 .. 2 * max_params
            loop
               box_filled (i) := False;
            end loop;
         else
            xpt := xpt + xi;
         end if;
         if xpt > npts
         then
            xpt := 0;
         end if;
         if xpt < 0
         then
            xpt := npts;
         end if;
         if xi /= 0
         then
            Erase_Message;
         end if;
         Write_FreqO;
         sfreq := xmin (8) + Round ((freq - sxmin) * sfx1);
         for k in 1 .. 3
         loop
            for ij in 1 .. max_params
            loop
               if s_param_table (ij).all.calc
               then
                  case k is
                     when 1 =>
                        restore_boxO (ij);
                     when 2 =>
                        box_filled (ij) := False;
                        box_filled (ij + max_params) := False;
                        if c_plot (ij).all.filled
                        then
                           calc_posO (c_plot (ij).all.x,
                                      c_plot (ij).all.y, 0, 1, sfreq, False);
                           if spline_in_smith
                           then
                              move_boxO (spx, spy, ij);
                           end if;
                           if not (Large_Smith)
                           then
                              if spline_in_rect
                              then
                                 move_boxO (sfreq, spp, ij + max_params);
                              end if;
                           end if;
                        end if;
                        --  2:
                     when 3 =>
                        for kk in 0 .. 1
                        loop
                           nb := ij + kk * max_params;
                           if box_filled (nb)
                           then
                              pattern (box_dot (1, nb),
                                       box_dot (2, nb), ij, 128);
                           end if;
                        end loop;
                        --  kk
                        if c_plot (ij).all.filled
                        then
                           write_sO (ij);
                        end if;
                        --  3 :
                     when others =>
                        null;
                  end case;
               end if;
            end loop;
         end loop;
         --  case
      end if;
      --  if do time
   end move_marker;

   procedure Analysis (do_time, b_out : Boolean) is
      --  [BP2P]: Label "100001" Was "exit_analysis"
      --  netmem        : LONGINT;
      old_net : net;
      old_net_start : net;
      net_end_ptr1 : marker;
      net_end_ptr2 : marker;
      ptrall : marker;
      ptrvar : marker;
      ptranalysis : marker;
      MemError : Boolean;
   begin
      filled_OK := False;
      MemError := False;
      if net_start = null
      then
         message (1) := "No circuit";
         message (2) := "to analyze";
         Write_Message;
      else
         Get_s_and_f (do_time);
         if bad_compt
         then
            --  Save initial Markers
            --  Mark original memory setting
            --  netmem:= MemAvail;
            --  Copy original network
            Write_Message;
            cx := ccompt.all.x_block;
         else
            filled_OK := True;
            TextCol (LightGray);
            GotoXY (xmin (6), ymin (6) + 1);
            Put (" Press h to halt ");
            TextCol (White);
            GotoXY (xmin (6) + 7, ymin (6) + 1);
            Put ('h');
            old_net := cnet;
            old_net_start := net_start;
            Mark_Mem (ptrall);
            Init_Marker (net_end_ptr1);
            copy_networks (net_beg, ptrall, net_end_ptr1);
            if not Marked (net_end_ptr1)
            then
               --  [BP2P]: Label "100001" Was "exit_analysis"
               MemError := True;
               goto LABEL_100001;
            end if;
            --  set up nets for analysis
            --  IF (MemAvail < (netmem - MemAvail + npts * 256)) THEN
            --  BEGIN
            --  MemError:= TRUE;
            --  GOTO Exit_Analysis;
            --  END;
            --  Initialize markers for copy of new network
            --  main analysis loop
            set_up_net;
            Init_Marker (net_end_ptr2);
            Mark_Mem (ptranalysis);
            for xpt in 0 .. npts
            loop
               if Alt_Sweep
               then
                  --  Force parts to use fd
                  --  use freq data for alt parameter
                  freq := design_freq;
                  x_sweep.Load_Data (fmin + xpt * finc);
               else
                  if (xpt = npts) and then (npts /= 0)
                  then
                     freq := sxmax;
                  else
                     freq := fmin + xpt * finc;
                  end if;
                  --  These are normalized to the window
               end if;
               --  * For alternate sweep freq:=design_freq and finc=0 *
               --  make copy of network for analysis
               copy_networks (net_beg, ptranalysis, net_end_ptr2);
               if not Marked (net_end_ptr2)
               then
                  --  [BP2P]: Label "100001" Was "Exit_Analysis"
                  MemError := True;
                  goto LABEL_100001;
               end if;
               --  parts calculation and at xpt=0 fills device file s_ifile
               --  interpolation data
               Fill_compts;
               if No_mem_left
               then
                  --  [BP2P]: Label "100001" Was "exit_analysis"
                  goto LABEL_100001;
               end if;
               --  check for 16 bytes
               if bad_compt
               then
                  --  [BP2P]: Label "100001" Was "Exit_Analysis"
                  filled_OK := False;
                  goto LABEL_100001;
               else
                  while internal_joint_remaining
                  loop
                     --  loop over joints
                     if netK = netL
                     then
                        --  join connectors on same net
                        reduce_net;
                     else
                        join_net;
                     end if;
                  end loop;
                  --  join two nets
               end if;
               --  if bad_compt
               if Alt_Sweep
               then
                  freq := fmin + xpt * finc;
               end if;
               --  Restore freq for use in linked lists
               --  uses freq, calls write_freqO
               --  free memory used for analysis
               --  -> not for xpt = 0; keep device file's interpolation data
               Fill_Sa (P2Ada_no_keyword_out);
               if xpt /= 0
               then
                  Release_Mem (ptrvar);
               else
                  Mark_Mem (ptrvar);
               end if;
               --  check for abort
               if keypressed
               then
                  --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
                  chs := ReadKey;
                  if chs and ('h' | 'H' => True, others => False)
                  then
                     --  [BP2P]: Label "100001" Was "exit_analysis"
                     filled_OK := False;
                     Erase_Message;
                     message (2) := "      HALT       ";
                     Write_Message;
                     goto LABEL_100001;
                  end if;
                  beep;
               end if;
               --  if keypreseed
            end loop;
            --  for xpt:= 0 to npts
            <<LABEL_100001>>
            if No_mem_left or else MemError
            then
               filled_OK := False;
               Erase_Message;
               message (1) := " Circuit is too  ";
               message (2) := " large for Puff  ";
               message (3) := "   to analyze    ";
               Write_Message;
            end if;
            --  Restore network
            --  Release all network copies
            --  Restore initial markers
            copy_networks (net_beg, ptrall, net_end_ptr1);
            Release_Mem (ptrall);
            cnet := old_net;
            net_start := old_net_start;
         end if;
         --  if bad_compt
      end if;
      --  if cnet
   end Analysis;

   procedure Write_Coordinates (time : Boolean) is
      --  to prevent scrolling
      i : Integer;
      tcompt : compt;
      temp : Unbounded_String;
   begin
      WindMax := WindMax + 1;
      if time
      then
         --  was 56-
         --  was 56-
         --  was 56,13
         --  was 53,6
         --  was 66,13
         TextCol (LightGray);
         temp := rho_fac_compt.all.descript;
         Delete (temp, 1, 13);
         gotoxy (x_y_plot_text (1, 1) - Length (temp), x_y_plot_text (1, 2));
         Put (temp);
         temp := '-' + temp;
         gotoxy (x_y_plot_text (3, 1) - Length (temp), x_y_plot_text (3, 2));
         Put (temp);
         gotoxy (x_y_plot_text (4, 1), x_y_plot_text (4, 2));
         Put (sxmin, 6, 3, 0);
         gotoxy (x_y_plot_text (6, 1) - 6, x_y_plot_text (6, 2));
         Put (sxmax, 6, 3, 0);
         Gotoxy (x_y_plot_text (2, 1), x_y_plot_text (2, 2));
         Put (" S");
         gotoxy (x_y_plot_text (5, 1), x_y_plot_text (5, 2));
         Put ("t  sec");
      else
         for i in 1 .. 10
         loop
            if i = 1
            then
               tcompt := coord_start;
            else
               tcompt := tcompt.all.next_compt;
            end if;
            --  [P2Ada]: WITH instruction
            --  [P2Ada]: !Help! No type found -> add 'P2Ada_Var_1.' to fields
            declare P2Ada_Var_1 : compt_record renames tcompt.all;
            begin
               if right
               then
                  xp := xorig - Length (descript);
               end if;
               if Length (descript) > x_block or else i < 7
               then
                  --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
                  write_compt (LightGray, tcompt);
                  if i and (7 .. 10 => True, others => False)
                  then
                     pattern (xmin (2) * charx - 1,
                              (ymin (2) + 2 + i - 6) * chary - 8, i - 6, 0);
                  end if;
                  --  write the marker patterns box, X, diamond, and +
               end if;
            end;
            --  [P2Ada]: end of WITH
            --  with
         end loop;
         --  for
         --  [P2Ada]: !Help! Maybe (file,...) here
         --  [P2Ada]: !Help! Maybe (file,...) here
         --  was 53,6
         --  was 53,7
         TextCol (LightGray);
         Gotoxy (x_y_plot_text (2, 1), x_y_plot_text (2, 2));
         Put (the_bar);
         Put ('S');
         Put (the_bar);
         gotoxy (x_y_plot_text (2, 1), x_y_plot_text (2, 2) + 1);
         Put (" dB");
         if not (Alt_Sweep)
         then
            gotoxy (x_y_plot_text (5, 1), x_y_plot_text (5, 2));
            Put ("f  Hz");
         else
            x_sweep.Label_Axis;
         end if;
         --  use alternate sweep data
      end if;
      --  do_time
      if not (Alt_Sweep)
      then
         Write_Plot_Prefix (time);
      end if;
      --  write x-coordinate prefix
      --  Restore to normal scrolling
      WindMax := WindMax - 1;
   end Write_Coordinates;

   procedure draw_ticksO (x1, y1, x2, y2 : Integer; incx, incy : Long_Float)
   is
      --  was 8080
      i, xinc, yinc : Integer;
   begin
      --  TODO: SetLineStyle (UserBitLn, 16 #08 88 #0F ormWidth);
      SetCol (Green);
      for i in 1 .. 9
      loop
         xinc := Round (i * (x2 - x1) / 10.0);
         yinc := Round (i * (y2 - y1) / 5.0);
         Line (x1 + xinc, y1 - 1, x1 + xinc, y2);
         if i < 5
         then
            Line (x1, y1 + yinc, x2 - 1, y1 + yinc);
         end if;
      end loop;
      --  for
      SetLineStyle (SolidLn, 0, normwidth);
   end draw_ticksO;

   procedure Write_BigSmith_Coordinates is
      --  Erase key area
      --  clear key area
      --  to prevent scrolling
      i : Integer;
      tcompt : compt;
   begin
      Clear_Window (xmin (2), ymin (2), xmax (2), ymax (2));
      TextCol (LightGray);
      WindMax := WindMax + 1;
      for i in 3 .. 10
      loop
         if i = 3
         then
            tcompt := coord_start;
         else
            tcompt := tcompt.all.next_compt;
         end if;
         --  [P2Ada]: WITH instruction
         --  [P2Ada]: !Help! No type found -> add 'P2Ada_Var_2.' to fields
         declare P2Ada_Var_2 : compt_record renames tcompt.all;
         begin
            if right
            then
               xp := xorig - (descript'length);
            end if;
            if ((descript'length) > x_block) or else (i < 7)
            then
               --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
               write_compt (LightGray, tcompt);
               if i and (7 .. 10 => True, others => False)
               then
                  pattern (xmin (2) * charx - 1, (ymin (2) + 2 + i - 6) * chary - 8, i - 6, 0);
               end if;
               --  write the marker patterns box, X, diamond, and +
            end if;
         end;
         --  [P2Ada]: end of WITH
         --  with
      end loop;
      --  for
      TextCol (LightGray);
      Gotoxy (x_y_plot_text (4, 1), x_y_plot_text (4, 2) - 1);
      Put ("Start");
      Gotoxy (x_y_plot_text (6, 1) - 4, x_y_plot_text (6, 2) - 1);
      Put ("Stop");
      if not (Alt_Sweep)
      then
         --  write x-coordinate prefix
         Gotoxy (x_y_plot_text (5, 1), x_y_plot_text (5, 2));
         Put ("f  Hz");
         Write_Plot_Prefix (false);
      else
         x_sweep.Label_Axis;
      end if;
      --  use alternate sweep data
      --  Restore to normal scrolling
      WindMax := WindMax - 1;
   end Write_BigSmith_Coordinates;

   procedure calc_posO (x, y, theta, scf : Long_Float; sfreq : Integer;
                        dash : Boolean)
   is
      p2, p3, p4 : Long_Float;
   begin
      p2 := ((x) ** 2) + ((y) ** 2);
      if Sqrt (p2) < 1.02 * rho_fac
      then
         spline_in_smith := True;
         if abs (theta) > 0
         then
            --  *disabled***
            --  thet:=theta*freq/design_freq;
            --  sint:=sin(thet);
            --  cost:=cos(thet);
            --  spx:=Round(centerx+(x*cost-y*sint)*rad/rho_fac);
            --  spy:=Round((centery-(x*sint+y*cost)*rad*yf/rho_fac))*scf);
            --  **disabled**
            --  [P2Ada]: empty in Pascal
            null;
         else
            spx := Round (centerx + (x * rad / rho_fac));
            spy := Round ((centery - (y * rad * yf / rho_fac)) * scf);
         end if;
      else
         spline_in_smith := false;
      end if;
      --  * end if sqrt(p2)<1.02*rho_fac *
      if p2 > 1.0 / infty
      then
         --  !*	was Ln_Asm
         --  When compiled in the $N+ mode this
         --  Ln function worked only intermitently.
         --  The Ln function has therefore been
         --  rewritten in assembler. *!
         --  10*log(p2)
         p4 := Log (p2);
         p3 := 10.0 * p4 / ln10;
         p2 := p3;
      else
         p2 := - 1.0 * infty;
      end if;
      if betweenr (symin, p2, symax, sigma)
      then
         --  check to see if it fits
         if not (Large_Smith)
         then
            spline_in_rect := True;
         end if;
         spp := Round ((ymax (8) - (p2 - symin) * sfy1) * scf);
      else
         spline_in_rect := False;
         if p2 > symax
         then
            --  Put just at the top of the graph
            --  was ymin[8]-5 and ymax[8]+5
            spp := Round ((ymin (8) - 5) * scf);
         else
            spp := Round ((ymax (8) + 5) * scf);
         end if;
         --  Put at the bottom of the graph
      end if;
      --  else
      if dash and then not (betweeni (xmin (8), sfreq, xmax (8)))
      then
         spline_in_rect := False;
         spline_in_smith := False;
      end if;
   end calc_posO;

end pfun3;
