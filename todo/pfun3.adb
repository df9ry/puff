--  ParamStr,...
--  UpCase
--  For Shift_Left/Right
--  This is for Pi :
--  This is for Sqrt, Sin, Cos, etc. :
--  This is for Dispose. P2Ada writes automatically:
--  "Dispose is new Ada.Unchecked_Deallocation(<type>, <pointer type>)".
--  Units found in the Free Pascal's RTL's
--  Unit found in TURBO.TPL
--  Custom replacement unit for TUBO's "Crt" and "Graph" units
--  Add other puff units
--  [P2Ada]: place it before main procedure
with Ada.Text_IO;
use Ada.Text_IO;
with Ada.Integer_Text_IO;
use Ada.Integer_Text_IO;
with Ada.Float_Text_IO;
use Ada.Float_Text_IO;
with Ada.Long_Float_Text_IO;
use Ada.Long_Float_Text_IO;
with Ada.Direct_IO;
with Ada.Command_Line;
use Ada.Command_Line;
with Ada.Characters.Handling;
use Ada.Characters.Handling;
with Interfaces;
use Interfaces;
with Ada.Numerics;
use Ada.Numerics;
with Ada.Numerics.Elementary_Functions;
use Ada.Numerics.Elementary_Functions;
with Ada.Numerics.Long_Elementary_Functions;
use Ada.Numerics.Long_Elementary_Functions;
with Ada.Unchecked_Deallocation;
with Dos, Printer, xgraph, pfun1, pfun2;

package pfun3 is

   procedure write_freqO;

   procedure write_sO (ij : integer);

   procedure HighLight_Window;

   procedure Toggle_Circuit_Type;

   procedure Write_Board_Parameters;

   procedure Write_Expanded_Parts;

   procedure Write_Parts_ListO;

   procedure Write_Plot_Prefix (time2 : boolean);

   procedure Write_Coordinates (time : boolean);

   procedure Write_BigSmith_Coordinates;

   procedure restore_boxO (ij : integer);

   procedure move_boxO (xn, yn, nb : integer);

   procedure draw_ticksO (x1, y1, x2, y2 : integer; incx, incy : Long_Float);

   procedure Write_File_Name (fname : file_string);

   procedure calc_posO (x, y, theta, scf : Long_Float; sfreq : integer; dash : boolean);
   --  Included in Smith and Magplot are:
   --  procedure splineO(ij : integer);
   --  procedure smith_plotO(x1,y1,col : integer; Var linex : boolean);
   --  procedure rect_plotO(x1,y1,col : integer;Var linex : boolean);

   procedure Smith_and_Magplot (lighten, dash, boxes : boolean);
   --  Component Editing

   procedure Draw_Graph (x1, y1, x2, y2 : integer; time : boolean);

   procedure del_char (tcompt : compt);

   procedure back_char (tcompt : compt);

   procedure add_char (tcompt : compt);

   procedure choose_part (ky : Character);

   procedure draw_net (tnet : net);

   function con_found return boolean;

   function new_net (ports : integer; choice : boolean) return net;

   procedure dispose_net (vnet : net);

   function new_con (tnet : net; dirt : integer) return conn;

   procedure dispose_con (vcon : conn);

   procedure draw_port (mnet : net; col : integer);

   procedure draw_to_port (tnet : net; port_number : integer);

   procedure draw_ports (tnet : net);

   procedure node_look;

   procedure goto_port (port_number : integer);

   procedure new_port (x, y : Long_Float; port_number : integer);

   procedure Draw_Circuit;

   function off_boardO (step_size : Long_Float) return boolean;
   --  Get_Key includes:
   --  procedure draw_cursorO;
   --  procedure erase_cursorO;
   --  procedure ggotoxy(var cursor_displayed : boolean);

   procedure Get_Key;

   procedure ground_node;

   procedure unground;
   --  *******************************************************************

   procedure join_port (port_number, ivt : integer);
end pfun3;
--  Translated on 7-Feb-2024 by (New) P2Ada v. 28-Oct-2009
--  Translated on 7-Feb-2024 by (New) P2Ada v. 28-Oct-2009
--  The following with/use clauses are put graciously by P2Ada.
--  Some of them may be useless, your Ada compiler will tell it you.
--  (GNAT: with '-gnatwa')
--  ParamStr,...
--  UpCase
--  For Shift_Left/Right
--  This is for Pi :
--  This is for Sqrt, Sin, Cos, etc. :
--  This is for Dispose. P2Ada writes automatically:
--  "Dispose is new Ada.Unchecked_Deallocation(<type>, <pointer type>)".
with Ada.Text_IO;
use Ada.Text_IO;
with Ada.Integer_Text_IO;
use Ada.Integer_Text_IO;
with Ada.Float_Text_IO;
use Ada.Float_Text_IO;
with Ada.Long_Float_Text_IO;
use Ada.Long_Float_Text_IO;
with Ada.Direct_IO;
with Ada.Command_Line;
use Ada.Command_Line;
with Ada.Characters.Handling;
use Ada.Characters.Handling;
with Interfaces;
use Interfaces;
with Ada.Numerics;
use Ada.Numerics;
with Ada.Numerics.Elementary_Functions;
use Ada.Numerics.Elementary_Functions;
with Ada.Numerics.Long_Elementary_Functions;
use Ada.Numerics.Long_Elementary_Functions;
with Ada.Unchecked_Deallocation;

package body pfun3 is
   --  [P2Ada]: This is for 'Write([Boolean])'

   package Boolean_Text_IO is new Enumeration_IO (Boolean);
   use Boolean_Text_IO;
   --  [P2Ada]: This is for 'file' without type
   --  [P2Ada]: This is for the Halt pseudo-procedure

   package Byte_Direct_IO is new Ada.Direct_IO (Unsigned_8);
   Program_halted : exception;
   --  *
   --  Write frequency in the plot window box
   --  as the marker is moved.
   --  Makes it's own calculation to determine freq.
   --  *

   procedure write_freqO is
   begin
      TextCol (lightgray);
      if Alt_Sweep
      then
         --  if alt_sweep put in x_sweep part label and unit label
         --  else put in 'f' and 'Hz'
         --  [P2Ada]: !Help! Maybe (file,...) here
         --  Frequency prefix (k,M,G,etc.) is freq_prefix
         x_sweep.Label_Plot_Box;
      else
         GotoXY (xmin (2) + 4, ymin (2) + 2);
         Put ('f');
         GotoXY (xmin (2) + 16, ymin (2) + 2);
         Put (freq_prefix);
         Put ("Hz");
      end if;
      --  Now write the number
      --  These parameters are all normalized
      --  was 40,20
      freq := fmin + xpt * finc;
      TextCol (green);
      GotoXY (xmin (2) + 6, ymin (2) + 2);
      Put (freq, 8, 4, 0);
   end write_freqO;
   --  write_freqO
   --  *
   --  Write s-parameters in plot window box as marker is moved.
   --  *

   procedure write_sO (ij : integer) is
      --  was mgotoxy, was 43,20
      rho, lnrho, deg : Long_Float;
   begin
      GotoXY (xmin (2) + 5, ymin (2) + 2 + ij);
      rho := ((c_plot (ij).all.x) ** 2) + ((c_plot (ij).all.y) ** 2);
      deg := atan2 (c_plot (ij).all.x, c_plot (ij).all.y);
      if rho > 1.0e - 10
      then
         if rho < 1.0e + 10
         then
            --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
            lnrho := 10 * Log (rho) / ln10;
            if s_param_table (ij).all.descript (2) and ('f' | 'F' => True, others => False)
            then
               lnrho := lnrho / 2.0;
            end if;
            Put (lnrho, 7, 2, 0);
            Put ("dB");
            Put (deg, 6, 1, 0);
            Put (degree);
         else
            Put ("   ");
            Put (infin);
            Put (ity);
            Put ("           ");
         end if;
      else
         Put ("   0            ");
      end if;
   end write_sO;
   --  * Write_sO *
   --  *
   --  Causes F# key to be highlighted on the screen
   --  when that window has been selected.
   --  *

   procedure HighLight_Window is
      --  highlight selected window
   begin
      GotoXY (window_f (window_number).all.xp + 1, window_f (window_number).all.yp);
      TextCol (white);
      Put ('F');
      Put (window_number);
   end HighLight_Window;
   --  * Highlight_Window *
   --  *
   --  Toggle between stripline and microstrip.
   --  Called by Board4 in PUFF20.
   --  *

   procedure Toggle_Circuit_Type is
   begin
      Board_Changed := true;
      TextCol (lightgray);
      GotoXY (xmin (4), ymin (4) + 6);
      Put ("Tab  ");
      if Manhattan_Board
      then
         stripline := true;
         Manhattan_Board := false;
         Put ("stripline ");
      else
         if stripline
         then
            --  make calculations easier
            stripline := false;
            Put ("microstrip");
         else
            Manhattan_Board := true;
            stripline := true;
            Put ("Manhattan ");
         end if;
      end if;
   end Toggle_Circuit_Type;
   --  *
   --  Write board parameter in screen area.
   --  *

   procedure Write_Board_Parameters is
      --  erase and write border in board color
      --  write BOARD Header
      tcompt : compt;
   begin
      Make_Text_Border (xmin (4) - 1, ymin (4) - 1, xmax (4) + 1, ymax (4) + 1, col_window (4), true);
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
            write_compt (lightgray, tcompt);
            exit when tcompt.all.next_compt = null;
         end loop;
         TextCol (lightgray);
         GotoXY (xmin (4), ymin (4) + 6);
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
   --  * Write_Board_Parameters *
   --  *
   --  Erase parts list area, write zd and fd,
   --  draw window box, list parts.
   --  Called only by Read_Net.
   --  *

   procedure Write_Expanded_Parts is
      tcompt : compt;
   begin
      Make_Text_Border (xmin (3) - 1, ymin (3) - 1, xmax (5) + 1, ymax (5) + 1, col_window (3), true);
      write_compt (col_window (3), window_f (3));
      if (window_number = 3)
      then
         Highlight_Window;
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
            write_compt (lightgray, tcompt);
            exit when tcompt.all.next_compt = null;
         end loop;
      end if;
   end Write_Expanded_Parts;
   --  * Write_Expanded_Parts *
   --  *
   --  Erase parts list area, write zd and fd,
   --  draw window box, list parts.
   --  Called only by Read_Net.
   --  *

   procedure Write_Parts_ListO is
      tcompt : compt;
      y : integer;
   begin
      Make_Text_Border (xmin (3) - 1, ymin (3) - 1, xmax (3) + 1, ymax (3) + 1, col_window (3), true);
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
            write_compt (lightgray, tcompt);
            y := y + 1;
            exit when y = ymax (3) + 1;
         end loop;
      end if;
   end Write_Parts_ListO;
   --  * Write_Parts_ListO *
   --  *
   --  Find the frequency or time unit prefix and write
   --  it in the plot window.
   --  *

   procedure Write_Plot_Prefix (time2 : boolean) is
      Freq_Prefix : string := "EPTGMk m" + Mu + "npfa";
      Time_Prefix : string := "afpn" + Mu + "m kMGTPE";
      i : integer;
   begin
      GotoXY (x_y_plot_text (5, 1) + 2, x_y_plot_text (5, 2));
      TextCol (lightgray);
      if time2
      then
         i := 0;
         loop
            i := i + 1;
            exit when (Freq_Prefix (i) = s_board (2, 2));
         end loop;
         --  find index for frequency prefix
         --  write time prefix
         Put (Time_Prefix (i));
      else
         Put (s_board (2, 2));
      end if;
      --  write frequency prefix
   end Write_Plot_Prefix;
   --  * Write_Plot_Prefix *
   --  *
   --  Write parameters in the plot window
   --  *

   procedure Write_Coordinates (time : boolean) is
      --  to prevent scrolling
      i : integer;
      tcompt : compt;
      temp : line_string;
   begin
      WindMax := WindMax + 1;
      if time
      then
         --  was 56-
         --  was 56-
         --  was 56,13
         --  was 53,6
         --  was 66,13
         TextCol (lightgray);
         temp := rho_fac_compt.all.descript;
         Delete (temp, 1, 13);
         gotoxy (x_y_plot_text (1, 1) - (temp'length), x_y_plot_text (1, 2));
         Put (temp);
         temp := '-' + temp;
         gotoxy (x_y_plot_text (3, 1) - (temp'length), x_y_plot_text (3, 2));
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
            declare P2Ada_Var_1 : < > renames tcompt.all;
            begin
               if right
               then
                  xp := xorig - (descript'length);
               end if;
               if ((descript'length) > x_block) or else (i < 7)
               then
                  --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
                  write_compt (lightgray, tcompt);
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
         --  [P2Ada]: !Help! Maybe (file,...) here
         --  [P2Ada]: !Help! Maybe (file,...) here
         --  was 53,6
         --  was 53,7
         TextCol (lightgray);
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
   --  * Write_Coordinates *
   --  *
   --  Write coordinates for the VGA Big Smith window
   --  Linked list for coordinates has been reduced to
   --  8 elements (no dBmax, dBmin).
   --  *

   procedure Write_BigSmith_Coordinates is
      --  Erase key area
      --  clear key area
      --  to prevent scrolling
      i : integer;
      tcompt : compt;
   begin
      clear_window (xmin (2), ymin (2), xmax (2), ymax (2));
      TextCol (lightgray);
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
         declare P2Ada_Var_2 : < > renames tcompt.all;
         begin
            if right
            then
               xp := xorig - (descript'length);
            end if;
            if ((descript'length) > x_block) or else (i < 7)
            then
               --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
               write_compt (lightgray, tcompt);
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
      TextCol (lightgray);
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
   --  * Write_BigSmith_Coordinates *
   --  *
   --  Restore pixels that were covered by marker.
   --  *

   procedure restore_boxO (ij : integer) is
      nb, k, xn, yn : integer;
   begin
      for k in 0 .. 1
      loop
         nb := ij + k * max_params;
         if box_filled (nb)
         then
            xn := box_dot (1, nb);
            yn := box_dot (2, nb);
            PutBox (nb, xn - 4, yn - 4, 9, 9);
         end if;
         --  if box_filled
      end loop;
      --  for k
   end restore_boxO;
   --  restore_boxO
   --  *
   --  Move marker by first storing the dots that will be covered.
   --  EGA routine saves in an array.
   --  *

   procedure move_boxO (xn, yn, nb : integer) is
   begin
      box_filled (nb) := true;
      box_dot (1, nb) := xn;
      box_dot (2, nb) := yn;
      GetBox (nb, xn - 4, yn - 4, 9, 9);
   end move_boxO;
   --  move_boxO
   --  *
   --  Draw ticks on rectangular plot.
   --  *

   procedure draw_ticksO (x1, y1, x2, y2 : integer; incx, incy : Long_Float) is
      --  was 8080
      i, xinc, yinc : integer;
   begin
      SetLineStyle (UserBitLn, 16 #08 88 #0F ormWidth);
      SetCol (Green);
      for i in 1 .. 9
      loop
         xinc := Round (i * (x2 - x1) / 10.0);
         yinc := Round (i * (y2 - y1) / 5.0);
         Line (x1 + xinc, y1 - 1, x1 + xinc, y2);
         if (i < 5)
         then
            Line (x1, y1 + yinc, x2 - 1, y1 + yinc);
         end if;
      end loop;
      --  for
      SetLineStyle (SolidLn, 0, NormWidth);
   end draw_ticksO;
   --  draw_ticksO
   --  *
   --  Write file name above Parts.
   --  Remove any subdirectory information and .puf
   --  *

   procedure Write_File_Name (fname : file_string) is
      --  Erase old file name
      i : integer;
      temp_str : string (1 .. 19);
   begin
      GotoXY (filename_position (1), filename_position (2));
      for i in filename_position (1) .. filename_position (3)
      loop
         Put (' ');
      end loop;
      --  Remove dir from filename
      loop
         i := Pos ('\', fname);
         if i > 0
         then
            Delete (fname, 1, i);
         end if;
         exit when i = 0;
      end loop;
      --  Write filename on screen
      temp_str := "file : " + fname;
      TextCol (col_window (1));
      GotoXY (filename_position (1) + (filename_position (3) - filename_position (1) - (temp_str'length)) / 2, filename_position (2));
      Put (temp_str);
   end Write_File_Name;
   --  * Write_File_Name *
   --  *
   --  Given the complex s-parameter co(x,y):=rho find where the dot
   --  should be plotted on screen. Plotting parameters which are
   --  returned are (spx,spy) for the Smith plot and (spp) for the
   --  rectangular plot. Clipping is also performed here by checking
   --  the values of the returned parameters to see if they lie within
   --  the limits of symin,symax and xmin[8],xmax[8]. If not they are
   --  put at ymax[8] and ymin[8].
   --  Screen magnification set by:
   --  if its_EGA then scf:=1 else scf:=hir;
   --  in Smith_and_Magplot
   --  *

   procedure calc_posO (x, y, theta, scf : Long_Float; sfreq : integer; dash : boolean) is
      p2, p3, p4 : Long_Float;
   begin
      p2 := ((x) ** 2) + ((y) ** 2);
      if sqrt (p2) < 1.02 * rho_fac
      then
         spline_in_smith := true;
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
            spline_in_rect := true;
         end if;
         spp := Round ((ymax (8) - (p2 - symin) * sfy1) * scf);
      else
         spline_in_rect := false;
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
         spline_in_rect := false;
         spline_in_smith := false;
      end if;
   end calc_posO;
   --  calc_posO
   --  ***********************************************************************
   --  *
   --  Plot s-parameter curves.  This is done after all
   --  the data points have been calculated
   --  from the Analysis procedure.
   --  Now includes Smith_PlotO, Rect_PlotO, SplineO
   --  *

   procedure Smith_and_Magplot (lighten, dash, boxes : boolean) is
      --  ****************************************************************
      jxsdif, scf, cx1, cx2, cx3, cx4, cy1, cy2, cy3, cy4, sqfmfj, sqfjmf, fmfj, fjmf, spar1, spar2 : Long_Float;
      sfreq, jfreq, j, nopts, ij, col, txpt : integer;
      line_s, line_r : boolean;
      cplt : plot_param;
      cspc : spline_param;
      --  *
      --  Calculate spline coefficients.
      --  Johnson and Riess Numerical Analysis p41,241.
      --  *

      procedure splineO (ij : integer) is
         --  SplineO
         zx, zy, u : array (0 .. 1000) of Long_Float;
         m, i : integer;
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
            declare P2Ada_Var_3 : < > renames cspc.all;
            begin
               --  [P2Ada]: WITH instruction
               --  [P2Ada]: !Help! No type found -> add 'P2Ada_Var_4.' to fields
               declare P2Ada_Var_4 : < > renames cplt.all;
               begin
                  h := sqrt (((yf * (next_p.all.y - y)) ** 2) + ((next_p.all.x - x) ** 2));
                  if h < 0.000001
                  then
                     h := 0.000001;
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
         u (1) := 2 * (cspc.all.next_c.all.h + cspc.all.h);
         zx (1) := 6 * ((cplt.all.next_p.all.next_p.all.x - cplt.all.next_p.all.x) / cspc.all.next_c.all.h - (cplt.all.next_p.all.x - cplt.all.x) / cspc.all.h);
         zy (1) := 6 * ((cplt.all.next_p.all.next_p.all.y - cplt.all.next_p.all.y) / cspc.all.next_c.all.h - (cplt.all.next_p.all.y - cplt.all.y) / cspc.all.h);
         for i in 2 .. m
         loop
            --  [P2Ada]: WITH instruction
            --  [P2Ada]: !Help! No type found -> add 'P2Ada_Var_5.' to fields
            cplt := cplt.all.next_p;
            cspc := cspc.all.next_c;
            declare P2Ada_Var_5 : < > renames cspc.all;
            begin
               --  [P2Ada]: WITH instruction
               --  [P2Ada]: !Help! No type found -> add 'P2Ada_Var_6.' to fields
               declare P2Ada_Var_6 : < > renames cplt.all;
               begin
                  --  a_i-1i=a_ii-1=h_i-1,a_ii=2(h_i+h_i-1)
                  --  u_ii=a_ii-L_i.i-1a_i-1,i
                  --  2.33
                  li := h / u (i - 1);
                  u (i) := 2 * (next_c.all.h + h) - h * li;
                  zx (i) := 6 * ((next_p.all.next_p.all.x - next_p.all.x) / next_c.all.h - (next_p.all.x - x) / h) - li * zx (i - 1);
                  zy (i) := 6 * ((next_p.all.next_p.all.y - next_p.all.y) / next_c.all.h - (next_p.all.y - y) / h) - li * zy (i - 1);
               end;
               --  [P2Ada]: end of WITH
            end;
            --  [P2Ada]: end of WITH
            --  with
         end loop;
         --  for i
         cspc := spline_end;
         cspc.all.sx := 0;
         cspc.all.sy := 0;
         cspc := cspc.all.prev_c;
         cspc.all.sx := zx (m) / u (m);
         cspc.all.sy := zy (m) / u (m);
         for i in 1 .. m - 1
         loop
            --  [P2Ada]: WITH instruction
            --  [P2Ada]: !Help! No type found -> add 'P2Ada_Var_7.' to fields
            cspc := cspc.all.prev_c;
            declare P2Ada_Var_7 : < > renames cspc.all;
            begin
               sx := (zx (m - i) - h * next_c.all.sx) / u (m - i);
               sy := (zy (m - i) - h * next_c.all.sy) / u (m - i);
            end;
            --  [P2Ada]: end of WITH
         end loop;
         cspc := cspc.all.prev_c;
         cspc.all.sx := 0;
         cspc.all.sy := 0;
      end splineO;
      --  splineO
      --  **************************************************************
      --  *
      --  Plot curve on Smith plot
      --  *

      procedure smith_plotO (x1, y1, col : integer; linex : in out boolean) is
         --  Smith_PlotO
      begin
         if spline_in_smith
         then
            if linex
            then
               SetCol (col);
               Line (xvalo (1), yvalo (1), x1, y1);
            end if;
            --  if linex
            --  if spline_
            linex := true;
         else
            linex := false;
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

      procedure rect_plotO (x1, y1, col : integer; linex : in out boolean) is
         --  Rect_PlotO
         --  clip rectangular plot, plot with relative positioning,
         --  and reset the graphics pointer
      begin
         SetViewPort (xmin (8), ymin (8) - 1, xmax (8), ymax (8), true);
         if linex
         then
            SetCol (col);
            Line (xvalo (2) - xmin (8), yvalo (2) - ymin (8) + 1, x1 - xmin (8), y1 - ymin (8) + 1);
         end if;
         --  Remove clipping
         linex := true;
         SetViewPort (xmin (12), ymin (12), xmax (12), ymax (12), false);
         xvalo (2) := x1;
         yvalo (2) := y1;
      end rect_plotO;
      --  rect_plotO
      --  **********************************************************
      --  * Smith_and_Magplot *
      --  difference in x between plot points
   begin
      jxsdif := sfx1 * finc;
      scf := 1;
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
               line_s := false;
               line_r := false;
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
                  freq := fmin + txpt * finc;
                  sfreq := xmin (8) + Round ((freq - sxmin) * sfx1);
                  if cplt.all.filled
                  then
                     calc_posO (cplt.all.x, cplt.all.y, 0, scf, sfreq, dash);
                     if not (Large_Smith)
                     then
                        rect_plotO (sfreq, spp, col, line_r);
                        if spline_in_rect and then boxes
                        then
                           box (sfreq, Round (spp / scf), ij);
                        end if;
                     end if;
                     smith_plotO (spx, spy, col, line_s);
                     if spline_in_smith and then boxes
                     then
                        box (spx, Round (spy / scf), ij);
                     end if;
                     if txpt < npts
                     then
                        --  [P2Ada]: WITH instruction
                        --  [P2Ada]: !Help! No type found -> add 'P2Ada_Var_8.' to fields
                        declare P2Ada_Var_8 : < > renames cspc.all;
                        begin
                           --  [P2Ada]: WITH instruction
                           --  [P2Ada]: !Help! No type found -> add 'P2Ada_Var_9.' to fields
                           declare P2Ada_Var_9 : < > renames cplt.all;
                           begin
                              cx1 := sx / (6 * h);
                              cx2 := next_c.all.sx / (6 * h);
                              cy1 := sy / (6 * h);
                              cy2 := next_c.all.sy / (6 * h);
                              cx3 := next_p.all.x / h - next_c.all.sx * h / 6;
                              cx4 := x / h - sx * h / 6;
                              cy3 := next_p.all.y / h - next_c.all.sy * h / 6;
                              cy4 := y / h - sy * h / 6;
                              if h * rad / rho_fac > 40
                              then
                                 nopts := 10;
                              else
                                 nopts := Round (h * rad / (rho_fac * 4)) + 1;
                              end if;
                              for j in 1 .. nopts - 1
                              loop
                                 fmfj := j * h / nopts;
                                 fjmf := h - fmfj;
                                 sqfmfj := ((fmfj) ** 2);
                                 sqfjmf := ((fjmf) ** 2);
                                 spar1 := (cx1 * sqfjmf + cx4) * fjmf + (cx2 * sqfmfj + cx3) * fmfj;
                                 spar2 := (cy1 * sqfjmf + cy4) * fjmf + (cy2 * sqfmfj + cy3) * fmfj;
                                 jfreq := Round (j * jxsdif / nopts);
                                 calc_posO (spar1, spar2, 0, scf, sfreq + jfreq, dash);
                                 if not (Large_Smith)
                                 then
                                    rect_plotO (sfreq + jfreq, spp, col, line_r);
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
   --  *Smith_and_Magplot*
   --  ***************************************************************************
   --  *
   --  Draw rectangular graph.
   --  *

   procedure Draw_Graph (x1, y1, x2, y2 : integer; time : boolean) is
      --  erase previous graph
   begin
      clear_window_gfx (xmin (11), ymin (11), xmax (11), ymax (11));
      if not (time)
      then
         clear_window (xmin (2), ymin (2), xmax (2), ymax (2));
      end if;
      --  clear key area
      TextCol (lightgray);
      Write_Coordinates (time);
      draw_box (x1, y1, x2, y2, lightgreen);
      draw_ticksO (x1, y1, x2, y2, (x2 - x1) / 4.0, (y2 - y1) / 4.0);
   end Draw_Graph;
   --  * Draw_Graph *
   --  ******************  START COMPONENT MANIPULATION   ****************
   --  *
   --  Delete character -- Del.
   --  *

   procedure del_char (tcompt : compt) is
   begin
      if (window_number = 2)
      then
         WindMax := WindMax + 1;
      end if;
      --  prevent scrolling
      tcompt.all.changed := true;
      delete (tcompt.all.descript, cx + 1, 1);
      write_compt (lightgray, ccompt);
      Put (' ');
      gotoxy (tcompt.all.xp + cx, tcompt.all.yp);
      if (window_number = 2)
      then
         WindMax := WindMax - 1;
      end if;
      --  allow scrolling
   end del_char;
   --  del_char
   --  *
   --  Backspace and delete character.
   --  *

   procedure back_char (tcompt : compt) is
   begin
      if cx > tcompt.all.x_block
      then
         gotoxy (tcompt.all.xp + cx, tcompt.all.yp);
         cx := cx - 1;
         del_char (tcompt);
      end if;
   end back_char;
   --  back_char
   --  *
   --  Add character to parameter or part.
   --  Allows only 1..4 to be added to s-parameters
   --  *

   procedure add_char (tcompt : compt) is
      i, lendes : integer;
      an_s : boolean;
   begin
      TextCol (white);
      an_s := false;
      for i in 1 .. 4
      loop
         if (s_param_table (i) = tcompt)
         then
            an_s := true;
         end if;
      end loop;
      --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
      if an_s and then not (key and (' ' | '1' .. '4' => True, others => False))
      then
         --  ignore if not 1..4 for S's
         --  [P2Ada]: WITH instruction
         --  [P2Ada]: !Help! No type found -> add 'P2Ada_Var_10.' to fields
         beep;
      else
         declare P2Ada_Var_10 : < > renames tcompt.all;
         begin
            if not (insert_key)
            then
               delete (descript, cx + 1, 1);
            end if;
            insert (key, descript, cx + 1);
            lendes := (descript'length);
            if lendes > xmaxl
            then
               erase_message;
               message (2) := "Line too long";
               delete (descript, lendes, 1);
               write_message;
            end if;
            cx := cx + 1;
            if cx > xmaxl
            then
               cx := cx - 1;
            end if;
            if (window_number = 2)
            then
               WindMax := WindMax + 1;
            end if;
            --  prevent scrolling
            if right
            then
               if (xp + (descript'length) - 1 >= xorig) or else (xp + cx >= xorig)
               then
                  gotoxy (xorig - 1, yp);
                  Put (' ');
                  xp := xp - 1;
               end if;
            end if;
            write_compt (lightgray, tcompt);
            changed := true;
            gotoxy (xp + cx, yp);
            if (window_number = 2)
            then
               WindMax := WindMax - 1;
            end if;
            --  allow scrolling
         end;
         --  [P2Ada]: end of WITH
      end if;
      --  else with tcompt
   end add_char;
   --  add_char
   --  *
   --  Select one of the parts [a..r].
   --  *

   procedure Choose_Part (ky : Character) is
      --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
      tcompt : compt;
      found : boolean;
   begin
      if ky and ('A' .. 'R' => True, others => False)
      then
         ky := Character (Character'Pos (ky) + 32);
      end if;
      tcompt := null;
      found := false;
      missing_part := false;
      loop
         if tcompt = null
         then
            tcompt := part_start;
         else
            tcompt := tcompt.all.next_compt;
         end if;
         if (tcompt.all.descript (1) = ky) and then tcompt.all.parsed
         then
            found := true;
         end if;
         exit when (tcompt.all.next_compt = null) or else found;
      end loop;
      --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
      if found and then not ((ky and ('j' .. 'r' => True, others => False)) and then not (Large_Parts))
      then
         write_compt (lightgray, compt1);
         compt1 := tcompt;
         write_compt (white, compt1);
      else
         message (1) := ky + " is not a";
         message (2) := "valid part";
         update_key := false;
         missing_part := true;
      end if;
   end Choose_Part;
   --  * Choose_Part *
   --  *******************  CIRCUIT DRAWING Functions ******************
   --  *
   --  Calls routine to draw net on circuit board.
   --  Drawing routines are in PFUN2
   --  *

   procedure Draw_Net (tnet : net) is
   begin
      lengthxy (tnet);
      if read_kbd or else demo_mode
      then
         case tnet.all.com.all.typ is
            when 't' =>
               Draw_tline (tnet, true, false);
            when 'q' =>
               Draw_tline (tnet, true, false);
            when 'l' =>
               Draw_tline (tnet, false, false);
            when 'x' =>
               --  transformer
               Draw_xformer (tnet);
            when 'a' =>
               --  attenuator
               Draw_tline (tnet, false, false);
            when 'd' | 'i' =>
               Draw_device (tnet);
            when 'c' =>
               Draw_tline (tnet.all.other_net, true, false);
               Draw_tline (tnet, true, true);
            when others =>
               --  [P2Ada]: no otherwise / else in Pascal
               null;
         end case;
      end if;
      --  case
   end Draw_Net;
   --  * Draw_Net *
   --  *
   --  Looks for ccon on cnet in direction of arrow.
   --  On exit cnet=network to remove or step over.
   --  If ccon is connected to an external port then
   --  cnet is unchanged.
   --  *

   function con_found return boolean is
      Result_con_found : boolean;
      found : boolean;
   begin
      ccon := null;
      found := false;
      port_dirn_used := false;
      if cnet /= null
      then
         loop
            if ccon = null
            then
               ccon := cnet.all.con_start;
            else
               ccon := ccon.all.next_con;
            end if;
            if (dirn and then ccon.all.dir) > 0
            then
               found := true;
            end if;
            exit when found or else (ccon.all.next_con = null);
         end loop;
         if found
         then
            if ext_port (ccon)
            then
               --  * Delete to disallow "Paths over ports" *
               message (1) := "Cannot go over";
               message (2) := "path to port";
               port_dirn_used := true;
               update_key := false;
            else
               cnet := ccon.all.mate.all.net;
            end if;
         end if;
         --  if found
      end if;
      --  if cnet
      Result_con_found := found;
      return Result_con_found;
   end con_found;
   --  con_found
   --  *
   --  Makes a new network on the end of the linked list.
   --  If choice then network is node else network is part.
   --  *

   function new_net (ports : integer; choice : boolean) return net is
      Result_new_net : net;
      tnet : net;
   begin
      if net_start = null
      then
         New_n (net_start);
         tnet := net_start;
      else
         tnet := net_start;
         while tnet.all.next_net /= null
         loop
            tnet := tnet.all.next_net;
         end loop;
         New_n (tnet.all.next_net);
         tnet := tnet.all.next_net;
      end if;
      --  [P2Ada]: WITH instruction
      --  [P2Ada]: !Help! No type found -> add 'P2Ada_Var_11.' to fields
      declare P2Ada_Var_11 : < > renames tnet.all;
      begin
         next_net := null;
         node := choice;
         con_start := null;
         ports_connected := 0;
         number_of_con := ports;
         xr := xm;
         yr := ym;
         if node
         then
            grounded := false;
            com := null;
         else
            com := compt1;
         end if;
      end;
      --  [P2Ada]: end of WITH
      --  with
      Result_new_net := tnet;
      if not (tnet.all.node)
      then
         if compt1.all.typ = 'c'
         then
            --  [P2Ada]: WITH instruction
            --  [P2Ada]: !Help! No type found -> add 'P2Ada_Var_12.' to fields
            New_n (tnet.all.other_net);
            tnet := tnet.all.other_net;
            declare P2Ada_Var_12 : < > renames tnet.all;
            begin
               com := compt1;
               dirn_xy;
               xr := xm + yii * compt1.all.con_space;
               yr := ym + xii * compt1.all.con_space;
            end;
            --  [P2Ada]: end of WITH
            --  with
         end if;
      end if;
      --  if ccompt1
      return Result_new_net;
   end new_net;
   --  new_net
   --  *
   --  Remove a network form the linked list.
   --  *

   procedure dispose_net (vnet : net) is
      found : boolean;
      tnet : net;
   begin
      tnet := null;
      found := false;
      loop
         if tnet = null
         then
            tnet := net_start;
            if tnet = vnet
            then
               --  if tnet=vnet
               net_start := net_start.all.next_net;
               tnet := net_start;
               found := true;
            end if;
            --  if tnet <> nil
         else
            if tnet.all.next_net = vnet
            then
               found := true;
               tnet.all.next_net := tnet.all.next_net.all.next_net;
            else
               tnet := tnet.all.next_net;
            end if;
         end if;
         --  if tnet <> nil
         exit when found or else (tnet.all.next_net = null);
      end loop;
      if not (found)
      then
         message (2) := "dispose_net";
         shutdown;
      end if;
   end dispose_net;
   --  dispose_net
   --  *
   --  Make a new connector.
   --  *

   function new_con (tnet : net; dirt : integer) return conn is
      Result_new_con : conn;
      tcon : conn;
   begin
      if tnet.all.con_start = null
      then
         New_conn (tnet.all.con_start);
         tcon := tnet.all.con_start;
      else
         tcon := tnet.all.con_start;
         while tcon.all.next_con /= null
         loop
            tcon := tcon.all.next_con;
         end loop;
         New_conn (tcon.all.next_con);
         tcon := tcon.all.next_con;
      end if;
      --  [P2Ada]: WITH instruction
      --  [P2Ada]: !Help! No type found -> add 'P2Ada_Var_13.' to fields
      declare P2Ada_Var_13 : < > renames tcon.all;
      begin
         port_type := 0;
         next_con := null;
         net := tnet;
         cxr := xm;
         dir := dirt;
         cyr := ym;
      end;
      --  [P2Ada]: end of WITH
      --  with tcon^
      --  [P2Ada]: WITH instruction
      --  [P2Ada]: !Help! No type found -> add 'P2Ada_Var_14.' to fields
      declare P2Ada_Var_14 : < > renames tnet.all;
      begin
         if node and then (number_of_con > 1)
         then
            xr := (xr * (number_of_con - 1) + xm) / number_of_con;
            yr := (yr * (number_of_con - 1) + ym) / number_of_con;
         end if;
      end;
      --  [P2Ada]: end of WITH
      Result_new_con := tcon;
      return Result_new_con;
   end new_con;
   --  new_con
   --  *
   --  Dispose a connector.
   --  *

   procedure dispose_con (vcon : conn) is
      found : boolean;
      tcon : conn;
      vnet : net;
      i : integer;
   begin
      tcon := null;
      found := false;
      vnet := vcon.all.net;
      vnet.all.number_of_con := vnet.all.number_of_con - 1;
      if vnet.all.number_of_con = 0
      then
         dispose_net (vnet);
      else
         loop
            if tcon = null
            then
               tcon := vnet.all.con_start;
               if tcon = vcon
               then
                  vnet.all.con_start := vcon.all.next_con;
                  found := true;
               end if;
            else
               if tcon.all.next_con = vcon
               then
                  found := true;
                  tcon.all.next_con := tcon.all.next_con.all.next_con;
               else
                  tcon := tcon.all.next_con;
               end if;
            end if;
            exit when found or else (tcon.all.next_con = null);
         end loop;
         if not (found)
         then
            message (2) := "dispose_con";
            shutdown;
         end if;
      end if;
      --  if vcon
      --  [P2Ada]: WITH instruction
      --  [P2Ada]: !Help! No type found -> add 'P2Ada_Var_15.' to fields
      declare P2Ada_Var_15 : < > renames vnet.all;
      begin
         if node and then (number_of_con > 0)
         then
            for i in 1 .. number_of_con
            loop
               if i = 1
               then
                  tcon := vnet.all.con_start;
                  xr := 0;
                  yr := 0;
               else
                  tcon := tcon.all.next_con;
               end if;
               xr := xr + tcon.all.cxr / number_of_con;
               yr := yr + tcon.all.cyr / number_of_con;
            end loop;
         end if;
      end;
      --  [P2Ada]: end of WITH
      --  for i
   end dispose_con;
   --  dispose_con
   --  *
   --  Draws a small box and number for an external port.
   --  *

   procedure Draw_Port (mnet : net; col : integer) is
      x, y, i, yj : integer;
   begin
      SetCol (col);
      x := xmin (1) + Round (mnet.all.xr / csx);
      y := ymin (1) + Round (mnet.all.yr / csy);
      i := 0;
      yj := 0;
      case mnet.all.ports_connected is
         when 1 | 3 =>
            --  i:= 0;
            SetTextJustify (RightText, CenterText);
            i := - 3;
         when 2 | 4 =>
            --  i:= 2;
            SetTextJustify (LeftText, CenterText);
            i := 5;
         when others =>
            --  [P2Ada]: no otherwise / else in Pascal
            null;
      end case;
      --  case
      --  write number
      yj := y;
      OutTextXY (x + i, yj, Character'Val (48 + mnet.all.ports_connected));
      fill_box (x - 2, y - 2, x + 2, y + 2, col);
   end Draw_Port;
   --  draw_port
   --  *
   --  Draws a connectinon to an external port.
   --  *

   procedure draw_to_port (tnet : net; port_number : integer) is
      xp, yp, offset, xli, yli, xsn, ysn : integer;
   begin
      portnet (port_number).all.node := true;
      xsn := cwidthxZ02;
      ysn := cwidthyZ02;
      case port_number is
         when 1 | 3 =>
            offset := 2;
         when 2 | 4 =>
            offset := - 2;
            xsn := - xsn;
         when others =>
            --  [P2Ada]: no otherwise / else in Pascal
            null;
      end case;
      --  case
      xli := Round (tnet.all.xr / csx) + xmin (1);
      yli := Round (tnet.all.yr / csy) + ymin (1);
      xp := Round (portnet (port_number).all.xr / csx) + offset + xmin (1);
      yp := Round (portnet (port_number).all.yr / csy) + ymin (1);
      if yli < yp
      then
         ysn := - ysn;
      end if;
      if abs (tnet.all.yr - portnet (port_number).all.yr) < widthz0 / 2.0
      then
         puff_draw (xp, yp + ysn, xli, yp + ysn, lightgray);
         puff_draw (xli, yp - ysn, xli, yp + ysn, lightgray);
         puff_draw (xp, yp - ysn, xli, yp - ysn, lightgray);
      else
         puff_draw (xli - xsn, yli, xli + xsn, yli, lightgray);
         puff_draw (xli + xsn, yli, xli + xsn, yp, lightgray);
         puff_draw (xli + xsn, yp, xli, yp - ysn, lightgray);
         puff_draw (xli, yp - ysn, xp, yp - ysn, lightgray);
         puff_draw (xp, yp + ysn, xli - xsn, yp + ysn, lightgray);
         puff_draw (xli - xsn, yp + ysn, xli - xsn, yli, lightgray);
      end if;
      Draw_port (portnet (port_number), LightRed);
   end draw_to_port;
   --  draw_to_port
   --  *
   --  Loops over tnet's connections to external ports.
   --  *

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
   --  draw_ports
   --  *
   --  Looks for a node at current cursor postion.
   --  *

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
            --  [P2Ada]: WITH instruction
            --  [P2Ada]: !Help! No type found -> add 'P2Ada_Var_16.' to fields
            declare P2Ada_Var_16 : < > renames tnet.all;
            begin
               if (con_start /= null)
               then
                  if (abs (con_start.all.cxr - xm) < resln) and then node and then (abs (con_start.all.cyr - ym) < resln)
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
   --  node_look
   --  *
   --  Go to an external port.
   --  *

   procedure goto_port (port_number : integer) is
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
   --  goto_port
   --  *
   --  Make a new external port.
   --  *

   procedure new_port (x, y : Long_Float; port_number : integer) is
      --  [P2Ada]: WITH instruction
      --  [P2Ada]: !Help! No type found -> add 'P2Ada_Var_17.' to fields
   begin
      New_n (portnet (port_number));
      declare P2Ada_Var_17 : < > renames portnet (port_number).all;
      begin
         --  not_connected yet
         next_net := null;
         number_of_con := 0;
         con_start := null;
         xr := x;
         yr := y;
         ports_connected := port_number;
         node := false;
      end;
      --  [P2Ada]: end of WITH
      --  with
   end new_port;
   --  new port
   --  *
   --  Draw the entire circuit.
   --  *

   procedure Draw_Circuit is
      --  * Clear Layout screen *
      --  * Draw double box to look like text border *
      --  * write "LAYOUT" header *
      tnet : net;
      port_number, xb, yb, xo, yo : integer;
   begin
      xo := xmin (1);
      xb := Round (bmax / csx) + xo;
      yo := ymin (1);
      yb := Round (bmax / csy) + yo;
      clear_window_gfx (xmin (9), ymin (9), xmax (9), ymax (9));
      Draw_Box (xo, yo - 7, xb, yb + 2, col_window (1));
      Draw_Box (xo, yo - 5, xb, yb, col_window (1));
      Write_Compt (col_window (1), window_f (1));
      if net_start = null
      then
         new_port (bmax / 2.0, bmax / 2.0, 0);
         new_port (0.0, (bmax - con_sep) / 2.0, 1);
         new_port (bmax, (bmax - con_sep) / 2.0, 2);
         min_ports := 2;
         if con_sep /= 0
         then
            new_port (0.0, (bmax + con_sep) / 2.0, 3);
            new_port (bmax, (bmax + con_sep) / 2.0, 4);
            min_ports := 4;
         end if;
      end if;
      --  if net_start
      for port_number in 1 .. min_ports
      loop
         draw_port (portnet (port_number), Brown);
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
               if tnet.all.grounded
               then
                  draw_groundO (tnet.all.xr, tnet.all.yr);
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
   --  draw_circuit
   --  *
   --  Check to see if part will fit on circuit board.
   --  *

   function off_boardO (step_size : Long_Float) return boolean is
      --  [P2Ada]: WITH instruction
      --  [P2Ada]: !Help! No type found -> add 'P2Ada_Var_18.' to fields
      Result_off_boardO : boolean;
      xrep, yrep, xrem, yrem : Long_Float;
      off_boardt : boolean;
   begin
      dirn_xy;
      declare P2Ada_Var_18 : < > renames compt1.all;
      begin
         if step_size = 1
         then
            xrep := xm + lngth * xii + yii * (width / 2.0 + con_space);
            yrep := ym + lngth * yii + xii * (width / 2.0 + con_space);
            xrem := xm + lngth * xii - yii * width / 2.0;
            yrem := ym + lngth * yii - xii * width / 2.0;
            if betweenr (0, xrep, bmax, 0) and then betweenr (0, yrep, bmax, 0) and then betweenr (0, xrem, bmax, 0) and then betweenr (0, yrem, bmax, 0)
            then
               off_boardt := false;
            else
               off_boardt := true;
            end if;
            --  if step_size<>1
         else
            xrep := xm + (lngth * xii + yii * con_space) / 2.0;
            yrep := ym + (lngth * yii + xii * con_space) / 2.0;
            if betweenr (0, xrep, bmax, 0) and then betweenr (0, yrep, bmax, 0)
            then
               off_boardt := false;
            else
               off_boardt := true;
            end if;
         end if;
      end;
      --  [P2Ada]: end of WITH
      --  else,with
      if off_boardt
      then
         if not (read_kbd)
         then
            key := F3;
            read_kbd := true;
            compt3 := compt1;
            cx3 := compt3.all.x_block;
            draw_circuit;
         end if;
         erase_message;
         message (1) := "The part lies";
         message (2) := "outside the board";
         update_key := false;
      end if;
      --  if off_
      Result_off_boardO := off_boardt;
      return Result_off_boardO;
   end off_boardO;
   --  off_boardO
   --  *********************************************************************
   --  *
   --  Get key from keyboard. See Turbo manual Appendix K.
   --  Includes Draw_Cursor, Erase_Cursor, and Ggotoxy
   --  *

   procedure Get_Key is
      --  [BP2P]: Label "100001" Was "end_blink"
      --  ***************************************************
      cursor_displayed : boolean;
      key_ord : integer;
      --  *
      --  Draw circuit cursor X by first storing
      --  the dots that will be covered.
      --  *

      procedure Draw_Cursor is
         i, j, x_ii, y_ii, xo, yo : integer;
      begin
         cross_dot (1) := xi + xmin (1);
         cross_dot (2) := yi + ymin (1);
         j := 4;
         xo := cross_dot (1);
         yo := cross_dot (2);
         GetBox (0, xo - 5, yo - 4, 11, 9);
         PutPixel (xo, yo, white);
         x_ii := 1;
         y_ii := 1;
         for i in 1 .. 8
         loop
            PutPixel (xo + x_ii, yo + y_ii, white);
            PutPixel (xo + x_ii, yo - y_ii, white);
            PutPixel (xo - x_ii, yo + y_ii, white);
            PutPixel (xo - x_ii, yo - y_ii, white);
            if ((i) mod 2 /= 0)
            then
               x_ii := x_ii + 1;
            else
               y_ii := y_ii + 1;
            end if;
            j := j + 4;
         end loop;
      end Draw_Cursor;
      --  Draw_Cursor
      --  *****************************************************
      --  *
      --  Erase cursor and restore covered pixels.
      --  *

      procedure Erase_Cursor is
         xo, yo : integer;
      begin
         xo := cross_dot (1);
         yo := cross_dot (2);
         PutBox (0, xo - 5, yo - 4, 11, 9);
      end Erase_Cursor;
      --  Erase_Cursor
      --  ******************************************************
      --  *
      --  Activate flashing cursor.
      --  *

      procedure ggotoxy (cursor_displayed : in out boolean) is
         x, y, i, imax : integer;
      begin
         if ccompt /= null
         then
            x := ccompt.all.xp + cx;
            if (x > Max_Text_X)
            then
               x := Max_Text_X;
            end if;
            y := ccompt.all.yp;
            if cursor_displayed
            then
               --  prevent scrolling
               WindMax := WindMax + 1;
               TextCol (lightgray);
               if (cx >= (ccompt.all.descript'length))
               then
                  Put (' ');
               else
                  Put (ccompt.all.descript (cx + 1));
               end if;
               --  allow scrolling
               GotoXY (x, y);
               WindMax := WindMax - 1;
            else
               GotoXY (x, y);
               if insert_key
               then
                  imax := 6;
               else
                  imax := 2;
               end if;
               SetCol (white);
               for i in imin .. imax
               loop
                  Line (charx * (x - 1), chary * y - i - 2, charx * x - 2, chary * y - i - 2);
               end loop;
            end if;
            --  if cursor_displayed
            cursor_displayed := not (cursor_displayed);
         end if;
         --  if ccompt <> nil
      end ggotoxy;
      --  ggotoxy
      --  ********************************************************
      --  * Get_Key *
   begin
      if read_kbd
      then
         if demo_mode
         then
            Get (key_ord);
            Skip_Line;
            key := Character (key_ord);
         else
            cursor_displayed := false;
            if window_number = 1
            then
               Draw_Cursor;
            else
               ggotoxy (cursor_displayed);
            end if;
            if not (keypressed)
            then
               --  blink cursor
               if window_number /= 1
               then
                  loop
                     P2Ada_no_keyword_Delay (200);
                     if keypressed
                     then
                        --  [BP2P]: Label "100001" Was "end_blink"
                        goto LABEL_100001;
                     end if;
                     ggotoxy (cursor_displayed);
                     exit when false;
                  end loop;
               end if;
            end if;
            --  [BP2P]: Label "100001" Was "end_blink"
            <<LABEL_100001>>
            key := ReadKey;
            if key = Alt_o
            then
               key := Omega;
            end if;
            --  Ohms symbol
            if key = Alt_d
            then
               key := Degree;
            end if;
            if (window_number = 2)
            then
               if key = Alt_s
               then
                  key := Mu;
               end if;
            else
               if key = Alt_m
               then
                  key := Mu;
               end if;
            end if;
            --  ! conflict with sh_down
            if key = Alt_p
            then
               key := Parallel;
            end if;
            if window_number = 1
            then
               Erase_Cursor;
            else
               if cursor_displayed
               then
                  ggotoxy (cursor_displayed);
               end if;
            end if;
         end if;
         --  if demo
         --  if read_kbd
         --  redraw_circuit
      else
         if key_i > 0
         then
            if key_list (key_i).noden /= node_number
            then
               key := F3;
               read_kbd := true;
               compt3 := compt1;
               cx3 := compt3.all.x_block;
               draw_circuit;
               message (1) := "Circuit changed";
               message (2) := "Edit part or";
               message (3) := "erase circuit";
               return;
            end if;
         end if;
         key_i := key_i + 1;
         if key_i > key_end
         then
            --  end of redraw
            read_kbd := true;
            key := key_o;
            circuit_changed := false;
            board_changed := false;
            draw_circuit;
         else
            key := key_list (key_i).keyl;
         end if;
         --  Apply appropriate draw function from keylist
      end if;
      --  read_kbd
   end Get_Key;
   --  * Get_Key *
   --  *****************************************************************
   --  *
   --  Ground a node.
   --  *

   procedure ground_node is
   begin
      if cnet /= null
      then
         --  [P2Ada]: WITH instruction
         --  [P2Ada]: !Help! No type found -> add 'P2Ada_Var_19.' to fields
         declare P2Ada_Var_19 : < > renames cnet.all;
         begin
            if not (grounded)
            then
               grounded := true;
               draw_groundO (xr, yr);
            end if;
         end;
         --  [P2Ada]: end of WITH
      end if;
   end ground_node;
   --  ground_node
   --  *
   --  Remove a ground.
   --  *

   procedure unground is
   begin
      if cnet /= null
      then
         --  [P2Ada]: WITH instruction
         --  [P2Ada]: !Help! No type found -> add 'P2Ada_Var_20.' to fields
         declare P2Ada_Var_20 : < > renames cnet.all;
         begin
            if grounded
            then
               grounded := false;
               if read_kbd
               then
                  draw_circuit;
               end if;
            end if;
         end;
         --  [P2Ada]: end of WITH
      end if;
   end unground;
   --  unground
   --  *
   --  Join cnet to an external port.
   --  *

   procedure join_port (port_number, ivt : integer) is
      found : boolean;
      tcon : conn;
      dirt : integer;
   begin
      if port_number <= min_ports
      then
         if ivt = 1
         then
            --  connect
            if ym > portnet (port_number).all.yr
            then
               dirt := 1;
            else
               dirt := 8;
            end if;
            if abs (ym - portnet (port_number).all.yr) < widthz0 / 2.0
            then
               case port_number is
                  when 1 | 3 =>
                     dirt := 4;
                  when 2 | 4 =>
                     dirt := 2;
                  when others =>
                     --  [P2Ada]: no otherwise / else in Pascal
                     null;
               end case;
            end if;
            --  case
            if not (portnet (port_number).all.node)
            then
               if cnet = null
               then
                  cnet := new_net (0, true);
               end if;
               portnet (port_number).all.node := true;
               if read_kbd or else demo_mode
               then
                  draw_to_port (cnet, port_number);
               end if;
               tcon := new_con (cnet, dirt);
               cnet.all.ports_connected := cnet.all.ports_connected + 1;
               tcon.all.port_type := port_number;
               tcon.all.mate := null;
               cnet.all.number_of_con := cnet.all.number_of_con + 1;
            else
               message (1) := "Port " + Character (port_number + Character'Pos ('0')) + " is";
               message (2) := "already joined";
            end if;
            --  erase
            --  added for dx_dy
         else
            iv := 0;
            tcon := null;
            found := false;
            if cnet /= null
            then
               loop
                  if tcon = null
                  then
                     tcon := cnet.all.con_start;
                  else
                     tcon := tcon.all.next_con;
                  end if;
                  if tcon.all.port_type = port_number
                  then
                     found := true;
                  end if;
                  exit when found or else (tcon.all.next_con = null);
               end loop;
            end if;
            if found
            then
               cnet.all.ports_connected := cnet.all.ports_connected - 1;
               dispose_con (tcon);
               Node_Look;
               portnet (port_number).all.node := false;
               if read_kbd
               then
                  Draw_Circuit;
               end if;
            else
               Goto_Port (port_number);
            end if;
         end if;
      end if;
      --  else ivt
   end join_port;
   --  join_port
end pfun3;
