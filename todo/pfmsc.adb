--  ParamStr,...
--  UpCase
--  For Shift_Left/Right
--  This is for Pi :
--  This is for Sqrt, Sin, Cos, etc. :
--  This is for Dispose. P2Ada writes automatically:
--  "Dispose is new Ada.Unchecked_Deallocation(<type>, <pointer type>)".
--  Unit found in Free Pascal RTL's
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
with Dos, xgraph, pfun1, pfun2;

package pfmsc is

   procedure Draw_EGA_Smith (imped_chart : boolean);

   procedure Write_Commands;

   procedure Board_HELP_window;
   --  Device_Read uses and includes the routines:
   --  procedure Get_Device_Params(tcompt : compt; var fname : file_string;
   --  var len : double);
   --  procedure pars_tplate(tp:file_string;var n_c,n_s:integer;var f_p:boolean);
   --  procedure read_number(var s : double);

   procedure Device_Read (tcompt : compt; indef : boolean);
end pfmsc;
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

package body pfmsc is
   --  [P2Ada]: This is for 'Write([Boolean])'

   package Boolean_Text_IO is new Enumeration_IO (Boolean);
   use Boolean_Text_IO;
   --  [P2Ada]: This is for 'file' without type
   --  [P2Ada]: This is for the Halt pseudo-procedure

   package Byte_Direct_IO is new Ada.Direct_IO (Unsigned_8);
   Program_halted : exception;
   --  *
   --  Draw the smith chart with radius rho_fac.
   --  If imped_chart = true then draw impedance chart
   --  else draw admittance chart.
   --  *

   procedure Draw_EGA_Smith (imped_chart : boolean) is
      --  ,Angle_2
      --  ,a,b
      --  ,arc_length
      --  *****************************************************
      I : INTEGER;
      RG_reye, reye : integer;
      theta_start_high, theta_start_low, theta_in, beta : Long_Float;
      --  *
      --  Find maximum angle allowed before an arc circle
      --  goes outside the Smith chart.
      --  Must have a<>0, b<>0.
      --  *

      procedure Get_thetas (a, b, cir_rad : Long_Float; delta_x : integer) is
         x : Long_Float;
      begin
         if ((a + b) <= cir_rad)
         then
            --  if entire circle inside radius
            --  Cos(theta_in) = x
            --  this ArcCos function valid for -1 < x < 1
            theta_start_high := 0;
            theta_start_low := 0;
            theta_in := 180;
         else
            x := 0.5 * (a / b + b / a - ((1.0 * cir_rad) ** 2) / (a * b));
            theta_in := 90 - 180 * arctan (x / sqrt (1.0 - ((x) ** 2))) / Pi;
            if imped_chart or else (delta_x > 0)
            then
               --  admittance chart
               theta_start_high := 270 - beta - theta_in;
               theta_start_low := 450 + beta - theta_in;
            else
               theta_start_high := 270 + beta - theta_in;
               theta_start_low := 450 - beta - theta_in;
            end if;
            if imped_chart and then (delta_x < 0)
            then
               theta_start_high := 360 - theta_in;
            end if;
            if not (imped_chart) and then (delta_x > 0)
            then
               theta_start_high := 180 - theta_in;
            end if;
         end if;
      end Get_thetas;
      --  *********************************************************
      --  *
      --  Make dotted arcs inside the smith chart
      --  *

      procedure Make_Dot_Arcs (Arc_Rad, RG_delta_x, clip_rad : integer) is
         --  k : integer;
         --  Angle_2:= Round( 180* Arc_length / (Pi * Arc_Rad) );
         --  if Angle_2 = 0 then Angle_2 := 1;
         a, b : Long_Float;
         i, j : INTEGER;
      begin
         a := Arc_rad;
         if RG_delta_x = 0
         then
            --  b is the distance between circle centers
            --  right triangle for XB circles
            b := sqrt (((1.0 * reye) ** 2) + ((a) ** 2));
            beta := 180 * arctan (1.0 * reye / a) / Pi;
            if b < (a + clip_rad)
            then
               --  for k:= 1 to Trunc(2*theta_in) div Angle_2 do begin
               --  ths:= Round(theta_start_high) + (k-1)*Angle_2;
               --  Arc(centerx+RG_reye,centery-Round(yf*Arc_Rad),
               --  ths,ths+1,Arc_Rad);
               --  {plotting one degree makes a single arc point}
               --  ths:= Round(theta_start_low) + (k-1)*Angle_2;
               --  Arc(centerx+RG_reye,centery+Round(yf*Arc_Rad),
               --  ths,ths+1,Arc_Rad);
               --  end; {for k}
               --  BUG fix: Arc doesn't like coordinates outside of screen!!
               Get_thetas (a, b, clip_rad, 0);
               i := rad + RG_reye;
               j := Round (theta_start_high + (2 * theta_in) + 0.5);
               while (Integer (cos (Pi * j / 180) * Arc_rad) >= rad - RG_reye)
               loop
                  j := j - 1;
               end loop;
               Arc (i, rad - Round (yf * Arc_rad), Integer (theta_start_high), j, Arc_Rad);
               j := Integer (theta_start_low);
               while (Integer (cos (Pi * j / 180) * Arc_rad) >= rad - RG_reye)
               loop
                  j := j + 1;
               end loop;
               Arc (i, rad + Round (yf * Arc_Rad), j, Round (theta_start_low + (2 * theta_in) + 0.5), Arc_Rad);
            end if;
            --  if b<
            --  mag of delta_x for RG circles
         else
            b := 1.0 * abs (RG_delta_x);
            beta := 90;
            if (a < b + clip_rad) and then (b < a + clip_rad)
            then
               --  for k:=1 to Trunc(2*theta_in) div Angle_2 do begin
               --  ths:= Round(theta_start_high) + (k-1)*Angle_2;
               --  Arc(centerx+RG_delta_x,centery,ths,ths+1,Arc_rad);
               --  {plotting one degree makes a single arc point}
               --  {plot continuously for the circle}
               --  end; {for k}
               Get_thetas (a, b, clip_rad, RG_delta_x);
               Arc (rad + RG_delta_x, rad, Integer (theta_start_high), Round (theta_start_high + (2 * theta_in) + 0.5), Arc_rad);
            end if;
            --  if a< and b<
         end if;
      end Make_Dot_Arcs;
      --  *********************************************************
      --  *Draw_EGA_Smith*
   begin
      SetViewPort (centerx - rad, centery - rad, centerx + rad, centery + rad, TRUE);
      SetFillStyle (SolidFill, black);
      SetCol (Green);
      reye := Round (rad / rho_fac);
      if imped_chart
      then
         RG_reye := reye;
      else
         RG_reye := - reye;
      end if;
      if rho_fac > 0.29
      then
         --  ignore lines for tiny smith chart
         --  If very large raw just outer circle
         if (rho_fac <= 15)
         then
            --  if Large_Smith then
            --  {* Set arc length to 10 degrees (0.2618 radians)x .25 reye *}
            --  Arc_length:= 0.036*reye*rho_fac
            --  else
            --  {* Set arc length to 15 degrees (0.2618 radians)x .25 reye *}
            --  Arc_length:= 0.055*reye*rho_fac;
            --  r circles, except smallest one
            for I in 2 .. 5
            loop
               Make_Dot_Arcs ((I * reye) / 6, ((6 - I) * RG_reye) / 6, rad);
            end loop;
            --  If large chart then draw r circles outside eye
            if rho_fac > 1.5
            then
               Make_Dot_Arcs (reye, 2 * RG_reye, rad);
               Make_Dot_Arcs (3 * reye, - 2 * RG_reye, rad);
               Make_Dot_Arcs (3 * reye, 4 * RG_reye, rad);
            end if;
            --  Make_Dot_Arcs(reye div 2,0,rad);  {center 0 calls xb circles}
            --  {	     Make_Dot_Arcs(reye,0,rad); }
            --  Make_Dot_Arcs(2*reye,0,rad);
            --  end
            --  else begin
            --  {             IF (rho_fac > 1.0) THEN BEGIN
            --  FOR I:= 1 TO 3 DO BEGIN
            --  Make_Dot_Arcs ((I * reye) DIV 3, 0, reye);
            --  END;
            --  FOR I:= 1 TO 2 DO BEGIN
            --  Make_Dot_Arcs ((3 * reye) DIV I, 0, reye);
            --  END;
            --  END ELSE BEGIN
            --  draw xb circles
            --  Draw smallest r circle and clear area inside (if visible)
            --  This makes the xb circles stop at the inner r circle
            Make_Dot_arcs (reye * 5, 0, rad);
            Make_Dot_arcs (reye * 2, 0, rad);
            Make_Dot_arcs (reye / 2, 0, rad);
            if (rho_fac > 0.66)
            then
               FillEllipse ((5 * RG_reye) / 6 + rad, rad, reye / 6, reye / 6);
            end if;
            --  if necessary also left side
            if (rho_fac > 1.0)
            then
               FillEllipse ((7 * RG_reye) / 6 + rad, rad, reye / 6, reye / 6);
            end if;
            --  Now draw xb circles that cut smallest r circle
            Make_Dot_Arcs (reye / 5, 0, rad);
            Make_Dot_Arcs (reye, 0, rad);
         end if;
         --  if not(rho_fac>15)
         --  rho > 0.3
      else
         if rho_fac > 0.18
         then
            --  if .18<rho_fac<.3 then tick and cir
            --  Arc_length:= 0.05*reye*2*rho_fac;
            --  rho > 0.18
            --  if rho_fac < .18 then just draw a tick mark
            Make_Dot_Arcs (reye / 2, RG_reye / 2, rad);
            Line (rad, rad - 3, rad, rad + 3);
         else
            Line (rad, rad - 3, rad, rad + 3);
         end if;
      end if;
      --  real axis
      --  outer circles
      Line (0, rad, 2 * rad, rad);
      SetCol (lightgreen);
      if rho_fac > 1.0
      then
         Circle (rad, rad, reye);
      end if;
      if (blackwhite)
      then
         SetColor (lightgreen);
      end if;
      --  draw outer circle
      --  overpaint overetched arc parts
      Circle (rad, rad, rad);
      reye := 2 * rad;
      FloodFill (0, 0, lightgreen);
      FloodFill (0, reye, lightgreen);
      FloodFill (reye, 0, lightgreen);
      FloodFill (reye, reye, lightgreen);
      if (blackwhite)
      then
         SetColor (white);
         Circle (rad, rad, rad);
      end if;
      --  { mark at (1 0) }
      --  SetCol(green);
      --  IF (blackwhite) THEN SetFillStyle(solidFill, white) ELSE SetFillStyle(solidFill, green);
      --  FillEllipse(rad, rad, 2, 2);
      --  {  Circle(rad, rad, 2); }
      SetViewPort (xmin (12), ymin (12), xmax (12), ymax (12), False);
   end Draw_EGA_Smith;
   --  *Draw_EGA_Smith*
   --  *
   --  Write commands in help box.
   --  *

   procedure Write_Commands is
      --  * Commands for help window *
      command : array (1 .. 3, 1 .. 9, 1 .. 4) of string (1 .. 17) := (((Character'Val (27) + ' ' + Character'Val (26) + ' ' + Character'Val (24) + ' ' + Character'Val (25), " draw part", "", ""), ('=', "  ground " & Character'Val (132) & "      ", "", ""), ("1..4", " connect path", "", ""), ("a..r", "  select part", "", ""), ("Ctrl-e", " erase crct", "", ""), ("Ctrl-n", " go to node", "", ""), ("Shift", " move/erase", "", ""), ("F10", "  toggle help", "", ""), ("Esc", "  exit        ", "", "")), ((Character'Val (27) + ' ' + Character'Val (26) + ' ' + Character'Val (24) + ' ' + Character'Val (25), "   cursor ", "", ""), ("p,Ctrl-p", "  plot   ", "", ""), ("PgUp,PgDn", " marker ", "", ""), ("Ctrl-s", "  save file", "", ""), ("Ctrl-a", "  artwork  ", "", ""), ("i,s", " impulse, step", "", ""), ("Tab", " toggle Smith", "", ""), ("Alt-s", " large Smith", "", ""), ("F10, Esc", " help, exit   ", "", "")), ((Character'Val (27) + ' ' + Character'Val (26) + ' ' + Character'Val (24) + ' ' + Character'Val (25), "   cursor ", "", ""), ("Del,Backspace,Ins", "", "", ""), ("Alt-o ", Omega, "   Alt-m ", Mu), ("Alt-d ", Degree, "   Alt-p ", Parallel), ("Ctrl-e", " erase crct", "", ""), ("Ctrl-r", " read file ", "", ""), ("Tab", "  extra parts", "", ""), ("F10", "  toggle help", "", ""), ("Esc", "  exit        ", "", "")));
      i, imax : integer;
   begin
      imax := ymax (4) - ymin (4) + 1;
      if (imax > 9)
      then
         imax := 9;
      end if;
      if not ((window_number = 3) and then read_kbd and then circuit_changed)
      then
         erase_message;
      end if;
      Make_Text_Border (xmin (4) - 1, ymin (4) - 1, xmax (4) + 1, ymax (4) + 1, col_window (window_number), true);
      for i in 1 .. imax
      loop
         --  write help window elements
         --  position of command window
         GotoXY (xmin (4), ymin (4) - 1 + i);
         TextCol (white);
         Put (command (window_number, i, 1));
         TextCol (lightgray);
         Put (command (window_number, i, 2));
         TextCol (white);
         Put (command (window_number, i, 3));
         TextCol (lightgray);
         Put (command (window_number, i, 4));
      end loop;
      --  Write header for help window
      write_compt (col_window (window_number), command_f (window_number));
   end Write_Commands;
   --  * Write_Commands *
   --  *
   --  Erase parts list area, write zd and fd,
   --  draw window box, list parts.
   --  Called only by Read_Net.
   --  *

   procedure Board_HELP_window is
      --  clear and write border
      --  write BOARD Header
      --  Default
   begin
      Make_Text_Border (xmin (3) - 1, ymin (3) - 1, xmax (3) + 1, ymax (3) + 1, col_window (4), true);
      write_compt (col_window (4), command_f (4));
      Window (xmin (3), ymin (3), xmax (3), ymax (3));
      TextCol (lightgray);
      Put ("zd : norm. impedance");
      New_Line;
      Put ("fd : design freq.");
      New_Line;
      Put ("er : diel. constant");
      New_Line;
      Put ("h  : sub. thickness");
      New_Line;
      Put ("s  : board size");
      New_Line;
      Put ("c  : conn. separation");
      New_Line;
      Put ("Tab : toggle type");
      New_Line;
      Window (1, 1, Max_Text_X, Max_Text_Y);
   end Board_HELP_window;
   --  * Board_HELP_window *
   --  *
   --  Read device description from parts window (tcompt^.descript)
   --  and extract filename and length information.
   --  *

   procedure Get_Device_Params (tcompt : compt; fname : in out file_string; len : in out Long_Float) is
      --  default length was Manh_length
      --  * fname = e.g. 'e device fsc10 2mm' *
      potential_numbers : array (Character) of Boolean := ('+' | '-' | '.' | ',' | '0' .. '9' => True, others => False);
      p1, p2, code, i, j, long : integer;
      c_string, s_value : line_string;
      found_value : boolean;
   begin
      len := 0.0;
      fname := tcompt.all.descript;
      for j in 1 .. 2
      loop
         --  find index for blanks in descript
         --  delete through blanks in descript
         p1 := Pos (' ', fname);
         Delete (fname, 1, p1);
      end loop;
      --  get string length
      --  now fname = 'fsc10 2mm  '
      p2 := (fname'length);
      while fname (p2) = ' '
      loop
         --  remove any blanks at end
         Delete (fname, p2, 1);
         p2 := p2 - 1;
      end loop;
      --  here fname = 'fsc10 2mm' or 'fsc10'
      --  now c_string = 'fsc10 2mm'
      p1 := Pos (' ', fname);
      c_string := fname;
      if p2 = 0
      then
         ccompt := tcompt;
         bad_compt := true;
         message (1) := "Invalid device";
         message (2) := "specification";
         return;
      else
         if (p1 > 0)
         then
            Delete (fname, p1, p2);
         end if;
      end if;
      --  now fname = 'fsc10'
      if not (Manhattan (tcompt) or else (p1 = 0))
      then
         --  now c_string = '2mm'
         Delete (c_string, 1, p1);
         long := (c_string'length);
         while c_string (long) = ' '
         loop
            long := long - 1;
         end loop;
         --  remove blanks at end
         found_value := false;
         s_value := "";
         j := 1;
         while (c_string (j) = ' ') and then (j < long + 1)
         loop
            j := j + 1;
         end loop;
         --  Skip spaces
         loop
            --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
            if c_string (j) and potential_numbers
            then
               if not (c_string (j) = '+')
               then
                  --  ignore +
                  if c_string (j) = ','
                  then
                     --  . for ,
                     s_value := s_value + '.';
                  else
                     s_value := s_value + c_string (j);
                  end if;
               end if;
               --  + check
               j := j + 1;
            else
               found_value := true;
            end if;
            exit when (found_value or else (j = long + 1));
         end loop;
         --  convert string to double number
         Val (s_value, len, code);
         if (code /= 0) or else (Pos ('m', c_string) = 0) or else (long = 0)
         then
            ccompt := tcompt;
            bad_compt := true;
            message (1) := "Invalid length";
            message (2) := "or filename";
            return;
         end if;
         --  Here j is right of the number
         while (c_string (j) = ' ') and then (j < long + 1)
         loop
            j := j + 1;
         end loop;
         --  Skip spaces
         --  * if j=long then j must point to an 'm' *
         --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
         if (c_string (j) and Eng_Dec_Mux) and then (j < long)
         then
            if c_string (j) = 'm'
            then
               --  is 'm' a unit or prefix?
               i := j + 1;
               while (c_string (i) = ' ') and then (i < long + 1)
               loop
                  i := i + 1;
               end loop;
               --  * Skip spaces to check for some unit *
               if (c_string (i) = 'm')
               then
                  --  it's the prefix milli 'm' next to an 'm'
                  --  make j point past the prefix, to the unit
                  len := Eng_Prefix ('m') * len;
                  j := i;
               end if;
               --  if 'm' is a unit do nothing
               --  if other than 'm' factor in prefix
               --  advance from prefix toward unit
            else
               len := Eng_Prefix (c_string (j)) * len;
               j := j + 1;
            end if;
         end if;
         --  if in Eng_Dec_Mux
         --  return length in millimeters, not meters
         len := 1000 * len;
         while (c_string (j) = ' ') and then (j < long + 1)
         loop
            j := j + 1;
         end loop;
         if (len < 0) or else (c_string (j) /= 'm')
         then
            ccompt := tcompt;
            bad_compt := true;
            message (1) := "Negative length";
            message (2) := "or invalid unit";
            return;
         end if;
      end if;
      --  if not Manhattan
   end Get_Device_Params;
   --  * Get_Device_Params *
   --  ************************************************************************
   --  *
   --  Device equivalent of tline.
   --  Included within are Pars_tplate and Read_Number.
   --  Only to be used when action= true
   --  get device parameters (filename,draw length)
   --  check that file exists.
   --  Check for .puf or .dev files.
   --  Indef specifies whether or not to enable the generation
   --  of indefinite scattering parameters (extra port).
   --  type compt has the following s_param records:
   --  tcompt^.s_begin,
   --  tcompt^.s_file,
   --  tcompt^.s_ifile,
   --  tcompt^.f_file
   --  *

   procedure Device_Read (tcompt : compt; indef : boolean) is
      --  [BP2P]: Label "100001" Was "read_finish"
      --  s1,s2        			: array[1..10,1..10] of Tcomplex;
      --  **************************************************************
      fname : file_string;
      c_ss, c_f : s_param;
      template : string (1 .. 128);
      ext_string : string (1 .. 3);
      first_char : string (1 .. 1);
      char1, char2 : Character;
      freq_present, Eesof_format : boolean;
      i, j, number_of_s, code1, number_of_ports, Eesof_ports : integer;
      f1, mag, ph : Long_Float;
      --  *
      --  Partition template. Extract number of connectors and frequencies.
      --  template (tp:file_string) is in the form ' f  s11  s21  s12  s22 '
      --  *

      procedure Pars_tplate (tp : file_string; n_c, n_s : in out integer; f_p : in out boolean) is
         i, j, i1, i2, x, code : integer;
         ijc : array (1 .. 16) of string (1 .. 2);
      begin
         if (Pos ('f', tp) > 0) or else (Pos ('F', tp) > 0)
         then
            f_p := true;
         else
            f_p := false;
         end if;
         n_s := 0;
         loop
            i1 := Pos ('s', tp);
            i := i1;
            i2 := Pos ('S', tp);
            if i1 < i2
            then
               if i1 > 0
               then
                  i := i1;
               else
                  i := i2;
               end if;
            else
               if i2 > 0
               then
                  i := i2;
               else
                  i := i1;
               end if;
            end if;
            if i > 0
            then
               Delete (tp, 1, i);
               n_s := n_s + 1;
               if (tp'length) >= 2
               then
                  ijc (n_s) := tp;
                  Delete (tp, 1, 2);
               end if;
            end if;
            exit when ((tp'length) = 0) or else (i = 0);
         end loop;
         n_c := 1;
         for i in 1 .. n_s
         loop
            Val (ijc (i), x, code);
            if code /= 0
            then
               bad_compt := true;
               message (1) := "Bad port number";
               message (2) := "in device";
               message (3) := "file template";
               return;
            end if;
            iji (i, 1) := x / 10;
            iji (i, 2) := x - iji (i, 1) * 10;
            if (iji (i, 1) < 1) or else (iji (i, 2) < 1)
            then
               bad_compt := true;
               message (1) := "0 port number";
               message (2) := "in device";
               message (3) := "file template";
               return;
            end if;
            if iji (i, 1) > n_c
            then
               n_c := iji (i, 1);
            end if;
            if iji (i, 2) > n_c
            then
               n_c := iji (i, 2);
            end if;
            for j in 1 .. i - 1
            loop
               if (iji (i, 1) = iji (j, 1)) and then (iji (i, 2) = iji (j, 2))
               then
                  bad_compt := true;
                  message (1) := "Repeated sij";
                  message (2) := "in device";
                  message (3) := "file template";
                  return;
               end if;
            end loop;
         end loop;
         --  for i := 1 to n_s
         if n_s = 0
         then
            bad_compt := true;
            message (1) := "No port numbers";
            message (2) := "in device";
            message (3) := "file template";
         end if;
      end Pars_tplate;
      --  Pars_tplate
      --  ********************************************************
      --  *
      --  Read s-parameter values from files.
      --  *

      procedure Read_Number (s : in out Long_Float) is
         --  first_char is the very first valid file character
         ss : string (1 .. 128);
         char1 : Character;
         code : integer;
         found : boolean;
      begin
         ss := first_char;
         found := false;
         if (ss = "")
         then
            --  search for first valid character if not in first_char
            if not (End_of_File (dev_file))
            then
               --  goto next number
               loop
                  --  keep reading characters until a valid one is found
                  if SeekEoln (dev_file)
                  then
                     --  [P2Ada]: !Help! Maybe (file)
                     Get (dev_file);
                     Skip_Line;
                  end if;
                  --  if blank line then advance to next line
                  --  [P2Ada]: !Help! Maybe (file,...) here
                  --  read single character
                  --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
                  Get (dev_file);
                  Get (char1);
                  if char1 and (lbrack | '#' | '!' => True, others => False)
                  then
                     --  [P2Ada]: !Help! Maybe (file)
                     Get (dev_file);
                     Skip_Line;
                  end if;
                  --  skip potential comment lines
                  --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
                  if char1 and ('+' | '-' | '.' | ',' | '0' .. '9' | 'e' | 'E' | '\' => True, others => False)
                  then
                     ss := char1;
                     found := true;
                  end if;
                  exit when found or else End_of_File (dev_file);
               end loop;
            end if;
         end if;
         found := false;
         if not End_of_File (dev_file)
         then
            loop
               --  continue reading characters and add to string ss until invalid
               --  [P2Ada]: !Help! Maybe (file,...) here
               --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
               Get (dev_file);
               Get (char1);
               if char1 and ('+' | '-' | '.' | ',' | '0' .. '9' | 'e' | 'E' => True, others => False)
               then
                  ss := ss + char1;
               else
                  found := true;
               end if;
               exit when found or else End_of_Line (dev_file) or else End_of_File (dev_file);
            end loop;
         end if;
         --  turn string ss into double number
         Val (ss, s, code);
         if (code /= 0) or else ((ss'length) = 0)
         then
            bad_compt := true;
            message (1) := "Extra or missing";
            message (2) := "number in";
            message (3) := "device file";
         end if;
         --  if code <> 0
      end Read_Number;
      --  read_number
      --  ********************************************************
      --  *
      --  Look for the start of useable data in a file
      --  including a template or numbers.
      --  Do so via a character search for:
      --  'f', 'F', 's' or 'S' for templates,
      --  or any numeric character for Eesof files.
      --  *

      procedure Seek_File_Start (temp_exists : boolean) is
         char1 : Character;
         found : boolean;
      begin
         found := false;
         first_char := "";
         loop
            --  keep reading characters until a valid one is found
            while SeekEoln (dev_file)
            loop
               --  [P2Ada]: !Help! Maybe (file)
               Get (dev_file);
               Skip_Line;
            end loop;
            --  Advance past any blank lines
            loop
               --  [P2Ada]: !Help! Maybe (file,...) here
               --  find single character
               Get (dev_file);
               Get (char1);
               exit when (char1 /= ' ') or else End_of_File (dev_file);
            end loop;
            --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
            if char1 and (lbrack | '#' | '!' => True, others => False)
            then
               --  [P2Ada]: !Help! Maybe (file)
               Get (dev_file);
               Skip_Line;
            end if;
            --  Skip lines with comments
            if temp_exists
            then
               --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
               if char1 and ('f' | 'F' | 's' | 'S' => True, others => False)
               then
                  first_char := char1;
                  found := true;
               end if;
               --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
            else
               if char1 and ('+' | '-' | '.' | ',' | '0' .. '9' => True, others => False)
               then
                  first_char := char1;
                  found := true;
               end if;
            end if;
            exit when found or else End_of_File (dev_file);
         end loop;
      end Seek_File_Start;
      --  ********************************************************
      --  * Device_Read *
      --  get filename and length
   begin
      Get_Device_Params (tcompt, fname, tcompt.all.lngth);
      if bad_compt
      then
         return;
      end if;
      --  ! length check moved from this location
      Eesof_format := false;
      i := Pos ('.', fname);
      if (i = 0)
      then
         --  add .dev extension
         --  Check for Eesof type extension
         --  copy 3 character extension
         fname := fname + ".dev";
      else
         ext_string := Copy (fname, i + 1, 3);
         if ((ext_string'length) = 3)
         then
            --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
            --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
            --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
            if (ext_string (1) and ('s' | 'S' => True, others => False)) and then (ext_string (2) and ('1' .. '4' => True, others => False)) and then (ext_string (3) and ('p' | 'P' => True, others => False))
            then
               Val (ext_string (2), Eesof_ports, code1);
               if code1 = 0
               then
                  Eesof_format := true;
               end if;
            end if;
            --  eesof check
         end if;
         --  length check
      end if;
      --  ext check
      if (tcompt.all.f_file = null) or else tcompt.all.changed
      then
         if fileexists (true, dev_file, fname)
         then
            --  [P2Ada]: WITH instruction
            --  [P2Ada]: !Help! No type found -> add 'P2Ada_Var_1.' to fields
            declare P2Ada_Var_1 : < > renames tcompt.all;
            begin
               if Eesof_format
               then
                  --  Skip lines looking for start of data
                  --  ! WARNING Seek_File_Start stores the first
                  --  data character in first_char!
                  --  * set up iji[] array *
                  Seek_File_Start (false);
                  number_of_ports := Eesof_ports;
                  number_of_s := ((number_of_ports) ** 2);
                  freq_present := true;
                  for i in 1 .. number_of_ports
                  loop
                     for j in 1 .. number_of_ports
                     loop
                        iji (number_of_ports * (i - 1) + j, 1) := i;
                        iji (number_of_ports * (i - 1) + j, 2) := j;
                     end loop;
                  end loop;
                  --  * Must correct for 2-ports since their order is goofy *
                  if (number_of_ports = 2)
                  then
                     iji (2, 1) := 2;
                     iji (2, 2) := 1;
                     iji (3, 1) := 1;
                     iji (3, 2) := 2;
                  end if;
               else
                  while SeekEoln (dev_file)
                  loop
                     --  [P2Ada]: !Help! Maybe (file)
                     Get (dev_file);
                     Skip_Line;
                  end loop;
                  --  Advance past any blank lines
                  --  [P2Ada]: !Help! Maybe (file,...) here
                  --  [P2Ada]: !Help! Maybe (file)
                  --  read first line string
                  Get (dev_file);
                  Get (template);
                  Skip_Line;
                  if Pos ("\b", template) > 0
                  then
                     --  * if a .PUF file *
                     loop
                        --  * then move to \s section *
                        --  [P2Ada]: !Help! Maybe (file,...) here
                        --  [P2Ada]: !Help! Maybe (file,...) here
                        --  [P2Ada]: !Help! Maybe (file)
                        --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
                        Get (dev_file);
                        Get (char1);
                        Get (char2);
                        Skip_Line;
                        exit when ((char1 = '\') and then (char2 and ('s' | 'S' => True, others => False))) or else End_of_File (dev_file);
                     end loop;
                     if End_of_File (dev_file)
                     then
                        --  [BP2P]: Label "100001" Was "read_finish"
                        bad_compt := true;
                        message (1) := "s-parameters";
                        message (2) := "not found in";
                        message (3) := "device file";
                        goto LABEL_100001;
                     end if;
                  end if;
                  --  if Pos('\b')
                  --  now check for valid template = e.g. ' f   s11  s21  s12  s22 '
                  while (template (1) = ' ')
                  loop
                     Delete (template, 1, 1);
                  end loop;
                  --  delete leading blanks
                  --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
                  if (template (1) and ('f' | 'F' | 's' | 'S' => True, others => False))
                  then
                     --  have a potentially valid template, get info
                     --  Skip lines looking for template
                     Pars_tplate (template, number_of_ports, number_of_s, freq_present);
                  else
                     Seek_File_Start (true);
                     if End_of_File (dev_file)
                     then
                        --  [BP2P]: Label "100001" Was "read_finish"
                        bad_compt := true;
                        message (1) := "template";
                        message (2) := "not found in";
                        message (3) := "device file";
                        goto LABEL_100001;
                     end if;
                     --  [P2Ada]: !Help! Maybe (file,...) here
                     --  [P2Ada]: !Help! Maybe (file)
                     --  Put back character removed by Seek_File_Start
                     --  This to initialize Read_Number
                     --  get info from template
                     Get (dev_file);
                     Get (template);
                     Skip_Line;
                     Insert (first_char, template, 1);
                     first_char := "";
                     Pars_tplate (template, number_of_ports, number_of_s, freq_present);
                  end if;
                  --  end template search
                  --  * Number_of_ports is how many are in the file *
                  --  * Number_of_con is how many will result *
               end if;
               --  else Eesof_format
               if indef
               then
                  number_of_con := number_of_ports + 1;
               else
                  number_of_con := number_of_ports;
               end if;
               --  * Initialize sdevice elements to zero *
               for j in 1 .. number_of_con
               loop
                  for i in 1 .. number_of_con
                  loop
                     sdevice (i, j).r := 0;
                     sdevice (i, j).i := 0;
                  end loop;
               end loop;
               --  width:=0;
               --  ! This section moved from 4th line--get_device returns 0 for Manh
               if Manhattan (tcompt) or else (tcompt.all.lngth = 0)
               then
                  if (number_of_con > 1)
                  then
                     tcompt.all.lngth := Manh_length * (number_of_con - 1);
                  else
                     tcompt.all.lngth := Manh_length;
                  end if;
               end if;
               --  symmetrical
               tcompt.all.width := tcompt.all.lngth;
               if tcompt.all.lngth <= resln
               then
                  --  [BP2P]: Label "100001" Was "read_finish"
                  bad_compt := true;
                  message (1) := "Device length";
                  message (2) := "must be";
                  message (3) := '>' + sresln;
                  goto LABEL_100001;
               end if;
               --  use f1 to detect start of noise parameters
               --  * LOOP to read in s-parameters from file *
               con_space := 0.0;
               c_ss := null;
               c_f := null;
               f1 := - 1.0;
               loop
                  if freq_present
                  then
                     if c_f = null
                     then
                        New_s (tcompt.all.f_file);
                        c_f := tcompt.all.f_file;
                     else
                        New_s (c_f.all.next_s);
                        c_f := c_f.all.next_s;
                     end if;
                     --  c_f=nil
                     --  Must set up next call to Read_Number in case
                     --  on the first pass first_char was set. This
                     --  only occurs with Eesof files, effecting only
                     --  the first frequency data point
                     --  * Compare with last freq. for start of noise parameters *
                     --  if last frequency was larger
                     c_f.all.next_s := null;
                     New_c (c_f.all.z);
                     Read_Number (c_f.all.z.all.c.r);
                     first_char := "";
                     if (f1 > c_f.all.z.all.c.r) or else bad_compt
                     then
                        --  have reached EOF
                        --  [BP2P]: Label "100001" Was "read_finish"
                        erase_message;
                        bad_compt := false;
                        goto LABEL_100001;
                     end if;
                     --  if bad_compt
                     --  Save last freq point
                     --  if freq_present=true
                     f1 := c_f.all.z.all.c.r;
                  else
                     tcompt.all.f_file := null;
                  end if;
                  --  end else
                  for i in 1 .. number_of_s
                  loop
                     Read_Number (mag);
                     if bad_compt
                     then
                        if (i = 1) and then not (freq_present)
                        then
                           --  reached EOF
                           erase_message;
                           bad_compt := false;
                        end if;
                        --  if i=1
                        --  [BP2P]: Label "100001" Was "read_finish"
                        goto LABEL_100001;
                     end if;
                     --  if bad_compt=true
                     Read_Number (ph);
                     if bad_compt
                     then
                        --  [BP2P]: Label "100001" Was "read_finish"
                        goto LABEL_100001;
                     end if;
                     sdevice (iji (i, 1), iji (i, 2)).r := one * mag * cos (ph * pi / 180);
                     sdevice (iji (i, 1), iji (i, 2)).i := one * mag * sin (ph * pi / 180);
                  end loop;
                  --  for i:=1 to number_of_s
                  --  * Here sdevice[] is filled with s-parameters
                  --  for a single frequency. Indef_Matrix will
                  --  generate additional scattering parameters
                  --  to fill an additional port number.	   *
                  if indef
                  then
                     Indef_Matrix (sdevice, number_of_ports);
                  end if;
                  for j in 1 .. number_of_con
                  loop
                     for i in 1 .. number_of_con
                     loop
                        if c_ss = null
                        then
                           New_s (tcompt.all.s_file);
                           c_ss := tcompt.all.s_file;
                        else
                           New_s (c_ss.all.next_s);
                           c_ss := c_ss.all.next_s;
                        end if;
                        --  fill parameters
                        c_ss.all.next_s := null;
                        New_c (c_ss.all.z);
                        c_ss.all.z.all.c.r := sdevice (i, j).r;
                        c_ss.all.z.all.c.i := sdevice (i, j).i;
                     end loop;
                  end loop;
                  --  for j,i:= 1 to number_of_con
                  exit when End_of_File (dev_file);
               end loop;
               --  end repeat
            end;
            --  [P2Ada]: end of WITH
            --  with
            --  if (tcompt^.f_file = nil) and fileexists=true
            --  if fileexists = false
            <<LABEL_100001>>
            close (dev_file);
         else
            bad_compt := true;
         end if;
      end if;
   end Device_Read;
   --  * Device_Read *
   --  *********************************************************************
end pfmsc;
