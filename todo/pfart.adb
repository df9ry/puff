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

package body pfart is
   --  [P2Ada]: This is for 'Write([Boolean])'

   package Boolean_Text_IO is new Enumeration_IO (Boolean);
   use Boolean_Text_IO;
   --  [P2Ada]: This is for 'file' without type
   --  [P2Ada]: This is for the Halt pseudo-procedure
   --  Bit map for artwork

   package Byte_Direct_IO is new Ada.Direct_IO (Unsigned_8);
   Program_halted : exception;
   bita : array (0 .. 1200) of Unsigned_8;
   dot_step, ydot, mb, rowl, xdot_max : integer;
   --  *
   --  Given node tnet, find out the width in the
   --  x and y direction of connecting parts for chamfer.
   --  *

   procedure get_widthxyO (tnet : net; widthx, widthy : in out Long_Float) is
      direction : integer;
      width : Long_Float;
      tcon : conn;
   begin
      widthx := 0;
      widthy := 0;
      tnet.all.nodet := 0;
      tcon := null;
      loop
         if tcon = null
         then
            tcon := tnet.all.con_start;
         else
            tcon := tcon.all.next_con;
         end if;
         direction := tcon.all.dir;
         if tcon.all.mate = null
         then
            width := widthZ0;
         else
            width := tcon.all.mate.all.net.all.com.all.width;
         end if;
         if width /= 0
         then
            tnet.all.nodet := tnet.all.nodet + direction;
            case direction is
               when 1 | 8 =>
                  widthx := width;
               when 2 | 4 =>
                  widthy := width;
               when others =>
                  --  [P2Ada]: no otherwise / else in Pascal
                  null;
            end case;
            --  case
         end if;
         exit when tcon.all.next_con = null;
      end loop;
   end get_widthxyO;
   --  get_widthxyO
   --  *
   --  Calculate corners of white triangle for chamfer.
   --  Returns different values of nx2,ny2 than init_line.
   --  Right-triangle coordinates returned are:
   --  (nx1,ny1) (90 degree corner), (nx1,yend) and (xend,ny1)
   --  where xend=nx1+nx2, yend=ny1+ny2
   --  xend and yend are computed and used in fill_shape.
   --  *

   procedure init_chamferO (tnet : net; widthx, widthy : Long_Float) is
      --  [P2Ada]: WITH instruction
      --  [P2Ada]: !Help! No type found -> add 'P2Ada_Var_1.' to fields
      hwidthx, hwidthy : integer;
   begin
      declare P2Ada_Var_1 : < > renames tnet.all;
      begin
         if (widthx * widthy = 0) or else (number_of_con /= 2)
         then
            chamfer := false;
         else
            chamfer := true;
         end if;
         hwidthx := Round (widthx * 0.5 / psx);
         hwidthy := Round (widthy * 0.5 / psy);
         if chamfer
         then
            case nodet is
               when 10 =>
                  nx1 := Round (xr / psx) - hwidthx;
                  ny1 := Round (yr / psy) - hwidthy;
                  nx2 := Round ((miter_fraction * 2.0) * widthx / psx);
                  ny2 := Round ((miter_fraction * 2.0) * widthy / psy);
               when 12 =>
                  nx1 := Round (xr / psx) + hwidthx;
                  ny1 := Round (yr / psy) - hwidthy;
                  nx2 := -Round ((miter_fraction * 2.0) * widthx / psx);
                  ny2 := Round ((miter_fraction * 2.0) * widthy / psy);
               when 5 =>
                  nx1 := Round (xr / psx) + hwidthx;
                  ny1 := Round (yr / psy) + hwidthy;
                  nx2 := -Round ((miter_fraction * 2.0) * widthx / psx);
                  ny2 := -Round ((miter_fraction * 2.0) * widthy / psy);
               when 3 =>
                  nx1 := Round (xr / psx) - hwidthx;
                  ny1 := Round (yr / psy) + hwidthy;
                  nx2 := Round ((miter_fraction * 2.0) * widthx / psx);
                  ny2 := -Round ((miter_fraction * 2.0) * widthy / psy);
               when others =>
                  chamfer := false;
            end case;
         end if;
         --  case
      end;
      --  [P2Ada]: end of WITH
      --  with tnet
   end init_chamferO;
   --  init_chamferO
   --  *
   --  Calculate dot positons of line tnet for artwork.
   --  *

   procedure init_lineO (tnet : net) is
      --  [P2Ada]: WITH instruction
      --  [P2Ada]: !Help! No type found -> add 'P2Ada_Var_2.' to fields
      xt : integer;
   begin
      lengthxy (tnet);
      declare P2Ada_Var_2 : < > renames tnet.all;
      begin
         nx1 := Round ((xr - yii * lengthxm / 2.0) / psx);
         ny1 := Round ((yr - xii * lengthym / 2.0) / psy);
         nx2 := nx1 + Round (lengthxm * (xii + yii) / psx);
         ny2 := ny1 + Round (lengthym * (yii + xii) / psy);
         if nx1 > nx2
         then
            xt := nx1;
            nx1 := nx2;
            nx2 := xt;
         end if;
         if ny1 > ny2
         then
            xt := ny1;
            ny1 := ny2;
            ny2 := xt;
         end if;
      end;
      --  [P2Ada]: end of WITH
      --  with tnet
   end init_lineO;
   --  init_lineO
   --  *
   --  Updated to function for LaserJet and Dot matrix data.
   --  Fills array bita[1..xdot_max] that will be sent to printer.
   --  For Dot matrix, bita[] is up to 960 dot columns (8").
   --  For LaserJet, bita[] is 8 rows of up to 150 dot rows
   --  connected end to end, causing the maximum
   --  size of bita to be 1200 bytes.
   --  Toggle between dot matrix and Laserjet routines is
   --  accomplished by examining boolean variable Laser_Art.
   --  Called by Net_Loop.
   --  *

   procedure fill_shape (tnet : net; corner : boolean) is
      --  **************************************************
      yval, xbeg, xend, ybeg, yend, ix, i, left, right, in_right, in_left,
         right_byte, left_byte : integer;
      dot_skip : Integer_8;
      slope : Long_Float;
      temp : Unsigned_8;
      --  *
      --  Used to white out a single pixel for chamfers.
      --  *

      procedure white_out (i, ix : integer) is
         mask : Unsigned_8;
      begin
         if Laser_Art
         then
            --  if LaserJet
            --  [P2Ada]: If modular type, better Shift_Right(,)
            --  if Dot matrix
            left := ix / 8;
            in_right := ix mod 8;
            mask := 128 / (2 ** in_right);
            bita (left + 150 * i) := bita (left + 150 * i) and then not (mask);
         else
            bita (ix) := bita (ix) and then not (temp);
         end if;
      end white_out;
      --  **************************************************
      --  *
      --  Use for filling array for the Laserjet Printer.
      --  Fills pixels (bita[] bytes) from nx1 to nx2.
      --  *

      procedure black_out (i : integer) is
         --  [P2Ada]: WITH instruction
         --  [P2Ada]: !Help! No type found -> add 'P2Ada_Var_3.' to fields
         ix : integer;
      begin
         declare P2Ada_Var_3 : < > renames tnet.all;
         begin
            --  [P2Ada]: If modular type, better Shift_Right(,)
            --  use shr and shl to form
            --  [P2Ada]: If modular type, better Shift_Left(,)
            --  bytes not full of pixels
            left := nx1 / 8;
            in_right := nx1 mod 8;
            right := nx2 / 8;
            in_left := 7 - nx2 mod 8;
            left_byte := 255 / (2 ** in_right);
            right_byte := (255 * (2 ** in_left)) and then 255;
            if left = right
            then
               --  for right > left fill partial pixel bytes
               bita (left + 150 * i)  := bita (left + 150 * i) or else
                                         (left_byte and then right_byte);
            else
               bita (left + 150 * i)  := bita (left + 150 * i) or else
                                         left_byte;
               bita (right + 150 * i) := bita (right + 150 * i) or else
                                         right_byte;
            end if;
            if right > left + 1
            then
               for ix in left + 1 .. right - 1
               loop
                  bita (ix + 150 * i) := 255;
               end loop;
            end if;
         end;
         --  [P2Ada]: end of WITH
         --  with
      end black_out;
      --  ***************************************************
   begin
      if Laser_Art
      then
         --  Laserjet has true 150 dpi
         dot_skip := 1;
      else
         dot_skip := 2;
      end if;
      --  Skip dot matrix dots to produce 2*72 dpi
      --  [P2Ada]: WITH instruction
      --  [P2Ada]: !Help! No type found -> add 'P2Ada_Var_4.' to fields
      declare P2Ada_Var_4 : < > renames tnet.all;
      begin
         if corner
         then
            --  chamfer corner
            ybeg := ny1;
            if ny2 < 0
            then
               ybeg := ybeg + ny2;
            end if;
            yend := ybeg + abs (ny2);
            if yend + 10 > ydot
            then
               remain := true;
            end if;
            xbeg := nx1;
            if nx2 < 0
            then
               xbeg := xbeg + nx2;
            end if;
            xend := xbeg + abs (nx2);
            if xbeg < 0
            then
               xbeg := 0;
            end if;
            if xbeg > xdot_max
            then
               xbeg := xdot_max;
            end if;
            if xend < 0
            then
               xend := 0;
            end if;
            if xend > xdot_max
            then
               xend := xdot_max;
            end if;
            if xbeg = xend
            then
               slope := 1;
            else
               slope := (yend - ybeg) / (xend - xbeg);
            end if;
            temp := 128;
            for i in 0 .. 7
            loop
               yval := ydot + dot_skip * i;
               if i /= 0
               then
                  --  [P2Ada]: If modular type, better Shift_Right(,)
                  temp := temp / (2 ** 1);
               end if;
               if (ybeg <= yval) and then (yval <= yend)
               then
                  if xend > rowl
                  then
                     rowl := xend;
                  end if;
                  for ix in xbeg .. xend
                  loop
                     case nodet is
                        when 10 =>
                           if yval < (yend - Round ((ix - xbeg) * slope))
                           then
                              white_out (i, ix);
                           end if;
                        when 12 =>
                           if yval < (ybeg + Round ((ix - xbeg) * slope))
                           then
                              white_out (i, ix);
                           end if;
                        when 5 =>
                           if yval > (yend - Round ((ix - xbeg) * slope))
                           then
                              white_out (i, ix);
                           end if;
                        when 3 =>
                           if yval > (ybeg + Round ((ix - xbeg) * slope))
                           then
                              white_out (i, ix);
                           end if;
                        when others =>
                           --  [P2Ada]: no otherwise / else in Pascal
                           null;
                     end case;
                     --  case
                  end loop;
                  --  for ix
               end if;
               --  if ybeg<= yval
            end loop;
            --  for i=0 to 7
            --  if not corner fill in shape
         else
            if ny2 + 10 > ydot
            then
               remain := true;
            end if;
            temp := 0;
            for i in 0 .. 7
            loop
               --  [P2Ada]: If modular type, better Shift_Left(,)
               temp := temp * (2 ** 1);
               if (ny1 <= ydot + dot_skip * i) and then
                  (ydot + dot_skip * i <= ny2)
               then
                  temp := (temp + 1);
                  if nx2 > rowl
                  then
                     rowl := nx2;
                  end if;
               end if;
               --  if y1
            end loop;
            --  for i
            if nx1 < 0
            then
               nx1 := 0;
            end if;
            --  * Clip corners *
            if nx1 > xdot_max
            then
               nx1 := xdot_max;
            end if;
            if nx2 < 0
            then
               nx2 := 0;
            end if;
            if nx2 > xdot_max
            then
               nx2 := xdot_max;
            end if;
            if temp > 0
            then
               if Laser_Art
               then
                  for i in reverse 0 .. 7
                  loop
                     if (temp and then 1) = 1
                     then
                        black_out (i);
                     end if;
                     --  [P2Ada]: If modular type, better Shift_Right(,)
                     temp := temp / (2 ** 1);
                  end loop;
                  --  for i
               else
                  for ix in nx1 .. nx2
                  loop
                     bita (ix) := bita (ix) or else temp;
                  end loop;
               end if;
            end if;
            --  if temp and/or Laser_Art
         end if;
      end;
      --  [P2Ada]: end of WITH
      --  if not corner
   end fill_shape;
   --  fill_shape
   --  *
   --  Perform artwork connections to external ports.
   --  *

   procedure fill_port (tNt : net) is
      tptyr, tptxr : Long_Float;
      tport, tpt : net;
      tcon : conn;
      nodet1, x1, y1, x2, y2 : integer;
   begin
      tcon := null;
      loop
         if tcon = null
         then
            tcon := tNt.all.con_start;
         else
            tcon := tcon.all.next_con;
         end if;
         if ext_port (tcon)
         then
            tport := portnet (tcon.all.port_type);
            y1 := Round (tNt.all.yr / psy);
            y2 := Round (tport.all.yr / psy);
            if ydot = 0
            then
               tpt := tport;
               x1 := Round (tNt.all.xr / psx);
               x2 := Round (tpt.all.xr / psx);
               tptxr := tpt.all.xr;
               tptyr := tpt.all.yr;
               New_n (tpt.all.other_net);
               tpt := tpt.all.other_net;
               tpt.all.ny1 := y2 - pwidthyZ02;
               tpt.all.ny2 := y2 + pwidthyZ02;
               if x1 < x2
               then
                  tpt.all.nx1 := x1;
                  tpt.all.nx2 := x2;
               else
                  tpt.all.nx1 := x2;
                  tpt.all.nx2 := x1;
               end if;
               New_n (tpt.all.other_net);
               tpt := tpt.all.other_net;
               tpt.all.nx1 := x1 - pwidthxZ02;
               tpt.all.nx2 := x1 + pwidthxZ02;
               if y1 < y2
               then
                  tpt.all.ny1 := y1;
                  tpt.all.ny2 := y2;
               else
                  tpt.all.ny1 := y2;
                  tpt.all.ny2 := y1;
               end if;
               if tNt.all.yr > tptyr
               then
                  if tNt.all.xr > tptxr
                  then
                     nodet1 := 12;
                  else
                     nodet1 := 10;
                  end if;
               else
                  if tNt.all.xr > tptxr
                  then
                     nodet1 := 5;
                  else
                     nodet1 := 3;
                  end if;
               end if;
               --  chamfers
               New_n (tpt.all.other_net);
               tpt := tpt.all.other_net;
               tpt.all.xr := tNt.all.xr;
               tpt.all.yr := tptyr;
               tpt.all.nodet := nodet1;
               tpt.all.number_of_con := 2;
               init_chamferO (tpt, widthZ0, widthZ0);
            end if;
            --  horiz line
            tport := tport.all.other_net;
            fill_shape (tport, false);
            if abs (y2 - y1) > pwidthyZ02
            then
               --  vert. line
               --  do chamfer
               tport := tport.all.other_net;
               fill_shape (tport, false);
               tport := tport.all.other_net;
               fill_shape (tport, true);
            end if;
         end if;
         --  if tcon
         exit when tcon.all.next_con = null;
      end loop;
   end fill_port;
   --  fill_port
   --  *
   --  Loop over parts for artwork mask.
   --  *

   procedure net_loop is
      tnet : net;
      widthx, widthy : Long_Float;
   begin
      remain := false;
      ydot := ydot + dot_step;
      tnet := null;
      loop
         if tnet = null
         then
            tnet := net_start;
         else
            tnet := tnet.all.next_net;
         end if;
         if ydot = 0
         then
            if tnet.all.node
            then
               get_widthxyO (tnet, widthx, widthy);
               init_chamferO (tnet, widthx, widthy);
            else
               dirn := tnet.all.con_start.all.dir;
               init_lineO (tnet);
               if tnet.all.com.all.typ = 'c'
               then
                  init_lineO (tnet.all.other_net);
               end if;
            end if;
            --  if tnet^.node
         end if;
         --  if ydot
         if not (tnet.all.node)
         then
            --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
            if tnet.all.com.all.typ and
               ('q' | 't' | 'c' => True, others => False)
            then
               fill_shape (tnet, false);
            end if;
            --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
            if tnet.all.com.all.typ and ('c' => True, others => False)
            then
               fill_shape (tnet.all.other_net, false);
            end if;
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
         if tnet.all.node
         then
            if tnet.all.ports_connected > 0
            then
               fill_port (tnet);
            end if;
            if tnet.all.chamfer
            then
               fill_shape (tnet, true);
            end if;
            --  fill chamfer
         end if;
         exit when tnet.all.next_net = null;
      end loop;
   end net_loop;
   --  * net_loop *
   --  *
   --  Used to generate HPGL files.
   --  Called in Plot via Ctrl-a when enabled in .puf file
   --  Code here is tricky! X and Y coordinates
   --  must be swapped for compatibility between printer
   --  coordinate systems and the plotter coordinate system.
   --  Units in millimeters must be changed to plotter units
   --  with conversion factor of 40 plu/mm.
   --  *

   procedure Make_HPGL_File is
      --  *****************************************************
      tport, tpt, tnet : net;
      tcon : conn;
      nodet1, drive, plu_size, xoffset, yoffset, lx, ly, x2, y2, max_x_plu,
         max_y_plu : integer;
      fname, pap_size : file_string;
      widthx, widthy, sf : Long_Float;
      pap_char : Character;
      --  *
      --  Calculate corners of white triangle for chamfer.
      --  Parameter units used are millimeters.
      --  *

      procedure HPGL_chamfer (tnet : net; widthx, widthy, sf : Long_Float) is
         --  [P2Ada]: WITH instruction
         --  [P2Ada]: !Help! No type found -> add 'P2Ada_Var_5.' to fields
         rx1, ry1, rx2, ry2 : Long_Float;
      begin
         declare P2Ada_Var_5 : < > renames tnet.all;
         begin
            if (widthx * widthy = 0) or else (number_of_con /= 2)
            then
               chamfer := false;
            else
               chamfer := true;
            end if;
            if chamfer
            then
               case nodet is
                  when 10 =>
                     rx1 := (xr - widthx / 2.0);
                     rx2 := +(miter_fraction * 2.0) * widthx;
                     ry1 := (yr - widthy / 2.0);
                     ry2 := +(miter_fraction * 2.0) * widthy;
                  when 12 =>
                     rx1 := (xr + widthx / 2.0);
                     rx2 := -(miter_fraction * 2.0) * widthx;
                     ry1 := (yr - widthy / 2.0);
                     ry2 := +(miter_fraction * 2.0) * widthy;
                  when 5 =>
                     rx1 := (xr + widthx / 2.0);
                     rx2 := -(miter_fraction * 2.0) * widthx;
                     ry1 := (yr + widthy / 2.0);
                     ry2 := -(miter_fraction * 2.0) * widthy;
                  when 3 =>
                     rx1 := (xr - widthx / 2.0);
                     rx2 := +(miter_fraction * 2.0) * widthx;
                     ry1 := (yr + widthy / 2.0);
                     ry2 := -(miter_fraction * 2.0) * widthy;
                  when others =>
                     chamfer := false;
               end case;
            end if;
            --  if chamfer, case
            if chamfer
            then
               --  [P2Ada]: !Help! Maybe (file,...) here
               --  [P2Ada]: !Help! Maybe (file,...) here
               --  [P2Ada]: !Help! Maybe (file,...) here
               --  [P2Ada]: !Help! Maybe (file,...) here
               --  [P2Ada]: !Help! Maybe (file,...) here
               --  [P2Ada]: !Help! Maybe (file,...) here
               --  [P2Ada]: !Help! Maybe (file,...) here
               --  [P2Ada]: !Help! Maybe (file,...) here
               --  [P2Ada]: !Help! Maybe (file,...) here
               --  [P2Ada]: !Help! Maybe (file)
               Put (net_file);
               Put ("PUPA");
               Put (xoffset + Round (sf * ry1));
               Put (',');
               Put (yoffset + Round (sf * (rx1 + rx2)));
               Put (";PDPA");
               Put (xoffset + Round (sf * (ry1 + ry2)));
               Put (',');
               Put (yoffset + Round (sf * (rx1)));
               Put (";PU;");
               New_Line;
            end if;
         end;
         --  [P2Ada]: end of WITH
         --  with tnet
      end HPGL_chamfer;
      --  HPGL_chamfer
      --  ********************************************************
      --  * Make_HPGL_File *
   begin
      if net_start = null
      then
         message (1) := "No circuit";
         message (2) := "to do HPGL file";
         write_message;
      else
         fname := input_string ("HP-GL file name:", "     (*.HPG)");
         if fname = ""
         then
            return;
         end if;
         if Pos (':', fname) = 2
         then
            drive := Character'Pos (fname (1)) - Character'Pos ('a') + 1;
         else
            drive := - 1;
         end if;
         if enough_space (drive)
         then
            if pos ('.', fname) = 0
            then
               fname := fname + ".HPG";
            end if;
            --  $I-
            --  $I+
            Assign (net_file, fname);
            Rewrite (net_file);
            if IOresult = 0
            then
               --  red* 40 plotter units per mm (plu/mm)
               --  board size in plu's
               sf := 40 * reduction;
               plu_size := Round (bmax * sf);
               pap_size := input_string ("Select Paper Size",
                                         " A,B,A4,A3: (A)");
               if pap_size = ""
               then
                  pap_char := 'A';
               else
                  pap_char := pap_size (1);
               end if;
               case pap_char is
                  --  determine maximum plotter unit
                  when 'a' | 'A' =>
                     if pap_size (2) = '3'
                     then
                        --  A3 size
                        max_x_plu := 16158;
                        max_y_plu := 11040;
                     else
                        if pap_size (2) = '4'
                        then
                           --  A4 size
                           --  A size
                           max_x_plu := 11040;
                           max_y_plu := 7721;
                        else
                           max_x_plu := 10365;
                           max_y_plu := 7962;
                        end if;
                     end if;
                  when 'b' | 'B' =>
                     --  B size
                     max_x_plu := 16640;
                     max_y_plu := 10365;
                  when others =>
                     begin
                        --  default to A
                        max_x_plu := 10365;
                        max_y_plu := 7962;
                     end;
               end case;
               --  case
               if (plu_size > max_x_plu) or else (plu_size > max_y_plu)
               then
                  --  use to page center
                  --  * Initialize and select pen 1 *
                  --  * Place paper size selection in file *
                  --  [P2Ada]: !Help! Maybe (file,...) here
                  message (1) := "Reduction ratio";
                  message (2) := "too large";
                  message (3) := "Edit .puf file";
                  write_message;
                  Close (net_file);
               else
                  xoffset := (max_x_plu - plu_size) / 2;
                  yoffset := (max_y_plu - plu_size) / 2;
                  tnet := null;
                  Put (net_file);
                  Put ("IN;SP1;");
                  if max_x_plu > 16000
                  then
                     --  [P2Ada]: !Help! Maybe (file,...) here
                     --  [P2Ada]: !Help! Maybe (file)
                     --  [P2Ada]: !Help! Maybe (file,...) here
                     --  [P2Ada]: !Help! Maybe (file)
                     Put (net_file);
                     Put ("PS0;");
                     New_Line;
                  else
                     Put (net_file);
                     Put ("PS4;");
                     New_Line;
                  end if;
                  loop
                     if tnet = null
                     then
                        tnet := net_start;
                     else
                        tnet := tnet.all.next_net;
                     end if;
                     if tnet.all.node
                     then
                        --  [P2Ada]: WITH instruction
                        --  [P2Ada]: !Help! No type found -> add 'P2Ada_Var_6.'
                        --  to fields
                        get_widthxyO (tnet, widthx, widthy);
                        HPGL_chamfer (tnet, widthx, widthy, sf);
                        declare P2Ada_Var_6 : < > renames tnet.all;
                        begin
                           if ports_connected > 0
                           then
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
                                    --  * Draw horizontal connection to port *
                                    --  y-start
                                    --  x-start
                                    --  goto corner,  X and Y swapped
                                    --  [P2Ada]: !Help! Maybe (file,...) here
                                    --  [P2Ada]: !Help! Maybe (file,...) here
                                    --  [P2Ada]: !Help! Maybe (file,...) here
                                    --  [P2Ada]: !Help! Maybe (file,...) here
                                    --  [P2Ada]: !Help! Maybe (file,...) here
                                    --  [P2Ada]: !Help! Maybe (file)
                                    --  delta-y
                                    --  delta-x
                                    --  Edge rectangle relative
                                    --  [P2Ada]: !Help! Maybe (file,...) here
                                    --  [P2Ada]: !Help! Maybe (file,...) here
                                    --  [P2Ada]: !Help! Maybe (file,...) here
                                    --  [P2Ada]: !Help! Maybe (file,...) here
                                    --  [P2Ada]: !Help! Maybe (file,...) here
                                    --  [P2Ada]: !Help! Maybe (file)
                                    tport := portnet (tcon.all.port_type);
                                    lx := Round (sf * (tport.all.yr - widthZ0
                                                 / 2.0));
                                    ly := Round (sf * (tport.all.xr));
                                    Put (net_file);
                                    Put ("PUPA");
                                    Put (xoffset + lx);
                                    Put (',');
                                    Put (yoffset + ly);
                                    Put (";PD;");
                                    New_Line;
                                    x2 := Round (sf * widthZ0);
                                    y2 := Round (sf * (xr - tport.all.xr));
                                    Put (net_file);
                                    Put ("ER");
                                    Put (x2);
                                    Put (',');
                                    Put (y2);
                                    Put (";PU;");
                                    New_Line;
                                    if abs tnet.all.yr - tport.all.yr >
                                                                 widthz0 / 2.0
                                    then
                                       --  * Draw vertical connection to port *
                                       --  x-start
                                       --  y-start
                                       --  goto corner,  X and Y swapped
                                       --  delta-x
                                       --  delta-y
                                       --  Edge rectangle relative
                                       ly := Round (sf * (xr - widthZ0 / 2.0));
                                       lx := Round (sf * (tport.all.yr));
                                       Put (net_file);
                                       Put ("PUPA");
                                       Put (xoffset + lx);
                                       Put (',');
                                       Put (yoffset + ly);
                                       Put (";PD;");
                                       New_Line;
                                       y2 := Round (sf * widthZ0);
                                       x2 := Round (sf * (yr - tport.all.yr));
                                       Put (net_file);
                                       Put ("ER");
                                       Put (x2);
                                       Put (',');
                                       Put (y2);
                                       Put (";PU;");
                                       New_Line;
                                       if yr > tport.all.yr
                                       then
                                          if xr > tport.all.xr
                                          then
                                             nodet1 := 12;
                                          else
                                             nodet1 := 10;
                                          end if;
                                       else
                                          if xr > tport.all.xr
                                          then
                                             nodet1 := 5;
                                          else
                                             nodet1 := 3;
                                          end if;
                                       end if;
                                       New_n (tpt);
                                       tpt.all.xr := xr;
                                       tpt.all.yr := tport.all.yr;
                                       tpt.all.nodet := nodet1;
                                       tpt.all.number_of_con := 2;
                                       HPGL_chamfer (tpt, widthZ0, widthZ0,
                                                     sf);
                                    end if;
                                    --  if abs(tnet..
                                 end if;
                                 --  if ext_port
                                 exit when tcon.all.next_con = null;
                              end loop;
                           end if;
                        end;
                        --  [P2Ada]: end of WITH
                        --  with tnet^ do, if tnet^.ports_connected
                        --  if tnet^.node
                        --  [P2Ada]: "x in y" -> "x and y" redefine "and"
                        --           before
                     else
                        if (tnet.all.com.all.typ and ('q' | 't' | 'c' => True,
                                                      others => False))
                        then
                           --  * Draw tlines, qlines, and clines *
                           --  [P2Ada]: WITH instruction
                           --  [P2Ada]: !Help! No type found -> add
                           --           'P2Ada_Var_7.' to fields
                           dirn := tnet.all.con_start.all.dir;
                           lengthxy (tnet);
                           declare P2Ada_Var_7 : < > renames tnet.all;
                           begin
                              lx := Round (sf * (yr - abs (xii) * lengthym
                                           / 2.0));
                              ly := Round (sf * (xr - abs (yii) * lengthxm
                                           / 2.0));
                              if yii = 0
                              then
                                 x2 := Round (sf * lengthym);
                              else
                                 x2 := Round (sf * yii * lengthym);
                              end if;
                              if xii = 0
                              then
                                 y2 := Round (sf * lengthxm);
                              else
                                 y2 := Round (sf * xii * lengthxm);
                              end if;
                              --  [P2Ada]: !Help! Maybe (file,...) here
                              --  [P2Ada]: !Help! Maybe (file,...) here
                              --  [P2Ada]: !Help! Maybe (file,...) here
                              --  [P2Ada]: !Help! Maybe (file,...) here
                              --  [P2Ada]: !Help! Maybe (file,...) here
                              --  [P2Ada]: !Help! Maybe (file)
                              --  * Edge rectangle relative *
                              --  [P2Ada]: !Help! Maybe (file,...) here
                              --  [P2Ada]: !Help! Maybe (file,...) here
                              --  [P2Ada]: !Help! Maybe (file,...) here
                              --  [P2Ada]: !Help! Maybe (file,...) here
                              --  [P2Ada]: !Help! Maybe (file,...) here
                              --  [P2Ada]: !Help! Maybe (file)
                              Put (net_file);
                              Put ("PUPA");
                              Put (xoffset + lx);
                              Put (',');
                              Put (yoffset + ly);
                              Put (";PD;");
                              New_Line;
                              Put (net_file);
                              Put ("ER");
                              Put (x2);
                              Put (',');
                              Put (y2);
                              Put (";PU;");
                              New_Line;
                           end;
                           --  [P2Ada]: end of WITH
                           --  with tnet
                           if tnet.all.com.all.typ = 'c'
                           then
                              --  [P2Ada]: WITH instruction
                              --  [P2Ada]: !Help! No type found ->
                              --           add 'P2Ada_Var_8.' to fields
                              declare P2Ada_Var_8 : < > renames tnet.all.other_net.all;
                              begin
                                 --  [P2Ada]: !Help! Maybe (file,...) here
                                 --  [P2Ada]: !Help! Maybe (file,...) here
                                 --  [P2Ada]: !Help! Maybe (file,...) here
                                 --  [P2Ada]: !Help! Maybe (file,...) here
                                 --  [P2Ada]: !Help! Maybe (file,...) here
                                 --  [P2Ada]: !Help! Maybe (file)
                                 --  * Edge rectangle relative *
                                 --  [P2Ada]: !Help! Maybe (file,...) here
                                 --  [P2Ada]: !Help! Maybe (file,...) here
                                 --  [P2Ada]: !Help! Maybe (file,...) here
                                 --  [P2Ada]: !Help! Maybe (file,...) here
                                 --  [P2Ada]: !Help! Maybe (file,...) here
                                 --  [P2Ada]: !Help! Maybe (file)
                                 lx := Round (sf * (yr - abs (xii) * lengthym
                                              / 2.0));
                                 ly := Round (sf * (xr - abs (yii) * lengthxm
                                              / 2.0));
                                 Put (net_file);
                                 Put ("PUPA");
                                 Put (xoffset + lx);
                                 Put (',');
                                 Put (yoffset + ly);
                                 Put (";PD;");
                                 New_Line;
                                 Put (net_file);
                                 Put ("ER");
                                 Put (x2);
                                 Put (',');
                                 Put (y2);
                                 Put (";PU;");
                                 New_Line;
                              end;
                              --  [P2Ada]: end of WITH
                           end if;
                           --  if tnet^ = c, with tnet
                        end if;
                     end if;
                     --  if not tnet^.node
                     exit when tnet.all.next_net = null;
                  end loop;
                  --  * Present paper and put pen away *
                  --  [P2Ada]: !Help! Maybe (file,...) here
                  --  [P2Ada]: !Help! Maybe (file,...) here
                  --  [P2Ada]: !Help! Maybe (file,...) here
                  --  [P2Ada]: !Help! Maybe (file)
                  Put (net_file);
                  Put ("IP;PA0,");
                  Put (max_y_plu);
                  Put (";SP0;");
                  New_Line;
                  Close (net_file);
                  message (1) := "HP-GL data";
                  message (2) := "written to file";
                  message (3) := fname;
                  write_message;
               end if;
               --  if reduction too small
            end if;
            --  if IOResult=0
         end if;
         --  if enoughspace
      end if;
      --  if not net_start= nil
   end Make_HPGL_File;
   --  *Make_HPGL_File*
   --  *
   --  Revised Procedure for directing artwork to dot-matrix
   --  or LaserJet printers.
   --  *

   procedure Printer_Artwork is
      --  [BP2P]: Label "100001" Was "exit_artwork"
      --  local printer file; hides the global variable of the same name
      --  ***************************************************
      ix : integer;
      lpt_label : string;
      lst : Ada.Text_IO.File_Type;
      --  *
      --  Prompt user for labels to on the top of artwork mask.
      --  *

      function top_labels return boolean is
         Result_top_labels : boolean;
      begin
         Result_top_labels := false;
         Result_top_labels := true;
         name := input_string (lpt_label, " Enter label #1");
         network_name := input_string (lpt_label, " Enter label #2");
         return Result_top_labels;
      end top_labels;
      --  top_labels
      --  ***************************************************
      --  *
      --  Print labels on the top of artwork mask.
      --  *

      procedure Matrix_labels is
         --  switch on  emphasised, double strike
         --  switch off emphasised, double strike
      begin
         Put (lst, Character'Val (27) + 'E');
         Put (lst, Character'Val (27) + 'G');
         Put (lst, name, 26 + (name'Length) / 2);
         New_Line (lst);
         Put (lst, network_name, 26 + (network_name'Length) / 2);
         New_Line (lst);
         Put (lst, Character'Val (27) + 'F');
         Put (lst, Character'Val (27) + 'H');
         p_labels := false;
      end Matrix_labels;
      --  print_labelsO
      --  ***************************************************
      --  *
      --  Initialize laser printer and put labels
      --  on the top of artwork mask.
      --  *

      procedure Laser_labels is
         --  Reset Printer
         --  Put in landscape mode, 8 1/2 x 11
         --  page size, ASCII symbol set, fixed spacing,
         --  10cpi, 12pt, upright, courier bold font
         --  position cursor for text
         --  move down 70 dots
         --  position cursor for text
         --  add carriage return
         --  turn off bold font
         --  center artwork
         --  Put in raster graphics 150 dpi mode
         --  start graphics at current cursor position
         xpos, ypos : integer;
      begin
         Put (lst, Character'Val (27));
         Put (lst, 'E');
         Put (lst, Character'Val (27));
         Put (lst, "&l0O");
         Put (lst, Character'Val (27));
         Put (lst, "&l2A");
         Put (lst, Character'Val (27));
         Put (lst, "(0U");
         Put (lst, Character'Val (27));
         Put (lst, "(s0p10h12v0s3b3T");
         xpos := 1200 - 30 * (name'Length) / 2;
         ypos := 1450 - xdot_max;
         Put (lst, Character'Val (27));
         Put (lst, "*p");
         Put (lst, xpos);
         Put (lst, 'x');
         Put (lst, ypos);
         Put (lst, 'Y');
         Put (lst, name);
         xpos := 1200 - 30 * (network_name'Length) / 2;
         ypos := 1500 - xdot_max;
         Put (lst, Character'Val (27));
         Put (lst, "*p");
         Put (lst, xpos);
         Put (lst, 'x');
         Put (lst, ypos);
         Put (lst, 'Y');
         Put (lst, network_name);
         Put (lst, Character'Val (13));
         Put (lst, Character'Val (27));
         Put (lst, "(s0B");
         xpos := 1200 - xdot_max;
         ypos := 1600 - xdot_max;
         Put (lst, Character'Val (27));
         Put (lst, "*p");
         Put (lst, xpos);
         Put (lst, 'x');
         Put (lst, ypos);
         Put (lst, 'Y');
         Put (lst, Character'Val (27));
         Put (lst, "*t150R");
         Put (lst, Character'Val (27));
         Put (lst, "*r1A");
         p_labels := false;
      end Laser_labels;
      --  Laser_labels
      --  ***************************************************
      --  *
      --  *

      procedure Send_Matrix_Data is
         ix : integer;
      begin
         if not (p_labels)
         then
            if rowl > xdot_max
            then
               rowl := xdot_max;
            end if;
            --  * Put printer in dual-density bit-image
            --  graphics mode (half-speed) and specify
            --  total number of bit image bytes
            --  to be n=n1+(n2*256)   *
            Put (lst, Character'Val (27) + 'L');
            Put (lst, Character'Val ((rowl + 1) mod 256));
            Put (lst, Character'Val ((rowl + 1) / 256));
            for ix in 0 .. rowl
            loop
               Put (lst, Character'Val (bita (ix)));
            end loop;
            --  * Write row of dot-columns *
            --  * Carriage return of given spacing *
            Put (lst, Character'Val (13));
            if (mb) mod 2 /= 0
            then
               --  * 13/216" spacing *
               Put (lst, Character'Val (27) + 'J');
               Put (lst, Character'Val (13));
            else
               Put (lst, Character'Val (27) + 'J');
               Put (lst, Character'Val (11));
            end if;
            --  * 11/216" spacing *
         end if;
         --  if not(p_labels)
         if (mb) mod 2 /= 0
         then
            --  * Alternate dot steps for *
            dot_step := 9;
         else
            dot_step := 7;
         end if;
         --  * different spacings *
      end Send_Matrix_Data;
      --  Send_Matrix_Data
      --  ***************************************************
      --  *
      --  *

      procedure Send_Laser_Data is
         ix, iy, byte_total : integer;
      begin
         if not (p_labels)
         then
            if rowl > xdot_max
            then
               rowl := xdot_max;
            end if;
            byte_total := rowl / 8 + 1;
            if (rowl mod 8) > 0
            then
               byte_total := byte_total + 1;
            end if;
            for iy in 0 .. 7
            loop
               --  * loop for 8 rows *
               --  * prepare to send data bytes *
               Put (lst, Character'Val (27));
               Put (lst, "*b");
               Put (lst, byte_total);
               Put (lst, 'W');
               for ix in 0 .. (byte_total - 1)
               loop
                  Put (lst, Character'Val (bita (ix + 150 * iy)));
               end loop;
               --  * send data *
            end loop;
            --  for iy
         end if;
         --  if not(p_labels)
         --  * 8 rows of data *
         dot_step := 8;
      end Send_Laser_Data;
      --  Send_Laser_Data
      --  ***************************************************
      --  * Printer_Artwork *
   begin
      if net_start = null
      then
         message (1) := "No circuit";
         message (2) := "to do artwork";
         write_message;
      else
         if reduction * bmax > 8 * 25.4
         then
            --  *--------------------------------------------------------------
            --  <Linux Printer Options>
            --  The following lines determine where printer-data will be sent.
            --  You should uncomment (and possibly change) the option that
            --- suits your system.
            --  The first option just sends it to /dev/null, i.e., the data
            --  will be ignored:
            --  *
            --  * If you have a suitable printer, you can simply send the data
            --  to the printer, through the /usr/bin/lpr program. Note that the
            --  data contains all kind of control sequences, so it should not
            --  be interpreted by any printer filters. In this example, we
            --  assume that on your system a printer called 'raw' is defined
            --  for this purpose:
            --  *
            --  assignlst(lst, '|/usr/bin/lpr -Praw');
            --  * You may also want to send the data to a simple file, and send
            --  it to the printer by hand. This is e.g. useful if the printer
            --  is not directly reachable from this machine. As an example, we
            --  send the data to a file named 'puff.lst':
            --  *
            --  assignlst(lst, '/tmp/puff.lst|');
            --  * Note the '|' at the end of the filename. If you remove it,
            --  the file will also be sent to the printer using the 'lpr'
            --  program, and deleted afterwards.
            --  See the documentation of the free pascal compiler for more
            --  information on printing.
            --  --------------------------------------------------------------*
            --  * These initial values activate *
            --  * chamfer routines in net_loop  *
            message (1) := "Reduction ratio";
            message (2) := "too large";
            message (3) := "Edit .puf file";
            write_message;
         else
            assignlst (lst, "/dev/null|");
            rewrite (lst);
            ydot := 0;
            dot_step := 0;
            remain := true;
            if Laser_Art
            then
               --  maximum number of dots at 150 dpi
               xdot_max := Round (reduction * bmax * 150 / 25.4);
               if xdot_max > 1200
               then
                  xdot_max := 1200;
               end if;
               --  maximum number of dots at 120 dpi
               lpt_label := "  LaserJet Art";
            else
               xdot_max := Round (reduction * bmax * 120 / 25.4);
               if xdot_max > 960
               then
                  xdot_max := 960;
               end if;
               lpt_label := " Dot-Matrix Art";
            end if;
            mb := -1;
            p_labels := top_labels;
            if p_labels
            then
               message (2) := "Press h to halt";
               write_message;
               while remain
               loop
                  if keypressed
                  then
                     --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
                     chs := ReadKey;
                     if chs and ('h' | 'H' => True, others => False)
                     then
                        --  [BP2P]: Label "100001" Was "exit_artwork"
                        message (2) := "      HALT       ";
                        write_message;
                        goto LABEL_100001;
                     end if;
                     --  if key
                     beep;
                  end if;
                  --  if keypressed
                  rowl := 0;
                  for ix in 0 .. 1200
                  loop
                     bita (ix) := 0;
                  end loop;
                  --  Initialize data
                  mb := mb + 1;
                  Net_Loop;
                  if (rowl > 0) and then p_labels
                  then
                     if Laser_Art
                     then
                        Laser_labels;
                     else
                        Matrix_labels;
                     end if;
                  end if;
                  if Laser_Art
                  then
                     Send_Laser_Data;
                  else
                     Send_Matrix_Data;
                  end if;
               end loop;
               --  while remain
               message (2) := "Artwork completed";
               write_message;
               if Laser_Art
               then
                  Put (lst, Character'Val (27));
                  Put (lst, 'E');
               end if;
               --  reset LaserJet, eject page
            end if;
            --  if p_labels
            <<LABEL_100001>>
            close (lst);
         end if;
      end if;
   end Printer_Artwork;
   --  * Printer_Artwork *
end pfart;
