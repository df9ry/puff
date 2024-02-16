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
--  *
--  internal:
--  Procedure Make_coord_and_parts_listO;
--  Procedure Make_Titles;
--  Function CommandLine;
--  Procedure Init_Puff_Parameters;
--  *
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

package pfst is

   procedure Puff_Start;

   procedure Screen_Init;

   procedure Screen_Plan;
end pfst;
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

package body pfst is
   --  [P2Ada]: This is for 'Write([Boolean])'

   package Boolean_Text_IO is new Enumeration_IO (Boolean);
   use Boolean_Text_IO;
   --  [P2Ada]: This is for 'file' without type
   --  [P2Ada]: This is for the Halt pseudo-procedure

   package Byte_Direct_IO is new Ada.Direct_IO (Unsigned_8);
   Program_halted : exception;
   --  *
   --  Set up linked list of components for all
   --  parameters in the Plot window and
   --  Board Window.
   --  Called by Puff_Start.
   --  *

   procedure Make_coord_and_parts_listO is
      --  set-up coordinates linked list
      tcompt : compt;
      i : integer;
   begin
      for i in 1 .. 10
      loop
         if i = 1
         then
            coord_start := new #01 Help #01 < >;
            tcompt := coord_start;
         else
            tcompt.all.next_compt := new #01 Help #01 < >;
            tcompt.all.next_compt.all.prev_compt := tcompt;
            tcompt := tcompt.all.next_compt;
         end if;
      end loop;
      --  set-up parts linked list
      tcompt.all.next_compt := coord_start;
      coord_start.all.prev_compt := tcompt;
      for i in 1 .. 18
      loop
         if i = 1
         then
            part_start := new #01 Help #01 < >;
            tcompt := part_start;
            tcompt.all.prev_compt := null;
            tcompt.all.yp := ymin (3);
         else
            tcompt.all.next_compt := new #01 Help #01 < >;
            tcompt.all.next_compt.all.prev_compt := tcompt;
            tcompt := tcompt.all.next_compt;
            tcompt.all.yp := tcompt.all.prev_compt.all.yp + 1;
         end if;
         --  [P2Ada]: WITH instruction
         --  [P2Ada]: !Help! No type found -> add 'P2Ada_Var_1.' to fields
         declare P2Ada_Var_1 : < > renames tcompt.all;
         begin
            --  initialize for device and indef
            descript := Character (Character'Pos ('a') + i - 1) + ' ';
            changed := false;
            right := false;
            parsed := false;
            sweep_compt := false;
            f_file := null;
            s_file := null;
            s_ifile := null;
            used := 0;
            x_block := 2;
            if (i <= 9)
            then
               xp := xmin (3);
               xmaxl := xmax (3) - xmin (3);
            else
               xp := xmin (5);
               xmaxl := xmax (5) - xmin (5);
            end if;
         end;
         --  [P2Ada]: end of WITH
         --  with
      end loop;
      --  i
      --  set-up board linked list beginning at board_start
      tcompt.all.next_compt := null;
      for i in 1 .. 6
      loop
         if i = 1
         then
            board_start := new #01 Help #01 < >;
            tcompt := board_start;
            tcompt.all.prev_compt := null;
         else
            tcompt.all.next_compt := new #01 Help #01 < >;
            tcompt.all.next_compt.all.prev_compt := tcompt;
            tcompt := tcompt.all.next_compt;
         end if;
      end loop;
      --  i
      tcompt.all.next_compt := null;
   end Make_coord_and_parts_listO;
   --  * Make_coord_and_parts_listsO*
   --  *
   --  Set up titles for the three windows.
   --  *

   procedure Make_Titles is
      i : integer;
   begin
      for i in 1 .. 4
      loop
         --  was to 3
         window_f (i) := new #01 Help #01 < >;
         command_f (i) := new #01 Help #01 < >;
         case i is when 1 =>
            --  [P2Ada]: WITH instruction
            --  [P2Ada]: !Help! No type found -> add 'P2Ada_Var_2.' to fields
            declare P2Ada_Var_2 : < > renames window_f (1).all;
            begin
               --  xp was 2
               xp := layout_position (1);
               yp := layout_position (2);
               descript := " F1 : LAYOUT ";
            end;
            --  [P2Ada]: end of WITH
            --  [P2Ada]: WITH instruction
            --  [P2Ada]: !Help! No type found -> add 'P2Ada_Var_3.' to fields
            declare P2Ada_Var_3 : < > renames command_f (1).all;
            begin
               --  place header above board window
               descript := " LAYOUT HELP ";
               xp := 1 + (xmax (4) + xmin (4) - (descript'length)) / 2;
               yp := ymin (4) - 1;
            end;
            --  [P2Ada]: end of WITH
         when 2 =>
            --  [P2Ada]: WITH instruction
            --  [P2Ada]: !Help! No type found -> add 'P2Ada_Var_4.' to fields
            declare P2Ada_Var_4 : < > renames window_f (2).all;
            begin
               --  xp was 41
               descript := " F2 : PLOT ";
               xp := 1 + (xmax (2) + xmin (2) - (descript'length)) / 2;
               yp := ymin (2) - 1;
            end;
            --  [P2Ada]: end of WITH
            --  [P2Ada]: WITH instruction
            --  [P2Ada]: !Help! No type found -> add 'P2Ada_Var_5.' to fields
            declare P2Ada_Var_5 : < > renames command_f (2).all;
            begin
               descript := " PLOT HELP ";
               xp := 1 + (xmax (4) + xmin (4) - (descript'length)) / 2;
               yp := ymin (4) - 1;
            end;
            --  [P2Ada]: end of WITH
         when 3 =>
            --  [P2Ada]: WITH instruction
            --  [P2Ada]: !Help! No type found -> add 'P2Ada_Var_6.' to fields
            declare P2Ada_Var_6 : < > renames window_f (3).all;
            begin
               --  xp was 2
               descript := " F3 : PARTS ";
               xp := 1 + (xmax (3) + xmin (3) - (descript'length)) / 2;
               yp := ymin (3) - 1;
            end;
            --  [P2Ada]: end of WITH
            --  [P2Ada]: WITH instruction
            --  [P2Ada]: !Help! No type found -> add 'P2Ada_Var_7.' to fields
            declare P2Ada_Var_7 : < > renames command_f (3).all;
            begin
               --  place header above board window
               descript := " PARTS HELP ";
               xp := 1 + (xmax (4) + xmin (4) - (descript'length)) / 2;
               yp := ymin (4) - 1;
            end;
            --  [P2Ada]: end of WITH
         when 4 =>
            --  [P2Ada]: WITH instruction
            --  [P2Ada]: !Help! No type found -> add 'P2Ada_Var_8.' to fields
            declare P2Ada_Var_8 : < > renames window_f (4).all;
            begin
               --  xp was 2
               descript := " F4 : BOARD ";
               xp := 1 + (xmax (4) + xmin (4) - (descript'length)) / 2;
               yp := ymin (4) - 1;
            end;
            --  [P2Ada]: end of WITH
            --  board values are drawn over the command box
            --  [P2Ada]: WITH instruction
            --  [P2Ada]: !Help! No type found -> add 'P2Ada_Var_9.' to fields
            declare P2Ada_Var_9 : < > renames command_f (4).all;
            begin
               descript := " BOARD HELP ";
               xp := 1 + (xmax (3) + xmin (3) - (descript'length)) / 2;
               yp := ymin (3) - 1;
            end;
            --  [P2Ada]: end of WITH
            --  board help window is drawn over parts window
         when others =>
            --  [P2Ada]: no otherwise / else in Pascal
            null;
      end case;
      --  case
   end loop;
   --  for i
end Make_Titles;
--  * Make_Titles *
--  *
--  Get information which might follow initial
--  PUFF command eg. c> puff lowpass.
--  Use -D to put in demo mode.
--  *

function CommandLine return file_string is
   --  returns first command line parameter
   Result_CommandLine : file_string;
   Buffer : file_string;
   i : integer;
begin
   Buffer := Argument (1);
   if Pos ("-D", buffer) > 0
   then
      buffer := "";
      demo_mode := true;
   else
      demo_mode := false;
   end if;
   if Pos ("-BW", buffer) > 0
   then
      buffer := Argument (2);
      blackwhite := true;
      for i in 1 .. 4
      loop
         col_window (i) := white;
         s_color (i) := white;
      end loop;
   else
      blackwhite := FALSE;
   end if;
   if (buffer = "")
   then
      commandline := "setup";
   else
      commandline := buffer;
   end if;
   return Result_CommandLine;
end CommandLine;
--  * Commandline *
--  *
--  Preparation required before reading board
--  *

procedure Prep_to_Read_Board is
   i : integer;
begin
   for i in 1 .. 8
   loop
      --  initialize board values
      --  initialize board unit prefixes
      s_board (i, 1) := ' ';
      s_board (i, 2) := ' ';
   end loop;
   board_read := false;
   for i in 1 .. 12
   loop
      board (i) := false;
   end loop;
end Prep_to_Read_Board;
--  * Prep_to_Read_Board *
--  *
--  Initializes linked lists and windows.
--  Set up to be called only after the board
--  parameters have been read, but before the key!
--  Set_Up_Board must be called sometime after
--  this procedure when reading new graphics.
--  *

procedure Init_Puff_Parameters is
   --  * Initialize s-parameter linked lists *
   i, ij : integer;
   cspc : spline_param;
begin
   for xpt in 0 .. ptmax
   loop
      if xpt = 0
      then
         spline_start := new #01 Help #01 < >;
         cspc := spline_start;
      else
         cspc.all.next_c := new #01 Help #01 < >;
         cspc.all.next_c.all.prev_c := cspc;
         cspc := cspc.all.next_c;
      end if;
      for ij in 1 .. max_params
      loop
         if xpt = 0
         then
            plot_start (ij) := new #01 Help #01 < >;
            c_plot (ij) := plot_start (ij);
         else
            c_plot (ij).all.next_p := new #01 Help #01 < >;
            c_plot (ij).all.next_p.all.prev_p := c_plot (ij);
            c_plot (ij) := c_plot (ij).all.next_p;
         end if;
      end loop;
   end loop;
   --  xpt
   --  * Prep to read key *
   Make_Coord_and_Parts_ListO;
   compt1 := null;
   Make_Titles;
   for i in 1 .. 6
   loop
      s_key (i) := ' ';
   end loop;
   for i in 7 .. 10
   loop
      s_key (i) := "";
   end loop;
   --  pfun2
   --  begin with impedance Smith chart
   Set_Up_KeyO;
   admit_chart := false;
end Init_Puff_Parameters;
--  * Init_Puff_Parameters *
--  *
--  Start puff and print mode information.
--  *

procedure Puff_Start is
   --  ***********************************************
   memK : Integer_32;
   i : integer;
   Ch : Character;

   procedure Blank_Blue_Cursor is
      --  Put cursor near bottom of screen
      --  on Linux we don't blank the cursor
   begin
      GotoXY (64, 22);
   end Blank_Blue_Cursor;
   --  *****************************************************
   --  * Puff_Start *
   --  2K buffer for device files
   --  2K buffer for net_files
   --  Remember initial text mode
   --  Put in 80x25 Color Display Mode
   --  Read input string for file name
   --  MemAvail is not available with the fpc compiler
begin
   SetTextBuf (dev_file, big_text_buf);
   SetTextBuf (net_file, big_text_buf);
   OrigMode := Lastmode;
   TextMode (co80);
   puff_file := Commandline;
   memK := 2000 * 1024;
   if puff_file = "setup"
   then
      --  * if setup.puf to be loaded then display information *
      --  make border does a ClrScr to black
      --  *** Make_Text_Border doesn't work here ****
      TextBackground (blue);
      ClrScr;
      TextCol (yellow);
      for i in 2 .. 23
      loop
         --  * Draw Border for startup screen *
         GotoXY (2, i);
         Put (Character'Val (186));
         GotoXY (79, i);
         Put (Character'Val (186));
      end loop;
      for i in 3 .. 78
      loop
         GotoXY (i, 2);
         Put (Character'Val (205));
         GotoXY (i, 24);
         Put (Character'Val (205));
      end loop;
      --  make cursor invisible
      GotoXY (2, 2);
      Put (Character'Val (201));
      GotoXY (79, 2);
      Put (Character'Val (187));
      GotoXY (2, 24);
      Put (Character'Val (200));
      GotoXY (79, 24);
      Put (Character'Val (188));
      GotoXY (17, 24);
      Put (" Press Esc to abort, any other key to run Puff ");
      TextCol (white);
      GotoXY (32, 4);
      Put ("PUFF, Version 20181104");
      Gotoxy (31, 7);
      Put ("Copyright (C) 1991");
      GotoXY (15, 8);
      Put ("Scott W. Wedge, Richard Compton, David Rutledge");
      GotoXY (30, 10);
      Put ("(C) 1997, 1998 Andreas Gerstlauer");
      GotoXY (8, 13);
      Put ("2010 - Code released under the GNU General Public License version 3");
      GotoXY (25, 16);
      Put ("Linux version by:");
      GotoXY (25, 18);
      Put ("2000-2018 Pieter-Tjerk de Boer");
      GotoXY (25, 19);
      Put ("     2009 Leland C. Scott");
      GotoXY (2, 27);
      Put ("For more information, see:");
      GotoXY (2, 29);
      Put ("http://www.its.caltech.edu/~mmic/puff.html -> for the original version");
      GotoXY (2, 31);
      Put ("http://wwwhome.cs.utwente.nl/~ptdeboer/ham/puff.html -> for the Linux version");
      TextCol (white);
      message_color := white;
      Blank_Blue_Cursor;
      loop
         Ch := ReadKey;
         exit when (Ch /= screenresize);
      end loop;
      if (Ch = Character'Val (0))
      then
         Ch := Character (Character'Pos (ReadKey) + 128);
      end if;
      if Ch = Esc
      then
         --  Has the Esc key been pressed?
         --  Abort and restore textmode if Esc pressed
         TextMode (OrigMode);
         ClrScr;
         raise Program_halted (2);
      end if;
   end if;
   --  if 'setup'
   --  erase blue and yellow to leave black screen
   --  Initialize message box, in case early errors result
   --  define complex variable co1 = 1+j0
   --  erase key list
   --  **
   --  Check here for enough memory to run Puff
   --  - Additional data structures require 17,296 bytes.
   --  - Simple circuit will require 36,864 bytes.
   --  - 128 bytes/point required for plot data.
   --  Minimum is 50 points.
   --  **
   TextBackground (Black);
   ClrScr;
   Max_Text_Y := 25;
   Max_Text_X := 80;
   message_color := lightred;
   xmin (6) := 32;
   ymin (6) := 11;
   xmax (6) := 49;
   ymax (6) := 13;
   insert_key := true;
   Large_Smith := false;
   Large_Parts := false;
   Extra_Parts_Used := false;
   co (co1, 1.0, 0);
   key_end := 0;
   memK := Round (memK / 1024);
   case memK is
      when 1 .. 80 =>
         message (2) := "Insufficient";
         message (3) := "memory to run Puff";
         shutdown;
      when 81 .. 100 =>
         ptmax := 50;
      when 101 .. 150 =>
         ptmax := 100;
      when 151 .. 200 =>
         ptmax := 250;
      when 201 .. 1000 =>
         ptmax := 500;
      when others =>
         ptmax := 1000;
   end case;
   --  case
   --  * Init_Puff_Parameters; *
   Prep_to_Read_Board;
   read_kbd := true;
   if pos ('.', puff_file) = 0
   then
      puff_file := puff_file + ".puf";
   end if;
   if not (fileexists (puff_file /= "setup.puf", net_file, puff_file))
   then
      if not (setupexists (puff_file))
      then
         erase_message;
         message (2) := "setup.puf";
         message (3) := "not found";
         shutdown;
      end if;
   end if;
   Close (net_file);
end Puff_Start;
--  * Puff_Start *
--  *
--  Set up extendended graphics characters.
--  Called by Screen_Init.
--  *

procedure Set_Up_Char is
   --  [P2Ada]: empty in Pascal
begin
   null;
end Set_Up_Char;
--  *Set_Up_Char*
--  ******************** GRAPHICS INITIALIZATION *******************
--  *
--  Plan how to subdivide the total screen into our "windows".
--  *

procedure Screen_Plan is
   --  Screen_Plan
   --  1:1 aspect ratio for VGA
   --  xmax[12]:=639;
   --  ymax[12]:=479;
   --  if (not its_VGA) then begin
   --  its_VGA:=True;
   --  xmax[12]:=1279;
   --  ymax[12]:=959;
   --  end;
   --  *********************************************
   --  Graphics Parameter Constants
   --  Window indexing system:
   --  [1] : Layout window (graphics),
   --  [2] : Plot window (text),
   --  [3] : Parts window (text),
   --  [4] : Board window (text),
   --  [5] : Extra parts (text),
   --  [6] : Help window (text),
   --  [7] : Smith chart (graphics),
   --  [8] : x-y plot (graphics),
   --  [9] : Layout erase region (graphics),
   --  [10]: Smith erase region (graphics),
   --  [11]: x-y plot erase region (graphics),
   --  [12]: full window size (graphics).
   --  ************************************************
   --  total number of text rows
   --  from the excess rows beyond 34, first 1 or 2 are used to expand the BOARD window (to make the entire help message fit); anything left is used to expand the PARTS window
   i : integer;
   parts_height : integer;
   board_height : integer;
begin
   xmin (12) := 0;
   ymin (12) := 0;
   xmax (12) := ScreenWidth;
   ymax (12) := ScreenHeight;
   yf := 1.0;
   xmin (2) := 2;
   xmax (2) := 23;
   xmin (3) := 2;
   xmax (3) := 23;
   xmin (4) := 2;
   xmax (4) := 23;
   xmin (5) := 2;
   xmax (5) := 23;
   xmin (6) := 2;
   xmax (6) := 23;
   i := ymax (12) / 14;
   board_height := 7;
   parts_height := 9;
   if (i = 35)
   then
      board_height := 8;
   end if;
   if (i >= 36)
   then
      board_height := 9;
      parts_height := i - 27;
      if (parts_height > 18)
      then
         parts_height := 18;
      end if;
   end if;
   --  plot window
   --  parts window
   --  board window
   --  extra parts window
   --  help window
   --  half of screen height, rounded down to a multiple of character height
   --  in principle, that's the height at which we put the boundary between layout+smith and plot
   --  however, both layout and smith need to be more or less square/round, so this may not fit
   --  in that case, move this boundary up until it fits
   --  I'm lazy, so just make it a loop, instead of computing the right values straight away...
   ymin (2) := 2;
   ymax (2) := 8;
   ymin (3) := 16;
   ymax (3) := ymin (3) + parts_height - 1;
   ymin (4) := ymax (3) + 3;
   ymax (4) := ymin (4) + board_height - 1;
   ymin (5) := ymax (3) + 1;
   ymax (5) := ymin (3) + 17;
   ymin (6) := 11;
   ymax (6) := 13;
   i := 14 * ((ymax (12) + 1) / 28);
   i := i + 14;
   xmin (9) := (xmax (2) + 1) * 8;
   ymin (9) := 0;
   ymin (10) := 0;
   xmax (10) := xmax (12) + 1;
   loop
      begin
         i := i - 14;
         ymax (9) := i - 14;
         ymax (10) := i;
         xmin (10) := 8 * ((xmax (10) - i + 14) / 8);
         xmax (9) := xmin (10);
      end;
      exit when (xmax (9) - xmin (9) >= ymax (9) - ymin (9));
   end loop;
   --  compute layout window corner coordinates from its erase coordinates
   --  xmax[1] is never used
   --  position of text "F1 : LAYOUT", in text coordinates
   --  compute plot window corner coordinates from its erase coordinates
   --  compute Smith window corner coordinates from its erase coordinates
   --  note: these aren't used anywhere, and may be meaningless/wrong
   --  center of Smith chart
   --  radius of Smith chart
   ymin (11) := i;
   ymax (11) := 14 * ((ymax (12) + 1) / 14);
   xmin (11) := xmin (9);
   xmax (11) := xmax (12) + 1;
   xmin (1) := xmin (9) + 11;
   ymin (1) := ymin (9) + 12;
   xmax (1) := xmax (9) - 10;
   ymax (1) := ymax (9) - 12;
   layout_position (1) := xmin (9) / 8 + 8;
   layout_position (2) := 1;
   xmin (8) := xmin (11) + 50;
   ymin (8) := ymin (11) + 7;
   xmax (8) := xmax (11) - 32;
   ymax (8) := ymax (11) - 16;
   xmin (7) := xmin (10) + 12;
   ymin (7) := ymin (10) + 1;
   xmax (7) := xmax (10) - 4;
   ymax (7) := ymax (10) - 51;
   centerx := (xmax (10) + xmin (10)) / 2 - 1;
   centery := (ymax (10) + ymin (10) - 10) / 2;
   rad := (xmax (10) - xmin (10)) / 2 - 4;
   if (Large_Smith)
   then
      xmin (10) := xmin (11);
      xmax (10) := xmax (12);
      ymin (10) := 0;
      ymax (10) := ymax (12);
      centerx := (xmin (10) + xmax (10)) / 2;
      centery := (ymin (10) + ymax (10) - 18) / 2;
      rad := (xmax (10) - xmin (10)) / 2 - 7;
      if (rad > centery - 13)
      then
         rad := centery - 13;
      end if;
   end if;
   --  * Specify (x,y) text positions for text in the x-y plots *
   --  *  x_y_plot_text
   --  [1,x] : dBmax
   --  [2,x] : |S|
   --  [3,x] : dBmin
   --  [4,x] : fmin
   --  [5,x] : f _Hz
   --  [6,x] : fmax
   --  *
   --  end of this field
   Max_Text_Y := ymax (12) / 14;
   Max_Text_X := xmax (12) / 8;
   x_y_plot_text (1) (1) := xmin (8) / 8 + 1;
   x_y_plot_text (2) (1) := xmin (8) / 8 - 3;
   x_y_plot_text (3) (1) := xmin (8) / 8 + 1;
   x_y_plot_text (4) (1) := xmin (8) / 8 + 1;
   x_y_plot_text (5) (1) := (xmin (8) + xmax (8)) / 16 - 1;
   x_y_plot_text (6) (1) := xmax (8) / 8 + 1;
   x_y_plot_text (1) (2) := ymin (8) / 14 + 2;
   x_y_plot_text (2) (2) := (ymin (8) + ymax (8)) / 28 + 0;
   x_y_plot_text (3) (2) := ymax (8) / 14 + 1;
   x_y_plot_text (4) (2) := ymax (8) / 14 + 2;
   x_y_plot_text (5) (2) := ymax (8) / 14 + 2;
   x_y_plot_text (6) (2) := ymax (8) / 14 + 2;
   filename_position (1) := xmin (9) / 8 + 3;
   filename_position (2) := 1 + ymax (9) / 14;
   filename_position (3) := xmax (9) / 8;
   checking_position (1) := xmin (9) / 8 + 4;
   checking_position (2) := ymax (9) / 28;
   layout_position (1) := xmin (9) / 8 + 8;
   layout_position (2) := 1;
   i := (xmax (1) - xmin (1) - (3 + ymax (1) - ymin (1)));
   if (i > 0)
   then
      --  if there's a lot of horizontal room, more or less center the layout window
      i := i / 16;
      xmin (1) := xmin (1) + 8 * i;
      layout_position (1) := layout_position (1) + i;
   end if;
   Make_Titles;
end Screen_Plan;
--  * Screen_Plan *
--  *
--  Initialize VGA mode.
--  Called by Read_Net();
--  *

procedure Screen_Init is
   --  Screen_Init
   Gr_Error_Code : integer;
begin
   Screen_Plan;
   clear_window_gfx (xmin (12), ymin (12), xmax (12), ymax (12));
   Init_Puff_Parameters;
   InitGraph (GraphDriver, GraphMode, "");
   Gr_Error_Code := GraphResult;
   if Gr_Error_Code /= grOk
   then
      TextMode (OrigMode);
      Put ("Graphics error: ");
      Put (GraphErrorMsg (Gr_Error_Code));
      New_Line;
      raise Program_halted (4);
   end if;
   --  error message
   --  Write text characters thru BIOS
   --  DirectVideo is a CRT unit var
   --  needed to mix text with graphics
   --  * MESSAGE Box *
   --  * PLOT Box *
   DirectVideo := false;
   Set_Up_Char;
   message_color := lightred;
   Make_Text_Border (xmin (6) - 1, ymin (6) - 1, xmax (6) + 1, ymax (6) + 1, LightRed, true);
   Make_Text_Border (xmin (2) - 1, ymin (2) - 1, xmax (2) + 1, ymax (2) + 1, Green, true);
   bad_compt := false;
   write_compt (col_window (2), window_f (2));
   key := F3;
   compt3 := part_start;
   cx3 := compt3.all.x_block;
end Screen_Init;
--  * Screen_Init *
end pfst;

--  <<END OF DOCUMENT>>  --
