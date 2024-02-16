with Ada.Text_IO;       use Ada.Text_IO;
with Ada.Command_Line;  use Ada.Command_Line;
with Ada.Strings;       use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

with Interfaces;        use Interfaces;

with xgraph;            use xgraph;

package body pfst is

   demo_mode : Boolean;

   function CommandLine return String is
      --  returns first command line parameter
      Result_CommandLine : String := "";
      Buffer             : String := "";
      i                  : Integer;
   begin
      Buffer := Argument (1);
      if Fixed.Count (Buffer, "-D") > 0
      then
         Buffer := "";
         demo_mode := True;
      else
         demo_mode := False;
      end if;
      if Fixed.Count (Buffer, "-BW") > 0
      then
         Buffer := Argument (2);
         blackwhite := True;
         for i in 1 .. 4
         loop
            col_window (i) := white;
            s_color (i) := white;
         end loop;
      else
         blackwhite := False;
      end if;
      if Buffer = ""
      then
         Result_CommandLine := "setup";
      else
         Result_CommandLine := Buffer;
      end if;
      return Result_CommandLine;
   end CommandLine;

   procedure Puff_Start is
      memK : Integer_32;
      i    : Integer_32;
      Ch   : Character;

      --  Put cursor near bottom of screen
      --  on Linux we don't blank the cursor
      procedure Blank_Blue_Cursor is
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
      --  SetTextBuf (dev_file, big_text_buf);
      --  SetTextBuf (net_file, big_text_buf);
      --  OrigMode := Lastmode;
      TextMode (co80);
      --  puff_file := Commandline;
      memK := 2000 * 1024;
      if puff_file = "setup"
      then
         --  * if setup.puf to be loaded then display information *
         --  make border does a ClrScr to black
         --  *** Make_Text_Border doesn't work here ****
         TextBackground (Blue);
         ClrScr;
         TextCol (Yellow);
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
         GotoXY (31, 7);
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
         message_color := White;
         Blank_Blue_Cursor;
         loop
            Ch := ReadKey;
            exit when Ch /= screenresize;
         end loop;
         if Ch = Character'Val (0)
         then
            Ch := Character (Character'Pos (ReadKey) + 128);
         end if;
         if Ch = ASCII.ESC
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
      -- Close (net_file);
   end Puff_Start;

   procedure Screen_Init is
   begin
      null;
   end Screen_Init;

   procedure Screen_Plan is
   begin
      null;
   end Screen_Plan;

end pfst;
