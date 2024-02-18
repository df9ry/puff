with Ada.Command_Line;        use Ada.Command_Line;
with Ada.Strings;             use Ada.Strings;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Interfaces;              use Interfaces;
with Interfaces.C;            use Interfaces.C;

with xgraph;                  use xgraph;
with pfun1;                   use pfun1;
with utils;                   use utils;

with Ada.Text_IO;

package body pfst is

   function CommandLine return String is
      Buffer : Unbounded_String := To_Unbounded_String ("setup");
   begin
      demo_mode := False;
      blackwhite := False;
      for iarg in 1 .. Argument_Count loop
         if Argument (iarg) = "-D"
         then
            demo_mode := True;
            goto Continue;
         end if;
         if Argument (iarg) = "-BW"
         then
            blackwhite := True;
            for i in 1 .. 4
            loop
               col_window (i) := White;
               s_color (i) := White;
            end loop;
            goto Continue;
         end if;
         Buffer := To_Unbounded_String (Argument (iarg));
         <<Continue>>
      end loop;
      return To_String (Buffer);
   end CommandLine;

   --  Preparation required before reading board
   procedure Prep_to_Read_Board is
   begin
      for i in 1 .. 8
      loop
         --  initialize board values
         --  initialize board unit prefixes
         s_board (i, 1) := To_Unbounded_String (" ");
         s_board (i, 2) := To_Unbounded_String (" ");
      end loop;
      board_read := False;
      for i in 1 .. 12
      loop
         board (i) := False;
      end loop;
   end Prep_to_Read_Board;

   procedure Puff_Start is
      Ch : char;

      --  Put cursor near bottom of screen
      --  on Linux we don't blank the cursor
      procedure Blank_Blue_Cursor is
      begin
         GotoXY (64, 22);
      end Blank_Blue_Cursor;

   --  *****************************************************
   --  * Puff_Start *
   begin
      OrigMode := Integer (LastMode);
      TextMode (co80);
      puff_file := To_Unbounded_String (CommandLine);
      if puff_file = "setup"
      then
         TextBackground (Blue);
         ClrScr;
         TextCol (Yellow);
         --  * Draw Border for startup screen *
         GotoXY (4, 2);
         PutCh (char'Val (201));
         GotoXY (4, 24);
         PutCh (char'Val (200));
         for i in 3 .. 23
         loop
            GotoXY (4, Integer_32 (i));
            PutCh (char'Val (186));
            GotoXY (79, Integer_32 (i));
            PutCh (char'Val (186));
         end loop;
         for i in 5 .. 78
         loop
            GotoXY (Integer_32 (i), 2);
            PutCh (char'Val (205));
            GotoXY (Integer_32 (i), 24);
            PutCh (char'Val (205));
         end loop;
         GotoXY (79, 2);
         PutCh (char'Val (187));
         GotoXY (79, 24);
         PutCh (char'Val (188));
         GotoXY (17, 24);
         PutStr (To_C (" Press Esc to abort, any other key to run Puff "));
         TextCol (White);
         GotoXY (5, 4);
         PutStr (To_C ("PUFF, Version 20240216"));
         GotoXY (5, 5);
         PutStr (To_C ("First release by:"));
         GotoXY (8, 6);
         PutStr (To_C ("Scott W. Wedge, Richard Compton, David Rutledge"));
         GotoXY (8, 7);
         PutStr (To_C ("Andreas Gerstlauer"));
         GotoXY (5, 9);
         PutStr (To_C ("2010 - Code released under the GNU General Public " &
                "License version 3"));
         GotoXY (5, 11);
         PutStr (To_C ("Linux version by:"));
         GotoXY (8, 12);
         PutStr (To_C ("Pieter-Tjerk de Boer"));
         GotoXY (8, 13);
         PutStr (To_C ("Leland C. Scott"));
         GotoXY (5, 15);
         PutStr (To_C ("ADA version by:"));
         GotoXY (8, 16);
         PutStr (To_C ("Tania Hagn (DF9RY)"));
         GotoXY (2, 27);
         PutStr (To_C ("For more information, see:"));
         GotoXY (2, 29);
         PutStr (To_C ("http://www.its.caltech.edu/~mmic/puff.html -> " &
                "for the original version"));
         GotoXY (2, 30);
         PutStr (To_C ("http://wwwhome.cs.utwente.nl/~ptdeboer/ham/puff.html" &
                " -> for the Linux version"));
         GotoXY (2, 31);
         PutStr (To_C ("https://df9ry.de -> for the ADA version"));
         TextCol (White);
         message_color := White;
         Blank_Blue_Cursor;
         loop
            Ch := char'Val (ReadKey);
            exit when Ch /= screenresize;
         end loop;
         if Ch = null_char
         then
            Ch := char'Val (ReadKey + 128);
         end if;
         if Ch = Esc
         then
            --  Has the Esc key been pressed?
            --  Abort and restore textmode if Esc pressed
            TextMode (Integer_32 (OrigMode));
            ClrScr;
            raise Program_halted with "ESC pressed";
         end if;
      end if;
      TextBackground (Black);
      ClrScr;
      Max_Text_Y := 25;
      Max_Text_X := 80;
      message_color := LightRed;
      xmin (6) := 32;
      ymin (6) := 11;
      xmax (6) := 49;
      ymax (6) := 13;
      insert_key := True;
      Large_Smith := False;
      Large_Parts := False;
      Extra_Parts_Used := False;
      co (co1, 1.0, 0.0);
      key_end := 0;
      --  * Init_Puff_Parameters; *
      Prep_to_Read_Board;
      read_kbd := True;
      if Tail (puff_file, 4) /= ".puf"
      then
         puff_file := puff_file & ".puf";
      end if;
      if not File_Exists (puff_file /= "setup.puf", puff_file)
      then
         if not (Setup_Exists (puff_file))
         then
            Erase_Message;
            message (2) := To_Unbounded_String ("setup.puf");
            message (3) := To_Unbounded_String ("not found");
            shutdown;
         end if;
      end if;
      Ada.Text_IO.Close (net_file);
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
