with Ada.Command_Line;        use Ada.Command_Line;
with Ada.Strings;             use Ada.Strings;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Ada.Text_IO;             use Ada.Text_IO;
with Interfaces;              use Interfaces;
with Interfaces.C;            use Interfaces.C;

with xgraph;                  use xgraph;
with pfun1;                   use pfun1;
with pfun2;                   use pfun2;
with Utils;                   use Utils;

package body pfst is

   --  *
   --  Set up linked list of components for all
   --  parameters in the Plot window and
   --  Board Window.
   --  Called by Puff_Start.
   --  *
   procedure Make_Coord_and_Parts_ListO is
      --  set-up coordinates linked list
      tcompt : compt;
   begin
      for i in 1 .. 10
      loop
         if i = 1
         then
            coord_start := new compt_record;
            tcompt := coord_start;
         else
            tcompt.next_compt := new compt_record;
            tcompt.next_compt.prev_compt := tcompt;
            tcompt := tcompt.next_compt;
         end if;
      end loop;
      --  set-up parts linked list
      tcompt.next_compt := coord_start;
      coord_start.prev_compt := tcompt;
      for i in 1 .. 18
      loop
         if i = 1
         then
            part_start := new compt_record;
            tcompt := part_start;
            tcompt.prev_compt := null;
            tcompt.yp := ymin (3);
         else
            tcompt.next_compt := new compt_record;
            tcompt.next_compt.prev_compt := tcompt;
            tcompt := tcompt.next_compt;
            tcompt.yp := tcompt.prev_compt.yp + 1;
         end if;
         declare P2Ada_Var_1 : compt_record renames tcompt.all;
         begin
            --  initialize for device and indef
            P2Ada_Var_1.descript := To_Unbounded_String
              ("" & Character'Val (LC_A_ORD + i - 1) & " ");
            P2Ada_Var_1.changed := False;
            P2Ada_Var_1.right := False;
            P2Ada_Var_1.parsed := False;
            P2Ada_Var_1.sweep_compt := False;
            P2Ada_Var_1.f_file := null;
            P2Ada_Var_1.s_file := null;
            P2Ada_Var_1.s_ifile := null;
            P2Ada_Var_1.used := 0;
            P2Ada_Var_1.x_block := 2;
            if i <= 9
            then
               P2Ada_Var_1.xp := xmin (3);
               P2Ada_Var_1.xmaxl := xmax (3) - xmin (3);
            else
               P2Ada_Var_1.xp := xmin (5);
               P2Ada_Var_1.xmaxl := xmax (5) - xmin (5);
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
            board_start := new compt_record;
            tcompt := board_start;
            tcompt.prev_compt := null;
         else
            tcompt.next_compt := new compt_record;
            tcompt.next_compt.prev_compt := tcompt;
            tcompt := tcompt.next_compt;
         end if;
      end loop;
      --  i
      tcompt.next_compt := null;
   end Make_Coord_and_Parts_ListO;

   --  Set up titles for the three windows.
   --  *
   procedure Make_Titles is
   begin
      for i in 1 .. 4
      loop
         --  was to 3
         window_f (i) := new compt_record;
         command_f (i) := new compt_record;
         case i is when 1 =>
            declare P2Ada_Var_2 : compt renames window_f (1);
            begin
               --  xp was 2
               P2Ada_Var_2.xp := layout_position (1);
               P2Ada_Var_2.yp := layout_position (2);
               P2Ada_Var_2.descript := To_Unbounded_String (" F1 : LAYOUT ");
            end;
            declare P2Ada_Var_3 : compt renames command_f (1);
            begin
               --  place header above board window
               P2Ada_Var_3.descript := To_Unbounded_String (" LAYOUT HELP ");
               P2Ada_Var_3.xp := 1 + (xmax (4) + xmin (4) -
                                      Length (P2Ada_Var_3.descript)) / 2;
               P2Ada_Var_3.yp := ymin (4) - 1;
            end;
         when 2 =>
            declare P2Ada_Var_4 : compt renames window_f (2);
            begin
               --  xp was 41
               P2Ada_Var_4.descript := To_Unbounded_String (" F2 : PLOT ");
               P2Ada_Var_4.xp := 1 + (xmax (2) + xmin (2) -
                                      Length (P2Ada_Var_4.descript)) / 2;
               P2Ada_Var_4.yp := ymin (2) - 1;
            end;
            declare P2Ada_Var_5 : compt renames command_f (2);
            begin
               P2Ada_Var_5.descript := To_Unbounded_String (" PLOT HELP ");
               P2Ada_Var_5.xp := 1 + (xmax (4) + xmin (4) -
                                      Length (P2Ada_Var_5.descript)) / 2;
               P2Ada_Var_5.yp := ymin (4) - 1;
            end;
         when 3 =>
            declare P2Ada_Var_6 : compt renames window_f (3);
            begin
               --  xp was 2
               P2Ada_Var_6.descript := To_Unbounded_String (" F3 : PARTS ");
               P2Ada_Var_6.xp := 1 + (xmax (3) + xmin (3) -
                                      Length (P2Ada_Var_6.descript)) / 2;
               P2Ada_Var_6.yp := ymin (3) - 1;
            end;
            declare P2Ada_Var_7 : compt renames command_f (3);
            begin
               --  place header above board window
               P2Ada_Var_7.descript := To_Unbounded_String (" PARTS HELP ");
               P2Ada_Var_7.xp := 1 + (xmax (4) + xmin (4) -
                                      Length (P2Ada_Var_7.descript)) / 2;
               P2Ada_Var_7.yp := ymin (4) - 1;
            end;
         when 4 =>
            declare P2Ada_Var_8 : compt renames window_f (4);
            begin
               --  xp was 2
               P2Ada_Var_8.descript := To_Unbounded_String (" F4 : BOARD ");
               P2Ada_Var_8.xp := 1 + (xmax (4) + xmin (4) -
                                      Length (P2Ada_Var_8.descript)) / 2;
               P2Ada_Var_8.yp := ymin (4) - 1;
            end;
            declare P2Ada_Var_9 : compt renames command_f (4);
            begin
               P2Ada_Var_9.descript := To_Unbounded_String (" BOARD HELP ");
               P2Ada_Var_9.xp := 1 + (xmax (3) + xmin (3) -
                                      Length (P2Ada_Var_9.descript)) / 2;
               P2Ada_Var_9.yp := ymin (3) - 1;
            end;
         end case;
      end loop;
   end Make_Titles;

--  Initializes linked lists and windows.
   --  Set up to be called only after the board
   --  parameters have been read, but before the key!
   --  Set_Up_Board must be called sometime after
   --  this procedure when reading new graphics.
   --  *
   procedure Init_Puff_Parameters is
      --  * Initialize s-parameter linked lists *
      cspc : spline_param;
   begin
      for xpt in 0 .. ptmax
      loop
         if xpt = 0
         then
            spline_start := new spline_record;
            cspc := spline_start;
         else
            cspc.next_c := new spline_record;
            cspc.next_c.prev_c := cspc;
            cspc := cspc.all.next_c;
         end if;
         for ij in 1 .. max_params
         loop
            if xpt = 0
            then
               plot_start (ij) := new plot_record;
               c_plot (ij) := plot_start (ij);
            else
               c_plot (ij).next_p := new plot_record;
               c_plot (ij).next_p.prev_p := c_plot (ij);
               c_plot (ij) := c_plot (ij).next_p;
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
         s_key (i) := To_Unbounded_String (" ");
      end loop;
      for i in 7 .. 10
      loop
         s_key (i) := To_Unbounded_String ("");
      end loop;
      --  pfun2
      --  begin with impedance Smith chart
      Set_Up_KeyO;
      admit_chart := False;
   end Init_Puff_Parameters;

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
         --   ResizeWindow (Integer_32 (80 * 8), Integer_32 (32 * 15));
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
      if not fileexists (puff_file /= "setup.puf", net_file, puff_file)
      then
         Erase_Message;
         message (2) := To_Unbounded_String ("setup.puf");
         message (3) := To_Unbounded_String ("not found");
         shutdown;
      end if;
   end Puff_Start;

   procedure Screen_Init is
      Gr_Error_Code : Integer_32;
      --  msg : String;
   begin
      Screen_Plan;
      Clear_Window_gfx (xmin (12), ymin (12), xmax (12), ymax (12));
      Init_Puff_Parameters;
      InitGraph (GraphDriver, GraphMode, "");
      Gr_Error_Code := GraphResult;
      if Gr_Error_Code /= GROK
      then
         TextMode (Integer_32 (OrigMode));
         Put ("Graphics error: ");
         --  msg := To_String (Gr_Error_Code);
         --  Put (msg);
         New_Line;
         raise Program_halted with "Within Screen_Init";
      end if;
      --  error message
      --  Write text characters thru BIOS
      --  DirectVideo is a CRT unit var
      --  needed to mix text with graphics
      --  * MESSAGE Box *
      --  * PLOT Box *
      DirectVideo := False;
      message_color := LightRed;
      Make_Text_Border (xmin (6) - 1, ymin (6) - 1, xmax (6) + 1, ymax (6) + 1,
                        LightRed, True);
      Make_Text_Border (xmin (2) - 1, ymin (2) - 1, xmax (2) + 1, ymax (2) + 1,
                        Green, True);
      bad_compt := False;
      write_compt (col_window (2), window_f (2));
      key := F3;
      compt3 := part_start;
      cx3 := compt3.all.x_block;
   end Screen_Init;

   procedure Screen_Plan is
   begin
      null;
   end Screen_Plan;

end pfst;
