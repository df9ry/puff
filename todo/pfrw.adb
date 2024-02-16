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

package pfrw is

   procedure Read_Board (read_graphics : boolean);

   procedure read_keyO;

   procedure read_partsO;

   procedure read_circuitO;

   procedure Read_S_Params;

   procedure save_boardO;

   procedure save_keyO;

   procedure save_partsO;

   procedure save_circuitO;

   procedure save_s_paramsO;

   procedure bad_board;

   procedure read_setup (fname2 : in out file_string);
end pfrw;
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

package body pfrw is
   --  [P2Ada]: This is for 'Write([Boolean])'

   package Boolean_Text_IO is new Enumeration_IO (Boolean);
   use Boolean_Text_IO;
   --  [P2Ada]: This is for 'file' without type
   --  [P2Ada]: This is for the Halt pseudo-procedure

   package Byte_Direct_IO is new Ada.Direct_IO (Unsigned_8);
   Program_halted : exception;
   --  *
   --  Read board parameters from .puf file.
   --  *

   procedure Read_Board (read_graphics : boolean) is
      --  * Artwork Reduction Ratios in mm/dot *
      --  25.4 mm/in  /(120 dots) in x dirn for matrix artwork
      --  25.4 mm/in  /(144 dots) in y dirn for matrix artwork
      --  LaserJet reduction ratio = 25.4 mm/in * 1/150dpi
      --  unit-prefix string
      --  *****************************************************
      red_psx : constant := 0.2117;
      red_psy : constant := 0.1764;
      red_lasr : constant := 0.169333;
      i : integer;
      value : Long_Float;
      unit_prf : string (1 .. 80);
      char1, char2, char3, prefix : Character;
      --  *
      --  Look for prefixes when reading board parameters.
      --  If no prefix and no unit then return 'x' to
      --  designate that default prefixes are to be used.
      --  *

      function file_prefix (id_string : string) return Character is
         Result_file_prefix : Character;
         pot_prefix : Character;
      begin
         while id_string (1) = ' '
         loop
            Delete (id_string, 1, 1);
         end loop;
         --  delete leading blanks
         --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
         if id_string (1) and Eng_Dec_Mux
         then
            --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
            pot_prefix := id_string (1);
            if (id_string (1) = 'm') and then (not (id_string (2) and ('H' | 'O' | 'm' => True, others => False)))
            then
               pot_prefix := ' ';
            end if;
            --  if its just meters, then no prefix
         else
            if id_string (1) = 'U'
            then
               --  U = micro
               --  convert U to Mu
               --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
               pot_prefix := Mu;
            else
               if (id_string (1) and ('H' | 'O' => True, others => False))
               then
                  --  Hz or Ohms?
                  --  if units but no prefix
                  --  if no prefix or units return 'x'
                  pot_prefix := ' ';
               else
                  pot_prefix := 'x';
               end if;
            end if;
         end if;
         Result_file_prefix := pot_prefix;
         return Result_file_prefix;
      end file_prefix;
      --  *******************************************************
      --  *
      --  *

      procedure Attach_Prefix (b_num : integer; def_prefix : Character; board_param : in out Long_Float; zero_OK : boolean) is
         --  [P2Ada]: !Help! Maybe (file,...) here
         --  [P2Ada]: !Help! Maybe (file,...) here
         --  [P2Ada]: !Help! Maybe (file)
      begin
         Get (net_file);
         Get (value);
         Get (unit_prf);
         Skip_Line;
         prefix := file_prefix (unit_prf);
         if def_prefix = 'G'
         then
            --  Do not attach prefix for xHz
            board_param := value;
            if prefix = 'x'
            then
               --  use default prefix
               s_board (b_num, 2) := 'G';
               freq_prefix := 'G';
            else
               s_board (b_num, 2) := prefix;
               freq_prefix := prefix;
            end if;
         else
            if prefix = 'x'
            then
               --  use default prefix
               --  if prefix is given
               --  return value in default units
               board_param := value;
               s_board (b_num, 2) := def_prefix;
            else
               board_param := value * Eng_Prefix (prefix) / Eng_Prefix (def_prefix);
               s_board (b_num, 2) := prefix;
            end if;
         end if;
         if (board_param > 0.0) or else ((board_param = 0.0) and then zero_OK)
         then
            board (b_num) := true;
            Put (value, 7, 3, 0, s_board (b_num, 1));
            if ((value < 1.0e - 3) or else (value > 1.0e + 3)) and then not (value = 0.0)
            then
               Put (value, 8, s_board (b_num, 1));
            end if;
            --  Write in exponential notation for small/large numbers
         end if;
      end Attach_Prefix;
      --  ****************************************************
      --  * Read_Board *
      --  * Default parameters for old .puf files without new parameters *
   begin
      for i in 9 .. 12
      loop
         board (i) := true;
      end loop;
      --  Make these parameters optional
      --  initialize new parameters for old puff files
      Art_Form := 0;
      Laser_Art := False;
      metal_thickness := 0.0;
      s_board (9, 1) := "  0.000";
      s_board (9, 2) := 'm';
      surface_roughness := 0.0;
      s_board (10, 1) := "  0.000";
      s_board (10, 2) := Mu;
      loss_tangent := 0.0;
      s_board (11, 1) := "  0.000";
      s_board (11, 2) := ' ';
      conductivity := 5.80e + 7;
      s_board (12, 1) := "  5.8E+7";
      s_board (12, 2) := ' ';
      loop
         if SeekEoln (net_file)
         then
            --  ignore blank lines
            --  [P2Ada]: !Help! Maybe (file)
            --  advance to beginning of next line
            Get (net_file);
            Skip_Line;
            char1 := ' ';
         else
            loop
               --  [P2Ada]: !Help! Maybe (file,...) here
               Get (net_file);
               Get (char1);
               exit when char1 /= ' ';
            end loop;
            if char1 /= '\'
            then
               --  [P2Ada]: !Help! Maybe (file,...) here
               Get (net_file);
               Get (char2);
               if (char2 /= ' ')
               then
                  loop
                     --  [P2Ada]: !Help! Maybe (file,...) here
                     Get (net_file);
                     Get (char3);
                     exit when char3 = ' ';
                  end loop;
               end if;
               case char1 is
                  when 'z' | 'Z' =>
                     Attach_Prefix (1, ' ', z0, false);
                  when 'f' | 'F' =>
                     --  prefix not attached here
                     Attach_Prefix (2, 'G', design_freq, false);
                  when 'e' | 'E' =>
                     --  [P2Ada]: !Help! Maybe (file,...) here
                     --  [P2Ada]: !Help! Maybe (file)
                     --  no units here
                     Get (net_file);
                     Get (er);
                     Skip_Line;
                     if er > 0
                     then
                        --  no units
                        board (3) := true;
                        Put (er, 7, 3, 0, s_board (3, 1));
                        s_board (3, 2) := ' ';
                     end if;
                  when 'h' | 'H' =>
                     Attach_Prefix (4, 'm', substrate_h, false);
                  when 's' | 'S' =>
                     --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
                     if (char2 and ('r' | 'R' => True, others => False))
                     then
                        --  init-optional value
                        Board (10) := false;
                        Attach_Prefix (10, Mu, surface_roughness, true);
                     else
                        Attach_Prefix (5, 'm', bmax, false);
                     end if;
                  when 'c' | 'C' =>
                     --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
                     if (char2 and ('d' | 'D' => True, others => False))
                     then
                        --  init-optional
                        --  [P2Ada]: !Help! Maybe (file,...) here
                        --  [P2Ada]: !Help! Maybe (file)
                        --  no units here
                        board (12) := false;
                        Get (net_file);
                        Get (conductivity);
                        Skip_Line;
                        if (conductivity > 0)
                        then
                           --  no units
                           board (12) := true;
                           Put (conductivity, 8, s_board (12, 1));
                           s_board (12, 2) := ' ';
                        end if;
                     else
                        Attach_Prefix (6, 'm', con_sep, true);
                     end if;
                  when 'r' | 'R' =>
                     Attach_Prefix (7, 'm', resln, false);
                     sresln := s_board (7, 1) + s_board (7, 2) + 'm';
                  when 'a' | 'A' =>
                     Attach_Prefix (8, 'm', artwork_cor, true);
                  when 'm' | 'M' =>
                     --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
                     if (char2 and ('t' | 'T' => True, others => False))
                     then
                        --  init-optional value
                        --  [P2Ada]: !Help! Maybe (file,...) here
                        --  [P2Ada]: !Help! Maybe (file)
                        Board (9) := false;
                        Attach_Prefix (9, 'm', metal_thickness, true);
                     else
                        Get (net_file);
                        Get (miter_fraction);
                        Skip_Line;
                        if (0 <= miter_fraction) and then (miter_fraction < 1)
                        then
                           board (16) := true;
                        end if;
                     end if;
                  when 'l' | 'L' =>
                     --  Loss Tangent
                     --  init-optional
                     --  [P2Ada]: !Help! Maybe (file,...) here
                     --  [P2Ada]: !Help! Maybe (file)
                     --  no units here
                     Board (11) := false;
                     Get (net_file);
                     Get (loss_tangent);
                     Skip_Line;
                     if loss_tangent >= 0
                     then
                        --  no units
                        board (11) := true;
                        Put (loss_tangent, 8, s_board (11, 1));
                        s_board (11, 2) := ' ';
                     end if;
                  when 'p' | 'P' =>
                     --  [P2Ada]: !Help! Maybe (file,...) here
                     --  [P2Ada]: !Help! Maybe (file)
                     Get (net_file);
                     Get (reduction);
                     Skip_Line;
                     if reduction > 0
                     then
                        if Laser_Art
                        then
                           --  setup 150 DPI
                           --  setup 144 x 120 DPI
                           psx := red_lasr / reduction;
                           psy := red_lasr / reduction;
                           board (13) := true;
                        else
                           psx := red_psx / reduction;
                           psy := red_psy / reduction;
                           board (13) := true;
                        end if;
                     end if;
                     --  if reduction and/or Laser_art
                  when 'd' | 'D' =>
                     if read_graphics
                     then
                        --  [P2Ada]: !Help! Maybe (file,...) here
                        --  [P2Ada]: !Help! Maybe (file)
                        --  ignore the VGA/EGA setting on Linux
                        --  [P2Ada]: !Help! Maybe (file,...) here
                        --  [P2Ada]: !Help! Maybe (file)
                        Get (net_file);
                        Get (value);
                        Skip_Line;
                        display := Round (value);
                        board (14) := true;
                        imin := 1;
                     else
                        Get (net_file);
                        Get (value);
                        Skip_Line;
                        board (14) := true;
                     end if;
                  when 'o' | 'O' =>
                     --  * New entry for artwork output *
                     --  [P2Ada]: !Help! Maybe (file,...) here
                     --  [P2Ada]: !Help! Maybe (file)
                     Get (net_file);
                     Get (value);
                     Skip_Line;
                     Art_Form := Round (value);
                     if Art_Form = 1
                     then
                        Laser_Art := True;
                     end if;
                  when 't' | 'T' =>
                     --  [P2Ada]: !Help! Maybe (file,...) here
                     --  [P2Ada]: !Help! Maybe (file)
                     Get (net_file);
                     Get (value);
                     Skip_Line;
                     if (Round (value) = 2)
                     then
                        --  makes calculations easier
                        Manhattan_Board := true;
                        stripline := true;
                     else
                        Manhattan_Board := false;
                        stripline := Round (value) /= 0;
                     end if;
                     --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
                     if (Round (value) and (0 .. 2 => True, others => False))
                     then
                        board (15) := true;
                     end if;
                  when others =>
                     begin
                        message (2) := "Unknown board";
                        message (3) := "parameter in .puf";
                        shutdown;
                     end;
               end case;
               --  case char1
            end if;
            --  if char1
         end if;
         --  if SeekEoln
         exit when (char1 = '\') or else End_of_File (net_file);
      end loop;
      board_read := board (1);
      for i in 2 .. 12
      loop
         board_read := board_read and then board (i);
      end loop;
      if board_read and then not (read_graphics)
      then
         Fresh_Dimensions;
      end if;
   end Read_Board;
   --  * Read_Board *
   --  *
   --  Read key from .puf file.
   --  *

   procedure Read_KeyO is
      len, j, i : integer;
      des : line_string;
      c1, c2, c3, char1 : Character;
   begin
      for i in 1 .. 6
      loop
         s_key (i) := ' ';
      end loop;
      for i in 7 .. 10
      loop
         s_key (i) := "";
      end loop;
      loop
         if End_of_Line (net_file)
         then
            --  ignore blank lines
            --  [P2Ada]: !Help! Maybe (file)
            --  [P2Ada]: !Help! Maybe (file,...) here
            Get (net_file);
            Skip_Line;
            char1 := ' ';
         else
            Get (net_file);
            Get (char1);
            des := "";
            if char1 /= '\'
            then
               des := char1;
               loop
                  --  [P2Ada]: !Help! Maybe (file,...) here
                  Get (net_file);
                  Get (char1);
                  des := des + char1;
                  exit when (char1 = lbrack) or else End_of_Line (net_file);
               end loop;
               --  [P2Ada]: !Help! Maybe (file)
               --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
               Get (net_file);
               Skip_Line;
               c1 := des (1);
               c2 := des (2);
               c3 := des (3);
               while not (des (1) and ('+' | '-' | '.' | ',' | '0' .. '9' | 'e' | 'E' => True, others => False))
               loop
                  Delete (des, 1, 1);
               end loop;
               --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
               len := (des'length);
               while not (des (len) and ('+' | '-' | '.' | ',' | '0' .. '9' | 'e' | 'E' => True, others => False)) and then (len > 0)
               loop
                  Delete (des, len, 1);
                  len := (des'length);
               end loop;
               case c1 is
                  when 'd' | 'D' =>
                     --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
                     if c2 and ('u' | 'U' => True, others => False)
                     then
                        s_key (1) := des;
                     else
                        s_key (2) := des;
                     end if;
                  when 'f' | 'F' =>
                     case c2 is
                        when 'l' | 'L' =>
                           s_key (3) := des;
                        when 'u' | 'U' =>
                           s_key (4) := des;
                        when 'd' | 'D' =>
                           if c3 = '/'
                           then
                              s_key (5) := des;
                           end if;
                        when others =>
                           --  [P2Ada]: no otherwise / else in Pascal
                           null;
                     end case;
                     --  case
                  when 'p' | 'P' =>
                     --  pts=number of points
                     s_key (5) := des;
                  when 's' | 'S' =>
                     --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
                     if c2 and ('r' | 'R' => True, others => False)
                     then
                        s_key (6) := des;
                     else
                        j := 6;
                        loop
                           j := j + 1;
                           exit when ((s_key (j) 'l' ength) = 0) or else (j > 9);
                        end loop;
                        s_key (j) := des;
                     end if;
                  when others =>
                     --  [P2Ada]: no otherwise / else in Pascal
                     null;
               end case;
               --  case
            end if;
            --  if char1 ..
         end if;
         --  if Eoln
         exit when (char1 = '\') or else End_of_File (net_file);
      end loop;
   end Read_KeyO;
   --  * Read_Key *
   --  *
   --  Read parts from .puf file.
   --  Called by Read_Net() in pfmain1a.pas.
   --  Upon a call to read_partsO the read index has already
   --  advanced to the point where a '\p' has been read.
   --  *

   procedure read_partsO is
      --  * Clear previous parts list *
      char1 : Character;
      i, j : integer;
      des : line_string;
      tcompt : compt;
   begin
      Large_Parts := False;
      for i in 1 .. 18
      loop
         if i = 1
         then
            tcompt := part_start;
         else
            tcompt := tcompt.all.next_compt;
         end if;
         --  [P2Ada]: WITH instruction
         --  [P2Ada]: !Help! No type found -> add 'P2Ada_Var_1.' to fields
         declare P2Ada_Var_1 : < > renames tcompt.all;
         begin
            descript := Character (Character'Pos ('a') + i - 1) + ' ';
            used := 0;
            changed := false;
            parsed := false;
            f_file := null;
            s_file := null;
            s_ifile := null;
         end;
         --  [P2Ada]: end of WITH
         --  with
      end loop;
      --  for i:=1 to 18
      j := 0;
      loop
         if End_of_Line (net_file)
         then
            --  if at end_of_line..
            --  [P2Ada]: !Help! Maybe (file)
            --  do carriage return
            --  initialize char1
            --  [P2Ada]: !Help! Maybe (file,...) here
            Get (net_file);
            Skip_Line;
            char1 := ' ';
         else
            Get (net_file);
            Get (char1);
            if char1 /= '\'
            then
               --  dont read first line with '\p'
               --  [P2Ada]: !Help! Maybe (file,...) here
               --  [P2Ada]: !Help! Maybe (file)
               --  read string
               Get (net_file);
               Get (des);
               Skip_Line;
               insert (char1, des, 1);
               j := j + 1;
               if j <= 18
               then
                  i := Pos (lbrack, des);
                  if i > 0
                  then
                     Delete (des, i, (des'length));
                  end if;
                  for i in 1 .. (des'length)
                  loop
                     case des (i) is
                        when 'O' =>
                           des (i) := Omega;
                        when 'D' =>
                           des (i) := Degree;
                        when 'U' =>
                           des (i) := Mu;
                        when '|' =>
                           des (i) := Parallel;
                        when others =>
                           --  [P2Ada]: no otherwise / else in Pascal
                           null;
                     end case;
                  end loop;
                  --  case
                  while des ((des'length)) = ' '
                  loop
                     delete (des, (des'length), 1);
                  end loop;
                  --  delete extra blanks
                  if j = 1
                  then
                     tcompt := part_start;
                  else
                     tcompt := tcompt.all.next_compt;
                  end if;
                  --  [P2Ada]: WITH instruction
                  --  [P2Ada]: !Help! No type found -> add 'P2Ada_Var_2.' to fields
                  declare P2Ada_Var_2 : < > renames tcompt.all;
                  begin
                     if ((des'length) = 0)
                     then
                        --  descript:=descript
                        --  leave part blank
                        changed := true;
                     else
                        descript := descript + des;
                        changed := true;
                        if (j > 9)
                        then
                           Large_Parts := True;
                        end if;
                     end if;
                  end;
                  --  [P2Ada]: end of WITH
                  --  if and with
               end if;
               --  if j <= 18
            end if;
            --  if char1<>'\'
         end if;
         --  else Eoln
         exit when (char1 = '\') or else End_of_File (net_file);
      end loop;
   end read_partsO;
   --  read_partsO
   --  *
   --  Read in circuit from .puf file.
   --  *

   procedure read_circuitO is
      key_i, nn : integer;
      char1 : Character;
   begin
      circuit_changed := true;
      key_end := 0;
      loop
         if not (End_of_File (net_file))
         then
            --  read circuit
            if End_of_Line (net_file)
            then
               --  ignore blank lines
               --  [P2Ada]: !Help! Maybe (file)
               --  [P2Ada]: !Help! Maybe (file,...) here
               Get (net_file);
               Skip_Line;
               char1 := ' ';
            else
               Get (net_file);
               Get (char1);
               if char1 /= '\'
               then
                  --  [P2Ada]: !Help! Maybe (file,...) here
                  --  [P2Ada]: !Help! Maybe (file,...) here
                  --  [P2Ada]: !Help! Maybe (file)
                  Get (net_file);
                  Get (key_i);
                  Get (nn);
                  Skip_Line;
                  key := Character (key_i);
                  update_key_list (nn);
               end if;
               --  if char1
            end if;
         end if;
         --  if Eoln
         exit when (char1 = '\') or else End_of_File (net_file);
      end loop;
      --  set_up for redraw
      key_i := 0;
   end read_circuitO;
   --  read_circuitO
   --  *
   --  Read s-parameters from .puf file.
   --  Uses procedure Read_Number.
   --  *

   procedure Read_S_Params is
      --  ********************************************************
      ij : integer;
      freq, mag, ph : Long_Float;
      char1 : Character;
      --  *
      --  Read s-parameter values from files.
      --  *

      procedure Read_Number (s : in out Long_Float) is
         --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
         potential_numbers : constant Character := ('+' | '-' | '.' | '0' .. '9' | 'e' | 'E' => True, others => False);
         ss : string (1 .. 128);
         code : integer;
         found : boolean;
      begin
         ss := "";
         if char1 and potential_numbers
         then
            --  char1 is the first freq character
            --  Search for first valid numeric character
            --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
            ss := char1;
         else
            found := false;
            if char1 and (lbrack | '#' | '!' => True, others => False)
            then
               --  [P2Ada]: !Help! Maybe (file)
               Get (net_file);
               Skip_Line;
            end if;
            --  skip potential comment lines
            loop
               --  [P2Ada]: !Help! Maybe (file,...) here
               --  read another character
               --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
               Get (net_file);
               Get (char1);
               if char1 and (lbrack | '#' | '!' => True, others => False)
               then
                  --  [P2Ada]: !Help! Maybe (file)
                  Get (net_file);
                  Skip_Line;
               end if;
               --  skip potential comment lines
               --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
               if char1 and potential_numbers
               then
                  ss := char1;
                  found := true;
               end if;
               exit when found or else (char1 = '\');
            end loop;
         end if;
         found := false;
         if not (char1 = '\')
         then
            loop
               --  Add to string ss
               --  [P2Ada]: !Help! Maybe (file,...) here
               --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
               Get (net_file);
               Get (char1);
               if char1 and potential_numbers
               then
                  ss := ss + char1;
               else
                  found := true;
               end if;
               exit when found or else End_of_Line (net_file);
            end loop;
         end if;
         if (ss /= "")
         then
            Val (ss, s, code);
            if (code /= 0)
            then
               s := 0.0;
            end if;
         else
            s := 0.0;
         end if;
         --  turn string ss into double number
      end Read_Number;
      --  read_number
      --  ********************************************************
      --  * Read_S_Params *
      --  [P2Ada]: !Help! Maybe (file)
      --  Advance through \s comment line
   begin
      filled_OK := true;
      npts := - 1;
      Get (net_file);
      Skip_Line;
      for ij in 1 .. max_params
      loop
         s_param_table (ij).all.calc := false;
         c_plot (ij) := null;
         plot_des (ij) := null;
      end loop;
      loop
         if End_of_Line (net_file)
         then
            --  ignore blank lines
            --  [P2Ada]: !Help! Maybe (file)
            --  [P2Ada]: !Help! Maybe (file,...) here
            Get (net_file);
            Skip_Line;
            char1 := ' ';
         else
            Get (net_file);
            Get (char1);
            if (char1 /= '\') and then (npts + 1 < ptmax)
            then
               Read_Number (freq);
               npts := npts + 1;
               if npts = 0
               then
                  fmin := freq;
               end if;
               ij := 0;
               loop
                  ij := ij + 1;
                  if c_plot (ij) = null
                  then
                     c_plot (ij) := plot_start (ij);
                  else
                     c_plot (ij) := c_plot (ij).all.next_p;
                  end if;
                  if abs (freq - design_freq) = 0
                  then
                     plot_des (ij) := c_plot (ij);
                  end if;
                  --  restore markers to fd
                  --  [P2Ada]: !Help! Maybe (file,...) here
                  --  [P2Ada]: !Help! Maybe (file,...) here
                  s_param_table (ij).all.calc := true;
                  c_plot (ij).all.filled := true;
                  Get (net_file);
                  Get (mag);
                  Get (ph);
                  c_plot (ij).all.x := mag * cos (ph * pi / 180);
                  c_plot (ij).all.y := mag * sin (ph * pi / 180);
                  exit when End_of_Line (net_file) or else (ij = max_params);
               end loop;
               --  [P2Ada]: !Help! Maybe (file)
               Get (net_file);
               Skip_Line;
            end if;
            --  if char
         end if;
         --  if Eoln else
         exit when (char1 = '\') or else End_of_File (net_file);
      end loop;
      if npts <= 1
      then
         filled_OK := false;
      end if;
      for ij in 1 .. max_params
      loop
         plot_end (ij) := c_plot (ij);
      end loop;
      finc := (freq - fmin) / npts;
   end Read_S_Params;
   --  * Read_S_Params *
   --  *
   --  Save board parameters to .puf file.
   --  *

   procedure Save_BoardO is
      sl, i : integer;
   begin
      for i in 1 .. 12
      loop
         --  * Convert Mu's to U's *
         if s_board (i, 2) = Mu
         then
            s_board (i, 2) := 'U';
         end if;
      end loop;
      --  [P2Ada]: !Help! Maybe (file,...) here
      --  [P2Ada]: !Help! Maybe (file,...) here
      --  [P2Ada]: !Help! Maybe (file,...) here
      --  [P2Ada]: !Help! Maybe (file,...) here
      --  [P2Ada]: !Help! Maybe (file,...) here
      --  [P2Ada]: !Help! Maybe (file,...) here
      --  [P2Ada]: !Help! Maybe (file,...) here
      --  [P2Ada]: !Help! Maybe (file,...) here
      --  [P2Ada]: !Help! Maybe (file)
      --  [P2Ada]: !Help! Maybe (file,...) here
      --  [P2Ada]: !Help! Maybe (file,...) here
      --  [P2Ada]: !Help! Maybe (file,...) here
      --  [P2Ada]: !Help! Maybe (file,...) here
      --  [P2Ada]: !Help! Maybe (file,...) here
      --  [P2Ada]: !Help! Maybe (file,...) here
      --  [P2Ada]: !Help! Maybe (file)
      --  [P2Ada]: !Help! Maybe (file,...) here
      --  [P2Ada]: !Help! Maybe (file,...) here
      --  [P2Ada]: !Help! Maybe (file,...) here
      --  [P2Ada]: !Help! Maybe (file,...) here
      --  [P2Ada]: !Help! Maybe (file,...) here
      --  [P2Ada]: !Help! Maybe (file,...) here
      --  [P2Ada]: !Help! Maybe (file)
      Put (net_file);
      Put ("\b");
      Put (lbrack);
      Put ("oard");
      Put (rbrack);
      Put (' ');
      Put (lbrack);
      Put (".puf file for PUFF, version 2.1d");
      Put (rbrack);
      New_Line;
      Put (net_file);
      Put ("d ");
      Put (display, 6);
      Put ("     ");
      Put (lbrack);
      Put ("display: 0 VGA or PUFF chooses, 1 EGA");
      Put (rbrack);
      New_Line;
      Put (net_file);
      Put ("o ");
      Put (Art_Form, 6);
      Put ("     ");
      Put (lbrack);
      Put ("artwork output format: 0 dot-matrix, 1 LaserJet, 2 HPGL file");
      Put (rbrack);
      New_Line;
      if Manhattan_Board
      then
         sl := 2;
      else
         if stripline
         then
            sl := 1;
         else
            sl := 0;
         end if;
      end if;
      --  [P2Ada]: !Help! Maybe (file,...) here
      --  [P2Ada]: !Help! Maybe (file,...) here
      --  [P2Ada]: !Help! Maybe (file,...) here
      --  [P2Ada]: !Help! Maybe (file,...) here
      --  [P2Ada]: !Help! Maybe (file,...) here
      --  [P2Ada]: !Help! Maybe (file,...) here
      --  [P2Ada]: !Help! Maybe (file)
      --  [P2Ada]: !Help! Maybe (file,...) here
      --  [P2Ada]: !Help! Maybe (file,...) here
      --  [P2Ada]: !Help! Maybe (file,...) here
      --  [P2Ada]: !Help! Maybe (file,...) here
      --  [P2Ada]: !Help! Maybe (file,...) here
      --  [P2Ada]: !Help! Maybe (file)
      --  [P2Ada]: !Help! Maybe (file,...) here
      --  [P2Ada]: !Help! Maybe (file,...) here
      --  [P2Ada]: !Help! Maybe (file,...) here
      --  [P2Ada]: !Help! Maybe (file,...) here
      --  [P2Ada]: !Help! Maybe (file,...) here
      --  [P2Ada]: !Help! Maybe (file)
      --  [P2Ada]: !Help! Maybe (file,...) here
      --  [P2Ada]: !Help! Maybe (file,...) here
      --  [P2Ada]: !Help! Maybe (file,...) here
      --  [P2Ada]: !Help! Maybe (file,...) here
      --  [P2Ada]: !Help! Maybe (file,...) here
      --  [P2Ada]: !Help! Maybe (file)
      --  [P2Ada]: !Help! Maybe (file,...) here
      --  [P2Ada]: !Help! Maybe (file,...) here
      --  [P2Ada]: !Help! Maybe (file,...) here
      --  [P2Ada]: !Help! Maybe (file,...) here
      --  [P2Ada]: !Help! Maybe (file,...) here
      --  [P2Ada]: !Help! Maybe (file)
      --  [P2Ada]: !Help! Maybe (file,...) here
      --  [P2Ada]: !Help! Maybe (file,...) here
      --  [P2Ada]: !Help! Maybe (file,...) here
      --  [P2Ada]: !Help! Maybe (file,...) here
      --  [P2Ada]: !Help! Maybe (file,...) here
      --  [P2Ada]: !Help! Maybe (file)
      --  [P2Ada]: !Help! Maybe (file,...) here
      --  [P2Ada]: !Help! Maybe (file,...) here
      --  [P2Ada]: !Help! Maybe (file,...) here
      --  [P2Ada]: !Help! Maybe (file,...) here
      --  [P2Ada]: !Help! Maybe (file,...) here
      --  [P2Ada]: !Help! Maybe (file)
      --  [P2Ada]: !Help! Maybe (file,...) here
      --  [P2Ada]: !Help! Maybe (file,...) here
      --  [P2Ada]: !Help! Maybe (file,...) here
      --  [P2Ada]: !Help! Maybe (file,...) here
      --  [P2Ada]: !Help! Maybe (file,...) here
      --  [P2Ada]: !Help! Maybe (file)
      --  [P2Ada]: !Help! Maybe (file,...) here
      --  [P2Ada]: !Help! Maybe (file,...) here
      --  [P2Ada]: !Help! Maybe (file,...) here
      --  [P2Ada]: !Help! Maybe (file,...) here
      --  [P2Ada]: !Help! Maybe (file,...) here
      --  [P2Ada]: !Help! Maybe (file)
      --  [P2Ada]: !Help! Maybe (file,...) here
      --  [P2Ada]: !Help! Maybe (file,...) here
      --  [P2Ada]: !Help! Maybe (file,...) here
      --  [P2Ada]: !Help! Maybe (file,...) here
      --  [P2Ada]: !Help! Maybe (file,...) here
      --  [P2Ada]: !Help! Maybe (file)
      --  [P2Ada]: !Help! Maybe (file,...) here
      --  [P2Ada]: !Help! Maybe (file,...) here
      --  [P2Ada]: !Help! Maybe (file,...) here
      --  [P2Ada]: !Help! Maybe (file,...) here
      --  [P2Ada]: !Help! Maybe (file,...) here
      --  [P2Ada]: !Help! Maybe (file)
      --  [P2Ada]: !Help! Maybe (file,...) here
      --  [P2Ada]: !Help! Maybe (file,...) here
      --  [P2Ada]: !Help! Maybe (file,...) here
      --  [P2Ada]: !Help! Maybe (file,...) here
      --  [P2Ada]: !Help! Maybe (file,...) here
      --  [P2Ada]: !Help! Maybe (file)
      --  [P2Ada]: !Help! Maybe (file,...) here
      --  [P2Ada]: !Help! Maybe (file,...) here
      --  [P2Ada]: !Help! Maybe (file,...) here
      --  [P2Ada]: !Help! Maybe (file,...) here
      --  [P2Ada]: !Help! Maybe (file,...) here
      --  [P2Ada]: !Help! Maybe (file)
      --  [P2Ada]: !Help! Maybe (file,...) here
      --  [P2Ada]: !Help! Maybe (file,...) here
      --  [P2Ada]: !Help! Maybe (file,...) here
      --  [P2Ada]: !Help! Maybe (file,...) here
      --  [P2Ada]: !Help! Maybe (file,...) here
      --  [P2Ada]: !Help! Maybe (file,...) here
      --  [P2Ada]: !Help! Maybe (file)
      --  [P2Ada]: !Help! Maybe (file,...) here
      --  [P2Ada]: !Help! Maybe (file,...) here
      --  [P2Ada]: !Help! Maybe (file,...) here
      --  [P2Ada]: !Help! Maybe (file,...) here
      --  [P2Ada]: !Help! Maybe (file,...) here
      --  [P2Ada]: !Help! Maybe (file,...) here
      --  [P2Ada]: !Help! Maybe (file)
      Put (net_file);
      Put ("t ");
      Put (sl, 6);
      Put ("     ");
      Put (lbrack);
      Put ("type: 0 for microstrip, 1 for stripline, 2 for Manhattan");
      Put (rbrack);
      New_Line;
      Put (net_file);
      Put ("zd  ");
      Put (s_board (1, 1) + ' ' + s_board (1, 2) + "Ohms ");
      Put (lbrack);
      Put ("normalizing impedance. 0<zd");
      Put (rbrack);
      New_Line;
      Put (net_file);
      Put ("fd  ");
      Put (s_board (2, 1) + ' ' + s_board (2, 2) + "Hz   ");
      Put (lbrack);
      Put ("design frequency. 0<fd");
      Put (rbrack);
      New_Line;
      Put (net_file);
      Put ("er  ");
      Put (s_board (3, 1) + "       ");
      Put (lbrack);
      Put ("dielectric constant. er>0");
      Put (rbrack);
      New_Line;
      Put (net_file);
      Put ("h   ");
      Put (s_board (4, 1) + ' ' + s_board (4, 2) + "m    ");
      Put (lbrack);
      Put ("dielectric thickness. h>0");
      Put (rbrack);
      New_Line;
      Put (net_file);
      Put ("s   ");
      Put (s_board (5, 1) + ' ' + s_board (5, 2) + "m    ");
      Put (lbrack);
      Put ("circuit-board side length. s>0");
      Put (rbrack);
      New_Line;
      Put (net_file);
      Put ("c   ");
      Put (s_board (6, 1) + ' ' + s_board (6, 2) + "m    ");
      Put (lbrack);
      Put ("connector separation. c>=0");
      Put (rbrack);
      New_Line;
      Put (net_file);
      Put ("r   ");
      Put (s_board (7, 1) + ' ' + s_board (7, 2) + "m    ");
      Put (lbrack);
      Put ("circuit resolution, r>0, use Um for micrometers");
      Put (rbrack);
      New_Line;
      Put (net_file);
      Put ("a   ");
      Put (s_board (8, 1) + ' ' + s_board (8, 2) + "m    ");
      Put (lbrack);
      Put ("artwork width correction.");
      Put (rbrack);
      New_Line;
      Put (net_file);
      Put ("mt  ");
      Put (s_board (9, 1) + ' ' + s_board (9, 2) + "m    ");
      Put (lbrack);
      Put ("metal thickness, use Um for micrometers.");
      Put (rbrack);
      New_Line;
      Put (net_file);
      Put ("sr  ");
      Put (s_board (10, 1) + ' ' + s_board (10, 2) + "m    ");
      Put (lbrack);
      Put ("metal surface roughness, use Um for micrometers.");
      Put (rbrack);
      New_Line;
      Put (net_file);
      Put ("lt   ");
      Put (s_board (11, 1) + "   ");
      Put (lbrack);
      Put ("dielectric loss tangent.");
      Put (rbrack);
      New_Line;
      Put (net_file);
      Put ("cd   ");
      Put (s_board (12, 1) + "   ");
      Put (lbrack);
      Put ("conductivity of metal in mhos/meter.");
      Put (rbrack);
      New_Line;
      Put (net_file);
      Put ("p   ");
      Put (reduction, 7, 3, 0);
      Put ("       ");
      Put (lbrack);
      Put ("photographic reduction ratio. p<=203.2mm/s");
      Put (rbrack);
      New_Line;
      Put (net_file);
      Put ("m   ");
      Put (miter_fraction, 7, 3, 0);
      Put ("       ");
      Put (lbrack);
      Put ("mitering fraction.  0<=m<1");
      Put (rbrack);
      New_Line;
      for i in 1 .. 12
      loop
         --  * Convert U's back to Mu's *
         if s_board (i, 2) = 'U'
         then
            s_board (i, 2) := Mu;
         end if;
      end loop;
   end Save_BoardO;
   --  save_boardO
   --  *
   --  Save Plot window parameters to .puf file.
   --  *

   procedure save_keyO is
      --  [P2Ada]: !Help! Maybe (file,...) here
      --  [P2Ada]: !Help! Maybe (file,...) here
      --  [P2Ada]: !Help! Maybe (file,...) here
      --  [P2Ada]: !Help! Maybe (file,...) here
      --  [P2Ada]: !Help! Maybe (file)
      tcompt : compt;
      i : integer;
      temp : line_string;
   begin
      Put (net_file);
      Put ("\k");
      Put (lbrack);
      Put ("ey for plot window");
      Put (rbrack);
      New_Line;
      for i in 1 .. 10
      loop
         if i = 1
         then
            tcompt := coord_start;
         else
            tcompt := tcompt.all.next_compt;
         end if;
         --  [P2Ada]: WITH instruction
         --  [P2Ada]: !Help! No type found -> add 'P2Ada_Var_3.' to fields
         declare P2Ada_Var_3 : < > renames tcompt.all;
         begin
            case i is
               when 1 =>
                  --  [P2Ada]: !Help! Maybe (file,...) here
                  --  [P2Ada]: !Help! Maybe (file,...) here
                  --  [P2Ada]: !Help! Maybe (file,...) here
                  --  [P2Ada]: !Help! Maybe (file,...) here
                  --  [P2Ada]: !Help! Maybe (file,...) here
                  --  [P2Ada]: !Help! Maybe (file)
                  Put (net_file);
                  Put ("du  " + descript);
                  Put ("   ");
                  Put (lbrack);
                  Put ("upper dB-axis limit");
                  Put (rbrack);
                  New_Line;
               when 2 =>
                  --  [P2Ada]: !Help! Maybe (file,...) here
                  --  [P2Ada]: !Help! Maybe (file,...) here
                  --  [P2Ada]: !Help! Maybe (file,...) here
                  --  [P2Ada]: !Help! Maybe (file,...) here
                  --  [P2Ada]: !Help! Maybe (file,...) here
                  --  [P2Ada]: !Help! Maybe (file)
                  Put (net_file);
                  Put ("dl  " + descript);
                  Put ("   ");
                  Put (lbrack);
                  Put ("lower dB-axis limit");
                  Put (rbrack);
                  New_Line;
               when 3 =>
                  --  [P2Ada]: !Help! Maybe (file,...) here
                  --  [P2Ada]: !Help! Maybe (file,...) here
                  --  [P2Ada]: !Help! Maybe (file,...) here
                  --  [P2Ada]: !Help! Maybe (file,...) here
                  --  [P2Ada]: !Help! Maybe (file,...) here
                  --  [P2Ada]: !Help! Maybe (file)
                  Put (net_file);
                  Put ("fl  " + descript);
                  Put ("   ");
                  Put (lbrack);
                  Put ("lower frequency limit. fl>=0");
                  Put (rbrack);
                  New_Line;
               when 4 =>
                  --  [P2Ada]: !Help! Maybe (file,...) here
                  --  [P2Ada]: !Help! Maybe (file,...) here
                  --  [P2Ada]: !Help! Maybe (file,...) here
                  --  [P2Ada]: !Help! Maybe (file,...) here
                  --  [P2Ada]: !Help! Maybe (file,...) here
                  --  [P2Ada]: !Help! Maybe (file)
                  Put (net_file);
                  Put ("fu  " + descript);
                  Put ("   ");
                  Put (lbrack);
                  Put ("upper frequency limit. fu>fl");
                  Put (rbrack);
                  New_Line;
               when 5 =>
                  --  delete "Points"
                  --  [P2Ada]: !Help! Maybe (file,...) here
                  --  [P2Ada]: !Help! Maybe (file,...) here
                  --  [P2Ada]: !Help! Maybe (file,...) here
                  --  [P2Ada]: !Help! Maybe (file,...) here
                  --  [P2Ada]: !Help! Maybe (file,...) here
                  --  [P2Ada]: !Help! Maybe (file)
                  temp := descript;
                  delete (temp, 1, 6);
                  Put (net_file);
                  Put ("pts" + temp);
                  Put ("   ");
                  Put (lbrack);
                  Put ("number of points, positive integer");
                  Put (rbrack);
                  New_Line;
               when 6 =>
                  --  [P2Ada]: !Help! Maybe (file,...) here
                  --  [P2Ada]: !Help! Maybe (file,...) here
                  --  [P2Ada]: !Help! Maybe (file,...) here
                  --  [P2Ada]: !Help! Maybe (file,...) here
                  --  [P2Ada]: !Help! Maybe (file,...) here
                  --  [P2Ada]: !Help! Maybe (file)
                  temp := descript;
                  delete (temp, 1, 12);
                  Put (net_file);
                  Put ("sr" + temp);
                  Put ("   ");
                  Put (lbrack);
                  Put ("Smith-chart radius. sr>0");
                  Put (rbrack);
                  New_Line;
               when 7 .. 10 =>
                  temp := descript;
                  delete (temp, 1, 1);
                  if (temp'length) > 0
                  then
                     --  [P2Ada]: !Help! Maybe (file,...) here
                     Put (net_file);
                     Put ("S   " + temp);
                     if i = 7
                     then
                        --  [P2Ada]: !Help! Maybe (file,...) here
                        --  [P2Ada]: !Help! Maybe (file,...) here
                        --  [P2Ada]: !Help! Maybe (file,...) here
                        --  [P2Ada]: !Help! Maybe (file,...) here
                        --  [P2Ada]: !Help! Maybe (file)
                        --  [P2Ada]: !Help! Maybe (file)
                        Put (net_file);
                        Put ("   ");
                        Put (lbrack);
                        Put ("subscripts must be 1, 2, 3, or 4");
                        Put (rbrack);
                        New_Line;
                     else
                        Put (net_file);
                        New_Line;
                     end if;
                  end if;
               when others =>
                  --  [P2Ada]: no otherwise / else in Pascal
                  null;
            end case;
         end;
         --  [P2Ada]: end of WITH
         --  case
      end loop;
      --  i
   end save_keyO;
   --  save_keyO
   --  *
   --  Save list of parts to .puf file.
   --  *

   procedure Save_PartsO is
      --  [P2Ada]: !Help! Maybe (file,...) here
      --  [P2Ada]: !Help! Maybe (file,...) here
      --  [P2Ada]: !Help! Maybe (file,...) here
      --  [P2Ada]: !Help! Maybe (file,...) here
      --  [P2Ada]: !Help! Maybe (file,...) here
      --  [P2Ada]: !Help! Maybe (file,...) here
      --  [P2Ada]: !Help! Maybe (file,...) here
      --  [P2Ada]: !Help! Maybe (file,...) here
      --  [P2Ada]: !Help! Maybe (file)
      tcompt : compt;
      des : line_string;
      i : integer;
   begin
      tcompt := null;
      Put (net_file);
      Put ("\p");
      Put (lbrack);
      Put ("arts window");
      Put (rbrack);
      Put (' ');
      Put (lbrack);
      Put ("O = Ohms, D = degrees, U = micro, |=parallel");
      Put (rbrack);
      New_Line;
      loop
         --  write component list
         if tcompt = null
         then
            --  find starting pointer
            tcompt := part_start;
         else
            tcompt := tcompt.all.next_compt;
         end if;
         --  or find next
         des := tcompt.all.descript;
         if (des'length) > 2
         then
            --  if descript more than just a letter
            --  delete part letter designation
            Delete (des, 1, 2);
            for i in 1 .. (des'length)
            loop
               case des (i) is
                  --  change to O's, D's, U's, and |'s
                  when Omega =>
                     des (i) := 'O';
                  when Degree =>
                     des (i) := 'D';
                  when Mu =>
                     des (i) := 'U';
                  when Parallel =>
                     des (i) := '|';
                  when others =>
                     --  [P2Ada]: no otherwise / else in Pascal
                     null;
               end case;
            end loop;
            --  case
            --  [P2Ada]: !Help! Maybe (file,...) here
            --  [P2Ada]: !Help! Maybe (file)
            --  if length(des) > 2
            --  write blank message
            --  [P2Ada]: !Help! Maybe (file,...) here
            --  [P2Ada]: !Help! Maybe (file,...) here
            --  [P2Ada]: !Help! Maybe (file,...) here
            --  [P2Ada]: !Help! Maybe (file,...) here
            --  [P2Ada]: !Help! Maybe (file)
            Put (net_file);
            Put (des);
            New_Line;
         else
            Put (net_file);
            Put (lbrack);
            Put ("Blank at Part ");
            Put (des);
            Put (rbrack);
            New_Line;
         end if;
         exit when tcompt.all.next_compt = null;
      end loop;
   end Save_PartsO;
   --  * Save_PartsO *
   --  *
   --  Save circuit to .puf file.
   --  *

   procedure save_circuitO is
   begin
      for key_i in 1 .. key_end
      loop
         if key_i = 1
         then
            --  [P2Ada]: !Help! Maybe (file,...) here
            --  [P2Ada]: !Help! Maybe (file,...) here
            --  [P2Ada]: !Help! Maybe (file,...) here
            --  [P2Ada]: !Help! Maybe (file,...) here
            --  [P2Ada]: !Help! Maybe (file)
            Put (net_file);
            Put ("\c");
            Put (lbrack);
            Put ("ircuit");
            Put (rbrack);
            New_Line;
         end if;
         --  [P2Ada]: !Help! Maybe (file,...) here
         --  [P2Ada]: !Help! Maybe (file,...) here
         Put (net_file);
         Put (Character'Pos (key_list (key_i).keyl), 4);
         Put (key_list (key_i).noden, 4);
         case key_list (key_i).keyl is
            when right_arrow =>
               --  [P2Ada]: !Help! Maybe (file,...) here
               --  [P2Ada]: !Help! Maybe (file)
               Put (net_file);
               Put ("  right");
               New_Line;
            when left_arrow =>
               --  [P2Ada]: !Help! Maybe (file,...) here
               --  [P2Ada]: !Help! Maybe (file)
               Put (net_file);
               Put ("  left");
               New_Line;
            when down_arrow =>
               --  [P2Ada]: !Help! Maybe (file,...) here
               --  [P2Ada]: !Help! Maybe (file)
               Put (net_file);
               Put ("  down");
               New_Line;
            when up_arrow =>
               --  [P2Ada]: !Help! Maybe (file,...) here
               --  [P2Ada]: !Help! Maybe (file)
               Put (net_file);
               Put ("  up");
               New_Line;
            when sh_right =>
               --  [P2Ada]: !Help! Maybe (file,...) here
               --  [P2Ada]: !Help! Maybe (file)
               Put (net_file);
               Put ("  shift-right");
               New_Line;
            when sh_left =>
               --  [P2Ada]: !Help! Maybe (file,...) here
               --  [P2Ada]: !Help! Maybe (file)
               Put (net_file);
               Put ("  shift-left");
               New_Line;
            when sh_down =>
               --  [P2Ada]: !Help! Maybe (file,...) here
               --  [P2Ada]: !Help! Maybe (file)
               Put (net_file);
               Put ("  shift-down");
               New_Line;
            when sh_up =>
               --  [P2Ada]: !Help! Maybe (file,...) here
               --  [P2Ada]: !Help! Maybe (file)
               Put (net_file);
               Put ("  shift-up");
               New_Line;
            when sh_1 =>
               --  [P2Ada]: !Help! Maybe (file,...) here
               --  [P2Ada]: !Help! Maybe (file)
               Put (net_file);
               Put ("  shift-1");
               New_Line;
            when sh_2 =>
               --  [P2Ada]: !Help! Maybe (file,...) here
               --  [P2Ada]: !Help! Maybe (file)
               Put (net_file);
               Put ("  shift-2");
               New_Line;
            when sh_3 =>
               --  [P2Ada]: !Help! Maybe (file,...) here
               --  [P2Ada]: !Help! Maybe (file)
               Put (net_file);
               Put ("  shift-3");
               New_Line;
            when sh_4 =>
               --  [P2Ada]: !Help! Maybe (file,...) here
               --  [P2Ada]: !Help! Maybe (file)
               Put (net_file);
               Put ("  shift-4");
               New_Line;
            when '+' =>
               --  [P2Ada]: !Help! Maybe (file,...) here
               --  [P2Ada]: !Help! Maybe (file)
               Put (net_file);
               Put ("  shift-=");
               New_Line;
            when Ctrl_n =>
               --  [P2Ada]: !Help! Maybe (file,...) here
               --  [P2Ada]: !Help! Maybe (file)
               Put (net_file);
               Put ("  Ctrl-n");
               New_Line;
            when others =>
               --  [P2Ada]: !Help! Maybe (file,...) here
               --  [P2Ada]: !Help! Maybe (file,...) here
               --  [P2Ada]: !Help! Maybe (file)
               Put (net_file);
               Put ("  ");
               Put (key_list (key_i).keyl);
               New_Line;
         end case;
         --  case
      end loop;
      --  for key_i
   end save_circuitO;
   --  save_circuitO
   --  *
   --  Save s-parameters to .puf file.
   --  *

   procedure save_s_paramsO is
      number_of_parameters, ij, txpt : integer;
      first_line : string (1 .. 120);
      mag, deg : Long_Float;
      last_plot_ptr : array (1 .. max_params) of plot_param;
   begin
      if filled_OK
      then
         --  [P2Ada]: !Help! Maybe (file,...) here
         --  [P2Ada]: !Help! Maybe (file,...) here
         --  [P2Ada]: !Help! Maybe (file,...) here
         --  [P2Ada]: !Help! Maybe (file,...) here
         --  [P2Ada]: !Help! Maybe (file)
         Put (net_file);
         Put ("\s");
         Put (lbrack);
         Put ("parameters");
         Put (rbrack);
         New_Line;
         number_of_parameters := 0;
         first_line := "";
         for ij in 1 .. max_params
         loop
            if s_param_table (ij).all.calc
            then
               --  save last plot position
               last_plot_ptr (ij) := c_plot (ij);
               c_plot (ij) := null;
               if first_line = ""
               then
                  first_line := "   f              " + s_param_table (ij).all.descript;
               else
                  first_line := first_line + "              " + s_param_table (ij).all.descript;
               end if;
               number_of_parameters := number_of_parameters + 1;
            end if;
         end loop;
         --  for ij;if s_param_table
         --  [P2Ada]: !Help! Maybe (file,...) here
         --  [P2Ada]: !Help! Maybe (file)
         Put (net_file);
         Put (first_line);
         New_Line;
         for txpt in 0 .. npts
         loop
            --  [P2Ada]: !Help! Maybe (file,...) here
            freq := fmin + finc * txpt;
            Put (net_file);
            Put (freq, 9, 5, 0);
            for ij in 1 .. max_params
            loop
               if s_param_table (ij).all.calc
               then
                  if c_plot (ij) = null
                  then
                     c_plot (ij) := plot_start (ij);
                  else
                     c_plot (ij) := c_plot (ij).all.next_p;
                  end if;
                  mag := sqrt (((c_plot (ij).all.x) ** 2) + ((c_plot (ij).all.y) ** 2));
                  deg := atan2 (c_plot (ij).all.x, c_plot (ij).all.y);
                  if betweenr (0.1, mag, 99.0, 0.0)
                  then
                     --  [P2Ada]: !Help! Maybe (file,...) here
                     --  [P2Ada]: !Help! Maybe (file,...) here
                     --  [P2Ada]: !Help! Maybe (file,...) here
                     --  [P2Ada]: !Help! Maybe (file,...) here
                     --  [P2Ada]: !Help! Maybe (file,...) here
                     --  [P2Ada]: !Help! Maybe (file,...) here
                     --  [P2Ada]: !Help! Maybe (file,...) here
                     Put (net_file);
                     Put (mag, 10, 5, 0);
                     Put (' ');
                     Put (deg, 6, 1, 0);
                  else
                     Put (net_file);
                     Put (' ');
                     Put (mag, 9);
                     Put (' ');
                     Put (deg, 6, 1, 0);
                  end if;
               end if;
            end loop;
            --  for ij ; if s_param_table
            --  [P2Ada]: !Help! Maybe (file)
            Put (net_file);
            New_Line;
         end loop;
         --  for txpt:=0 to npts
         for ij in 1 .. max_params
         loop
            if s_param_table (ij).all.calc
            then
               c_plot (ij) := last_plot_ptr (ij);
            end if;
         end loop;
         --  restore last plot position
      end if;
      --  if filled_OK
   end save_s_paramsO;
   --  save_s_paramsO
   --  *
   --  Give error message when bad board element is present.
   --  *

   procedure bad_board is
      i : integer;
   begin
      erase_message;
      message (2) := "Bad or invalid";
      i := 0;
      loop
         i := i + 1;
         if not (board (i))
         then
            case i is
               when 1 =>
                  message (3) := "zd";
               when 2 =>
                  message (3) := "fd";
               when 3 =>
                  message (3) := "er";
               when 4 =>
                  message (3) := 'h';
               when 5 =>
                  message (3) := 's';
               when 6 =>
                  message (3) := 'c';
               when 7 =>
                  message (3) := 'r';
               when 8 =>
                  message (3) := 'a';
               when 9 =>
                  message (3) := "mt";
               when 10 =>
                  message (3) := "sr";
               when 11 =>
                  message (3) := "lt";
               when 12 =>
                  message (3) := "cd";
               when 13 =>
                  message (3) := 'p';
               when 14 =>
                  message (3) := 'd';
               when 15 =>
                  message (3) := 't';
               when 16 =>
                  message (3) := 'm';
               when others =>
                  --  [P2Ada]: no otherwise / else in Pascal
                  null;
            end case;
            --  case
            message (3) := message (3) + " in .puf file";
         end if;
         exit when not (board (i));
      end loop;
      shutdown;
   end bad_board;
   --  * bad_board *
   --  *
   --  Read board parameters in setup.puf.
   --  *

   procedure read_setup (fname2 : in out file_string) is
      char1, char2 : Character;
   begin
      if setupexists (fname2)
      then
         loop
            --  [P2Ada]: !Help! Maybe (file,...) here
            --  [P2Ada]: !Help! Maybe (file,...) here
            --  [P2Ada]: !Help! Maybe (file)
            --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
            Get (net_file);
            Get (char1);
            Get (char2);
            Skip_Line;
            exit when ((char1 = '\') and then (char2 and ('b' | 'B' => True, others => False))) or else End_of_File (net_file);
         end loop;
         if not (End_of_File (net_file))
         then
            Read_Board (true);
         end if;
         Close (net_file);
      end if;
   end read_setup;
   --  * read_setup *
end pfrw;
