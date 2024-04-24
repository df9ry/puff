
with Ada.Numerics.Long_Elementary_Functions;
use  Ada.Numerics.Long_Elementary_Functions;

with Ada.Long_Float_Text_IO; use Ada.Long_Float_Text_IO;
with Ada.Text_IO;            use Ada.Text_IO;

with Interfaces.C;           use Interfaces.C;

with pfst;                   use pfst;
with pfun1;                  use pfun1;
with pfun2;                  use pfun2;
with pfun3;                  use pfun3;
with Utils;                  use Utils;

package body pfrw is

   procedure Read_Board (read_graphics : Boolean) is
      --  * Artwork Reduction Ratios in mm/dot *
      --  25.4 mm/in  /(120 dots) in x dirn for matrix artwork
      --  25.4 mm/in  /(144 dots) in y dirn for matrix artwork
      --  LaserJet reduction ratio = 25.4 mm/in * 1/150dpi
      --  unit-prefix string
      --  *****************************************************
      red_psx : constant := 0.2117;
      red_psy : constant := 0.1764;
      red_lasr : constant := 0.169333;
      value : Long_Float;
      unit_prf : String (1 .. 80);
      char1, char2, char3, prefix : Character;
      --  *
      --  Look for prefixes when reading board parameters.
      --  If no prefix and no unit then return 'x' to
      --  designate that default prefixes are to be used.
      --  *

      function file_prefix (id_string : String) return Character is
         Result_file_prefix : Character;
         pot_prefix : Character;
         id_string_v : Unbounded_String := To_Unbounded_String (id_string);
      begin
         while Element (id_string_v, 1) = ' '
         loop
            Delete (id_string_v, 1, 1);
         end loop;
         --  delete leading blanks)
         --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
         if is_in (To_C (Element (id_string_v, 1)), Eng_Dec_Mux)
         then
            --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
            pot_prefix := Element (id_string_v, 1);
            if Element (id_string_v, 1) = 'm' and then
              Element (id_string_v, 2) /= 'H' and then
              Element (id_string_v, 2) /= 'O'
            then
               pot_prefix := ' ';
            end if;
            --  if its just meters, then no prefix
         else
            if Element (id_string_v, 1) = 'U'
            then
               --  U = micro
               --  convert U to Mu
               --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
               pot_prefix := Character (Mu);
            else
               if Element (id_string_v, 1) = 'H'
                 or else Element (id_string_v, 1) = 'O'
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

      procedure Attach_Prefix (b_num : Integer; def_prefix : Character;
                               board_param : in out Long_Float;
                               zero_OK : Boolean) is
      begin
         Get (net_file, value);
         Get (net_file, unit_prf);
         Skip_Line;
         prefix := file_prefix (unit_prf);
         if def_prefix = 'G'
         then
            --  Do not attach prefix for xHz
            board_param := value;
            if prefix = 'x'
            then
               --  use default prefix
               s_board (b_num, 2) := To_Unbounded_String ("G");
               freq_prefix := 'G';
            else
               s_board (b_num, 2) := To_Unbounded_String ("" & prefix);
               freq_prefix := To_C (prefix);
            end if;
         else
            if prefix = 'x'
            then
               --  use default prefix
               --  if prefix is given
               --  return value in default units
               board_param := value;
               s_board (b_num, 2) := To_Unbounded_String ("" & def_prefix);
            else
               board_param := value *
                 Eng_Prefix (prefix) / Eng_Prefix (def_prefix);
               s_board (b_num, 2) := To_Unbounded_String ("" & prefix);
            end if;
         end if;
         if (board_param > 0.0) or else ((board_param = 0.0) and then zero_OK)
         then
            board (b_num) := True;
            Put (net_file, value, 7, 3, 0);
            Put (net_file, To_String (s_board (b_num, 1)));
            if ((value < 1.0e-3) or else (value > 1.0e+3)) and then
              not (value = 0.0)
            then
               Put (net_file, value, 8);
               Put (net_file, To_String (s_board (b_num, 1)));
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
         board (i) := True;
      end loop;
      --  Make these parameters optional
      --  initialize new parameters for old puff files
      Art_Form := 0;
      Laser_Art := False;
      metal_thickness := 0.0;
      s_board (9, 1) := To_Unbounded_String ("  0.000");
      s_board (9, 2) := To_Unbounded_String ("m");
      surface_roughness := 0.0;
      s_board (10, 1) := To_Unbounded_String ("  0.000");
      s_board (10, 2) := To_Unbounded_String ("" & Character (Mu));
      loss_tangent := 0.0;
      s_board (11, 1) := To_Unbounded_String ("  0.000");
      s_board (11, 2) := To_Unbounded_String (" ");
      conductivity := 5.80e+7;
      s_board (12, 1) := To_Unbounded_String ("  5.8E+7");
      s_board (12, 2) := To_Unbounded_String (" ");
      loop
         if Utils.SeekEoln (net_file)
         then
            --  ignore blank lines
            --  [P2Ada]: !Help! Maybe (file)
            --  advance to beginning of next line
            Utils.Skip_Line (net_file);
            char1 := ' ';
         else
            loop
               Get (net_file, char1);
               exit when char1 /= ' ';
            end loop;
            if char1 /= '\'
            then
               Get (net_file, char2);
               if char2 /= ' '
               then
                  loop
                     Get (net_file, char3);
                     exit when char3 = ' ';
                  end loop;
               end if;
               case char1 is
                  when 'z' | 'Z' =>
                     Attach_Prefix (1, ' ', Z0, False);
                  when 'f' | 'F' =>
                     --  prefix not attached here
                     Attach_Prefix (2, 'G', design_freq, False);
                  when 'e' | 'E' =>
                     Get (net_file, er);
                     Utils.Skip_Line (net_file);
                     if er > 0.0
                     then
                        --  no units
                        board (3) := True;
                        Put (net_file, er, 7, 3, 0);
                        Put (net_file, (To_String (s_board (3, 1))));
                        s_board (3, 2) := To_Unbounded_String (" ");
                     end if;
                  when 'h' | 'H' =>
                     Attach_Prefix (4, 'm', substrate_h, False);
                  when 's' | 'S' =>
                     if char2 = 'r' or else char2 = 'R'
                     then
                        --  init-optional value
                        board (10) := False;
                        Attach_Prefix (10, Character (Mu), surface_roughness,
                                       True);
                     else
                        Attach_Prefix (5, 'm', bmax, False);
                     end if;
                  when 'c' | 'C' =>
                     if char2 = 'd' or else char2 = 'D'
                     then
                        --  init-optional
                        --  no units here
                        board (12) := False;
                        Get (net_file, conductivity);
                        Utils.Skip_Line (net_file);
                        if conductivity > 0.0
                        then
                           --  no units
                           board (12) := True;
                           Put (net_file, conductivity, 8);
                           Put (net_file, To_String (s_board (12, 1)));
                           s_board (12, 2) := To_Unbounded_String (" ");
                        end if;
                     else
                        Attach_Prefix (6, 'm', con_sep, True);
                     end if;
                  when 'r' | 'R' =>
                     Attach_Prefix (7, 'm', resln, False);
                     sresln := s_board (7, 1) & s_board (7, 2) & 'm';
                  when 'a' | 'A' =>
                     Attach_Prefix (8, 'm', artwork_cor, True);
                  when 'm' | 'M' =>
                     if char2 = 't' or else char2 = 'T'
                     then
                        --  init-optional value
                        board (9) := False;
                        Attach_Prefix (9, 'm', metal_thickness, True);
                     else
                        Get (net_file, miter_fraction);
                        Utils.Skip_Line (net_file);
                        if 0.0 <= miter_fraction and then miter_fraction < 1.0
                        then
                           board (16) := True;
                        end if;
                     end if;
                  when 'l' | 'L' =>
                     --  Loss Tangent
                     --  init-optional
                     --  no units here
                     board (11) := False;
                     Get (net_file, loss_tangent);
                     Utils.Skip_Line (net_file);
                     if loss_tangent >= 0.0
                     then
                        --  no units
                        board (11) := True;
                        Put (net_file, loss_tangent, 8);
                        Put (net_file, To_String (s_board (11, 1)));
                        s_board (11, 2) := To_Unbounded_String (" ");
                     end if;
                  when 'p' | 'P' =>
                     Get (net_file, reduction);
                     Utils.Skip_Line (net_file);
                     if reduction > 0.0
                     then
                        if Laser_Art
                        then
                           --  setup 150 DPI
                           --  setup 144 x 120 DPI
                           psx := red_lasr / reduction;
                           psy := red_lasr / reduction;
                           board (13) := True;
                        else
                           psx := red_psx / reduction;
                           psy := red_psy / reduction;
                           board (13) := True;
                        end if;
                     end if;
                     --  if reduction and/or Laser_art
                  when 'd' | 'D' =>
                     if read_graphics
                     then
                        --  ignore the VGA/EGA setting on Linux
                        Get (net_file, value);
                        Utils.Skip_Line (net_file);
                        display := Round (value);
                        board (14) := True;
                        imin := 1;
                     else
                        Get (net_file, value);
                        Utils.Skip_Line (net_file);
                        board (14) := True;
                     end if;
                  when 'o' | 'O' =>
                     --  * New entry for artwork output *
                     --  [P2Ada]: !Help! Maybe (file,...) here
                     --  [P2Ada]: !Help! Maybe (file)
                     Get (net_file, value);
                     Utils.Skip_Line (net_file);
                     Art_Form := Round (value);
                     if Art_Form = 1
                     then
                        Laser_Art := True;
                     end if;
                  when 't' | 'T' =>
                     Get (net_file, value);
                     Utils.Skip_Line (net_file);
                     if Round (value) = 2
                     then
                        --  makes calculations easier
                        Manhattan_Board := True;
                        stripline := True;
                     else
                        Manhattan_Board := False;
                        stripline := Round (value) /= 0;
                     end if;
                     --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
                     if Round (value) >= 0 and then Round (value) <= 2
                     then
                        board (15) := True;
                     end if;
                  when others =>
                     begin
                        message (2) := To_Unbounded_String ("Unknown board");
                        message (3) :=
                          To_Unbounded_String ("parameter in .puf");
                        shutdown;
                     end;
               end case;
               --  case char1
            end if;
            --  if char1
         end if;
         --  if SeekEoln
         exit when (char1 = '\') or else End_Of_File (net_file);
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

   --  Read parts from .puf file.
   --  Called by Read_Net() in pfmain1a.pas.
   --  Upon a call to read_partsO the read index has already
   --  advanced to the point where a '\p' has been read.
   --  *
   procedure Read_PartsO is
      --  * Clear previous parts list *
      char1 : Character;
      i, j : Integer;
      des : Unbounded_String;
      tcompt : compt;
   begin
      Large_Parts := False;
      for i in 1 .. 18
      loop
         if i = 1
         then
            tcompt := part_start;
         else
            tcompt := tcompt.next_compt;
         end if;
         --  [P2Ada]: WITH instruction
         --  [P2Ada]: !Help! No type found -> add 'P2Ada_Var_1.' to fields
         declare P2Ada_Var_1 : compt_record renames tcompt.all;
         begin
            P2Ada_Var_1.descript := To_Unbounded_String
              ("" & Character'Val (LC_A_ORD + i - 1) & " ");
            P2Ada_Var_1.used := 0;
            P2Ada_Var_1.changed := False;
            P2Ada_Var_1.parsed := False;
            P2Ada_Var_1.f_file := null;
            P2Ada_Var_1.s_file := null;
            P2Ada_Var_1.s_ifile := null;
         end;
         --  [P2Ada]: end of WITH
         --  with
      end loop;
      --  for i:=1 to 18
      j := 0;
      loop
         if End_Of_Line (net_file)
         then
            --  if at end_of_line..
            --  [P2Ada]: !Help! Maybe (file)
            --  do carriage return
            --  initialize char1
            --  [P2Ada]: !Help! Maybe (file,...) here
            --  Get (net_file);
            Skip_Line;
            char1 := ' ';
         else
            --  Get (net_file);
            Get (char1);
            if char1 /= '\'
            then
               --  dont read first line with '\p'
               --  [P2Ada]: !Help! Maybe (file,...) here
               --  [P2Ada]: !Help! Maybe (file)
               --  read string
               --  Get (net_file);
               --  Get (des);
               Skip_Line;
               --  Insert (char1, des, 1);
               j := j + 1;
               if j <= 18
               then
                  i := 0; --  Pos (lbrack, des);
                  if i > 0
                  then
                     Delete (des, i, Length (des));
                  end if;
                  --  for i in 1 .. Length (des)
                  --  loop
                  --     case Element (des, i) is
                  --        when 'O' =>
                  --           des (i) := Omega;
                  --        when 'D' =>
                  --           des (i) := Degree;
                  --        when 'U' =>
                  --           des (i) := Mu;
                  --        when '|' =>
                  --           des (i) := Parallel;
                  --        when others =>
                  --           --  [P2Ada]: no otherwise / else in Pascal
                  --           null;
                  --     end case;
                  --  end loop;
                  --  case
                  while Element (des, Length (des)) = ' '
                  loop
                     Delete (des, Length (des), 1);
                  end loop;
                  --  delete extra blanks
                  if j = 1
                  then
                     tcompt := part_start;
                  else
                     tcompt := tcompt.all.next_compt;
                  end if;
                  declare P2Ada_Var_2 : compt_record renames tcompt.all;
                  begin
                     if Length (des) = 0
                     then
                        --  descript:=descript
                        --  leave part blank
                        P2Ada_Var_2.changed := True;
                     else
                        P2Ada_Var_2.descript := P2Ada_Var_2.descript & des;
                        P2Ada_Var_2.changed := True;
                        if j > 9
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
         exit when char1 = '\' or else End_Of_File (net_file);
      end loop;
   end Read_PartsO;

   --  *
   --  Read key from .puf file.
   --  *
   procedure Read_KeyO is
      --  len, j, i : Integer;
      --  des : line_string;
      --  c1, c2, c3, char1 : Character;
   begin
      for i in 1 .. 6
      loop
         s_key (i) := To_Unbounded_String (" ");
      end loop;
      for i in 7 .. 10
      loop
         s_key (i) := To_Unbounded_String ("");
      end loop;
      --  loop
      --     if End_Of_Line (net_file)
      --     then
      --        Get (net_file);
      --        Skip_Line;
      --        char1 := ' ';
      --     else
      --        Get (net_file);
      --        Get (char1);
      --        des := "";
      --        if char1 /= '\'
      --        then
      --           des := char1;
      --           loop
      --              Get (net_file);
      --              Get (char1);
      --              des := des + char1;
      --              exit when (char1 = lbrack) or else End_Of_Line
      --                 (net_file);
      --           end loop;
      --           Get (net_file);
      --           Skip_Line;
      --           c1 := des (1);
      --           c2 := des (2);
      --           c3 := des (3);
      --           while not is_in (des (1), CharArray ('+', '-', '.', ',',
      --                 '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
      --                 'e'))
      --           loop
      --              Delete (des, 1, 1);
      --           end loop;
      --           --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
      --           len := Length (des);
      --           while not is_in (des (len), CharArray ('+', '-', '.', ',',
      --                 '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
      --                 'e', 'E')) and then len > 0
      --           loop
      --              Delete (des, len, 1);
      --              len := (des'Length);
      --           end loop;
      --           case c1 is
      --              when 'd' | 'D' =>
      --                 --  [P2Ada]: "x in y" -> "x and y" redefine "and"
      --                 before
      --                 if c2 and ('u' | 'U' => True, others => False)
      --                 then
      --                    s_key (1) := des;
      --                 else
      --                    s_key (2) := des;
      --                 end if;
      --              when 'f' | 'F' =>
      --                 case c2 is
      --                    when 'l' | 'L' =>
      --                       s_key (3) := des;
      --                    when 'u' | 'U' =>
      --                       s_key (4) := des;
      --                    when 'd' | 'D' =>
      --                       if c3 = '/'
      --                       then
      --                          s_key (5) := des;
      --                       end if;
      --                    when others =>
      --                       --  [P2Ada]: no otherwise / else in Pascal
      --                       null;
      --                 end case;
      --                 --  case
      --              when 'p' | 'P' =>
      --                 --  pts=number of points
      --                 s_key (5) := des;
      --              when 's' | 'S' =>
      --                 --  [P2Ada]: "x in y" -> "x and y" redefine "and"
      --                 before
      --                 if c2 and ('r' | 'R' => True, others => False)
      --                 then
      --                    s_key (6) := des;
      --                 else
      --                    j := 6;
      --                    loop
      --                       j := j + 1;
      --                       exit when Length (s_key (j)) = 0 or else
      --                          (j > 9);
      --                    end loop;
      --                    s_key (j) := des;
      --                 end if;
      --              when others =>
      --                 --  [P2Ada]: no otherwise / else in Pascal
      --                 null;
      --           end case;
      --           --  case
      --        end if;
      --        --  if char1 ..
      --     end if;
      --     --  if Eoln
      --     exit when (char1 = '\') or else End_of_File (net_file);
      --  end loop;
   end Read_KeyO;

      --  Read s-parameters from .puf file.
   --  Uses procedure Read_Number.
   --  *

   procedure Read_S_Params is
      --  ********************************************************
      freq, mag, ph : constant Long_Float := 0.0;
      char1 : Character;
      ij : Integer;
   begin
      filled_OK := True;
      npts := -1;
      --  Get (net_file);
      Skip_Line;
      for ij in 1 .. max_params
      loop
         s_param_table (ij).calc := False;
         c_plot (ij) := null;
         plot_des (ij) := null;
      end loop;
      loop
         if End_Of_Line (net_file)
         then
            --  Get (net_file);
            Skip_Line;
            char1 := ' ';
         else
            --  Get (net_file);
            Get (char1);
            if (char1 /= '\') and then (npts + 1 < ptmax)
            then
               --  Read_Number (freq);
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
                  if abs (freq - design_freq) = 0.0
                  then
                     plot_des (ij) := c_plot (ij);
                  end if;
                  --  restore markers to fd
                  --  [P2Ada]: !Help! Maybe (file,...) here
                  --  [P2Ada]: !Help! Maybe (file,...) here
                  s_param_table (ij).calc := True;
                  c_plot (ij).filled := True;
                  --  Get (net_file);
                  --  Get (mag);
                  --  Get (ph);
                  c_plot (ij).x := mag * Cos (ph * Pi / 180.0);
                  c_plot (ij).y := mag * Sin (ph * Pi / 180.0);
                  exit when End_Of_Line (net_file) or else (ij = max_params);
               end loop;
               --  [P2Ada]: !Help! Maybe (file)
               --  Get (net_file);
               Skip_Line;
            end if;
            --  if char
         end if;
         --  if Eoln else
         exit when (char1 = '\') or else End_Of_File (net_file);
      end loop;
      if npts <= 1
      then
         filled_OK := False;
      end if;
      for ij in 1 .. max_params
      loop
         plot_end (ij) := c_plot (ij);
      end loop;
      finc := (freq - fmin) / Long_Float (npts);
   end Read_S_Params;

   --  *
   --  Read in circuit from .puf file.
   --  *

   procedure Read_CircuitO is
      key_i, nn : constant Integer := 0;
      char1 : Character;
   begin
      circuit_changed := True;
      key_end := 0;
      loop
         if not (End_Of_File (net_file))
         then
            --  read circuit
            if End_Of_Line (net_file)
            then
               Skip_Line;
               char1 := ' ';
            else
               --  Get (net_file);
               Get (char1);
               if char1 /= '\'
               then
                  Skip_Line;
                  key := char'Val (key_i);
                  update_key_list (nn);
               end if;
               --  if char1
            end if;
         end if;
         --  if Eoln
         exit when char1 = '\' or else End_Of_File (net_file);
      end loop;
      --  set_up for redraw
      --  key_i := 0;
   end Read_CircuitO;

   --  Read in xxx.puf file.
   --  *
   --  procedure Read_Net (file_name : Unbounded_String) is
   --  begin
   --     --  Change current file name
   --     --  ccompt:=Points_compt;   {previous location of plot_manager}
   --     --  if filled_OK then Plot_Manager(false,true,false,true);
   --     --  New(net_beg);
   --     --  Initialize alt_sweep object
   --     --  Parse before plotting to check for alt_sweep
   --     puff_file := file_name;
   --     Write_File_Name (file_name);
   --     ccompt := part_start;
   --     cx := ccompt.x_block;
   --     compt3 := ccompt;
   --     cx3 := cx;
   --     action := True;
   --     x_sweep.Init_Use;
   --     Pars_Compt_List;
--      if Large_Parts
--      then
--         Write_Expanded_Parts;
--      else
--         Write_Parts_ListO;
--         Write_Board_Parameters;
--      end if;
--      if filled_OK
--      then
--         Plot_Manager (False, True, False, True, True);
--      else
--         Draw_Graph (xmin (8), ymin (8), xmax (8), ymax (8), False);
--         Pick_Smith (admit_chart);
--      end if;
   --     key := F3;
--  end Read_Net;

   procedure Read_Net (fname : in out Unbounded_String;
                       init_graphics : Boolean)
   is
      char1, char2 : Character;
      file_read, bad_file : Boolean;
   begin
      file_read := False;
      marker_OK := False;
      filled_OK := False;
      bad_file := False;
      if (Index (fname, " ") = 0) and then (fname /= "")
      then
         fname := fname & ".puf";
      end if;
      if fileexists (True, net_file, fname)
      then
         Get (net_file, char1);
         loop
            if char1 = Character'Val (13)
            then
               --  Don't skip 2 lines on CR's
               Get (net_file, char2);
            else
               Get (net_file, char2);
               Utils.Skip_Line (net_file);
            end if;
            if char1 = '\'
            then
               case char2 is
                  when 'b' | 'B' =>
                     --  board parameters
                     Read_Board (init_graphics);
                     if board_read
                     then
                        if init_graphics
                        then
                           Screen_Init;
                           --  Init_Mem;
                           Fresh_Dimensions;
                        end if;
                        --  !
                        Erase_Circuit;
                        Set_Up_Board;
                     end if;
                  when 'k' | 'K' =>
                     --  read in 'key' = plot parameters
                     Read_KeyO;
                     Set_Up_KeyO;
                     bad_compt := False;
                     rho_fac := Get_Real (rho_fac_compt, 1);
                     if bad_compt or else (rho_fac <= 0.0)
                     then
                        rho_fac := 1.0;
                        rho_fac_compt.all.descript :=
                          To_Unbounded_String ("Smith radius 1.0");
                     end if;
                  when 'p' | 'P' =>
                     --  read parts list
                     Read_PartsO;
                  when 's' | 'S' =>
                     --  read in calculated s-parameters
                     Read_S_Params;
                  when 'c' | 'C' =>
                     --  read circuit
                     Read_CircuitO;
                  when others =>
                     begin
                        --  * readln(net_file); *
                        --  else advance a line
                        --  * read(net_file,char1); *
                        --  ! Should have character after backslash
                        message (1) := To_Unbounded_String ("Improper");
                        message (2) := To_Unbounded_String ("Puff file");
                        Write_Message;
                        bad_file := True;
                        board_read := False;
                        if not (init_graphics)
                        then
                           Close (net_file);
                           return;
                        end if;
                     end;
               end case;
               --  case
               --  [P2Ada]: !Help! Maybe (file,...) here
            else
               Get (net_file, char1);
            end if;
            --  look for '\' on this line
            exit when bad_file or else End_Of_File (net_file);
         end loop;
         Close (net_file);
         file_read := True;
      end if;
      --  if fileexists
      if not (board_read)
      then
         --  if couldn't Read_Board data
         --  then read setup.puf board data
         read_setup (fname);
         if not (board_read)
         then
            bad_board;
         end if;
         if init_graphics
         then
            Screen_Init;
            Fresh_Dimensions;
         end if;
         Erase_Circuit;
         file_read := True;
      end if;
      if file_read
      then
         --  Change current file name
         --  ccompt:=Points_compt;   {previous location of plot_manager}
         --  if filled_OK then Plot_Manager(false,true,false,true);
         --  New(net_beg);
         --  Initialize alt_sweep object
         --  Parse before plotting to check for alt_sweep
         puff_file := fname;
         Write_File_Name (fname);
         ccompt := part_start;
         cx := ccompt.all.x_block;
         compt3 := ccompt;
         cx3 := cx;
         action := True;
         x_sweep.Init_Use;
         Pars_Compt_List;
         if Large_Parts
         then
            Write_Expanded_Parts;
         else
            Write_Parts_ListO;
            Write_Board_Parameters;
         end if;
         if filled_OK
         then
            Plot_Manager (False, True, False, True, True);
         else
            Draw_Graph (xmin (8), ymin (8), xmax (8), ymax (8), False);
            Pick_Smith (admit_chart);
         end if;
         key := F3;
      end if;
      --  if file_read
   end Read_Net;

   procedure read_setup (fname2 : in out Unbounded_String) is
      char1, char2 : Character;
   begin
      if setupexists (fname2)
      then
         loop
            Get (net_file, char1);
            Get (net_file, char2);
            Utils.Skip_Line (net_file);
            exit when ((char1 = '\')
              and then (char2 = 'b' or else char2 = 'B'))
              or else End_Of_File (net_file);
         end loop;
         if not (End_Of_File (net_file))
         then
            Read_Board (True);
         end if;
         Close (net_file);
      end if;
   end read_setup;

   procedure bad_board is
      i : Integer;
   begin
      Erase_Message;
      message (2) := To_Unbounded_String ("Bad or invalid");
      i := 0;
      loop
         i := i + 1;
         if not (board (i))
         then
            case i is
               when 1 =>
                  message (3) := To_Unbounded_String ("zd");
               when 2 =>
                  message (3) := To_Unbounded_String ("fd");
               when 3 =>
                  message (3) := To_Unbounded_String ("er");
               when 4 =>
                  message (3) := To_Unbounded_String ("hv");
               when 5 =>
                  message (3) := To_Unbounded_String ("s");
               when 6 =>
                  message (3) := To_Unbounded_String ("c");
               when 7 =>
                  message (3) := To_Unbounded_String ("r");
               when 8 =>
                  message (3) := To_Unbounded_String ("a");
               when 9 =>
                  message (3) := To_Unbounded_String ("mt");
               when 10 =>
                  message (3) := To_Unbounded_String ("sr");
               when 11 =>
                  message (3) := To_Unbounded_String ("lt");
               when 12 =>
                  message (3) := To_Unbounded_String ("cd");
               when 13 =>
                  message (3) := To_Unbounded_String ("p");
               when 14 =>
                  message (3) := To_Unbounded_String ("d");
               when 15 =>
                  message (3) := To_Unbounded_String ("t");
               when 16 =>
                  message (3) := To_Unbounded_String ("m");
               when others =>
                  null;
            end case;
            --  case
            message (3) := message (3) & " in .puf file";
         end if;
         exit when not (board (i));
      end loop;
      shutdown;
   end bad_board;
end pfrw;
