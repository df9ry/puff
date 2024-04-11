
with Ada.Numerics.Long_Elementary_Functions;
use  Ada.Numerics.Long_Elementary_Functions;

with Interfaces.C;          use Interfaces.C;

with pfst;                  use pfst;
with pfun1;                 use pfun1;
with pfun2;                 use pfun2;
with pfun3;                 use pfun3;
with Scanner;               use Scanner;
with Utils;                 use Utils;

package body pfrw is

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
   procedure Read_Net (net_file : File_Type; init_graphics : Boolean) is
      --  file_read, bad_file : Boolean;
   begin
      --  file_read := False;
      marker_OK := False;
      filled_OK := False;
      --  bad_file := False;

      loop
         exit when End_Of_File (net_file);
         SkipWhitespace (net_file);
         if cur_ch = '\'
         then
            GetCh (net_file);
            case cur_ch is
               when 'b' | 'B' =>
                  --  board parameters
                  Read_Board (net_file, init_graphics);
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
                     rho_fac_compt.descript := To_Unbounded_String
                                                     ("Smith radius 1.0");
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
                     message (1) := To_Unbounded_String ("Improper");
                     message (2) := To_Unbounded_String ("CMD: " & cur_ch);
                     Write_Message;
                     --  bad_file := True;
                     board_read := False;
                     shutdown;
                     return;
                  end;
            end case;
         else
            message (1) := To_Unbounded_String ("Improper");
            message (2) := To_Unbounded_String ("START: " & cur_ch);
            Write_Message;
            --  bad_file := True;
            board_read := False;
            shutdown;
            return;
         end if;
      end loop;
   end Read_Net;

   procedure Post_Read_Net (file_name : Unbounded_String) is
   begin
      --  Change current file name
      --  ccompt:=Points_compt;   {previous location of plot_manager}
      --  if filled_OK then Plot_Manager(false,true,false,true);
      --  New(net_beg);
      --  Initialize alt_sweep object
      --  Parse before plotting to check for alt_sweep
      puff_file := file_name;
      Write_File_Name (file_name);
      ccompt := part_start;
      cx := ccompt.x_block;
      compt3 := ccompt;
      cx3 := cx;
      action := True;
      x_sweep.Init_Use;
      Pars_Compt_List;
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
      key := F3;
   end Post_Read_Net;

   procedure Read_Board (net_file : File_Type; read_graphics : Boolean) is
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
      SkipToEndOfLine (net_file);
--      loop
--         if SkipToEndOfLine (net_file)
--         then
--            --  ignore blank lines
--            Get (net_file);
--            Skip_Line;
--            char1 := ' ';
--         else
--            loop
--               Get (net_file);
--               Get (char1);
--               exit when char1 /= ' ';
--            end loop;
--            if char1 /= '\'
--            then
--               Get (net_file);
--               Get (char2);
--               if char2 /= ' '
--               then
--                  loop
--                     Get (net_file);
--                     Get (char3);
--                     exit when char3 = ' ';
--                  end loop;
--               end if;
--               case char1 is
--                  when 'z' | 'Z' =>
--                     Attach_Prefix (1, ' ', z0, false);
--                  when 'f' | 'F' =>
--                     --  prefix not attached here
--                     Attach_Prefix (2, 'G', design_freq, false);
--                  when 'e' | 'E' =>
--                     --  no units here
--                     Get (net_file);
--                     Get (er);
--                     Skip_Line;
--                     if er > 0
--                     then
--                        --  no units
--                        board (3) := true;
--                        Put (er, 7, 3, 0, s_board (3, 1));
--                        s_board (3, 2) := ' ';
--                     end if;
--                  when 'h' | 'H' =>
--                     Attach_Prefix (4, 'm', substrate_h, false);
--                  when 's' | 'S' =>
--                     --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
--                     if char2 and ('r' | 'R' => True, others => False)
--                     then
--                        --  init-optional value
--                        Board (10) := false;
--                        Attach_Prefix (10, Mu, surface_roughness, true);
--                     else
--                        Attach_Prefix (5, 'm', bmax, false);
--                     end if;
--                  when 'c' | 'C' =>
--                     --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
--                     if char2 and ('d' | 'D' => True, others => False)
--                     then
--                        --  init-optional
--                        --  [P2Ada]: !Help! Maybe (file,...) here
--                        --  [P2Ada]: !Help! Maybe (file)
--                        --  no units here
--                        board (12) := false;
--                        Get (net_file);
--                        Get (conductivity);
--                        Skip_Line;
--                        if conductivity > 0
--                        then
--                           --  no units
--                           board (12) := true;
--                           Put (conductivity, 8, s_board (12, 1));
--                           s_board (12, 2) := ' ';
--                        end if;
--                     else
--                        Attach_Prefix (6, 'm', con_sep, true);
--                     end if;
--                  when 'r' | 'R' =>
--                     Attach_Prefix (7, 'm', resln, false);
--                     sresln := s_board (7, 1) + s_board (7, 2) + 'm';
--                  when 'a' | 'A' =>
--                     Attach_Prefix (8, 'm', artwork_cor, true);
--                  when 'm' | 'M' =>
--                     --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
--                     if char2 and ('t' | 'T' => True, others => False)
--                     then
--                        --  init-optional value
--                        --  [P2Ada]: !Help! Maybe (file,...) here
--                        --  [P2Ada]: !Help! Maybe (file)
--                        Board (9) := false;
--                        Attach_Prefix (9, 'm', metal_thickness, true);
--                     else
--                        Get (net_file);
--                        Get (miter_fraction);
--                        Skip_Line;
--                        if (0 <= miter_fraction)
--                             and then (miter_fraction < 1)
--                        then
--                           board (16) := true;
--                        end if;
--                     end if;
--                  when 'l' | 'L' =>
--                     --  Loss Tangent
--                     --  init-optional
--                     --  [P2Ada]: !Help! Maybe (file,...) here
--                     --  [P2Ada]: !Help! Maybe (file)
--                     --  no units here
--                     Board (11) := false;
--                     Get (net_file);
--                     Get (loss_tangent);
--                     Skip_Line;
--                     if loss_tangent >= 0
--                     then
--                        --  no units
--                        board (11) := true;
--                        Put (loss_tangent, 8, s_board (11, 1));
--                        s_board (11, 2) := ' ';
--                     end if;
--                  when 'p' | 'P' =>
--                     --  [P2Ada]: !Help! Maybe (file,...) here
--                     --  [P2Ada]: !Help! Maybe (file)
--                     Get (net_file);
--                     Get (reduction);
--                     Skip_Line;
--                     if reduction > 0
--                     then
--                        if Laser_Art
--                        then
--                           --  setup 150 DPI
--                           --  setup 144 x 120 DPI
--                           psx := red_lasr / reduction;
--                           psy := red_lasr / reduction;
--                           board (13) := true;
--                        else
--                           psx := red_psx / reduction;
--                           psy := red_psy / reduction;
--                           board (13) := true;
--                        end if;
--                     end if;
--                     --  if reduction and/or Laser_art
--                  when 'd' | 'D' =>
--                     if read_graphics
--                     then
--                        --  [P2Ada]: !Help! Maybe (file,...) here
--                        --  [P2Ada]: !Help! Maybe (file)
--                        --  ignore the VGA/EGA setting on Linux
--                        --  [P2Ada]: !Help! Maybe (file,...) here
--                        --  [P2Ada]: !Help! Maybe (file)
--                        Get (net_file);
--                        Get (value);
--                        Skip_Line;
--                        display := Round (value);
--                        board (14) := true;
--                        imin := 1;
--                     else
--                        Get (net_file);
--                        Get (value);
--                        Skip_Line;
--                        board (14) := true;
--                     end if;
--                  when 'o' | 'O' =>
--                     --  * New entry for artwork output *
--                     --  [P2Ada]: !Help! Maybe (file,...) here
--                     --  [P2Ada]: !Help! Maybe (file)
--                     Get (net_file);
--                     Get (value);
--                     Skip_Line;
--                     Art_Form := Round (value);
--                     if Art_Form = 1
--                     then
--                        Laser_Art := True;
--                     end if;
--                  when 't' | 'T' =>
--                     --  [P2Ada]: !Help! Maybe (file,...) here
--                     --  [P2Ada]: !Help! Maybe (file)
--                     Get (net_file);
--                     Get (value);
--                     Skip_Line;
--                     if Round (value) = 2
--                     then
--                        --  makes calculations easier
--                        Manhattan_Board := true;
--                        stripline := true;
--                     else
--                        Manhattan_Board := false;
--                        stripline := Round (value) /= 0;
--                     end if;
--                     --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
--                     if Round (value) and (0 .. 2 => True, others => False)
--                     then
--                        board (15) := true;
--                     end if;
--                  when others =>
--                     begin
--                        message (2) := "Unknown board";
--                        message (3) := "parameter in .puf";
--                        shutdown;
--                     end;
--               end case;
--               --  case char1
--            end if;
--            --  if char1
--         end if;
--         --  if SeekEoln
--         exit when (char1 = '\') or else End_Of_File (net_file);
--      end loop;
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

end pfrw;
