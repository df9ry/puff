
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with pfun1;                 use pfun1;

package body pfrw is

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
      loop
         if SeekEoln (net_file)
         then
            --  ignore blank lines
            Get (net_file);
            Skip_Line;
            char1 := ' ';
         else
            loop
               Get (net_file);
               Get (char1);
               exit when char1 /= ' ';
            end loop;
            if char1 /= '\'
            then
               Get (net_file);
               Get (char2);
               if char2 /= ' '
               then
                  loop
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
                     if char2 and ('r' | 'R' => True, others => False)
                     then
                        --  init-optional value
                        Board (10) := false;
                        Attach_Prefix (10, Mu, surface_roughness, true);
                     else
                        Attach_Prefix (5, 'm', bmax, false);
                     end if;
                  when 'c' | 'C' =>
                     --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
                     if char2 and ('d' | 'D' => True, others => False)
                     then
                        --  init-optional
                        --  [P2Ada]: !Help! Maybe (file,...) here
                        --  [P2Ada]: !Help! Maybe (file)
                        --  no units here
                        board (12) := false;
                        Get (net_file);
                        Get (conductivity);
                        Skip_Line;
                        if conductivity > 0
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
                     if char2 and ('t' | 'T' => True, others => False)
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
                     if Round (value) = 2
                     then
                        --  makes calculations easier
                        Manhattan_Board := true;
                        stripline := true;
                     else
                        Manhattan_Board := false;
                        stripline := Round (value) /= 0;
                     end if;
                     --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
                     if Round (value) and (0 .. 2 => True, others => False)
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

end pfrw;
