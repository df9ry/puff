with Ada.Text_IO;             use Ada.Text_IO;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;

with xgraph;                  use xgraph;
with pfst;                    use pfst;
with pfun1;                   use pfun1;
with Utils;                   use Utils;
with Scanner;                 use Scanner;

procedure puff is

   --  Read in xxx.puf file.
   --  *
   procedure Read_Net (net_file : File_Type; init_graphics : Boolean) is
      file_read, bad_file : Boolean;
   begin
      file_read := False;
      marker_OK := False;
      filled_OK := False;
      bad_file := False;

      loop
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
                        Init_Mem;
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
                  rho_fac := get_real (rho_fac_compt, 1);
                  if bad_compt or else (rho_fac <= 0.0)
                  then
                     rho_fac := 1.0;
                     rho_fac_compt.descript := To_Unbounded_String
                                                     ("Smith radius 1.0");
                  end if;
               when 'p' | 'P' =>
                  --  read parts list
                  read_partsO;
               when 's' | 'S' =>
                  --  read in calculated s-parameters
                  Read_S_Params;
               when 'c' | 'C' =>
                  --  read circuit
                  read_circuitO;
               when others =>
                  begin
                     message (1) := To_Unbounded_String ("Improper");
                     message (2) := To_Unbounded_String ("Puff file");
                     Write_Message;
                     bad_file := True;
                     board_read := False;
                     if not (init_graphics)
                     then
                        return;
                     end if;
                  end;
            end case;
         else





      then
         --  Change current file name
         --  ccompt:=Points_compt;   {previous location of plot_manager}
         --  if filled_OK then Plot_Manager(false,true,false,true);
         --  New(net_beg);
         --  Initialize alt_sweep object
         --  Parse before plotting to check for alt_sweep
         puff_file := fname;
         write_file_name (fname);
         ccompt := part_start;
         cx := ccompt.all.x_block;
         compt3 := ccompt;
         cx3 := cx;
         action := true;
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
            Plot_Manager (false, true, false, true, true);
         else
            Draw_Graph (xmin (8), ymin (8), xmax (8), ymax (8), false);
            Pick_Smith (admit_chart);
         end if;
         key := F3;
      end if;
      --  if file_read
   end Read_Net;

begin
   Init_X;
   Puff_Start;

exception
   when Program_halted =>
      Put_Line ("Regular program halt.");
   when others =>
      Put_Line ("Other exception occurred!");
end puff;
