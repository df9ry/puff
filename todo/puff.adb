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

procedure PUFF is
   --  [P2Ada]: This is for 'Write([Boolean])'

   package Boolean_Text_IO is new Enumeration_IO (Boolean);
   use Boolean_Text_IO;
   --  [P2Ada]: This is for 'file' without type
   --  [P2Ada]: This is for the Halt pseudo-procedure
   --  Custom replacement for TUBO's "Crt" and "Graphics" units
   --  artwork code
   --  puff file read/write code
   --  puff start code
   --  Smith chart, help, and device file reading
   --  [P2Ada]: place it before main procedure
   --  puff FFT code

   package Byte_Direct_IO is new Ada.Direct_IO (Unsigned_8);
   Program_halted : exception;
   with dos, xgraph, initc, pfun1, pfun2, pfun3, pfart, pfrw, pfst, pfmsc, pffft;
   --  *
   --  Pick which Smith chart to draw.
   --  When smith_type is true then admittance chart selected.
   --  Both Smith chart routines are in PFST.PAS
   --  *

   procedure Pick_Smith (smith_type : boolean) is
      --  Erase Smith Chart region
   begin
      clear_window_gfx (xmin (10), ymin (10), xmax (10), ymax (10));
      Draw_EGA_Smith (not (smith_type));
      if Large_Smith
      then
         Write_BigSmith_Coordinates;
      end if;
   end Pick_Smith;
   --  *Pick_Smith *
   --  *
   --  Used to toggle background color.
   --  if next_color true then change to next color.
   --  if next_color false then change to last color.
   --  *

   procedure Change_Bk_Color is
      --  Last_Palette : PaletteType;
      color_var : Unsigned_16;
   begin
      color_var := GetBkColor;
      if color_var = Black
      then
         SetBkColor (Blue);
      else
         SetBkColor (Black);
      end if;
   end Change_Bk_Color;
   --  * Change_Bk_Color *
   --  ************* Main Circuit Drawing Functions *****************
   --  *
   --  Calls node_look to look for a node then
   --  looks for a port at current cursor postion.
   --  *

   function port_or_node_found return boolean is
      Result_port_or_node_found : boolean;
      i : integer;
   begin
      node_look;
      if cnet /= null
      then
         Result_port_or_node_found := true;
      else
         Result_port_or_node_found := false;
         for i in 1 .. min_ports
         loop
            if (abs (portnet (i).all.xr - xm) < resln) and then not (portnet (i).all.node) and then (abs (portnet (i).all.yr - ym) < resln)
            then
               Result_port_or_node_found := true;
               join_port (i, 1);
               return Result_port_or_node_found;
            end if;
         end loop;
      end if;
      --  if cnet
      return Result_port_or_node_found;
   end port_or_node_found;
   --  port_or_node_found
   --  *
   --  Check for drawing direction across the
   --  ends of a clines part.
   --  cur_dir returns 1:up, 2:right, 4:left, 8:down
   --  Called by Move_Net(dirnt,ivt : integer);
   --  *

   function Coupler_Jump (cur_dir : integer) return boolean is
      Result_Coupler_Jump : boolean;
      tcon : conn;
      port, d2 : integer;
      coupler_skip : boolean;
   begin
      coupler_jump := false;
      coupler_skip := false;
      if cnet /= null
      then
         --  see if a cline is at this connection
         tcon := null;
         loop
            if tcon = null
            then
               tcon := cnet.all.con_start;
            else
               tcon := tcon.all.next_con;
            end if;
            if tcon.all.mate /= null
            then
               if tcon.all.mate.all.net.all.com.all.typ = 'c'
               then
                  --  found cline
                  d2 := tcon.all.dir;
                  port := tcon.all.mate.all.conn_no;
                  case d2 is
                     --  * check conditions for jumping over ends *
                     when 1 =>
                        if ((port = 1) and then (cur_dir = 4) or else (port = 3) and then (cur_dir = 2) or else (port = 2) and then (cur_dir = 2) or else (port = 4) and then (cur_dir = 4))
                        then
                           coupler_skip := true;
                        end if;
                     when 2 =>
                        if ((port = 1) and then (cur_dir = 8) or else (port = 3) and then (cur_dir = 1) or else (port = 2) and then (cur_dir = 1) or else (port = 4) and then (cur_dir = 8))
                        then
                           coupler_skip := true;
                        end if;
                     when 4 =>
                        if ((port = 1) and then (cur_dir = 1) or else (port = 3) and then (cur_dir = 8) or else (port = 2) and then (cur_dir = 8) or else (port = 4) and then (cur_dir = 1))
                        then
                           coupler_skip := true;
                        end if;
                     when 8 =>
                        if ((port = 1) and then (cur_dir = 2) or else (port = 3) and then (cur_dir = 4) or else (port = 2) and then (cur_dir = 4) or else (port = 4) and then (cur_dir = 2))
                        then
                           coupler_skip := true;
                        end if;
                     when others =>
                        --  [P2Ada]: no otherwise / else in Pascal
                        null;
                  end case;
                  --  case
               end if;
            end if;
            --  if found cline
            exit when (tcon.all.next_con = null) or else (coupler_skip);
         end loop;
      end if;
      --  if cnet <> nil
      if coupler_skip
      then
         --  move ports: 1->3, 2->4, 3->1, 4->2
         case port is
            when 1 | 2 =>
               cnet := tcon.all.mate.all.next_con.all.next_con.all.mate.all.net;
            when 3 =>
               cnet := tcon.all.mate.all.net.all.con_start.all.mate.all.net;
            when 4 =>
               cnet := tcon.all.mate.all.net.all.con_start.all.next_con.all.mate.all.net;
            when others =>
               --  [P2Ada]: no otherwise / else in Pascal
               null;
         end case;
         --  case
         --  ! This is for dx_dy
         increment_pos (0);
         coupler_jump := true;
         iv := 0;
      end if;
      return Result_Coupler_Jump;
   end Coupler_Jump;
   --  * Coupler_Jump *
   --  *
   --  Connect up a new network to circuit.
   --  For the clines it must check for proper directions.
   --  *

   procedure Add_Net is
      --  ************************************************************
      i : integer;
      vcon : conn;
      vnet : net;
      special_coupler : boolean;
      --  *
      --  Don't allow cursor to step over path to external port.
      --  *

      function occupied_portO return boolean is
         Result_occupied_portO : boolean;
         i : integer;
      begin
         Result_occupied_portO := false;
         for i in 1 .. min_ports
         loop
            --  [P2Ada]: WITH instruction
            --  [P2Ada]: !Help! No type found -> add 'P2Ada_Var_1.' to fields
            declare P2Ada_Var_1 : < > renames portnet (i).all;
            begin
               if (abs (xr - xm) < resln) and then (abs (yr - ym) < resln) and then node
               then
                  --  jump back to circuit from port
                  Result_occupied_portO := true;
                  snapO;
               end if;
            end;
            --  [P2Ada]: end of WITH
         end loop;
         --  for-with
         return Result_occupied_portO;
      end occupied_portO;
      --  occupied_portO
      --  ****************************************************************
   begin
      special_coupler := look_backO;
      if not (off_boardO (1.0) or else occupied_portO)
      then
         --  Set flag if part from extra list has been used
         --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
         if (compt1.all.descript (1) and ('j' .. 'r' => True, others => False))
         then
            Extra_Parts_Used := True;
         end if;
         compt1.all.used := compt1.all.used + 1;
         vnet := new_net (compt1.all.number_of_con, false);
         Draw_Net (vnet);
         for i in 1 .. compt1.all.number_of_con
         loop
            --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
            if special_coupler and then (i and (1 | 3 => True, others => False))
            then
               cnet := Mate_Node (i);
               cnet.all.number_of_con := cnet.all.number_of_con + 1;
            else
               if port_or_node_found
               then
                  --  advance count
                  cnet.all.number_of_con := cnet.all.number_of_con + 1;
               else
                  cnet := new_net (1, true);
               end if;
               --  or make new node
            end if;
            --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
            vcon := new_con (vnet, dirn);
            vcon.all.conn_no := i;
            ccon := new_con (cnet, dirn);
            if (vnet.all.com.all.typ and ('i' | 'd' => True, others => False)) and then (i /= 1) and then (i /= compt1.all.number_of_con)
            then
               case dirn is
                  when 2 | 4 =>
                     vcon.all.dir := 6;
                     ccon.all.dir := 6;
                  when 1 | 8 =>
                     vcon.all.dir := 9;
                     ccon.all.dir := 9;
                  when others =>
                     --  [P2Ada]: no otherwise / else in Pascal
                     null;
               end case;
               --  case
            end if;
            --  if
            ccon.all.mate := vcon;
            vcon.all.mate := ccon;
            if i /= compt1.all.number_of_con
            then
               increment_pos (i);
            end if;
         end loop;
         --  for i
      end if;
      --  if not off_boardO
   end Add_Net;
   --  * Add_Net *
   --  *
   --  Remove a network from the circuit.
   --  *

   procedure rem_net is
      i, sdirn : integer;
      mnode, snet, onet : net;
   begin
      if not (port_dirn_used)
      then
         snet := null;
         cnet.all.com.all.used := cnet.all.com.all.used - 1;
         for i in 1 .. cnet.all.number_of_con
         loop
            if i = 1
            then
               ccon := cnet.all.con_start;
            else
               ccon := ccon.all.next_con;
            end if;
            --  [P2Ada]: WITH instruction
            --  [P2Ada]: !Help! No type found -> add 'P2Ada_Var_2.' to fields
            mnode := ccon.all.mate.all.net;
            dispose_con (ccon.all.mate);
            declare P2Ada_Var_2 : < > renames mnode.all;
            begin
               if number_of_con = 1
               then
                  onet := cnet;
                  cnet := mnode;
                  sdirn := dirn;
                  if ext_port (con_start)
                  then
                     join_port (con_start.all.port_type, 0);
                  end if;
                  cnet := onet;
                  dirn := sdirn;
               end if;
            end;
            --  [P2Ada]: end of WITH
            if mnode.all.number_of_con > 0
            then
               snet := mnode;
            end if;
         end loop;
         --  for i
         lengthxy (cnet);
         increment_pos (1);
         dispose_net (cnet);
         node_look;
         if cnet = null
         then
            cnet := snet;
            if cnet /= null
            then
               increment_pos (0);
            end if;
         end if;
         if read_kbd
         then
            draw_circuit;
         end if;
      end if;
      --  if not port
   end rem_net;
   --  rem_net
   --  *
   --  Step a distance = 1/2 part size,
   --  on exit cnet points to node if found.
   --  *

   procedure step_line is
   begin
      if not (port_dirn_used or else off_boardO (0.5))
      then
         compt1.all.step := true;
         increment_pos (- 1);
         node_look;
      end if;
      --  if not port
   end step_line;
   --  step_line
   --  *
   --  Step over a line, on exit cnet points to node.
   --  Enter:cnet=network that you are stepping over.
   --  Exit:cnet=node
   --  *

   procedure step_over_line is
      tcon : conn;
      i, j : integer;
   begin
      if not (port_dirn_used)
      then
         --  ! This is for dx_dy
         iv := 0;
         tcon := ccon.all.mate;
         if cnet.all.number_of_con = 1
         then
            --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
            message (1) := "Cannot step";
            message (2) := "over 1 port";
            update_key := false;
            cnet := cnet.all.con_start.all.mate.all.net;
         else
            if (cnet.all.com.all.typ and ('i' | 'd' => True, others => False))
            then
               if dirn = cnet.all.con_start.all.dir
               then
                  cnet := tcon.all.next_con.all.mate.all.net;
               else
                  j := tcon.all.conn_no - 1;
                  tcon := cnet.all.con_start;
                  for i in 2 .. j
                  loop
                     tcon := tcon.all.next_con;
                  end loop;
                  cnet := tcon.all.mate.all.net;
               end if;
               --  if not a device
            else
               case tcon.all.conn_no is
                  when 1 | 3 =>
                     cnet := tcon.all.next_con.all.mate.all.net;
                  when 2 =>
                     cnet := cnet.all.con_start.all.mate.all.net;
                  when 4 =>
                     cnet := cnet.all.con_start.all.next_con.all.next_con.all.mate.all.net;
                  when others =>
                     --  [P2Ada]: no otherwise / else in Pascal
                     null;
               end case;
               --  case
            end if;
            increment_pos (0);
         end if;
      end if;
      --  if not port
   end step_over_line;
   --  step_over_line
   --  *
   --  Procedure for calling either
   --  rem_net :  delete part from the circuit
   --  step_over_line : jump to opposite end of current part
   --  step_line : move half the distance of current part
   --  add_net : add part (net) to circuit
   --  if coupler_jump() then jump across clines ends
   --  *

   procedure Move_Net (dirnt, ivt : integer) is
   begin
      if compt1.all.parsed and then not (missing_part)
      then
         dirn := dirnt;
         iv := ivt;
         if con_found
         then
            if iv = 0
            then
               rem_net;
            else
               step_over_line;
            end if;
         else
            if iv = 0
            then
               step_line;
            else
               if not (coupler_jump (dirnt))
               then
                  add_net;
               end if;
            end if;
         end if;
      else
         erase_message;
         if not (read_kbd)
         then
            --  if problem during circuit read
            --  delete 'checking circuit'
            key := F3;
            read_kbd := true;
            compt3 := ccompt;
            cx3 := compt3.all.x_block;
            message (1) := "Part used in";
            message (2) := "layout has been";
            message (3) := "deleted";
            GotoXY (checking_position (1), checking_position (2));
            Put ("                  ");
         else
            message (2) := "Invalid part";
         end if;
         update_key := false;
      end if;
      --  if parsed
   end Move_Net;
   --  * Move_Net *
   --  *
   --  Pars the component list.
   --  If action=true then find part dimensions
   --  else find s-parameters.
   --  Careful here on memory management!
   --  *

   procedure Pars_Compt_List is
      pars, reload_all_devices : boolean;
      tcompt : compt;
   begin
      if action
      then
         --  Check for alt_sweep and device file changes
         tcompt := null;
         reload_all_devices := false;
         loop
            --  step through and Reset if a sweep_compt was changed
            if tcompt = null
            then
               tcompt := part_start;
            else
               tcompt := tcompt.all.next_compt;
            end if;
            --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
            x_sweep.Check_Reset (tcompt);
            if tcompt.all.changed and then (get_lead_charO (tcompt) and ('i' | 'd' => True, others => False))
            then
               if (Marked (dev_beg))
               then
                  Release_Mem (dev_beg);
               end if;
               --  Release Device
               --  re-mark block
               --  memory block
               --  net memory sits above device memory
               --  force Redraw_circuit to reset net_beg
               --  forces all device files to be reloaded
               Mark_Mem (dev_beg);
               Init_Marker (net_beg);
               circuit_changed := true;
               reload_all_devices := true;
            end if;
            exit when (tcompt.all.next_compt = null);
         end loop;
         if reload_all_devices
         then
            --  Check for unchanged devic files
            tcompt := null;
            loop
               --  force all device files to be reloaded at marker dev_beg
               if tcompt = null
               then
                  tcompt := part_start;
               else
                  tcompt := tcompt.all.next_compt;
               end if;
               --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
               if (get_lead_charO (tcompt) and ('i' | 'd' => True, others => False))
               then
                  tcompt.all.changed := True;
               end if;
               exit when (tcompt.all.next_compt = null);
            end loop;
         end if;
      end if;
      tcompt := null;
      bad_compt := false;
      loop
         if tcompt = null
         then
            tcompt := part_start;
         else
            tcompt := tcompt.all.next_compt;
         end if;
         --  [P2Ada]: WITH instruction
         --  [P2Ada]: !Help! No type found -> add 'P2Ada_Var_3.' to fields
         declare P2Ada_Var_3 : < > renames tcompt.all;
         begin
            if changed and then ((used > 0) or else step)
            then
               circuit_changed := true;
            end if;
            if action
            then
               pars := changed;
            else
               pars := used > 0;
            end if;
            if pars
            then
               parsed := true;
               if action
               then
                  --  init to check for alt_sweep-needed?
                  typ := get_lead_charO (tcompt);
                  sweep_compt := false;
               end if;
               case typ is
                  when 't' =>
                     tlineO (tcompt);
                  when 'q' =>
                     qline (tcompt);
                  when 'c' =>
                     clinesO (tcompt);
                  when 'd' | 'i' =>
                     if action
                     then
                        Device_Read (tcompt, (typ = 'i'));
                     else
                        Device_S (tcompt, (typ = 'i'));
                     end if;
                  when 'l' =>
                     lumpedO (tcompt);
                  when 'x' =>
                     transformer (tcompt);
                  when 'a' =>
                     attenuator (tcompt);
                  when ' ' =>
                     parsed := false;
                  when others =>
                     begin
                        parsed := false;
                        bad_compt := true;
                        message (1) := typ + " is an";
                        message (2) := "unknown part";
                     end;
               end case;
               --  case
            end if;
            --  if pars
            if not (bad_compt)
            then
               changed := false;
            else
               if window_number = 3
               then
                  ccompt := tcompt;
               end if;
            end if;
         end;
         --  [P2Ada]: end of WITH
         --  with
         exit when ((tcompt.all.next_compt = null) or else bad_compt);
      end loop;
      if bad_compt
      then
         write_message;
      end if;
   end Pars_Compt_List;
   --  * Pars_Compt_List *
   --  *
   --  Pressing the "=" sign will cause a tline or clines
   --  to be parsed, and the values of the computations
   --  to be displayed.  This only works with
   --  tlines, qlines, and clines.
   --  *

   procedure Pars_Single_Part (tcompt : compt) is
      --  *****************************************************
      pos_prefix : string := "EPTGMk m" + Mu + "npfa";
      i : integer;
      d_s, avg_ere : Long_Float;

      procedure Big_Check (dt : in out Long_Float; i_in : in out integer) is
      begin
         while (abs (dt) > 1000.0)
         loop
            --  set-up prefix change
            dt := dt / 1000;
            i_in := i_in - 1;
         end loop;
      end Big_Check;
      --  *****************************************************

      procedure Small_Check (dt : in out Long_Float; i_in : in out integer) is
      begin
         while (abs (dt) < 0.01)
         loop
            --  set-up prefix change
            dt := dt * 1000;
            i_in := i_in + 1;
         end loop;
      end Small_Check;
      --  *****************************************************
      --  [P2Ada]: WITH instruction
      --  [P2Ada]: !Help! No type found -> add 'P2Ada_Var_4.' to fields
   begin
      erase_message;
      action := true;
      bad_compt := false;
      declare P2Ada_Var_4 : < > renames tcompt.all;
      begin
         --  force future re-parsing and check reset
         --  init x_sweep for later re-parsing
         changed := true;
         x_sweep.Check_Reset (tcompt);
         typ := get_lead_charO (tcompt);
         case typ is
            when 't' =>
               tlineO (tcompt);
            when 'q' =>
               qline (tcompt);
            when 'c' =>
               clinesO (tcompt);
            when others =>
               beep;
         end case;
         --  case
         if bad_compt
         then
            --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
            write_message;
         else
            if (typ and ('t' | 'q' | 'c' => True, others => False))
            then
               if super
               then
                  --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
                  TextCol (lightgray);
                  GotoXY (xmin (6) + 2, ymin (6));
                  if (typ and ('t' | 'q' => True, others => False))
                  then
                     Put ("Z : ");
                     Put (zed, 7, 3, 0);
                     Put (Omega);
                  end if;
                  if (typ = 'c')
                  then
                     Put ("Ze: ");
                     Put (zed, 7, 3, 0);
                     Put (Omega);
                     GotoXY (xmin (6) + 2, ymin (6) + 1);
                     Put ("Zo: ");
                     Put (zedo, 7, 3, 0);
                     Put (Omega);
                     if stripline
                     then
                        avg_ere := er;
                     else
                        avg_ere := 4 * e_eff_e0 * e_eff_o0 / ((sqrt (e_eff_e0) + sqrt (e_eff_o0)) ** 2);
                     end if;
                     GotoXY (xmin (6) + 2, ymin (6) + 2);
                     Put ("l : ");
                     Put ((lngth0 * sqrt (avg_ere) * 360 / lambda_fd), 7, 3, 0);
                     Put (Degree);
                  else
                     GotoXY (xmin (6) + 2, ymin (6) + 1);
                     Put ("l : ");
                     Put ((wavelength * 360), 7, 3, 0);
                     Put (Degree);
                  end if;
                  --  if super
                  --  specify prefix for mm
                  --  Check for large or small values of d_s
               else
                  TextCol (lightgray);
                  i := 8;
                  d_s := lngth;
                  if (d_s /= 0.0)
                  then
                     Big_Check (d_s, i);
                     Small_Check (d_s, i);
                  end if;
                  --  specify prefix for mm
                  --  Check for large or small values of d_s
                  GotoXY (xmin (6) + 2, ymin (6));
                  Put ("l: ");
                  Put (d_s, 7, 3, 0);
                  Put (pos_prefix (i));
                  Put ('m');
                  i := 8;
                  d_s := width;
                  if (d_s /= 0.0)
                  then
                     Big_Check (d_s, i);
                     Small_Check (d_s, i);
                  end if;
                  GotoXY (xmin (6) + 2, ymin (6) + 1);
                  Put ("w: ");
                  Put (d_s, 7, 3, 0);
                  Put (pos_prefix (i));
                  Put ('m');
                  if typ = 'c'
                  then
                     --  specify prefix for mm
                     --  Check for large or small values of d_s
                     i := 8;
                     d_s := con_space - width;
                     if (d_s /= 0.0)
                     then
                        Big_Check (d_s, i);
                        Small_Check (d_s, i);
                     end if;
                     GotoXY (xmin (6) + 2, ymin (6) + 2);
                     Put ("s: ");
                     Put (d_s, 7, 3, 0);
                     Put (pos_prefix (i));
                     Put ('m');
                  end if;
               end if;
            end if;
         end if;
         --  typ in
      end;
      --  [P2Ada]: end of WITH
      --  with
   end Pars_Single_Part;
   --  * Pars_Single_Part *
   --  *
   --  Parse the entries in the board parameter list.
   --  *

   procedure Board_Parser is
      tcompt : compt;
      value : Long_Float;
      unit_type, prefix : Character;
      value_str : line_string;
      alt_param : boolean;
   begin
      tcompt := null;
      bad_compt := false;
      loop
         if tcompt = null
         then
            tcompt := board_start;
         else
            tcompt := tcompt.all.next_compt;
         end if;
         --  [P2Ada]: WITH instruction
         --  [P2Ada]: !Help! No type found -> add 'P2Ada_Var_5.' to fields
         declare P2Ada_Var_5 : < > renames tcompt.all;
         begin
            if changed
            then
               --  first letter gives parameter
               --  ! if unit is 'm' then value is returned in mm
               --  Otherwise prefix is factored in
               Board_Changed := true;
               parsed := true;
               typ := descript (1);
               Get_Param (tcompt, 1, value, value_str, unit_type, prefix, alt_param);
               if not (bad_compt)
               then
                  case typ is
                     when 'z' =>
                        if (value > 0) and then (unit_type = Omega)
                        then
                           z0 := value;
                           s_board (1, 1) := value_str;
                           s_board (1, 2) := prefix;
                        else
                           Message (3) := "in zd";
                           bad_compt := true;
                        end if;
                     when 'f' =>
                        if (value > 0) and then (unit_type = 'H')
                        then
                           --  !normalize-take out the prefix
                           design_freq := value / Eng_prefix (prefix);
                           s_board (2, 1) := value_str;
                           s_board (2, 2) := prefix;
                           freq_prefix := prefix;
                        else
                           Message (3) := "in fd";
                           bad_compt := true;
                        end if;
                     when 'e' =>
                        if (value > 0) and then (unit_type = '?')
                        then
                           er := value;
                           s_board (3, 1) := value_str;
                           s_board (3, 2) := prefix;
                        else
                           Message (3) := "in er";
                           bad_compt := true;
                        end if;
                     when 'h' =>
                        if (value > 0) and then (unit_type = 'm')
                        then
                           substrate_h := value;
                           s_board (4, 1) := value_str;
                           s_board (4, 2) := prefix;
                        else
                           Message (3) := "in h";
                           bad_compt := true;
                        end if;
                     when 's' =>
                        if (value > 0) and then (unit_type = 'm')
                        then
                           --  Ensure con_sep re-calculation
                           --  since it's effected by bmax
                           bmax := value;
                           s_board (5, 1) := value_str;
                           s_board (5, 2) := prefix;
                           tcompt.all.next_compt.all.changed := true;
                        else
                           Message (3) := "in s";
                           bad_compt := true;
                        end if;
                     when 'c' =>
                        if (value >= 0) and then (unit_type = 'm')
                        then
                           con_sep := value;
                           s_board (6, 1) := value_str;
                           s_board (6, 2) := prefix;
                        else
                           Message (3) := "in c";
                           bad_compt := true;
                        end if;
                     when others =>
                        --  [P2Ada]: no otherwise / else in Pascal
                        null;
                  end case;
                  --  case
                  if bad_compt
                  then
                     Message (2) := "Bad value or unit";
                  end if;
               end if;
               --  if not(bad_unit)
            end if;
            --  if changed
            if not (bad_compt)
            then
               changed := false;
            else
               if window_number = 4
               then
                  ccompt := tcompt;
               end if;
            end if;
         end;
         --  [P2Ada]: end of WITH
         --  with
         exit when ((tcompt.all.next_compt = null) or else bad_compt);
      end loop;
      if bad_compt
      then
         write_message;
      end if;
   end Board_Parser;
   --  * Board_Parser *
   --  *
   --  Get values of coordintes in Plot window.
   --  symin and symax are used by other plotting
   --  routines.
   --  *

   procedure Get_Coords is
      --  point to dBmax value
      tcoord : compt;
   begin
      bad_compt := false;
      tcoord := dBmax_ptr;
      symax := get_real (tcoord, 1);
      if bad_compt
      then
         return;
      end if;
      --  point to dBmin value
      tcoord := tcoord.all.next_compt;
      symin := get_real (tcoord, 1);
      if bad_compt
      then
         return;
      end if;
      if symin >= symax
      then
         bad_compt := true;
         message (1) := "Must have";
         message (2) := "dB(max) > dB(min)";
         ccompt := tcoord;
         return;
      end if;
      --  point to fmin
      tcoord := tcoord.all.next_compt;
      sxmin := get_real (tcoord, 1);
      if bad_compt
      then
         return;
      end if;
      if (sxmin < 0) and then not (Alt_Sweep)
      then
         bad_compt := true;
         message (1) := "Must have";
         message (2) := "frequency >= 0";
         ccompt := tcoord;
         return;
      end if;
      --  point to fmax
      tcoord := tcoord.all.next_compt;
      sxmax := get_real (tcoord, 1);
      if bad_compt
      then
         return;
      end if;
      if (sxmax < 0) and then not (Alt_Sweep)
      then
         bad_compt := true;
         message (1) := "Must have";
         message (2) := "frequency >= 0";
         ccompt := tcoord;
         return;
      end if;
      if sxmin >= sxmax
      then
         bad_compt := true;
         message (1) := "Plot must have";
         message (2) := "x_max > x_min";
         ccompt := tcoord;
         return;
      end if;
      sfx1 := (xmax (8) - xmin (8)) / (sxmax - sxmin);
      sfy1 := (ymax (8) - ymin (8)) / (symax - symin);
      sigma := (symax - symin) / 100.0;
      rho_fac := get_real (rho_fac_compt, 1);
      if (rho_fac <= 0.0) or else bad_compt
      then
         bad_compt := true;
         message (1) := "The Smith chart";
         message (2) := "radius must be >0";
         ccompt := rho_fac_compt;
      end if;
   end Get_Coords;
   --  * Get_coords *
   --  *
   --  Fill array sa with s-parameters and then load
   --  into linked list of plot parameters ready for spline.
   --  *

   procedure Fill_Sa (P2Ada_no_keyword_out : BOOLEAN) is
      i, j, ij, sfreq : integer;
      tcon, scon : conn;
      s : s_param;
      sa : array (1 .. max_params, 1 .. max_params) of access TComplex;
      --  s-params array

      procedure Dispose is new Ada.Unchecked_Deallocation (TComplex, max_params);
   begin
      cnet := net_start;
      FillChar (sa, (sa'size / 8), 0);
      if (not bad_compt)
      then
         loop
            tcon := cnet.all.con_start;
            if (tcon /= null)
            then
               loop
                  j := tcon.all.port_type;
                  s := tcon.all.s_start;
                  scon := cnet.all.con_start;
                  loop
                     i := scon.all.port_type;
                     if (s /= null)
                     then
                        if ((i * j > 0) and then (s.all.z /= null))
                        then
                           sa (i, j) := addr (s.all.z.all.c);
                        end if;
                        s := s.all.next_s;
                     end if;
                     scon := scon.all.next_con;
                     exit when (scon = null);
                  end loop;
                  tcon := tcon.all.next_con;
                  exit when (tcon = null);
               end loop;
            end if;
            cnet := cnet.all.next_net;
            exit when (cnet = null);
         end loop;
      end if;
      sfreq := xmin (8) + Round ((freq - sxmin) * sfx1);
      for ij in 1 .. max_params
      loop
         if (P2Ada_no_keyword_out)
         then
            Write_FreqO;
         end if;
         --  re-calculates freq
         if s_param_table (ij).all.calc
         then
            if xpt = 0
            then
               c_plot (ij) := plot_start (ij);
               plot_des (ij) := null;
            else
               c_plot (ij) := c_plot (ij).all.next_p;
            end if;
            plot_end (ij) := c_plot (ij);
            if xpt = Round ((design_freq - fmin) / finc)
            then
               plot_des (ij) := c_plot (ij);
            end if;
            --  xpt is the point where the design freq. is located
            c_plot (ij).all.filled := false;
            case s_param_table (ij).all.descript (1) is
               when 's' | 'S' =>
                  i := si (ij);
                  j := sj (ij);
                  if sa (i, j) /= null
                  then
                     c_plot (ij).all.x := sa (i, j).all.r;
                     c_plot (ij).all.y := sa (i, j).all.i;
                  else
                     c_plot (ij).all.x := 0;
                     c_plot (ij).all.y := 0;
                  end if;
                  c_plot (ij).all.filled := true;
               when others =>
                  --  [P2Ada]: no otherwise / else in Pascal
                  null;
            end case;
            --  case
            if ((not (bad_compt)) and then (P2Ada_no_keyword_out))
            then
               write_sO (ij);
               calc_posO (c_plot (ij).all.x, c_plot (ij).all.y, 0, 1, sfreq, false);
               if spline_in_smith
               then
                  box (spx, spy, ij);
               end if;
               if not (Large_Smith) and then spline_in_rect
               then
                  box (sfreq, spp, ij);
               end if;
            end if;
         end if;
         --  if s_param
      end loop;
      --  for ij
   end Fill_Sa;
   --  * Fill_Sa *
   --  *
   --  Get an s-parameter from a linked list and remove it.
   --  *

   function get_s_and_remove (index : integer; start : in out s_param) return s_param is
      Result_get_s_and_remove : s_param;
      i : integer;
      s : s_param;
   begin
      if index = 1
      then
         Result_get_s_and_remove := start;
         start := start.all.next_s;
      else
         for i in 1 .. index - 1
         loop
            if i = 1
            then
               s := start;
            else
               s := s.all.next_s;
            end if;
            if s = null
            then
               message (2) := "get_s_and_remove";
               shutdown;
            end if;
         end loop;
         --  For i
         Result_get_s_and_remove := s.all.next_s;
         s.all.next_s := s.all.next_s.all.next_s;
      end if;
      --  if index
      return Result_get_s_and_remove;
   end get_s_and_remove;
   --  get_c_con_and_remove
   --  *
   --  Get a connector from a linked list and remove it.
   --  *

   function get_c_and_remove (index : integer; start : in out conn) return conn is
      Result_get_c_and_remove : conn;
      i : integer;
      s : conn;
   begin
      if index = 1
      then
         Result_get_c_and_remove := start;
         start := start.all.next_con;
      else
         for i in 1 .. index - 1
         loop
            if i = 1
            then
               s := start;
            else
               s := s.all.next_con;
            end if;
            if s = null
            then
               message (2) := "get_c_and_remove";
               shutdown;
            end if;
         end loop;
         --  For i
         Result_get_c_and_remove := s.all.next_con;
         s.all.next_con := s.all.next_con.all.next_con;
      end if;
      --  if index
      return Result_get_c_and_remove;
   end get_c_and_remove;
   --  get_c_con_and_remove
   --  *
   --  Find k given tnet and tcon.
   --  *

   function get_kL_from_con (tnet : net; tcon : conn) return integer is
      Result_get_kL_from_con : integer;
      kL : integer;
      found : boolean;
      vcon : conn;
   begin
      found := false;
      kL := 0;
      vcon := null;
      loop
         if vcon = null
         then
            vcon := tnet.all.con_start;
         else
            vcon := vcon.all.next_con;
         end if;
         kL := kL + 1;
         if vcon = tcon
         then
            found := true;
         end if;
         exit when ((vcon.all.next_con = null) or else (found));
      end loop;
      if not (found)
      then
         message (2) := "get_kL_from_con";
         shutdown;
      end if;
      Result_get_kL_from_con := kL;
      return Result_get_kL_from_con;
   end get_kL_from_con;
   --  get_kL_from_con
   --  *
   --  Look for next joint to make connection.
   --  If no joint found then internal_joint_remaining:=false.
   --  *

   function internal_joint_remaining return boolean is
      Result_internal_joint_remaining : boolean;
      csize, size : integer;
      cNmate : net;
   begin
      Conk := null;
      cnet := net_start;
      csize := 1000;
      loop
         ccon := cnet.all.con_start;
         if (ccon /= null)
         then
            loop
               if ccon.all.port_type <= 0
               then
                  cNmate := ccon.all.mate.all.net;
                  if cNmate = cnet
                  then
                     size := cnet.all.number_of_con - 2;
                  else
                     size := cnet.all.number_of_con + cNmate.all.number_of_con - 2;
                  end if;
                  if betweeni (1, size, csize)
                  then
                     csize := size - 1;
                     Conk := ccon;
                  end if;
                  --  if size - found simpler net to remove
               end if;
               --  if ccon^.mate
               ccon := ccon.all.next_con;
               exit when (ccon = null);
            end loop;
         end if;
         cnet := cnet.all.next_net;
         exit when (cnet = null);
      end loop;
      if Conk /= null
      then
         netK := Conk.all.net;
         netL := Conk.all.mate.all.net;
         Result_internal_joint_remaining := true;
      else
         Result_internal_joint_remaining := false;
      end if;
      if No_mem_left
      then
         Result_internal_joint_remaining := false;
      end if;
      --  exit analysis if out of memory
      return Result_internal_joint_remaining;
   end internal_joint_remaining;
   --  internal_joint_remaining
   --  *
   --  Does connector belong to set for which
   --  s-parameters need to be calculated.
   --  *

   function calc_con (Conj : conn) return boolean is
      Result_calc_con : boolean;
   begin
      if ext_port (Conj)
      then
         Result_calc_con := inp (Conj.all.port_type);
      else
         Result_calc_con := true;
      end if;
      return Result_calc_con;
   end calc_con;
   --  * calc_con *
   --  *
   --  Join connectors from different networks.
   --  *

   procedure Join_Net is
      --  rc
      biL, bLL, akj, akk, aij, aik, start : s_param;
      sizea, sizeb, i, j, k, L : integer;
      num1, num2, num3, num : Tcomplex;
      ConL, Conj : conn;
   begin
      k := get_kL_from_con (netK, ConK);
      L := get_kL_from_con (netL, Conk.all.mate);
      Conk := get_c_and_remove (k, netK.all.con_start);
      ConL := get_c_and_remove (L, netL.all.con_start);
      akk := get_s_and_remove (k, Conk.all.s_start);
      bLL := get_s_and_remove (L, ConL.all.s_start);
      prp (num, akk.all.z.all.c, bLL.all.z.all.c);
      co (num3, 1.0, 0.0);
      di (num1, num3, num);
      rc (num2, num1);
      sizea := netK.all.number_of_con - 1;
      sizeb := netL.all.number_of_con - 1;
      prp (num1, bLL.all.z.all.c, num2);
      for j in 1 .. sizea
      loop
         if j = 1
         then
            Conj := netK.all.con_start;
         else
            Conj := Conj.all.next_con;
         end if;
         if calc_con (Conj)
         then
            akj := get_s_and_remove (k, Conj.all.s_start);
            prp (num, num1, akj.all.z.all.c);
            prp (num3, num2, akj.all.z.all.c);
            aij := Conj.all.s_start;
            aik := Conk.all.s_start;
            if (aij.all.z /= null)
            then
               supr (aij.all.z.all.c, aik.all.z.all.c, num);
            end if;
            for i in 2 .. sizea
            loop
               aij := aij.all.next_s;
               aik := aik.all.next_s;
               if (aij.all.z /= null)
               then
                  supr (aij.all.z.all.c, aik.all.z.all.c, num);
               end if;
            end loop;
            --  for i
            for i in 1 .. sizeb
            loop
               if i = 1
               then
                  biL := ConL.all.s_start;
               else
                  biL := biL.all.next_s;
               end if;
               new_s (aij.all.next_s);
               aij := aij.all.next_s;
               if (biL.all.z /= null)
               then
                  New_c (aij.all.z);
                  prp (aij.all.z.all.c, biL.all.z.all.c, num3);
               else
                  aij.all.z := null;
               end if;
            end loop;
            --  for i
            aij.all.next_s := null;
         end if;
         --  if calc conj
      end loop;
      --  end j
      if sizea = 0
      then
         netK.all.con_start := netL.all.con_start;
      else
         Conj.all.next_con := netL.all.con_start;
      end if;
      prp (num1, akk.all.z.all.c, num2);
      for j in 1 .. sizeb
      loop
         if j = 1
         then
            Conj := netL.all.con_start;
         else
            Conj := Conj.all.next_con;
         end if;
         if calc_con (Conj)
         then
            Conj.all.net := netK;
            akj := get_s_and_remove (L, Conj.all.s_start);
            prp (num, num1, akj.all.z.all.c);
            prp (num3, num2, akj.all.z.all.c);
            aij := Conj.all.s_start;
            aik := ConL.all.s_start;
            if (aij.all.z /= null)
            then
               supr (aij.all.z.all.c, aik.all.z.all.c, num);
            end if;
            for i in 2 .. sizeb
            loop
               aij := aij.all.next_s;
               aik := aik.all.next_s;
               if (aij.all.z /= null)
               then
                  supr (aij.all.z.all.c, aik.all.z.all.c, num);
               end if;
            end loop;
            --  for i
            for i in 1 .. sizea
            loop
               if i = 1
               then
                  biL := Conk.all.s_start;
                  new_s (start);
                  aij := start;
               else
                  biL := biL.all.next_s;
                  new_s (aij.all.next_s);
                  aij := aij.all.next_s;
               end if;
               aij.all.next_s := Conj.all.s_start;
               if (biL.all.z /= null)
               then
                  New_c (aij.all.z);
                  prp (aij.all.z.all.c, biL.all.z.all.c, num3);
               else
                  aij.all.z := null;
               end if;
            end loop;
            --  for i
            if sizea > 0
            then
               Conj.all.s_start := start;
            end if;
         end if;
         --  if conj
      end loop;
      --  for j
      dispose_net (netL);
      netK.all.number_of_con := sizea + sizeb;
   end Join_Net;
   --  join_net
   --  *
   --  Join connectors from the same networks.
   --  *

   procedure Reduce_Net is
      akj, akk, aij, aik, P2Ada_no_keyword_aLL, aiL, akL, aLk, aLj : s_param;
      num1, num2, num3, num4 : TComplex;
      ConL, Conj : conn;
      i, k, L : integer;
   begin
      k := get_kL_from_con (netK, Conk);
      L := get_kL_from_con (netK, Conk.all.mate);
      if k < L
      then
         i := k;
         k := L;
         L := i;
      end if;
      Conk := get_c_and_remove (k, netK.all.con_start);
      ConL := get_c_and_remove (L, netK.all.con_start);
      akk := get_s_and_remove (k, Conk.all.s_start);
      aLk := get_s_and_remove (L, Conk.all.s_start);
      akL := get_s_and_remove (k, ConL.all.s_start);
      P2Ada_no_keyword_aLL := get_s_and_remove (L, ConL.all.s_start);
      di (num3, co1, akL.all.z.all.c);
      di (num4, co1, aLK.all.z.all.c);
      prp (num2, num3, num4);
      prp (num3, P2Ada_no_keyword_aLL.all.z.all.c, akk.all.z.all.c);
      di (num4, num2, num3);
      rc (num1, num4);
      Conj := netK.all.con_start;
      while Conj /= null
      loop
         if calc_con (Conj)
         then
            akj := get_s_and_remove (k, Conj.all.s_start);
            aLj := get_s_and_remove (L, Conj.all.s_start);
            num4.r := 0.0;
            num4.i := 0.0;
            di (num3, co1, aLK.all.z.all.c);
            supr (num4, akj.all.z.all.c, num3);
            supr (num4, aLj.all.z.all.c, akk.all.z.all.c);
            prp (num2, num1, num4);
            num4.r := 0.0;
            num4.i := 0.0;
            di (num3, co1, akL.all.z.all.c);
            supr (num4, aLj.all.z.all.c, num3);
            supr (num4, akj.all.z.all.c, P2Ada_no_keyword_aLL.all.z.all.c);
            prp (num3, num1, num4);
            aij := Conj.all.s_start;
            aiL := ConL.all.s_start;
            aik := Conk.all.s_start;
            if aij.all.z /= null
            then
               supr (aij.all.z.all.c, aiL.all.z.all.c, num2);
               supr (aij.all.z.all.c, aik.all.z.all.c, num3);
            end if;
            while (aij.all.next_s /= null)
            loop
               aij := aij.all.next_s;
               aiL := aiL.all.next_s;
               aik := aik.all.next_s;
               if aij.all.z /= null
               then
                  supr (aij.all.z.all.c, aiL.all.z.all.c, num2);
                  supr (aij.all.z.all.c, aik.all.z.all.c, num3);
               end if;
            end loop;
            --  while aij
         end if;
         --  if calc_con conj
         Conj := Conj.all.next_con;
      end loop;
      --  Conj
      netK.all.number_of_con := netK.all.number_of_con - 2;
   end Reduce_Net;
   --  reduce net
   --  *
   --  Remove nodes in network with 2 ports.
   --  These require no connecting tee's or crosses
   --  for reduction. Makes connectors from one net
   --  mate up with the other.
   --  *

   procedure rem_node (tnet : net) is
      --  !* Dead code? tnet^.ports_connected=2 is not permitted by the call!
      tcon : conn;
      i, j : integer;
   begin
      ccon := tnet.all.con_start;
      if tnet.all.ports_connected = 2
      then
         --  2 port node connect to two ports
         for j in 1 .. 2
         loop
            if j = 1
            then
               ccon := tnet.all.con_start;
            else
               ccon := ccon.all.next_con;
            end if;
            for i in 1 .. 2
            loop
               if i = 1
               then
                  new_s (ccon.all.s_start);
                  c_s := ccon.all.s_start;
               else
                  new_s (c_s.all.next_s);
                  c_s := c_s.all.next_s;
               end if;
               new_c (c_s.all.z);
               if i /= j
               then
                  c_s.all.z.all.c.r := - 1.0;
               else
                  c_s.all.z.all.c.r := 0.0;
               end if;
               c_s.all.z.all.c.i := 0.0;
            end loop;
            --  for i
            c_s.all.next_s := null;
         end loop;
         --  j
      else
         if tnet.all.ports_connected > 0
         then
            --  if node is connected to port
            if ext_port (ccon)
            then
               tcon := ccon.all.next_con.all.mate;
               tcon.all.port_type := ccon.all.port_type;
            else
               tcon := ccon.all.mate;
               tcon.all.port_type := ccon.all.next_con.all.port_type;
            end if;
            tcon.all.mate := null;
            tcon.all.net.all.ports_connected := tnet.all.ports_connected;
         else
            ccon.all.mate.all.mate := ccon.all.next_con.all.mate;
            ccon.all.next_con.all.mate.all.mate := ccon.all.mate;
         end if;
         dispose_net (tnet);
      end if;
   end rem_node;
   --  * rem_node *
   --  *
   --  Set up frequency independent s-parameters for each network.
   --  Does opens, shorts, tee's, and crosses.
   --  *

   procedure Set_Up_Element (ports : integer) is
      i, j, jj : integer;
      pta : array (1 .. max_net_size) of integer;
      onlyinp, onlyout : array (1 .. max_net_size) of boolean;
   begin
      if ports = 1
      then
         --  open or short
         new_s (cnet.all.con_start.all.s_start);
         c_s := cnet.all.con_start.all.s_start;
         c_s.all.next_s := null;
         New_c (c_s.all.z);
         if cnet.all.grounded
         then
            --  short
            co (c_s.all.z.all.c, - one, 0.0);
         else
            co (c_s.all.z.all.c, one, 0.0);
         end if;
         --  open
      else
         jj := 1;
         for j in 1 .. ports
         loop
            --  check to see if input or output port
            if j = 1
            then
               ccon := cnet.all.con_start;
            else
               ccon := ccon.all.next_con;
            end if;
            if ext_port (ccon)
            then
               pta (jj) := ccon.all.port_type;
               onlyout (jj) := P2Ada_no_keyword_out (pta (jj)) and then not (inp (pta (jj)));
               onlyinp (jj) := inp (pta (jj)) and then not (P2Ada_no_keyword_out (pta (jj)));
               if not (P2Ada_no_keyword_out (pta (jj)) or else inp (pta (jj)))
               then
                  ccon := get_c_and_remove (jj, cnet.all.con_start);
                  cnet.all.number_of_con := cnet.all.number_of_con - 1;
                  jj := jj - 1;
               end if;
               --  if not(out..)
            else
               onlyout (jj) := false;
               onlyinp (jj) := false;
            end if;
            --  if ext
            jj := jj + 1;
         end loop;
         --  j
         for j in 1 .. cnet.all.number_of_con
         loop
            if j = 1
            then
               ccon := cnet.all.con_start;
            else
               ccon := ccon.all.next_con;
            end if;
            for i in 1 .. cnet.all.number_of_con
            loop
               if i = 1
               then
                  new_s (ccon.all.s_start);
                  c_s := ccon.all.s_start;
               else
                  new_s (c_s.all.next_s);
                  c_s := c_s.all.next_s;
               end if;
               if onlyinp (i) or else onlyout (j)
               then
                  c_s.all.z := null;
               else
                  new_c (c_s.all.z);
                  if cnet.all.node
                  then
                     c_s.all.z.all.c.i := 0.0;
                     if cnet.all.grounded
                     then
                        --  if grounded
                        if i = j
                        then
                           c_s.all.z.all.c.r := - one;
                        else
                           c_s.all.z.all.c.r := 0.0;
                        end if;
                        --  Tee or Cross
                     else
                        if i = j
                        then
                           c_s.all.z.all.c.r := one * ((2 / ports) - 1);
                        else
                           c_s.all.z.all.c.r := one * (2 / ports);
                        end if;
                     end if;
                     --  if grounded
                  end if;
               end if;
               --  if onlyinp
            end loop;
            --  i
            c_s.all.next_s := null;
         end loop;
         --  j
      end if;
      --  if 1 port
   end Set_Up_Element;
   --  * set_up_element *
   --  *
   --  Transfer s-parameters from parts to networks.
   --  *

   procedure Fill_Compts is
      --  Load s-param data into memory, at xpt=0 fill device s_ifile
      i, j, ii, jj : integer;
      v_s : s_param;
      coni, conj : conn;
   begin
      action := false;
      Pars_Compt_List;
      cnet := net_start;
      if (not (bad_compt))
      then
         loop
            if not (cnet.all.node) and then (cnet.all.number_of_con > 0)
            then
               coni := cnet.all.con_start;
               loop
                  ii := coni.all.conn_no;
                  conj := cnet.all.con_start;
                  c_s := coni.all.s_start;
                  loop
                     jj := conj.all.conn_no;
                     v_s := null;
                     if c_s.all.z /= null
                     then
                        for i in 1 .. cnet.all.com.all.number_of_con
                        loop
                           for j in 1 .. cnet.all.com.all.number_of_con
                           loop
                              if v_s = null
                              then
                                 v_s := cnet.all.com.all.s_begin;
                              else
                                 v_s := v_s.all.next_s;
                              end if;
                              if (ii = i) and then (jj = j)
                              then
                                 c_s.all.z.all.c.r := v_s.all.z.all.c.r;
                                 c_s.all.z.all.c.i := v_s.all.z.all.c.i;
                              end if;
                              --  if ii
                           end loop;
                        end loop;
                        --  for j
                     end if;
                     --  if c_s
                     conj := conj.all.next_con;
                     c_s := c_s.all.next_s;
                     exit when ((conj = null) or else (c_s = null));
                  end loop;
                  coni := coni.all.next_con;
                  exit when (coni = null);
               end loop;
            end if;
            --  if not(cnet^.node)
            cnet := cnet.all.next_net;
            exit when (cnet = null);
         end loop;
      end if;
   end Fill_Compts;
   --  * Fill_Compts *
   --  *
   --  Procedure for setting up nets
   --  *

   procedure set_up_net is
      i : integer;
   begin
      for i in 1 .. 2
      loop
         --  set_up_nets
         cnet := null;
         loop
            --  Loop through network, setting up connections
            if cnet = null
            then
               cnet := net_start;
            else
               cnet := cnet.all.next_net;
            end if;
            --  [P2Ada]: WITH instruction
            --  [P2Ada]: !Help! No type found -> add 'P2Ada_Var_6.' to fields
            declare P2Ada_Var_6 : < > renames cnet.all;
            begin
               if i = 1
               then
                  if ((number_of_con = 2) and then node and then not (grounded) and then (ports_connected /= 2))
                  then
                     rem_node (cnet);
                  end if;
               else
                  Set_Up_Element (number_of_con);
               end if;
            end;
            --  [P2Ada]: end of WITH
            exit when cnet.all.next_net = null;
         end loop;
      end loop;
      --  set_up_nets and do freq indept stuff
   end set_up_net;
   --  * set_up_net *
   --  *
   --  Find parameter in plot box, i.e. which s to calc at fd/df
   --  For do_time it is necessary to prompt for fd/df.
   --  Otherwise the number of Points are used.
   --  *

   procedure Get_s_and_f (do_time : boolean) is
      pt_end, pt_start, ij, i, j, code1, code2 : integer;
      real_npts : Long_Float;
      q_fac_string : file_string;
   begin
      ccompt := Points_compt;
      cx := ccompt.all.x_block;
      bad_compt := false;
      if do_time
      then
         --  * Use fd/df for time sweep *
         q_fac_string := input_string ("  Integer fd/df", "     <10>");
         if (q_fac_string = "")
         then
            --  fd/df
            q_fac := 10;
         else
            Val (q_fac_string, q_fac, code1);
            if (code1 /= 0)
            then
               bad_compt := true;
            end if;
         end if;
         if bad_compt
         then
            message (2) := "Invalid fd/df";
            return;
         end if;
         bad_compt := true;
         if q_fac < 1
         then
            message (1) := "fd/df too small";
            message (2) := "or negative";
            return;
         end if;
         --  normalized
         finc := design_freq / q_fac;
         if (sxmin / finc < 10000) and then (sxmax / finc < 10000)
         then
            if abs (sxmin / finc - Round (sxmin / finc)) < 0.001
            then
               pt_start := Round (sxmin / finc);
            else
               pt_start := Integer (sxmin / finc) + 1;
            end if;
            fmin := finc * pt_start;
            pt_end := Integer (sxmax / finc);
         else
            message (2) := "fd/df too large";
            return;
         end if;
         npts := pt_end - pt_start;
         if npts < 0
         then
            message (2) := "fd/df too small";
            return;
         end if;
         if npts > ptmax
         then
            message (2) := "fd/df too large";
            return;
         end if;
         --  if do_time
         --  * Use Points for frequency or other sweep
      else
         real_npts := Get_Real (Points_compt, 1);
         if (Integer (real_npts) > ptmax)
         then
            bad_compt := true;
         else
            npts := Integer (real_npts) - 1;
         end if;
         --  number of points
         if bad_compt or else (npts < 0)
         then
            bad_compt := true;
            message (1) := "Invalid number";
            message (2) := "of points";
            return;
         end if;
         --  if npts=0 then plot a point at fmin
         fmin := sxmin;
         if (npts /= 0)
         then
            finc := (sxmax - sxmin) / npts;
         end if;
         --  This to allow plotting of 1 point i.e. npts=0
      end if;
      --  if do_time
      for ij in 1 .. min_ports
      loop
         inp (ij) := false;
         P2Ada_no_keyword_out (ij) := false;
      end loop;
      for ij in 1 .. max_params
      loop
         --  [P2Ada]: WITH instruction
         --  [P2Ada]: !Help! No type found -> add 'P2Ada_Var_7.' to fields
         declare P2Ada_Var_7 : < > renames s_param_table (ij).all;
         begin
            calc := false;
            if (descript'length) >= 3
            then
               Val (descript (2), i, code1);
               Val (descript (3), j, code2);
               if (code1 = 0) and then (code2 = 0)
               then
                  if betweeni (1, i, min_ports) and then betweeni (1, j, min_ports)
                  then
                     si (ij) := i;
                     sj (ij) := j;
                     if portnet (i).all.node and then portnet (j).all.node
                     then
                        inp (j) := true;
                        P2Ada_no_keyword_out (i) := true;
                        calc := true;
                        bad_compt := false;
                     end if;
                     --  if port
                  end if;
               end if;
               --  if code and between
            end if;
            --  if length
         end;
         --  [P2Ada]: end of WITH
      end loop;
      --  for ij and with
      if bad_compt
      then
         ccompt := rho_fac_compt;
         move_cursor (0, 1);
         message (1) := "No pair of Sij";
         message (2) := "correspond to";
         message (3) := "connected ports";
         return;
      end if;
   end Get_s_and_f;
   --  * Get_s_and_f *
   --  *
   --  Main procedure for directing the analysis.
   --  Pass do_time on to get_s_and_f in order to
   --  return FFT parameters based on q_fac.
   --  *

   procedure Analysis (do_time, P2Ada_no_keyword_out : boolean) is
      --  [BP2P]: Label "100001" Was "exit_analysis"
      --  netmem        : LONGINT;
      old_net : net;
      old_net_start : net;
      net_end_ptr1 : marker;
      net_end_ptr2 : marker;
      ptrall : marker;
      ptrvar : marker;
      ptranalysis : marker;
      MemError : BOOLEAN;
   begin
      filled_OK := false;
      MemError := FALSE;
      if net_start = null
      then
         message (1) := "No circuit";
         message (2) := "to analyze";
         write_message;
      else
         Get_s_and_f (do_time);
         if bad_compt
         then
            --  Save initial Markers
            --  Mark original memory setting
            --  netmem:= MemAvail;
            --  Copy original network
            write_message;
            cx := ccompt.all.x_block;
         else
            filled_OK := true;
            TextCol (lightgray);
            GotoXY (xmin (6), ymin (6) + 1);
            Put (" Press h to halt ");
            TextCol (white);
            GotoXY (xmin (6) + 7, ymin (6) + 1);
            Put ('h');
            old_net := cnet;
            old_net_start := net_start;
            Mark_Mem (ptrall);
            Init_Marker (net_end_ptr1);
            copy_networks (net_beg, ptrall, net_end_ptr1);
            if not Marked (net_end_ptr1)
            then
               --  [BP2P]: Label "100001" Was "exit_analysis"
               MemError := TRUE;
               goto LABEL_100001;
            end if;
            --  set up nets for analysis
            --  IF (MemAvail < (netmem - MemAvail + npts * 256)) THEN
            --  BEGIN
            --  MemError:= TRUE;
            --  GOTO Exit_Analysis;
            --  END;
            --  Initialize markers for copy of new network
            --  main analysis loop
            set_up_net;
            Init_Marker (net_end_ptr2);
            Mark_Mem (ptranalysis);
            for xpt in 0 .. npts
            loop
               if Alt_Sweep
               then
                  --  Force parts to use fd
                  --  use freq data for alt parameter
                  freq := design_freq;
                  x_sweep.Load_Data (fmin + xpt * finc);
               else
                  if (xpt = npts) and then (npts /= 0)
                  then
                     freq := sxmax;
                  else
                     freq := fmin + xpt * finc;
                  end if;
                  --  These are normalized to the window
               end if;
               --  * For alternate sweep freq:=design_freq and finc=0 *
               --  make copy of network for analysis
               copy_networks (net_beg, ptranalysis, net_end_ptr2);
               if (not Marked (net_end_ptr2))
               then
                  --  [BP2P]: Label "100001" Was "Exit_Analysis"
                  MemError := TRUE;
                  goto LABEL_100001;
               end if;
               --  parts calculation and at xpt=0 fills device file s_ifile interpolation data
               Fill_compts;
               if (No_mem_left)
               then
                  --  [BP2P]: Label "100001" Was "exit_analysis"
                  goto LABEL_100001;
               end if;
               --  check for 16 bytes
               if (bad_compt)
               then
                  --  [BP2P]: Label "100001" Was "Exit_Analysis"
                  filled_OK := FALSE;
                  goto LABEL_100001;
               else
                  while internal_joint_remaining
                  loop
                     --  loop over joints
                     if netK = netL
                     then
                        --  join connectors on same net
                        reduce_net;
                     else
                        join_net;
                     end if;
                  end loop;
                  --  join two nets
               end if;
               --  if bad_compt
               if Alt_Sweep
               then
                  freq := fmin + xpt * finc;
               end if;
               --  Restore freq for use in linked lists
               --  uses freq, calls write_freqO
               --  free memory used for analysis
               --  -> not for xpt = 0; keep device file's interpolation data
               Fill_Sa (P2Ada_no_keyword_out);
               if (xpt /= 0)
               then
                  Release_Mem (ptrvar);
               else
                  Mark_Mem (ptrvar);
               end if;
               --  check for abort
               if keypressed
               then
                  --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
                  chs := ReadKey;
                  if chs and ('h' | 'H' => True, others => False)
                  then
                     --  [BP2P]: Label "100001" Was "exit_analysis"
                     filled_OK := false;
                     erase_message;
                     message (2) := "      HALT       ";
                     write_message;
                     goto LABEL_100001;
                  end if;
                  beep;
               end if;
               --  if keypreseed
            end loop;
            --  for xpt:= 0 to npts
            <<LABEL_100001>>
            if (No_mem_left or else MemError)
            then
               filled_OK := false;
               erase_message;
               message (1) := " Circuit is too  ";
               message (2) := " large for Puff  ";
               message (3) := "   to analyze    ";
               write_message;
            end if;
            --  Restore network
            --  Release all network copies
            --  Restore initial markers
            copy_networks (net_beg, ptrall, net_end_ptr1);
            Release_Mem (ptrall);
            cnet := old_net;
            net_start := old_net_start;
         end if;
         --  if bad_compt
      end if;
      --  if cnet
   end Analysis;
   --  * Analysis *
   --  *
   --  Move marker on Smith chart and rectangular plot.
   --  Move_marker(+/- 1) is invoked from Plot2
   --  by Page-Up and Page-Down keys.
   --  Move_marker(0) is called by plot_manager.
   --  *

   procedure move_marker (xi : integer) is
      i, ij, k, kk, nb, sfreq : integer;
   begin
      if marker_OK
      then
         for ij in 1 .. max_params
         loop
            if s_param_table (ij).all.calc
            then
               case xi is
                  when 0 =>
                     if plot_des (ij) = null
                     then
                        --  point for cursor placement
                        xpt := 0;
                     else
                        c_plot (ij) := plot_des (ij);
                        xpt := Round ((design_freq - fmin) / finc);
                        if not (betweeni (0, xpt, npts))
                        then
                           xpt := 0;
                        end if;
                     end if;
                     if xpt = 0
                     then
                        c_plot (ij) := plot_start (ij);
                     end if;
                     --  0:
                  when 1 =>
                     if c_plot (ij) = plot_end (ij)
                     then
                        c_plot (ij) := plot_start (ij);
                     else
                        c_plot (ij) := c_plot (ij).all.next_p;
                     end if;
                  when - 1 =>
                     if c_plot (ij) = plot_start (ij)
                     then
                        c_plot (ij) := plot_end (ij);
                     else
                        c_plot (ij) := c_plot (ij).all.prev_p;
                     end if;
                  when others =>
                     --  [P2Ada]: no otherwise / else in Pascal
                     null;
               end case;
            end if;
         end loop;
         --  case xi
         if xi = 0
         then
            for i in 1 .. 2 * max_params
            loop
               box_filled (i) := false;
            end loop;
         else
            xpt := xpt + xi;
         end if;
         if xpt > npts
         then
            xpt := 0;
         end if;
         if xpt < 0
         then
            xpt := npts;
         end if;
         if (xi /= 0)
         then
            Erase_Message;
         end if;
         Write_FreqO;
         sfreq := xmin (8) + Round ((freq - sxmin) * sfx1);
         for k in 1 .. 3
         loop
            for ij in 1 .. max_params
            loop
               if s_param_table (ij).all.calc
               then
                  case k is
                     when 1 =>
                        restore_boxO (ij);
                     when 2 =>
                        box_filled (ij) := false;
                        box_filled (ij + max_params) := false;
                        if c_plot (ij).all.filled
                        then
                           calc_posO (c_plot (ij).all.x, c_plot (ij).all.y, 0, 1, sfreq, false);
                           if spline_in_smith
                           then
                              move_boxO (spx, spy, ij);
                           end if;
                           if not (Large_Smith)
                           then
                              if spline_in_rect
                              then
                                 move_boxO (sfreq, spp, ij + max_params);
                              end if;
                           end if;
                        end if;
                        --  2:
                     when 3 =>
                        for kk in 0 .. 1
                        loop
                           nb := ij + kk * max_params;
                           if box_filled (nb)
                           then
                              pattern (box_dot (1, nb), box_dot (2, nb), ij, 128);
                           end if;
                        end loop;
                        --  kk
                        if c_plot (ij).all.filled
                        then
                           write_sO (ij);
                        end if;
                        --  3 :
                     when others =>
                        --  [P2Ada]: no otherwise / else in Pascal
                        null;
                  end case;
               end if;
            end loop;
         end loop;
         --  case
      end if;
      --  if do time
   end move_marker;
   --  move_marker

   procedure show_real is
      ij : integer;
      d, r, i : Long_Float;
      u : Character;
   begin
      if (marker_ok)
      then
         for ij in 1 .. max_params
         loop
            if (s_param_table (ij) = ccompt)
            then
               if ((ccompt.all.calc) and then (c_plot (ij).all.filled) and then ((ccompt.all.descript = "S11") or else (ccompt.all.descript = "S22") or else (ccompt.all.descript = "S33") or else (ccompt.all.descript = "S44")))
               then
                  --  [P2Ada]: WITH instruction
                  --  [P2Ada]: !Help! No type found -> add 'P2Ada_Var_8.' to fields
                  declare P2Ada_Var_8 : < > renames c_plot (ij).all;
                  begin
                     GotoXY (xmin (6) + 2, ymin (6));
                     if (admit_chart)
                     then
                        d := ((1 + x) ** 2) + ((y) ** 2);
                        r := (1 - ((x) ** 2) - ((y) ** 2));
                        if (r = 0.0)
                        then
                           Put ("Rp:");
                        else
                           r := (d * z0 / r);
                           if (abs (r) <= 1e + 9)
                           then
                              Put ("Rp:");
                              Put (r, 10, 3, 0);
                              Put (' ');
                              Put (Omega);
                           else
                              Put ("Rp:         ");
                              Put (infin);
                              Put (ity);
                           end if;
                        end if;
                        i := 2 * y;
                        GotoXY (xmin (6) + 2, ymin (6) + 1);
                        if (i = 0.0)
                        then
                           Put ("Xp:");
                        else
                           i := (d * z0 / i);
                           if (abs (i) <= 1e + 9)
                           then
                              Put ("Xp:");
                              Put (i, 10, 3, 0);
                              Put (' ');
                              Put (Omega);
                           else
                              Put ("Xp:         ");
                              Put (infin);
                              Put (ity);
                           end if;
                        end if;
                     else
                        d := ((1 - x) ** 2) + ((y) ** 2);
                        if (d = 0.0)
                        then
                           d := 5e - 324;
                        end if;
                        r := ((1 - ((x) ** 2) - ((y) ** 2)) * z0 / d);
                        i := (2 * y * z0 / d);
                        if (abs (r) <= 1e + 9)
                        then
                           if (abs (i) <= 1e + 9)
                           then
                              Put ("Rs:");
                              Put (r, 10, 3, 0);
                              Put (' ');
                              Put (Omega);
                              GotoXY (xmin (6) + 2, ymin (6) + 1);
                              Put ("Xs:");
                              Put (i, 10, 3, 0);
                              Put (' ');
                              Put (Omega);
                           else
                              Put ("Rs:");
                              GotoXY (xmin (6) + 2, ymin (6) + 1);
                              Put ("Xs:         ");
                              Put (infin);
                              Put (ity);
                           end if;
                        else
                           if (abs (i) <= 1e + 9)
                           then
                              Put ("Rs:         ");
                              Put (infin);
                              Put (ity);
                              GotoXY (xmin (6) + 2, ymin (6) + 1);
                              Put ("Xs:");
                           else
                              Put ("Rs:         ");
                              Put (infin);
                              Put (ity);
                              GotoXY (xmin (6) + 2, ymin (6) + 1);
                              Put ("Xs:         ");
                              Put (infin);
                              Put (ity);
                           end if;
                        end if;
                     end if;
                     d := fmin + xpt * finc;
                     case freq_prefix is
                        when 'G' =>
                           d := d * 1000000000.0;
                        when 'M' =>
                           d := d * 1000000.0;
                        when 'k' =>
                           d := d * 1000.0;
                        when others =>
                           RunError (0);
                     end case;
                     GotoXY (xmin (6) + 2, ymin (6) + 2);
                     if (i /= 0.0)
                     then
                        if (i > 0)
                        then
                           Put ("L :");
                           d := (i / (2 * pi * d));
                           u := 'H';
                        else
                           Put ("C :");
                           d := (- 1 / (2 * pi * d * i));
                           u := 'F';
                        end if;
                        GotoXY (xmin (6) + 5, ymin (6) + 2);
                        if (d > 1e + 9)
                        then
                           Put ("        ");
                           Put (infin);
                           Put (ity);
                        else
                           if (d >= 1.0)
                           then
                              Put (d, 10, 3, 0);
                              Put (' ');
                              Put (u);
                           else
                              if (d >= 0.001)
                              then
                                 Put (d * 1000.0, 10, 3, 0);
                                 Put (" m");
                                 Put (u);
                              else
                                 if (d >= 0.000001)
                                 then
                                    Put (d * 1000000.0, 10, 3, 0);
                                    Put (" æ");
                                    Put (u);
                                 else
                                    if (d >= 0.000000001)
                                    then
                                       Put (d * 1000000000.0, 10, 3, 0);
                                       Put (" n");
                                       Put (u);
                                    else
                                       Put (d * 1000000000000.0, 10, 3, 0);
                                       Put (" p");
                                       Put (u);
                                    end if;
                                 end if;
                              end if;
                           end if;
                        end if;
                     end if;
                  end;
                  --  [P2Ada]: end of WITH
                  --  if .. then .. with
               else
                  beep;
               end if;
            end if;
            --  if s_param...
         end loop;
         --  for ...
         --  if marker_ok
      else
         beep;
      end if;
   end show_real;
   --  *
   --  Main procedure for directing anlaysis followed by plotting.
   --  *

   procedure Plot_Manager (do_analysis, clear_plot, do_time, boxes, P2Ada_no_keyword_out : boolean) is
      --  used to erase any residual S's
      ticko : integer;
   begin
      ticko := GetTimerTicks;
      erase_message;
      move_cursor (0, 0);
      Get_Coords;
      if bad_compt
      then
         write_message;
         cx := ccompt.all.x_block;
         filled_OK := false;
      else
         Pick_Smith (admit_chart);
         if not (Large_Smith)
         then
            Draw_Graph (xmin (8), ymin (8), xmax (8), ymax (8), false);
         end if;
         cx := ccompt.all.x_block;
         if not (clear_plot) and then filled_OK
         then
            Smith_and_Magplot (true, true, true);
            move_marker (0);
         end if;
         if do_analysis
         then
            Analysis (do_time, P2Ada_no_keyword_out);
         end if;
         --  Start Analysis
         if filled_OK
         then
            --  plot spline points after analysis
            --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
            marker_OK := true;
            Smith_and_Magplot (false, false, boxes);
            ticko := GetTimerTicks - ticko;
            if (do_analysis and then not (key and ('h' | 'H' => True, others => False)))
            then
               --  center text in message box
               --  was ticko/18.2:8:1
               erase_message;
               TextCol (lightgray);
               Gotoxy ((xmin (6) + xmax (6) - 16) / 2, ymin (6) + 1);
               Put ("Time ");
               Put (ticko / 18.2, 6, 1, 0);
               Put (" secs");
               beep;
            end if;
            move_marker (0);
            if demo_mode
            then
               rcdelay (300);
            end if;
         else
            marker_OK := false;
         end if;
      end if;
      --  if bad_compt
   end Plot_Manager;
   --  * Plot_Manager
   --  *
   --  Erase circuit board. Redraw if read_kbd=false.
   --  Check to see if device files need to be re-copied into memory.
   --  Very significant memory management control here
   --  involving the net_beg pointer.
   --  Called by:
   --  Redraw_Circuit,
   --  Read_Net (after or if not board_read),
   --  Lpayout1 (if Ctrl_e),
   --  Parts3 (if Ctrl_e)
   --  *

   procedure Erase_Circuit is
   begin
      erase_message;
      if compt1 /= null
      then
         write_compt (lightgray, compt1);
      end if;
      compt1 := part_start;
      if (Marked (net_beg))
      then
         Release_Mem (net_beg);
      end if;
      --  release networks memory
      --  set_up for redraw
      Mark_Mem (net_beg);
      key_i := 0;
      if read_kbd
      then
         --  erase key_list
         filled_OK := false;
         circuit_changed := false;
         Board_Changed := false;
         marker_OK := false;
         key_end := 0;
         Extra_Parts_Used := false;
      end if;
      net_start := null;
      cnet := null;
      Draw_Circuit;
   end Erase_Circuit;
   --  Erase_Circuit
   --  *
   --  Activated by the <Tab> key, this allows
   --  re-plotting for under a new smith chart.
   --  *

   procedure Toggle_Smith_and_Plot is
      --  Toggle Smith type
      --  used to erase any residual S's
   begin
      admit_chart := not (admit_chart);
      Erase_message;
      move_cursor (0, 0);
      Get_Coords;
      if bad_compt
      then
         write_message;
         cx := ccompt.all.x_block;
         filled_OK := false;
      else
         Pick_Smith (admit_chart);
         if not (Large_Smith)
         then
            Draw_Graph (xmin (8), ymin (8), xmax (8), ymax (8), false);
         end if;
         cx := ccompt.all.x_block;
         if filled_OK
         then
            Smith_and_Magplot (false, true, true);
            move_marker (0);
         end if;
      end if;
   end Toggle_Smith_and_Plot;
   --  * Toggle_Smith_and_Plot *
   --  *
   --  Set up for circuit redraw.
   --  *

   procedure Redraw_Circuit is
   begin
      read_kbd := false;
      Erase_Circuit;
      key_o := key;
      key := F1;
      GotoXY (checking_position (1), checking_position (2));
      TextCol (lightred);
      if not (demo_mode)
      then
         Put ("Checking Circuit.");
      end if;
   end Redraw_Circuit;
   --  * Redraw_Circuit *
   --  *
   --  Change linked list of coordinates to that required for
   --  Large Smith chart.
   --  *

   procedure Large_Smith_Coords is
      --  Advance coord_start pointer to fmin position
      --  Change wrap-around point
   begin
      coord_start := fmin_ptr;
      fmin_ptr.all.prev_compt := s_param_table (4);
      s_param_table (4).all.next_compt := fmin_ptr;
   end Large_Smith_Coords;
   --  * Large_Smith_Coords *
   --  *
   --  Change linked list of coordinates to that required for
   --  small Smith chart.
   --  *

   procedure Small_Smith_Coords is
      --  return coord_start pointer to dBmax
      --  Change wrap-around point
   begin
      coord_start := dBmax_ptr;
      fmin_ptr.all.prev_compt := dBmax_ptr.all.next_compt;
      s_param_table (4).all.next_compt := dBmax_ptr;
   end Small_Smith_Coords;
   --  * Small_Smith_Coords *
   --  *
   --  Change parameters to enlarge/shrink Smith chart.
   --  *

   procedure Toggle_Large_Smith (return_key : Character) is
   begin
      Large_Smith := not (Large_Smith);
      if not (Large_Smith)
      then
         --  make small Smith
         --  Erase Large Smith region
         --  Setup circuit re-draw and return to F2
         --  Make large Smith
         --  Move if invalid cursor position
         Small_Smith_Coords;
         clear_window_gfx (xmin (10), ymin (10), xmax (10), ymax (10));
         Screen_Plan;
         Draw_Graph (xmin (8), ymin (8), xmax (8), ymax (8), false);
         key := return_key;
         Redraw_Circuit;
         Write_File_Name (puff_file);
      else
         Large_Smith_Coords;
         Screen_Plan;
         if (ccompt = dBmax_ptr) or else (ccompt = dBmax_ptr.all.next_compt)
         then
            ccompt := Points_compt;
            cx := ccompt.all.x_block;
         end if;
      end if;
      Pick_Smith (admit_chart);
      if Large_Smith
      then
         Write_BigSmith_Coordinates;
      end if;
      if filled_OK
      then
         Smith_and_Magplot (false, true, true);
         move_marker (0);
      end if;
   end Toggle_Large_Smith;
   --  * Toggle_Large_Smith *
   --  *
   --  Read in xxx.puf file.
   --  *

   procedure Read_Net (fname : in out file_string; init_graphics : boolean) is
      char1, char2 : Character;
      file_read, bad_file : boolean;
   begin
      file_read := false;
      marker_OK := false;
      filled_OK := false;
      bad_file := false;
      if (pos ('.', fname) = 0) and then (fname /= "")
      then
         fname := fname + ".puf";
      end if;
      if fileexists (true, net_file, fname)
      then
         --  [P2Ada]: !Help! Maybe (file,...) here
         Get (net_file);
         Get (char1);
         loop
            if char1 = Character'Val (13)
            then
               --  [P2Ada]: !Help! Maybe (file,...) here
               --  Don't skip 2 lines on CR's
               --  [P2Ada]: !Help! Maybe (file,...) here
               --  [P2Ada]: !Help! Maybe (file)
               Get (net_file);
               Get (char2);
            else
               Get (net_file);
               Get (char2);
               Skip_Line;
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
                     bad_compt := false;
                     rho_fac := get_real (rho_fac_compt, 1);
                     if bad_compt or else (rho_fac <= 0.0)
                     then
                        rho_fac := 1;
                        rho_fac_compt.all.descript := "Smith radius 1.0";
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
                        --  * readln(net_file); *
                        --  else advance a line
                        --  * read(net_file,char1); *
                        --  ! Should have character after backslash
                        Message (1) := "Improper";
                        Message (2) := "Puff file";
                        Write_Message;
                        bad_file := true;
                        board_read := false;
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
               Get (net_file);
               Get (char1);
            end if;
            --  look for '\' on this line
            exit when bad_file or else End_of_File (net_file);
         end loop;
         Close (net_file);
         file_read := true;
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
         file_read := true;
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
   --  * Read_Net *
   --  *
   --  Routine for saving .puf file.
   --  *

   procedure Save_Net is
      --  erase residual s-parameters
      fname : file_string;
      drive : integer;
   begin
      Move_Cursor (0, 0);
      ccompt := Points_compt;
      cx := ccompt.all.x_block;
      fname := input_string ("File to save:", '<' + puff_file + '>');
      if fname = ""
      then
         fname := puff_file;
      end if;
      --  Default: save under current file name
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
            fname := fname + ".puf";
         end if;
         --  $I-
         --  $I+
         Assign (net_file, fname);
         Rewrite (net_file);
         if IOresult = 0
         then
            puff_file := fname;
            if not (Large_Smith)
            then
               Write_File_Name (fname);
            end if;
            save_boardO;
            save_keyO;
            save_partsO;
            save_s_paramsO;
            save_circuitO;
            close (net_file);
            erase_message;
         else
            message (1) := "Invalid filename";
            write_message;
         end if;
         --  if IOresult
      end if;
      --  if enough
   end Save_Net;
   --  * Save_Net *
   --  *
   --  Check that on exit Esc key was not accidently pressed.
   --  *

   procedure check_esc is
      --  so cursor doesn't blink
      tcompt : compt;
   begin
      message (1) := "Exit? Type Esc to";
      message (2) := "confirm, or other";
      message (3) := "  key to resume  ";
      write_message;
      tcompt := ccompt;
      ccompt := null;
      loop
         get_key;
         exit when key /= screenresize;
      end loop;
      if key /= Esc
      then
         key := not_esc;
      end if;
      ccompt := tcompt;
      erase_message;
   end check_esc;
   --  check_esc
   --  *
   --  Toggle help window in appropriate area of screen.
   --  help_displayed is a global variable.
   --  *

   procedure Toggle_Help_Window is
   begin
      if help_displayed
      then
         if (window_number = 4)
         then
            if Large_Parts
            then
               --  Write Large parts list under board window
               Write_Expanded_Parts;
               Write_Board_Parameters;
               Highlight_Window;
            else
               Write_Parts_ListO;
            end if;
            --  help window was over parts
         else
            if Large_Parts
            then
               --  help window was over board
               Write_Expanded_Parts;
            else
               Write_Board_Parameters;
            end if;
         end if;
      else
         if (window_number = 4)
         then
            --  Different area then write_commands
            Board_Help_Window;
         else
            if (window_number = 3)
            then
               if ((Character'Pos (ccompt.all.descript (1)) > 106))
               then
                  --  goto top if part>'i'
                  ccompt := part_start;
                  cx := ccompt.all.x_block;
               end if;
               --  write commands over Board window
               Write_Commands;
            else
               Write_Commands;
            end if;
         end if;
         --  write commands over Board window
      end if;
      help_displayed := not (help_displayed);
   end Toggle_Help_Window;
   --  *
   --  Toggle between small and large Parts Lists.
   --  *

   procedure Toggle_Parts_Lists is
   begin
      if Extra_Parts_Used
      then
         erase_message;
         Message (1) := "Cannot Toggle.";
         Message (2) := "Extra parts used";
         Message (3) := "in Layout.";
         Write_Message;
      else
         if Large_Parts
         then
            --  Clear area
            --  Write Board and small Parts windows
            --  return pointer to first part
            clear_window (xmin (3) - 1, ymin (3) - 1, xmax (5) + 1, ymax (5) + 1);
            Write_Board_Parameters;
            Write_Parts_ListO;
            Highlight_Window;
            ccompt := part_start;
            cx := part_start.all.x_block;
         else
            Write_Expanded_Parts;
         end if;
         Large_Parts := not (Large_Parts);
      end if;
      --  extra parts
   end Toggle_Parts_Lists;
   --  * Toggle_Parts_Lists *
   --  *
   --  Procedure for directing time domain functions
   --  called from the Plot2 window.
   --  *

   procedure Time_Domain_Manager is
   begin
      Erase_Message;
      if Alt_Sweep
      then
         Message (1) := "Frequency";
         Message (2) := "sweep needed";
         Message (3) := "for time plot";
         Write_Message;
      else
         if Large_Smith
         then
            --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
            --  time-analyze
            --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
            Message (1) := "Unavailable";
            Message (2) := "with large";
            Message (3) := "Smith chart";
            Write_Message;
         else
            step_fn := key and ('s' | 'S' => True, others => False);
            Plot_Manager (true, true, true, false, true);
            if (filled_OK and then not (key and ('h' | 'H' => True, others => False)))
            then
               Time_Response;
            end if;
            --  do inverse FFT and time plot
         end if;
      end if;
   end Time_Domain_Manager;
   --  * Time_Domain_Manager *
   --  *
   --  Process resizing of the "screen" (i.e., the X11 window under Linux)
   --  *

   procedure Screen_Resize is
      --  update some stored coordinates in the F4 window
      --  update some stored coordinates in the F2 window
      --  need to call this to update the board size in other variables than the x/ymin/max arrays
      --  need to call this to update the magnitude plot vertical scale factor
      tmp : compt;
   begin
      Small_Smith_Coords;
      Screen_Plan;
      clear_window_gfx (xmin (12), ymin (12), xmax (12), ymax (12));
      tmp := ccompt;
      Set_Up_Board;
      Update_KeyO_locations;
      ccompt := tmp;
      Fresh_Dimensions;
      Get_Coords;
      if (not Large_Smith)
      then
         Draw_Circuit;
         Write_File_Name (puff_file);
      end if;
      Write_Board_Parameters;
      Write_Parts_ListO;
      Write_Message;
      Make_Text_Border (xmin (6) - 1, ymin (6) - 1, xmax (6) + 1, ymax (6) + 1, LightRed, true);
      Make_Text_Border (xmin (2) - 1, ymin (2) - 1, xmax (2) + 1, ymax (2) + 1, Green, true);
      write_compt (col_window (2), window_f (2));
      if (Large_Smith)
      then
         Large_Smith_Coords;
      end if;
      Plot_Manager (false, true, false, true, true);
   end Screen_Resize;
   --  * Screen_Resize *
   --  *
   --  Procedure for directing circuit drawing functions in
   --  the circuit window.
   --  *

   procedure Layout1 is
   begin
      missing_part := false;
      write_compt (white, compt1);
      loop
         Get_Key;
         if (key /= F3) and then read_kbd
         then
            erase_message;
         end if;
         case key is
            --  these do not effect the keylist
            when Ctrl_e =>
               Erase_Circuit;
               write_compt (white, compt1);
            when Esc =>
               check_esc;
            when F5 =>
               Change_Bk_Color;
            when F10 =>
               Toggle_Help_Window;
            when screenresize =>
               Screen_Resize;
            when others =>
               begin
                  --  HKMP bad
                  update_key := read_kbd;
                  case key is
                     when right_arrow =>
                        move_net (2, 1);
                     when left_arrow =>
                        move_net (4, 1);
                     when down_arrow =>
                        move_net (8, 1);
                     when up_arrow =>
                        move_net (1, 1);
                     when sh_right =>
                        move_net (2, 0);
                     when sh_left =>
                        move_net (4, 0);
                     when sh_down | Mu =>
                        move_net (8, 0);
                     when sh_up =>
                        move_net (1, 0);
                     when sh_1 =>
                        join_port (1, 0);
                     when sh_2 =>
                        join_port (2, 0);
                     when sh_3 =>
                        join_port (3, 0);
                     when sh_4 =>
                        join_port (4, 0);
                     when 'a' .. 'r' | 'A' .. 'R' =>
                        choose_part (key);
                     when '1' .. '4' =>
                        join_port (Character'Pos (key) - Character'Pos ('1') + 1, 1);
                     when '=' =>
                        ground_node;
                     when '+' =>
                        unground;
                     when Ctrl_n =>
                        snapO;
                     when others =>
                        begin
                           --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
                           update_key := false;
                           if not (key and (F1 .. F4 | screenresize => True, others => False))
                           then
                              beep;
                           end if;
                        end;
                  end case;
                  --  case key
                  if update_key
                  then
                     update_key_list (node_number);
                  end if;
               end;
         end case;
         --  case key
         --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
         dx_dyO;
         write_message;
         exit when key and (F2 .. F4 | Esc => True, others => False);
      end loop;
      write_compt (lightgray, compt1);
      if help_displayed
      then
         Write_Board_Parameters;
      end if;
   end Layout1;
   --  * Layout1 *
   --  *
   --  Procedure for directing plotting routines in the Plot window.
   --  *

   procedure Plot2 is
   begin
      ccompt := Points_compt;
      cx := ccompt.all.x_block;
      previous_key := ' ';
      loop
         Get_Key;
         case key is
            when '0' .. '9' | '.' | ' ' | '-' | '+' =>
               add_char (ccompt);
            when Del =>
               del_char (ccompt);
            when backspace =>
               back_char (ccompt);
            when Ins =>
               insert_key := not (insert_key);
            when Up_Arrow =>
               move_cursor (0, - 1);
            when Down_arrow =>
               move_cursor (0, 1);
            when Left_arrow =>
               move_cursor (- 1, 0);
            when Right_arrow =>
               move_cursor (1, 0);
            when PgDn =>
               move_marker (- 1);
            when PgUp =>
               move_marker (+ 1);
            when '=' =>
               show_real;
            when 'i' | 'I' | 's' | 'S' =>
               Time_Domain_Manager;
            when Ctrl_s =>
               if Large_Smith
               then
                  Small_Smith_Coords;
               end if;
               Save_Net;
               if Large_Smith
               then
                  Large_Smith_Coords;
               end if;
            when Ctrl_a =>
               if Art_Form = 2
               then
                  Make_HPGL_file;
               else
                  Printer_Artwork;
               end if;
            when Ctrl_p =>
               --  replot
               Plot_manager (true, false, false, true, true);
            when 'p' | 'P' =>
               --  analyze
               Plot_manager (true, true, false, false, true);
            when 'q' | 'Q' =>
               Plot_Manager (true, true, false, false, false);
            when Ctrl_q =>
               Plot_Manager (true, false, false, true, false);
            when Tab =>
               Toggle_Smith_and_Plot;
            --  Alt_s
            when Mu =>
               Toggle_Large_Smith (F2);
            when screenresize =>
               Screen_Resize;
            when F5 =>
               Change_Bk_Color;
            when F10 =>
               Toggle_Help_Window;
            when Esc =>
               check_esc;
            when others =>
               --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
               if not (key and (F1 .. F4 => True, others => False))
               then
                  beep;
               end if;
         end case;
         --  case
         --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
         previous_key := key;
         exit when key and (F1 | F3 | F4 | Esc => True, others => False);
      end loop;
      if (key /= ESC)
      then
         Erase_Message;
      end if;
      if Large_Smith
      then
         Toggle_Large_Smith (key);
      end if;
      if help_displayed
      then
         Write_Board_Parameters;
      end if;
   end Plot2;
   --  * Plot2 *
   --  *
   --  Procedure for directing editing actions in the parts window.
   --  *

   procedure Parts3 is
      --  [BP2P]: Label "100002" Was "component_start"
      --  [BP2P]: Label "100002" Was "component_start"
      tmp_file_name : string;
   begin
      ccompt := compt3;
      cx := cx3;
      <<LABEL_100002>>
      loop
         if read_kbd
         then
            get_key;
         end if;
         case key is
            when C_R =>
               Carriage_Return;
            when right_arrow =>
               move_cursor (1, 0);
            when left_arrow =>
               move_cursor (- 1, 0);
            when Ins =>
               insert_key := not (insert_key);
            when 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '+' | '-' | '.' | ',' | ' ' | '(' | ')' | ':' | '\' | '?' | '!' | Omega | Degree | Parallel | Mu =>
               add_char (ccompt);
            when del =>
               del_char (ccompt);
            when backspace =>
               back_char (ccompt);
            when down_arrow =>
               move_cursor (0, 1);
            when up_arrow =>
               move_cursor (0, - 1);
            when '=' =>
               Pars_Single_Part (ccompt);
            when Ctrl_r =>
               tmp_file_name := input_string ("File to read:", ' ');
               if (tmp_file_name /= "")
               then
                  puff_file := tmp_file_name;
                  Read_Net (puff_file, false);
               end if;
            when Ctrl_e =>
               Erase_Circuit;
            when F5 =>
               Change_Bk_Color;
            when F10 =>
               Toggle_Help_Window;
            when Tab =>
               Toggle_Parts_Lists;
            when Esc =>
               check_esc;
            when screenresize =>
               Screen_Resize;
            when others =>
               --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
               if not (key and (F1 .. F4 => True, others => False))
               then
                  beep;
               end if;
         end case;
         --  case
         --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
         exit when key and (F1 .. F4 | Esc => True, others => False);
      end loop;
      compt3 := ccompt;
      cx3 := cx;
      if (key /= Esc) and then (key /= not_esc)
      then
         erase_message;
         action := true;
         Pars_Compt_List;
         if bad_compt
         then
            --  [BP2P]: Label "100002" Was "component_start"
            read_kbd := true;
            cx := ccompt.all.x_block;
            goto LABEL_100002;
         end if;
      end if;
      if not (board_changed and then (key = F4))
      then
         if circuit_changed and then (key /= Esc)
         then
            Redraw_Circuit;
         end if;
      end if;
      --  Delay redraw if F4 and board_changed, in case of error
      if help_displayed
      then
         if Large_Parts
         then
            Write_Expanded_Parts;
         else
            Write_Board_Parameters;
         end if;
         help_displayed := false;
      end if;
   end Parts3;
   --  * Parts3 *
   --  *
   --  Procedure for directing editing in the Board window.
   --  *

   procedure Board4 is
      --  [BP2P]: Label "100003" Was "Start_Board_Label"
      --  in case a previous help window displayed
      --  [BP2P]: Label "100003" Was "Start_Board_Label"
   begin
      Write_Board_Parameters;
      HighLight_Window;
      ccompt := board_start;
      cx := ccompt.all.x_block;
      <<LABEL_100003>>
      loop
         Get_Key;
         case key is
            when '0' .. '9' | '.' | ' ' | 'E' | 'P' | 'T' | 'G' | 'M' | 'k' | 'm' | Mu | 'n' | 'p' | 'f' | 'a' | Omega | 'h' | 'H' | 'z' | 'Z' =>
               add_char (ccompt);
            when Del =>
               del_char (ccompt);
            when backspace =>
               back_char (ccompt);
            when Ins =>
               insert_key := not (insert_key);
            when Up_Arrow =>
               move_cursor (0, - 1);
            when Down_arrow =>
               move_cursor (0, 1);
            when Left_arrow =>
               move_cursor (- 1, 0);
            when Right_arrow =>
               move_cursor (1, 0);
            when C_R =>
               Carriage_Return;
            when Tab =>
               --  sets board_changed
               Toggle_Circuit_Type;
            when F10 =>
               Toggle_Help_Window;
            when F5 =>
               Change_Bk_Color;
            when Esc =>
               check_esc;
            when screenresize =>
               Screen_Resize;
            when others =>
               --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
               if not (key and (F1 .. F4 => True, others => False))
               then
                  beep;
               end if;
         end case;
         --  case
         --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
         exit when key and (F1 .. F3 | Esc => True, others => False);
      end loop;
      if (key /= Esc)
      then
         --  compute new board parameters
         erase_message;
         action := true;
         Board_Parser;
         if bad_compt
         then
            --  [BP2P]: Label "100003" Was "Start_Board_Label"
            read_kbd := true;
            cx := ccompt.all.x_block;
            goto LABEL_100003;
         end if;
         if board_changed
         then
            --  load in new board parameters
            --  write new prefix for x-coord
            --  compute new component parameters
            Fresh_Dimensions;
            Write_Plot_Prefix (false);
            Pars_Compt_List;
            if bad_compt
            then
               --  If error occurs,
               --  point to last change
               --  [BP2P]: Label "100003" Was "Start_Board_Label"
               read_kbd := true;
               cx := ccompt.all.x_block;
               goto LABEL_100003;
            end if;
            Redraw_Circuit;
         end if;
      end if;
      if Large_Parts
      then
         Write_Expanded_Parts;
      else
         if help_displayed
         then
            Write_Parts_ListO;
         end if;
      end if;
   end Board4;
   --  * Board4 *
   --  **************************************************************
   --  Main Program
   --  ***************************************************************
   --  Init device file memory block
   --  Init network memory block
begin
   Puff_Start;
   Init_Marker (dev_beg);
   Init_Marker (net_beg);
   Read_Net (puff_file, true);
   read_kbd := not (circuit_changed);
   loop
      window_number := Character'Pos (key) - Character'Pos (F1) + 1;
      help_displayed := false;
      HighLight_Window;
      case window_number is
         when 1 =>
            Layout1;
         when 2 =>
            Plot2;
         when 3 =>
            Parts3;
         when 4 =>
            Board4;
         when others =>
            --  [P2Ada]: no otherwise / else in Pascal
            null;
      end case;
      if not ((window_number = 4) and then Large_Parts)
      then
         write_comptm (3, col_window (window_number), window_f (window_number));
      end if;
      exit when key = Esc;
   end loop;
   CloseGraph;
   TextMode (OrigMode);
   ClrScr;
end PUFF;
