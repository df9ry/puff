
with Ada.Numerics.Long_Elementary_Functions;
use  Ada.Numerics.Long_Elementary_Functions;

with pfun1; use pfun1;
with pfun2; use pfun2;
with pfun3; use pfun3;
with pfmsc; use pfmsc;

package body pfun1 is

   --  memsize : constant := 2 * 1024 * 1024;
   --  membase : Integer_32;
   memused : Integer_32;

   procedure Init_Marker (P : in out marker) is
   begin
      P.Used := -1;
   end Init_Marker;

   --  *
   --  Pars the component list.
   --  If action=true then find part dimensions
   --  else find s-parameters.
   --  Careful here on memory management!
   --  *
   procedure Pars_Compt_List is
      pars, reload_all_devices : Boolean;
      tcompt : compt;
      i_or_d : constant CharacterArray := ('i', 'd');
   begin
      if action
      then
         --  Check for alt_sweep and device file changes
         tcompt := null;
         reload_all_devices := False;
         loop
            --  step through and Reset if a sweep_compt was changed
            if tcompt = null
            then
               tcompt := part_start;
            else
               tcompt := tcompt.all.next_compt;
            end if;
            x_sweep.Check_Reset (tcompt);
            if tcompt.changed and then
               is_in (get_lead_charO (tcompt), i_or_d)
            then
               if Marked (dev_beg)
               then
                  Release_Mem (dev_beg);
               end if;
               Mark_Mem (dev_beg);
               Init_Marker (net_beg);
               circuit_changed := True;
               reload_all_devices := True;
            end if;
            exit when tcompt.next_compt = null;
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
               if is_in (get_lead_charO (tcompt), i_or_d)
               then
                  tcompt.changed := True;
               end if;
               exit when tcompt.next_compt = null;
            end loop;
         end if;
      end if;
      tcompt := null;
      bad_compt := False;
      loop
         if tcompt = null
         then
            tcompt := part_start;
         else
            tcompt := tcompt.all.next_compt;
         end if;
         --  [P2Ada]: WITH instruction
         --  [P2Ada]: !Help! No type found -> add 'P2Ada_Var_3.' to fields
         declare P2Ada_Var_3 : compt_record renames tcompt.all;
         begin
            if P2Ada_Var_3.changed and then
              (P2Ada_Var_3.used > 0 or else P2Ada_Var_3.step)
            then
               circuit_changed := True;
            end if;
            if action
            then
               pars := P2Ada_Var_3.changed;
            else
               pars := P2Ada_Var_3.used > 0;
            end if;
            if pars
            then
               P2Ada_Var_3.parsed := True;
               if action
               then
                  --  init to check for alt_sweep-needed?
                  P2Ada_Var_3.typ := To_C (get_lead_charO (tcompt));
                  P2Ada_Var_3.sweep_compt := False;
               end if;
               case P2Ada_Var_3.typ is
                  when 't' =>
                     tlineO (tcompt);
                  when 'q' =>
                     qline (tcompt);
                  when 'c' =>
                     clinesO (tcompt);
                  when 'd' | 'i' =>
                     if action
                     then
                        Device_Read (tcompt, (P2Ada_Var_3.typ = 'i'));
                     else
                        Device_S (tcompt, (P2Ada_Var_3.typ = 'i'));
                     end if;
                  when 'l' =>
                     lumpedO (tcompt);
                  when 'x' =>
                     Transformer (tcompt);
                  when 'a' =>
                     Attenuator (tcompt);
                  when ' ' =>
                     P2Ada_Var_3.parsed := False;
                  when others =>
                     begin
                        P2Ada_Var_3.parsed := False;
                        bad_compt := True;
                        message (1) := To_Unbounded_String
                          ("" & Character (P2Ada_Var_3.typ) & " is an");
                        message (2) := To_Unbounded_String ("unknown part");
                     end;
               end case;
               --  case
            end if;
            --  if pars
            if not (bad_compt)
            then
               P2Ada_Var_3.changed := False;
            else
               if window_number = 3
               then
                  ccompt := tcompt;
               end if;
            end if;
         end;
         --  [P2Ada]: end of WITH
         --  with
         exit when tcompt.next_compt = null or else bad_compt;
      end loop;
      if bad_compt
      then
         Write_Message;
      end if;
   end Pars_Compt_List;

   --  This procedure has been drastically reduced using
   --  Turbo Pascal graphics.
   --  Used for erasing sections of the screen (color=brown)
   --  and for drawing tlines (color=white).
   --  *
   procedure fill_box (x1, y1, x2, y2, color : Integer) is
   begin
      if blackwhite
      then
         SetFillStyle (SOLIDFILL, White);
      else
         SetFillStyle (SOLIDFILL, Integer_32 (color));
      end if;
      Bar (Integer_32 (x1), Integer_32 (y1), Integer_32 (x2),
           Integer_32 (y2));
   end fill_box;

   --  *
   --  Update array which contains keystrokes used to layout circuit.
   --  *
   procedure update_key_list (nn : Integer) is
   begin
      if key_end = 0
      then
         key_end := 1;
      else
         key_end := key_end + 1;
      end if;
      if key_end = key_max
      then
         key_end := key_max - 1;
         message (1) := To_Unbounded_String ("Circuit is too");
         message (2) := To_Unbounded_String ("complex for");
         message (3) := To_Unbounded_String ("redraw");
         Write_Message;
      end if;
      key_list (key_end).keyl := key;
      key_list (key_end).noden := nn;
   end update_key_list;

   procedure SetCol (col : Unsigned_16) is
   begin
      if blackwhite
      then
         case col is
            when 0 =>
               --  1..7 : SetColor(lightgray);
               SetColor (0);
            when 8 =>
               SetColor (0);
            when others =>
               SetColor (White);
         end case;
      else
         SetColor (col);
      end if;
   end SetCol;

   procedure Draw_Box (xs, ys, xm, ym, color : Integer) is
   begin
      SetCol (Unsigned_16 (color));
      Rectangle (Integer_32 (xs), Integer_32 (ys), Integer_32 (xm),
                 Integer_32 (ym));
   end Draw_Box;

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
      Erase_Message;
      if compt1 /= null
      then
         write_compt (LightGray, compt1);
      end if;
      compt1 := part_start;
      if Marked (net_beg)
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
         filled_OK := False;
         circuit_changed := False;
         board_changed := False;
         marker_OK := False;
         key_end := 0;
         Extra_Parts_Used := False;
      end if;
      net_start := null;
      cnet := null;
      Draw_Circuit;
   end Erase_Circuit;

   --  procedure Init_Mem is
   --  begin
   --     getmem (membase, memsize);
   --     if (membase = null)
   --     then
   --        Put ("Memory allocation error!!");
   --        New_Line;
   --        raise Program_halted;
   --     end if;
   --     memused := 0;
   --  end Init_Mem;

   --  *
   --  Creates text border pattern for start screen and other windows
   --  *
   procedure Make_Text_Border (x1, y1, x2, y2, colour : Integer;
                               single : Boolean)
   is
      sing_vert : constant Character := Character'Val (179);
      doub_vert : constant Character := Character'Val (186);
      --  sing_horz : constant Character := Character'Val (196);
      doub_horz : constant Character := Character'Val (205);
      sing_UL   : constant Character := Character'Val (213);
      doub_UL   : constant Character := Character'Val (201);
      sing_UR   : constant Character := Character'Val (184);
      doub_UR   : constant Character := Character'Val (187);
      sing_LL   : constant Character := Character'Val (212);
      doub_LL   : constant Character := Character'Val (200);
      sing_LR   : constant Character := Character'Val (190);
      doub_LR   : constant Character := Character'Val (188);
      vert, horz, UL, UR, LL, LR : Character;
   begin
      if single
      then
         vert := sing_vert;
         horz := doub_horz;
         UL := sing_UL;
         UR := sing_UR;
         LL := sing_LL;
         LR := sing_LR;
      else
         vert := doub_vert;
         horz := doub_horz;
         UL := doub_UL;
         UR := doub_UR;
         LL := doub_LL;
         LR := doub_LR;
      end if;
      --  clear area
      Clear_Window (x1, y1, x2, y2);
      TextCol (Unsigned_16 (colour));
      for i in y1 .. y2
      loop
         --  * Draw Border for startup screen *
         GotoXY (Integer_32 (x1), Integer_32 (i));
         Put (vert);
         GotoXY (Integer_32 (x2), Integer_32 (i));
         Put (vert);
      end loop;
      for i in x1 .. x2
      loop
         GotoXY (Integer_32 (i), Integer_32 (y1));
         Put (horz);
         GotoXY (Integer_32 (i), Integer_32 (y2));
         Put (horz);
      end loop;
      GotoXY (Integer_32 (x1), Integer_32 (y1));
      Put (UL);
      GotoXY (Integer_32 (x2), Integer_32 (y1));
      Put (UR);
      GotoXY (Integer_32 (x1), Integer_32 (y2));
      Put (LL);
      GotoXY (Integer_32 (x2), Integer_32 (y2));
      Put (LR);
   end Make_Text_Border;

   procedure prp (vu : in out TComplex; vX, vY : TComplex) is
   begin
      vu.r := vX.r * vY.r - vX.i * vY.i;
      vu.i := vX.r * vY.i + vX.i * vY.r;
   end prp;

   procedure co (co : in out TComplex; s, t : Long_Float) is
   begin
      co.r := s;
      co.i := t;
   end co;

   procedure rc (rc : in out TComplex; z : TComplex) is
      --  ! was real, changed 10/15/90
      --  !* check for 1/0 added here *
      mag : Long_Float;
   begin
      mag := ((z.r) ** 2) + ((z.i) ** 2);
      if mag = 0.0
      then
         --  Although this is equivalent to saying 1/0 = 0
         --  it works properly for the few times it occurs
         rc.r := 0.0;
         rc.i := 0.0;
      else
         rc.r := z.r / mag;
         rc.i := -z.i / mag;
      end if;
   end rc;

   procedure sm (sm : in out TComplex; s : Long_Float; t : TComplex) is
   begin
      sm.r := s * t.r;
      sm.i := s * t.i;
   end sm;

   procedure beep is
   begin
      Sound (250);
      C_Delay (50);
      NoSound;
   end beep;

   --  *
   --  Interuptable delay for use in demo mode.
   --  *
   procedure rcdelay (j : Integer) is
   begin
      for i in 1 .. j
      loop
         if KeyPressed /= 0
         then
            chs := char'Val (ReadKey);
            if chs = 'S'
            then
               beep;
               TextMode (co80);
               raise Program_halted with "From rcdelay";
            else
               C_Delay (2000);
            end if;
         else
            C_Delay (10);
         end if;
      end loop;
   end rcdelay;

   --  *
   --  Write message in center box.
   --  *
   procedure Write_Message is
   begin
      TextCol (Unsigned_16 (message_color));
      GotoXY (Integer_32 ((xmin (6) + xmax (6) - (Length (message (1)))) / 2),
              Integer_32 (ymin (6)));
      PutStr (To_C (To_String (message (1))));
      GotoXY (Integer_32 ((xmin (6) + xmax (6) - (Length (message (1)))) / 2),
              Integer_32 (ymin (6)) + 1);
      PutStr (To_C (To_String (message (2))));
      GotoXY (Integer_32 ((xmin (6) + xmax (6) - (Length (message (1)))) / 2),
              Integer_32 (ymin (6)) + 2);
      PutStr (To_C (To_String (message (3))));
      if message (1) & message (2) & message (3) /= "" and then read_kbd
      then
         beep;
      end if;
      if demo_mode
      then
         rcdelay (50);
      end if;
   end Write_Message;

   --  *
   --  Flash error message in message window.
   --  *
   procedure Write_Error (time : Long_Float) is
   begin
      Write_Message;
      C_Delay (Integer_32 (1000.0 * time + 0.5));
      Erase_Message;
   end Write_Error;

   --  *
   --  Erase message in center box.
   --  *
   procedure Erase_Message is
   begin
      Clear_Window (xmin (6), ymin (6), xmax (6), ymax (6));
      message (1) := To_Unbounded_String ("");
      message (2) := To_Unbounded_String ("");
      message (3) := To_Unbounded_String ("");
   end Erase_Message;

   --  Clear region of screen. Text coordinates are the input.
   --  Erasing was done in textmode, now in graphics mode.
   --  *
   procedure Clear_Window (x1, y1, x2, y2 : Integer) is
   begin
      Window (1, 1, Integer_32 (Max_Text_X), Integer_32 (Max_Text_Y));
      SetFillStyle (SOLIDFILL, Black);
      Bar (Integer_32 (8 * (x1 - 1)), Integer_32 (14 * (y1 - 1)),
           Integer_32 (8 * x2), Integer_32 (14 * y2));
   end Clear_Window;

   --  Clear region of screen. Graphics coordinates are the input.
   --  Erasing was done in textmode, now in graphics mode.
   --  *
   procedure Clear_Window_gfx (x1, y1, x2, y2 : Integer) is
   begin
      Window (1, 1, Integer_32 (Max_Text_X), Integer_32 (Max_Text_Y));
      SetFillStyle (SOLIDFILL, Black);
      Bar (Integer_32 (x1), Integer_32 (y1), Integer_32 (x2),
           Integer_32 (y2));
   end Clear_Window_gfx;

   function File_Exists_And_Open (file : in out Ada.Text_IO.File_Type;
                                  fname : Unbounded_String) return Boolean
   is
   begin
      Ada.Text_IO.Open (file, In_File, To_String (fname));
      return True;
   exception
      when Name_Error =>
         return False;
      when others =>
         Put_Line ("Other exception in File_Exists");
         return False;
   end File_Exists_And_Open;

   function setupexists (fname : out Unbounded_String) return Boolean is
      Result_setupexists : Boolean;
      found : Boolean;
   begin
      found := False;
      message (2) := fname;
      if fname /= "setup.puf"
      then
         fname := To_Unbounded_String ("setup.puf");
         found := fileexists (False, net_file, fname);
      end if;
      if not (found)
      then
         fname := To_Unbounded_String ("../PUFF/setup.puf");
         found := fileexists (False, net_file, fname);
      end if;
      if found
      then
         Erase_Message;
         message (1) := To_Unbounded_String ("Missing board#");
         message (2) := To_Unbounded_String ("Try");
         message (3) := fname;
         Write_Error (2.0);
      end if;
      Result_setupexists := found;
      return Result_setupexists;
   end setupexists;

   --  Called when a disastrous error condition
   --  has been reached to stop Puff.
   --  *
   procedure shutdown is
      dummy : Integer_32;
   begin
      CloseGraph;
      TextMode (Integer_32 (OrigMode));
      message (1) := To_Unbounded_String ("FATAL ERROR:");
      Write_Message;
      GotoXY (3, 23);
      PutStr (To_C ("Press any key to quit"));
      dummy := ReadKey;
      loop
         exit when KeyPressed = 1;
      end loop;
      raise Program_halted with "Within shutdown";
   end shutdown;

   procedure Check_Reset (Self : in out Sweep'class; tcompt : compt) is
   begin
      if (Self.element = tcompt) and then tcompt.changed
      then
         Init_Use (Self);
         tcompt.sweep_compt := False;
      end if;
   end Check_Reset;

   procedure Label_Axis (Self : in out Sweep'class) is
      label_string : Unbounded_String;
      label_lngth : Integer;
   begin
      label_string := Self.part_label & " : " & Self.unit_label;
      label_lngth := Length (label_string);
      GotoXY (Integer_32 (x_y_plot_text (5, 1) + 2 - label_lngth / 2),
              Integer_32 (x_y_plot_text (5, 2)));
      Put (To_String (label_string));
   end Label_Axis;

   procedure Label_Plot_Box (Self : in out Sweep'class) is
      --  write part label over 'f'
   begin
      GotoXY (Integer_32 (xmin (2)), Integer_32 (ymin (2) + 2));
      Put (To_String (Self.part_label));
      GotoXY (Integer_32 (xmin (2) + 17), Integer_32 (ymin (2) + 2));
      Put (To_String (Self.unit_label));
      if Length (Self.unit_label) <= 2
      then
         for i in 0 .. (2 - Length (Self.unit_label))
         loop
            Put (' ');
         end loop;
      end if;
   end Label_Plot_Box;

   procedure Load_Prop_Const (Self : in out Sweep'class;
                              prop_consta, prop_constb : Long_Float)
   is
   begin
      Self.prop_const1 := prop_consta;
      Self.prop_const2 := prop_constb;
   end Load_Prop_Const;

   --  Read index for lumped element and clines sweep_compt.
   --  LUMPED
   --  i=1 : resistance
   --  i=2 : + reactance or susceptance
   --  i=3 : - reactance or susceptance
   --  CLINES
   --  i=1 : even mode impedance only
   --  i=2 : odd mode impedance only
   --  i=3 : even and odd mode impedances given, even is the variable
   --  i=4 : even and odd mode impedances given, odd is the variable
   --  *
   procedure Load_Index (Self : in out Sweep'class; i : Integer) is
      us : Unbounded_String;
   begin
      Self.index := i;
      if Self.id = 'j' and then Self.index = 3
      then
         us := "-" & Self.unit_label;
         Self.unit_label := us;
      end if;
   end Load_Index;

   --  * Init_Element *
   procedure Init_Element (Self : in out Sweep'class; tcompt : compt;
                           in_id, in_prefix, in_unit : C.char)
   is
      potential_units : constant CharArray :=
        (Degree, Omega, 'm', 'h', 's', 'S', 'z', 'Z', 'y', 'Y', 'Q');
   begin
      if not (Self.used)
      then
         Self.used := True;
         Self.element := tcompt;
         Self.id := in_id;
         Self.prefix := in_prefix;
         Self.Omega0 := 2.0 * Pi * design_freq *
           Eng_Prefix (Character (freq_prefix));
         Self.units := in_unit;
         Alt_Sweep := True;
         tcompt.all.sweep_compt := True;
         Self.part_label := "Part " & tcompt.descript;
         if is_in (Self.prefix, Eng_Dec_Mux)
         then
            Self.unit_label := To_Unbounded_String
              (Character (Self.prefix) & "");
         else
            Self.unit_label := To_Unbounded_String ("");
         end if;
         if Self.id = 'j'
         then
            Self.unit_label := 'j' & Self.unit_label;
         end if;
         --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
         if Self.id = 'F' or else Self.id = 'H'
         then
            Self.unit_label := Self.unit_label & Character (Self.id);
         else
            if Self.id = 'a'
            then
               Self.unit_label := Self.unit_label & "dB";
            else
               if Self.id = 't'
               then
                  Self.unit_label := Self.unit_label & "n:1";
               else
                  if is_in (Self.units, potential_units)
                  then
                     Self.unit_label :=
                       Self.unit_label & Character (Self.units);
                  end if;
               end if;
            end if;
         end if;
      else
         Self.element.all.changed := True;
         Init_Use (Self);
         tcompt.all.sweep_compt := False;
         bad_compt := True;
         message (1) := To_Unbounded_String ("Parts list has");
         message (2) := To_Unbounded_String ("multiple sweep");
         message (3) := To_Unbounded_String ("parameters");
      end if;
   end Init_Element;

   procedure Init_Use (Self : in out Sweep'class) is
   begin
      Self.element := null;
      Self.used := False;
      Alt_Sweep := False;
      Self.unit_label := To_Unbounded_String ("");
      Self.index := 0;
   end Init_Use;

   procedure TextCol (col : Unsigned_16) is
   begin
      if blackwhite and then col /= Black
      then
         TextColor (White);
      else
         TextColor (col);
      end if;
   end TextCol;

   --  Function for calculating width in mils of microstrip
   --  and stripline transmission lines.  See Puff manual
   --  pages 12-13 for details.
   --  Microstrip models are from Owens:
   --  Radio and Elect Eng, 46, pp 360-364, 1976.
   --  Stripline models are from  Cohn:
   --  MTT-3 pp19-126, March 1955.
   --  See also Gupta, Garg, Chadha:
   --  CAD of Microwave Circuits Artech House, 1981.
   --  *
   function widtht (zed : Long_Float) return Long_Float is
      Result_widtht : Long_Float;
      lnpi_2 : constant := 0.451583;
      ln4_pi : constant := 0.241564;
      Hp, expH, de, x : Long_Float;

      procedure High_Z_Error is
      begin
         bad_compt := True;
         message (1) := To_Unbounded_String ("Impedance");
         message (2) := To_Unbounded_String ("too large");
         Result_widtht := 0.0;
      end High_Z_Error;
   begin
      if stripline
      then
         x := zed * Sqrt (er) / (30.0 * Pi);
         if Pi * x > 87.0
         then
            --  or else kkk(x) will explode
            High_Z_Error;
         else
            Result_widtht := substrate_h * 2.0 * Arctanh (kkk (x)) / Pi;
         end if;
         --  Use kkk for k factor
         --  if microstripline then
      else
         if zed > (44.0 - 2.0 * er)
         then
            Hp := (zed / 120.0) * Sqrt (2.0 * (er + 1.0)) + (er - 1.0) *
              (lnpi_2 + ln4_pi / er) / (2.0 * (er + 1.0));
            if Hp > 87.0
            then
               --  e^87 = 6.0e37
               High_Z_Error;
            else
               expH := Exp (Hp);
               Result_widtht := substrate_h /
                 (expH / 8.0 - 1.0 / (4.0 * expH));
            end if;
            --  if zed <= (44-2*er)
         else
            de := 60.0 * ((Pi) ** 2.0) / (zed * Sqrt (er));
            Result_widtht := substrate_h * (2.0 / Pi * ((de - 1.0) -
                                Log (2.0 * de - 1.0)) + (er - 1.0) *
                                (Log (de - 1.0) + 0.293 - 0.517 / er) /
                                (Pi * er));
         end if;
         --  if zed
      end if;
      --  if stripline
      return Result_widtht;
   end widtht;

   procedure write_compt (color : Integer; tcompt : compt) is
   begin
      TextCol (Unsigned_16 (color));
      declare P2Ada_Var_1 : compt_record renames tcompt.all;
      begin
         GotoXY (Integer_32 (P2Ada_Var_1.xp), Integer_32 (P2Ada_Var_1.yp));
         Put (To_String (P2Ada_Var_1.descript));
      end;
      --  [P2Ada]: end of WITH
   end write_compt;

   --  * write_compt *
   --  *
   --  Display the first m characters of a component.
   --  *

   procedure write_comptm (m, color : Integer; tcompt : compt) is
   begin
      TextCol (Unsigned_16 (color));
      declare P2Ada_Var_2 : compt_record renames tcompt.all;
      begin
         GotoXY (Integer_32 (P2Ada_Var_2.xp), Integer_32 (P2Ada_Var_2.yp));
         for i in 1 .. m
         loop
            Put (To_String (P2Ada_Var_2.descript));
         end loop;
      end;
      --  [P2Ada]: end of WITH
   end write_comptm;

   function Marked (P : marker) return Boolean is
      Result_Marked : Boolean;
   begin
      Result_Marked := (P.Used >= 0);
      return Result_Marked;
   end Marked;

   procedure Release_Mem (P : marker) is
   begin
      if P.Used >= 0
      then
         memused := P.Used;
      end if;
   end Release_Mem;

   procedure Mark_Mem (P : in out marker) is
   begin
      P.Used := memused;
   end Mark_Mem;

   --  *
   --  Function used to calculate Cohn's "k" factor for stripline
   --  width formulas.  See equation 3.6, page 13 of the Puff Manual.
   --  Used by widthO, w_s_stripline_cline.
   --  *
   function kkk (x : Long_Float) return Long_Float is
      Result_kkk : Long_Float;
      expx : Long_Float;
   begin
      if x > 1.0
      then
         expx := Exp (Pi * x);
         Result_kkk := Sqrt (1.0 - (((((expx - 2.0) /
                             (expx + 2.0)) ** 2.0)) ** 2.0));
      else
         expx := Exp (Pi / x);
         Result_kkk := (((expx - 2.0) / (expx + 2.0)) ** 2.0);
      end if;
      return Result_kkk;
   end kkk;

   function Get_Real (tcompt : compt; n : Integer) return Long_Float is
      --  Result_Get_Real : Long_Float;
      --  c_string, s_value : line_string;
      --  j, code, long : integer;
      --  value : Long_Float;
      --  found : boolean;
   begin
      --  c_string := tcompt.all.descript;
      --  j := goto_numeral (n, c_string);
      if bad_compt
      then
         ccompt := tcompt;
         return Long_Float (n);
      end if;
      --  s_value := "";
      --  long := (c_string'length);
      --  found := false;
      --  loop
      --     --  '+',
      --     --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
      --     if c_string (j) and ('-' | '.' | ',' | '0' .. '9' => True,
      --                                                others => False)
      --     then
      --        if c_string (j) = ','
      --        then
      --           s_value := s_value + '.';
      --        else
      --           s_value := s_value + c_string (j);
      --        end if;
      --        j := j + 1;
      --     else
      --        found := True;
      --     end if;
      --     exit when (found or else (j = long + 1));
      --  end loop;
      --  Val (s_value, value, code);
      --  if (code /= 0) or else ((s_value'length) = 0)
      --  then
      --     ccompt := tcompt;
      --     bad_compt := True;
      --     message (2) := "Invalid number";
      --     return Result_Get_Real;
      --  end if;
      --  get_real := value;
      --  return Result_Get_Real;
      return 0.0;
   end Get_Real;

   function betweenr (x1, x2, x3, sigma : Long_Float) return Boolean is
      Result_betweenr : Boolean;
   begin
      if x1 > x3
      then
         if (x3 - sigma <= x2) and then (x2 <= x1 + sigma)
         then
            Result_betweenr := True;
         else
            Result_betweenr := False;
         end if;
      else
         if (x1 - sigma <= x2) and then (x2 <= x3 + sigma)
         then
            Result_betweenr := True;
         else
            Result_betweenr := False;
         end if;
      end if;
      return Result_betweenr;
   end betweenr;

   procedure dirn_xy is
   begin
      xii := 0;
      yii := 0;
      case dirn is
         when 2 =>
            --  East
            xii := 1;
         when 4 =>
            --  West
            xii := -1;
         when 8 =>
            --  South
            yii := 1;
         when 1 =>
            --  North
            yii := -1;
         when others =>
            null;
      end case;
   end dirn_xy;

   procedure increment_pos (i : Integer) is
   begin
      dirn_xy;
      if (i) mod 2 /= 0
      then
         if i = -1
         then
            xm := xm + (compt1.lngth * Long_Float (xii)
                     + Long_Float (yii) * compt1.con_space) / 2.0;
            ym := ym + (compt1.lngth * Long_Float (yii)
                     + Long_Float (xii) * compt1.con_space) / 2.0;
         else
            if compt1.typ = 'i' or else compt1.typ = 'd'
            then
               if compt1.all.number_of_con /= 1
               then
                  xm := xm + lengthxm * Long_Float (xii) /
                    Long_Float (compt1.number_of_con - 1);
                  ym := ym + lengthym * Long_Float (yii) /
                    Long_Float (compt1.all.number_of_con - 1);
               end if;
            else
               xm := xm + lengthxm * Long_Float (xii);
               ym := ym + lengthym * Long_Float (yii);
            end if;
         end if;
      else
         if i = 0
         then
            xm := cnet.xr;
            ym := cnet.yr;
         else
            if compt1.typ = 'i' or else compt1.typ = 'd'
            then
               xm := xm + lengthxm * Long_Float (xii) /
                 Long_Float (compt1.all.number_of_con - 1);
               ym := ym + lengthym * Long_Float (yii) /
                 Long_Float (compt1.all.number_of_con - 1);
            else
               xm := xm + lengthxm * Long_Float (xii)
                 - compt1.con_space * Long_Float (yii);
               ym := ym + lengthym * Long_Float (yii)
                 - compt1.con_space * Long_Float (xii);
            end if;
         end if;
         --  if i else
      end if;
      --  if odd else
      --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
      xi := Round (xm / csx);
      yi := Round (ym / csy);
      if (compt1.typ /= 'i' and then compt1.typ /= 'd')
        or else (i <= 0) or else (i = (compt1.all.number_of_con - 1))
      then
         case dirn is
            when 1 =>
               dirn := 8;
            when 2 =>
               dirn := 4;
            when 4 =>
               dirn := 2;
            when 8 =>
               dirn := 1;
            when others =>
               null;
         end case;
      end if;
      --  case
   end increment_pos;

   function super_line (tcompt : compt) return Boolean is
      --  * super line if a '!' is present in clines or tline *
      Result_super_line : Boolean;
      c_string : Unbounded_String;
      long : Integer;
   begin
      Result_super_line := False;
      c_string := tcompt.descript;
      long := Length (c_string);
      while long > 0
      loop
         if (Element (c_string, long) = '!') and then
            (tcompt.typ = 'c' or else tcompt.typ = 't')
         then
            Result_super_line := True;
         end if;
         long := long - 1;
      end loop;
      return Result_super_line;
   end super_line;

   function goto_numeral (n : Integer; x : Unbounded_String) return Integer is
      Result_goto_numeral : Integer;
      long, i, j : Integer;
      found : Boolean;
      num1 : constant CharArray :=
        ('?', '+', '-', '.', ',',
         '0', '1', '2', '3', '4', '5', '6', '7', '8', '9');
      num2 : constant CharArray :=
        ('?', '-', '.', ',',
         '0', '1', '2', '3', '4', '5', '6', '7', '8', '9');
   begin
      i := 0;
      found := False;
      Result_goto_numeral := 0;
      j := 1;
      long := Length (x);
      if long > 0
      then
         loop
            if Element (x, j) = '('
            then
               j := Index (x, "(");
            end if;
            if is_in (char (Element (x, j)), num1)
            then
               i := i + 1;
               if i = n
               then
                  found := True;
               else
                  loop
                     --  step over number to find next number
                     --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
                     j := j + 1;
                     exit when not is_in (char (Element (x, j)), num2) or else
                                         (j = long + 1);
                  end loop;
               end if;
            else
               j := j + 1;
            end if;
            exit when found or else (j = long + 1);
         end loop;
      end if;
      if found
      then
         Result_goto_numeral := j;
      else
         bad_compt := True;
         message (2) := To_Unbounded_String ("Number is missing");
      end if;
      return Result_goto_numeral;
   end goto_numeral;

   function Eng_Prefix (c : Character) return Long_Float is
      Result_Eng_Prefix : Long_Float;
   begin
      case c is
         when 'E' =>
            Result_Eng_Prefix := 1.0e+18;
         when 'P' =>
            Result_Eng_Prefix := 1.0e+15;
         when 'T' =>
            Result_Eng_Prefix := 1.0e+12;
         when 'G' =>
            Result_Eng_Prefix := 1.0e+09;
         when 'M' =>
            Result_Eng_Prefix := 1.0e+06;
         when 'k' =>
            Result_Eng_Prefix := 1.0e+03;
         when 'm' =>
            Result_Eng_Prefix := 1.0e-03;
         when Character (Mu) =>
            Result_Eng_Prefix := 1.0e-06;
         when 'n' =>
            Result_Eng_Prefix := 1.0e-09;
         when 'p' =>
            Result_Eng_Prefix := 1.0e-12;
         when 'f' =>
            Result_Eng_Prefix := 1.0e-15;
         when 'a' =>
            Result_Eng_Prefix := 1.0e-18;
         when others =>
            Result_Eng_Prefix := 1.0;
      end case;
      --  case
      return Result_Eng_Prefix;
   end Eng_Prefix;

   procedure Get_Param (tcompt       : compt;
                        n            : Integer;
                        value        : in out Long_Float;
                        value_string : in out Unbounded_String;
                        u1, prefix   : in out Character;
                        alt_param    : in out Boolean) is
      potential_units : constant CharArray :=
        (Degree, Omega, 'm', 'h', 'H', 's', 'S', 'z', 'Z', 'y', 'Y');
      potential_numbers : constant CharArray :=
        ('+', '-', '.', ',',
         '0', '1', '2', '3', '4', '5', '6', '7', '8', '9');
      c_string, s_value : Unbounded_String;
      i, j, long : Integer;
      found_value : Boolean;
   begin
      alt_param := False;
      c_string := tcompt.all.descript;
      j := goto_numeral (n, c_string);
      if bad_compt
      then
         ccompt := tcompt;
         return;
      end if;
      long := Length (c_string);
      while Element (c_string, long) = ' '
      loop
         long := long - 1;
      end loop;
      --  ignore end spaces
      if j > 0
      then
         found_value := False;
         s_value := To_Unbounded_String ("");
         loop
            if is_in (char (Element (c_string, j)), potential_numbers)
            then
               if not (Element (c_string, j) = '+')
               then
                  --  force a skip over '+' signs
                  if Element (c_string, j) = ','
                  then
                     --  sub '.' for ','
                     s_value := s_value & '.';
                  else
                     s_value := s_value & Element (c_string, j);
                  end if;
               end if;
               --  '+' sign check
               j := j + 1;
            else
               if Element (c_string, j) = '?'
               then
                  --  Check here for variable
                  alt_param := True;
                  j := j + 1;
               else
                  found_value := True;
               end if;
            end if;
            exit when found_value or else (j = long + 1);
         end loop;
         --  Val (s_value, value, code);
         s_value := To_Unbounded_String (Long_Float'Image (value));
         --  if (code /= 0) or else (Length (s_value) = 0)
         if Length (s_value) = 0
         then
            if alt_param
            then
               --  return these for uninitialized variables
               value := 1.0;
               value_string := To_Unbounded_String ("1.0");
            else
               ccompt := tcompt;
               bad_compt := True;
               message (2) := To_Unbounded_String ("Invalid number");
               return;
            end if;
         else
            value_string := s_value;
         end if;
      end if;
      while (Element (c_string, j) = ' ') and then (j < long + 1)
      loop
         j := j + 1;
      end loop;
      --  Skip spaces
      --  initialize prefix to blank
      --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
      prefix := ' ';
      if is_in (char (Element (c_string, j)), Eng_Dec_Mux) and then (j <= long)
      then
         if Element (c_string, j) = 'm'
         then
            --  is 'm' a unit or prefix?
            i := j + 1;
            while (Element (c_string, i) = ' ') and then (i < long + 1)
            loop
               i := i + 1;
            end loop;
            --  * Skip spaces to check for some unit *
            --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
            if is_in (char (Element (c_string, i)), potential_units)
            then
               --  it's the prefix milli 'm' next to a unit
               --  make j point past the prefix, to the unit
               prefix := 'm';
               value := Eng_Prefix ('m') * value;
               j := i;
            end if;
            --  if 'm' is a unit do nothing
            --  if other than 'm' factor in prefix
            --  advance from prefix toward unit
         else
            prefix := Element (c_string, j);
            value := Eng_Prefix (Element (c_string, j)) * value;
            j := j + 1;
         end if;
      end if;
      while (Element (c_string, j) = ' ') and then (j <= long)
      loop
         j := j + 1;
      end loop;
      --  Skip spaces
      if j <= long
      then
         u1 := Element (c_string, j);
      else
         u1 := '?';
      end if;
      if u1 = 'm'
      then
         value := 1000.0 * value;
      end if;
      --  return millimeters, not meters
   end Get_Param;

   procedure w_s_stripline_cline (zede, zedo : Long_Float;
                                  woh, soh : in out Long_Float) is
      ke, ko : Long_Float;
   begin
      ke := kkk (zede * Sqrt (er) / (30.0 * Pi));
      ko := kkk (zedo * Sqrt (er) / (30.0 * Pi));
      woh := 2.0 * Arctanh (Sqrt (ke * ko)) / Pi;
      soh := 2.0 * Arctanh (Sqrt (ke / ko) * (1.0 - ko) / (1.0 - ke)) / Pi;
   end w_s_stripline_cline;

   function cl_cosh (x : Long_Float) return Long_Float is
      Result_cl_cosh : Long_Float;
      exp1 : Long_Float;
   begin
      if x > 300.0
      then
         exp1 := infty;
         bad_compt := True;
         message (1) := To_Unbounded_String ("cline impedances");
         message (2) := To_Unbounded_String ("can't be realized");
         message (3) := To_Unbounded_String ("in microstrip");
      else
         exp1 := Exp (x);
         Result_cl_cosh := (exp1 + 1.0 / exp1) / 2.0;
      end if;
      return Result_cl_cosh;
   end cl_cosh;

   procedure w_s_microstrip_cline (we, wo : Long_Float;
                                   woh, soh : in out Long_Float) is
      tol : constant := 0.0001;
      codd, ceven, g, fg, dfg, dg, g1 : Long_Float;
      i : Integer;
   begin
      ceven := cl_cosh (Pi * we / 2.0);
      codd := cl_cosh (Pi * wo / 2.0);
      soh := 2.0 * Arccosh ((ceven + codd - 2.0) / (codd - ceven)) / Pi;
      if bad_compt
      then
         return;
      end if;
      --  starting guess
      g := cl_cosh (Pi * soh / 2.0);
      i := 0;
      loop
         --  newton algorithm
         --  !* beware of divergence in this loop *
         g1 := g;
         fg := 0.0;
         dfg := 0.0;
         error (g, wo, ceven, fg, dfg);
         if bad_compt
         then
            return;
         end if;
         dg := fg / dfg;
         g := g1 - dg;
         i := i + 1;
         if g <= 1.0
         then
            i := 101;
         end if;
         exit when (abs (dg) < abs (tol * g)) or else (i > 100);
      end loop;
      if i > 100
      then
         bad_compt := True;
         message (1) := To_Unbounded_String ("cline impedances");
         message (2) := To_Unbounded_String ("can't be realized");
         message (3) := To_Unbounded_String ("in microstrip");
         return;
      end if;
      soh := 2.0 * Arccosh (g) / Pi;
      woh := Arccosh (0.5 * ((g + 1.0) * ceven + g - 1.0)) / Pi - soh / 2.0;
   end w_s_microstrip_cline;

   procedure error (g, wo, ceven : Long_Float; fg, dfg : in out Long_Float) is
      hh, sqm1, rsqm1, dcdg, acoshh, acoshg, u1, u2, du1dg, du2dg, dhdg, dcdu1,
      dcdu2, dcdh : Long_Float;
   begin
      hh := 0.5 * ((g + 1.0) * ceven + g - 1.0);
      dhdg := 0.5 * (ceven + 1.0);
      acoshh := Arccosh (hh);
      if bad_compt
      then
         return;
      end if;
      acoshg := Arccosh (g);
      if bad_compt
      then
         return;
      end if;
      sqm1 := ((hh) ** 2) - 1.0;
      rsqm1 := Sqrt (sqm1);
      dcdh := (rsqm1 + hh) / (hh * rsqm1 + sqm1);
      sqm1 := ((g) ** 2) - 1.0;
      rsqm1 := Sqrt (sqm1);
      dcdg := (rsqm1 + g) / (g * rsqm1 + sqm1);
      u1 := ((g + 1.0) * ceven - 2.0) / (g - 1.0);
      du1dg := ((g - 1.0) * ceven - ((g + 1.0) * ceven - 2.0)) /
        ((g - 1.0) ** 2);
      u2 := acoshh / acoshg;
      du2dg := (acoshg * dcdh * dhdg - acoshh * dcdg) / ((acoshg) ** 2);
      sqm1 := ((u1) ** 2) - 1.0;
      rsqm1 := Sqrt (sqm1);
      dcdu1 := (rsqm1 + u1) / (u1 * rsqm1 + sqm1);
      sqm1 := ((u2) ** 2) - 1.0;
      rsqm1 := Sqrt (sqm1);
      dcdu2 := (rsqm1 + u2) / (u2 * rsqm1 + sqm1);
      if er > 6.0
      then
         fg := (2.0 * Arccosh (u1) + Arccosh (u2)) / Pi - wo;
         if bad_compt
         then
            return;
         end if;
         dfg := (2.0 * dcdu1 * du1dg + dcdu2 * du2dg) / Pi;
      else
         fg := (2.0 * Arccosh (u1) + 4.0 * Arccosh (u2) / (1.0 + er / 2.0)) /
           Pi - wo;
         if bad_compt
         then
            return;
         end if;
         dfg := (2.0 * dcdu1 * du1dg + 4.0 * dcdu2 * du2dg /
                 (1.0 + er / 2.0)) / Pi;
      end if;
   end error;

   procedure capac (W_h, S_h, er : Long_Float;
                    ce, co : in out Long_Float) is
      cp, cf, cfp, cga, cgd, ere, zo, a : Long_Float;
   begin
      ere := (er + 1.0) / 2.0 + (er - 1.0) / 2.0 / Sqrt (1.0 + 10.0 / W_h);
      if W_h <= 1.0
      then
         zo := 370.0 * Log (8.0 / W_h + 0.25 * W_h) / (2.0 * Pi * Sqrt (ere));
      else
         zo := 370.0 / ((W_h + 1.393 + 0.667 * Log (W_h + 1.44)) * Sqrt (ere));
      end if;
      a := Exp (-0.1 * Exp (2.33 - 2.53 * W_h));
      cp := er * W_h;
      cf := 0.5 * (Sqrt (ere) / (zo / (120.0 * Pi)) - cp);
      cfp := cf * Sqrt (er / ere) / (1.0 + a * Tanh (8.0 * S_h) / S_h);
      cga := kkk (S_h / (S_h + 2.0 * W_h));
      cgd := er * Log (1.0 / Tanh (Pi * S_h / 4.0)) / Pi + 0.65 * cf *
        (0.02 * Sqrt (er) / S_h + 1.0 - 1.0 / ((er) ** 2));
      ce := cp + cf + cfp;
      co := cp + cf + cga + cgd;
   end capac;

   procedure ere_even_odd (W_h, S_h : Long_Float;
                           ee, eo : in out Long_Float) is
      ce1, co1, cee, coe : Long_Float;
   begin
      ce1 := 0.0;
      co1 := 0.0;
      capac (W_h, S_h, 1.0, ce1, co1);
      cee := 0.0;
      coe := 0.0;
      capac (W_h, S_h, er, cee, coe);
      ee := cee / ce1;
      eo := coe / co1;
   end ere_even_odd;

   procedure di (di : in out TComplex; s, t : TComplex) is
   begin
      di.r := s.r - t.r;
      di.i := s.i - t.i;
   end di;

   procedure su (su : in out TComplex; s, t : TComplex) is
   begin
      su.r := s.r + t.r;
      su.i := s.i + t.i;
   end su;

   procedure Get_Lumped_Params (tcompt : compt;
                                v1, v2, v3, v4 : in out Long_Float;
                                u, last_ID, last_prefix : in out Character;
                                alt_param, parallel_cir : in out Boolean) is
      c_string, s_value : Unbounded_String;
      j, long, sign : Integer;
      value, L_value, temp_val, C_value, omega0 : Long_Float;
      found, par_error : Boolean;
      ident, scale_char : Character;

      set1 : constant CharacterArray := ('?', '+', '-', '.', ',', 'j',
         '0', '1', '2', '3', '4', '5', '6', '7', '8', '9');
      set2 : constant CharacterArray := ('.', ',',
         '0', '1', '2', '3', '4', '5', '6', '7', '8', '9');
      set3 : constant CharacterArray := ('j', 'm', 'H', 'F',
         '0', '1', '2', '3', '4', '5', '6', '7', '8', '9');
      set4 : constant CharacterArray := ('y', 'Y', 'z', 'Z', 's', 'S',
         Character (Omega),
         '0', '1', '2', '3', '4', '5', '6', '7', '8', '9');

      --  *
      --  Skip one or more spaces to advance to next
      --  legitimate data or unit value.
      --  *
      procedure skip_space is
      begin
         loop
            --  advance past spaces
            j := j + 1;
            exit when (Element (c_string, j) /= ' ') or else (j = long + 1);
         end loop;
      end skip_space;

      --  ****************************************************
      --  * Get_lumped_params *
      --  convert design Freq to rad/sec times prefix
      --  look past id letter
   begin
      v1 := 0.0;
      v2 := 0.0;
      v3 := 0.0;
      v4 := 0.0;
      u := '?';
      last_ID := ' ';
      last_prefix := ' ';
      L_value := 0.0;
      C_value := 0.0;
      par_error := False;
      parallel_cir := False;
      alt_param := False;
      omega0 := 2.0 * Pi * design_freq * Eng_Prefix (Character (freq_prefix));
      c_string := tcompt.all.descript;
      j := 2;
      if bad_compt
      then
         return;
      end if;
      long := Length (c_string);
      while Element (c_string, long) = ' '
      loop
         long := long - 1;
      end loop;
      --  ignore end blanks
      if Index (c_string, "" & Character (Parallel)) > 0
      then
         parallel_cir := True;
      end if;
      --  * Look for character which represents a parallel circuit *
      for i in 1 .. 4
      loop
         s_value := To_Unbounded_String ("");
         scale_char := ' ';
         found := False;
         if j > long
         then
            --  [BP2P]: Label "100001" Was "exit_get_lumped"
            goto LABEL_100001;
         end if;
         --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
         while not is_in (Element (c_string, j), set1)
         loop
            --  Advance characters until legitimate data found
            j := j + 1;
            if j > long
            then
               --  [BP2P]: Label "100001" Was "exit_get_lumped"
               goto LABEL_100001;
            end if;
         end loop;
         if Element (c_string, j) = '+'
         then
            skip_space;
         end if;
         if Element (c_string, j) = '-'
         then
            skip_space;
            sign := -1;
         else
            sign := 1;
         end if;
         if Element (c_string, j) = 'j'
         then
            skip_space;
            ident := 'j';
         else
            ident := ' ';
         end if;
         --  Check for sweep variable
         if Element (c_string, j) = '?'
         then
            if not alt_param
            then
               alt_param := True;
               skip_space;
            else
               par_error := True;
               goto LABEL_100001;
            end if;
         end if;
         --  * Load string with number characters *
         loop
            --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
            if is_in (Element (c_string, j), set2)
            then
               if Element (c_string, j) = ','
               then
                  s_value := s_value & '.';
               else
                  s_value := s_value & Element (c_string, j);
               end if;
               j := j + 1;
            else
               found := True;
            end if;
            exit when found or else (j = long + 1);
         end loop;
         if Element (c_string, j) = ' '
         then
            skip_space;
         end if;
         --  * Look for engineering prefixes *
         --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
         if is_in (char (Element (c_string, j)), Eng_Dec_Mux)
           and then (j < long)
         then
            --  * ignore 'm' if last character *
            scale_char := Element (c_string, j);
            skip_space;
         end if;
         if is_in (Element (c_string, j), set3) and then (j /= long + 1)
         then
            ident := Element (c_string, j);
            skip_space;
         end if;
         if j <= long
         then
            if is_in (Element (c_string, j), set4)
            then
               u := Element (c_string, j);
               skip_space;
            end if;
         end if;
         --  Val (s_value, value, code);
         --  if (code /= 0) or else (Length (s_value) = 0)
         --  then
         --     if alt_param and then ident /= 'm'
         --     then
         --        --  return 1.0 for uninitialized variables
         --        --  [BP2P]: Label "100001" Was "exit_get_lumped"
         --        value := 1.0;
         --     else
         --        par_error := True;
         --        goto LABEL_100001;
         --     end if;
         --  end if;
         value := Long_Float'Value (To_String (s_value));
         value := value * Long_Float (sign) * Eng_Prefix (scale_char);
         case ident is
            when 'F' =>
               if C_value = 0.0
               then
                  C_value := value;
                  if C_value = 0.0
                  then
                     C_value := 1.0 / infty;
                  end if;
                  --  watch for zero capacitance
                  --  [BP2P]: Label "100001" Was "exit_get_lumped"
               else
                  par_error := True;
                  goto LABEL_100001;
               end if;
            when 'H' =>
               if L_value = 0.0
               then
                  L_value := value;
                  if L_value = 0.0
                  then
                     L_value := 1.0 / infty;
                  end if;
                  --  watch for zero inductance
                  --  [BP2P]: Label "100001" Was "exit_get_lumped"
               else
                  par_error := True;
                  goto LABEL_100001;
               end if;
            when 'j' =>
               if value > 0.0
               then
                  if v2 = 0.0
                  then
                     --  [BP2P]: Label "100001" Was "exit_get_lumped"
                     v2 := value;
                  else
                     par_error := True;
                     goto LABEL_100001;
                  end if;
               else
                  if v3 = 0.0
                  then
                     --  [BP2P]: Label "100001" Was "exit_get_lumped"
                     v3 := value;
                  else
                     par_error := True;
                     goto LABEL_100001;
                  end if;
               end if;
            when 'm' =>
               --  convert from meters to mm
               --  * return v4=0 if solo m i.e. Manhattan *
               v4 := 1000.0 * value;
            when others =>
               if v1 = 0.0
               then
                  --  [BP2P]: Label "100001" Was "exit_get_lumped"
                  v1 := value;
               else
                  par_error := True;
                  goto LABEL_100001;
               end if;
         end case;
         --  case
         --  Save part ID and prefix for alt_param
         --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
         if ident /= ' ' and then ident /= 'm'
         then
            last_ID := ident;
         end if;
         if scale_char /= ' ' and then ident /= 'm'
         then
            last_prefix := scale_char;
         end if;
      end loop;
      --  for i
      <<LABEL_100001>>
      if not par_error
      then
         --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
         if u = 'z' or else u = 'Z' or else u = Character (Omega)
         then
            --  * Swap values if parallel circuit is desired *
            temp_val := v2;
            if v1 /= 0.0
            then
               v1 := 1.0 / v1;
            end if;
            if v3 /= 0.0
            then
               v2 := -1.0 / v3;
            end if;
            if temp_val /= 0.0
            then
               v3 := -1.0 / temp_val;
            end if;
            if u = Character (Omega)
            then
               --  *swap units too *
               u := 'S';
            else
               u := 'y';
            end if;
         end if;
         --  if u in
         --  * Add in capacitor and inductor values *
         if C_value /= 0.0
         then
            case u is
               when Character (Omega) =>
                  v3 := v3 - 1.0 / (omega0 * C_value);
               when 'z' | 'Z' =>
                  v3 := v3 - 1.0 / (Z0 * omega0 * C_value);
               when 'y' | 'Y' =>
                  v2 := v2 + Z0 * omega0 * C_value;
               when 's' | 'S' =>
                  v2 := v2 + omega0 * C_value;
               when others =>
                  --  if 'F' the only unit
                  if parallel_cir
                  then
                     u := 'S';
                     if v1 /= 0.0
                     then
                        v1 := 1.0 / v1;
                     end if;
                     --  assume ohms @ v1 if no units
                     v2 := v2 + omega0 * C_value;
                  else
                     u := Character (Omega);
                     v3 := v3 - 1.0 / (omega0 * C_value);
                  end if;
            end case;
         end if;
         --  if..case
         if L_value /= 0.0
         then
            case u is
               when Character (Omega) =>
                  v2 := v2 + omega0 * L_value;
               when 'z' | 'Z' =>
                  v2 := v2 + omega0 * L_value / Z0;
               when 'y' | 'Y' =>
                  v3 := v3 - Z0 / (omega0 * L_value);
               when 's' | 'S' =>
                  v3 := v3 - 1.0 / (omega0 * L_value);
               when others =>
                  --  if 'H' the only unit
                  if parallel_cir
                  then
                     u := 'S';
                     if v1 /= 0.0
                     then
                        v1 := 1.0 / v1;
                     end if;
                     --  assume ohms @ v1 if no units
                     v3 := v3 - 1.0 / (omega0 * L_value);
                  else
                     u := Character (Omega);
                     v2 := v2 + omega0 * L_value;
                  end if;
                  --  else
            end case;
         end if;
         --  if..case
      end if;
      --  not par_error
      if par_error or else (u = '?')
      then
         ccompt := tcompt;
         bad_compt := True;
         message (1) := To_Unbounded_String ("Error in");
         message (2) := To_Unbounded_String ("lumped element");
         message (3) := To_Unbounded_String ("description");
      end if;
   end Get_Lumped_Params;

   procedure box (x, y, ij : Integer) is
      x1 : Integer := x;
      y1 : Integer := y;
   begin
      case ij is
         when 2 =>
            y1 := y1 - 1;
            x1 := x1 - 1;
         when 3 =>
            x1 := x1 - 1;
         when 4 =>
            y1 := y1 - 1;
         when others =>
            --  [P2Ada]: no otherwise / else in Pascal
            null;
      end case;
      --  case
      PutPixel (Integer_32 (x1), Integer_32 (y1),
                Integer_32 (s_color (ij)));
      PutPixel (Integer_32 (x1 + 1), Integer_32 (y1),
                Integer_32 (s_color (ij)));
      PutPixel (Integer_32 (x1), Integer_32 (y1 + 1),
                Integer_32 (s_color (ij)));
      PutPixel (Integer_32 (x1 + 1), Integer_32 (y1 + 1),
                Integer_32 (s_color (ij)));
   end box;

   procedure lengthxy (tnet : net) is
      lengths, widths : Long_Float;
   begin
      dirn_xy;
      if tnet /= null
      then
         lengths := tnet.all.com.all.lngth;
         widths := tnet.all.com.all.width;
      else
         --  Put (lst);
         Put ("error");
         New_Line;
      end if;
      lengthxm := lengths * Long_Float (abs (xii)) + widths *
                            Long_Float (abs (yii));
      lengthym := lengths * Long_Float (abs (yii)) + widths *
                            Long_Float (abs (xii));
   end lengthxy;

   function ext_port (tcon : conn) return Boolean is
      Result_ext_port : Boolean;
   begin
      Result_ext_port := betweeni (1, tcon.all.port_type, min_ports);
      return Result_ext_port;
   end ext_port;

   function betweeni (x1, x2, x3 : Integer) return Boolean is
      Result_betweeni : Boolean;
   begin
      if (x1 <= x2) and then (x2 <= x3)
      then
         Result_betweeni := True;
      else
         Result_betweeni := False;
      end if;
      return Result_betweeni;
   end betweeni;

   procedure puff_draw (x1, y1, x2, y2, color : Integer) is
   begin
      SetCol (Unsigned_16 (color));
      Line (Integer_32 (x1), Integer_32 (y1),
            Integer_32 (x2), Integer_32 (y2));
   end puff_draw;

   function Manhattan (tcompt : compt) return Boolean is
      Result_Manhattan : Boolean;
      c_string : Unbounded_String;
      long : Integer;
   begin
      if Manhattan_Board
      then
         Result_Manhattan := True;
      else
         c_string := tcompt.all.descript;
         long := Length (c_string);
         while Element (c_string, long) = ' '
         loop
            long := long - 1;
         end loop;
         --  ignore end blanks
         --  * Select Manhattan if last character is an 'M' *
         if Element (c_string, long) = 'M'
         then
            Result_Manhattan := True;
         else
            Result_Manhattan := False;
         end if;
         --  * Select Manhattan if a '?' is present in clines or tline *
         while long > 0
         loop
            if Element (c_string, long) = '?' and then
              (tcompt.all.typ = 'c' or else tcompt.all.typ = 't')
            then
               Result_Manhattan := True;
            end if;
            long := long - 1;
         end loop;
      end if;
      --  else
      return Result_Manhattan;
   end Manhattan;

   function fileexists (note : Boolean;
                        inf : in out File_Type;
                        fname : Unbounded_String) return Boolean
   is
   begin
      if fname = ""
      then
         return False;
      end if;
      Open (inf, In_File, To_String (fname));
      return True;
   exception
      when Name_Error =>
         begin
            if note
            then
               message (2) := To_Unbounded_String ("File not found");
               message (3) := fname;
               Write_Message;
               delay (1000.0);
            end if;
         end;
      return False;
   end fileexists;

   procedure Equate_Zs (z1 : in out TComplex; z2 : TComplex) is
   begin
      z1.r := z2.r;
      z1.i := z2.i;
   end Equate_Zs;

   procedure Matrix_Conv (a : in out s_conv_matrix; n : Integer) is
      --  initialize pointers for c[i,j] b[i,j] and make copy of a[i,j]
      b, c : s_conv_matrix;
      co_0, co_1 : TComplex;
   begin
      co (co_0, 0.0, 0.0);
      co (co_1, 1.0, 0.0);
      for j in 1 .. n
      loop
         for i in 1 .. n
         loop
            if i = j
            then
               --  di is complex difference
               --  su is complex sum
               --  di is complex difference
               --  su is complex sum
               di (b (i, j), co_1, a (i, j));
               su (c (i, j), co_1, a (i, j));
            else
               di (b (i, j), co_0, a (i, j));
               Equate_Zs (c (i, j), a (i, j));
            end if;
         end loop;
      end loop;
      --  * Now B = (I - A) and C = (I + A)  *
      --  * Now B = (I - A) and C = (I + A)^(-1)  *
      --  * Now A = (I - A)(I + A)^(-1)  *
      Matrix_Inversion (c, n);
      Matrix_Mux (a, b, c, n);
   end Matrix_Conv;

   procedure Matrix_Inversion (a : in out s_conv_matrix; n : Integer) is
      --  complex zero
      --  complex one
      --  * LU decomposition of matrix a *
      d : Long_Float;
      co_0, co_1 : TComplex;
      indx : s_conv_index;
      col : s_conv_vector;
      y : s_conv_matrix;
   begin
      co (co_0, 0.0, 0.0);
      co (co_1, 1.0, 0.0);
      d := 0.0;
      LU_Decomp (a, n, indx, d);
      for j in 1 .. n
      loop
         for i in 1 .. n
         loop
            Equate_Zs (col (i), co_0);
         end loop;
         --  fill column with zeros
         --  Put 1+j0 in the proper position
         Equate_Zs (col (j), co_1);
         LU_Sub (a, n, indx, col);
         for i in 1 .. n
         loop
            Equate_Zs (y (i, j), col (i));
         end loop;
      end loop;
      --  * Fill Matrix "a" with data in "y" *
      for j in 1 .. n
      loop
         for i in 1 .. n
         loop
            Equate_Zs (a (i, j), y (i, j));
         end loop;
      end loop;
   end Matrix_Inversion;

   procedure Matrix_Mux (a : in out s_conv_matrix; b, c : s_conv_matrix;
                         n : Integer) is
      sum, co_0 : TComplex;
   begin
      co (co_0, 0.0, 0.0);
      for j in 1 .. n
      loop
         for i in 1 .. n
         loop
            --  initialize sum to zero
            Equate_Zs (sum, co_0);
            for k in 1 .. n
            loop
               supr (sum, b (i, k), c (k, j));
            end loop;
            --  * sum:= sum + b[i,k]*c[k,j] *
            --  fill matrix "a"
            Equate_Zs (a (i, j), sum);
         end loop;
      end loop;
   end Matrix_Mux;

   procedure LU_Decomp (a : in out s_conv_matrix; n : Integer;
                           indx : in out s_conv_index; d : in out Long_Float)
   is
      --  new(vv);
      --  * Loop over rows to get implicit scaling information *
      tiny : constant := 1.0e-20;
      type s_real_vector is array (1 .. Conv_size) of Long_Float;
      imax : Integer;
      sum, dum_z, z_dum : TComplex;
      big, dum_r : Long_Float;
      vv : s_real_vector;
   begin
      d := 1.0;
      for i in 1 .. n
      loop
         big := 0.0;
         for j in 1 .. n
         loop
            if co_mag (a (i, j)) > big
            then
               big := co_mag (a (i, j));
            end if;
         end loop;
         if big = 0.0
         then
            --  if zeros all along the column...
            --  ! This will not cause recovery *
            message (1) := To_Unbounded_String ("Warning!");
            message (2) := To_Unbounded_String ("indef part has");
            message (3) := To_Unbounded_String ("singular matrix");
            Write_Message;
            big := tiny;
         end if;
         --  save the scaling for future reference
         vv (i) := 1.0 / big;
      end loop;
      for j in 1 .. n
      loop
         for i in 1 .. j - 1
         loop
            Equate_Zs (sum, a (i, j));
            for k in 1 .. i - 1
            loop
               diffpr (sum, a (i, k), a (k, j));
            end loop;
            --  * sum=sum-a[i,k]*a[k,j] *
            Equate_Zs (a (i, j), sum);
         end loop;
         big := 0.0;
         for i in j .. n
         loop
            Equate_Zs (sum, a (i, j));
            for k in 1 .. j - 1
            loop
               diffpr (sum, a (i, k), a (k, j));
            end loop;
            --  * sum=sum-a[i,k]*a[k,j] *
            --  * Check here for a better figure of merit for the pivot *
            Equate_Zs (a (i, j), sum);
            dum_r := vv (i) * co_mag (sum);
            if dum_r >= big
            then
               --  *if better, exchange and save index*
               big := dum_r;
               imax := i;
            end if;
         end loop;
         if j /= imax
         then
            --  * Interchange rows if needed *
            for k in 1 .. n
            loop
               Equate_Zs (dum_z, a (imax, k));
               Equate_Zs (a (imax, k), a (j, k));
               Equate_Zs (a (j, k), dum_z);
            end loop;
            --  * Interchange the scale factor *
            d := -d;
            vv (imax) := vv (j);
         end if;
         indx (j) := imax;
         if co_mag (a (j, j)) = 0.0
         then
            a (j, j).r := tiny;
         end if;
         if j /= n
         then
            --  complex reciprocal-creates pointer z_dum
            rc (z_dum, a (j, j));
            for i in j + 1 .. n
            loop
               --  ! is this legal?
               --  *a[i,j] := a[i,j]*z_dum *
               prp (dum_z, a (i, j), z_dum);
               Equate_Zs (a (i, j), dum_z);
            end loop;
         end if;
      end loop;
   end LU_Decomp;

   procedure LU_Sub (a : s_conv_matrix; n : Integer;
                     indx : s_conv_index; b : in out s_conv_vector) is
      ip, ii : Integer;
      sum, dum_z : TComplex;
   begin
      ii := 0;
      for i in 1 .. n
      loop
         --  sum := b[ip]
         --  b[ip] := b[i]
         ip := indx (i);
         Equate_Zs (sum, b (ip));
         Equate_Zs (b (ip), b (i));
         if ii /= 0
         then
            for j in ii .. i - 1
            loop
               diffpr (sum, a (i, j), b (j));
            end loop;
            --  sum := sum-a[i,j]*b[j]
         else
            if co_mag (sum) /= 0.0
            then
               ii := i;
            end if;
         end if;
         Equate_Zs (b (i), sum);
      end loop;
      for i in reverse 1 .. n
      loop
         Equate_Zs (sum, b (i));
         for j in i + 1 .. n
         loop
            diffpr (sum, a (i, j), b (j));
         end loop;
         --  sum := sum-a[i,j]*b[j]
         --  complex reciprocal - new pointer
         --  ! is this legal?
         rc (dum_z, a (i, i));
         prp (b (i), sum, dum_z);
      end loop;
   end LU_Sub;

   procedure supr (vu : in out TComplex; vX, vY : TComplex) is
   begin
      vu.r := vu.r + vX.r * vY.r - vX.i * vY.i;
      vu.i := vu.i + vX.r * vY.i + vX.i * vY.r;
   end supr;

   function co_mag (z : TComplex) return Long_Float is
      Result_co_mag : Long_Float;
   begin
      Result_co_mag := Sqrt (((z.r) ** 2) + ((z.i) ** 2));
      return Result_co_mag;
   end co_mag;

   procedure diffpr (vu : in out TComplex; vX, vY : TComplex) is
   begin
      vu.r := vu.r - vX.r * vY.r + vX.i * vY.i;
      vu.i := vu.i - vX.r * vY.i - vX.i * vY.r;
   end diffpr;

   procedure move_cursor (x1, y1 : Integer) is
      has_spaces : Boolean;
      long : Integer;
   begin
      has_spaces := False;
      if x1 /= 0
      then
         --  [P2Ada]: WITH instruction
         declare P2Ada_Var_3 : compt_record renames ccompt.all;
         begin
            long := Length (P2Ada_Var_3.descript);
            if cx + x1 <= long
            then
               cx := cx + x1;
               if cx < P2Ada_Var_3.x_block
               then
                  cx := P2Ada_Var_3.x_block;
               end if;
               if P2Ada_Var_3.right and then
                 (cx + P2Ada_Var_3.xp >= P2Ada_Var_3.xorig)
               then
                  if window_number = 2
                  then
                     WindMax := WindMax + 1;
                  end if;
                  --  Increment WindMax to prevent scrolling in plot window
                  P2Ada_Var_3.xp := P2Ada_Var_3.xp - 1;
                  write_compt (LightGray, ccompt);
                  Put (' ');
                  if window_number = 2
                  then
                     WindMax := WindMax - 1;
                  end if;
                  --  Restore WindMax
               end if;
            end if;
         end;
      else
         Erase_Message;
         if Index (ccompt.all.descript, " ") /= 0
         then
            has_spaces := True;
         end if;
         if (window_number = 2) and then (((Length (ccompt.all.descript)) < 3)
                                          or else (has_spaces))
         then
            for i in 1 .. max_params
            loop
               --  Delete invalid S-parameter designations
               if s_param_table (i) = ccompt
               then
                  --  erase invalid s-parameter on screen
                  Delete (ccompt.all.descript, 2, 2);
                  GotoXY (Integer_32 (ccompt.all.xp - 2),
                          Integer_32 (ccompt.all.yp));
                  Put ("     ");
               end if;
            end loop;
         end if;
         if y1 /= 0
         then
            --  if y1=0 then skip the following
            if y1 = -1
            then
               if ccompt.all.prev_compt = null
               then
                  beep;
               else
                  ccompt := ccompt.all.prev_compt;
               end if;
               --  y1=1
            else
               if (ccompt.all.next_compt = null) or else
                 (not (Large_Parts)
                  and then (window_number = 3)
                  and then (Character'Pos (Element (ccompt.all.descript, 1)) -
                                Character'Pos ('a') >= ymax (3) -
                                  ymin (3))) or else
                 (Large_Parts and then (window_number = 3)
                  and then
                    (Character'Pos (Element (ccompt.all.descript, 1)) -
                         Character'Pos ('a') >= ymax (3) - ymin (3)) and then
                  help_displayed)
               then
                  beep;
               else
                  ccompt := ccompt.all.next_compt;
               end if;
            end if;
            --  y1=-1
            if (window_number = 2) and then (Length (ccompt.all.descript) = 1)
            then
               for i in 1 .. max_params
               loop
                  if s_param_table (i) = ccompt
                  then
                     --  write "S"
                     pattern (xmin (2) * charx - 1, (ymin (2) + 2 + i) *
                                chary - 8, i, 0);
                     write_comptm (1, LightGray, ccompt);
                  end if;
               end loop;
            end if;
            long := Length (ccompt.all.descript);
            if cx > long
            then
               cx := long;
            end if;
            if window_number = 2
            then
               cx := ccompt.all.x_block;
            end if;
            if cx < ccompt.all.x_block
            then
               cx := ccompt.all.x_block;
            end if;
         end if;
         --  if y1 <> 0
      end if;
      --  else
   end move_cursor;

   procedure pattern (x1, y1, ij, pij : Integer) is
      pragma Unreferenced (pij);
   begin
      SetCol (Unsigned_16 (s_color (ij)));
      case ij is
         when 1 =>
            --  draw the box
            Rectangle (Integer_32 (x1 - 3),
                       Integer_32 (y1 - 3),
                       Integer_32 (x1 + 3),
                       Integer_32 (y1 + 3));
         when 2 =>
            --  draw X
            Line (Integer_32 (x1 - 3),
                  Integer_32 (y1 - 3),
                  Integer_32 (x1 + 3),
                  Integer_32 (y1 + 3));
            Line (Integer_32 (x1 - 3),
                  Integer_32 (y1 + 3),
                  Integer_32 (x1 + 3),
                  Integer_32 (y1 - 3));
         when 3 =>
            --  draw diamond
            Line (Integer_32 (x1 - 4),
                  Integer_32 (y1),
                  Integer_32 (x1),
                  Integer_32 (y1 + 4));
            Line (Integer_32 (x1 - 3),
                  Integer_32 (y1 - 1),
                  Integer_32 (x1),
                  Integer_32 (y1 - 4));
            Line (Integer_32 (x1 + 4),
                  Integer_32 (y1),
                  Integer_32 (x1 + 1),
                  Integer_32 (y1 + 3));
            Line (Integer_32 (x1 + 3),
                  Integer_32 (y1 - 1),
                  Integer_32 (x1 + 1),
                  Integer_32 (y1 - 3));
         when 4 =>
            --  draw +
            Line (Integer_32 (x1 - 4),
                  Integer_32 (y1),
                  Integer_32 (x1 + 4),
                  Integer_32 (y1));
            Line (Integer_32 (x1),
                  Integer_32 (y1 + 4),
                  Integer_32 (x1),
                  Integer_32 (y1 - 4));
         when others =>
            null;
      end case;
   end pattern;

end pfun1;
