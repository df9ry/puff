with Ada.Directories;

package body pfun1 is

   --  *
   --  Create a complex number type.
   --  *

   procedure co (co : in out TComplex; s, t : Long_Float) is
   begin
      co.r := s;
      co.i := t;
   end co;

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

   function File_Exists (note : Boolean;
                         fname : Unbounded_String) return Boolean is
   begin
      if fname = "" or else not Ada.Directories.Exists (To_String (fname))
      then
         return False;
      end if;
      if note
      then
         message (2) := To_Unbounded_String ("File not found");
         message (3) := fname;
         Write_Message;
         C_Delay (1000);
      end if;
      return False;
   end File_Exists;

   function Setup_Exists (fname : in out Unbounded_String) return Boolean is
      Result_setupexists : Boolean;
      found : Boolean;
   begin
      found := False;
      message (2) := fname;
      if fname /= "setup.puf"
      then
         fname := To_Unbounded_String ("setup.puf");
         found := File_Exists (False, fname);
      end if;
      if not (found)
      then
         fname := To_Unbounded_String ("/PUFF/setup.puf");
         found := File_Exists (False, fname);
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
   end Setup_Exists;

   --  Called when a disastrous error condition
   --  has been reached to stop Puff.
   --  *
   procedure shutdown is
   begin
      CloseGraph;
      TextMode (Integer_32 (OrigMode));
      message (1) := To_Unbounded_String ("FATAL ERROR:");
      Write_Message;
      GotoXY (1, 23);
      PutStr (To_C ("Press any key to quit"));
      loop
         exit when KeyPressed /= 0;
      end loop;
      raise Program_halted with "Within shutdown";
   end shutdown;

   procedure Check_Reset (Self : in out Sweep'class; tcompt : compt) is
   begin
      if (Self.element = tcompt) and then tcompt.all.changed
      then
         Init_Use (Self);
         tcompt.all.sweep_compt := False;
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

   function Eng_Prefix (c : char) return Long_Float is
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
         when Mu =>
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
         Self.Omega0 := 2.0 * Pi * design_freq * Eng_Prefix (freq_prefix);
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

end pfun1;
