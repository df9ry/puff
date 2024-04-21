
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Utils;                 use Utils;

package body pfmsc is

   procedure Device_Read (tcompt : compt; indef : Boolean) is
      fname : Unbounded_String;
      c_ss, c_f : s_param;
      template : Unbounded_String;
      ext_string : Unbounded_String;
      first_char : Character;
      char1, char2 : Character;
      freq_present, Eesof_format : Boolean;
      i, j, number_of_s, code1, number_of_ports, Eesof_ports : Integer;
      f1, mag, ph : Long_Float;
      --  *
      --  Partition template. Extract number of connectors and frequencies.
      --  template (tp:file_string) is in the form ' f  s11  s21  s12  s22 '
      --  *

      procedure Pars_tplate (tp : Unbounded_String; n_c, n_s : in out Integer;
                             f_p : in out Boolean) is
         i, i1, i2, x : Integer;
         ijc : array (1 .. 16) of Unbounded_String;
         tp1 : Unbounded_String :=  tp;
      begin
         if (Index (tp, "f") > 0) or else (Index (tp, "F") > 0)
         then
            f_p := True;
         else
            f_p := False;
         end if;
         n_s := 0;
         loop
            i1 := Index (tp, "s");
            i := i1;
            i2 := Index (tp, "S");
            if i1 < i2
            then
               if i1 > 0
               then
                  i := i1;
               else
                  i := i2;
               end if;
            else
               if i2 > 0
               then
                  i := i2;
               else
                  i := i1;
               end if;
            end if;
            if i > 0
            then
               Delete (tp1, 1, i);
               n_s := n_s + 1;
               if Length (tp1) >= 2
               then
                  ijc (n_s) := tp1;
                  Delete (tp1, 1, 2);
               end if;
            end if;
            exit when Length (tp1) = 0 or else i = 0;
         end loop;
         n_c := 1;
         for i in 1 .. n_s
         loop
            x := Integer'Value (To_String (ijc (i)));
            --  Val (ijc (i), x, code);
            --  if code /= 0
            --  then
            --     bad_compt := True;
            --     message (1) := To_Unbounded_String ("Bad port number");
            --     message (2) := To_Unbounded_String ("in device");
            --     message (3) := To_Unbounded_String ("file template");
            --     return;
            --  end if;
            iji (i, 1) := x / 10;
            iji (i, 2) := x - iji (i, 1) * 10;
            if (iji (i, 1) < 1) or else (iji (i, 2) < 1)
            then
               bad_compt := True;
               message (1) := To_Unbounded_String ("0 port number");
               message (2) := To_Unbounded_String ("in device");
               message (3) := To_Unbounded_String ("file template");
               return;
            end if;
            if iji (i, 1) > n_c
            then
               n_c := iji (i, 1);
            end if;
            if iji (i, 2) > n_c
            then
               n_c := iji (i, 2);
            end if;
            for j in 1 .. i - 1
            loop
               if (iji (i, 1) = iji (j, 1)) and then (iji (i, 2) = iji (j, 2))
               then
                  bad_compt := True;
                  message (1) := To_Unbounded_String ("Repeated sij");
                  message (2) := To_Unbounded_String ("in device");
                  message (3) := To_Unbounded_String ("file template");
                  return;
               end if;
            end loop;
         end loop;
         --  for i := 1 to n_s
         if n_s = 0
         then
            bad_compt := True;
            message (1) := To_Unbounded_String ("No port numbers");
            message (2) := To_Unbounded_String ("in device");
            message (3) := To_Unbounded_String ("file template");
         end if;
      end Pars_tplate;
      --  Pars_tplate
      --  ********************************************************
      --  *
      --  Read s-parameter values from files.
      --  *

      procedure Read_Number (s : in out Long_Float) is
         --  first_char is the very first valid file character
         ss : Unbounded_String;
         char1 : Character;
         code : Integer;
         found : Boolean;
         set1 : constant CharacterArray :=
           ('+', '-', '.', ',', 'e', 'E',
            '0', '1', '2', '3', '4', '5', '6', '7', '8', '9');
      begin
         ss := To_Unbounded_String ("" & first_char);
         found := False;
         if ss = ""
         then
            --  search for first valid character if not in first_char
            if not (End_of_File (dev_file))
            then
               --  goto next number
               loop
                  --  keep reading characters until a valid one is found
                  if SeekEoln (dev_file)
                  then
                     --  [P2Ada]: !Help! Maybe (file)
                     Get (dev_file);
                     Skip_Line;
                  end if;
                  Get (dev_file);
                  Get (char1);
                  if char1 = Character (lbrack) or else
                     char1 = '#' or else char1 = '!'
                  then
                     Get (dev_file);
                     Skip_Line;
                  end if;
                  --  skip potential comment lines
                  --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
                  if is_in (char1, set1)
                  then
                     ss := To_Unbounded_String ("" & char1);
                     found := True;
                  end if;
                  exit when found or else End_of_File (dev_file);
               end loop;
            end if;
         end if;
         found := False;
         if not End_of_File (dev_file)
         then
            loop
               --  continue reading characters and add to string ss until
               --  invalid
               Get (dev_file);
               Get (char1);
               if is_in (char1, set1)
               then
                  ss := ss & char1;
               else
                  found := True;
               end if;
               exit when found or else End_Of_Line (dev_file) or else
                 End_Of_File (dev_file);
            end loop;
         end if;
         --  turn string ss into double number
         s := Long_Float'Value (To_String (ss));
         --  Val (ss, s, code);
         --  if code /= 0 or else Length (ss) = 0
         --  then
         --     bad_compt := True;
         --     message (1) := To_Unbounded_String ("Extra or missing");
         --     message (2) := To_Unbounded_String ("number in");
         --     message (3) := To_Unbounded_String ("device file");
         --  end if;
         --  if code <> 0
      end Read_Number;

      procedure Seek_File_Start (temp_exists : Boolean) is
         char1 : Character;
         found : Boolean;
         set1 : constant CharacterArray := (
            '+', '-', '.', ',',
            '0', '1', '2', '3', '4', '5', '6', '7', '8', '9');
      begin
         found := False;
         first_char := ' ';
         loop
            --  keep reading characters until a valid one is found
            while SeekEoln (dev_file)
            loop
               --  [P2Ada]: !Help! Maybe (file)
               Get (dev_file);
               Skip_Line;
            end loop;
            --  Advance past any blank lines
            loop
               Get (dev_file);
               Get (char1);
               exit when (char1 /= ' ') or else End_of_File (dev_file);
            end loop;
            --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
            if char1 = Character (lbrack) or else
               char1 = '#' or else char1 = '!'
            then
               --  [P2Ada]: !Help! Maybe (file)
               Get (dev_file);
               Skip_Line;
            end if;
            --  Skip lines with comments
            if temp_exists
            then
               --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
               if char1 = 'f' or else char1 = 'F' or else
                  char1 = 's' or else char1 = 'S'
               then
                  first_char := char1;
                  found := True;
               end if;
               --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
            else
               if is_in (char1, set1)
               then
                  first_char := char1;
                  found := True;
               end if;
            end if;
            exit when found or else End_of_File (dev_file);
         end loop;
      end Seek_File_Start;

   begin
      Get_Device_Params (tcompt, fname, tcompt.all.lngth);
      if bad_compt
      then
         return;
      end if;
      --  ! length check moved from this location
      Eesof_format := False;
      i := Index (fname, ".");
      if i = 0
      then
         --  add .dev extension
         --  Check for Eesof type extension
         --  copy 3 character extension
         fname := fname & ".dev";
      else
         ext_string := Copy (fname, i + 1, 3);
         if Length (ext_string) = 3
         then
            if (ext_string (1) = 's' or else ext_string (1) = 'S')
              and then (ext_string (2) and ('1' .. '4' => True,
              others => False)) and then (ext_string (3) and
              ('p' | 'P' => True, others => False))
            then
               Val (ext_string (2), Eesof_ports, code1);
               if code1 = 0
               then
                  Eesof_format := True;
               end if;
            end if;
            --  eesof check
         end if;
         --  length check
      end if;
      --  ext check
      if (tcompt.all.f_file = null) or else tcompt.all.changed
      then
         if fileexists (True, dev_file, fname)
         then
            --  [P2Ada]: WITH instruction
            --  [P2Ada]: !Help! No type found -> add 'P2Ada_Var_1.' to fields
            declare P2Ada_Var_1 : compt_record renames tcompt.all;
            begin
               if Eesof_format
               then
                  --  Skip lines looking for start of data
                  --  ! WARNING Seek_File_Start stores the first
                  --  data character in first_char!
                  --  * set up iji[] array *
                  Seek_File_Start (False);
                  number_of_ports := Eesof_ports;
                  number_of_s := ((number_of_ports) ** 2);
                  freq_present := True;
                  for i in 1 .. number_of_ports
                  loop
                     for j in 1 .. number_of_ports
                     loop
                        iji (number_of_ports * (i - 1) + j, 1) := i;
                        iji (number_of_ports * (i - 1) + j, 2) := j;
                     end loop;
                  end loop;
                  --  * Must correct for 2-ports since their order is goofy *
                  if number_of_ports = 2
                  then
                     iji (2, 1) := 2;
                     iji (2, 2) := 1;
                     iji (3, 1) := 1;
                     iji (3, 2) := 2;
                  end if;
               else
                  while SeekEoln (dev_file)
                  loop
                     --  [P2Ada]: !Help! Maybe (file)
                     Get (dev_file);
                     Skip_Line;
                  end loop;
                  --  Advance past any blank lines
                  --  [P2Ada]: !Help! Maybe (file,...) here
                  --  [P2Ada]: !Help! Maybe (file)
                  --  read first line string
                  Get (dev_file);
                  Get (template);
                  Skip_Line;
                  if Pos ("\b", template) > 0
                  then
                     --  * if a .PUF file *
                     loop
                        --  * then move to \s section *
                        --  [P2Ada]: !Help! Maybe (file,...) here
                        --  [P2Ada]: !Help! Maybe (file,...) here
                        --  [P2Ada]: !Help! Maybe (file)
                        --  [P2Ada]: "x in y" -> "x and y" redefine "and"
                        --  before
                        Get (dev_file);
                        Get (char1);
                        Get (char2);
                        Skip_Line;
                        exit when ((char1 = '\') and then
                                     (char2 = 's' or else char2 = 'S')) or else
                           End_Of_File (dev_file);
                     end loop;
                     if End_of_File (dev_file)
                     then
                        --  [BP2P]: Label "100001" Was "read_finish"
                        bad_compt := True;
                        message (1) := To_Unbounded_String ("s-parameters");
                        message (2) := To_Unbounded_String ("not found in");
                        message (3) := To_Unbounded_String ("device file");
                        goto LABEL_100001;
                     end if;
                  end if;
                  --  if Pos('\b')
                  --  now check for valid template = e.g. ' f   s11  s21  s12
                  --  s22 '
                  while Element (template, 1) = ' '
                  loop
                     Delete (template, 1, 1);
                  end loop;
                  --  delete leading blanks
                  --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
                  if (template (1) and ('f' | 'F' | 's' | 'S' => True, 
                      others => False))
                  then
                     --  have a potentially valid template, get info
                     --  Skip lines looking for template
                     Pars_tplate (template, number_of_ports, number_of_s,
                                  freq_present);
                  else
                     Seek_File_Start (True);
                     if End_of_File (dev_file)
                     then
                        --  [BP2P]: Label "100001" Was "read_finish"
                        bad_compt := True;
                        message (1) := To_Unbounded_String ("template");
                        message (2) := To_Unbounded_String ("not found in");
                        message (3) := To_Unbounded_String ("device file");
                        goto LABEL_100001;
                     end if;
                     --  [P2Ada]: !Help! Maybe (file,...) here
                     --  [P2Ada]: !Help! Maybe (file)
                     --  Put back character removed by Seek_File_Start
                     --  This to initialize Read_Number
                     --  get info from template
                     Get (dev_file);
                     Get (template);
                     Skip_Line;
                     Insert (first_char, template, 1);
                     first_char := ' ';
                     Pars_tplate (template, number_of_ports, number_of_s,
                                  freq_present);
                  end if;
                  --  end template search
                  --  * Number_of_ports is how many are in the file *
                  --  * Number_of_con is how many will result *
               end if;
               --  else Eesof_format
               if indef
               then
                  number_of_con := number_of_ports + 1;
               else
                  number_of_con := number_of_ports;
               end if;
               --  * Initialize sdevice elements to zero *
               for j in 1 .. number_of_con
               loop
                  for i in 1 .. number_of_con
                  loop
                     sdevice (i, j).r := 0;
                     sdevice (i, j).i := 0;
                  end loop;
               end loop;
               --  width:=0;
               --  ! This section moved from 4th line--get_device returns 0 for
               --  Manh
               if Manhattan (tcompt) or else (tcompt.lngth = 0)
               then
                  if number_of_con > 1
                  then
                     tcompt.all.lngth := Manh_length * (number_of_con - 1);
                  else
                     tcompt.all.lngth := Manh_length;
                  end if;
               end if;
               --  symmetrical
               tcompt.all.width := tcompt.all.lngth;
               if tcompt.all.lngth <= resln
               then
                  --  [BP2P]: Label "100001" Was "read_finish"
                  bad_compt := True;
                  message (1) := To_Unbounded_String ("Device length");
                  message (2) := To_Unbounded_String ("must be");
                  message (3) := To_Unbounded_String (">" & sresln'Image);
                  goto LABEL_100001;
               end if;
               --  use f1 to detect start of noise parameters
               --  * LOOP to read in s-parameters from file *
               con_space := 0.0;
               c_ss := null;
               c_f := null;
               f1 := -1.0;
               loop
                  if freq_present
                  then
                     if c_f = null
                     then
                        New_s (tcompt.all.f_file);
                        c_f := tcompt.all.f_file;
                     else
                        New_s (c_f.all.next_s);
                        c_f := c_f.all.next_s;
                     end if;
                     --  c_f=nil
                     --  Must set up next call to Read_Number in case
                     --  on the first pass first_char was set. This
                     --  only occurs with Eesof files, effecting only
                     --  the first frequency data point
                     --  * Compare with last freq. for start of noise
                     --  parameters *
                     --  if last frequency was larger
                     c_f.all.next_s := null;
                     New_c (c_f.all.z);
                     Read_Number (c_f.all.z.all.c.r);
                     first_char := "";
                     if (f1 > c_f.all.z.all.c.r) or else bad_compt
                     then
                        --  have reached EOF
                        --  [BP2P]: Label "100001" Was "read_finish"
                        Erase_Message;
                        bad_compt := False;
                        goto LABEL_100001;
                     end if;
                     --  if bad_compt
                     --  Save last freq point
                     --  if freq_present=true
                     f1 := c_f.all.z.all.c.r;
                  else
                     tcompt.all.f_file := null;
                  end if;
                  --  end else
                  for i in 1 .. number_of_s
                  loop
                     Read_Number (mag);
                     if bad_compt
                     then
                        if (i = 1) and then not (freq_present)
                        then
                           --  reached EOF
                           Erase_Message;
                           bad_compt := False;
                        end if;
                        --  if i=1
                        --  [BP2P]: Label "100001" Was "read_finish"
                        goto LABEL_100001;
                     end if;
                     --  if bad_compt=true
                     Read_Number (ph);
                     if bad_compt
                     then
                        --  [BP2P]: Label "100001" Was "read_finish"
                        goto LABEL_100001;
                     end if;
                     sdevice (iji (i, 1), iji (i, 2)).r := one * mag * cos (ph * pi / 180);
                     sdevice (iji (i, 1), iji (i, 2)).i := one * mag * sin (ph * pi / 180);
                  end loop;
                  --  for i:=1 to number_of_s
                  --  * Here sdevice[] is filled with s-parameters
                  --  for a single frequency. Indef_Matrix will
                  --  generate additional scattering parameters
                  --  to fill an additional port number.    *
                  if indef
                  then
                     Indef_Matrix (sdevice, number_of_ports);
                  end if;
                  for j in 1 .. number_of_con
                  loop
                     for i in 1 .. number_of_con
                     loop
                        if c_ss = null
                        then
                           New_s (tcompt.all.s_file);
                           c_ss := tcompt.all.s_file;
                        else
                           New_s (c_ss.all.next_s);
                           c_ss := c_ss.all.next_s;
                        end if;
                        --  fill parameters
                        c_ss.all.next_s := null;
                        New_c (c_ss.all.z);
                        c_ss.all.z.all.c.r := sdevice (i, j).r;
                        c_ss.all.z.all.c.i := sdevice (i, j).i;
                     end loop;
                  end loop;
                  --  for j,i:= 1 to number_of_con
                  exit when End_of_File (dev_file);
               end loop;
               --  end repeat
            end;
            --  [P2Ada]: end of WITH
            --  with
            --  if (tcompt^.f_file = nil) and fileexists=true
            --  if fileexists = false
            <<LABEL_100001>>
            close (dev_file);
         else
            bad_compt := True;
         end if;
      end if;
   end Device_Read;

end pfmsc;
