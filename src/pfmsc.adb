
with Ada.Numerics.Long_Elementary_Functions;
use  Ada.Numerics.Long_Elementary_Functions;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;

with Interfaces.C;          use Interfaces.C;

with Utils;
with StringUtils;

package body pfmsc is

   procedure Get_Device_Params (tcompt : compt;
                                fname : in out Unbounded_String;
                                len : in out Long_Float) is
      potential_numbers : constant Utils.CharacterArray :=
         ('+', '-', '.', ',',
          '0', '1', '2', '3', '4', '5', '6', '7', '8', '9');
      p1, p2, i, j, long : Integer;
      c_string, s_value : Unbounded_String;
      found_value : Boolean;
   begin
      len := 0.0;
      fname := tcompt.all.descript;
      for j in 1 .. 2
      loop
         --  find index for blanks in descript
         --  delete through blanks in descript
         p1 := Index (fname, " ");
         Delete (fname, 1, p1);
      end loop;
      --  get string length
      --  now fname = 'fsc10 2mm  '
      p2 := Length (fname);
      while Element (fname, p2) = ' '
      loop
         --  remove any blanks at end
         Delete (fname, p2, 1);
         p2 := p2 - 1;
      end loop;
      --  here fname = 'fsc10 2mm' or 'fsc10'
      --  now c_string = 'fsc10 2mm'
      p1 := Index (fname, " ");
      c_string := fname;
      if p2 = 0
      then
         ccompt := tcompt;
         bad_compt := True;
         message (1) := To_Unbounded_String ("Invalid device");
         message (2) := To_Unbounded_String ("specification");
         return;
      else
         if p1 > 0
         then
            Delete (fname, p1, p2);
         end if;
      end if;
      --  now fname = 'fsc10'
      if not (Manhattan (tcompt) or else p1 = 0)
      then
         --  now c_string = '2mm'
         Delete (c_string, 1, p1);
         long := Length (c_string);
         while Element (c_string, long) = ' '
         loop
            long := long - 1;
         end loop;
         --  remove blanks at end
         found_value := False;
         s_value := To_Unbounded_String ("");
         j := 1;
         while (Element (c_string, j) = ' ') and then (j < long + 1)
         loop
            j := j + 1;
         end loop;
         --  Skip spaces
         loop
            --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
            if Utils.is_in (Element (c_string, j), potential_numbers)
            then
               if not (Element (c_string, j) = '+')
               then
                  --  ignore +
                  if Element (c_string, j) = ','
                  then
                     --  . for ,
                     s_value := s_value & '.';
                  else
                     s_value := s_value & Element (c_string, j);
                  end if;
               end if;
               --  + check
               j := j + 1;
            else
               found_value := True;
            end if;
            exit when found_value or else (j = long + 1);
         end loop;
         --  convert string to double number
         len := Long_Float'Value (To_String (s_value));
         --  Val (s_value, len, code);
         --  if (code /= 0) or else (Pos ('m', c_string) = 0)
         --    or else (long = 0)
         --  then
         --     ccompt := tcompt;
         --     bad_compt := true;
         --     message (1) := "Invalid length";
         --     message (2) := "or filename";
         --     return;
         --  end if;
         --  Here j is right of the number
         while (Element (c_string, j) = ' ') and then (j < long + 1)
         loop
            j := j + 1;
         end loop;
         --  Skip spaces
         --  * if j=long then j must point to an 'm' *
         --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
         if Utils.is_in (To_C (Element (c_string, j)), Eng_Dec_Mux)
           and then j < long
         then
            if Element (c_string, j) = 'm'
            then
               --  is 'm' a unit or prefix?
               i := j + 1;
               while (Element (c_string, i) = ' ') and then i < long + 1
               loop
                  i := i + 1;
               end loop;
               --  * Skip spaces to check for some unit *
               if Element (c_string, i) = 'm'
               then
                  --  it's the prefix milli 'm' next to an 'm'
                  --  make j point past the prefix, to the unit
                  len := Eng_Prefix ('m') * len;
                  j := i;
               end if;
               --  if 'm' is a unit do nothing
               --  if other than 'm' factor in prefix
               --  advance from prefix toward unit
            else
               len := Eng_Prefix (Element (c_string, j)) * len;
               j := j + 1;
            end if;
         end if;
         --  if in Eng_Dec_Mux
         --  return length in millimeters, not meters
         len := 1000.0 * len;
         while Element (c_string, j) = ' ' and then j < long + 1
         loop
            j := j + 1;
         end loop;
         if len < 0.0 or else Element (c_string, j) /= 'm'
         then
            ccompt := tcompt;
            bad_compt := True;
            message (1) := To_Unbounded_String ("Negative length");
            message (2) := To_Unbounded_String ("or invalid unit");
            return;
         end if;
      end if;
      --  if not Manhattan
   end Get_Device_Params;

   procedure Device_Read (tcompt : compt; indef : Boolean) is
      fname : Unbounded_String;
      c_ss, c_f : s_param;
      template : Unbounded_String;
      ext_string : Unbounded_String;
      first_char : Character;
      char1, char2 : Character;
      freq_present, Eesof_format : Boolean;
      i, number_of_s, number_of_ports, Eesof_ports : Integer;
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

      procedure Read_Number (s : in out Long_Float) is
         --  first_char is the very first valid file character
         ss : Unbounded_String;
         char1 : Character;
         --  code : Integer;
         found : Boolean;
         set1 : constant Utils.CharacterArray :=
           ('+', '-', '.', ',', 'e', 'E',
            '0', '1', '2', '3', '4', '5', '6', '7', '8', '9');
      begin
         ss := To_Unbounded_String ("" & first_char);
         found := False;
         if ss = ""
         then
            --  search for first valid character if not in first_char
            if not (End_Of_File (dev_file))
            then
               --  goto next number
               loop
                  --  keep reading characters until a valid one is found
                  if Utils.SeekEoln (dev_file)
                  then
                     Utils.Skip_Line (dev_file);
                  end if;
                  Get (dev_file, char1);
                  if char1 = Character (lbrack) or else
                     char1 = '#' or else char1 = '!'
                  then
                     Utils.Skip_Line (dev_file);
                  end if;
                  if Utils.is_in (char1, set1)
                  then
                     ss := To_Unbounded_String ("" & char1);
                     found := True;
                  end if;
                  exit when found or else End_Of_File (dev_file);
               end loop;
            end if;
         end if;
         found := False;
         if not End_Of_File (dev_file)
         then
            loop
               --  continue reading characters and add to string ss until
               --  invalid
               Get (dev_file, char1);
               if Utils.is_in (char1, set1)
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
         set1 : constant Utils.CharacterArray := (
            '+', '-', '.', ',',
            '0', '1', '2', '3', '4', '5', '6', '7', '8', '9');
      begin
         found := False;
         first_char := ' ';
         loop
            while Utils.SeekEoln (dev_file)
            loop
               Utils.Skip_Line (dev_file);
            end loop;
            loop
               Get (dev_file, char1);
               exit when (char1 /= ' ') or else End_Of_File (dev_file);
            end loop;
            if char1 = Character (lbrack) or else
               char1 = '#' or else char1 = '!'
            then
               Utils.Skip_Line (dev_file);
            end if;
            if temp_exists
            then
               if char1 = 'f' or else char1 = 'F' or else
                  char1 = 's' or else char1 = 'S'
               then
                  first_char := char1;
                  found := True;
               end if;
            else
               if Utils.is_in (char1, set1)
               then
                  first_char := char1;
                  found := True;
               end if;
            end if;
            exit when found or else End_Of_File (dev_file);
         end loop;
      end Seek_File_Start;

   begin
      Get_Device_Params (tcompt, fname, tcompt.all.lngth);
      if bad_compt
      then
         return;
      end if;
      Eesof_format := False;
      i := Index (fname, ".");
      if i = 0
      then
         fname := fname & ".dev";
      else
         ext_string := StringUtils.Copy (fname, i + 1, 3);
         if Length (ext_string) = 3
         then
            if (Element (ext_string, 1) = 's'
                or else Element (ext_string, 1) = 'S')

              and then (Element (ext_string, 2) >= '1'
                        and then Element (ext_string, 2) <= '4')

              and then (Element (ext_string, 3) = 'p'
                        or else Element (ext_string, 3) = 'P')
            then
               Eesof_ports := Integer'Value ("" & Element (ext_string, 2));
               --  Val (ext_string (2), Eesof_ports, code1);
               --  if code1 = 0
               --  then
               --     Eesof_format := True;
               --  end if;
            end if;
         end if;
      end if;
      if (tcompt.all.f_file = null) or else tcompt.all.changed
      then
         if fileexists (True, dev_file, fname)
         then
            declare P2Ada_Var_1 : compt_record renames tcompt.all;
            begin
               if Eesof_format
               then
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
                  if number_of_ports = 2
                  then
                     iji (2, 1) := 2;
                     iji (2, 2) := 1;
                     iji (3, 1) := 1;
                     iji (3, 2) := 2;
                  end if;
               else
                  while Utils.SeekEoln (dev_file)
                  loop
                     Skip_Line (dev_file);
                  end loop;
                  Utils.Get (dev_file, template);
                  Skip_Line (dev_file);
                  if Index (template, "\b") > 0
                  then
                     loop
                        Get (dev_file, char1);
                        Get (dev_file, char2);
                        Skip_Line (dev_file);
                        exit when ((char1 = '\') and then
                                     (char2 = 's' or else char2 = 'S')) or else
                           End_Of_File (dev_file);
                     end loop;
                     if End_Of_File (dev_file)
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
                  if Element (template, 1) = 'f' or else
                     Element (template, 1) = 'F' or else
                     Element (template, 1) = 's' or else
                     Element (template, 1) = 'S'
                  then
                     Pars_tplate (template, number_of_ports, number_of_s,
                                  freq_present);
                  else
                     Seek_File_Start (True);
                     if End_Of_File (dev_file)
                     then
                        bad_compt := True;
                        message (1) := To_Unbounded_String ("template");
                        message (2) := To_Unbounded_String ("not found in");
                        message (3) := To_Unbounded_String ("device file");
                        goto LABEL_100001;
                     end if;
                     Utils.Get (dev_file, template);
                     Skip_Line (dev_file);
                     Insert (template, 1, first_char'Image);
                     first_char := ' ';
                     Pars_tplate (template, number_of_ports, number_of_s,
                                  freq_present);
                  end if;
               end if;
               if indef
               then
                  P2Ada_Var_1.number_of_con := number_of_ports + 1;
               else
                  P2Ada_Var_1.number_of_con := number_of_ports;
               end if;
               for j in 1 .. P2Ada_Var_1.number_of_con
               loop
                  for i in 1 .. P2Ada_Var_1.number_of_con
                  loop
                     sdevice (i, j).r := 0.0;
                     sdevice (i, j).i := 0.0;
                  end loop;
               end loop;
               if Manhattan (tcompt) or else (tcompt.lngth = 0.0)
               then
                  if P2Ada_Var_1.number_of_con > 1
                  then
                     tcompt.all.lngth := Manh_length *
                       Long_Float (P2Ada_Var_1.number_of_con - 1);
                  else
                     tcompt.all.lngth := Manh_length;
                  end if;
               end if;
               tcompt.all.width := tcompt.all.lngth;
               if tcompt.all.lngth <= resln
               then
                  bad_compt := True;
                  message (1) := To_Unbounded_String ("Device length");
                  message (2) := To_Unbounded_String ("must be");
                  --  message (3) := To_Unbounded_String (">" & sresln'Image);
                  message (3) := To_Unbounded_String (">" & sresln);
                  goto LABEL_100001;
               end if;
               P2Ada_Var_1.con_space := 0.0;
               c_ss := null;
               c_f := null;
               f1 := -1.0;
               loop
                  if freq_present
                  then
                     if c_f = null
                     then
                        tcompt.all.f_file := new s_parameter_record;
                        c_f := tcompt.all.f_file;
                     else
                        c_f.all.next_s := new s_parameter_record;
                        c_f := c_f.all.next_s;
                     end if;
                     c_f.all.next_s := null;
                     c_f.all.z := new TMemComplex;
                     Read_Number (c_f.all.z.all.c.r);
                     first_char := ' ';
                     if (f1 > c_f.all.z.all.c.r) or else bad_compt
                     then
                        Erase_Message;
                        bad_compt := False;
                        goto LABEL_100001;
                     end if;
                     f1 := c_f.all.z.all.c.r;
                  else
                     tcompt.all.f_file := null;
                  end if;
                  for i in 1 .. number_of_s
                  loop
                     mag := 0.0;
                     Read_Number (mag);
                     if bad_compt
                     then
                        if (i = 1) and then not (freq_present)
                        then
                           --  reached EOF
                           Erase_Message;
                           bad_compt := False;
                        end if;
                        goto LABEL_100001;
                     end if;
                     ph := 0.0;
                     Read_Number (ph);
                     if bad_compt
                     then
                        goto LABEL_100001;
                     end if;
                     sdevice (iji (i, 1), iji (i, 2)).r := one * mag *
                       Cos (ph * Pi / 180.0);
                     sdevice (iji (i, 1), iji (i, 2)).i := one * mag *
                       Sin (ph * Pi / 180.0);
                  end loop;
                  if indef
                  then
                     Indef_Matrix (sdevice, number_of_ports);
                  end if;
                  for j in 1 .. P2Ada_Var_1.number_of_con
                  loop
                     for i in 1 .. P2Ada_Var_1.number_of_con
                     loop
                        if c_ss = null
                        then
                           tcompt.all.s_file := new s_parameter_record;
                           c_ss := tcompt.all.s_file;
                        else
                           c_ss.all.next_s := new s_parameter_record;
                           c_ss := c_ss.all.next_s;
                        end if;
                        --  fill parameters
                        c_ss.all.next_s := null;
                        c_ss.all.z := new TMemComplex;
                        c_ss.all.z.all.c.r := sdevice (i, j).r;
                        c_ss.all.z.all.c.i := sdevice (i, j).i;
                     end loop;
                  end loop;
                  exit when End_Of_File (dev_file);
               end loop;
            end;
            <<LABEL_100001>>
            Close (dev_file);
         else
            bad_compt := True;
         end if;
      end if;
   end Device_Read;

   procedure Indef_Matrix (S : in out s_conv_matrix; n : Integer) is
      --  ******************************************
      co_0, sum : TComplex;
      --  *
      --  Change sign of complex number
      --  *

      procedure Sign_Change (z : in out TComplex) is
      begin
         z.r := -z.r;
         z.i := -z.i;
      end Sign_Change;
      --  ******************************************
      --  *
      --  z1 = z1 +z2
      --  *

      procedure Sum_Up (z1 : in out TComplex; z2 : TComplex) is
      begin
         z1.r := z1.r + z2.r;
         z1.i := z1.i + z2.i;
      end Sum_Up;
      --  *******************************************
      --  *
      --  Swap s-parameters between ports 2 and 3.
      --  To be called only for the 2 port device.
      --  *

      procedure Swap_2_and_3 (T : in out s_conv_matrix) is
         --  * S12 <-> S13 *
         --  * S21 <-> S31 *
         --  * S23 <-> S32 *
         --  * S22 <-> S33 *
         temp_z : TComplex;
      begin
         Equate_Zs (temp_z, T (1, 3));
         Equate_Zs (T (1, 3), T (1, 2));
         Equate_Zs (T (1, 2), temp_z);
         Equate_Zs (temp_z, T (3, 1));
         Equate_Zs (T (3, 1), T (2, 1));
         Equate_Zs (T (2, 1), temp_z);
         Equate_Zs (temp_z, T (2, 3));
         Equate_Zs (T (2, 3), T (3, 2));
         Equate_Zs (T (3, 2), temp_z);
         Equate_Zs (temp_z, T (3, 3));
         Equate_Zs (T (3, 3), T (2, 2));
         Equate_Zs (T (2, 2), temp_z);
      end Swap_2_and_3;
      --  ********************************************
      --  Changes S to a normalized admittance matrix
      --  * Y n-port to Y n+1 port routine: *
   begin
      Matrix_Conv (S, n);
      co (co_0, 0.0, 0.0);
      for j in 1 .. n
      loop
         --  initialize sum to complex zero
         Equate_Zs (sum, co_0);
         for i in 1 .. n
         loop
            Sum_Up (sum, S (i, j));
         end loop;
         --  new value for Y[n+1,j]
         Sign_Change (sum);
         Equate_Zs (S (n + 1, j), sum);
      end loop;
      for i in 1 .. n
      loop
         --  initialize sum to complex zero
         Equate_Zs (sum, co_0);
         for j in 1 .. n
         loop
            Sum_Up (sum, S (i, j));
         end loop;
         --  new value for Y[i,n+1]
         Sign_Change (sum);
         Equate_Zs (S (i, n + 1), sum);
      end loop;
      --  initialize sum to complex zero
      Equate_Zs (sum, co_0);
      for i in 1 .. n
      loop
         for j in 1 .. n
         loop
            Sum_Up (sum, S (i, j));
         end loop;
      end loop;
      --  new value for Y[n+1,n+1]
      --  Change from Y to indef S matrix
      Equate_Zs (S (n + 1, n + 1), sum);
      Matrix_Conv (S, n + 1);
      if n = 2
      then
         Swap_2_and_3 (S);
      end if;
      --  * Exchange ports 2 and 3 for the 3 port indef *
   end Indef_Matrix;

end pfmsc;
