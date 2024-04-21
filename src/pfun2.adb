
with Ada.Numerics.Long_Elementary_Functions;
use  Ada.Numerics.Long_Elementary_Functions;

with Ada.Strings.Unbounded;
use  Ada.Strings.Unbounded;

with Ada.Characters.Handling;
use  Ada.Characters.Handling;

with Interfaces;   use Interfaces;
with Interfaces.C; use Interfaces.C;
with Utils;        use Utils;

with xgraph;       use xgraph;

package body pfun2 is

   type rg_pmc is array (1 .. 2) of PMemComplex;

   procedure snapO is
      distance, distancet : Long_Float;
      tnet, pnet : net;
      found : Boolean;
      i : Integer;
   begin
      tnet := null;
      found := False;
      if net_start /= null
      then
         if read_kbd
         then
            distance := 1.0e10;
            loop
               if tnet = null
               then
                  tnet := net_start;
               else
                  tnet := tnet.all.next_net;
               end if;
               if tnet.all.node
               then
                  distancet := Sqrt (((tnet.all.xr - xm) ** 2) +
                                     ((tnet.all.yr - ym) ** 2));
                  if betweenr (0.0, distancet, distance, -resln / 2.0)
                  then
                     distance := distancet;
                     found := True;
                     pnet := tnet;
                  end if;
               end if;
               exit when tnet.all.next_net = null;
            end loop;
         else
            i := 0;
            loop
               if tnet = null
               then
                  tnet := net_start;
               else
                  tnet := tnet.all.next_net;
               end if;
               if tnet.all.node
               then
                  i := i + 1;
                  if i = key_list (key_i).noden
                  then
                     found := True;
                     pnet := tnet;
                  end if;
               end if;
               exit when (tnet.all.next_net = null) or else found;
            end loop;
         end if;
      end if;
      --  else if read_kbd
      if found
      then
         cnet := pnet;
         increment_pos (0);
      end if;
   end snapO;

   function Manhattan (tcompt : compt) return Boolean is
      Result_Manhattan : Boolean;
      c_string : Unbounded_String;
      long : Integer;
   begin
      if Manhattan_Board
      then
         Result_Manhattan := True;
      else
         c_string := tcompt.descript;
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
            --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
            if (Element (c_string, long) = '?') and then
               (tcompt.typ = 'c' or else tcompt.typ = 't')
            then
               Result_Manhattan := True;
            end if;
            long := long - 1;
         end loop;
      end if;
      return Result_Manhattan;
   end Manhattan;

   function Rough_alpha (alpha_in : Long_Float) return Long_Float is
      Result_Rough_alpha : Long_Float;
      skin_depth, angle_arg : Long_Float;
   begin
      if (freq > 0.0) and then (surface_roughness > 0.0)
      then
         skin_depth := 1.0e6 / Sqrt (Pi * freq *
                                       Eng_Prefix (Character (freq_prefix)) *
                                       Mu_0 * conductivity);
         angle_arg := 1.4 * ((surface_roughness / skin_depth) ** 2);
         Result_Rough_alpha := alpha_in *
           (1.0 + 2.0 * Arctan (angle_arg) / Pi);
      else
         Result_Rough_alpha := alpha_in;
      end if;
      return Result_Rough_alpha;
   end Rough_alpha;

   procedure tlineO (tcompt : compt) is
      --  ,sh,ch
      --  were extended intermediate results
      j : Integer;
      c_ss : array (1 .. 2) of rg_pmc;
      rds : TComplex;
      unit1, prefix : Character;
      Manhat_on, alt_param : Boolean;
      zd, ere, value, alpha_tl, beta_l, elength, gamma, width,
      wavelength : Long_Float;
      value_str : Unbounded_String;
      sh, ch : TComplex;
   begin
      if action
      then
         declare P2Ada_Var_15 : compt_record renames tcompt.all;
         begin
            --  default attenuation factors nepers/mm
            --  initialize
            --  * an 'M' at the end of ^.descript? *
            P2Ada_Var_15.alpha_c := 0.0;
            P2Ada_Var_15.alpha_d := 0.0;
            P2Ada_Var_15.alpha_co := 0.0;
            P2Ada_Var_15.alpha_do := 0.0;
            P2Ada_Var_15.super := False;
            Manhat_on := Manhattan (tcompt);
            if (Index (P2Ada_Var_15.descript, "?") > 0) and then
              super_line (tcompt)
            then
               bad_compt := True;
               message (1) := To_Unbounded_String ("Combined");
               message (2) := To_Unbounded_String ("? and !");
               message (3) := To_Unbounded_String ("disallowed");
               return;
            end if;
            --  value_str not used here
            value := 0.0;
            unit1 := ' ';
            prefix := ' ';
            alt_param := False;
            Get_Param (tcompt, 1, value, value_str, unit1, prefix, alt_param);
            if bad_compt
            then
               return;
            end if;
--  ??          if alt_param
--  ??            then
--  ??               x_sweep.init_element (tcompt, 'z', prefix, unit1);
--  ??            end if;
            if bad_compt
            then
               return;
            end if;
            case unit1 is
               when Character (Omega) =>
                  zd := value;
               when 's' | 'S' =>
                  zd := 1.0 / value;
               when 'z' | 'Z' =>
                  zd := Z0 * value;
               when 'y' | 'Y' =>
                  zd := Z0 / value;
               when others =>
                  begin
                     bad_compt := True;
                     message (1) := To_Unbounded_String ("Invalid tline");
                     message (2) := To_Unbounded_String ("impedance unit");
                     message (3) := To_Unbounded_String ("Use y, z, S or ")
                                                           & Character (Omega);
                     return;
                  end;
                  --  else
            end case;
            --  case
            if not (Manhat_on) and then (zd <= 0.0)
            then
               --  OK if alt_param
               bad_compt := True;
               message (1) := To_Unbounded_String ("Negative");
               message (2) := To_Unbounded_String ("or zero tline");
               message (2) := To_Unbounded_String ("impedance");
               return;
            end if;
            --  Get_Param will return 1.0 for zed if alt_param and no number
            --  given
            if (alt_param and then (value_str = "1.0")) or else (zd <= 0.0)
            then
               width := widthZ0;
            else
               width := widtht (zd);
            end if;
            --  width is always needed for ere calculation
            if bad_compt
            then
               return;
            end if;
            if not (Manhat_on)
            then
               if width < resln
               then
                  bad_compt := True;
                  message (1) := To_Unbounded_String ("Impedance too big");
                  message (2) := To_Unbounded_String ("tline too narrow");
                  message (3) := To_Unbounded_String ("(<" & sresln & ")");
                  return;
               end if;
               if width > bmax
               then
                  bad_compt := True;
                  message (1) := To_Unbounded_String ("Impedance is");
                  message (2) := To_Unbounded_String ("too small:");
                  message (3) := To_Unbounded_String ("tline too wide");
                  return;
               end if;
            end if;
            --  not Manhat_on
            if super_line (tcompt) and then ((width / substrate_h) < 0.0001)
            then
               bad_compt := True;
               message (1) := To_Unbounded_String ("Impedance out of");
               message (2) := To_Unbounded_String (" range for");
               message (3) := To_Unbounded_String ("tline! model");
               return;
            end if;
            if stripline
            then
               --  ere calculation is a function of width!
               ere := er;
            else
               ere := (er + 1.0) / 2.0 +
                 (er - 1.0) / 2.0 / Sqrt (1.0 + 10.0 * substrate_h / width);
            end if;
            --  value_str not used here
            Get_Param (tcompt, 2, value, value_str, unit1, prefix, alt_param);
            if bad_compt
            then
               return;
            end if;
--  ??            if alt_param
--  ??            then
--  ??               x_sweep.init_element (tcompt, 'l', prefix, unit1);
--  ??            end if;
            if bad_compt
            then
               return;
            end if;
            case unit1 is
               when Character (Degree) =>
                  wavelength := value / 360.0;
                  elength := Lambda_fd * wavelength / Sqrt (ere);
               when 'm' =>
                  --  mmlong
                  elength := value;
                  wavelength := elength * Sqrt (ere) / Lambda_fd;
--  ??                  if alt_param
--  ??                  then
--  ??                     x_sweep.Load_Prop_Const (sqrt (ere) /
--  ??                         lambda_fd, 0.0);
--  ??                  end if;
               when 'h' | 'H' =>
                  elength := value * substrate_h;
                  wavelength := elength * Sqrt (ere) / Lambda_fd;
--  ??                  if alt_param
--  ??                  then
--  ??                     x_sweep.Load_Prop_Const (substrate_h * Sqrt (ere) /
--  ??                                                Lambda_fd, 0.0);
--  ??                  end if;
               when others =>
                  begin
                     bad_compt := True;
                     message (1) := To_Unbounded_String ("Invalid tline");
                     message (2) := To_Unbounded_String ("length unit");
                     message (3) := To_Unbounded_String ("Use mm, h or " &
                                                           Character (Degree));
                     return;
                  end;
            end case;
            --  case
            --  save lngth factor in mm
            --  * Enable lossy and dispersive line (super_line) here *
            --  ??  elength0 := elength;
            if super_line (tcompt)
            then
               if stripline
               then
                  super_stripline (tcompt);
               else
                  super_microstrip (tcompt);
               end if;
            end if;
            if Manhat_on
            then
               --  Artwork correction and Manh_width after ere calculation
               width := Manh_width;
               elength := Manh_length;
            else
               width := width + artwork_cor;
            end if;
            --  * Check here for artwork length corrections *
            j := goto_numeral (3, tcompt.all.descript);
            if bad_compt
            then
               Erase_Message;
            end if;
            --  ignore lack of correction
            bad_compt := False;
            if (j > 0) and then not (Manhat_on)
            then
               --  value_str and prefix not used here
               Get_Param (tcompt, 3, value, value_str, unit1,
                          prefix, alt_param);
               if alt_param
               then
                  bad_compt := True;
               end if;
               if bad_compt
               then
                  return;
               end if;
               case unit1 is
                  --  Add or subtract line for artwork
                  when Character (Degree) =>
                     elength := elength + Lambda_fd * (value / 360.0) /
                       Sqrt (ere);
                  when 'h' | 'H' =>
                     elength := elength + value * substrate_h;
                  when 'm' =>
                     elength := elength + value;
                  when others =>
                     begin
                        bad_compt := True;
                        message (1) := To_Unbounded_String ("Improper units");
                        message (2) := To_Unbounded_String ("used in length");
                        message (3) := To_Unbounded_String (" correction   ");
                        return;
                     end;
               end case;
               --  case unit1
            end if;
            --  if j > 0
            if elength > bmax
            then
               bad_compt := True;
               message (1) := To_Unbounded_String ("tline is longer");
               message (2) := To_Unbounded_String ("than board size");
               return;
            end if;
            if elength < resln
            then
               bad_compt := True;
               message (1) := To_Unbounded_String ("tline too short");
               message (2) := To_Unbounded_String ("Length must be");
               message (3) := To_Unbounded_String (">" & sresln);
               return;
            end if;
            P2Ada_Var_15.con_space := 0.0;
            P2Ada_Var_15.number_of_con := 2;
         end;
         --  [P2Ada]: end of WITH
         --  with tcompt^ do
         --  if action
         --  if no action
         --  Calculate scattering parameters
         --  Normalized frequency
         --  Re-compute Zo and wavelength when dispersive microstrip
      else
         gamma := freq / design_freq;
         if tcompt.all.super and then not (stripline)
         then
            ms_dispersion (tcompt);
         end if;
         --  ! Beware of problems here with trig operations
         --  ******Real gamma routines
         --  sh := Sin(elength*gamma);   {! was Sin_asm }
         --  ch := Cos(elength*gamma);   {! was Cos_asm }
         --  zd:=tcompt^.zed/z0;
         --  rds := rc(co(2*zd*ch,(sqr(zd)+1.0)*sh));
         --  new(u);
         --  c_ss[1,1]:=prp(u,co(0.0,(sqr(zd)-1.0)*sh),rds);
         --  c_ss[2,2]:=co(c_ss[1,1]^.r,c_ss[1,1]^.i);
         --  c_ss[1,2]:=sm(2*zd,rds);
         --  c_ss[2,1]:=co(c_ss[1,2]^.r,c_ss[1,2]^.i);
         --  *****
         --  ******Complex gamma routines*************
         elength := 2.0 * Pi * tcompt.wavelength;
         alpha_tl := (tcompt.alpha_d * gamma +
                        Rough_alpha (tcompt.alpha_c) * Sqrt (gamma)) *
           tcompt.all.lngth0;
         beta_l := elength * gamma;
         sh.r := Sinh (alpha_tl) * Cos (beta_l);
         sh.i := Cosh (alpha_tl) * Sin (beta_l);
         ch.r := Cosh (alpha_tl) * Cos (beta_l);
         ch.i := Sinh (alpha_tl) * Sin (beta_l);
         zd := tcompt.zed / Z0;
         for j in 1 .. 2
         loop
            for i in 1 .. 2
            loop
               c_ss (i) (j) := new TMemComplex;
            end loop;
         end loop;
         --  **********************************************
         co (c_ss (2) (1).all.c, 2.0 * zd * ch.r + (((zd) ** 2) + 1.0)
             * sh.r, 2.0 * zd * ch.i + (((zd) ** 2) + 1.0) * sh.i);
         rc (rds, c_ss (2) (1).all.c);
         co (c_ss (2) (1).all.c, (((zd) ** 2) - 1.0) * sh.r,
             (((zd) ** 2.0) - 1.0) * sh.i);
         prp (c_ss (1) (1).all.c, c_ss (2) (1).all.c, rds);
         co (c_ss (2) (2).all.c, c_ss (1) (1).all.c.r, c_ss (1) (1).all.c.i);
         sm (c_ss (1) (2).all.c, 2.0 * zd, rds);
         co (c_ss (2) (1).all.c, c_ss (1) (2).all.c.r, c_ss (1) (2).all.c.i);
         c_s := null;
         for j in 1 .. 2
         loop
            for i in 1 .. 2
            loop
               if c_s = null
               then
                  tcompt.all.s_begin := new s_parameter_record;
                  c_s := tcompt.all.s_begin;
               else
                  c_s.all.next_s := new s_parameter_record;
                  c_s := c_s.all.next_s;
               end if;
               c_s.all.next_s := null;
               c_s.all.z := c_ss (i) (j);
            end loop;
         end loop;
         --  i
      end if;
      --  action
   end tlineO;
   --  * tlineO *
   --  *

   procedure qline (tcompt : compt) is
      --  ,sh,ch
      --  were extended intermediate results
      j, des_lngth : Integer;
      rds : TComplex;
      c_ss : array (1 .. 2) of rg_pmc;
      unit1, prefix : Character;
      Manhat_on, alt_param : Boolean;
      zd, ere, value, alpha_tl, beta_l, elength, gamma : Long_Float;
      value_str : Unbounded_String;
      sh, ch : TComplex;
   begin
      if action
      then
         --  [P2Ada]: WITH instruction
         --  [P2Ada]: !Help! No type found -> add 'P2Ada_Var_16.' to fields
         declare P2Ada_Var_16 : compt_record renames tcompt.all;
         begin
            --  default attenuation factors nepers/mm
            --  initialize
            --  * an 'M' at the end of ^.descript? *
            P2Ada_Var_16.alpha_c := 0.0;
            P2Ada_Var_16.alpha_d := 0.0;
            P2Ada_Var_16.alpha_co := 0.0;
            P2Ada_Var_16.alpha_do := 0.0;
            P2Ada_Var_16.super := False;
            Manhat_on := Manhattan (tcompt);
            if Index (P2Ada_Var_16.descript, "!") > 0
            then
               bad_compt := True;
               message (1) := To_Unbounded_String ("qline! is");
               message (2) := To_Unbounded_String ("an invalid");
               message (3) := To_Unbounded_String ("part");
               return;
            end if;
            --  value_str not used here
            value := 0.0;
            unit1 := ' ';
            prefix := ' ';
            alt_param := False;
            Get_Param (tcompt, 1, value, value_str, unit1, prefix, alt_param);
            if bad_compt
            then
               return;
            end if;
--  ??              if alt_param
--  ??              then
--  ??                 x_sweep.init_element (tcompt, 'z', prefix, unit1);
--  ??              end if;
            if bad_compt
            then
               return;
            end if;
            case unit1 is
               when Character (Omega) =>
                  P2Ada_Var_16.zed := value;
               when 's' | 'S' =>
                  P2Ada_Var_16.zed := 1.0 / value;
               when 'z' | 'Z' =>
                  P2Ada_Var_16.zed := Z0 * value;
               when 'y' | 'Y' =>
                  P2Ada_Var_16.zed := Z0 / value;
               when others =>
                  begin
                     bad_compt := True;
                     message (1) := To_Unbounded_String ("Invalid qline");
                     message (2) := To_Unbounded_String ("impedance unit");
                     message (3) := To_Unbounded_String ("Use y, z, S or " &
                                                           Character (Omega));
                     return;
                  end;
                  --  else
            end case;
            --  case
            if not (Manhat_on) and then (P2Ada_Var_16.zed <= 0.0)
            then
               --  OK if alt_param
               bad_compt := True;
               message (1) := To_Unbounded_String ("Negative");
               message (2) := To_Unbounded_String ("or zero qline");
               message (2) := To_Unbounded_String ("impedance");
               return;
            end if;
            --  Get_Param will return 1.0 for zed if alt_param and no number
            --  given
            if (alt_param and then (value_str = "1.0")) or else
              (P2Ada_Var_16.zed <= 0.0)
            then
               P2Ada_Var_16.width := widthZ0;
            else
               P2Ada_Var_16.width := widtht (P2Ada_Var_16.zed);
            end if;
            --  width is always needed for ere calculation
            if bad_compt
            then
               return;
            end if;
            if not (Manhat_on)
            then
               if P2Ada_Var_16.width < resln
               then
                  bad_compt := True;
                  message (1) := To_Unbounded_String ("Impedance too big");
                  message (2) := To_Unbounded_String ("qline too narrow");
                  message (3) := To_Unbounded_String ("(<" & sresln & ')');
                  return;
               end if;
               if P2Ada_Var_16.width > bmax
               then
                  bad_compt := True;
                  message (1) := To_Unbounded_String ("Impedance is");
                  message (2) := To_Unbounded_String ("too small:");
                  message (3) := To_Unbounded_String ("qline too wide");
                  return;
               end if;
            end if;
            --  not Manhat_on
            if stripline
            then
               --  ere calculation is a function of width!
               ere := er;
            else
               ere := (er + 1.0) / 2.0 + (er - 1.0) / 2.0 /
                 Sqrt (1.0 + 10.0 * substrate_h / P2Ada_Var_16.width);
            end if;
            --  value_str not used here
            Get_Param (tcompt, 2, value, value_str, unit1, prefix, alt_param);
            if bad_compt
            then
               return;
            end if;
--  ??              if alt_param
--  ??              then
--  ??                 x_sweep.init_element (tcompt, 'l', prefix, unit1);
--  ??              end if;
            if bad_compt
            then
               return;
            end if;
            case unit1 is
               when Character (Degree) =>
                  P2Ada_Var_16.wavelength := value / 360.0;
                  P2Ada_Var_16.lngth := Lambda_fd * P2Ada_Var_16.wavelength /
                    Sqrt (ere);
               when 'm' =>
                  --  mmlong
                  P2Ada_Var_16.lngth := value;
                  P2Ada_Var_16.wavelength := P2Ada_Var_16.lngth *
                    Sqrt (ere) / Lambda_fd;
--  ??                  if alt_param
--  ??                  then
--  ??                     x_sweep.Load_Prop_Const (sqrt (ere) / lambda_fd,
--  ??                     0.0P2Ada_Var_16.P2Ada_Var_16.P2Ada_Var_16.);
--  ??                  end if;
               when 'h' | 'H' =>
                  P2Ada_Var_16.lngth := value * substrate_h;
                  P2Ada_Var_16.wavelength := P2Ada_Var_16.lngth * Sqrt (ere) /
                    Lambda_fd;
                  if alt_param
                  then
                     x_sweep.Load_Prop_Const (substrate_h * Sqrt (ere) /
                                                Lambda_fd, 0.0);
                  end if;
               when others =>
                  begin
                     bad_compt := True;
                     message (1) := To_Unbounded_String ("Invalid qline");
                     message (2) := To_Unbounded_String ("length unit");
                     message (3) := To_Unbounded_String ("Use mm, h or " &
                                                           Character (Degree));
                     return;
                  end;
            end case;
            --  case
            --  save lngth factor in mm
            P2Ada_Var_16.lngth0 := P2Ada_Var_16.lngth;
            if Manhat_on
            then
               --  Artwork correction and Manh_width after ere calculation
               P2Ada_Var_16.width := Manh_width;
               P2Ada_Var_16.lngth := Manh_length;
            else
               P2Ada_Var_16.width := P2Ada_Var_16.width + artwork_cor;
            end if;
            --  * Check for Q factor *
            j := goto_numeral (3, tcompt.all.descript);
            if bad_compt
            then
               Erase_Message;
            end if;
            --  ignore lack of Q
            bad_compt := False;
            if j > 0
            then
               --  get Q value
               --  value_str, prefix not used here, unit1 should be 'Q'
               Get_Param (tcompt, 3, value, value_str, unit1, prefix,
                          alt_param);
 --  ??              if alt_param
 --  ??              then
 --  ??                 x_sweep.init_element (tcompt, 'Q', prefix, 'Q');
 --  ??              end if;
               if bad_compt
               then
                  return;
               end if;
               --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
               if unit1 = Character (Degree) or else unit1 = 'm' or else
                  unit1 = 'h' or else unit1 = 'H'
               then
                  bad_compt := True;
                  message (1) := To_Unbounded_String ("Corrections");
                  message (2) := To_Unbounded_String ("not allowed for");
                  message (3) := To_Unbounded_String ("qline length");
                  return;
               end if;
               --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
               if (value = 0.0) or else not (unit1 = 'q' or else unit1 = 'Q')
               then
                  bad_compt := True;
                  message (1) := To_Unbounded_String ("Invalid or zero");
                  message (2) := To_Unbounded_String ("Q factor");
                  return;
               end if;
               des_lngth := Length (P2Ada_Var_16.descript);
               loop
                  --  Find location of 'Q'
                  j := j + 1;
                  exit when (Element (P2Ada_Var_16.descript, j) = unit1)
                    or else (j > des_lngth);
               end loop;
               loop
                  --  Skip spaces to
                  --  find 'd' or 'c' after Q
                  j := j + 1;
                  exit when (Element (P2Ada_Var_16.descript, j) /= ' ')
                    or else (j > des_lngth);
               end loop;
               if P2Ada_Var_16.lngth0 /= 0.0
               then
                  --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
                  P2Ada_Var_16.alpha_co := (Pi * P2Ada_Var_16.wavelength) /
                    (value * P2Ada_Var_16.lngth0);
                  if (j <= des_lngth) and then
                    (P2Ada_Var_16.descript = "C" or else
                        P2Ada_Var_16.descript = "c")
                  then
                     --  default to dielectric loss
                     P2Ada_Var_16.alpha_c := P2Ada_Var_16.alpha_co;
                  else
                     P2Ada_Var_16.alpha_d := P2Ada_Var_16.alpha_co;
                  end if;
                  --  reset to 0
                  P2Ada_Var_16.alpha_co := 0.0;
               end if;
            end if;
            --  if j > 0
            P2Ada_Var_16.con_space := 0.0;
            P2Ada_Var_16.number_of_con := 2;
         end;
         --  [P2Ada]: end of WITH
         --  with tcompt^ do
         --  if no action
         --  Calculate scattering parameters
         --  Normalized frequency
         --  ! Warning, these alphas get huge for small Q's
      else
         gamma := freq / design_freq;
         elength := 2.0 * Pi * tcompt.all.wavelength;
         alpha_tl := (tcompt.all.alpha_d * gamma + tcompt.all.alpha_c *
                        Sqrt (gamma)) * tcompt.all.lngth0;
         if alpha_tl > 80.0
         then
            alpha_tl := 80.0;
         end if;
         beta_l := elength * gamma;
         sh.r := Sinh (alpha_tl) * Cos (beta_l);
         sh.i := Cosh (alpha_tl) * Sin (beta_l);
         ch.r := Cosh (alpha_tl) * Cos (beta_l);
         ch.i := Sinh (alpha_tl) * Sin (beta_l);
         zd := tcompt.all.zed / Z0;
         for i in 1 .. 2
         loop
            for j in 1 .. 2
            loop
               c_ss (i) (j) := new TMemComplex;
            end loop;
         end loop;
         co (c_ss (1) (1).all.c, 2.0 * zd * ch.r + (((zd) ** 2) + 1.0) * sh.r,
             2.0 * zd * ch.i + (((zd) ** 2) + 1.0) * sh.i);
         rc (rds, c_ss (1) (1).all.c);
         co (c_ss (2) (1).all.c, (((zd) ** 2) - 1.0) * sh.r,
             (((zd) ** 2) - 1.0) * sh.i);
         prp (c_ss (1) (1).all.c, c_ss (2) (1).all.c, rds);
         co (c_ss (2) (2).all.c, c_ss (1) (1).all.c.r, c_ss (1) (1).all.c.i);
         sm (c_ss (1) (2).all.c, 2.0 * zd, rds);
         co (c_ss (2) (1).all.c, c_ss (1) (2).all.c.r, c_ss (1) (2).all.c.i);
         c_s := null;
         for j in 1 .. 2
         loop
            for i in 1 .. 2
            loop
               if c_s = null
               then
                  tcompt.all.s_begin := new s_parameter_record;
                  c_s := tcompt.all.s_begin;
               else
                  c_s.all.next_s := new s_parameter_record;
                  c_s := c_s.all.next_s;
               end if;
               c_s.all.next_s := null;
               c_s.all.z := c_ss (i) (j);
            end loop;
         end loop;
         --  i
      end if;
      --  action
   end qline;

   procedure clinesO (tcompt : compt) is
      type rg_tc is array (1 .. 2) of TComplex;
      type rgrg_tc is array (1 .. 2) of rg_tc;
      --  sh,ch,
      i, j, mi, mj, z_index : Integer;
      rds : TComplex;
      c_s : s_param;
      unit1, prefix : Character;
      seo : array (1 .. 2) of rgrg_tc;
      wide, wido, zd, elength, gamma, widthc, spacec, alpha_tl, beta_l, value,
      zt, ere, eree, ereo : Long_Float;
      sh, ch : TComplex;
      check_correction_three, Manhat_on, alt_param : Boolean;
      value_str : Unbounded_String;
   begin
      if action
      then
         --  [P2Ada]: WITH instruction
         --  [P2Ada]: !Help! No type found -> add 'P2Ada_Var_17.' to fields
         declare P2Ada_Var_17 : compt_record renames tcompt.all;
         begin
            --  * an 'M' at the end of ^.descript? *
            Manhat_on := Manhattan (tcompt);
            if bad_compt
            then
               return;
            end if;
            if (Index (P2Ada_Var_17.descript, "?") > 0)
              and then super_line (tcompt)
            then
               bad_compt := True;
               message (1) := To_Unbounded_String ("Combined");
               message (2) := To_Unbounded_String ("? and !");
               message (3) := To_Unbounded_String ("disallowed");
               return;
            end if;
            --  default attenuation factors nepers/mm
            P2Ada_Var_17.alpha_c := 0.0;
            P2Ada_Var_17.alpha_d := 0.0;
            P2Ada_Var_17.alpha_co := 0.0;
            P2Ada_Var_17.alpha_do := 0.0;
            P2Ada_Var_17.super := False;
            P2Ada_Var_17.number_of_con := 4;
            z_index := 0;
            P2Ada_Var_17.zed := 0.0;
            P2Ada_Var_17.zedo := 0.0;
            P2Ada_Var_17.wavelength := 0.0;
            P2Ada_Var_17.lngth := 0.0;
            check_correction_three := False;
            for i in 1 .. 3
            loop
               if (P2Ada_Var_17.lngth = 0.0)
                 and then (P2Ada_Var_17.wavelength = 0.0)
               then
                  j := goto_numeral (i, tcompt.all.descript);
                  bad_compt := False;
                  if (j > 0) or else (i < 3)
                  then
                     --  value_str and prefix not used here
                     value := 0.0;
                     unit1 := ' ';
                     prefix := ' ';
                     alt_param := False;
                     Get_Param (tcompt, i, value, value_str, unit1, prefix,
                                alt_param);
                     if bad_compt
                     then
                        return;
                     end if;
                     if alt_param
                     then
                        --  init sweep object
                        --  [P2Ada]: "x in y" -> "x and y" redefine "and"
                        --  before
                        if unit1 = Character (Omega)
                          or else unit1 = 's'
                          or else unit1 = 'S'
                          or else unit1 = 'z'
                          or else unit1 = 'Z'
                          or else unit1 = 'y'
                        then
                           if P2Ada_Var_17.zed = 0.0
                           then
                              --  variable is the 1st z
                              --  variable is the 2nd z
--  ??                              x_sweep.init_element (tcompt, 'z', prefix,
--  ??                                                    unit1);
                              z_index := 1;
                           else
--  ??                              x_sweep.init_element (tcompt, 'z', prefix,
--  ??                                                    unit1);
                              z_index := 2;
                           end if;
                           --  [P2Ada]: "x in y" -> "x and y" redefine "and"
                           --  before
                        else
                           if unit1 = Character (Degree)
                             or else unit1 = 'm'
                             or else unit1 = 'h'
                             or else unit1 = 'H'
                           then
                              --  if length units
                              --  variable is length
--  ??                              x_sweep.init_element (tcompt, 'l', prefix,
--  ??                                                    unit1);
                              z_index := 5;
                           end if;
                        end if;
                     end if;
                     --  if alt_param
                     if bad_compt
                     then
                        return;
                     end if;
                     --  Read in first impedance as zed, second as zedo
                     case unit1 is
                        when Character (Omega) =>
                           if P2Ada_Var_17.zed = 0.0
                           then
                              P2Ada_Var_17.zed := value;
                           else
                              P2Ada_Var_17.zedo := value;
                           end if;
                        when 's' | 'S' =>
                           if P2Ada_Var_17.zed = 0.0
                           then
                              P2Ada_Var_17.zed := 1.0 / value;
                           else
                              P2Ada_Var_17.zedo := 1.0 / value;
                           end if;
                        when 'z' | 'Z' =>
                           if P2Ada_Var_17.zed = 0.0
                           then
                              P2Ada_Var_17.zed := Z0 * value;
                           else
                              P2Ada_Var_17.zedo := Z0 * value;
                           end if;
                        when 'y' | 'Y' =>
                           if P2Ada_Var_17.zed = 0.0
                           then
                              P2Ada_Var_17.zed := Z0 / value;
                           else
                              P2Ada_Var_17.zedo := Z0 / value;
                           end if;
                        when Character (Degree) =>
                           P2Ada_Var_17.wavelength := value / 360.0;
                        when 'm' =>
                           --  mmlong
                           P2Ada_Var_17.lngth := value;
                           if z_index = 5
                           then
                              z_index := 6;
                           end if;
                           --  var in meters
                        when 'h' | 'H' =>
                           --  mmlong
                           P2Ada_Var_17.lngth := value * substrate_h;
                           if z_index = 5
                           then
                              z_index := 7;
                           end if;
                           --  var in h
                        when others =>
                           begin
                              bad_compt := True;
                              message (1) :=
                                To_Unbounded_String ("Missing clines");
                              message (2) :=
                                To_Unbounded_String ("unit. Use y, z");
                              message (3) :=
                                To_Unbounded_String ("S, " & Character (Omega)
                                                       & ", mm, h or " &
                                                       Character (Degree));
                              return;
                           end;
                     end case;
                     --  case
                  end if;
                  --  (j > 0) or (i < 3)
                  if (i = 2) and then ((P2Ada_Var_17.wavelength /= 0.0)
                                       or else (P2Ada_Var_17.lngth /= 0.0))
                  then
                     check_correction_three := True;
                  end if;
                  --  * Set flag if 3rd term (i=3) in tcompt^.descript
                  --  is a possible length correction *
               end if;
            end loop;
            --  for i=1 to 3
            if not (check_correction_three)
            then
               i := 4;
            end if;
            if (P2Ada_Var_17.zed = 0.0) and then (P2Ada_Var_17.zedo = 0.0)
            then
               --  if zed=0 then zed:=sqr(Z0)/zedo;
               bad_compt := True;
               message (1) := To_Unbounded_String ("Both cline even");
               message (2) := To_Unbounded_String ("& odd impedances");
               message (3) := To_Unbounded_String ("not found or zero");
               return;
            else
               if P2Ada_Var_17.zedo = 0.0
               then
                  --  calculate zedo if blank
                  P2Ada_Var_17.zedo := ((Z0) ** 2) / P2Ada_Var_17.zed;
               else
                  if z_index = 1
                  then
                     --  both given, even is var
                     z_index := 3;
                  else
                     if z_index = 2
                     then
                        z_index := 4;
                     end if;
                  end if;
               end if;
               --  both given, odd is var
            end if;
            if P2Ada_Var_17.zed < P2Ada_Var_17.zedo
            then
               --  swap even and odd impedances
               zt := P2Ada_Var_17.zed;
               P2Ada_Var_17.zed := P2Ada_Var_17.zedo;
               P2Ada_Var_17.zedo := zt;
               case z_index is
                  when 1 =>
                     --  single given, odd mode is variable
                     z_index := 2;
                  when 2 =>
                     --  single given, even mode is variable
                     z_index := 1;
                  when 3 =>
                     --  both given, odd mode is variable
                     z_index := 4;
                  when 4 =>
                     --  both given, even mode is variable
                     z_index := 3;
                  when others =>
                     --  [P2Ada]: no otherwise / else in Pascal
                     null;
               end case;
               --  case
            end if;
            if (P2Ada_Var_17.zed <= 0.0) or else (P2Ada_Var_17.zedo <= 0.0)
            then
               bad_compt := True;
               message (1) := To_Unbounded_String ("cline");
               message (2) := To_Unbounded_String ("impedances must");
               message (3) := To_Unbounded_String ("be positive");
               return;
            end if;
            --  Load index's 1..4 for alt parameter sweep
            if (z_index /= 0) and then (z_index < 5)
            then
               x_sweep.Load_Index (z_index);
            end if;
            --  * All dimensions must be calculated, even for Manhattan,
            --  to guarantee that electrical lengths are the same be it
            --  regular or Manhattan *
            if P2Ada_Var_17.zed * 0.97 < P2Ada_Var_17.zedo
            then
               --  !* this factor was 0.98 in 1.0 *
               --  !* changed to avoid w_s_micro errors*
               bad_compt := True;
               message (1) := To_Unbounded_String ("cline even & odd");
               message (2) := To_Unbounded_String ("impedances are");
               message (3) := To_Unbounded_String ("too close");
               return;
            end if;
            if not (bad_compt)
            then
               if stripline
               then
                  widthc := 0.0;
                  spacec := 0.0;
                  w_s_stripline_cline (P2Ada_Var_17.zed,
                                       P2Ada_Var_17.zedo, widthc, spacec);
               else
                  wide := widtht (P2Ada_Var_17.zed / 2.0) / substrate_h;
                  wido := widtht (P2Ada_Var_17.zedo / 2.0) / substrate_h;
                  if bad_compt
                  then
                     return;
                  end if;
                  w_s_microstrip_cline (wide, wido, widthc, spacec);
               end if;
               if not (bad_compt)
               then
                  --  ! * Artwork correction added here ^ *
                  P2Ada_Var_17.width := widthc * substrate_h + artwork_cor;
                  P2Ada_Var_17.con_space := (widthc + spacec) * substrate_h;
                  if (P2Ada_Var_17.con_space < resln) and then not (Manhat_on)
                  then
                     bad_compt := True;
                     message (1) := To_Unbounded_String ("clines spacing is");
                     message (2) := To_Unbounded_String ('<' & sresln);
                     message (3) :=
                       To_Unbounded_String (Character (Omega) & "e/"
                                            & Character (Omega)
                                            & "o is too big");
                     return;
                  end if;
                  if (P2Ada_Var_17.width < resln) and then not (Manhat_on)
                  then
                     bad_compt := True;
                     message (1) := To_Unbounded_String ("Even impedance is");
                     message (2) := To_Unbounded_String ("too large. Width");
                     message (3) := To_Unbounded_String ('<' & sresln);
                     return;
                  end if;
                  if stripline
                  then
                     ereo := er;
                     eree := er;
                     ere := er;
                  else
                     ere_even_odd (widthc, spacec, eree, ereo);
                     ere := 4.0 * eree * ereo / ((Sqrt (eree) +
                                                 Sqrt (ereo)) ** 2);
                  end if;
                  if (P2Ada_Var_17.lngth = 0.0) and then
                    (P2Ada_Var_17.wavelength = 0.0)
                  then
                     bad_compt := True;
                     message (1) := To_Unbounded_String ("Missing cline");
                     message (2) := To_Unbounded_String ("length. Supply");
                     message (3) := To_Unbounded_String ("length in mm or "
                                                         & Character (Degree));
                     return;
                  else
                     if P2Ada_Var_17.lngth = 0.0
                     then
                        P2Ada_Var_17.lngth := Lambda_fd *
                          P2Ada_Var_17.wavelength / Sqrt (ere);
                     end if;
                     --  used in cl_dispersion
                     P2Ada_Var_17.wavelength := P2Ada_Var_17.lngth *
                       Sqrt (eree) / Lambda_fd;
                     P2Ada_Var_17.wavelengtho := P2Ada_Var_17.lngth *
                       Sqrt (ereo) / Lambda_fd;
                     P2Ada_Var_17.lngth0 := P2Ada_Var_17.lngth;
                     case z_index is
                        when 5 =>
                           x_sweep.Load_Prop_Const (Sqrt (eree) /
                                                      Sqrt (ere), Sqrt (ereo) /
                                                      Sqrt (ere));
                        when 6 =>
                           x_sweep.Load_Prop_Const (Sqrt (eree) /
                                                      Lambda_fd, Sqrt (ereo) /
                                                      Lambda_fd);
                        when 7 =>
                           x_sweep.Load_Prop_Const (substrate_h *
                                                      Sqrt (eree) / Lambda_fd,
                                                    substrate_h * Sqrt (ereo) /
                                                      Lambda_fd);
                        when others =>
                           --  [P2Ada]: no otherwise / else in Pascal
                           null;
                     end case;
                     --  case
                     j := goto_numeral (i, tcompt.all.descript);
                     if bad_compt
                     then
                        Erase_Message;
                     end if;
                     --  ignore corrections
                     --  * Invoke advanced models *
                     bad_compt := False;
                     if super_line (tcompt)
                     then
                        if stripline
                        then
                           super_cl_stripline (tcompt, widthc, spacec);
                        else
                           super_cl_microstrip (tcompt, widthc, spacec);
                        end if;
                     end if;
                     --  * Check for and if needed add length corrections *
                     if j > 0
                     then
                        --  value_str and prefix not used here
                        Get_Param (tcompt, i, value, value_str, unit1, prefix,
                                   alt_param);
                        if bad_compt
                        then
                           return;
                        end if;
                        case unit1 is
                           when Character (Degree) =>
                              P2Ada_Var_17.lngth := P2Ada_Var_17.lngth +
                                Lambda_fd * (value / 360.0) / Sqrt (ere);
                           when 'h' | 'H' =>
                              P2Ada_Var_17.lngth := P2Ada_Var_17.lngth +
                                value * substrate_h;
                           when 'm' =>
                              P2Ada_Var_17.lngth := P2Ada_Var_17.lngth + value;
                           when others =>
                              begin
                                 bad_compt := True;
                                 message (1) :=
                                   To_Unbounded_String ("Improper units");
                                 message (2) :=
                                   To_Unbounded_String ("used in length");
                                 message (3) :=
                                   To_Unbounded_String (" correction   ");
                                 return;
                              end;
                        end case;
                        --  case
                     end if;
                     --  if j > 0
                  end if;
                  --  else lngth=wavelength=0
                  if (P2Ada_Var_17.lngth < 0.0) and then not (Manhat_on)
                  then
                     bad_compt := True;
                     message (1) := To_Unbounded_String ("Negative");
                     message (2) := To_Unbounded_String ("cline length");
                     return;
                  end if;
               end if;
               --  if not_bad
            end if;
            --  if not_bad
            if bad_compt and then (z_index > 0)
            then
               message (1) := To_Unbounded_String ("Add or alter");
               message (2) := To_Unbounded_String ("best-guess");
               message (3) := To_Unbounded_String ("values");
            end if;
            if Manhat_on
            then
               --  * if Manhatton then fix dimensions *
               P2Ada_Var_17.width := Manh_width;
               P2Ada_Var_17.con_space := Manh_length;
               P2Ada_Var_17.lngth := Manh_length;
            end if;
            --  * if Manhatton *
         end;
      else
         gamma := freq / design_freq;
         if tcompt.all.super and then not (stripline)
         then
            ms_cl_dispersion (tcompt);
         end if;
         for i in 1 .. 2
         loop
            if i = 1
            then
               zd := tcompt.all.zed / Z0;
               elength := 2.0 * Pi * tcompt.all.wavelength;
               alpha_tl := (tcompt.all.alpha_d * gamma +
                              Rough_alpha (tcompt.all.alpha_c) * Sqrt (gamma))
                 * tcompt.all.lngth0;
            else
               zd := tcompt.all.zedo / Z0;
               elength := 2.0 * Pi * tcompt.all.wavelengtho;
               alpha_tl := (tcompt.all.alpha_do * gamma +
                              Rough_alpha (tcompt.all.alpha_co) * Sqrt (gamma))
                 * tcompt.all.lngth0;
            end if;
            beta_l := elength * gamma;
            sh.r := Sinh (alpha_tl) * Cos (beta_l);
            sh.i := Cosh (alpha_tl) * Sin (beta_l);
            ch.r := Cosh (alpha_tl) * Cos (beta_l);
            ch.i := Sinh (alpha_tl) * Sin (beta_l);
            co (seo (i) (1) (1), 2.0 * zd * ch.r + (((zd) ** 2) + 1.0) *
                  sh.r, 2.0 * zd * ch.i + (((zd) ** 2) + 1.0) * sh.i);
            rc (rds, seo (i) (1) (1));
            co (seo (i) (1) (2), 0.5 * (((zd) ** 2) - 1.0) * sh.r, 0.5 *
                (((zd) ** 2) - 1.0) * sh.i);
            prp (seo (i) (1) (1), seo (i) (1) (2), rds);
            sm (seo (i) (1) (2), zd, rds);
            seo (i) (2) (2) := seo (i) (1) (1);
            seo (i) (2) (1) := seo (i) (1) (2);
         end loop;
         --  for i
         c_s := null;
         for j in 1 .. 4
         loop
            if j > 2
            then
               mj := j - 2;
            else
               mj := j;
            end if;
            for i in 1 .. 4
            loop
               if c_s = null
               then
                  tcompt.all.s_begin := new s_parameter_record;
                  c_s := tcompt.all.s_begin;
               else
                  c_s.all.next_s := new s_parameter_record;
                  c_s := c_s.all.next_s;
               end if;
               c_s.all.next_s := null;
               c_s.all.z := new TMemComplex;
               if i > 2
               then
                  mi := i - 2;
               else
                  mi := i;
               end if;
               if (i > 2) xor (j > 2)
               then
                  di (c_s.all.z.all.c, seo (1) (mi) (mj), seo (2) (mi) (mj));
               else
                  su (c_s.all.z.all.c, seo (1) (mi) (mj), seo (2) (mi) (mj));
               end if;
            end loop;
            --  i
         end loop;
         --  j
      end if;
      --  action
   end clinesO;
   --  clinesO

   procedure lumpedO (tcompt : compt) is
      value : array (1 .. 4) of Long_Float;
      ff, zi : Long_Float;
      zo2, yb2, s11, s21 : TComplex;
      i, j : Integer;
      unit1, ident, prefix : Character;
      alt_param, parallel_cir : Boolean;
   begin
      if action
      then
         --  [P2Ada]: WITH instruction
         --  [P2Ada]: !Help! No type found -> add 'P2Ada_Var_18.' to fields
         declare P2Ada_Var_18 : compt_record renames tcompt.all;
         begin
            P2Ada_Var_18.number_of_con := 2;
            P2Ada_Var_18.con_space := 0.0;

            for i in value'Range loop
               value (i) := 0.0;
            end loop;

            unit1 := ' ';
            ident := ' ';
            prefix := ' ';
            alt_param := False;
            parallel_cir := False;
            Get_Lumped_Params (tcompt, value (1), value (2), value (3),
                               value (4), unit1, ident, prefix, alt_param,
                               parallel_cir);
            if bad_compt
            then
               return;
            end if;
            if alt_param
            then
               --  * Find the alt param -- allow only one *
               --  number of non-zero values
               j := 0;
               for i in 1 .. 3
               loop
                  if value (i) /= 0.0
                  then
                     j := j + 1;
                  end if;
               end loop;
               if j /= 1
               then
                  bad_compt := True;
                  message (1) := To_Unbounded_String ("One parameter");
                  message (2) := To_Unbounded_String ("only for");
                  message (3) := To_Unbounded_String ("swept lumped");
               else
                  if parallel_cir
                  then
                     --  j=1, value[1..3]
                     --  index of non-zero value
                     bad_compt := True;
                     message (1) := To_Unbounded_String ("Parallel circuit");
                     message (2) := To_Unbounded_String ("not allowed for");
                     message (3) := To_Unbounded_String ("swept lumped");
                  else
                     i := 0;
                     loop
                        i := i + 1;
                        exit when value (i) /= 0.0;
                     end loop;
                     --  if i >= 1 and then i <= 3
                     --  then
                     --  x_sweep.init_element (tcompt, ident, prefix, unit1);
                     --  end if;
                     x_sweep.Load_Index (i);
                  end if;
               end if;
            end if;
            --  if alt_param
            if bad_compt
            then
               return;
            end if;
            if Manhattan (tcompt) or else (value (4) = 0.0)
            then
               --  * Select Manhattan layout *
               P2Ada_Var_18.width := Manh_width;
               P2Ada_Var_18.lngth := 2.0 * Manh_width;
            else
               P2Ada_Var_18.lngth := value (4);
               P2Ada_Var_18.width := 0.0;
            end if;
            case unit1 is
               when Character (Omega) =>
                  P2Ada_Var_18.zed := value (1) / Z0;
                  P2Ada_Var_18.zedo := value (2) / Z0;
                  P2Ada_Var_18.wavelength := value (3) / Z0;
                  P2Ada_Var_18.spec_freq := 1.0;
               when 'z' | 'Z' =>
                  P2Ada_Var_18.zed := value (1);
                  P2Ada_Var_18.zedo := value (2);
                  P2Ada_Var_18.wavelength := value (3);
                  P2Ada_Var_18.spec_freq := 1.0;
               when 's' | 'S' =>
                  P2Ada_Var_18.zed := value (1) * Z0;
                  P2Ada_Var_18.zedo := value (2) * Z0;
                  P2Ada_Var_18.wavelength := value (3) * Z0;
                  P2Ada_Var_18.spec_freq := -1.0;
               when 'y' | 'Y' =>
                  P2Ada_Var_18.zed := value (1);
                  P2Ada_Var_18.zedo := value (2);
                  P2Ada_Var_18.wavelength := value (3);
                  P2Ada_Var_18.spec_freq := -1.0;
               when others =>
                  null;
            end case;
            --  case
            if P2Ada_Var_18.zed < 0.0
            then
               P2Ada_Var_18.zed := P2Ada_Var_18.zed * one;
            end if;
            if P2Ada_Var_18.lngth <= resln
            then
               bad_compt := True;
               message (1) := To_Unbounded_String ("lumped length");
               message (2) := To_Unbounded_String ("must be in m");
               message (3) := To_Unbounded_String ('>' & sresln);
            end if;
         end;
      else
         ff := freq / design_freq;
         declare P2Ada_Var_19 : compt_record renames tcompt.all;
         begin
            if P2Ada_Var_19.spec_freq > 0.0
            then
               --  z ,zedo=Ind wavlength=Cap
               if freq = 0.0
               then
                  if P2Ada_Var_19.wavelength = 0.0
                  then
                     co (s21, 1.0 / (1.0 + P2Ada_Var_19.zed / 2.0), 0.0);
                  else
                     co (s21, 0.0, 0.0);
                  end if;
               else
                  zi := (P2Ada_Var_19.zedo *
                           ff + P2Ada_Var_19.wavelength / ff) / 2.0;
                  co (zo2, 1.0 + P2Ada_Var_19.zed / 2.0, zi);
                  rc (s21, zo2);
               end if;
               --  y
               s11.r := 1.0 - s21.r;
               s11.i := -s21.i;
            else
               if freq = 0.0
               then
                  if P2Ada_Var_19.wavelength = 0.0
                  then
                     co (s11, 1.0 / (1.0 + P2Ada_Var_19.zed * 2.0), 0.0);
                  else
                     co (s11, 0.0, 0.0);
                  end if;
               else
                  zi := 2.0 * (P2Ada_Var_19.zedo *
                                 ff + P2Ada_Var_19.wavelength / ff);
                  co (yb2, 1.0 + 2.0 * P2Ada_Var_19.zed, zi);
                  rc (s11, yb2);
               end if;
               s21.r := 1.0 - s11.r;
               s21.i := -s11.i;
            end if;
         end;
         --  [P2Ada]: end of WITH
         c_s := null;
         for j in 1 .. 2
         loop
            for i in 1 .. 2
            loop
               if c_s = null
               then
                  tcompt.all.s_begin := new s_parameter_record;
                  c_s := tcompt.all.s_begin;
               else
                  c_s.all.next_s := new s_parameter_record;
                  c_s := c_s.all.next_s;
               end if;
               c_s.all.next_s := null;
               c_s.all.z := new TMemComplex;
               if i = j
               then
                  co (c_s.all.z.all.c, s11.r, s11.i);
               else
                  co (c_s.all.z.all.c, s21.r, s21.i);
               end if;
            end loop;
         end loop;
         --  i
      end if;
      --  action
   end lumpedO;

   procedure Transformer (tcompt : compt) is
      denom, turns_ratio, S11, S21 : Long_Float;
      unit1, prefix : Character;
      value_string : Unbounded_String;
      alt_param : Boolean;
   begin
      if action
      then
         declare P2Ada_Var_20 : compt_record renames tcompt.all;
         begin
            --  ! t_ratio has prefixes factored in - must catch as an error
            --  if ':' present it will return as unit1
            P2Ada_Var_20.number_of_con := 2;
            P2Ada_Var_20.con_space := 0.0;
            turns_ratio := 0.0;
            unit1 := ' ';
            prefix := ' ';
            alt_param := False;
            Get_Param (tcompt, 1, turns_ratio, value_string, unit1, prefix,
                       alt_param);
            if bad_compt
            then
               return;
            end if;
            if prefix /= ' '
            then
               --  ! no prefixes for unitless numbers
               bad_compt := True;
               message (1) := To_Unbounded_String ("No prefixes");
               message (2) := To_Unbounded_String ("allowed for");
               message (3) := To_Unbounded_String ("transformer");
               return;
            end if;
--  ??            if alt_param
--  ??            then
--  ??               x_sweep.init_element (tcompt, 't', prefix, unit1);
--  ??            end if;
            if bad_compt
            then
               return;
            end if;
            P2Ada_Var_20.width := Manh_length;
            P2Ada_Var_20.lngth := Manh_length;
            P2Ada_Var_20.zed := turns_ratio;
         end;
      else
         denom := ((tcompt.all.zed) ** 2) + 1.0;
         S11 := (denom - 2.0) / denom;
         S21 := 2.0 * tcompt.all.zed / denom;
         tcompt.all.s_begin := new s_parameter_record;
         c_s := tcompt.all.s_begin;
         c_s.all.z := new TMemComplex;
         co (c_s.all.z.all.c, S11, 0.0);
         for i in 2 .. 4
         loop
            c_s.all.next_s := new s_parameter_record;
            c_s := c_s.all.next_s;
            c_s.all.z := new TMemComplex;
            if i = 4
            then
               --  S22=-S11
               co (c_s.all.z.all.c, -S11, 0.0);
            else
               co (c_s.all.z.all.c, S21, 0.0);
            end if;
            --  S12=S21
         end loop;
         --  for i
         c_s.all.next_s := null;
      end if;
   end Transformer;
   --  * Transformer *
   --  *
   --  Procedure for drawing transformer and
   --  computing its s-parameters.
   --  S11 = S22 = 0
   --  S12 = S21 = tcompt^.zed
   --  *

   procedure Attenuator (tcompt : compt) is
      value : Long_Float;
      unit1, prefix : Character;
      value_string : Unbounded_String;
      alt_param : Boolean;
   begin
      if action
      then
         --  [P2Ada]: WITH instruction
         --  [P2Ada]: !Help! No type found -> add 'P2Ada_Var_21.' to fields
         declare P2Ada_Var_21 : compt_record renames tcompt.all;
         begin
            --  * value_string, prefix are ignored
            --  value has prefixes factored in at this point
            --  if entered in dB then unit1 returns 'd' or 'D'
            --  if no unit then unit1 = '?'
            P2Ada_Var_21.number_of_con := 2;
            P2Ada_Var_21.con_space := 0.0;
            value := 0.0;
            unit1 := ' ';
            prefix := ' ';
            alt_param := False;

            Get_Param (tcompt, 1, value, value_string, unit1, prefix,
                       alt_param);
            if bad_compt
            then
               return;
            end if;
            if prefix /= ' '
            then
               bad_compt := True;
               message (1) := To_Unbounded_String ("No prefixes");
               message (2) := To_Unbounded_String ("allowed for");
               message (3) := To_Unbounded_String ("attenuator");
               return;
            end if;
            --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
            if not (unit1 = 'd'
                    or else unit1 = 'D'
                    or else unit1 = '?')
            then
               bad_compt := True;
               message (1) := To_Unbounded_String ("Enter");
               message (2) := To_Unbounded_String ("attenuation");
               message (3) := To_Unbounded_String ("in dB");
               return;
            end if;
            if abs (value) > 99.0
            then
               bad_compt := True;
               message (1) := To_Unbounded_String ("Attenuation");
               message (2) := To_Unbounded_String ("out of range");
               return;
            end if;
--  ??            if alt_param
--  ??            then
--  ??               x_sweep.init_element (tcompt, 'a', prefix, unit1);
--  ??            end if;
            if bad_compt
            then
               return;
            end if;
            --  * Draw as a square box using *
            --  * Manhattan dimensions  *
            --  zed is dB value of S12,S21
            P2Ada_Var_21.width := 2.0 * Manh_width;
            P2Ada_Var_21.lngth := P2Ada_Var_21.width;
            P2Ada_Var_21.zed := value;
         end;
         --  [P2Ada]: end of WITH
         --  with
         --  action
         --  * Fill frequency independent scattering parameters *
         --  convert from dB for S12, S21
         --  S11
      else
         value := Exp (-ln10 * tcompt.all.zed / 20.0);
         tcompt.all.s_begin := new s_parameter_record;
         c_s := tcompt.all.s_begin;
         c_s.all.z := new TMemComplex;
         co (c_s.all.z.all.c, 0.0, 0.0);
         for i in 2 .. 4
         loop
            c_s.all.next_s := new s_parameter_record;
            c_s := c_s.all.next_s;
            c_s.all.z := new TMemComplex;
            if i = 4
            then
               --  S22
               co (c_s.all.z.all.c, 0.0, 0.0);
            else
               co (c_s.all.z.all.c, value, 0.0);
            end if;
            --  S12=S21
         end loop;
         --  for i
         c_s.all.next_s := null;
      end if;
   end Attenuator;

   procedure Device_S (tcompt : compt; indef : Boolean) is
      --  * Device_S *
      c_ss, c_f, c_is : s_param;
      s1, s2 : array (1 .. 10, 1 .. 10) of PMemComplex;
      found : Boolean := indef;
      txpt, tnpts : Integer;
      f1, f2, tfreq, tfmin, ffac : Long_Float;
   begin
      if Alt_Sweep
      then
         --  load twice for alt_sweep
         tnpts := 1;
      else
         tnpts := npts;
      end if;
      if xpt = 0
      then
         --  [P2Ada]: WITH instruction
         --  [P2Ada]: !Help! No type found -> add 'P2Ada_Var_22.' to fields
         declare P2Ada_Var_22 : compt_record renames tcompt.all;
         begin
            if P2Ada_Var_22.f_file /= null
            then
               tfmin := P2Ada_Var_22.f_file.all.z.all.c.r;
            end if;
            --  first frequency
            P2Ada_Var_22.s_ifile := null;
            for txpt in 0 .. tnpts
            loop
               if P2Ada_Var_22.f_file /= null
               then
                  if txpt = tnpts
                  then
                     tfreq := sxmax;
                  else
                     tfreq := fmin + Long_Float (txpt) * finc;
                  end if;
                  if Alt_Sweep
                  then
                     tfreq := design_freq;
                  end if;
                  found := False;
                  if tfmin <= tfreq
                  then
                     c_f := null;
                     c_ss := null;
                     loop
                        if c_f = null
                        then
                           c_f := P2Ada_Var_22.f_file;
                        else
                           c_f := c_f.all.next_s;
                        end if;
                        for j in 1 .. P2Ada_Var_22.number_of_con
                        loop
                           for i in 1 .. P2Ada_Var_22.number_of_con
                           loop
                              if c_ss = null
                              then
                                 c_ss := P2Ada_Var_22.s_file;
                              else
                                 c_ss := c_ss.all.next_s;
                              end if;
                              s1 (i, j) := c_ss.all.z;
                           end loop;
                        end loop;
                        --  i j
                        if c_f.all.next_s.all.z.all.c.r >= tfreq
                        then
                           found := True;
                        end if;
                        exit when found or else
                          (c_f.all.next_s.all.next_s = null);
                     end loop;
                     --  Get endpoints for interpolation
                     if found
                     then
                        f1 := c_f.all.z.all.c.r;
                        f2 := c_f.all.next_s.all.z.all.c.r;
                        for j in 1 .. P2Ada_Var_22.number_of_con
                        loop
                           for i in 1 .. P2Ada_Var_22.number_of_con
                           loop
                              c_ss := c_ss.all.next_s;
                              s2 (i, j) := c_ss.all.z;
                           end loop;
                        end loop;
                        --  for i,j
                     end if;
                  end if;
                  --  if tfmin < tfreq
                  --  if f_file=nil
               else
                  if txpt = 0
                  then
                     c_ss := null;
                     found := True;
                     for j in 1 .. P2Ada_Var_22.number_of_con
                     loop
                        for i in 1 .. P2Ada_Var_22.number_of_con
                        loop
                           if c_ss = null
                           then
                              c_ss := P2Ada_Var_22.s_file;
                           else
                              c_ss := c_ss.all.next_s;
                           end if;
                           s1 (i, j) := c_ss.all.z;
                           s2 (i, j) := s1 (i, j);
                        end loop;
                     end loop;
                     --  for i,j
                  end if;
                  --  if txpt=0
               end if;
               --  if f_file
               for j in 1 .. P2Ada_Var_22.number_of_con
               loop
                  for i in 1 .. P2Ada_Var_22.number_of_con
                  loop
                     if P2Ada_Var_22.s_ifile = null
                     then
                        P2Ada_Var_22.s_ifile := new s_parameter_record;
                        c_is := P2Ada_Var_22.s_ifile;
                     else
                        c_is.all.next_s := new s_parameter_record;
                        c_is := c_is.all.next_s;
                     end if;
                     c_is.all.next_s := null;
                     if found
                     then
                        --  *device s-parameter interpolation routine*
                        c_is.all.z := new TMemComplex;
                        if P2Ada_Var_22.f_file = null
                        then
                           ffac := 0.0;
                        else
                           ffac := (tfreq - f1) / (f2 - f1);
                        end if;
                        --  found = true
                        c_is.all.z.all.c.r := s1 (i, j).all.c.r + ffac *
                          (s2 (i, j).all.c.r - s1 (i, j).all.c.r);
                        c_is.all.z.all.c.i := s1 (i, j).all.c.i + ffac *
                          (s2 (i, j).all.c.i - s1 (i, j).all.c.i);
                     else
                        c_is.all.z := null;
                     end if;
                  end loop;
               end loop;
               --  for i,j to number_of_con
            end loop;
            --  for txpt
         end;
      end if;
      --  if xpt:=0
      if Alt_Sweep
      then
         --  only one freq
         txpt := 1;
      else
         txpt := xpt;
      end if;
      declare P2Ada_Var_23 : compt_record renames tcompt.all;
      begin
         P2Ada_Var_23.s_begin := P2Ada_Var_23.s_ifile;
         for k in 0 .. txpt - 1
         loop
            --  advance to next freq
            for j in 1 .. P2Ada_Var_23.number_of_con
            loop
               for i in 1 .. P2Ada_Var_23.number_of_con
               loop
                  P2Ada_Var_23.s_begin := P2Ada_Var_23.s_begin.all.next_s;
                  if P2Ada_Var_23.s_begin = null
                  then
                     message (2) := To_Unbounded_String ("s out of range");
                     shutdown;
                  end if;
               end loop;
            end loop;
         end loop;
         --  for i,j,k
         if P2Ada_Var_23.s_begin.all.z = null
         then
            bad_compt := True;
            Erase_Message;
            message (1) := To_Unbounded_String ("Frequency out of");
            message (2) := To_Unbounded_String ("   range given   ");
            message (3) := To_Unbounded_String ("in device file");
         end if;
      end;
      --  [P2Ada]: end of WITH
      --  with
   end Device_S;

   procedure dx_dyO is
      --  *****************************************************
      pos_prefix : constant Unbounded_String :=
        To_Unbounded_String ("EPTGMk m" & Character (Mu) & "npfa");
      i, j : Integer;
      dx, dy : Long_Float;
      dx_prefix, dy_prefix : Character;

      procedure Big_Check (dt : in out Long_Float; i_in : in out Integer) is
      begin
         while abs (dt) > 1000.0
         loop
            --  set-up prefix change
            dt := dt / 1000.0;
            i_in := i_in - 1;
         end loop;
      end Big_Check;
      --  *****************************************************

      procedure Small_Check (dt : in out Long_Float; i_in : in out Integer) is
      begin
         while abs (dt) < 0.01
         loop
            --  set-up prefix change
            dt := dt * 1000.0;
            i_in := i_in + 1;
         end loop;
      end Small_Check;
      --  *****************************************************
      --  Initial numbers are in mm
      --  start index is 'm' in mm
      --  Check for large or small values of dx,dy
   begin
      dx := (xm - xrold);
      dy := (ym - yrold);
      xrold := xm;
      yrold := ym;
      i := 8;
      if dx /= 0.0
      then
         Big_Check (dx, i);
         Small_Check (dx, i);
      end if;
      --  start index is 'm' in mm
      dx_prefix := Element (pos_prefix, i);
      j := 8;
      if dy /= 0.0
      then
         Big_Check (dy, j);
         Small_Check (dy, j);
      end if;
      dy_prefix := Element (pos_prefix, j);
      if read_kbd
      then
         TextCol (LightGray);
         GotoXY (Integer_32 (xmin (6) + 2), Integer_32 (ymin (6) + 1));
         if abs (dx) > resln
         then
            PutCh (Delta_Ch);
            PutStr ("x ");
            PutFloat (dx, 7, 3, 0);
            PutCh (char (dx_prefix));
            PutCh ('m');
            GotoXY (Integer_32 (xmin (6) + 2), Integer_32 (ymin (6)));
         else
            GotoXY (Integer_32 (xmin (6) + 2), Integer_32 (ymin (6) + 1));
         end if;
         if abs (dy) > resln
         then
            PutCh (Delta_Ch);
            PutStr ("y ");
            PutFloat (-dy, 7, 3, 0);
            PutCh (char (dy_prefix));
            PutCh ('m');
         end if;
      end if;
   end dx_dyO;

   function get_lead_charO (tcompt : compt) return Character is
      --  [BP2P]: Label "100001" Was "exit_label"
      Result_get_lead_charO : Character;
      xstr : Unbounded_String;
      char1 : Character;
   begin
      xstr := tcompt.all.descript;
      Delete (xstr, 1, 1);
      char1 := Element (xstr, 1);
      loop
         if Element (xstr, 1) = ' '
         then
            Delete (xstr, 1, 1);
         end if;
         if Length (xstr) = 0
         then
            --  [BP2P]: Label "100001" Was "exit_label"
            goto LABEL_100001;
         end if;
         char1 := Element (xstr, 1);
         exit when Element (xstr, 1) /= ' ';
      end loop;
      --  [BP2P]: Label "100001" Was "exit_label"
      --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
      <<LABEL_100001>>
      if char1 >= 'A' and then char1 <= 'Z'
      then
         char1 := To_Lower (char1);
         --  char1 := Character (Character'Pos (char1) + 32);
      end if;
      Result_get_lead_charO := char1;
      return Result_get_lead_charO;
   end get_lead_charO;
   --  get_lead_charO
   --  *
   --  Draw a transmission line and lumped element on the circuit board.
   --  *

   procedure Draw_tline (tnet : net; linex, seperate : Boolean) is
      x1, x2, y1, y2, x3, y3, x1e, y1e, x2e, y2e : Integer;
      x1r, y1r, x2r, y2r : Long_Float;
   begin
      x1r := tnet.all.xr - lengthxm * Long_Float (yii) / 2.0;
      x2r := x1r + lengthxm * Long_Float (xii + yii);
      y1r := tnet.all.yr - lengthym * Long_Float (xii) / 2.0;
      y2r := y1r + lengthym * Long_Float (yii + xii);
      x1 := Round (x1r / csx + Long_Float (xmin (1)));
      x2 := Round (x2r / csx + Long_Float (xmin (1)));
      x3 := Round ((x1r + x2r) / (2.0 * csx) + Long_Float (xmin (1)));
      y1 := Round (y1r / csy + Long_Float (ymin (1)));
      y2 := Round (y2r / csy + Long_Float (ymin (1)));
      y3 := Round ((y1r + y2r) / (2.0 * csy) + Long_Float (ymin (1)));
      x1e := Round (tnet.all.xr / csx) + xmin (1);
      y1e := Round (tnet.all.yr / csy) + ymin (1);
      x2e := Round ((tnet.all.xr + lengthxm *
                      Long_Float (xii)) / csx) + xmin (1);
      y2e := Round ((tnet.all.yr + lengthym *
                      Long_Float (yii)) / csy) + ymin (1);
      if linex
      then
         fill_box (x1, y1, x2, y2, Brown);
      else
         if x1 = x2
         then
            --  make 50 Ohms wide if width=0
            x1 := Round (Long_Float (x1) - widthZ0 / (2.0 * csx));
            x2 := Round (Long_Float (x2) + widthZ0 / (2.0 * csx));
         else
            if y1 = y2
            then
               --  make 50 Ohms wide if width=0
               y1 := Round (Long_Float (y1) - widthZ0 / (2.0 * csy));
               y2 := Round (Long_Float (y2) + widthZ0 / (2.0 * csy));
            end if;
         end if;
         --  if width<>0 i.e. Manhattan, just draw it
         Draw_Box (x1, y1, x2, y2, LightBlue);
         if tnet.all.com.all.typ = 'a'
         then
            --  * Make small boxes for atten terminals *
            box (x1e, y1e, 1);
            box (x2e, y2e, 1);
         end if;
      end if;
      if seperate
      then
         x1r := (tnet.all.xr + tnet.all.com.all.con_space *
                   Long_Float (yii) / 2.0) / csx + Long_Float (xmin (1));
         y1r := (tnet.all.yr + tnet.all.com.all.con_space *
                   Long_Float (xii) / 2.0) / csy + Long_Float (ymin (1));
         if not ((x1 = x2) or else (y1 = y2))
         then
            SetCol (Black);
            Line (Integer_32 (Round (x1r)),
                  Integer_32 (Round (y1r)),
                  Integer_32 (Round (x1r + lengthxm *
                      Long_Float (xii) / csx)),
                  Integer_32 (Round (y1r + lengthym *
                      Long_Float (yii) / csy)));
         end if;
      end if;
      --  * Write graphics character to identify part *
      --  make sure center dot is black
      SetTextJustify (centertext, centertext);
      PutPixel (Integer_32 (x3), Integer_32 (y3), Black);
      SetCol (Black);
      for i in -1 .. 1
      loop
         for j in -1 .. 1
         loop
            OutTextXY (x3 + i, y3 + j,
                       Interfaces.C.To_C
                         ("" & Element (tnet.all.com.all.descript, 1)));
         end loop;
      end loop;
      --  shadow
      --  letter
      SetCol (LightRed);
      OutTextXY (x3, y3,
                 Interfaces.C.To_C
                   ("" & Element (tnet.all.com.all.descript, 1)));
   end Draw_tline;
   --  * Draw_tline *
   --  *
   --  Draw a transformer on the circuit board.
   --  *

   procedure Draw_xformer (tnet : net) is
      --  center text for OutText
      --  * Make small boxes for xformer terminals *
      x1, x2, y1, y2, x3, y3, xt, yt : Integer;
   begin
      x1 := Round (tnet.all.xr / csx) + xmin (1);
      y1 := Round (tnet.all.yr / csy) + ymin (1);
      x2 := Round ((tnet.all.xr + lengthxm * Long_Float (xii)) /
                     csx) + xmin (1);
      y2 := Round ((tnet.all.yr + lengthym * Long_Float (yii)) /
                     csy) + ymin (1);
      xt := Round (Long_Float (yii) * lengthym / (2.0 * csx));
      yt := Round (Long_Float (xii) * lengthxm / (2.0 * csy));
      x3 := Round (Long_Float (x1 + x2) / 2.0);
      y3 := Round (Long_Float (y1 + y2) / 2.0);
      SetTextJustify (centertext, centertext);
      SetCol (LightBlue);
      Line (Integer_32 (x1 - xt),
            Integer_32 (y1 - yt),
            Integer_32 (x1 + xt),
            Integer_32 (y1 + yt));
      Line (Integer_32 (x1 + xt),
            Integer_32 (y1 + yt),
            Integer_32 (x2 + (xt / 2)),
            Integer_32 (y2 + (yt / 2)));
      Line (Integer_32 (x2 + (xt / 2)),
            Integer_32 (y2 + (yt / 2)),
            Integer_32 (x2 - (xt / 2)),
            Integer_32 (y2 - (yt / 2)));
      Line (Integer_32 (x2 - (xt / 2)),
            Integer_32 (y2 - (yt / 2)),
            Integer_32 (x1 - xt),
            Integer_32 (y1 - yt));
      SetCol (LightRed);
      OutTextXY (x3, y3,
                 Interfaces.C.To_C
                   ("" & Element (tnet.all.com.all.descript, 1)));
      box (x1, y1, 1);
      box (x2, y2, 1);
   end Draw_xformer;

   procedure Draw_device (tnet : net) is
      sx1, sy1 : Long_Float;
      x1, x2, y1, y2, x3, y3, xt, yt : Integer;
   begin
      x1 := Round (tnet.all.xr / csx) + xmin (1);
      y1 := Round (tnet.all.yr / csy) + ymin (1);
      x2 := Round ((tnet.all.xr + lengthxm *
                     Long_Float (xii)) / csx) + xmin (1);
      y2 := Round ((tnet.all.yr + lengthym *
                     Long_Float (yii)) / csy) + ymin (1);
      xt := Round (Long_Float (yii) * lengthym / (2.0 * csx));
      yt := Round (Long_Float (xii) * lengthxm / (2.0 * csy));
      x3 := Round (Long_Float ((2 * x1 + x2)) / 3.0);
      y3 := Round (Long_Float ((2 * y1 + y2)) / 3.0);
      SetTextJustify (centertext, centertext);
      SetCol (LightBlue);
      Line (Integer_32 (x1 - xt),
            Integer_32 (y1 - yt),
            Integer_32 (x1 + xt),
            Integer_32 (y1 + yt));
      Line (Integer_32 (x1 + xt),
            Integer_32 (y1 + yt),
            Integer_32 (x2),
            Integer_32 (y2));
      Line (Integer_32 (x2),
            Integer_32 (y2),
            Integer_32 (x1 - xt),
            Integer_32 (y1 - yt));
      SetCol (LightRed);
      OutTextXY (x3, y3,
                 Interfaces.C.To_C
                   ("" & Element (tnet.all.com.all.descript, 1)));
      declare P2Ada_Var_24 : net_record renames tnet.all;
      begin
         if P2Ada_Var_24.number_of_con = 1
         then
            box (x1, y1, 1);
         else
            sx1 := Long_Float ((x2 - x1) / (P2Ada_Var_24.number_of_con - 1));
            sy1 := Long_Float ((y2 - y1) / (P2Ada_Var_24.number_of_con - 1));
            for i in 0 .. P2Ada_Var_24.number_of_con - 1
            loop
               box (x1 + Round (Long_Float (i) * sx1),
                    y1 + Round (Long_Float (i) * sy1), 1);
            end loop;
            if tnet.all.com.all.typ = 'i'
            then
               if P2Ada_Var_24.number_of_con = 3
               then
                  --  Yellow box at port 2
                  box (x1 + Round (sx1), y1 + Round (sy1), 4);
               else
                  box (x1 + Round (Long_Float (P2Ada_Var_24.number_of_con - 1)
                       * sx1),
                       y1 + Round (Long_Float (P2Ada_Var_24.number_of_con - 1)
                       * sy1), 4);
               end if;
               --  Yellow box at last port
            end if;
         end if;
      end;
      --  [P2Ada]: end of WITH
   end Draw_device;
   --  * Draw_device *
   --  *
   --  Add a record to the list of paramters used
   --  in the plot window or the board window.
   --  Used below in Set_Up_KeyO and Set_Up_Board
   --  *

   procedure Add_Coord (x1, xb, xl, y1 : Integer; just, brd : Boolean;
                        tdes : Unbounded_String) is
   begin
      if ccompt = null
      then
         if brd
         then
            ccompt := board_start;
         else
            ccompt := coord_start;
         end if;
      else
         ccompt := ccompt.all.next_compt;
      end if;
      declare P2Ada_Var_25 : compt_record renames ccompt.all;
      begin
         xpt := x1;
         P2Ada_Var_25.xorig := x1;
         P2Ada_Var_25.xmaxl := xl;
         P2Ada_Var_25.x_block := xb;
         P2Ada_Var_25.yp := y1;
         P2Ada_Var_25.right := just;
         P2Ada_Var_25.descript := tdes;
      end;
   end Add_Coord;

   procedure Set_Up_KeyO is
   begin
      ccompt := null;
      Add_Coord (x_y_plot_text (1, 1), 0, 5, x_y_plot_text (1, 2), True, False,
                 s_key (1));
      dBmax_ptr := ccompt;
      Add_Coord (x_y_plot_text (3, 1), 0, 5, x_y_plot_text (3, 2), True, False,
                 s_key (2));
      Add_Coord (x_y_plot_text (4, 1), 0, 7, x_y_plot_text (4, 2), False,
                 False, s_key (3));
      fmin_ptr := ccompt;
      Add_Coord (x_y_plot_text (6, 1), 0, 7, x_y_plot_text (6, 2), True, False,
                 s_key (4));
      Add_Coord (xmin (2), 7, 12, ymin (2), False, False,
                 "Points " & s_key (5));
      Points_compt := ccompt;
      Add_Coord (xmin (2), 13, 17, ymin (2) + 1, False, False,
                 "Smith radius " & s_key (6));
      rho_fac_compt := ccompt;
      Add_Coord (xmin (2) + 2, 1, 3, ymin (2) + 3, False, False,
                 "S" & s_key (7));
      s_param_table (1) := ccompt;
      Add_Coord (xmin (2) + 2, 1, 3, ymin (2) + 4, False, False,
                 "S" & s_key (8));
      s_param_table (2) := ccompt;
      Add_Coord (xmin (2) + 2, 1, 3, ymin (2) + 5, False, False,
                 "S" & s_key (9));
      s_param_table (3) := ccompt;
      Add_Coord (xmin (2) + 2, 1, 3, ymin (2) + 6, False, False,
                 "S" & s_key (10));
      s_param_table (4) := ccompt;
   end Set_Up_KeyO;
   --  *Set_Up_KeyO*
   --  *
   --  Update the locations of the KeyO parameters after a screen resize
   --  *

   procedure Update_KeyO_locations is
      c : compt;
   begin
      c := coord_start;
      c.all.xp := x_y_plot_text (1, 1);
      c.all.xorig := c.all.xp;
      c.all.yp := x_y_plot_text (1, 2);
      c := c.all.next_compt;
      c.all.xp := x_y_plot_text (3, 1);
      c.all.xorig := c.all.xp;
      c.all.yp := x_y_plot_text (3, 2);
      c := c.all.next_compt;
      c.all.xp := x_y_plot_text (4, 1);
      c.all.xorig := c.all.xp;
      c.all.yp := x_y_plot_text (4, 2);
      c := c.all.next_compt;
      c.all.xp := x_y_plot_text (6, 1);
      c.all.xorig := c.all.xp;
      c.all.yp := x_y_plot_text (6, 2);
      c := c.all.next_compt;
      c.all.xp := xmin (2);
      c.all.xorig := c.all.xp;
      c.all.yp := ymin (2);
      c := c.all.next_compt;
      c.all.xp := xmin (2);
      c.all.xorig := c.all.xp;
      c.all.yp := ymin (2) + 1;
      c := c.all.next_compt;
      c.all.xp := xmin (2) + 2;
      c.all.xorig := c.all.xp;
      c.all.yp := ymin (2) + 3;
      c := c.all.next_compt;
      c.all.xp := xmin (2) + 2;
      c.all.xorig := c.all.xp;
      c.all.yp := ymin (2) + 4;
      c := c.all.next_compt;
      c.all.xp := xmin (2) + 2;
      c.all.xorig := c.all.xp;
      c.all.yp := ymin (2) + 5;
      c := c.all.next_compt;
      c.all.xp := xmin (2) + 2;
      c.all.xorig := c.all.xp;
      c.all.yp := ymin (2) + 6;
   end Update_KeyO_locations;

   procedure Set_Up_Board is
   begin
      ccompt := null;
      Add_Coord (xmin (4), 4, 16, ymin (4), False, True,
                 "zd  " & s_board (1, 1) & ' ' & s_board (1, 2) &
                   Character (Omega));
      Add_Coord (xmin (4), 4, 16, ymin (4) + 1, False, True,
                 "fd  " & s_board (2, 1) & ' ' & s_board (2, 2) & "Hz");
      Add_Coord (xmin (4), 4, 16, ymin (4) + 2, False, True,
                 "er  " & s_board (3, 1));
      Add_Coord (xmin (4), 4, 16, ymin (4) + 3, False, True,
                 "h   " & s_board (4, 1) & ' ' & s_board (4, 2) & 'm');
      Add_Coord (xmin (4), 4, 16, ymin (4) + 4, False, True,
                 "s   " & s_board (5, 1) & ' ' & s_board (5, 2) & 'm');
      Add_Coord (xmin (4), 4, 16, ymin (4) + 5, False, True,
                 "c   " & s_board (6, 1) & ' ' & s_board (6, 2) & 'm');
   end Set_Up_Board;
   --  *Set_Up_Board*
   --  *
   --  Determine screen and artwork layout variables.
   --  Called in Read_Board().
   --  Uses only global variables.
   --  Must be called after any change to bmax (s),z0 (zd).
   --  Sets all parts to ^.changed=true, forcing parsing
   --  therefore, call after changes to fd,er,h
   --  *

   procedure Fresh_Dimensions is
      tcompt : compt;
   begin
      csy := bmax / Long_Float (ymax (1) - ymin (1));
      csx := csy * yf;
      Manh_length := 0.1 * bmax;
      Manh_width := 0.05 * bmax;
      if abs (con_sep) > bmax
      then
         con_sep := bmax;
      end if;
      if Manhattan_Board
      then
         widthZ0 := Manh_width;
      else
         widthZ0 := widtht (Z0) + artwork_cor;
      end if;
      pwidthxZ02 := Round (widthZ0 * 0.5 / psx);
      pwidthyZ02 := Round (widthZ0 * 0.5 / psy);
      cwidthxZ02 := Round (widthZ0 * 0.5 / csx);
      cwidthyZ02 := Round (widthZ0 * 0.5 / csy);
      Rs_at_fd := Sqrt (Pi * design_freq *
                          Eng_Prefix (Character (freq_prefix)) * Mu_0 /
                          conductivity);
      Lambda_fd := c_in_mm / (design_freq *
                          Eng_Prefix (Character (freq_prefix)));
      tcompt := null;
      loop
         --  * Force parsing of each part *
         if tcompt = null
         then
            tcompt := part_start;
         else
            tcompt := tcompt.all.next_compt;
         end if;
         tcompt.all.changed := True;
         exit when tcompt.all.next_compt = null;
      end loop;
   end Fresh_Dimensions;
   --  * Fresh_Dimensions *
   --  *
   --  Draw ground on circuit.
   --  *

   procedure draw_groundO (xr, yr : Long_Float) is
      x1, y1 : Integer;
   begin
      if read_kbd or else demo_mode
      then
         x1 := Round (xr / csx) + xmin (1);
         y1 := Round (yr / csy) + ymin (1);
         SetCol (Yellow);
         Line (Integer_32 (x1 - 4),
               Integer_32 (y1),
               Integer_32 (x1 + 4),
               Integer_32 (y1));
         Line (Integer_32 (x1 - 2),
               Integer_32 (y1 + 2),
               Integer_32 (x1 + 2),
               Integer_32 (y1 + 2));
         Line (Integer_32 (x1 - 1),
               Integer_32 (y1 + 4),
               Integer_32 (x1 + 1),
               Integer_32 (y1 + 4));
      end if;
      --  read_kbd
   end draw_groundO;
   --  draw_groundO
   --  Look back at network to see how cline connection should be made.
   --  Called by Add_Net, puff.pas
   --  mate_node is a global array[1..4] of net;
   --  Mate_Node is set up for possible use in Add_Net.
   --  look_back is called as follows:
   --  special_coupler:=Look_Back;
   --  for i:=1 to compt1^.number_of_con do begin
   --  if special_coupler and (i in[1,3]) then begin
   --  cnet:=mate_node[i];
   --  cnet^.number_of_con:=cnet^.number_of_con+1;
   --  end;
   --  etc...
   --  end;

   function look_backO return Boolean is
      Result_Look_BackO : Boolean;
      tcon, scon, mtcon, mscon : conn;
      x1, x2, y1, y2, cs, cm : Long_Float;
      coupler_found : Boolean;
      tnet : net;
      d2 : Integer;
   begin
      Result_Look_BackO := False;
      coupler_found := False;
      if cnet /= null
      then
         if compt1.all.typ = 'c'
         then
            --  if a cline then
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
                  if tcon.all.mate.all.the_net.all.com.all.typ = 'c'
                  then
                     --  if cline to cline
                     --  find averages of connector separation between old and
                     --  new
                     cs := (tcon.all.mate.all.the_net.all.com.all.con_space +
                              compt1.all.con_space) / 2.0;
                     cm := (tcon.all.mate.all.the_net.all.com.all.con_space -
                              compt1.all.con_space) / 2.0;
                     d2 := Integer (tcon.all.dir);
                     mtcon := tcon.all.mate;
                     case tcon.all.mate.all.conn_no is
                        --  look at conns old clines
                        when 1 | 2 =>
                           --  advance 2 connectors
                           mscon := mtcon.all.next_con.all.next_con;
                        when 3 =>
                           --  con 3 -> con 1
                           mscon := mtcon.all.the_net.all.con_start;
                        when 4 =>
                           --  con 4 -> con 2
                           mscon := mtcon.all.the_net.all.con_start.all
                             .next_con;
                        when others =>
                           --  [P2Ada]: no otherwise / else in Pascal
                           null;
                     end case;
                     --  case
                     --  change to ccon form
                     --  note current marker positions
                     --  note old clines position
                     scon := mscon.all.mate;
                     mate_node (1) := cnet;
                     x1 := mate_node (1).all.xr;
                     y1 := mate_node (1).all.yr;
                     mate_node (3) := scon.all.the_net;
                     x2 := mate_node (3).all.xr;
                     y2 := mate_node (3).all.yr;
                     coupler_found := True;
                  end if;
               end if;
               --  if cline to cline
               exit when tcon.all.next_con = null;
            end loop;
            if coupler_found
            then
               --  if coupler to coupler connection
               if abs (x1 - x2) < resln
               then
                  --  if both in same x position
                  if y1 > y2
                  then
                     --  if cursor above old clines
                     case dirn is
                        when 2 =>
                           ym := ym - cs;
                           tnet := mate_node (1);
                           mate_node (1) := mate_node (3);
                           mate_node (3) := tnet;
                           Result_Look_BackO := True;
                        when 4 =>
                           ym := ym - cm;
                           Result_Look_BackO := True;
                        when 8 =>
                           if d2 = 2
                           then
                              dirn := 4;
                              ym := ym + compt1.all.con_space;
                           else
                              dirn := 2;
                           end if;
                        when others =>
                           --  [P2Ada]: no otherwise / else in Pascal
                           null;
                     end case;
                     --  case
                     --  y1>y2
                  else
                     case dirn is
                        when 4 =>
                           ym := ym + cs;
                           tnet := mate_node (1);
                           mate_node (1) := mate_node (3);
                           mate_node (3) := tnet;
                           Result_Look_BackO := True;
                        when 2 =>
                           ym := ym + cm;
                           Result_Look_BackO := True;
                        when 1 =>
                           if d2 = 4
                           then
                              dirn := 2;
                              ym := ym - compt1.all.con_space;
                           else
                              dirn := 4;
                           end if;
                        when others =>
                           --  [P2Ada]: no otherwise / else in Pascal
                           null;
                     end case;
                     --  case
                  end if;
                  --  y1 > y2
                  --  abs
               else
                  if x1 > x2
                  then
                     case dirn is
                        when 8 =>
                           xm := xm - cs;
                           tnet := mate_node (1);
                           mate_node (1) := mate_node (3);
                           mate_node (3) := tnet;
                           Result_Look_BackO := True;
                        when 1 =>
                           xm := xm - cm;
                           Result_Look_BackO := True;
                        when 2 =>
                           if d2 = 8
                           then
                              dirn := 1;
                              xm := xm + compt1.all.con_space;
                           else
                              dirn := 8;
                           end if;
                        when others =>
                           --  [P2Ada]: no otherwise / else in Pascal
                           null;
                     end case;
                     --  case
                     --  x1 >x2
                  else
                     case dirn is
                        when 1 =>
                           xm := xm + cs;
                           tnet := mate_node (1);
                           mate_node (1) := mate_node (3);
                           mate_node (3) := tnet;
                           Result_Look_BackO := True;
                        when 8 =>
                           xm := xm + cm;
                           Result_Look_BackO := True;
                        when 4 =>
                           if d2 = 1
                           then
                              --  i.e. d2=8
                              dirn := 8;
                              xm := xm - compt1.all.con_space;
                           else
                              dirn := 1;
                           end if;
                        when others =>
                           --  [P2Ada]: no otherwise / else in Pascal
                           null;
                     end case;
                     --  case
                  end if;
                  --  x1 > x2
               end if;
            end if;
            --  if abs
         end if;
      end if;
      --  if compt1
      return Result_Look_BackO;
   end look_backO;

   procedure super_stripline (tcompt : compt) is
      W, b, x, m, W_prime, b_t_W, W_over_b_t, delta_W_b_t, A_fac, Z_0_t, Q_fac,
      partial_Z_w, log_term : Long_Float;
   begin
      if metal_thickness > 0.0
      then
         --  * Calculation of Z_0 with finite thickness metal *
         --  * Calculation of alpha_c *
         --  W_prime defines units
         --  ! beware of sign errors here!
         W := tcompt.all.width;
         b := substrate_h;
         x := metal_thickness / b;
         m := 2.0 / (1.0 + 2.0 * x / (3.0 * (1.0 - x)));
         delta_W_b_t := x * (1.0 - 0.5 * Log (((x / (2.0 - x)) ** 2) +
                               Exp (m * Log (0.0796 * x /
                                 ((W / b) + 1.1 * x))))) / (Pi * (1.0 - x));
         W_over_b_t := (W / (b - metal_thickness)) + delta_W_b_t;
         W_prime := (b - metal_thickness) * W_over_b_t;
         b_t_W := 1.0 / W_over_b_t;
         A_fac := Log (1.0 + (4.0 * b_t_W / Pi) * ((8.0 * b_t_W / Pi) +
                         Sqrt (((8.0 * b_t_W / Pi) ** 2) + 6.27))) / Pi;
         Z_0_t := 30.0 * Pi * A_fac / Sqrt (er);
         Q_fac := Sqrt (1.0 + 6.27 * ((Pi * W_over_b_t / 8.0) ** 2));
         partial_Z_w := 30.0 * (3.135 / Q_fac - (1.0 + Q_fac - 0.303 / Q_fac) *
                                ((8.0 / (Pi * W_over_b_t)) ** 2)) /
           (Exp (Pi * A_fac) * W_prime);
         log_term := 1.0 + 2.0 * W_over_b_t - (3.0 * x / (2.0 - x) +
                                               Log (x / (2.0 - x))) / Pi;
         declare P2Ada_Var_8 : compt renames tcompt;
         begin
            --  nepers/mm
            P2Ada_Var_8.alpha_c := -Rs_at_fd * partial_Z_w * log_term /
              (120.0 * Pi * Z_0_t);
            P2Ada_Var_8.zed := Z_0_t;
         end;
         --  [P2Ada]: end of WITH
      end if;
      declare P2Ada_Var_9 : compt_record renames tcompt.all;
      begin
         --  nepers/mm
         P2Ada_Var_9.super := True;
         P2Ada_Var_9.alpha_d := Pi * Sqrt (er) * loss_tangent / Lambda_fd;
      end;
      --  [P2Ada]: end of WITH
   end super_stripline;

   procedure super_microstrip (tcompt : compt) is
      --  *********************************
         u_in, b, t_n, delta_ul, delta_ur, Z_0, e_eff, A_fac, b_t_W, ul, ur,
         Z_0_t : Long_Float;

      function a (u : Long_Float) return Long_Float is
         Result_a : Long_Float;
         a_int : Long_Float;
      begin
            a_int := 1.0 + Log ((Exp (4.0 * Log (u)) + ((u / 52.0) ** 2.0)) /
                                (Exp (4.0 * Log (u)) + 0.432)) / 49.0;
         Result_a := a_int + Log (1.0 + Exp (3.0 * Log (u / 18.1))) / 18.7;
         return Result_a;
      end a;
      --  *********************************

      function e_e (u : Long_Float) return Long_Float is
         Result_e_e : Long_Float;
      begin
         Result_e_e := (er + 1.0) / 2.0 + ((er - 1.0) / 2.0) *
           Exp (-1.0 * a (u) * b * Log (1.0 + 10.0 / u));
         return Result_e_e;
      end e_e;
      --  *********************************
   begin
      u_in := tcompt.all.width / substrate_h;
      b := 0.564 * Exp (0.053 * Log ((er - 0.9) / (er + 3.0)));
      t_n := metal_thickness / substrate_h;
      if t_n > 0.0
      then
         delta_ul := (t_n / Pi) *
           Log (1.0 + (4.0 * Exp (1.0)) / (t_n * ((Cosh (Sqrt (6.517 * u_in)) /
                Sinh (Sqrt (6.517 * u_in))) ** 2)));
         delta_ur := 0.5 * (1.0 + 1.0 / (Cosh (Sqrt (er - 1.0)))) * delta_ul;
         ul := u_in + delta_ul;
         ur := u_in + delta_ur;
      else
         ul := u_in;
         ur := u_in;
      end if;
      Z_0 := Hammerstad_Z (ur) / Sqrt (e_e (ur));
      e_eff := e_e (ur) * ((Hammerstad_Z (ul) / Hammerstad_Z (ur)) ** 2);
      b_t_W := 2.0 / u_in;
         A_fac := Log (1.0 + (4.0 * b_t_W / Pi) * ((8.0 * b_t_W / Pi) +
                         Sqrt (((8.0 * b_t_W / Pi) ** 2) + 6.27)));
      Z_0_t := 60.0 * A_fac / Sqrt (er);
      declare P2Ada_Var_7 : compt_record renames tcompt.all;
      begin
         P2Ada_Var_7.alpha_c := ms_alpha_c (tcompt.all.width, Z_0, e_eff);
         P2Ada_Var_7.alpha_d := ms_alpha_d (e_eff);
         P2Ada_Var_7.zed := Z_0;
         P2Ada_Var_7.zed_e0 := Z_0;
         P2Ada_Var_7.zed_S_e0 := Z_0_t;
         P2Ada_Var_7.e_eff_e0 := e_eff;
         P2Ada_Var_7.wavelength := P2Ada_Var_7.lngth0 *
           Sqrt (e_eff) / Lambda_fd;
         P2Ada_Var_7.super := True;
      end;
      --  [P2Ada]: end of WITH
   end super_microstrip;

   procedure ms_dispersion (tcompt : compt) is
      --  F4 = f*h in units of GHz-cm (freq normalized to board thickness
      u_in, P, P1, P2, P3, P4, F4, ere0, ere_f : Long_Float;
   begin
      F4 := substrate_h * freq *
        Eng_Prefix (Character (freq_prefix)) / 1.0e+10;
      if F4 > 25.0
      then
         declare P2Ada_Var_1 : compt_record renames tcompt.all;
         begin
            P2Ada_Var_1.zed := P2Ada_Var_1.zed_S_e0;
            P2Ada_Var_1.wavelength := P2Ada_Var_1.lngth0 *
              Sqrt (er) / Lambda_fd;
         end;
         --  [P2Ada]: end of WITH
      else
         if F4 > 0.0
         then
            u_in := tcompt.all.width / substrate_h;
            ere0 := tcompt.all.e_eff_e0;
            P1 := 0.27488 + (0.6315 + 0.525 / Exp (20.0 *
                               Log (1.0 + 0.157 * F4))) * u_in - 0.065683 *
                               Exp (-8.7513 * u_in);
            P2 := 0.33622 * (1.0 - Exp (-0.03442 * er));
            P3 := 0.0363 * Exp (-4.6 * u_in) * (1.0 - Exp (-1.0 *
                               Exp (4.97 * Log (F4 / 3.87))));
            P4 := 1.0 + 2.751 * (1.0 - Exp (-1.0 *
                               Exp (8.0 * Log (er / 15.916))));
            P := P1 * P2 * Exp (1.5763 * Log ((0.1844 + P3 * P4) * 10.0 * F4));
            ere_f := Disperse_f (er, ere0, P);
            declare P2Ada_Var_2 : compt_record renames tcompt.all;
            begin
               P2Ada_Var_2.zed := Disperse_f (P2Ada_Var_2.zed_S_e0,
                                              P2Ada_Var_2.zed_e0, P);
               P2Ada_Var_2.wavelength := P2Ada_Var_2.lngth0 *
                 Sqrt (ere_f) / Lambda_fd;
            end;
         else
            declare P2Ada_Var_3 : compt_record renames tcompt.all;
            begin
               --  reset dc values if a re-sweep w/o parsing
               P2Ada_Var_3.zed := P2Ada_Var_3.zed_e0;
               P2Ada_Var_3.wavelength := P2Ada_Var_3.lngth0 *
                 Sqrt (P2Ada_Var_3.e_eff_e0) / Lambda_fd;
            end;
            --  [P2Ada]: end of WITH
         end if;
      end if;
   end ms_dispersion;

   procedure super_cl_stripline (tcompt : compt;
                                 widthc, spacec : Long_Float) is
      W, S, b, t, theta_fac, C_f, A_e, A_o, Z_oe, Z_oo, C_ff,
                    A_eo, Rs_prop, A_c_int, A_co_int : Long_Float;
   begin
      if metal_thickness > 0.0
      then
         --  * Z calculations -- do not function for t=0 *
         --  * alpha calculations  :  units are nepers/mm  *
         --  break this up to prevent stack overflow
         --  [P2Ada]: WITH instruction
         --  [P2Ada]: !Help! No type found -> add 'P2Ada_Var_10.' to fields
         W := widthc * substrate_h;
         S := spacec * substrate_h;
         b := substrate_h;
         t := metal_thickness;
         theta_fac := Pi * S / (2.0 * b);
         C_f := 2.0 * Log ((2.0 * b - t) / (b - t)) - (t / b) *
           Log (t * (2.0 * b - t) / ((b - t) ** 2));
         A_e := 1.0 + Log (1.0 + Sinh (theta_fac) /
           Cosh (theta_fac)) / Log (2.0);
         A_o := 1.0 + Log (1.0 + Cosh (theta_fac) / Sinh (theta_fac)) /
           Log (2.0);
         Z_oe := 30.0 * Pi * (b - t) / (Sqrt (er) * (W + b * C_f * A_e /
                                        (2.0 * Pi)));
         Z_oo := 30.0 * Pi * (b - t) / (Sqrt (er) * (W + b * C_f * A_o /
                                        (2.0 * Pi)));
         C_ff := Log ((2.0 * b - t) / (b - t)) + 0.5 * Log (t * (2.0 * b - t) /
                                                            ((b - t) ** 2));
         A_eo := C_f * (-1.0 + S / b) / (4.0 * Log (2.0) * (Sinh (theta_fac) +
                                            Cosh (theta_fac)));
         Rs_prop := Rs_at_fd * Sqrt (er) / (3600.0 * ((Pi) ** 2) * (b - t));
         A_c_int := 1.0 - (A_e * C_ff / Pi) + (A_eo / Cosh (theta_fac));
         A_co_int := 1.0 - (A_o * C_ff / Pi) - (A_eo / Sinh (theta_fac));
         declare P2Ada_Var_10 : compt_record renames tcompt.all;
         begin
            P2Ada_Var_10.zed := Z_oe;
            P2Ada_Var_10.zedo := Z_oo;
            P2Ada_Var_10.alpha_c := Rs_prop *
              (60.0 * Pi + Z_oe * Sqrt (er) * A_c_int);
            P2Ada_Var_10.alpha_co := Rs_prop *
              (60.0 * Pi + Z_oo * Sqrt (er) * A_co_int);
         end;
      end if;
      declare P2Ada_Var_11 : compt_record renames tcompt.all;
      begin
         P2Ada_Var_11.super := True;
         P2Ada_Var_11.alpha_d := Pi * Sqrt (er) * loss_tangent / Lambda_fd;
         P2Ada_Var_11.alpha_do := P2Ada_Var_11.alpha_d;
      end;
      --  [P2Ada]: end of WITH
   end super_cl_stripline;

   procedure super_cl_microstrip (tcompt : compt;
                                  widthc, spacec : Long_Float) is
      W, S, g, W_over_h, B_fac, delta_W, delta_t, e_eff0, u, a, b, v, b_o, c_o,
      d_o, e_effe0, e_effo0, Z_L_0, Q1, Q2, Q3, Q4, Q5, Q6, Q7, Q8, Q9, Q10,
      Z_Le_0, Z_Lo_0, theta_fac, C_f, A_o, A_e, Z_oo, Z_oe,
      K_loss : Long_Float;
   begin
      W_over_h := widthc;
      g := spacec;
      S := spacec * substrate_h;
      W := widthc * substrate_h;
      if metal_thickness > 0.0
      then
         if W_over_h < 0.1592
         then
            B_fac := 2.0 * Pi * W;
         else
            B_fac := substrate_h;
         end if;
         --  [P2Ada]: WITH instruction
         --  [P2Ada]: !Help! No type found -> add 'P2Ada_Var_12.' to fields
         delta_W := metal_thickness * (1.0 + Log (2.0 * B_fac /
                                         metal_thickness)) / Pi;
         delta_t := metal_thickness / (er * g);
         declare P2Ada_Var_12 : compt_record renames tcompt.all;
         begin
            P2Ada_Var_12.u_even := (W + delta_W * (1.0 - 0.5 *
                         Exp (-0.69 * delta_W / delta_t))) / substrate_h;
            P2Ada_Var_12.u_odd := P2Ada_Var_12.u_even + delta_t / substrate_h;
            P2Ada_Var_12.g_fac := g;
         end;
      else
         declare P2Ada_Var_13 : compt_record renames tcompt.all;
         begin
            P2Ada_Var_13.u_even := W_over_h;
            P2Ada_Var_13.u_odd := P2Ada_Var_13.u_even;
            P2Ada_Var_13.g_fac := g;
         end;
         --  [P2Ada]: end of WITH
      end if;
      u := W_over_h;
      a := 1.0 + Log ((Exp (4.0 * Log (u)) + ((u / 52.0) ** 2)) /
                      (Exp (4.0 * Log (u)) + 0.432)) / 49.0;
      a := a + Log (1.0 + Exp (3.0 * Log (u / 18.1))) / 18.7;
      b := 0.564 * Exp (0.053 * Log ((er - 0.9) / (er + 3.0)));
      e_eff0 := (er + 1.0) / 2.0 + ((er - 1.0) / 2.0) *
        Exp (-a * b * Log (1.0 + 10.0 / u));
      u := tcompt.all.u_even;
      v := u * (20.0 + ((g) ** 2)) / (10.0 + ((g) ** 2)) + g * Exp (-g);
      a := 1.0 + (Log ((Exp (4.0 * Log (v)) + ((v / 52.0) ** 2)) /
        (Exp (4.0 * Log (v)) + 0.432)) / 49.0);
      a := a + (Log (1.0 + Exp (3.0 * Log (v / 18.1))) / 18.7);
      e_effe0 := 0.5 * (er + 1.0) + 0.5 * (er - 1.0) *
        Exp (-a * b * Log (1.0 + 10.0 / v));
      u := tcompt.all.u_odd;
      d_o := 0.593 + 0.694 * Exp (-0.562 * u);
      b_o := 0.747 * er / (0.15 + er);
      c_o := b_o - (b_o - 0.207) * Exp (-0.414 * u);
      a := 0.7287 * (e_eff0 - 0.5 * (er + 1.0)) * (1.0 - Exp (-0.179 * u));
      e_effo0 := (0.5 * (er + 1.0) + a - e_eff0) *
        Exp (-c_o * Exp (d_o * Log (g))) + e_eff0;
      u := tcompt.all.u_even;
      Z_L_0 := Hammerstad_Z (u) / Sqrt (e_eff0);
      Q1 := 0.8695 * Exp (0.194 * Log (u));
      Q2 := 1.0 + (0.7519 * g) + (0.189 * Exp (2.31 * Log (g)));
      Q3 := 0.1975 + Exp (-0.387 * Log (16.6 + Exp (6.0 * Log (8.4 / g))));
      Q3 := Q3 + (Log (Exp (10.0 * Log (g)) / (1.0 + Exp (10.0 *
                    Log (g / 3.4)))) / 241.0);
      Q4 := (2.0 * Q1 / Q2) * 1.0 / (Exp (-g) * Exp (Q3 * Log (u)) + (2.0 -
                                     Exp (-g)) * Exp (-Q3 * Log (u)));
      Z_Le_0 := Z_L_0 * Sqrt (e_eff0 / e_effe0) /
        (1.0 - (Z_L_0 / (120.0 * Pi)) * Sqrt (e_eff0) * Q4);
      u := tcompt.all.u_odd;
      Z_L_0 := Hammerstad_Z (u) / Sqrt (e_eff0);
      Q5 := 1.794 + 1.14 * Log (1.0 + 0.638 / (g + 0.517 *
                                  Exp (2.43 * Log (g))));
      Q6 := 0.2305 + Log (
                          Exp (10.0 * Log (g)) / (1.0 +
                          Exp (10.0 * Log (g / 5.8)))) / 281.3 +
                          Log (1.0 + 0.598 * Exp (1.154 * Log (g))) / 5.1;
      Q7 := (10.0 + 190.0 * ((g) ** 2)) / (1.0 + 82.3 * Exp (3.0 * Log (g)));
      Q8 := -6.5 - 0.95 * Log (g) - Exp (5.0 * Log (g / 0.15));
      if Q8 < -50.0
      then
         Q8 := 0.0;
      else
         Q8 := Exp (Q8);
      end if;
      Q9 := Log (Q7) * (Q8 + 1.00 / 16.5);
      Q10 := (Q2 * Q4 - Q5 * Exp (Log (u) * Q6 * Exp (-Q9 * Log (u)))) / Q2;
      Z_Lo_0 := Z_L_0 * Sqrt (e_eff0 / e_effo0) /
        (1.0 - (Z_L_0 / (120.0 * Pi)) * Sqrt (e_eff0) * Q10);
      b := 2.0 * substrate_h;
      theta_fac := Pi * S / (2.0 * b);
      C_f := 2.0 * Log (2.0);
      A_e := 1.0 + Log (1.0 + Sinh (theta_fac) / Cosh (theta_fac)) / Log (2.0);
      A_o := 1.0 + Log (1.0 + Cosh (theta_fac) / Sinh (theta_fac)) / Log (2.0);
      Z_oe := 60.0 * Pi * b / (Sqrt (er) * (W + b * C_f * A_e / (2.0 * Pi)));
      Z_oo := 60.0 * Pi * b / (Sqrt (er) * (W + b * C_f * A_o / (2.0 * Pi)));
      K_loss := Exp (-1.2 * Exp (0.7 * Log ((Z_Lo_0 + Z_Le_0) /
                     (240.0 * Pi))));
      declare P2Ada_Var_14 : compt_record renames tcompt.all;
      begin
         P2Ada_Var_14.alpha_c := Rs_at_fd * K_loss /
           (Z_Le_0 * P2Ada_Var_14.u_even * substrate_h);
         P2Ada_Var_14.alpha_co := Rs_at_fd * K_loss /
           (Z_Lo_0 * P2Ada_Var_14.u_odd * substrate_h);
         P2Ada_Var_14.alpha_d := ms_alpha_d (e_effe0);
         P2Ada_Var_14.alpha_do := ms_alpha_d (e_effo0);
         P2Ada_Var_14.zed := Z_Le_0;
         P2Ada_Var_14.zed_e0 := Z_Le_0;
         P2Ada_Var_14.zedo := Z_Lo_0;
         P2Ada_Var_14.zed_o0 := Z_Lo_0;
         P2Ada_Var_14.zed_S_e0 := Z_oe;
         P2Ada_Var_14.zed_S_o0 := Z_oo;
         P2Ada_Var_14.e_eff_e0 := e_effe0;
         P2Ada_Var_14.e_eff_o0 := e_effo0;
         P2Ada_Var_14.super := True;
      end;
      --  [P2Ada]: end of WITH
   end super_cl_microstrip;

   procedure ms_cl_dispersion (tcompt : compt) is
      --  f_n = f*h in units of GHz-mm (freq normalized to board thickness)
      u, g, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15,
      f_n, F_o_f, F_e_f, ere_e_f, ere_o_f : Long_Float;
   begin
      f_n := substrate_h * freq *
        Eng_Prefix (Character (freq_prefix)) / 1.0e+9;
      if f_n > 225.0
      then
         declare P2Ada_Var_4 : compt_record renames tcompt.all;
         begin
            --  lngth0 = lngth in mm
            P2Ada_Var_4.zed := P2Ada_Var_4.zed_S_e0;
            P2Ada_Var_4.zedo := P2Ada_Var_4.zed_S_o0;
            P2Ada_Var_4.wavelength := P2Ada_Var_4.lngth0 *
              Sqrt (er) / Lambda_fd;
            P2Ada_Var_4.wavelengtho := P2Ada_Var_4.wavelength;
         end;
         --  [P2Ada]: end of WITH
      else
         if f_n > 0.005
         then
            --  ! Numerically unstable and no affect at small f_n
            --  even mode calculation
            --  odd mode calculation - uses P1,P2,P3,P4
            --  P1 and P3 must be recalculated with new u, P2,P4 are OK
            --  [P2Ada]: WITH instruction
            --  [P2Ada]: !Help! No type found -> add 'P2Ada_Var_5.' to fields
            u := tcompt.all.u_even;
            g := tcompt.all.g_fac;
            P1 := 0.27488 + (0.6315 + 0.525 / Exp (20.0 *
                               Log (1.0 + 0.0157 * f_n))) *
              u - 0.065683 * Exp (-8.7513 * u);
            P2 := 0.33622 * (1.0 - Exp (-0.03442 * er));
            P3 := 0.0363 * Exp (-4.6 * u) *
              (1.0 - Exp (-1.0 * Exp (4.97 * Log (f_n / 38.7))));
            P4 := 1.0 + 2.751 * (1.0 - Exp (-1.0 * Exp (8.0 *
                                   Log (er / 15.916))));
            P5 := 0.334 * Exp (-3.3 * Exp (3.0 * Log (er / 15.0))) + 0.746;
            P6 := P5 * Exp (-1.0 * Exp (0.368 * Log (f_n / 18.0)));
            P7 := 1.0 + 4.069 * P6 * Exp (0.479 * Log (g)) *
              Exp (-1.347 * Exp (0.595 * Log (g)) - 0.17 *
                     Exp (2.5 * Log (g)));
            F_e_f := P1 * P2 * Exp (1.5763 *
                                      Log ((P3 * P4 + 0.1844 * P7) * f_n));
            ere_e_f := Disperse_f (er, tcompt.all.e_eff_e0, F_e_f);
            u := tcompt.all.u_odd;
            P1 := 0.27488 + (0.6315 + 0.525 / Exp (20.0 *
                               Log (1.0 + 0.0157 * f_n))) * u - 0.065683 *
              Exp (-8.7513 * u);
            P3 := 0.0363 * Exp (-4.6 * u) * (1.0 - Exp (-1.0 *
                                               Exp (4.97 * Log (f_n / 38.7))));
            P8 := 0.7168 * (1.0 + 1.076 / (1.0 + 0.0576 * (er - 1.0)));
            P9 := P8 - 0.7913 * (1.0 - Exp (-1.0 * Exp (1.424 *
                                   Log (f_n / 20.0)))) *
              Arctan (2.481 * Exp (0.946 * Log (er / 8.0)));
            P10 := 0.242 * Exp (0.55 * Log (er - 1.0));
            P11 := 0.6366 * (Exp (-0.3401 * f_n) - 1.0) *
              Arctan (1.263 * Exp (1.629 * Log (u / 3.0)));
            P12 := P9 + (1.0 - P9) / (1.0 + 1.183 * Exp (1.376 * Log (u)));
            P13 := 1.695 * P10 / (0.414 + 1.605 * P10);
            P14 := 0.8928 + 0.1072 * (1.0 - Exp (-0.42 * Exp (3.215 *
                                        Log (f_n / 20.0))));
            P15 := abs (1.0 - 0.8928 * (1.0 + P11) * P12 *
                          Exp (-P13 * Exp (1.092 * Log (g))) / P14);
            F_o_f := P1 * P2 * Exp (1.5763 * Log ((P3 * P4 + 0.1844) *
                                      f_n * P15));
            ere_o_f := Disperse_f (er, tcompt.all.e_eff_o0, F_o_f);
            declare P2Ada_Var_5 : compt_record renames tcompt.all;
            begin
               --  lngth0 = lngth in mm
               P2Ada_Var_5.zed := Disperse_f (P2Ada_Var_5.zed_S_e0,
                                              P2Ada_Var_5.zed_e0, F_e_f);
               P2Ada_Var_5.zedo := Disperse_f (P2Ada_Var_5.zed_S_o0,
                                               P2Ada_Var_5.zed_o0, F_o_f);
               P2Ada_Var_5.wavelength := P2Ada_Var_5.lngth0 *
                 Sqrt (ere_e_f) / Lambda_fd;
               P2Ada_Var_5.wavelengtho := P2Ada_Var_5.lngth0 *
                 Sqrt (ere_o_f) / Lambda_fd;
            end;
         else
            declare P2Ada_Var_6 : compt_record renames tcompt.all;
            begin
               --  lngth0 = lngth in mm
               P2Ada_Var_6.zed := P2Ada_Var_6.zed_e0;
               P2Ada_Var_6.zedo := P2Ada_Var_6.zed_o0;
               P2Ada_Var_6.wavelength := P2Ada_Var_6.lngth0 *
                 Sqrt (P2Ada_Var_6.e_eff_e0) / Lambda_fd;
               P2Ada_Var_6.wavelengtho := P2Ada_Var_6.lngth0 *
                 Sqrt (P2Ada_Var_6.e_eff_o0) / Lambda_fd;
            end;
            --  [P2Ada]: end of WITH
         end if;
      end if;
   end ms_cl_dispersion;

   function Hammerstad_Z (u : Long_Float) return Long_Float is
      Result_Hammerstad_Z : Long_Float;
      f : Long_Float;
   begin
      f := 6.0 + (2.0 * Pi - 6.0) *
        Exp (-1.0 * Exp (0.7528 * Log (30.666 / u)));
      Result_Hammerstad_Z := 60.0 *
        Log (f / u + Sqrt (1.0 + ((2.0 / u) ** 2)));
      return Result_Hammerstad_Z;
   end Hammerstad_Z;

   function ms_alpha_c (W_in, Z_0_in, er_e : Long_Float) return Long_Float is
      Result_ms_alpha_c : Long_Float;
      W_over_h, We_over_h, A_fac, B_fac, W_h_ratio : Long_Float;
   begin
      if metal_thickness > 0.0
      then
         --  * is W/h < 1/2pi ?*
         W_over_h := W_in / substrate_h;
         if W_over_h < 0.1592
         then
            B_fac := 2.0 * Pi * W_in;
            We_over_h := W_over_h + 1.25 * metal_thickness *
              (1.0 + Log (4.0 * Pi * W_in / metal_thickness)) /
                (Pi * substrate_h);
         else
            B_fac := substrate_h;
            We_over_h := W_over_h + 1.25 * metal_thickness *
              (1.0 + Log (2.0 * substrate_h / metal_thickness)) /
                (Pi * substrate_h);
         end if;
         A_fac := 1.0 + (1.0 + 1.25 * Log (2.0 * B_fac / metal_thickness) /
                           Pi) / We_over_h;
         if W_over_h < 1.0
         then
            W_h_ratio := (32.0 - ((We_over_h) ** 2)) /
              (32.0 + ((We_over_h) ** 2));
            W_h_ratio := W_h_ratio /
              (2.0 * Pi * Z_0_in);
         else
            W_h_ratio := 0.667 * We_over_h / (We_over_h + 1.444) + We_over_h;
            W_h_ratio := W_h_ratio * Z_0_in * er_e / ((120.0 * Pi) ** 2);
         end if;
         --  nepers per mm
         Result_ms_alpha_c := A_fac * W_h_ratio * Rs_at_fd / substrate_h;
      else
         Result_ms_alpha_c := 0.0;
      end if;
      return Result_ms_alpha_c;
   end ms_alpha_c;

   function ms_alpha_d (ere_in : Long_Float) return Long_Float is
      Result_ms_alpha_d : Long_Float;
      er_int : Long_Float;
   begin
      if er /= 1.0
      then
         --  nepers/mm
         er_int := Pi * er * (ere_in - 1.0) / (Sqrt (ere_in) * (er - 1.0));
         Result_ms_alpha_d := er_int * loss_tangent * design_freq *
           Eng_Prefix (Character (freq_prefix)) / c_in_mm;
      else
         Result_ms_alpha_d := Pi * Sqrt (er) * loss_tangent / Lambda_fd;
      end if;
      return Result_ms_alpha_d;
   end ms_alpha_d;

   function Disperse_f (er1, er2, F_eo : Long_Float) return Long_Float is
      Result_Disperse_f : Long_Float;
   begin
      Result_Disperse_f := er1 - (er1 - er2) / (1.0 + F_eo);
      return Result_Disperse_f;
   end Disperse_f;

end pfun2;
