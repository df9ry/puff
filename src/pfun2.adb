
with Ada.Numerics.Long_Elementary_Functions;
use  Ada.Numerics.Long_Elementary_Functions;

with Ada.Strings.Unbounded;
use  Ada.Strings.Unbounded;

with Interfaces;   use Interfaces;
with Interfaces.C; use Interfaces.C;
with Utils;        use Utils;

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
      i, j : Integer;
      c_ss : array (1 .. 2) of rg_pmc;
      rds : TComplex;
      unit1, prefix : Character;
      Manhat_on, alt_param : Boolean;
      zd, ere, value, alpha_tl, beta_l, elength, gamma, width,
      wavelength, elength0 : Long_Float;
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
            elength0 := elength;
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
               New_c (c_ss (i) (j));
            end loop;
         end loop;
         --  **********************************************
         co (c_ss (2) (1).all.c, 2 * zd * ch.r + (((zd) ** 2) + 1.0) * sh.r,
             2 * zd * ch.i + (((zd) ** 2) + 1.0) * sh.i);
         rc (rds, c_ss (2) (1).all.c);
         co (c_ss (2) (1).all.c, (((zd) ** 2) - 1.0) * sh.r,
             (((zd) ** 2) - 1.0) * sh.i);
         prp (c_ss (1) (1).all.c, c_ss (2) (1).all.c, rds);
         co (c_ss (2) (2).all.c, c_ss (1) (1).all.c.r, c_ss (1) (1).all.c.i);
         sm (c_ss (1) (2).all.c, 2 * zd, rds);
         co (c_ss (2) (1).all.c, c_ss (1) (2).all.c.r, c_ss (1) (2).all.c.i);
         c_s := null;
         for j in 1 .. 2
         loop
            for i in 1 .. 2
            loop
               if c_s = null
               then
                  new_s (tcompt.all.s_begin);
                  c_s := tcompt.all.s_begin;
               else
                  new_s (c_s.all.next_s);
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
      i, j, des_lngth : Integer;
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
         declare P2Ada_Var_16 : compt renames tcompt.all;
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
               message (1) := "qline! is";
               message (2) := "an invalid";
               message (3) := "part";
               return;
            end if;
            --  value_str not used here
            Get_Param (tcompt, 1, value, value_str, unit1, prefix, alt_param);
            if bad_compt
            then
               return;
            end if;
            if alt_param
            then
               x_sweep.init_element (tcompt, 'z', prefix, unit1);
            end if;
            if bad_compt
            then
               return;
            end if;
            case unit1 is
               when Omega =>
                  zed := value;
               when 's' | 'S' =>
                  zed := 1.0 / value;
               when 'z' | 'Z' =>
                  zed := Z0 * value;
               when 'y' | 'Y' =>
                  zed := Z0 / value;
               when others =>
                  begin
                     bad_compt := True;
                     message (1) := "Invalid qline";
                     message (2) := "impedance unit";
                     message (3) := "Use y, z, S or " + Omega;
                     return;
                  end;
                  --  else
            end case;
            --  case
            if not (Manhat_on) and then (zed <= 0.0)
            then
               --  OK if alt_param
               bad_compt := True;
               message (1) := "Negative";
               message (2) := "or zero qline";
               message (2) := "impedance";
               return;
            end if;
            --  Get_Param will return 1.0 for zed if alt_param and no number
            --  given
            if (alt_param and then (value_str = "1.0")) or else (zed <= 0.0)
            then
               width := widthZ0;
            else
               width := widtht (zed);
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
                  message (1) := "Impedance too big";
                  message (2) := "qline too narrow";
                  message (3) := "(<" + sresln + ')';
                  return;
               end if;
               if width > bmax
               then
                  bad_compt := True;
                  message (1) := "Impedance is";
                  message (2) := "too small:";
                  message (3) := "qline too wide";
                  return;
               end if;
            end if;
            --  not Manhat_on
            if stripline
            then
               --  ere calculation is a function of width!
               ere := er;
            else
               ere := (er + 1) / 2 + (er - 1) / 2 /
                 sqrt (1 + 10 * substrate_h / width);
            end if;
            --  value_str not used here
            Get_Param (tcompt, 2, value, value_str, unit1, prefix, alt_param);
            if bad_compt
            then
               return;
            end if;
            if alt_param
            then
               x_sweep.init_element (tcompt, 'l', prefix, unit1);
            end if;
            if bad_compt
            then
               return;
            end if;
            case unit1 is
               when degree =>
                  wavelength := value / 360.0;
                  lngth := Lambda_fd * wavelength / sqrt (ere);
               when 'm' =>
                  --  mmlong
                  lngth := value;
                  wavelength := lngth * sqrt (ere) / Lambda_fd;
                  if alt_param
                  then
                     x_sweep.Load_Prop_Const (sqrt (ere) / lambda_fd, 0.0);
                  end if;
               when 'h' | 'H' =>
                  lngth := value * substrate_h;
                  wavelength := lngth * sqrt (ere) / Lambda_fd;
                  if alt_param
                  then
                     x_sweep.Load_Prop_Const (substrate_h * Sqrt (ere) /
                                                Lambda_fd, 0.0);
                  end if;
               when others =>
                  begin
                     bad_compt := True;
                     message (1) := "Invalid qline";
                     message (2) := "length unit";
                     message (3) := "Use mm, h or " + Degree;
                     return;
                  end;
            end case;
            --  case
            --  save lngth factor in mm
            lngth0 := lngth;
            if Manhat_on
            then
               --  Artwork correction and Manh_width after ere calculation
               width := Manh_width;
               lngth := Manh_length;
            else
               width := width + artwork_cor;
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
               if alt_param
               then
                  x_sweep.init_element (tcompt, 'Q', prefix, 'Q');
               end if;
               if bad_compt
               then
                  return;
               end if;
               --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
               if unit1 and (degree | 'm' | 'h' | 'H' => True,
                              others => False)
               then
                  bad_compt := True;
                  message (1) := "Corrections";
                  message (2) := "not allowed for";
                  message (3) := "qline length";
                  return;
               end if;
               --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
               if (value = 0.0) or else not (unit1 and ('q' | 'Q' => True,
                                                        others => False))
               then
                  bad_compt := True;
                  message (1) := "Invalid or zero";
                  message (2) := "Q factor";
                  return;
               end if;
               des_lngth := (descript'Length);
               loop
                  --  Find location of 'Q'
                  j := j + 1;
                  exit when (descript (j) = unit1) or else (j > des_lngth);
               end loop;
               loop
                  --  Skip spaces to
                  --  find 'd' or 'c' after Q
                  j := j + 1;
                  exit when (descript (j) /= ' ') or else (j > des_lngth);
               end loop;
               if lngth0 /= 0.0
               then
                  --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
                  alpha_co := (Pi * wavelength) / (value * lngth0);
                  if (j <= des_lngth) and then (descript (j) and
                                                  ('C' | 'c' => True,
                                                   others => False))
                  then
                     --  default to dielectric loss
                     alpha_c := alpha_co;
                  else
                     alpha_d := alpha_co;
                  end if;
                  --  reset to 0
                  alpha_co := 0.0;
               end if;
            end if;
            --  if j > 0
            con_space := 0.0;
            number_of_con := 2;
         end;
         --  [P2Ada]: end of WITH
         --  with tcompt^ do
         --  if no action
         --  Calculate scattering parameters
         --  Normalized frequency
         --  ! Warning, these alphas get huge for small Q's
      else
         gamma := freq / design_freq;
         elength := 2 * Pi * tcompt.all.wavelength;
         alpha_tl := (tcompt.all.alpha_d * gamma + tcompt.all.alpha_c *
                        Sqrt (gamma)) * tcompt.all.lngth0;
         if alpha_tl > 80.0
         then
            alpha_tl := 80;
         end if;
         beta_l := elength * gamma;
         sh.r := sinh (alpha_tl) * cos (beta_l);
         sh.i := cosh (alpha_tl) * sin (beta_l);
         ch.r := cosh (alpha_tl) * cos (beta_l);
         ch.i := sinh (alpha_tl) * sin (beta_l);
         zd := tcompt.all.zed / z0;
         for i in 1 .. 2
         loop
            for j in 1 .. 2
            loop
               New_c (c_ss (i) (j));
            end loop;
         end loop;
         co (c_ss (1) (1).all.c, 2 * zd * ch.r + (((zd) ** 2) + 1.0) * sh.r,
             2 * zd * ch.i + (((zd) ** 2) + 1.0) * sh.i);
         rc (rds, c_ss (1) (1).all.c);
         co (c_ss (2) (1).all.c, (((zd) ** 2) - 1.0) * sh.r,
             (((zd) ** 2) - 1.0) * sh.i);
         prp (c_ss (1) (1).all.c, c_ss (2) (1).all.c, rds);
         co (c_ss (2) (2).all.c, c_ss (1) (1).all.c.r, c_ss (1) (1).all.c.i);
         sm (c_ss (1) (2).all.c, 2 * zd, rds);
         co (c_ss (2) (1).all.c, c_ss (1) (2).all.c.r, c_ss (1) (2).all.c.i);
         c_s := null;
         for j in 1 .. 2
         loop
            for i in 1 .. 2
            loop
               if c_s = null
               then
                  new_s (tcompt.all.s_begin);
                  c_s := tcompt.all.s_begin;
               else
                  new_s (c_s.all.next_s);
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
      value_str : line_string;
   begin
      if action
      then
         --  [P2Ada]: WITH instruction
         --  [P2Ada]: !Help! No type found -> add 'P2Ada_Var_17.' to fields
         declare P2Ada_Var_17 : compt renames tcompt.all;
         begin
            --  * an 'M' at the end of ^.descript? *
            Manhat_on := Manhattan (tcompt);
            if bad_compt
            then
               return;
            end if;
            if (Pos ('?', descript) > 0) and then super_line (tcompt)
            then
               bad_compt := True;
               message (1) := "Combined";
               message (2) := "? and !";
               message (3) := "disallowed";
               return;
            end if;
            --  default attenuation factors nepers/mm
            alpha_c := 0.0;
            alpha_d := 0.0;
            alpha_co := 0.0;
            alpha_do := 0.0;
            super := false;
            number_of_con := 4;
            z_index := 0;
            zed := 0;
            zedo := 0;
            wavelength := 0;
            lngth := 0;
            check_correction_three := False;
            for i in 1 .. 3
            loop
               if (lngth = 0) and then (wavelength = 0)
               then
                  j := goto_numeral (i, tcompt.all.descript);
                  bad_compt := False;
                  if (j > 0) or else (i < 3)
                  then
                     --  value_str and prefix not used here
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
                        if unit1 and (omega | 's' | 'S' | 'z' | 'Z' | 'y' |
                                      'Y' => True, others => False)
                        then
                           if zed = 0
                           then
                              --  variable is the 1st z
                              --  variable is the 2nd z
                              x_sweep.init_element (tcompt, 'z', prefix,
                                                    unit1);
                              z_index := 1;
                           else
                              x_sweep.init_element (tcompt, 'z', prefix,
                                                    unit1);
                              z_index := 2;
                           end if;
                           --  [P2Ada]: "x in y" -> "x and y" redefine "and"
                           --  before
                        else
                           if unit1 and (degree | 'm' | 'h' | 'H' => True,
                                          others => False)
                           then
                              --  if length units
                              --  variable is length
                              x_sweep.init_element (tcompt, 'l', prefix,
                                                    unit1);
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
                        when omega =>
                           if zed = 0
                           then
                              zed := value;
                           else
                              zedo := value;
                           end if;
                        when 's' | 'S' =>
                           if zed = 0
                           then
                              zed := 1.0 / value;
                           else
                              zedo := 1.0 / value;
                           end if;
                        when 'z' | 'Z' =>
                           if zed = 0
                           then
                              zed := Z0 * value;
                           else
                              zedo := Z0 * value;
                           end if;
                        when 'y' | 'Y' =>
                           if zed = 0
                           then
                              zed := Z0 / value;
                           else
                              zedo := Z0 / value;
                           end if;
                        when degree =>
                           wavelength := value / 360.0;
                        when 'm' =>
                           --  mmlong
                           lngth := value;
                           if z_index = 5
                           then
                              z_index := 6;
                           end if;
                           --  var in meters
                        when 'h' | 'H' =>
                           --  mmlong
                           lngth := value * substrate_h;
                           if z_index = 5
                           then
                              z_index := 7;
                           end if;
                           --  var in h
                        when others =>
                           begin
                              bad_compt := True;
                              message (1) := "Missing clines";
                              message (2) := "unit. Use y, z";
                              message (3) := "S, " + Omega + ", mm, h or " +
                                Degree;
                              return;
                           end;
                     end case;
                     --  case
                  end if;
                  --  (j > 0) or (i < 3)
                  if (i = 2) and then ((wavelength /= 0) or else (lngth /= 0))
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
            if (zed = 0) and then (zedo = 0)
            then
               --  if zed=0 then zed:=sqr(Z0)/zedo;
               bad_compt := True;
               message (1) := "Both cline even";
               message (2) := "& odd impedances";
               message (3) := "not found or zero";
               return;
            else
               if zedo = 0
               then
                  --  calculate zedo if blank
                  zedo := ((Z0) ** 2) / zed;
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
            if zed < zedo
            then
               --  swap even and odd impedances
               zt := zed;
               zed := zedo;
               zedo := zt;
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
            if (zed <= 0) or else (zedo <= 0)
            then
               bad_compt := True;
               message (1) := "cline";
               message (2) := "impedances must";
               message (3) := "be positive";
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
            if zed * 0.97 < zedo
            then
               --  !* this factor was 0.98 in 1.0 *
               --  !* changed to avoid w_s_micro errors*
               bad_compt := True;
               message (1) := "cline even & odd";
               message (2) := "impedances are";
               message (3) := "too close";
               return;
            end if;
            if not (bad_compt)
            then
               if stripline
               then
                  w_s_stripline_cline (zed, zedo, widthc, spacec);
               else
                  wide := widtht (zed / 2.0) / substrate_h;
                  wido := widtht (zedo / 2.0) / substrate_h;
                  if bad_compt
                  then
                     return;
                  end if;
                  w_s_microstrip_cline (wide, wido, widthc, spacec);
               end if;
               if not (bad_compt)
               then
                  --  ! * Artwork correction added here ^ *
                  width := widthc * substrate_h + artwork_cor;
                  con_space := (widthc + spacec) * substrate_h;
                  if (con_space < resln) and then not (Manhat_on)
                  then
                     bad_compt := True;
                     message (1) := "clines spacing is";
                     message (2) := '<' + sresln;
                     message (3) := Omega + "e/" + Omega + "o is too big";
                     return;
                  end if;
                  if (width < resln) and then not (Manhat_on)
                  then
                     bad_compt := True;
                     message (1) := "Even impedance is";
                     message (2) := "too large. Width";
                     message (3) := '<' + sresln;
                     return;
                  end if;
                  if stripline
                  then
                     ereo := er;
                     eree := er;
                     ere := er;
                  else
                     ere_even_odd (widthc, spacec, eree, ereo);
                     ere := 4 * eree * ereo / ((sqrt (eree) +
                                                 sqrt (ereo)) ** 2);
                  end if;
                  if (lngth = 0) and then (wavelength = 0)
                  then
                     bad_compt := True;
                     message (1) := "Missing cline";
                     message (2) := "length. Supply";
                     message (3) := "length in mm or " + Degree;
                     return;
                  else
                     if lngth = 0
                     then
                        lngth := Lambda_fd * wavelength / sqrt (ere);
                     end if;
                     --  used in cl_dispersion
                     wavelength := lngth * sqrt (eree) / Lambda_fd;
                     wavelengtho := lngth * sqrt (ereo) / Lambda_fd;
                     lngth0 := lngth;
                     case z_index is
                        when 5 =>
                           x_sweep.Load_Prop_Const (Sqrt (eree) /
                                                      sqrt (ere), sqrt (ereo) /
                                                      Sqrt (ere));
                        when 6 =>
                           x_sweep.Load_Prop_Const (Sqrt (eree) /
                                                      lambda_fd, sqrt (ereo) /
                                                      Lambda_fd);
                        when 7 =>
                           x_sweep.Load_Prop_Const (substrate_h *
                                                      sqrt (eree) / lambda_fd,
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
                           when Degree =>
                              lngth := lngth + Lambda_fd * (value / 360.0) /
                                sqrt (ere);
                           when 'h' | 'H' =>
                              lngth := lngth + value * substrate_h;
                           when 'm' =>
                              lngth := lngth + value;
                           when others =>
                              begin
                                 bad_compt := True;
                                 message (1) := "Improper units";
                                 message (2) := "used in length";
                                 message (3) := " correction   ";
                                 return;
                              end;
                        end case;
                        --  case
                     end if;
                     --  if j > 0
                  end if;
                  --  else lngth=wavelength=0
                  if (lngth < 0) and then not (Manhat_on)
                  then
                     bad_compt := True;
                     message (1) := "Negative";
                     message (2) := "cline length";
                     return;
                  end if;
               end if;
               --  if not_bad
            end if;
            --  if not_bad
            if bad_compt and then (z_index > 0)
            then
               message (1) := "Add or alter";
               message (2) := "best-guess";
               message (3) := "values";
            end if;
            if Manhat_on
            then
               --  * if Manhatton then fix dimensions *
               width := Manh_width;
               con_space := Manh_length;
               lngth := Manh_length;
            end if;
            --  * if Manhatton *
         end;
         --  [P2Ada]: end of WITH
         --  with
         --  if action true
         --  if no action then compute s-parameters
         --  normalized
         --  Re-compute Zo and wavelength when dispersive microstrip
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
               elength := 2 * Pi * tcompt.all.wavelength;
               alpha_tl := (tcompt.all.alpha_d * gamma +
                              Rough_alpha (tcompt.all.alpha_c) * sqrt (gamma))
                 * tcompt.all.lngth0;
            else
               zd := tcompt.all.zedo / Z0;
               elength := 2 * Pi * tcompt.all.wavelengtho;
               alpha_tl := (tcompt.all.alpha_do * gamma +
                              Rough_alpha (tcompt.all.alpha_co) * sqrt (gamma))
                 * tcompt.all.lngth0;
            end if;
            beta_l := elength * gamma;
            sh.r := sinh (alpha_tl) * cos (beta_l);
            sh.i := cosh (alpha_tl) * sin (beta_l);
            ch.r := cosh (alpha_tl) * cos (beta_l);
            ch.i := sinh (alpha_tl) * sin (beta_l);
            co (seo (i) (1) (1), 2 * zd * ch.r + (((zd) ** 2) + 1.0) *
                  sh.r, 2 * zd * ch.i + (((zd) ** 2) + 1.0) * sh.i);
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
                  new_s (tcompt.all.s_begin);
                  c_s := tcompt.all.s_begin;
               else
                  new_s (c_s.all.next_s);
                  c_s := c_s.all.next_s;
               end if;
               c_s.all.next_s := null;
               New_c (c_s.all.z);
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
         declare P2Ada_Var_18 : compt renames tcompt.all;
         begin
            number_of_con := 2;
            con_space := 0.0;
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
                  if value (i) /= 0
                  then
                     j := j + 1;
                  end if;
               end loop;
               if j /= 1
               then
                  bad_compt := True;
                  message (1) := "One parameter";
                  message (2) := "only for";
                  message (3) := "swept lumped";
               else
                  if parallel_cir
                  then
                     --  j=1, value[1..3]
                     --  index of non-zero value
                     bad_compt := True;
                     message (1) := "Parallel circuit";
                     message (2) := "not allowed for";
                     message (3) := "swept lumped";
                  else
                     i := 0;
                     loop
                        i := i + 1;
                        exit when value (i) /= 0;
                     end loop;
                     --  * value[i] is now the alt_parm *
                     --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
                     if i and (1 .. 3 => True, others => False)
                     then
                        x_sweep.init_element (tcompt, ident, prefix, unit1);
                     end if;
                     x_sweep.Load_Index (i);
                  end if;
               end if;
            end if;
            --  if alt_param
            if bad_compt
            then
               return;
            end if;
            if Manhattan (tcompt) or else (value (4) = 0)
            then
               --  * Select Manhattan layout *
               width := Manh_width;
               lngth := 2.0 * Manh_width;
            else
               lngth := value (4);
               width := 0;
            end if;
            case unit1 is
               when omega =>
                  zed := value (1) / Z0;
                  zedo := value (2) / Z0;
                  wavelength := value (3) / Z0;
                  spec_freq := 1;
               when 'z' | 'Z' =>
                  zed := value (1);
                  zedo := value (2);
                  wavelength := value (3);
                  spec_freq := 1;
               when 's' | 'S' =>
                  zed := value (1) * Z0;
                  zedo := value (2) * Z0;
                  wavelength := value (3) * Z0;
                  spec_freq := -1;
               when 'y' | 'Y' =>
                  zed := value (1);
                  zedo := value (2);
                  wavelength := value (3);
                  spec_freq := -1;
               when others =>
                  --  [P2Ada]: no otherwise / else in Pascal
                  null;
            end case;
            --  case
            if zed < 0
            then
               zed := zed * one;
            end if;
            if lngth <= resln
            then
               bad_compt := True;
               message (1) := "lumped length";
               message (2) := "must be in m";
               message (3) := '>' + sresln;
            end if;
         end;
         --  [P2Ada]: end of WITH
         --  with
         --  normalized
         --  [P2Ada]: WITH instruction
         --  [P2Ada]: !Help! No type found -> add 'P2Ada_Var_19.' to fields
      else
         ff := freq / design_freq;
         declare P2Ada_Var_19 : compt renames tcompt.all;
         begin
            if spec_freq > 0
            then
               --  z ,zedo=Ind wavlength=Cap
               if freq = 0
               then
                  if wavelength = 0
                  then
                     co (s21, 1 / (1 + zed / 2), 0);
                  else
                     co (s21, 0.0, 0.0);
                  end if;
               else
                  zi := (zedo * ff + wavelength / ff) / 2;
                  co (zo2, 1 + zed / 2, zi);
                  rc (s21, zo2);
               end if;
               --  y
               s11.r := 1 - s21.r;
               s11.i := -s21.i;
            else
               if freq = 0
               then
                  if wavelength = 0
                  then
                     co (s11, 1 / (1 + zed * 2), 0);
                  else
                     co (s11, 0.0, 0.0);
                  end if;
               else
                  zi := 2 * (zedo * ff + wavelength / ff);
                  co (yb2, 1 + 2 * zed, zi);
                  rc (s11, yb2);
               end if;
               s21.r := 1 - s11.r;
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
                  new_s (tcompt.all.s_begin);
                  c_s := tcompt.all.s_begin;
               else
                  new_s (c_s.all.next_s);
                  c_s := c_s.all.next_s;
               end if;
               c_s.all.next_s := null;
               New_c (c_s.all.z);
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
      i : Integer;
      unit1, prefix : Character;
      value_string : line_string;
      alt_param : Boolean;
   begin
      if action
      then
         --  [P2Ada]: WITH instruction
         --  [P2Ada]: !Help! No type found -> add 'P2Ada_Var_20.' to fields
         declare P2Ada_Var_20 : compt renames tcompt.all;
         begin
            --  ! t_ratio has prefixes factored in - must catch as an error
            --  if ':' present it will return as unit1
            number_of_con := 2;
            con_space := 0.0;
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
               message (1) := "No prefixes";
               message (2) := "allowed for";
               message (3) := "transformer";
               return;
            end if;
            if alt_param
            then
               x_sweep.init_element (tcompt, 't', prefix, unit1);
            end if;
            if bad_compt
            then
               return;
            end if;
            --  * Use Manhattan dimensions    *
            --  ***Negative values OK, just imply a 180 phase shift***
            --  if turns_ratio < 0 then turns_ratio:=Abs(turns_ratio);
            --  {take absolute value if negative}
            --  ****
            --  Pass turns ratio to tcompt^.zed
            width := Manh_length;
            lngth := Manh_length;
            zed := turns_ratio;
         end;
         --  [P2Ada]: end of WITH
         --  with
         --  action
         --  * Fill frequency independent scattering parameters *
         --  * Turns ratio given in tcompt^.zed *
         --  denominator
         --  S11=-S22
         --  S12= S21
         --  S11
      else
         denom := ((tcompt.all.zed) ** 2) + 1.0;
         S11 := (denom - 2.0) / denom;
         S21 := 2.0 * tcompt.all.zed / denom;
         new_s (tcompt.all.s_begin);
         c_s := tcompt.all.s_begin;
         New_c (c_s.all.z);
         co (c_s.all.z.all.c, S11, 0);
         for i in 2 .. 4
         loop
            new_s (c_s.all.next_s);
            c_s := c_s.all.next_s;
            New_c (c_s.all.z);
            if i = 4
            then
               --  S22=-S11
               co (c_s.all.z.all.c, -S11, 0);
            else
               co (c_s.all.z.all.c, S21, 0);
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
      i : Integer;
      unit1, prefix : Character;
      value_string : line_string;
      alt_param : Boolean;
   begin
      if action
      then
         --  [P2Ada]: WITH instruction
         --  [P2Ada]: !Help! No type found -> add 'P2Ada_Var_21.' to fields
         declare P2Ada_Var_21 : compt renames tcompt.all;
         begin
            --  * value_string, prefix are ignored
            --  value has prefixes factored in at this point
            --  if entered in dB then unit1 returns 'd' or 'D'
            --  if no unit then unit1 = '?'
            number_of_con := 2;
            con_space := 0.0;
            Get_Param (tcompt, 1, value, value_string, unit1, prefix,
                       alt_param);
            if bad_compt
            then
               return;
            end if;
            if prefix /= ' '
            then
               bad_compt := True;
               message (1) := "No prefixes";
               message (2) := "allowed for";
               message (3) := "attenuator";
               return;
            end if;
            --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
            if not (unit1 and ('d' | 'D' | '?' => True, others => False))
            then
               bad_compt := True;
               message (1) := "Enter";
               message (2) := "attenuation";
               message (3) := "in dB";
               return;
            end if;
            if abs (value) > 99
            then
               bad_compt := True;
               message (1) := "Attenuation";
               message (2) := "out of range";
               return;
            end if;
            if alt_param
            then
               x_sweep.init_element (tcompt, 'a', prefix, unit1);
            end if;
            if bad_compt
            then
               return;
            end if;
            --  * Draw as a square box using *
            --  * Manhattan dimensions  *
            --  zed is dB value of S12,S21
            width := 2.0 * Manh_width;
            lngth := width;
            zed := value;
         end;
         --  [P2Ada]: end of WITH
         --  with
         --  action
         --  * Fill frequency independent scattering parameters *
         --  convert from dB for S12, S21
         --  S11
      else
         value := Exp (-ln10 * tcompt.all.zed / 20.0);
         new_s (tcompt.all.s_begin);
         c_s := tcompt.all.s_begin;
         New_c (c_s.all.z);
         co (c_s.all.z.all.c, 0.0, 0.0);
         for i in 2 .. 4
         loop
            new_s (c_s.all.next_s);
            c_s := c_s.all.next_s;
            New_c (c_s.all.z);
            if i = 4
            then
               --  S22
               co (c_s.all.z.all.c, 0.0, 0.0);
            else
               co (c_s.all.z.all.c, value, 0);
            end if;
            --  S12=S21
         end loop;
         --  for i
         c_s.all.next_s := null;
      end if;
   end Attenuator;
   --  * Attenuator *
   --  *
   --  See PFMSC.PAS for Device_Read(): reads file data.
   --  Called only if action is false,
   --  to calculate interpolated device s-parameters.
   --  Indef specifies whether or not to enable the generation
   --  of indefinite scattering parameters (extra port).
   --  type compt has the following s_param records:
   --  tcompt^.s_begin,
   --  tcompt^.s_file,
   --  tcompt^.s_ifile,
   --  tcompt^.f_file
   --  *
   --  label
   --  read_finish;

   procedure Device_S (tcompt : compt; indef : boolean) is
      --  * Device_S *
      c_ss, c_f, c_is : s_param;
      s1, s2 : array (1 .. 10, 1 .. 10) of PMemComplex;
      found : Boolean;
      i, j, k, txpt, tnpts : Integer;
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
         declare P2Ada_Var_22 : compt renames tcompt.all;
         begin
            if f_file /= null
            then
               tfmin := f_file.all.z.all.c.r;
            end if;
            --  first frequency
            s_ifile := null;
            for txpt in 0 .. tnpts
            loop
               if f_file /= null
               then
                  if txpt = tnpts
                  then
                     tfreq := sxmax;
                  else
                     tfreq := fmin + txpt * finc;
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
                           c_f := f_file;
                        else
                           c_f := c_f.all.next_s;
                        end if;
                        for j in 1 .. number_of_con
                        loop
                           for i in 1 .. number_of_con
                           loop
                              if c_ss = null
                              then
                                 c_ss := s_file;
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
                        for j in 1 .. number_of_con
                        loop
                           for i in 1 .. number_of_con
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
                     for j in 1 .. number_of_con
                     loop
                        for i in 1 .. number_of_con
                        loop
                           if c_ss = null
                           then
                              c_ss := s_file;
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
               for j in 1 .. number_of_con
               loop
                  for i in 1 .. number_of_con
                  loop
                     if s_ifile = null
                     then
                        new_s (s_ifile);
                        c_is := s_ifile;
                     else
                        new_s (c_is.all.next_s);
                        c_is := c_is.all.next_s;
                     end if;
                     c_is.all.next_s := null;
                     if found
                     then
                        --  *device s-parameter interpolation routine*
                        New_c (c_is.all.z);
                        if f_file = null
                        then
                           ffac := 0;
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
         --  [P2Ada]: end of WITH
         --  with
      end if;
      --  if xpt:=0
      if Alt_Sweep
      then
         --  only one freq
         txpt := 1;
      else
         txpt := xpt;
      end if;
      --  [P2Ada]: WITH instruction
      --  [P2Ada]: !Help! No type found -> add 'P2Ada_Var_23.' to fields
      declare P2Ada_Var_23 : compt renames tcompt.all;
      begin
         s_begin := s_ifile;
         for k in 0 .. txpt - 1
         loop
            --  advance to next freq
            for j in 1 .. number_of_con
            loop
               for i in 1 .. number_of_con
               loop
                  s_begin := s_begin.all.next_s;
                  if s_begin = null
                  then
                     message (2) := "s out of range";
                     shutdown;
                  end if;
               end loop;
            end loop;
         end loop;
         --  for i,j,k
         if s_begin.all.z = null
         then
            bad_compt := True;
            Erase_Message;
            message (1) := "Frequency out of";
            message (2) := "   range given   ";
            message (3) := "in device file";
         end if;
      end;
      --  [P2Ada]: end of WITH
      --  with
   end Device_S;
   --  * Device_S *
   --  *
   --  Display change in x an y in mm when cursor X moves.
   --  *

   procedure dx_dyO is
      --  *****************************************************
      pos_prefix : string := "EPTGMk m" + Mu + "npfa";
      i, j : Integer;
      dx, dy : Long_Float;
      dx_prefix, dy_prefix : Character;

      procedure Big_Check (dt : in out Long_Float; i_in : in out integer) is
      begin
         while abs (dt) > 1000.0
         loop
            --  set-up prefix change
            dt := dt / 1000;
            i_in := i_in - 1;
         end loop;
      end Big_Check;
      --  *****************************************************

      procedure Small_Check (dt : in out Long_Float; i_in : in out integer) is
      begin
         while abs (dt) < 0.01
         loop
            --  set-up prefix change
            dt := dt * 1000;
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
      dx_prefix := pos_prefix (i);
      j := 8;
      if dy /= 0.0
      then
         Big_Check (dy, j);
         Small_Check (dy, j);
      end if;
      if read_kbd
      then
         TextCol (lightgray);
         GotoXY (xmin (6) + 2, ymin (6) + 1);
         if abs (dx) > resln
         then
            --  [P2Ada]: !Help! Maybe (file,...) here
            --  [P2Ada]: !Help! Maybe (file,...) here
            --  [P2Ada]: !Help! Maybe (file,...) here
            --  [P2Ada]: !Help! Maybe (file,...) here
            Put (P2Ada_no_keyword_delta);
            Put ("x ");
            Put (dx, 7, 3, 0);
            Put (dx_prefix);
            Put ('m');
            GotoXY (xmin (6) + 2, ymin (6));
         else
            GotoXY (xmin (6) + 2, ymin (6) + 1);
         end if;
         if abs (dy) > resln
         then
            --  [P2Ada]: !Help! Maybe (file,...) here
            --  [P2Ada]: !Help! Maybe (file,...) here
            --  [P2Ada]: !Help! Maybe (file,...) here
            --  [P2Ada]: !Help! Maybe (file,...) here
            Put (P2Ada_no_keyword_delta);
            Put ("y ");
            Put (-dy, 7, 3, 0);
            Put (dy_prefix);
            Put ('m');
         end if;
      end if;
   end dx_dyO;
   --  * dx_dyO *
   --  *
   --  Get lead character to determine if part is
   --  tline, clines, device, lumped, etc.
   --  *

   function get_lead_charO (tcompt : compt) return Character is
      --  [BP2P]: Label "100001" Was "exit_label"
      Result_get_lead_charO : Character;
      xstr : line_string;
      char1 : Character;
   begin
      xstr := tcompt.all.descript;
      Delete (xstr, 1, 1);
      char1 := xstr (1);
      loop
         if xstr (1) = ' '
         then
            Delete (xstr, 1, 1);
         end if;
         if (xstr'Length) = 0
         then
            --  [BP2P]: Label "100001" Was "exit_label"
            goto LABEL_100001;
         end if;
         char1 := xstr (1);
         exit when xstr (1) /= ' ';
      end loop;
      --  [BP2P]: Label "100001" Was "exit_label"
      --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
      <<LABEL_100001>>
      if char1 and ('A' .. 'Z' => True, others => False)
      then
         char1 := Character (Character'Pos (char1) + 32);
      end if;
      Result_get_lead_charO := char1;
      return Result_get_lead_charO;
   end get_lead_charO;
   --  get_lead_charO
   --  *
   --  Draw a transmission line and lumped element on the circuit board.
   --  *

   procedure Draw_tline (tnet : net; linex, seperate : boolean) is
      x1, x2, y1, y2, x3, y3, x1e, y1e, x2e, y2e, i, j : Integer;
      x1r, y1r, x2r, y2r : Long_Float;
   begin
      x1r := tnet.all.xr - lengthxm * yii / 2.0;
      x2r := x1r + lengthxm * (xii + yii);
      y1r := tnet.all.yr - lengthym * xii / 2.0;
      y2r := y1r + lengthym * (yii + xii);
      x1 := Round (x1r / csx + xmin (1));
      x2 := Round (x2r / csx + xmin (1));
      x3 := Round ((x1r + x2r) / (2.0 * csx) + xmin (1));
      y1 := Round (y1r / csy + ymin (1));
      y2 := Round (y2r / csy + ymin (1));
      y3 := Round ((y1r + y2r) / (2.0 * csy) + ymin (1));
      x1e := Round (tnet.all.xr / csx) + xmin (1);
      y1e := Round (tnet.all.yr / csy) + ymin (1);
      x2e := Round ((tnet.all.xr + lengthxm * xii) / csx) + xmin (1);
      y2e := Round ((tnet.all.yr + lengthym * yii) / csy) + ymin (1);
      if linex
      then
         fill_box (x1, y1, x2, y2, brown);
      else
         if x1 = x2
         then
            --  make 50 Ohms wide if width=0
            x1 := Round (x1 - widthZ0 / (2.0 * csx));
            x2 := Round (x2 + widthZ0 / (2.0 * csx));
         else
            if y1 = y2
            then
               --  make 50 Ohms wide if width=0
               y1 := Round (y1 - widthZ0 / (2.0 * csy));
               y2 := Round (y2 + widthZ0 / (2.0 * csy));
            end if;
         end if;
         --  if width<>0 i.e. Manhattan, just draw it
         draw_box (x1, y1, x2, y2, lightblue);
         if tnet.all.com.all.typ = 'a'
         then
            --  * Make small boxes for atten terminals *
            box (x1e, y1e, 1);
            box (x2e, y2e, 1);
         end if;
      end if;
      if seperate
      then
         x1r := (tnet.all.xr + tnet.all.com.all.con_space * yii / 2.0) /
           csx + xmin (1);
         y1r := (tnet.all.yr + tnet.all.com.all.con_space * xii / 2.0) /
           csy + ymin (1);
         if not ((x1 = x2) or else (y1 = y2))
         then
            SetCol (black);
            Line (Round (x1r), Round (y1r), Round (x1r + lengthxm * xii / csx),
                  Round (y1r + lengthym * yii / csy));
         end if;
      end if;
      --  * Write graphics character to identify part *
      --  make sure center dot is black
      SetTextJustify (CenterText, CenterText);
      PutPixel (x3, y3, Black);
      SetCol (Black);
      for i in -1 .. 1
      loop
         for j in -1 .. 1
         loop
            OutTextXY (x3 + i, y3 + j, tnet.all.com.all.descript (1));
         end loop;
      end loop;
      --  shadow
      --  letter
      SetCol (LightRed);
      OutTextXY (x3, y3, tnet.all.com.all.descript (1));
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
      x2 := Round ((tnet.all.xr + lengthxm * xii) / csx) + xmin (1);
      y2 := Round ((tnet.all.yr + lengthym * yii) / csy) + ymin (1);
      xt := Round (yii * lengthym / (2.0 * csx));
      yt := Round (xii * lengthxm / (2.0 * csy));
      x3 := Round ((x1 + x2) / 2.0);
      y3 := Round ((y1 + y2) / 2.0);
      SetTextJustify (CenterText, CenterText);
      SetCol (lightblue);
      Line (x1 - xt, y1 - yt, x1 + xt, y1 + yt);
      Line (x1 + xt, y1 + yt, x2 + (xt / 2), y2 + (yt / 2));
      Line (x2 + (xt / 2), y2 + (yt / 2), x2 - (xt / 2), y2 - (yt / 2));
      Line (x2 - (xt / 2), y2 - (yt / 2), x1 - xt, y1 - yt);
      SetCol (LightRed);
      OutTextXY (x3, y3, tnet.all.com.all.descript (1));
      box (x1, y1, 1);
      box (x2, y2, 1);
   end Draw_xformer;
   --  * Draw_xformer *
   --  *
   --  Draw a triangle to represent a device.
   --  *

   procedure Draw_device (tnet : net) is
      --  center text for OutText
      --  * Make small boxes for device terminals *
      --  [P2Ada]: WITH instruction
      --  [P2Ada]: !Help! No type found -> add 'P2Ada_Var_24.' to fields
      sx1, sy1 : Long_Float;
      x1, x2, y1, y2, x3, y3, xt, yt, i : Integer;
   begin
      x1 := Round (tnet.all.xr / csx) + xmin (1);
      y1 := Round (tnet.all.yr / csy) + ymin (1);
      x2 := Round ((tnet.all.xr + lengthxm * xii) / csx) + xmin (1);
      y2 := Round ((tnet.all.yr + lengthym * yii) / csy) + ymin (1);
      xt := Round (yii * lengthym / (2.0 * csx));
      yt := Round (xii * lengthxm / (2.0 * csy));
      x3 := Round ((2 * x1 + x2) / 3.0);
      y3 := Round ((2 * y1 + y2) / 3.0);
      SetTextJustify (CenterText, CenterText);
      SetCol (lightblue);
      Line (x1 - xt, y1 - yt, x1 + xt, y1 + yt);
      Line (x1 + xt, y1 + yt, x2, y2);
      Line (x2, y2, x1 - xt, y1 - yt);
      SetCol (LightRed);
      OutTextXY (x3, y3, tnet.all.com.all.descript (1));
      declare P2Ada_Var_24 : tnet renames tnet.all;
      begin
         if number_of_con = 1
         then
            box (x1, y1, 1);
         else
            sx1 := (x2 - x1) / (number_of_con - 1);
            sy1 := (y2 - y1) / (number_of_con - 1);
            for i in 0 .. number_of_con - 1
            loop
               box (x1 + Round (i * sx1), y1 + Round (i * sy1), 1);
            end loop;
            if tnet.all.com.all.typ = 'i'
            then
               if number_of_con = 3
               then
                  --  Yellow box at port 2
                  box (x1 + Round (sx1), y1 + Round (sy1), 4);
               else
                  box (x1 + Round ((number_of_con - 1) * sx1), y1 +
                         Round ((number_of_con - 1) * sy1), 4);
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

   procedure Add_Coord (x1, xb, xl, y1 : integer; just, brd : boolean;
                        tdes : line_string) is
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
      --  [P2Ada]: WITH instruction
      --  [P2Ada]: !Help! No type found -> add 'P2Ada_Var_25.' to fields
      declare P2Ada_Var_25 : compt renames ccompt.all;
      begin
         --  x-position
         --  length of number
         --  amount of text
         --  y-position
         xpt := x1;
         xorig := x1;
         xmax := xl;
         x_block := xb;
         yp := y1;
         right := just;
         descript := tdes;
      end;
      --  [P2Ada]: end of WITH
   end Add_Coord;
   --  add_coord
   --  *
   --  Set up parameters for Plot window.
   --  Called by Puff_Start to initialize to blank parameters,
   --  and by Read_Net() after reading file key.
   --  Uses Add_Coord;
   --  *

   procedure Set_Up_KeyO is
      --  dBmax
      --  save this position for swapping, also coord_start
      --  dBmin
      --  fmin
      --  fmax
   begin
      ccompt := null;
      Add_Coord (x_y_plot_text (1, 1), 0, 5, x_y_plot_text (1, 2), true, false,
                 s_key (1));
      dBmax_ptr := ccompt;
      Add_Coord (x_y_plot_text (3, 1), 0, 5, x_y_plot_text (3, 2), true, false,
                 s_key (2));
      Add_Coord (x_y_plot_text (4, 1), 0, 7, x_y_plot_text (4, 2), false,
                 false, s_key (3));
      fmin_ptr := ccompt;
      Add_Coord (x_y_plot_text (6, 1), 0, 7, x_y_plot_text (6, 2), true, false,
                 s_key (4));
      add_coord (xmin (2), 7, 12, ymin (2), false, false, "Points " +
                   s_key (5));
      Points_compt := ccompt;
      add_coord (xmin (2), 13, 17, ymin (2) + 1, false, false, "Smith radius "
                 + s_key (6));
      rho_fac_compt := ccompt;
      add_coord (xmin (2) + 2, 1, 3, ymin (2) + 3, false, false, 'S' +
                   s_key (7));
      s_param_table (1) := ccompt;
      add_coord (xmin (2) + 2, 1, 3, ymin (2) + 4, false, false, 'S' +
                   s_key (8));
      s_param_table (2) := ccompt;
      add_coord (xmin (2) + 2, 1, 3, ymin (2) + 5, false, false, 'S' +
                   s_key (9));
      s_param_table (3) := ccompt;
      add_coord (xmin (2) + 2, 1, 3, ymin (2) + 6, false, false, 'S' +
                   s_key (10));
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
   --  *Set_Up_KeyO*
   --  *
   --  Set up parameters for BOARD window.
   --  Called by Puff_Start to initialize blank parameters,
   --  and by Read_Net() after Read_Board.
   --  Uses Add_Coord;
   --  *

   procedure Set_Up_Board is
      --  zd norm impedance
      --  fd design_freq
      --  er dielectric const
      --  h substrate thick
      --  s board size
      --  c conn separation
   begin
      ccompt := null;
      add_coord (xmin (4), 4, 16, ymin (4), false, true, "zd  " +
                   s_board (1, 1) + ' ' + s_board (1, 2) + Omega);
      add_coord (xmin (4), 4, 16, ymin (4) + 1, false, true, "fd  " +
                   s_board (2, 1) + ' ' + s_board (2, 2) + "Hz");
      add_coord (xmin (4), 4, 16, ymin (4) + 2, false, true, "er  " +
                   s_board (3, 1));
      add_coord (xmin (4), 4, 16, ymin (4) + 3, false, true, "h   " +
                   s_board (4, 1) + ' ' + s_board (4, 2) + 'm');
      add_coord (xmin (4), 4, 16, ymin (4) + 4, false, true, "s   " +
                   s_board (5, 1) + ' ' + s_board (5, 2) + 'm');
      add_coord (xmin (4), 4, 16, ymin (4) + 5, false, true, "c   " +
                   s_board (6, 1) + ' ' + s_board (6, 2) + 'm');
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
      csy := bmax / (ymax (1) - ymin (1));
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
      --  * Artwork correction added ^ *
      --  artwork dimen
      --  screen dimen
      --  * (Global Var) Sheet resistance of metal at design_freq *
      --  * (Global Var) Wavelength in mm at design freq. *
      pwidthxZ02 := Round (widthZ0 * 0.5 / psx);
      pwidthyZ02 := Round (widthZ0 * 0.5 / psy);
      cwidthxZ02 := Round (widthZ0 * 0.5 / csx);
      cwidthyZ02 := Round (widthZ0 * 0.5 / csy);
      Rs_at_fd := sqrt (Pi * design_freq * Eng_Prefix (freq_prefix) * Mu_0 /
                          conductivity);
      Lambda_fd := c_in_mm / (design_freq * Eng_Prefix (freq_prefix));
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
         SetCol (yellow);
         Line (x1 - 4, y1, x1 + 4, y1);
         Line (x1 - 2, y1 + 2, x1 + 2, y1 + 2);
         Line (x1 - 1, y1 + 4, x1 + 1, y1 + 4);
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

   function Look_BackO return boolean is
      Result_Look_BackO : Boolean;
      tcon, scon, mtcon, mscon : conn;
      x1, x2, y1, y2, cs, cm : Long_Float;
      coupler_found : Boolean;
      tnet : net;
      d2 : Integer;
   begin
      look_backO := false;
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
                  if tcon.all.mate.all.net.all.com.all.typ = 'c'
                  then
                     --  if cline to cline
                     --  find averages of connector separation between old and
                     --  new
                     cs := (tcon.all.mate.all.net.all.com.all.con_space +
                              compt1.all.con_space) / 2.0;
                     cm := (tcon.all.mate.all.net.all.com.all.con_space -
                              compt1.all.con_space) / 2.0;
                     d2 := tcon.all.dir;
                     mtcon := tcon.all.mate;
                     case tcon.all.mate.all.conn_no is
                        --  look at conns old clines
                        when 1 | 2 =>
                           --  advance 2 connectors
                           mscon := mtcon.all.next_con.all.next_con;
                        when 3 =>
                           --  con 3 -> con 1
                           mscon := mtcon.all.net.all.con_start;
                        when 4 =>
                           --  con 4 -> con 2
                           mscon := mtcon.all.net.all.con_start.all.next_con;
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
                     Mate_Node (3) := scon.all.net;
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
                           Mate_Node (1) := Mate_Node (3);
                           mate_node (3) := tnet;
                           look_backO := true;
                        when 4 =>
                           ym := ym - cm;
                           look_backO := true;
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
                           Mate_Node (1) := Mate_Node (3);
                           mate_node (3) := tnet;
                           look_backO := true;
                        when 2 =>
                           ym := ym + cm;
                           look_backO := true;
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
                           Mate_Node (1) := Mate_Node (3);
                           mate_node (3) := tnet;
                           look_backO := true;
                        when 1 =>
                           xm := xm - cm;
                           look_backO := true;
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
                           Mate_Node (1) := Mate_Node (3);
                           mate_node (3) := tnet;
                           look_backO := true;
                        when 8 =>
                           xm := xm + cm;
                           look_backO := true;
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
   --  look_backO

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
      csy := bmax / Long_Float ((ymax (1) - ymin (1)));
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
      Rs_at_fd := Sqrt (Pi * design_freq * Eng_prefix (freq_prefix) * Mu_0 /
                          conductivity);
      Lambda_fd := c_in_mm / (design_freq * Eng_Prefix (freq_prefix));
      tcompt := null;
      loop
         --  * Force parsing of each part *
         if tcompt = null
         then
            tcompt := part_start;
         else
            tcompt := tcompt.next_compt;
         end if;
         tcompt.changed := True;
         exit when tcompt.next_compt = null;
      end loop;
   end Fresh_Dimensions;

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
         m := 2 / (1.0 + 2 * x / (3 * (1.0 - x)));
         delta_W_b_t := x * (1.0 - 0.5 * Log (((x / (2.0 - x)) ** 2) +
                               exp (m * Log (0.0796 * x /
                                 ((W / b) + 1.1 * x))))) / (Pi * (1.0 - x));
         W_over_b_t := (W / (b - metal_thickness)) + delta_W_b_t;
         W_prime := (b - metal_thickness) * W_over_b_t;
         b_t_W := 1 / W_over_b_t;
         A_fac := Log (1.0 + (4 * b_t_W / Pi) * ((8 * b_t_w / Pi) +
                         sqrt (((8 * b_t_w / Pi) ** 2) + 6.27))) / Pi;
         Z_0_t := 30 * Pi * A_fac / sqrt (er);
         Q_fac := sqrt (1.0 + 6.27 * ((Pi * W_over_b_t / 8) ** 2));
         partial_Z_w := 30.0 * (3.135 / Q_fac - (1 + Q_fac - 0.303 / Q_fac) *
                                ((8.0 / (Pi * W_over_b_t)) ** 2)) /
           (exp (Pi * A_fac) * W_prime);
         log_term := 1.0 + 2 * W_over_b_t - (3 * x / (2.0 - x) +
                                               Log (x / (2.0 - x))) / Pi;
         declare P2Ada_Var_8 : compt renames tcompt;
         begin
            --  nepers/mm
            alpha_c := -Rs_at_fd * partial_Z_w * log_term /
              (120 * Pi * Z_0_t);
            zed := Z_0_t;
         end;
         --  [P2Ada]: end of WITH
      end if;
      --  if t>0
      --  Add dielectric losses, despite metal thickness
      --  [P2Ada]: WITH instruction
      --  [P2Ada]: !Help! No type found -> add 'P2Ada_Var_9.' to fields
      declare P2Ada_Var_9 : compt renames tcompt.all;
      begin
         --  nepers/mm
         super := true;
         alpha_d := Pi * sqrt (er) * loss_tangent / lambda_fd;
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
            a_int := 1.0 + Log ((exp (4.0 * Log (u)) + ((u / 52.0) ** 2.0)) /
                                (exp (4.0 * Log (u)) + 0.432)) / 49.0;
         Result_a := a_int + Log (1.0 + exp (3 * Log (u / 18.1))) / 18.7;
         return Result_a;
      end a;
      --  *********************************

      function e_e (u : Long_Float) return Long_Float is
         Result_e_e : Long_Float;
      begin
         Result_e_e := (er + 1.0) / 2 + ((er - 1.0) / 2) *
           exp (-1.0 * a (u) * b * Log (1.0 + 10 / u));
         return Result_e_e;
      end e_e;
      --  *********************************
   begin
      u_in := tcompt.all.width / substrate_h;
      b := 0.564 * exp (0.053 * Log ((er - 0.9) / (er + 3.0)));
      t_n := metal_thickness / substrate_h;
      if t_n > 0.0
      then
            delta_ul := (t_n / Pi) * Log (1.0 + (4 * exp (1.0)) /
                                          (t_n * ((cosh (sqrt (6.517 * u_in)) /
                                          sinh (sqrt (6.517 * u_in))) ** 2)));
         delta_ur := 0.5 * (1.0 + 1.0 / (cosh (sqrt (er - 1.0)))) * delta_ul;
         ul := u_in + delta_ul;
         ur := u_in + delta_ur;
      else
         ul := u_in;
         ur := u_in;
      end if;
      --  * For ms_dispersion, compute value for zed_S_e0 from
      --  equivalent zero thickness stripline with b=2h.
      --  Double the value obtained from the formulas with b=2h.
      --  Use super_stripline formulas with t=0, b=2*h *
      --  [P2Ada]: WITH instruction
      --  [P2Ada]: !Help! No type found -> add 'P2Ada_Var_7.' to fields
      Z_0 := Hammerstad_Z (ur) / sqrt (e_e (ur));
      e_eff := e_e (ur) * ((Hammerstad_Z (ul) / Hammerstad_Z (ur)) ** 2);
      b_t_W := 2 / u_in;
         A_fac := Log (1.0 + (4 * b_t_W / Pi) * ((8 * b_t_w / Pi) +
                         sqrt (((8 * b_t_w / Pi) ** 2) + 6.27)));
      Z_0_t := 60 * A_fac / sqrt (er);
      declare P2Ada_Var_7 : compt renames tcompt.all;
      begin
         --  nepers/mm
         --  nepers/mm
         --  replace with new zed
         --  save f=0 value for ms_dispersion
         --  save value here for ms_dispersion
         --  save for tline and ms_dispersion use
         --  new length
         alpha_c := ms_alpha_c (tcompt.all.width, Z_0, e_eff);
         alpha_d := ms_alpha_d (e_eff);
         zed := Z_0;
         zed_e0 := Z_0;
         zed_S_e0 := Z_0_t;
         e_eff_e0 := e_eff;
         wavelength := lngth0 * sqrt (e_eff) / lambda_fd;
         super := true;
      end;
      --  [P2Ada]: end of WITH
   end super_microstrip;

   procedure ms_dispersion (tcompt : compt) is
      --  F4 = f*h in units of GHz-cm (freq normalized to board thickness
      u_in, P, P1, P2, P3, P4, F4, ere0, ere_f : Long_Float;
   begin
      F4 := substrate_h * freq * Eng_Prefix (freq_prefix) / 1.0e+10;
      if F4 > 25
      then
         --  ! Unstable, substitute asymptotic values
         --  [P2Ada]: WITH instruction
         --  [P2Ada]: !Help! No type found -> add 'P2Ada_Var_1.' to fields
         declare P2Ada_Var_1 : compt renames tcompt.all;
         begin
            zed := zed_S_e0;
            wavelength := lngth0 * sqrt (er) / lambda_fd;
         end;
         --  [P2Ada]: end of WITH
      else
         if F4 > 0.0
         then
            --  from advanced model super_microstrip
            --  [P2Ada]: WITH instruction
            --  [P2Ada]: !Help! No type found -> add 'P2Ada_Var_2.' to fields
            u_in := tcompt.all.width / substrate_h;
            ere0 := tcompt.all.e_eff_e0;
            P1 := 0.27488 + (0.6315 + 0.525 / exp (20 *
                               Log (1.0 + 0.157 * F4))) * u_in - 0.065683 *
                               exp (-8.7513 * u_in);
            P2 := 0.33622 * (1.0 - exp (-0.03442 * er));
            P3 := 0.0363 * exp (-4.6 * u_in) * (1.0 - exp (-1.0 *
                               exp (4.97 * Log (F4 / 3.87))));
            P4 := 1.0 + 2.751 * (1.0 - exp (-1.0 *
                               exp (8 * Log (er / 15.916))));
            P := P1 * P2 * exp (1.5763 * Log ((0.1844 + P3 * P4) * 10 * F4));
            ere_f := Disperse_f (er, ere0, P);
            declare P2Ada_Var_2 : compt renames tcompt.all;
            begin
               zed := Disperse_f (zed_S_e0, zed_e0, P);
               wavelength := lngth0 * sqrt (ere_f) / lambda_fd;
            end;
            --  [P2Ada]: end of WITH
            --  F4=0
            --  [P2Ada]: WITH instruction
            --  [P2Ada]: !Help! No type found -> add 'P2Ada_Var_3.' to fields
         else
            declare P2Ada_Var_3 : compt renames tcompt.all;
            begin
               --  reset dc values if a re-sweep w/o parsing
               zed := zed_e0;
               wavelength := lngth0 * sqrt (e_eff_e0) / lambda_fd;
            end;
            --  [P2Ada]: end of WITH
         end if;
      end if;
   end ms_dispersion;

end pfun2;
