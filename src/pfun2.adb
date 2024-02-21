
with Ada.Numerics.Long_Elementary_Functions;
use  Ada.Numerics.Long_Elementary_Functions;

with Utils; use Utils;

package body pfun2 is

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

end pfun2;
