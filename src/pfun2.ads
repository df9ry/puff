
with pfun1; use pfun1;

package pfun2 is

   procedure snapO;

   procedure tlineO (tcompt : compt);

   procedure qline (tcompt : compt);

   procedure clinesO (tcompt : compt);

   procedure lumpedO (tcompt : compt);

   procedure Transformer (tcompt : compt);

   procedure Attenuator (tcompt : compt);

   procedure Device_S (tcompt : compt; indef : Boolean);

   procedure dx_dyO;

   function get_lead_charO (tcompt : compt) return Character;

   procedure Draw_tline (tnet : net; linex, seperate : Boolean);

   procedure Draw_xformer (tnet : net);

   procedure Draw_device (tnet : net);

   procedure draw_groundO (xr, yr : Long_Float);

   procedure Set_Up_KeyO;

   procedure Update_KeyO_locations;

   procedure Set_Up_Board;

   procedure Fresh_Dimensions;

   function look_backO return Boolean;

   procedure super_stripline (tcompt : compt);

   procedure super_microstrip (tcompt : compt);

   procedure super_cl_stripline (tcompt : compt;
                                 widthc, spacec : Long_Float);

   procedure super_cl_microstrip (tcompt : compt;
                                  widthc, spacec : Long_Float);

   procedure ms_dispersion (tcompt : compt);

   procedure ms_cl_dispersion (tcompt : compt);

   function Hammerstad_Z (u : Long_Float) return Long_Float;

   function ms_alpha_c (W_in, Z_0_in, er_e : Long_Float) return Long_Float;

   function ms_alpha_d (ere_in : Long_Float) return Long_Float;

   function Disperse_f (er1, er2, F_eo : Long_Float) return Long_Float;

end pfun2;
