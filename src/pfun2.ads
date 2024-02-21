
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

   procedure Set_Up_KeyO;

   procedure Update_KeyO_locations;

   procedure Set_Up_Board;

   procedure Fresh_Dimensions;

   procedure draw_groundO (xr, yr : Long_Float);

   function look_backO return Boolean;

end pfun2;
