with pfun1; use pfun1;

package pfmsc is

   procedure Device_Read (tcompt : compt; indef : Boolean);

   procedure Indef_Matrix (S : in out s_conv_matrix; n : Integer);

   procedure Draw_EGA_Smith (imped_chart : Boolean);

end pfmsc;
