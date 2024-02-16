
with Ada.Text_IO;             use Ada.Text_IO;
with Ada.Integer_Text_IO;     use Ada.Integer_Text_IO;
with Ada.Float_Text_IO;       use Ada.Float_Text_IO;
with Ada.Long_Float_Text_IO;  use Ada.Long_Float_Text_IO;
with Ada.Direct_IO;
with Ada.Command_Line;        use Ada.Command_Line;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Interfaces;              use Interfaces;
with Ada.Numerics;            use Ada.Numerics;
with Ada.Numerics.Elementary_Functions;
   use Ada.Numerics.Elementary_Functions;
with Ada.Numerics.Long_Elementary_Functions;
   use Ada.Numerics.Long_Elementary_Functions;
with Ada.Unchecked_Deallocation;

with xgraph, pfun1, pfun2;

package pfart is
   procedure Make_HPGL_File;
   procedure Printer_Artwork;
end pfart;
