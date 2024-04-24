
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package pfrw is

   procedure Read_Net (fname : in out Unbounded_String;
                       init_graphics : Boolean);

   procedure Read_Board (read_graphics : Boolean);

   procedure read_setup (fname2 : in out Unbounded_String);

   procedure bad_board;

end pfrw;
