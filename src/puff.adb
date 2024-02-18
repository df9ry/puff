with Ada.Text_IO;             use Ada.Text_IO;

with xgraph;                  use xgraph;
with pfst;                    use pfst;
with utils;                   use utils;

procedure puff is

begin
   Init_X;
   Puff_Start;

exception
   when Program_halted =>
      Put_Line ("Regular program halt.");
end puff;
