with Ada.Text_IO;             use Ada.Text_IO;

with xgraph;                  use xgraph;
with pfst;                    use pfst;
with pfrw;                    use pfrw;
with pfun1;                   use pfun1;
with Utils;                   use Utils;

procedure puff is
begin
   Init_X;
   Puff_Start;
   Post_Read_Net (puff_file);

exception
   when Program_halted =>
      Put_Line ("Regular program halt.");
   when others =>
      Put_Line ("Other exception occurred!");
end puff;
