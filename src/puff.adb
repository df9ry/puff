with xgraph; use xgraph;

with pfst;   use pfst;

procedure puff is

   ch : Character;

begin
   Init_X;
   Puff_Start;

   loop
      ch := ReadKey;
      exit when ch = ASCII.ESC;
   end loop;
end puff;
