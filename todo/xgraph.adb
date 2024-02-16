--  ParamStr,...
--  UpCase
--  For Shift_Left/Right
--  This is for Pi :
--  This is for Sqrt, Sin, Cos, etc. :
--  This is for Dispose. P2Ada writes automatically:
--  "Dispose is new Ada.Unchecked_Deallocation(<type>, <pointer type>)".
with Ada.Text_IO;
use Ada.Text_IO;
with Ada.Integer_Text_IO;
use Ada.Integer_Text_IO;
with Ada.Float_Text_IO;
use Ada.Float_Text_IO;
with Ada.Long_Float_Text_IO;
use Ada.Long_Float_Text_IO;
with Ada.Direct_IO;
with Ada.Command_Line;
use Ada.Command_Line;
with Ada.Characters.Handling;
use Ada.Characters.Handling;
with Interfaces;
use Interfaces;
with Ada.Numerics;
use Ada.Numerics;
with Ada.Numerics.Elementary_Functions;
use Ada.Numerics.Elementary_Functions;
with Ada.Numerics.Long_Elementary_Functions;
use Ada.Numerics.Long_Elementary_Functions;
with Ada.Unchecked_Deallocation;

package xgraph is
   ViewPortType is record X1, Y1, X2, Y2 : Integer;
   Clip : Boolean;
end record;
--  cdecl; external;

procedure InitGraph (GraphDriver, GraphModus : in out integer; PathToDriver : in string);
--  cdecl; external;

procedure CloseGraph;
--  cdecl; external;

procedure SetColor (Color : Integer_32);
--  cdecl; external;

procedure SetFillStyle (Pattern, Color : Integer_32);
--  cdecl; external;

procedure SetLineStyle (LineStyle, Pattern, Width : Integer_32);
--  cdecl; external;

procedure Line (X1, Y1, X2, Y2 : Integer_32);
--  cdecl; external;

procedure PutPixel (X, Y : Integer_32; Color : Integer_32);
--  cdecl; external;

procedure Bar (X1, Y1, X2, Y2 : Integer_32);
--  cdecl; external;

procedure Rectangle (X1, Y1, X2, Y2 : Integer_32);
--  cdecl; external;

procedure Arc (X, Y : Integer_32; start, stop, radius : Integer_32);
--  cdecl; external;

procedure FillEllipse (X, Y : Integer_32; Xradius, YRadius : Integer_32);
--  cdecl; external;

procedure Circle (X, Y : Integer_32; Radius : Integer_32);
--  cdecl; external;

procedure FloodFill (X, Y : Integer_32; BorderColor : Integer_32);
--  cdecl; external;

procedure SetTextJustify (Horizontal, Vertical : Integer_32);

procedure OutTextXY (X, Y : Integer; TextString : in String);
--  cdecl; external;

procedure SetViewPort (X1, Y1, X2, Y2 : Integer_32; Clip : Boolean);
--  cdecl; external;

function GetBkColor return Integer_32;
--  cdecl; external;

procedure SetBkColor (Color : Integer_32);
--  cdecl; external;

function GraphErrorMsg (ErrorCode : Integer_32) return String;
--  cdecl; external;

function GraphResult return Integer_32;
--  cdecl; external;

procedure GetBox (bn, x, y, width, height : Integer_32);
--  cdecl; external;

procedure PutBox (bn, x, y, width, height : Integer_32);
--  cdecl; external;
--  external name 'ScreenHeight';
--  external name 'ScreenWidth';

procedure init_x;
ScreenHeight : integer;
ScreenWidth : integer;
SOLIDFILL : constant := 0;
lefttext : constant := 0;
centertext : constant := 1;
righttext : constant := 2;
userbitln : constant := 4;
normwidth : constant := 1;
solidln : constant := 0;
VGA : constant := 0;
EGA : constant := 1;
GROK : constant := 0;
Black : constant := 0;
Blue : constant := 1;
Green : constant := 2;
Cyan : constant := 3;
Red : constant := 4;
Magenta : constant := 5;
Brown : constant := 6;
LightGray : constant := 7;
DarkGray : constant := 8;
LightBlue : constant := 9;
LightGreen : constant := 10;
LightCyan : constant := 11;
LightRed : constant := 12;
LightMagenta : constant := 13;
Yellow : constant := 14;
White : constant := 15;
co80 : constant := 3;
WindMax : Unsigned_16 := 16 #08 4f #0F LastMode : Unsigned_16 := 3;
DirectVideo : Boolean := False;
--  cdecl; external name 'crtWindow';

procedure Window (X1, Y1, X2, Y2 : Integer_32);
--  cdecl; external;

procedure GotoXY (X : Integer_32; Y : Integer_32);
--  cdecl; external;

procedure Sound (hz : Integer_32);
--  cdecl; external;

procedure P2Ada_no_keyword_Delay (DTime : Integer_32);
--  cdecl; external;

procedure NoSound;
--  cdecl; external;

function KeyPressed return Boolean;
--  cdecl; external;

function ReadKey return Character;
--  cdecl; external;

procedure TextMode (Mode : Integer_32);
--  cdecl; external;

procedure TextColor (CL : Integer_32);
--  cdecl; external;

procedure TextBackground (CL : Integer_32);
--  cdecl; external;

procedure ClrScr;
--  cdecl; external;

function GetTimerTicks return Integer;
end xgraph;

--  <<END OF DOCUMENT>>  --
--  Translated on 7-Feb-2024 by (New) P2Ada v. 28-Oct-2009
--  Translated on 7-Feb-2024 by (New) P2Ada v. 28-Oct-2009
--  The following with/use clauses are put graciously by P2Ada.
--  Some of them may be useless, your Ada compiler will tell it you.
--  (GNAT: with '-gnatwa')
--  ParamStr,...
--  UpCase
--  For Shift_Left/Right
--  This is for Pi :
--  This is for Sqrt, Sin, Cos, etc. :
--  This is for Dispose. P2Ada writes automatically:
--  "Dispose is new Ada.Unchecked_Deallocation(<type>, <pointer type>)".
--  [P2Ada]: place it before main procedure
with Ada.Text_IO;
use Ada.Text_IO;
with Ada.Integer_Text_IO;
use Ada.Integer_Text_IO;
with Ada.Float_Text_IO;
use Ada.Float_Text_IO;
with Ada.Long_Float_Text_IO;
use Ada.Long_Float_Text_IO;
with Ada.Direct_IO;
with Ada.Command_Line;
use Ada.Command_Line;
with Ada.Characters.Handling;
use Ada.Characters.Handling;
with Interfaces;
use Interfaces;
with Ada.Numerics;
use Ada.Numerics;
with Ada.Numerics.Elementary_Functions;
use Ada.Numerics.Elementary_Functions;
with Ada.Numerics.Long_Elementary_Functions;
use Ada.Numerics.Long_Elementary_Functions;
with Ada.Unchecked_Deallocation;
with dos, linux;

package body xgraph is
   --  [P2Ada]: This is for 'Write([Boolean])'

   package Boolean_Text_IO is new Enumeration_IO (Boolean);
   use Boolean_Text_IO;
   --  [P2Ada]: This is for 'file' without type
   --  [P2Ada]: This is for the Halt pseudo-procedure

   package Byte_Direct_IO is new Ada.Direct_IO (Unsigned_8);
   Program_halted : exception;
   --  cdecl; external;

   procedure C_OutTextXY (X, Y : Integer_32; TextString : in out String) is

      procedure OutTextXY (X, Y : Integer; TextString : in String) is
         s : string;
      begin
         s := TextString;
         C_OutTextXY (X, Y, s);
      end OutTextXY;
      --  write(ln) and readln support code based on rtl/linux/crt.pp from the fpc source tree
      --  cdecl; external;

      procedure DoWrite (s : in out string) is
         --  cdecl; external;

         procedure DoWriteEnd is

            function xgraphWrite (F : in out TextRec) return Integer is
               Result_xgraphWrite : Integer;
               Temp : String;
               idx, i : Integer_32;
            begin
               idx := 0;
               while (F.BufPos > 0)
               loop
                  i := F.BufPos;
                  if i > 255
                  then
                     i := 255;
                  end if;
                  Move (F.BufPTR.all (idx), Temp (1), i);
                  SetLength (Temp, i);
                  DoWrite (Temp);
                  F.BufPos := F.BufPos - (i);
                  idx := idx + (i);
               end loop;
               DoWriteEnd;
               Result_xgraphWrite := 0;
               return Result_xgraphWrite;
            end xgraphWrite;
            --  Read from CRT associated file.

            function xgraphRead (F : in out TextRec) return Integer is
               Result_xgraphRead : Integer;
               c : Character;
               i : Integer_32;
            begin
               F.BufPos := 0;
               i := 0;
               loop
                  c := readkey;
                  case c is
                     --  ignore special keys
                     when Character'Val (0) =>
                        --  Backspace
                        c := readkey;
                     when Character'Val (8) =>
                        if i > 0
                        then
                           Put (Character'Val (8) & Character'Val (32) & Character'Val (8));
                           i := i - 1;
                        end if;
                        --  Unhandled extended key
                     when Character'Val (27) =>
                        --  CR
                        null;
                     when Character'Val (13) =>
                        F.BufPtr.all (i) := Character'Val (10);
                        Put (Character'Val (10));
                        i := i + 1;
                     when others =>
                        begin
                           Put (c);
                           F.BufPtr.all (i) := c;
                           i := i + 1;
                        end;
                  end case;
                  --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
                  exit when (c and (Character'Val (10) | Character'Val (13) => True, others => False)) or (i >= F.BufSize);
               end loop;
               F.BufEnd := i;
               Result_xgraphRead := 0;
               return Result_xgraphRead;
               return Result_xgraphRead;
            end xgraphRead;

            function xgraphReturn (F : in out TextRec) return Integer is
               Result_xgraphReturn : Integer;
            begin
               Result_xgraphReturn := 0;
               return Result_xgraphReturn;
            end xgraphReturn;

            function xgraphOpen (F : in out TextRec) return Integer is
               Result_xgraphOpen : Integer;
            begin
               if F.Mode = fmOutput
               then
                  --  TextRec(F).
                  --  TextRec(F).
                  InOutFunc := addr (xgraphWrite);
                  FlushFunc := addr (xgraphWrite);
               end if;
               if F.Mode = fmInput
               then
                  --  TextRec(F).
                  --  TextRec(F).
                  InOutFunc := addr (xgraphRead);
                  FlushFunc := addr (xgraphReturn);
               end if;
               Result_xgraphOpen := 0;
               return Result_xgraphOpen;
            end xgraphOpen;

            procedure Initialize is
               --  TextRec(Output).
               --  TextRec(Output).
               --  TextRec(Input).
               --  TextRec(Input).
               --  ScreenHeight:=500;
               --  ScreenWidth:=1000;
            begin
               Assign (Output, "");
               OpenFunc := System.Address_To_Access_Conversions.To_Address (xgraphOpen);
               Rewrite (Output);
               Handle := StdOutputHandle;
               Assign (Input, "");
               OpenFunc := System.Address_To_Access_Conversions.To_Address (xgraphOpen);
               Reset (Input);
               Handle := StdInputHandle;
               init_x;
            end Initialize;
