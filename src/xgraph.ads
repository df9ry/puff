with Interfaces; use Interfaces;

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;

package xgraph is

   type ViewPortType is record
      X1, Y1, X2, Y2 : Integer_32;
      Clip : Boolean;
   end record with Convention => C;

   ScreenHeight : Integer_32;
   pragma Import (C, ScreenHeight, "ScreenHeight");

   ScreenWidth  : Integer_32;
   pragma Import (C, ScreenWidth, "ScreenWidth");

   SOLIDFILL    : constant := 0;
   lefttext     : constant := 0;
   centertext   : constant := 1;
   righttext    : constant := 2;
   userbitln    : constant := 4;
   normwidth    : constant := 1;
   solidln      : constant := 0;
   VGA          : constant := 0;
   EGA          : constant := 1;
   GROK         : constant := 0;
   Black        : constant := 0;
   Blue         : constant := 1;
   Green        : constant := 2;
   Cyan         : constant := 3;
   Red          : constant := 4;
   Magenta      : constant := 5;
   Brown        : constant := 6;
   LightGray    : constant := 7;
   DarkGray     : constant := 8;
   LightBlue    : constant := 9;
   LightGreen   : constant := 10;
   LightCyan    : constant := 11;
   LightRed     : constant := 12;
   LightMagenta : constant := 13;
   Yellow       : constant := 14;
   White        : constant := 15;
   co80         : constant := 3;
   WindMax      : Unsigned_16 := 16#184f#;
   LastMode     : Unsigned_16 := 3;
   DirectVideo  : Boolean := False;
   --  cdecl; external name 'crtWindow';

   procedure InitGraph (GraphDriver, GraphModus : in out Integer;
                        PathToDriver : String);
   pragma Import (C, InitGraph, "InitGraph");

   procedure CloseGraph;
   pragma Import (C, CloseGraph, "CloseGraph");

   procedure SetColor (Color : Unsigned_16);
   pragma Import (C, SetColor, "SetColor");

   procedure SetFillStyle (Pattern, Color : Integer_32);
   pragma Import (C, SetFillStyle, "SetFillStyle");

   procedure SetLineStyle (LineStyle, Pattern, Width : Integer_32);
   pragma Import (C, SetLineStyle, "SetLineStyle");

   procedure Line (X1, Y1, X2, Y2 : Integer_32);
   pragma Import (C, Line, "Line");

   procedure PutPixel (X, Y : Integer_32; Color : Integer_32);
   pragma Import (C, PutPixel, "PutPixel");

   procedure Bar (X1, Y1, X2, Y2 : Integer_32);
   pragma Import (C, Bar, "Bar");

   procedure Rectangle (X1, Y1, X2, Y2 : Integer_32);
   pragma Import (C, Rectangle, "Rectangle");

   procedure Arc (X, Y : Integer_32; start, stop, radius : Integer_32);
   pragma Import (C, Arc, "Arc");

   procedure FillEllipse (X, Y : Integer_32; Xradius, YRadius : Integer_32);
   pragma Import (C, FillEllipse, "FillEllipse");

   procedure Circle (X, Y : Integer_32; Radius : Integer_32);
   pragma Import (C, Circle, "Circle");

   procedure FloodFill (X, Y : Integer_32; BorderColor : Integer_32);
   pragma Import (C, FloodFill, "FloodFill");

   procedure SetTextJustify (Horizontal, Vertical : Integer_32);
   pragma Import (C, SetTextJustify, "SetTextJustify");

   procedure OutTextXY (X, Y : Integer; TextString : char_array);
   pragma Import (C, OutTextXY, "OutTextXY");

   procedure SetViewPort (X1, Y1, X2, Y2 : Integer_32; Clip : Integer_8);
   pragma Import (C, SetViewPort, "SetViewPort");

   function GetBkColor return Unsigned_16;
   pragma Import (C, GetBkColor, "GetBkColor");

   procedure SetBkColor (Color : Unsigned_16);
   pragma Import (C, SetBkColor, "SetBkColor");

   function  GraphErrorMsg (ErrorCode : Integer_32) return char_array_access;
   pragma Import (C, GraphErrorMsg, "GraphErrorMsg");

   function GraphResult return Integer_32;
   pragma Import (C, GraphResult, "GraphResult");

   procedure GetBox (bn, x, y, width, height : Integer_32);
   pragma Import (C, GetBox, "GetBox");

   procedure PutBox (bn, x, y, width, height : Integer_32);
   pragma Import (C, PutBox, "PutBox");

   procedure Init_X;
   pragma Import (C, Init_X, "init_x");

   procedure Window (X1, Y1, X2, Y2 : Integer_32);
   pragma Import (C, Window, "crtWindow");

   procedure ResizeWindow (width, height : Integer_32);
   pragma Import (C, ResizeWindow, "ResizeWindow");

   procedure GotoXY (X : Integer_32; Y : Integer_32);
   pragma Import (C, GotoXY, "GotoXY");

   procedure PutStr (Text : char_array);
   pragma Import (C, PutStr, "PutStr");

   procedure PutCh (Ch : char);
   pragma Import (C, PutCh, "PutCh");

   procedure Sound (hz : Integer_32);
   pragma Import (C, Sound, "Sound");

   procedure C_Delay (DTime : Integer_32);
   pragma Import (C, C_Delay, "Delay");

   procedure NoSound;
   pragma Import (C, NoSound, "NoSound");

   function KeyPressed return Integer_8;
   pragma Import (C, KeyPressed, "KeyPressed");

   function ReadKey return Integer_32;
   pragma Import (C, ReadKey, "ReadKey");

   procedure TextMode (Mode : Integer_32);
   pragma Import (C, TextMode, "TextMode");

   procedure TextColor (CL : Unsigned_16);
   pragma Import (C, TextColor, "TextColor");

   procedure TextBackground (CL : Unsigned_16);
   pragma Import (C, TextBackground, "TextBackground");

   procedure ClrScr;
   pragma Import (C, ClrScr, "ClrScr");

   function GetTimerTicks return Integer_64;
   pragma Import (C, GetTimerTicks, "GetTimerTicks");

end xgraph;
