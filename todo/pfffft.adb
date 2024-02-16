--  ParamStr,...
--  UpCase
--  For Shift_Left/Right
--  This is for Pi :
--  This is for Sqrt, Sin, Cos, etc. :
--  This is for Dispose. P2Ada writes automatically:
--  "Dispose is new Ada.Unchecked_Deallocation(<type>, <pointer type>)".
--  Custom replacement unit for TUBO's "Crt" and "Graph" units
--  Use other puff unit
--  [P2Ada]: place it before main procedure
--  Local code:
--  Procedure Fill_Data_4_FFT(ij,istart,ifinish : integer; nf : double);
--  Procedure Four1(ij,nn,isign : integer);
--  Procedure Real_FFT(ij,n,isign : integer);
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
with xgraph, pfun1, pfun3;

package pffft is

   procedure Time_Response;
end pffft;
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

package body pffft is
   --  [P2Ada]: This is for 'Write([Boolean])'

   package Boolean_Text_IO is new Enumeration_IO (Boolean);
   use Boolean_Text_IO;
   --  [P2Ada]: This is for 'file' without type
   --  [P2Ada]: This is for the Halt pseudo-procedure
   --  2*256+2 nft max= 256
   --  data array for FFT

   package Byte_Direct_IO is new Ada.Direct_IO (Unsigned_8);
   Program_halted : exception;
   farray is array (1 .. 514, 1 .. max_params) of Long_Float;
   Data : farray;
   --  *
   --  Fill array data for FFT and apply weighting.
   --  Global variable filled is `Data' of type farray
   --  where farray = array[1..514,1..max_params] of double.
   --  514=2*nft+2   nft= 256, max_params=4
   --  ij: scattering parameter index.
   --  nf: normalization factor.
   --  wf:   Weighting factor applied for either the impulse
   --  or step function.
   --  cplt: Used to step through the linked list of frequency
   --  data. cplt^.x =real part, cplt^.y=imag.
   --  The weighting function used for the impulse response is
   --  a raised cosine function. It is purely real.
   --  The weighting function used for the step response is
   --  1/omega, band limited by a raised cosine. It is
   --  purely imaginary. And, is an odd function.
   --  Since the routines included in PUFF are only used
   --  to generate a real time function from the complex
   --  frequency function, it is only necessary to use
   --  positive frequency components. Therefore, the
   --  size of Data is half of that anticipated.
   --  *

   procedure Fill_Data_4_FFT (ij, istart, ifinish : integer; nf : Long_Float) is
      wf : Long_Float;
      i : integer;
      cplt : plot_param;
   begin
      for i in 1 .. (nft + 1)
      loop
         if betweeni (istart, i - 1, ifinish)
         then
            if (i - 1) = istart
            then
               cplt := plot_start (ij);
            else
               cplt := cplt.all.next_p;
            end if;
            if step_fn
            then
               --  Apply weighting for step function
               if not (((i) mod 2 /= 0))
               then
                  --  1/i weighting, band limited by raised cosine
                  --  wf is supposed to be -imaginary, so swap x and y
                  --  force function to be odd in (i-1)
                  wf := nf * (1.0 + cos (pi * (i - 1) / (ifinish + 1))) / (i - 1);
                  data (2 * i - 1, ij) := wf * cplt.all.y;
                  data (2 * i, ij) := - wf * cplt.all.x;
               else
                  data (2 * i - 1, ij) := 0;
                  data (2 * i, ij) := 0;
               end if;
               --  Apply raised cosine band limiting for the impulse response
            else
               wf := nf * (1.0 + cos (pi * (i - 1) / (ifinish + 1)));
               data (2 * i - 1, ij) := wf * cplt.all.x;
               data (2 * i, ij) := wf * cplt.all.y;
            end if;
            --  else if step_fn
            --  fill with zeros outside of frequency range
         else
            data (2 * i - 1, ij) := 0;
            data (2 * i, ij) := 0;
         end if;
      end loop;
      --  else if between
      data (2, ij) := data (2 * nft + 1, ij);
   end Fill_Data_4_FFT;
   --  * Fill_Data_4_FFT *
   --  *
   --  FFT as per Press et. al. Numerical Recipes p 394, p 754.
   --  Uses global array Data[1..514,1..4].
   --  if isign=1 then FFT, else inverse FFT (without scaling).
   --  However, in Puff since an inverse FFT is performed
   --  going from complex frequency data to real time data,
   --  use isign=1 when calling from Real_FFT.
   --  *
   --  label
   --  one,two;

   procedure Four1 (ij, nn, isign : integer) is
      ii, jj, n, i, m, j, mmax, istep : integer;
      wr, wi, wpr, wpi, wtemp, theta : Long_Float;
      tempr, tempi : Long_Float;
   begin
      n := 2 * nn;
      j := 1;
      for ii in 1 .. nn
      loop
         i := 2 * ii - 1;
         if (j > i)
         then
            Tempr := Data (j, ij);
            Tempi := Data (j + 1, ij);
            Data (j, ij) := Data (i, ij);
            Data (j + 1, ij) := Data (i + 1, ij);
            Data (i, ij) := Tempr;
            Data (i + 1, ij) := Tempi;
         end if;
         m := n / 2;
         while (m >= 2) and then (j > m)
         loop
            j := j - m;
            m := m / 2;
         end loop;
         j := j + m;
      end loop;
      --  for ii
      mmax := 2;
      while (n > mmax)
      loop
         istep := 2 * mmax;
         theta := 2 * pi / (isign * mmax);
         wpr := - 2.0 * ((sin (theta / 2.0)) ** 2);
         wpi := sin (theta);
         wr := 1.0;
         wi := 0.0;
         for ii in 1 .. (mmax / 2)
         loop
            m := 2 * ii - 1;
            for jj in 0 .. ((n - m) / istep)
            loop
               i := m + jj * istep;
               j := i + mmax;
               tempr := wr * data (j, ij) - wi * data (j + 1, ij);
               tempi := wr * data (j + 1, ij) + wi * data (j, ij);
               data (j, ij) := data (i, ij) - tempr;
               data (j + 1, ij) := data (i + 1, ij) - tempi;
               data (i, ij) := data (i, ij) + tempr;
               data (i + 1, ij) := data (i + 1, ij) + tempi;
            end loop;
            --  for jj
            wtemp := wr;
            wr := wr * wpr - wi * wpi + wr;
            wi := wi * wpr + wtemp * wpi + wi;
         end loop;
         --  for ii
         mmax := istep;
      end loop;
      --  while n>
   end Four1;
   --  * Four1 *
   --  *
   --  Perform real FFT as per Press et. al. Numerical Recipes p 400.
   --  (a.k.a. "realft")
   --  Uses global array Data[1..514,1..4].
   --  Called only by Time_Response.
   --  In puff isign=-1 so that the inverse FFT is always performed.
   --  Call is made to Four1 to execute FFT.
   --  Although not used here, if isign=+1 then FFT done.
   --  *

   procedure Real_FFT (ij, n, isign : integer) is
      i, i1, i2, i3, i4 : integer;
      c1, c2, h1r, h1i, h2i, h2r : Long_Float;
      wr, wi, wpr, wpi, wtemp, theta : Long_Float;
   begin
      theta := 2 * pi / (2.0 * n);
      wr := 1.0;
      wi := 0.0;
      c1 := 0.5;
      if (isign = 1)
      then
         c2 := - 0.5;
         theta := - theta;
         Four1 (ij, n, 1);
         data (2 * n + 1, ij) := data (1, ij);
         data (2 * n + 2, ij) := data (2, ij);
      else
         c2 := 0.5;
         data (2 * n + 1, ij) := data (2, ij);
         data (2 * n + 2, ij) := 0.0;
         data (2, ij) := 0.0;
      end if;
      wpr := - 2.0 * ((sin (theta / 2.0)) ** 2);
      wpi := sin (theta);
      for i in 1 .. (n / 2) + 1
      loop
         i1 := 2 * i - 1;
         i2 := i1 + 1;
         i3 := 2 * n + 3 - i2;
         i4 := i3 + 1;
         h1r := c1 * (data (i1, ij) + data (i3, ij));
         h1i := c1 * (data (i2, ij) - data (i4, ij));
         h2r := - c2 * (data (i2, ij) + data (i4, ij));
         h2i := c2 * (data (i1, ij) - data (i3, ij));
         data (i1, ij) := h1r + wr * h2r - wi * h2i;
         data (i2, ij) := h1i + wr * h2i + wi * h2r;
         data (i3, ij) := h1r - wr * h2r + wi * h2i;
         data (i4, ij) := - h1i + wr * h2i + wi * h2r;
         wtemp := wr;
         wr := wr * wpr - wi * wpi + wr;
         wi := wi * wpr + wtemp * wpi + wi;
      end loop;
      --  for i
      if (isign = 1)
      then
         data (2, ij) := data (2 * n + 1, ij);
      else
         Four1 (ij, n, 1);
      end if;
   end Real_FFT;
   --  * Real_FFT *
   --  *
   --  Main procedure for FFT to get time response.
   --  *

   procedure Time_Response is
      nf, delt, time, hir_temp : Long_Float;
      istart, ifinish, x1, y1, i, ij, col : integer;
   begin
      hir_temp := 1.0;
      Erase_Message;
      istart := Round (fmin / finc);
      ifinish := istart + npts;
      if ifinish > nft
      then
         message (2) := "fmax/df too large";
         Write_Message;
         return;
      end if;
      nf := 0;
      for i in 1 .. (nft + 1)
      loop
         if betweeni (istart, i - 1, ifinish)
         then
            if step_fn
            then
               if not (((i) mod 2 /= 0))
               then
                  if ((i / 2) mod 2 /= 0)
                  then
                     nf := nf + (1.0 + cos (pi * (i - 1) / (ifinish + 1))) / (i - 1);
                  else
                     nf := nf - (1.0 + cos (pi * (i - 1) / (ifinish + 1))) / (i - 1);
                  end if;
               end if;
            else
               if (i - 1) > 0
               then
                  nf := nf + (1.0 + cos (pi * (i - 1) / (ifinish + 1)));
               else
                  nf := nf + 1.0;
               end if;
            end if;
         end if;
      end loop;
      nf := 1.0 / nf;
      marker_OK := false;
      delt := 1.0 / (2.0 * nft * finc);
      sxmin := - q_fac / (8 * design_freq);
      sxmax := 3 * q_fac / (8 * design_freq);
      symax := rho_fac;
      symin := - rho_fac;
      Draw_Graph (xmin (8), ymin (8), xmax (8), ymax (8), true);
      sfx1 := (xmax (8) - xmin (8)) / (sxmax - sxmin);
      sfy1 := (ymax (8) - ymin (8)) / (symax - symin);
      for ij in 1 .. max_params
      loop
         if s_param_table (ij).all.calc
         then
            --  fill Data[1..514,1..4]
            --  convert complex data to real time function
            col := s_color (ij);
            Fill_Data_4_FFT (ij, istart, ifinish, nf);
            Real_FFT (ij, nft, - 1);
            for i in 1 .. 2 * nft
            loop
               time := (i - 1) * delt;
               if time > sxmax
               then
                  time := time - 2 * nft * delt;
               end if;
               x1 := xmin (8) + Round ((time - sxmin) * sfx1);
               if betweenr (symin, data (i, ij), symax, 1 / sfy1)
               then
                  y1 := ymax (8) - Round ((data (i, ij) - symin) * sfy1);
                  if betweeni (xmin (8), x1, xmax (8))
                  then
                     PutPixel (x1, Round (hir_temp * y1), col);
                  end if;
               end if;
            end loop;
            --  i
         end if;
      end loop;
      --  ij
      message (1) := "Type any key";
      message (2) := "to return to the";
      message (3) := "frequency domain";
      Write_Message;
      chs := ReadKey;
      if keypressed
      then
         chs := ReadKey;
      end if;
      Erase_Message;
      Draw_Graph (xmin (8), ymin (8), xmax (8), ymax (8), false);
   end Time_Response;
   --  * Time_Response *
end pffft;
