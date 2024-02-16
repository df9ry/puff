--  ParamStr,...
--  UpCase
--  For Shift_Left/Right
--  This is for Pi :
--  This is for Sqrt, Sin, Cos, etc. :
--  This is for Dispose. P2Ada writes automatically:
--  "Dispose is new Ada.Unchecked_Deallocation(<type>, <pointer type>)".
--  Unit found in Free Pascal RTL's
--  Unit found in Free Pascal RTL's
--  [P2Ada]: place it before main procedure
--  Custom replacement unit for TUBO's "Crt" and "Graph" units
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
with Dos, Printer, xgraph;

package pfun1 is
   --  * The following constants are the keyboard return codes.
   --  See Turbo Manual Appendix K.
   --  For the extended codes 27 XX, key=#128+XX 		*
   --  pseudo key, used to report a screen size change in the Linux version
   --  The following characters refer to extended graphics character set
   --  Lambda=#128;
   --  Shift_arrow=#130;
   --  ground=#132;
   --  * Number of parts in parts list *
   --  * Conv_size is maximum matrix size for sdevice et al.*
   --  * Engineering Decimal multipliers *
   --  * Attenuation factor for tee's, crosses,
   --  shorts, opens and negative resistors  *
   --  * Global const needed by the Assembler
   --  Routines for Sine_Asm, Cos_Asm 	*
   --  * Use only 10 digits of Pi to *
   --  * ensure numerical stability  *
   --  * Number of FFT points *
   --  * Speed of light in mm/s *
   --  * Permeability of free space H/m *
   --  **** Temp Circuit Board constants **************
   --  ************************************************
   --  * POINTER Types *
   --  [P2Ada]: Insert 'type s_parameter_record;' before.
   not_esc : constant String := Character'Val (0);
   Ctrl_a : constant String := Character'Val (1);
   Ctrl_d : constant String := Character'Val (4);
   Ctrl_e : constant String := Character'Val (5);
   backspace : constant String := Character'Val (8);
   Ctrl_n : constant String := Character'Val (14);
   Ctrl_p : constant String := Character'Val (16);
   Ctrl_r : constant String := Character'Val (18);
   Ctrl_s : constant String := Character'Val (19);
   Ctrl_c : constant String := Character'Val (3);
   Ctrl_q : constant String := Character'Val (17);
   Esc : constant String := Character'Val (27);
   sh_1 : constant String := Character'Val (33);
   sh_3 : constant String := Character'Val (35);
   sh_4 : constant String := Character'Val (36);
   sh_2 : constant String := Character'Val (64);
   Alt_o : constant String := Character'Val (152);
   Alt_d : constant String := Character'Val (160);
   Alt_m : constant String := Character'Val (178);
   Alt_p : constant String := Character'Val (153);
   Tab : constant String := Character'Val (9);
   sh_down : constant String := Character'Val (178);
   sh_left : constant String := Character'Val (180);
   sh_right : constant String := Character'Val (182);
   sh_up : constant String := Character'Val (184);
   C_R : constant String := Character'Val (13);
   F1 : constant String := Character'Val (187);
   F2 : constant String := Character'Val (188);
   F3 : constant String := Character'Val (189);
   shift_F3 : constant String := Character'Val (214);
   F4 : constant String := Character'Val (190);
   F5 : constant String := Character'Val (191);
   F6 : constant String := Character'Val (192);
   shift_F5 : constant String := Character'Val (216);
   F10 : constant String := Character'Val (196);
   Alt_s : constant String := Character'Val (159);
   up_arrow : constant String := Character'Val (200);
   PgUp : constant String := Character'Val (201);
   left_arrow : constant String := Character'Val (203);
   right_arrow : constant String := Character'Val (205);
   down_arrow : constant String := Character'Val (208);
   PgDn : constant String := Character'Val (209);
   Ins : constant String := Character'Val (210);
   Del : constant String := Character'Val (211);
   screenresize : constant String := Character'Val (255);
   P2Ada_no_keyword_Delta : constant String := Character'Val (235);
   the_bar : constant String := Character'Val (179);
   infin : constant String := Character'Val (236);
   ity : constant String := Character'Val (32);
   Parallel : constant String := Character'Val (186);
   Mu : constant String := Character'Val (230);
   Omega : constant String := Character'Val (234);
   Degree : constant String := Character'Val (248);
   lbrack : constant String := Character'Val (123);
   rbrack : constant String := Character'Val (125);
   des_len : constant := 22;
   max_net_size : constant := 9;
   Conv_size : constant := 9;
   col_window : array (1 .. 4) of integer := (Lightcyan, Lightgreen, Yellow, Lightblue);
   s_color : array (1 .. 4) of integer := (Lightred, Lightcyan, Lightblue, Yellow);
   Eng_Dec_Mux : array (Character) of Boolean := ('E' | 'P' | 'T' | 'G' | 'M' | 'k' | 'm' | Mu | 'n' | 'p' | 'f' | 'a' => True, others => False);
   charx : constant := 8;
   chary : constant := 14;
   key_max : constant := 2000;
   max_ports : constant := 4;
   max_params : constant := 4;
   one : Long_Float := 0.999999999998765;
   minus2 : integer := - 2;
   ln10 : Long_Float := 2.302585092994045684;
   Pi : Long_Float := 3.141592653;
   infty : Long_Float := 1.0e + 37;
   nft : constant := 256;
   c_in_mm : Long_Float := 3.0e + 11;
   Mu_0 : Long_Float := 1.25664e - 6;
   textfile is new Ada.Text_IO.File_Type;
   line_string is new String (1 .. des_len);
   file_string is new String (1 .. 127);
   char_s is array (1 .. 112) of Unsigned_8;
   s_param is access s_parameter_record;
   --  [P2Ada]: Insert 'type plot_record;' before.

   procedure Dispose is new Ada.Unchecked_Deallocation (s_parameter_record, s_param);
   plot_param is access plot_record;
   --  [P2Ada]: Insert 'type spline_record;' before.

   procedure Dispose is new Ada.Unchecked_Deallocation (plot_record, plot_param);
   spline_param is access spline_record;
   --  [P2Ada]: Insert 'type net_record;' before.

   procedure Dispose is new Ada.Unchecked_Deallocation (spline_record, spline_param);
   net is access net_record;
   --  [P2Ada]: Insert 'type connector_record;' before.

   procedure Dispose is new Ada.Unchecked_Deallocation (net_record, net);
   conn is access connector_record;
   --  [P2Ada]: Insert 'type compt_record;' before.

   procedure Dispose is new Ada.Unchecked_Deallocation (connector_record, conn);
   compt is access compt_record;

   procedure Dispose is new Ada.Unchecked_Deallocation (compt_record, compt);
   marker is record Used : Integer_32;
end record;
--  ********************* PUFF DATA STRUCTURES ****************
TComplex is
record
   --  ! This was changed from a real for speed increase
   r, i : Long_Float;
end record;
--  [P2Ada]: Insert 'type TMemComplex;' before.
PMemComplex is access TMemComplex;

procedure Dispose is new Ada.Unchecked_Deallocation (TMemComplex, PMemComplex);
TMemComplex is
record
   c : TComplex;
end record;
s_parameter_record is
record
   z : PMemComplex;
   next_s : s_param;
end record;
plot_record is
record
   next_p, prev_p : plot_param;
   filled : boolean;
   x, y : Long_Float;
end record;
spline_record is
record
   next_c, prev_c : spline_param;
   sx, sy, h : Long_Float;
end record;
net_record is
record
   --  postion in mm
   com : compt;
   con_start : conn;
   xr, yr : Long_Float;
   node, chamfer, grounded : boolean;
   next_net, other_net : net;
   nx1, nx2, ny1, ny2, number_of_con, nodet, ports_connected : integer;
end record;
connector_record is
record
   --  0 norm 1... max_port external 5,6 internal
   --  position in mm
   port_type, conn_no : integer;
   cxr, cyr : Long_Float;
   dir : Unsigned_8;
   net : net;
   next_con, mate : conn;
   s_start : s_param;
end record;
compt_record is
record
   lngth, width, zed, zedo, init_ere, alpha_c, alpha_d, alpha_co, alpha_do, lngth0, wavelength, wavelengtho, zed_e0, zed_o0, zed_S_e0, zed_S_o0, e_eff_e0, e_eff_o0, u_even, u_odd, g_fac, con_space, spec_freq : Long_Float;
   xp, xmaxl, x_block, xorig, yp, number_of_con, used : integer;
   s_begin, s_file, s_ifile, f_file : s_param;
   calc, changed, right, super, parsed, step, sweep_compt : boolean;
   next_compt, prev_compt : compt;
   descript : line_string;
   typ : Character;
end record;
key_record is
record
   keyl : Character;
   noden : integer;
end record;
--  * The following types are defined for S parameter conversions: *
--  * Conv_size is currently set to 9 *
--  **************** PUFF OBJECTS **********************
s_conv_matrix is array (1 .. conv_size, 1 .. conv_size) of TComplex;
s_conv_vector is array (1 .. conv_size) of TComplex;
s_conv_index is array (1 .. conv_size) of integer;
Sweep is tagged
record
   --  pointer to part
   --  ident, prefix and units
   --  labels for plot window
   --  Proportionality constants
   --  Part type index
   --  angular frequency at fd and new value
   --  has sweep been used?
   element : compt;
   id, prefix, units : Character;
   part_label, unit_label : string (1 .. 6);
   prop_const1, prop_const2 : Long_Float;
   index : integer;
   Omega0, new_value : Long_Float;
   used : boolean;
end record;

procedure Init_Use (Self : in out Sweep'class);

procedure Init_Element (Self : in out Sweep'class; tcompt : compt; in_id, in_prefix, in_unit : Character);

procedure Check_Reset (Self : in out Sweep'class; tcompt : compt);

procedure Label_Axis (Self : in out Sweep'class);

procedure Label_Plot_Box (Self : in out Sweep'class);

procedure Load_Prop_Const (Self : in out Sweep'class; prop_consta, prop_constb : in Long_Float);

procedure Load_Index (Self : in out Sweep'class; i : integer);
--  sweep object
--  **************** PUFF VARIABLES ********************
--  1+j0
--  2k buffer for net_ and dev_ files
--  x_sweep is the object with x-y plot information
--  List decribing circuit
--  used in reading board setup
--  plot window paramters
--  board window parameters
--  Which s-params to plot
--  used by plotting in rcplot
--  Dot colors under cross
--  Dot colors under markers
--  true if box_dot set
--  Record of port
--  Is port input or output?
--  used in layout of clines
--  Displayed message
--  * The following graphics variables were constants in Puff 1.5 *
--  These were constants in the EGA version
--  They contain both text and graphics
--  positions for each of the windows
--  Smith chart position variables
--  Needed for extra wide windows on Linux
--  Used for 25/34 line EGA/VGA switch
--  used to specify aspect ratios
--  was 1..3
--  Start and end of list of s-params
--  ,cursor_char
--  dirn 1=North 2=East 4=West 8=South
--  Names put on artwork
--  keys for linked list
--  prefix for design_freq
--  start, end, current and design s-params
--  Mark() beginning of network for later release
--  Mark() beginning of device file data for release
--  frequency minimum,frequency increment
--  characteristic impedance
--  radius factor of smith chart
--  fd/df=Q
--  resolution of circuit drawing in mm
--  scale factors for pixels for circuit drawing
--  max and min values on rectangular plot
--  photographic reduction ratio
--  relative substrate dielectric constant
--  substrate board size in mm
--  substrate thickness
--  connector seperation
--  current frequency and design frequency in GHz
--  Sheet resistance at design frequency
--  Wavelength in mm, in air, at design freq
--  circuit cursor postion in mm
--  width of normalizing impedance
--  length in x and y current part
--  Manhattan layout length - read_board
--  Manhattan layout width - read_board
--  units are mhos/meter
--  for dielectric, unitless
--  in millimeters
--  in micrometers
--  screen and mask half width Z0
--  color in message block
--  current window number
--  maximum number of graph points
--  Re(s),Im(s),|s| dot position
--  0 if dot-matrix, 1 LaserJet, 2 HPGL
--  used to remember initial text mode
--  Used for graphics initialization
--  True if LaserJet printer selected
--  admittance smith chart flag
--  is help window currently displayed?
--  True when a part j..r has been used for layout
--  true when extra parts are selected
--  is large Smith chart enabled? VGA only
--  do alternate parameter sweep
--  Draw all parts in Manhattan Geometry
--  True if part used in layout was deleted
--  ***** INTERFACE List of Functions and Procedures to be Public *****

procedure Load_Data (Self : in out Sweep'class; sweep_data : in Long_Float);
co1 : TComplex;
c_s : s_param;
conk, ccon : conn;
sresln : string (1 .. 9);
big_text_buf : array (1 .. 2048) of Character;
sdevice : s_conv_matrix;
x_sweep : sweep;
iji : array (1 .. 16, 1 .. 2) of integer;
key_list : array (1 .. key_max) of key_record;
board : array (1 .. 16) of boolean;
s_key : array (1 .. 10) of line_string;
s_board : array (1 .. 12, 1 .. 2) of line_string;
s_param_table : array (1 .. max_params) of compt;
xvalo, yvalo : array (1 .. 4) of integer;
cross_dot : array (1 .. 35) of integer;
box_dot : array (1 .. 26, 0 .. 8) of integer;
box_filled : array (1 .. 8) of boolean;
portnet : array (0 .. max_ports) of net;
inp, P2Ada_no_keyword_out : array (1 .. max_ports) of boolean;
si, sj : array (1 .. max_ports) of integer;
mate_node : array (1 .. 4) of net;
message : array (1 .. 3) of file_string;
xmin : array (1 .. 12) of integer;
ymin : array (1 .. 12) of integer;
xmax : array (1 .. 12) of integer;
ymax : array (1 .. 12) of integer;
centerx, centery, rad : integer;
Max_Text_X : integer;
Max_Text_Y : integer;
yf : Long_Float;
x_y_plot_text : array (1 .. 6, 1 .. 2) of integer;
filename_position : array (1 .. 3) of integer;
checking_position, layout_position : array (1 .. 2) of integer;
puff_file : file_string;
net_file, dev_file : textfile;
command_f, window_f : array (1 .. 4) of compt;
spline_start, spline_end : spline_param;
dirn : Unsigned_8;
name, network_name : line_string;
key, key_o, chs, previous_key, freq_prefix : Character;
plot_start, plot_end, c_plot, plot_des : array (1 .. max_params) of plot_param;
net_beg, dev_beg : marker;
Box_Sv_Pntr : array (1 .. 8) of Pointer;
Box_Sv_Size : array (1 .. 8) of Unsigned_16;
fmin, finc, Z0, rho_fac, q_fac, resln, sfx1, sfy1, xrold, yrold, sigma, sxmax, sxmin, symax, symin, reduction, er, bmax, substrate_h, con_sep, freq, design_freq, Rs_at_fd, Lambda_fd, xm, ym, psx, psy, csx, csy, artwork_cor, miter_fraction, widthZ0, lengthxm, lengthym, Manh_length, Manh_width, conductivity, loss_tangent, metal_thickness, surface_roughness : Long_Float;
cwidthxZ02, cwidthyZ02, pwidthxZ02, pwidthyZ02, message_color, key_i, key_end, xi, xii, yi, yii, window_number, ptmax, spx, spy, spp, displayo, display, Art_Form, idb, iv, xpt, npts, cx, cx3, min_ports, imin, OrigMode, GraphDriver, GraphMode : integer;
blackwhite : boolean;
read_kbd, board_read, insert_key, step_fn, stripline, update_key, spline_in_rect, spline_in_smith, Laser_Art, filled_OK, remain, admit_chart, circuit_changed, board_changed, marker_OK, p_labels, bad_compt, action, demo_mode, help_displayed, port_dirn_used, Extra_Parts_Used, Large_Parts, Large_Smith, Alt_Sweep, Manhattan_Board, missing_part : boolean;
Points_compt, rho_fac_compt, part_start, coord_start, board_start, ccompt, compt1, compt3, dBmax_ptr, fmin_ptr : compt;
netK, netL, cnet, net_start : net;

procedure puff_draw (x1, y1, x2, y2, color : integer);

procedure Draw_Box (xs, ys, xm, ym, color : integer);

procedure Make_Text_Border (x1, y1, x2, y2, colour : integer; single : boolean);

procedure fill_box (x1, y1, x2, y2, color : integer);

procedure clear_window (x1, y1, x2, y2 : integer);

procedure clear_window_gfx (x1, y1, x2, y2 : integer);

procedure pattern (x1, y1, ij, pij : integer);

procedure box (x, y, ij : integer);

procedure write_compt (color : integer; tcompt : compt);

procedure write_comptm (m, color : integer; tcompt : compt);

procedure beep;

procedure rcdelay (j : integer);

procedure write_message;

procedure erase_message;

procedure write_error (time : in Long_Float);

function input_string (mess1, mess2 : in line_string) return file_string;

procedure dirn_xy;

procedure increment_pos (i : integer);

procedure lengthxy (tnet : net);

function betweenr (x1, x2, x3, sigma : in Long_Float) return boolean;

function betweeni (x1, x2, x3 : integer) return boolean;
--  ***** These were sine_asm, cosine_asm, ln_asm ****
--  function Sin_387(theta_in : extended) :extended;
--  function Cos_387(theta_in : extended) : extended;
--  function Ln_387(arg_in : extended) : extended;
--  ************* removed for 5.5 testing ****************
--  * Complex Arithmetic *

function ext_port (tcon : conn) return boolean;

procedure prp (vu : in out TComplex; vX, vY : in Tcomplex);

procedure supr (vu : in out TComplex; vX, vY : in TComplex);

procedure co (co : in out TComplex; s, t : in Long_Float);

procedure di (di : in out TComplex; s, t : in Tcomplex);

procedure su (su : in out TComplex; s, t : in Tcomplex);

procedure rc (rc : in out TComplex; z : in Tcomplex);

procedure sm (sm : in out TComplex; s : in Long_Float; t : in Tcomplex);
--  * Parsing utilities *

function co_mag (z : in Tcomplex) return Long_Float;

function Eng_Prefix (c : Character) return Long_Float;

function Manhattan (tcompt : compt) return boolean;

function super_line (tcompt : compt) return boolean;

function goto_numeral (n : integer; x : line_string) return integer;

function Get_Real (tcompt : compt; n : integer) return Long_Float;

procedure Get_Param (tcompt : compt; n : integer; value : in out Long_Float; value_string : in out line_string; u1, prefix : in out Character; alt_param : in out boolean);

procedure Get_Lumped_Params (tcompt : compt; v1, v2, v3, v4 : in out Long_Float; u, last_ID, last_prefix : in out Character; alt_param, parallel_cir : in out boolean);

function arctanh (x : in Long_Float) return Long_Float;

function kkk (x : in Long_Float) return Long_Float;
--  width in mm

function widtht (zed : in Long_Float) return Long_Float;

function sinh (x : in Long_Float) return Long_Float;
--  * function cl_cosh(x : double) : double; *

function cosh (x : in Long_Float) return Long_Float;

function arccosh (x : in Long_Float) return Long_Float;

procedure error (g, wo, ceven : in Long_Float; fg, dfg : in out Long_Float);

procedure w_s_stripline_cline (zede, zedo : in Long_Float; woh, soh : in out Long_Float);

procedure w_s_microstrip_cline (we, wo : in Long_Float; woh, soh : in out Long_Float);

procedure shutdown;

function fileexists (note : boolean; inf : in out textfile; fname : file_string) return boolean;

function setupexists (fname : in out file_string) return boolean;

function atan2 (x, y : in Long_Float) return Long_Float;

function enough_space (defaultdrive : integer) return boolean;

function node_number return integer;

procedure update_key_list (nn : integer);

procedure Carriage_Return;

procedure move_cursor (x1, y1 : integer);

function tanh (x : in Long_Float) return Long_Float;

function KoK (k : in Long_Float) return Long_Float;

procedure capac (W_h, S_h, er : in Long_Float; ce, co : in out Long_Float);

procedure ere_even_odd (W_h, S_h : in Long_Float; ee, eo : in out Long_Float);
--  * Uses internal Procedures:
--  LU_Decomp();
--  LU_Sub();
--  Matrix_Inversion();
--  Matrix_Mux();
--  Matrix_Conv();
--  *

procedure Indef_Matrix (S : in out s_conv_matrix; n : integer);

function HeapFunc (Size : Unsigned_16) return integer;
--  ******************* Memory management ****************************

function No_mem_left return boolean;

procedure Init_Mem;

function Mem_Left return Integer_32;

procedure New_c (P : in out PMemComplex);

procedure New_s (P : in out s_param);

procedure New_plot (P : in out plot_param);

procedure New_spline (P : in out spline_param);

procedure New_n (P : in out net);

procedure New_conn (P : in out conn);

procedure New_compt (P : in out compt);

procedure Init_Marker (P : in out marker);

function Marked (P : in out marker) return BOOLEAN;

procedure Mark_Mem (P : in out marker);

procedure Release_Mem (P : in out marker);
--  -----------------------------

procedure Copy_Networks (NetStart, NetEnd : marker; CopyNetStart : in out marker);

procedure SetCol (col : Unsigned_16);

procedure TextCol (col : Unsigned_16);
end pfun1;

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
--  ******************** Graphics Procedures and Functions ************
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

package body pfun1 is
   --  [P2Ada]: This is for 'Write([Boolean])'

   package Boolean_Text_IO is new Enumeration_IO (Boolean);
   use Boolean_Text_IO;
   --  [P2Ada]: This is for 'file' without type
   --  [P2Ada]: This is for the Halt pseudo-procedure

   package Byte_Direct_IO is new Ada.Direct_IO (Unsigned_8);
   Program_halted : exception;
   --  *
   --  Line drawing routine.
   --  Only used by draw_ticks,
   --  Draw_Smith, and draw_to_port.
   --  *

   procedure puff_draw (x1, y1, x2, y2, color : integer) is
   begin
      SetCol (color);
      Line (x1, y1, x2, y2);
   end puff_draw;
   --  * puff_draw *
   --  *
   --  This procedure reduced using new graphics.
   --  *

   procedure Draw_Box (xs, ys, xm, ym, color : integer) is
   begin
      SetCol (color);
      Rectangle (xs, ys, xm, ym);
   end Draw_Box;
   --  draw_box
   --  *
   --  This procedure has been drastically reduced using
   --  Turbo Pascal graphics.
   --  Used for erasing sections of the screen (color=brown)
   --  and for drawing	tlines (color=white).
   --  *

   procedure fill_box (x1, y1, x2, y2, color : integer) is
   begin
      if (blackwhite)
      then
         SetFillStyle (SolidFill, white);
      else
         SetFillStyle (SolidFill, Color);
      end if;
      Bar (x1, y1, x2, y2);
   end fill_box;
   --  fill_box
   --  *
   --  Clear region of screen. Text coordinates are the input.
   --  Erasing was done in textmode, now in graphics mode.
   --  *

   procedure clear_window (x1, y1, x2, y2 : integer) is
      --  Window(x1,y1,x2,y2);   {specify area as window}
      --  TextCol(black);      {Here TextColor behaves as background}
      --  ClrScr;     {clear to background color}
      --  return to default window
   begin
      Window (1, 1, Max_Text_X, Max_Text_Y);
      SetFillStyle (SolidFill, Black);
      Bar (8 * (x1 - 1), 14 * (y1 - 1), 8 * x2, 14 * y2);
   end clear_window;
   --  clear_window
   --  *
   --  Clear region of screen. Graphics coordinates are the input.
   --  Erasing was done in textmode, now in graphics mode.
   --  *

   procedure clear_window_gfx (x1, y1, x2, y2 : integer) is
      --  return to default window
   begin
      Window (1, 1, Max_Text_X, Max_Text_Y);
      SetFillStyle (SolidFill, Black);
      Bar (x1, y1, x2, y2);
   end clear_window_gfx;
   --  clear_window_gfx
   --  *
   --  Creates text border pattern for start screen and other windows
   --  *

   procedure Make_Text_Border (x1, y1, x2, y2, colour : integer; single : boolean) is
      --  side bars
      --  corner bars
      sing_vert : constant String := Character'Val (179);
      doub_vert : constant String := Character'Val (186);
      sing_horz : constant String := Character'Val (196);
      doub_horz : constant String := Character'Val (205);
      sing_UL : constant String := Character'Val (213);
      doub_UL : constant String := Character'Val (201);
      sing_UR : constant String := Character'Val (184);
      doub_UR : constant String := Character'Val (187);
      sing_LL : constant String := Character'Val (212);
      doub_LL : constant String := Character'Val (200);
      sing_LR : constant String := Character'Val (190);
      doub_LR : constant String := Character'Val (188);
      vert, horz, UL, UR, LL, LR : Character;
      i : integer;
   begin
      if single
      then
         vert := sing_vert;
         horz := doub_horz;
         UL := sing_UL;
         UR := sing_UR;
         LL := sing_LL;
         LR := sing_LR;
      else
         vert := doub_vert;
         horz := doub_horz;
         UL := doub_UL;
         UR := doub_UR;
         LL := doub_LL;
         LR := doub_LR;
      end if;
      --  clear area
      clear_window (x1, y1, x2, y2);
      TextCol (colour);
      for i in y1 .. y2
      loop
         --  * Draw Border for startup screen *
         GotoXY (x1, i);
         Put (vert);
         GotoXY (x2, i);
         Put (vert);
      end loop;
      for i in x1 .. x2
      loop
         GotoXY (i, y1);
         Put (horz);
         GotoXY (i, y2);
         Put (horz);
      end loop;
      GotoXY (x1, y1);
      Put (UL);
      GotoXY (x2, y1);
      Put (UR);
      GotoXY (x1, y2);
      Put (LL);
      GotoXY (x2, y2);
      Put (LR);
   end Make_Text_Border;
   --  * Make_Text_Border *
   --  *
   --  Draw marker pattern for s-parameter plots
   --  ij=1 box, ij=2 X, ij=3 diamond, ij=4 +
   --  *

   procedure pattern (x1, y1, ij, pij : integer) is
   begin
      SetCol (s_color (ij));
      case ij is
         when 1 =>
            --  draw the box
            Rectangle (x1 - 3, y1 - 3, x1 + 3, y1 + 3);
         when 2 =>
            --  draw X
            Line (x1 - 3, y1 - 3, x1 + 3, y1 + 3);
            Line (x1 - 3, y1 + 3, x1 + 3, y1 - 3);
         when 3 =>
            --  draw diamond
            Line (x1 - 4, y1, x1, y1 + 4);
            Line (x1 - 3, y1 - 1, x1, y1 - 4);
            Line (x1 + 4, y1, x1 + 1, y1 + 3);
            Line (x1 + 3, y1 - 1, x1 + 1, y1 - 3);
         when 4 =>
            --  draw +
            Line (x1 - 4, y1, x1 + 4, y1);
            Line (x1, y1 + 4, x1, y1 - 4);
         when others =>
            --  [P2Ada]: no otherwise / else in Pascal
            null;
      end case;
      --  case
   end pattern;
   --  pattern
   --  *
   --  Draw small box to indicate where s-parameters are calculated.
   --  *

   procedure box (x, y, ij : integer) is
   begin
      case ij is
         when 2 =>
            y := y - 1;
            x := x - 1;
         when 3 =>
            x := x - 1;
         when 4 =>
            y := y - 1;
         when others =>
            --  [P2Ada]: no otherwise / else in Pascal
            null;
      end case;
      --  case
      PutPixel (x, y, s_color (ij));
      PutPixel (x + 1, y, s_color (ij));
      PutPixel (x, y + 1, s_color (ij));
      PutPixel (x + 1, y + 1, s_color (ij));
   end box;
   --  box
   --  *
   --  Display a component -- highlighted
   --  *

   procedure write_compt (color : integer; tcompt : compt) is
      --  [P2Ada]: WITH instruction
   begin
      TextCol (color);
      declare P2Ada_Var_1 : compt_record renames tcompt.all;
      begin
         gotoxy (P2Ada_Var_1.xp, P2Ada_Var_1.yp);
         Put (P2Ada_Var_1.descript);
      end;
      --  [P2Ada]: end of WITH
   end write_compt;
   --  * write_compt *
   --  *
   --  Display the first m characters of a component.
   --  *

   procedure write_comptm (m, color : integer; tcompt : compt) is
      --  [P2Ada]: WITH instruction
      i : integer;
   begin
      TextCol (color);
      declare P2Ada_Var_2 : compt_record renames tcompt.all;
      begin
         gotoxy (P2Ada_Var_2.xp, P2Ada_Var_2.yp);
         for i in 1 .. m
         loop
            Put (P2Ada_Var_2.descript (i));
         end loop;
      end;
      --  [P2Ada]: end of WITH
   end write_comptm;
   --  * write_comptm *
   --  *
   --  Make the Puff tone.
   --  *

   procedure beep is
   begin
      Sound (250);
      P2Ada_no_keyword_Delay (50);
      Nosound;
   end beep;
   --  *
   --  Interuptable delay for use in demo mode.
   --  *

   procedure rcdelay (j : integer) is
      i : integer;
   begin
      for i in 1 .. j
      loop
         if keypressed
         then
            chs := Readkey;
            if chs = 'S'
            then
               beep;
               textmode (co80);
               raise Program_halted (1);
            else
               P2Ada_no_keyword_delay (2000);
            end if;
         else
            P2Ada_no_keyword_delay (10);
         end if;
         --  if keypressed
      end loop;
      --  for i:= 1 to j
   end rcdelay;
   --  rcdelay
   --  *
   --  Write message in center box.
   --  *

   procedure write_message is
   begin
      TextCol (message_color);
      Gotoxy ((xmin (6) + xmax (6) - (message (1) 'l' ength)) / 2, ymin (6));
      Put (message (1));
      Gotoxy ((xmin (6) + xmax (6) - (message (2) 'l' ength)) / 2, ymin (6) + 1);
      Put (message (2));
      Gotoxy ((xmin (6) + xmax (6) - (message (3) 'l' ength)) / 2, ymin (6) + 2);
      Put (message (3));
      if (message (1) + message (2) + message (3) /= "") and then read_kbd
      then
         beep;
      end if;
      if demo_mode
      then
         rcdelay (50);
      end if;
   end write_message;
   --  write_message
   --  *
   --  Erase message in center box.
   --  *

   procedure Erase_message is
      --  set up window
   begin
      clear_window (xmin (6), ymin (6), xmax (6), ymax (6));
      Message (1) := "";
      Message (2) := "";
      Message (3) := "";
   end Erase_message;
   --  Erase_message
   --  *
   --  Flash error message in message window.
   --  *

   procedure write_error (time : in Long_Float) is
   begin
      write_message;
      P2Ada_no_keyword_delay (Round (1000 * time));
      erase_message;
   end write_error;
   --  write_error
   --  *
   --  Prompt user to input string (filename).
   --  The escape key cannot be used to exit here.
   --  *

   function input_string (mess1, mess2 : in line_string) return file_string is
      --  set up window for oversized text
      Result_input_string : file_string;
      answer : file_string;
   begin
      Erase_message;
      Window (xmin (6), ymin (6), xmax (6), ymax (6));
      TextCol (Yellow);
      Put (mess1);
      New_Line;
      Put (mess2);
      New_Line;
      Put ('?');
      Get (answer);
      Skip_Line;
      Result_input_string := answer;
      Erase_message;
      return Result_input_string;
   end input_string;
   --  input_string
   --  *
   --  Checks cursor direction.
   --  Maps dirn into x and y changes.
   --  *

   procedure dirn_xy is
   begin
      xii := 0;
      yii := 0;
      case dirn is
         when 2 =>
            --  East
            xii := 1;
         when 4 =>
            --  West
            xii := - 1;
         when 8 =>
            --  South
            yii := 1;
         when 1 =>
            --  North
            yii := - 1;
         when others =>
            --  [P2Ada]: no otherwise / else in Pascal
            null;
      end case;
   end dirn_xy;
   --  dirn_xy
   --  *
   --  Increment postion of cursor on circuit.
   --  *

   procedure increment_pos (i : integer) is
   begin
      dirn_xy;
      if ((i) mod 2 /= 0)
      then
         if i = - 1
         then
            --  step 1/2 of compt
            --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
            xm := xm + (compt1.all.lngth * xii + yii * compt1.all.con_space) / 2.0;
            ym := ym + (compt1.all.lngth * yii + xii * compt1.all.con_space) / 2.0;
         else
            if (compt1.all.typ and ('i' | 'd' => True, others => False))
            then
               if compt1.all.number_of_con /= 1
               then
                  xm := xm + lengthxm * xii / (compt1.all.number_of_con - 1);
                  ym := ym + lengthym * yii / (compt1.all.number_of_con - 1);
               end if;
            else
               xm := xm + lengthxm * xii;
               ym := ym + lengthym * yii;
            end if;
         end if;
         --  if odd i
      else
         if i = 0
         then
            --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
            xm := cnet.all.xr;
            ym := cnet.all.yr;
         else
            if (compt1.all.typ and ('i' | 'd' => True, others => False))
            then
               xm := xm + lengthxm * xii / (compt1.all.number_of_con - 1);
               ym := ym + lengthym * yii / (compt1.all.number_of_con - 1);
            else
               xm := xm + lengthxm * xii - compt1.all.con_space * yii;
               ym := ym + lengthym * yii - compt1.all.con_space * xii;
            end if;
         end if;
         --  if i else
      end if;
      --  if odd else
      --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
      xi := Round (xm / csx);
      yi := Round (ym / csy);
      if (not (compt1.all.typ and ('i' | 'd' => True, others => False))) or else (i <= 0) or else (i = (compt1.all.number_of_con - 1))
      then
         case dirn is
            when 1 =>
               dirn := 8;
            when 2 =>
               dirn := 4;
            when 4 =>
               dirn := 2;
            when 8 =>
               dirn := 1;
            when others =>
               --  [P2Ada]: no otherwise / else in Pascal
               null;
         end case;
      end if;
      --  case
   end increment_pos;
   --  increment pos
   --  *
   --  Convert part lengths and widths to increments
   --  in the x and y directions.
   --  *

   procedure lengthxy (tnet : net) is
      lengths, widths : Long_Float;
   begin
      dirn_xy;
      if tnet /= null
      then
         --  [P2Ada]: !Help! Maybe (file,...) here
         --  [P2Ada]: !Help! Maybe (file)
         lengths := tnet.all.com.all.lngth;
         widths := tnet.all.com.all.width;
      else
         Put (lst);
         Put ("error");
         New_Line;
      end if;
      lengthxm := lengths * abs (xii) + widths * abs (yii);
      lengthym := lengths * abs (yii) + widths * abs (xii);
   end lengthxy;
   --  lengthxy
   --  *
   --  True if real x2 is between x1-sigma and x3+sigma.
   --  *

   function betweenr (x1, x2, x3, sigma : in Long_Float) return boolean is
      Result_betweenr : boolean;
   begin
      if x1 > x3
      then
         if (x3 - sigma <= x2) and then (x2 <= x1 + sigma)
         then
            Result_betweenr := true;
         else
            Result_betweenr := false;
         end if;
      else
         if (x1 - sigma <= x2) and then (x2 <= x3 + sigma)
         then
            Result_betweenr := true;
         else
            Result_betweenr := false;
         end if;
      end if;
      return Result_betweenr;
   end betweenr;
   --  *
   --  Check to see that x2 is between x1 and x3
   --  *

   function betweeni (x1, x2, x3 : integer) return boolean is
      Result_betweeni : boolean;
   begin
      if (x1 <= x2) and then (x2 <= x3)
      then
         Result_betweeni := true;
      else
         Result_betweeni := false;
      end if;
      return Result_betweeni;
   end betweeni;
   --  *
   --  Check to see if a connector is joined to an external port.
   --  *

   function ext_port (tcon : conn) return boolean is
      Result_ext_port : boolean;
   begin
      Result_ext_port := betweeni (1, tcon.all.port_type, min_ports);
      return Result_ext_port;
   end ext_port;
   --  ******************** Complex Number Utilities ***********************
   --  *
   --  Complex product.
   --  Multiply two complex numbers.
   --  *

   procedure prp (vu : in out TComplex; vX, vY : in TComplex) is
   begin
      vu.r := vX.r * vY.r - vX.i * vY.i;
      vu.i := vX.r * vY.i + vX.i * vY.r;
   end prp;
   --  *
   --  Complex sum and product.
   --  Multiply two complex numbers and add.
   --  vu = vu + vx*vy
   --  *

   procedure supr (vu : in out TComplex; vX, vY : in TComplex) is
   begin
      vu.r := vu.r + vX.r * vY.r - vX.i * vY.i;
      vu.i := vu.i + vX.r * vY.i + vX.i * vY.r;
   end supr;
   --  *
   --  Complex difference and product.
   --  Multiply two complex numbers and subtract.
   --  vu = vu - vX*vY
   --  *

   procedure diffpr (vu : in out TComplex; vX, vY : in TComplex) is
   begin
      vu.r := vu.r - vX.r * vY.r + vX.i * vY.i;
      vu.i := vu.i - vX.r * vY.i - vX.i * vY.r;
   end diffpr;
   --  *
   --  Create a complex number type.
   --  *

   procedure co (co : in out TComplex; s, t : in Long_Float) is
   begin
      co.r := s;
      co.i := t;
   end co;
   --  co
   --  *
   --  Calculate difference of two complex numbers (s - t).
   --  *

   procedure di (di : in out TComplex; s, t : in TComplex) is
   begin
      di.r := s.r - t.r;
      di.i := s.i - t.i;
   end di;
   --  di
   --  *
   --  Calculate sum of two complex numbers (s + t).
   --  *

   procedure su (su : in out TComplex; s, t : in TComplex) is
   begin
      su.r := s.r + t.r;
      su.i := s.i + t.i;
   end su;
   --  s+t
   --  *
   --  Calculate the reciprocal of a complex number (1/z).
   --  *

   procedure rc (rc : in out TComplex; z : in TComplex) is
      --  ! was real, changed 10/15/90
      --  !* check for 1/0 added here *
      mag : Long_Float;
   begin
      mag := ((z.r) ** 2) + ((z.i) ** 2);
      if (mag = 0.0)
      then
         --  Although this is equivalent to saying 1/0 = 0
         --  it works properly for the few times it occurs
         rc.r := 0.0;
         rc.i := 0.0;
      else
         rc.r := z.r / mag;
         rc.i := - z.i / mag;
      end if;
   end rc;
   --  * 1/z *
   --  *
   --  Scale magnitude of a complex number s*t.
   --  *

   procedure sm (sm : in out TComplex; s : in Long_Float; t : in TComplex) is
   begin
      sm.r := s * t.r;
      sm.i := s * t.i;
   end sm;
   --  sm or s*t
   --  *
   --  Compute magnitude of a complex number.
   --  Used for pivoting in matrix inversion routines.
   --  *

   function co_mag (z : in TComplex) return Long_Float is
      Result_co_mag : Long_Float;
   begin
      Result_co_mag := sqrt (((z.r) ** 2) + ((z.i) ** 2));
      return Result_co_mag;
   end co_mag;
   --  * co_mag *
   --  *
   --  sets z1 := z2;
   --  *

   procedure Equate_Zs (z1 : in out TComplex; z2 : in Tcomplex) is
   begin
      z1.r := z2.r;
      z1.i := z2.i;
   end Equate_Zs;
   --  *********************** Parsing Utilities ***********************
   --  *
   --  Find multiplication factor for engineering prefixes
   --  *

   function Eng_Prefix (c : Character) return Long_Float is
      Result_Eng_Prefix : Long_Float;
   begin
      case c is
         when 'E' =>
            Result_Eng_Prefix := 1.0e + 18;
         when 'P' =>
            Result_Eng_Prefix := 1.0e + 15;
         when 'T' =>
            Result_Eng_Prefix := 1.0e + 12;
         when 'G' =>
            Result_Eng_Prefix := 1.0e + 09;
         when 'M' =>
            Result_Eng_Prefix := 1.0e + 06;
         when 'k' =>
            Result_Eng_Prefix := 1.0e + 03;
         when 'm' =>
            Result_Eng_Prefix := 1.0e - 03;
         when Mu =>
            Result_Eng_Prefix := 1.0e - 06;
         when 'n' =>
            Result_Eng_Prefix := 1.0e - 09;
         when 'p' =>
            Result_Eng_Prefix := 1.0e - 12;
         when 'f' =>
            Result_Eng_Prefix := 1.0e - 15;
         when 'a' =>
            Result_Eng_Prefix := 1.0e - 18;
         when others =>
            Result_Eng_Prefix := 1.0;
      end case;
      --  case
      return Result_Eng_Prefix;
   end Eng_Prefix;
   --  * Eng_Prefix *
   --  *
   --  Determine if Manhattan layout has been selected.
   --  Looks for 'M' at the end of tcompt^.descript
   --  or a '?' anywhere in the description.
   --  *

   function Manhattan (tcompt : compt) return boolean is
      Result_Manhattan : boolean;
      c_string : line_string;
      long : integer;
   begin
      if Manhattan_Board
      then
         Result_Manhattan := true;
      else
         c_string := tcompt.all.descript;
         long := (c_string'length);
         while c_string (long) = ' '
         loop
            long := long - 1;
         end loop;
         --  ignore end blanks
         --  * Select Manhattan if last character is an 'M' *
         if c_string (long) = 'M'
         then
            Result_Manhattan := true;
         else
            Result_Manhattan := false;
         end if;
         --  * Select Manhattan if a '?' is present in clines or tline *
         while (long > 0)
         loop
            --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
            if (c_string (long) = '?') and then (tcompt.all.typ and ('c' | 't' => True, others => False))
            then
               Result_Manhattan := true;
            end if;
            long := long - 1;
         end loop;
      end if;
      --  else
      return Result_Manhattan;
   end Manhattan;
   --  * Manhattan *
   --  *
   --  Determine if super line has been selected.
   --  Looks for '!' anywhere in tcompt^.descript.
   --  *

   function super_line (tcompt : compt) return boolean is
      --  * super line if a '!' is present in clines or tline *
      Result_super_line : boolean;
      c_string : line_string;
      long : integer;
   begin
      Result_super_line := false;
      c_string := tcompt.all.descript;
      long := (c_string'length);
      while (long > 0)
      loop
         --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
         if (c_string (long) = '!') and then (tcompt.all.typ and ('c' | 't' => True, others => False))
         then
            Result_super_line := true;
         end if;
         long := long - 1;
      end loop;
      return Result_super_line;
   end super_line;
   --  * super_line *
   --  *
   --  Find location of nth number in x.
   --  Used by tlineO, clineO, get_real, get_param
   --  Will also return location of '?'.
   --  *

   function goto_numeral (n : integer; x : line_string) return integer is
      Result_goto_numeral : integer;
      long, i, j : integer;
      found : boolean;
   begin
      i := 0;
      found := false;
      Result_goto_numeral := 0;
      j := 1;
      long := (x'length);
      if long > 0
      then
         loop
            if x (j) = '('
            then
               j := Pos (')', x);
            end if;
            --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
            if x (j) and ('?' | '+' | '-' | '.' | ',' | '0' .. '9' => True, others => False)
            then
               i := i + 1;
               if i = n
               then
                  found := true;
               else
                  loop
                     --  step over number to find next number
                     --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
                     j := j + 1;
                     exit when not (x (j) and ('?' | '-' | '.' | ',' | '0' .. '9' => True, others => False)) or else (j = long + 1);
                  end loop;
               end if;
            else
               j := j + 1;
            end if;
            exit when found or else (j = long + 1);
         end loop;
      end if;
      if found
      then
         Result_goto_numeral := j;
      else
         bad_compt := true;
         message (2) := "Number is missing";
      end if;
      return Result_goto_numeral;
   end goto_numeral;
   --  * goto_numeral *
   --  *
   --  Read nth number in tcompt.
   --  Called by get_coordsO, get_s_and_fO, Read_Net
   --  *

   function Get_Real (tcompt : compt; n : integer) return Long_Float is
      Result_Get_Real : Long_Float;
      c_string, s_value : line_string;
      j, code, long : integer;
      value : Long_Float;
      found : boolean;
   begin
      c_string := tcompt.all.descript;
      j := goto_numeral (n, c_string);
      if bad_compt
      then
         ccompt := tcompt;
         return Result_Get_Real;
      end if;
      s_value := "";
      long := (c_string'length);
      found := false;
      loop
         --  '+',
         --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
         if c_string (j) and ('-' | '.' | ',' | '0' .. '9' => True, others => False)
         then
            if c_string (j) = ','
            then
               s_value := s_value + '.';
            else
               s_value := s_value + c_string (j);
            end if;
            j := j + 1;
         else
            found := true;
         end if;
         exit when (found or else (j = long + 1));
      end loop;
      Val (s_value, value, code);
      if (code /= 0) or else ((s_value'length) = 0)
      then
         ccompt := tcompt;
         bad_compt := true;
         message (2) := "Invalid number";
         return Result_Get_Real;
      end if;
      get_real := value;
      return Result_Get_Real;
   end Get_Real;
   --  * Get_Real *
   --  *
   --  Get nth parameter in tcompt.
   --  Used for parsing tlines, clines, qlines, BOARD, etc.
   --  Ignores '+' signs.
   --  Called by tlineO, clineO, Atten, Transformer,etc.
   --  *

   procedure Get_Param (tcompt : compt; n : integer; value : in out Long_Float; value_string : in out line_string; u1, prefix : in out Character; alt_param : in out boolean) is
      potential_units : array (Character) of Boolean := (degree | Omega | 'm' | 'h' | 'H' | 's' | 'S' | 'z' | 'Z' | 'y' | 'Y' => True, others => False);
      potential_numbers : array (Character) of Boolean := ('+' | '-' | '.' | ',' | '0' .. '9' => True, others => False);
      c_string, s_value : line_string;
      i, j, code, long : integer;
      found_value : boolean;
   begin
      alt_param := false;
      c_string := tcompt.all.descript;
      j := goto_numeral (n, c_string);
      if bad_compt
      then
         ccompt := tcompt;
         return;
      end if;
      long := (c_string'length);
      while c_string (long) = ' '
      loop
         long := long - 1;
      end loop;
      --  ignore end spaces
      if j > 0
      then
         found_value := false;
         s_value := "";
         loop
            --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
            if c_string (j) and potential_numbers
            then
               if not (c_string (j) = '+')
               then
                  --  force a skip over '+' signs
                  if c_string (j) = ','
                  then
                     --  sub '.' for ','
                     s_value := s_value + '.';
                  else
                     s_value := s_value + c_string (j);
                  end if;
               end if;
               --  '+' sign check
               j := j + 1;
            else
               if c_string (j) = '?'
               then
                  --  Check here for variable
                  alt_param := true;
                  j := j + 1;
               else
                  found_value := true;
               end if;
            end if;
            exit when (found_value or else (j = long + 1));
         end loop;
         Val (s_value, value, code);
         if (code /= 0) or else ((s_value'length) = 0)
         then
            if (alt_param = true)
            then
               --  return these for uninitialized variables
               value := 1.0;
               value_string := "1.0";
            else
               ccompt := tcompt;
               bad_compt := true;
               message (2) := "Invalid number";
               return;
            end if;
         else
            value_string := s_value;
         end if;
      end if;
      while (c_string (j) = ' ') and then (j < long + 1)
      loop
         j := j + 1;
      end loop;
      --  Skip spaces
      --  initialize prefix to blank
      --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
      prefix := ' ';
      if (c_string (j) and Eng_Dec_Mux) and then (j <= long)
      then
         if c_string (j) = 'm'
         then
            --  is 'm' a unit or prefix?
            i := j + 1;
            while (c_string (i) = ' ') and then (i < long + 1)
            loop
               i := i + 1;
            end loop;
            --  * Skip spaces to check for some unit *
            --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
            if (c_string (i) and potential_units)
            then
               --  it's the prefix milli 'm' next to a unit
               --  make j point past the prefix, to the unit
               prefix := 'm';
               value := Eng_Prefix ('m') * value;
               j := i;
            end if;
            --  if 'm' is a unit do nothing
            --  if other than 'm' factor in prefix
            --  advance from prefix toward unit
         else
            prefix := c_string (j);
            value := Eng_Prefix (c_string (j)) * value;
            j := j + 1;
         end if;
      end if;
      while (c_string (j) = ' ') and then (j <= long)
      loop
         j := j + 1;
      end loop;
      --  Skip spaces
      if j <= long
      then
         u1 := c_string (j);
      else
         u1 := '?';
      end if;
      if u1 = 'm'
      then
         value := 1000 * value;
      end if;
      --  return millimeters, not meters
   end Get_Param;
   --  * Get_Param *
   --  *
   --  Get paramters for a lumped element.
   --  example tcompt^.descript := 'a lumped 50-j10+j10'#139' 4mm'
   --  ohms
   --  Currently, only a single alt_parameter may be
   --  passed to LumpedO, (either v1,v2,v3, or v4)
   --  otherwise, an error will occur (in LumpedO).
   --  *

   procedure Get_Lumped_Params (tcompt : compt; v1, v2, v3, v4 : in out Long_Float; u, last_ID, last_prefix : in out Character; alt_param, parallel_cir : in out boolean) is
      --  [BP2P]: Label "100001" Was "exit_get_lumped"
      --  ***************************************************
      c_string, s_value : line_string;
      i, j, code, long, sign : integer;
      value, L_value, temp_val, C_value, omega0 : Long_Float;
      found, par_error : boolean;
      ident, scale_char : Character;
      --  *
      --  Skip one or more spaces to advance to next
      --  legitimate data or unit value.
      --  *

      procedure skip_space is
      begin
         loop
            --  advance past spaces
            j := j + 1;
            exit when (c_string (j) /= ' ') or else (j = long + 1);
         end loop;
      end skip_space;
      --  ****************************************************
      --  * Get_lumped_params *
      --  convert design Freq to rad/sec times prefix
      --  look past id letter
   begin
      v1 := 0;
      v2 := 0;
      v3 := 0;
      v4 := 0;
      u := '?';
      last_ID := ' ';
      last_prefix := ' ';
      L_value := 0;
      C_value := 0;
      par_error := false;
      parallel_cir := false;
      alt_param := false;
      omega0 := 2 * Pi * design_freq * Eng_Prefix (freq_prefix);
      c_string := tcompt.all.descript;
      j := 2;
      if bad_compt
      then
         return;
      end if;
      long := (c_string'length);
      while c_string (long) = ' '
      loop
         long := long - 1;
      end loop;
      --  ignore end blanks
      if Pos (Parallel, c_string) > 0
      then
         parallel_cir := true;
      end if;
      --  * Look for character which represents a parallel circuit *
      for i in 1 .. 4
      loop
         s_value := "";
         scale_char := ' ';
         found := false;
         if j > long
         then
            --  [BP2P]: Label "100001" Was "exit_get_lumped"
            goto LABEL_100001;
         end if;
         --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
         while not (c_string (j) and ('?' | '+' | '-' | '.' | ',' | '0' .. '9' | 'j' => True, others => False))
         loop
            --  Advance characters until legitimate data found
            j := j + 1;
            if j > long
            then
               --  [BP2P]: Label "100001" Was "exit_get_lumped"
               goto LABEL_100001;
            end if;
         end loop;
         if c_string (j) = '+'
         then
            skip_space;
         end if;
         if c_string (j) = '-'
         then
            skip_space;
            sign := - 1;
         else
            sign := 1;
         end if;
         if c_string (j) = 'j'
         then
            skip_space;
            ident := 'j';
         else
            ident := ' ';
         end if;
         --  Check for sweep variable
         if c_string (j) = '?'
         then
            if alt_param = false
            then
               --  [BP2P]: Label "100001" Was "exit_get_lumped"
               alt_param := true;
               skip_space;
            else
               par_error := true;
               goto LABEL_100001;
            end if;
         end if;
         --  * Load string with number characters *
         loop
            --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
            if c_string (j) and ('.' | ',' | '0' .. '9' => True, others => False)
            then
               if c_string (j) = ','
               then
                  s_value := s_value + '.';
               else
                  s_value := s_value + c_string (j);
               end if;
               j := j + 1;
            else
               found := true;
            end if;
            exit when (found or else (j = long + 1));
         end loop;
         if (c_string (j) = ' ')
         then
            skip_space;
         end if;
         --  * Look for engineering prefixes *
         --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
         if (c_string (j) and Eng_Dec_Mux) and then (j < long)
         then
            --  * ignore 'm' if last character *
            scale_char := c_string (j);
            skip_space;
         end if;
         --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
         if (c_string (j) and ('j' | 'm' | 'H' | 'F' => True, others => False)) and then (j /= long + 1)
         then
            ident := c_string (j);
            skip_space;
         end if;
         if j <= long
         then
            --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
            if c_string (j) and ('y' | 'Y' | 'z' | 'Z' | 's' | 'S' | Omega => True, others => False)
            then
               u := c_string (j);
               skip_space;
            end if;
         end if;
         Val (s_value, value, code);
         if (code /= 0) or else ((s_value'length) = 0)
         then
            if (alt_param = true) and then (ident /= 'm')
            then
               --  return 1.0 for uninitialized variables
               --  [BP2P]: Label "100001" Was "exit_get_lumped"
               value := 1.0;
            else
               par_error := true;
               goto LABEL_100001;
            end if;
         end if;
         value := value * sign * Eng_Prefix (scale_char);
         case ident is
            when 'F' =>
               if C_value = 0
               then
                  C_value := value;
                  if C_value = 0
                  then
                     C_value := 1.0 / infty;
                  end if;
                  --  watch for zero capacitance
                  --  [BP2P]: Label "100001" Was "exit_get_lumped"
               else
                  par_error := true;
                  goto LABEL_100001;
               end if;
            when 'H' =>
               if L_value = 0
               then
                  L_value := value;
                  if L_value = 0
                  then
                     L_value := 1.0 / infty;
                  end if;
                  --  watch for zero inductance
                  --  [BP2P]: Label "100001" Was "exit_get_lumped"
               else
                  par_error := true;
                  goto LABEL_100001;
               end if;
            when 'j' =>
               if value > 0
               then
                  if v2 = 0
                  then
                     --  [BP2P]: Label "100001" Was "exit_get_lumped"
                     v2 := value;
                  else
                     par_error := true;
                     goto LABEL_100001;
                  end if;
               else
                  if v3 = 0
                  then
                     --  [BP2P]: Label "100001" Was "exit_get_lumped"
                     v3 := value;
                  else
                     par_error := true;
                     goto LABEL_100001;
                  end if;
               end if;
            when 'm' =>
               --  convert from meters to mm
               --  * return v4=0 if solo m i.e. Manhattan *
               v4 := 1000 * value;
            when others =>
               if v1 = 0
               then
                  --  [BP2P]: Label "100001" Was "exit_get_lumped"
                  v1 := value;
               else
                  par_error := true;
                  goto LABEL_100001;
               end if;
         end case;
         --  case
         --  Save part ID and prefix for alt_param
         --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
         if not (ident and (' ' | 'm' => True, others => False))
         then
            last_ID := ident;
         end if;
         if (scale_char /= ' ') and then (ident /= 'm')
         then
            last_prefix := scale_char;
         end if;
      end loop;
      --  for i
      <<LABEL_100001>>
      if not (par_error)
      then
         --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
         if (u and ('z' | 'Z' | Omega => True, others => False)) and then (parallel_cir = true)
         then
            --  * Swap values if parallel circuit is desired *
            temp_val := v2;
            if v1 /= 0
            then
               v1 := 1.0 / v1;
            end if;
            if v3 /= 0
            then
               v2 := - 1.0 / v3;
            end if;
            if temp_val /= 0
            then
               v3 := - 1.0 / temp_val;
            end if;
            if u = Omega
            then
               --  *swap units too *
               u := 'S';
            else
               u := 'y';
            end if;
         end if;
         --  if u in
         --  * Add in capacitor and inductor values *
         if C_value /= 0
         then
            case u is
               when Omega =>
                  v3 := v3 - 1.0 / (omega0 * C_value);
               when 'z' | 'Z' =>
                  v3 := v3 - 1.0 / (z0 * omega0 * C_value);
               when 'y' | 'Y' =>
                  v2 := v2 + z0 * omega0 * C_value;
               when 's' | 'S' =>
                  v2 := v2 + omega0 * C_value;
               when others =>
                  --  if 'F' the only unit
                  if parallel_cir
                  then
                     u := 'S';
                     if v1 /= 0
                     then
                        v1 := 1.0 / v1;
                     end if;
                     --  assume ohms @ v1 if no units
                     v2 := v2 + omega0 * C_value;
                  else
                     u := Omega;
                     v3 := v3 - 1.0 / (omega0 * C_value);
                  end if;
            end case;
         end if;
         --  if..case
         if L_value /= 0
         then
            case u is
               when Omega =>
                  v2 := v2 + omega0 * L_value;
               when 'z' | 'Z' =>
                  v2 := v2 + omega0 * L_value / z0;
               when 'y' | 'Y' =>
                  v3 := v3 - z0 / (omega0 * L_value);
               when 's' | 'S' =>
                  v3 := v3 - 1.0 / (omega0 * L_value);
               when others =>
                  --  if 'H' the only unit
                  if parallel_cir
                  then
                     u := 'S';
                     if v1 /= 0
                     then
                        v1 := 1.0 / v1;
                     end if;
                     --  assume ohms @ v1 if no units
                     v3 := v3 - 1.0 / (omega0 * L_value);
                  else
                     u := Omega;
                     v2 := v2 + omega0 * L_value;
                  end if;
                  --  else
            end case;
         end if;
         --  if..case
      end if;
      --  not par_error
      if par_error or else (u = '?')
      then
         ccompt := tcompt;
         bad_compt := true;
         message (1) := "Error in";
         message (2) := "lumped element";
         message (3) := "description";
      end if;
   end Get_Lumped_Params;
   --  * Get_Lumped_Params *
   --  ************************** Numerical Routines *************************

   function arctanh (x : in Long_Float) return Long_Float is
      Result_arctanh : Long_Float;
   begin
      Result_arctanh := 0.5 * Log ((1 + x) / (1 - x));
      return Result_arctanh;
   end arctanh;
   --  *
   --  Function used to calculate Cohn's "k" factor for stripline
   --  width formulas.  See equation 3.6, page 13 of the Puff Manual.
   --  Used by widthO, w_s_stripline_cline.
   --  *

   function kkk (x : in Long_Float) return Long_Float is
      Result_kkk : Long_Float;
      expx : Long_Float;
   begin
      if x > 1
      then
         expx := exp (pi * x);
         Result_kkk := sqrt (1 - (((((expx - 2) / (expx + 2)) ** 2)) ** 2));
      else
         expx := exp (pi / x);
         Result_kkk := (((expx - 2) / (expx + 2)) ** 2);
      end if;
      return Result_kkk;
   end kkk;
   --  kkk
   --  width in mils
   --  *
   --  Function for calculating width in mils of microstrip
   --  and stripline transmission lines.  See Puff manual
   --  pages 12-13 for details.
   --  Microstrip models are from Owens:
   --  Radio and Elect Eng, 46, pp 360-364, 1976.
   --  Stripline models are from  Cohn:
   --  MTT-3 pp19-126, March 1955.
   --  See also Gupta, Garg, Chadha:
   --  CAD of Microwave Circuits Artech House, 1981.
   --  *

   function widtht (zed : in Long_Float) return Long_Float is
      --  ******************************************
      Result_widtht : Long_Float;
      lnpi_2 : constant := 0.451583;
      ln4_pi : constant := 0.241564;
      Hp, expH, de, x : Long_Float;

      procedure High_Z_Error is
      begin
         bad_compt := true;
         message (1) := "Impedance";
         message (2) := "too large";
         Result_widtht := 0;
      end High_Z_Error;
      --  ******************************************
   begin
      if stripline
      then
         x := zed * sqrt (er) / (30 * pi);
         if (pi * x > 87.0)
         then
            --  or else kkk(x) will explode
            High_Z_Error;
         else
            Result_widtht := substrate_h * 2 * arctanh (kkk (x)) / pi;
         end if;
         --  Use kkk for k factor
         --  if microstripline then
      else
         if zed > (44 - 2 * er)
         then
            Hp := (zed / 120) * sqrt (2 * (er + 1)) + (er - 1) * (lnpi_2 + ln4_pi / er) / (2 * (er + 1));
            if Hp > 87.0
            then
               --  e^87 = 6.0e37
               High_Z_Error;
            else
               expH := exp (Hp);
               Result_widtht := substrate_h / (expH / 8 - 1 / (4 * expH));
            end if;
            --  if zed <= (44-2*er)
         else
            de := 60 * ((pi) ** 2) / (zed * sqrt (er));
            Result_widtht := substrate_h * (2 / pi * ((de - 1) - Log (2 * de - 1)) + (er - 1) * (Log (de - 1) + 0.293 - 0.517 / er) / (pi * er));
         end if;
         --  if zed
      end if;
      --  if stripline
      return Result_widtht;
   end widtht;
   --  widtht
   --  *
   --  Hyperbolic sine used for s-parameter
   --  calculations in tlines and clines.
   --  Reals will explode with x > 300.
   --  *

   function sinh (x : in Long_Float) return Long_Float is
      Result_sinh : Long_Float;
      a : Long_Float;
   begin
      a := exp (x);
      Result_sinh := (a - 1 / a) / 2;
      return Result_sinh;
   end sinh;
   --  * sinh *
   --  *
   --  Hyperbolic cosine used for tline and cline
   --  s-parameter calculation.
   --  Reals will explode with x > 300
   --  *

   function cosh (x : in Long_Float) return Long_Float is
      Result_cosh : Long_Float;
   begin
      Result_cosh := (exp (x) + 1 / exp (x)) / 2;
      return Result_cosh;
   end cosh;
   --  * cosh *
   --  *
   --  Hyperbolic cosine used for cline calculation.
   --  *

   function cl_cosh (x : in Long_Float) return Long_Float is
      Result_cl_cosh : Long_Float;
      exp1 : Long_Float;
   begin
      if x > 300
      then
         exp1 := infty;
         bad_compt := true;
         message (1) := "cline impedances";
         message (2) := "can" + Character (39) + "t be realized";
         message (3) := "in microstrip";
      else
         exp1 := exp (x);
         Result_cl_cosh := (exp1 + 1 / exp1) / 2;
      end if;
      return Result_cl_cosh;
   end cl_cosh;
   --  * cl_cosh *
   --  *
   --  Inverse cosh.
   --  Used for Procedures error() and w_s_microstip_cline.
   --  *

   function arccosh (x : in Long_Float) return Long_Float is
      Result_arccosh : Long_Float;
      sqx : Long_Float;
   begin
      sqx := ((x) ** 2);
      if sqx <= 1
      then
         Result_arccosh := 0;
         bad_compt := true;
         message (1) := "cline impedances";
         message (2) := "can" + Character (39) + "t be realized";
         message (3) := "in microstrip";
      else
         Result_arccosh := Log (x + sqrt (((x) ** 2) - 1));
      end if;
      return Result_arccosh;
   end arccosh;
   --  arccosh
   --  *
   --  This routine is used to calculate cline dimensions.
   --  When error=0 a consistent solution of the cline equations
   --  has been reached. See Edwards p139.
   --  *

   procedure error (g, wo, ceven : in Long_Float; fg, dfg : in out Long_Float) is
      hh, sqm1, rsqm1, dcdg, acoshh, acoshg, u1, u2, du1dg, du2dg, dhdg, dcdu1, dcdu2, dcdh : Long_Float;
   begin
      hh := 0.5 * ((g + 1) * ceven + g - 1);
      dhdg := 0.5 * (ceven + 1.0);
      acoshh := arccosh (hh);
      if bad_compt
      then
         return;
      end if;
      acoshg := arccosh (g);
      if bad_compt
      then
         return;
      end if;
      sqm1 := ((hh) ** 2) - 1;
      rsqm1 := sqrt (sqm1);
      dcdh := (rsqm1 + hh) / (hh * rsqm1 + sqm1);
      sqm1 := ((g) ** 2) - 1;
      rsqm1 := sqrt (sqm1);
      dcdg := (rsqm1 + g) / (g * rsqm1 + sqm1);
      u1 := ((g + 1) * ceven - 2) / (g - 1);
      du1dg := ((g - 1) * ceven - ((g + 1) * ceven - 2)) / ((g - 1) ** 2);
      u2 := acoshh / acoshg;
      du2dg := (acoshg * dcdh * dhdg - acoshh * dcdg) / ((acoshg) ** 2);
      sqm1 := ((u1) ** 2) - 1;
      rsqm1 := sqrt (sqm1);
      dcdu1 := (rsqm1 + u1) / (u1 * rsqm1 + sqm1);
      sqm1 := ((u2) ** 2) - 1;
      rsqm1 := sqrt (sqm1);
      dcdu2 := (rsqm1 + u2) / (u2 * rsqm1 + sqm1);
      if (er > 6)
      then
         fg := (2 * arccosh (u1) + arccosh (u2)) / pi - wo;
         if bad_compt
         then
            return;
         end if;
         dfg := (2 * dcdu1 * du1dg + dcdu2 * du2dg) / pi;
      else
         fg := (2 * arccosh (u1) + 4 * arccosh (u2) / (1.0 + er / 2.0)) / pi - wo;
         if bad_compt
         then
            return;
         end if;
         dfg := (2 * dcdu1 * du1dg + 4 * dcdu2 * du2dg / (1.0 + er / 2.0)) / pi;
      end if;
   end error;
   --  error
   --  *
   --  Computes ratios W/b and S/b for stripline clines.
   --  See Puff Manual page 14 for details.
   --  *

   procedure w_s_stripline_cline (zede, zedo : in Long_Float; woh, soh : in out Long_Float) is
      ke, ko : Long_Float;
   begin
      ke := kkk (zede * sqrt (er) / (30 * pi));
      ko := kkk (zedo * sqrt (er) / (30 * pi));
      woh := 2 * arctanh (sqrt (ke * ko)) / pi;
      soh := 2 * arctanh (sqrt (ke / ko) * (1 - ko) / (1 - ke)) / pi;
   end w_s_stripline_cline;
   --  w_s_stripline_cline
   --  *
   --  This routine uses Netwon's method to find the cline width
   --  and spacing. Errors occur in the repeat (newton	algorithm)
   --  loop if the even and odd impedances are too close.
   --  *

   procedure w_s_microstrip_cline (we, wo : in Long_Float; woh, soh : in out Long_Float) is
      tol : constant := 0.0001;
      codd, ceven, g, fg, dfg, dg, g1 : Long_Float;
      i : integer;
   begin
      ceven := cl_cosh (pi * we / 2);
      codd := cl_cosh (pi * wo / 2);
      soh := 2 * arccosh ((ceven + codd - 2) / (codd - ceven)) / pi;
      if bad_compt
      then
         return;
      end if;
      --  starting guess
      g := cl_cosh (pi * soh / 2);
      i := 0;
      loop
         --  newton algorithm
         --  !* beware of divergence in this loop *
         g1 := g;
         error (g, wo, ceven, fg, dfg);
         if bad_compt
         then
            return;
         end if;
         dg := fg / dfg;
         g := g1 - dg;
         i := i + 1;
         if g <= 1.0
         then
            i := 101;
         end if;
         exit when ((abs (dg) < abs (tol * g)) or else (i > 100));
      end loop;
      if i > 100
      then
         bad_compt := true;
         message (1) := "cline impedances";
         message (2) := "can" + Character (39) + "t be realized";
         message (3) := "in microstrip";
         return;
      end if;
      soh := 2.0 * arccosh (g) / pi;
      woh := arccosh (0.5 * ((g + 1) * ceven + g - 1)) / pi - soh / 2.0;
   end w_s_microstrip_cline;
   --  w_s_microstrip_cline
   --  *
   --  Called when a disastrous error condition
   --  has been reached to stop Puff.
   --  *

   procedure shutdown is
   begin
      CloseGraph;
      TextMode (OrigMode);
      message (1) := "FATAL ERROR:";
      write_message;
      Gotoxy (1, 23);
      Put ("Press any key to quit");
      ReadKey;
      raise Program_halted (3);
   end shutdown;
   --  *
   --  Performs an Assign and Reset on textfile if the file exists.
   --  Note that Textfile is used here. Consider changing the
   --  buffer size using SetTextBuffer.
   --  *

   function Fileexists (note : boolean; inf : in out textfile; fname : file_string) return boolean is
      Result_Fileexists : boolean;
   begin
      fileexists := False;
      if fname /= ""
      then
         --  $I-
         --  $I+
         --  * Disable I/O check in case file not present *
         Assign (inf, fname);
         Reset (inf);
         if IOResult = 0
         then
            --  * IOResult of 0 means success *
            fileexists := true;
         else
            if note
            then
               message (2) := "File not found";
               message (3) := fname;
               write_message;
               P2Ada_no_keyword_Delay (1000);
            end if;
         end if;
         --  if IO
      end if;
      --  if fname
      return Result_Fileexists;
   end Fileexists;
   --  * Fileexists *
   --  *
   --  Look for setup.puf in current directory of in \PUFF.
   --  *

   function setupexists (fname : in out file_string) return boolean is
      Result_setupexists : boolean;
      found : boolean;
   begin
      found := false;
      message (2) := fname;
      if fname /= "setup.puf"
      then
         fname := "setup.puf";
         found := fileexists (false, net_file, fname);
      end if;
      if not (found)
      then
         fname := "\PUFF\setup.puf";
         found := fileexists (false, net_file, fname);
      end if;
      if found
      then
         erase_message;
         message (1) := "Missing board#";
         message (2) := "Try";
         message (3) := fname;
         write_error (2);
      end if;
      Result_setupexists := found;
      return Result_setupexists;
   end setupexists;
   --  setupexists
   --  *
   --  Modified arctan to get phase in all quadrants
   --  and avoid blow ups when x=0.
   --  *

   function atan2 (x, y : in Long_Float) return Long_Float is
      Result_atan2 : Long_Float;
      atan2t : Long_Float;
   begin
      if x = 0
      then
         if y > 0
         then
            atan2t := 90.0;
         else
            atan2t := - 90.0;
         end if;
      else
         if x > 0
         then
            atan2t := 180 * arctan (y / x) / pi;
         else
            if y > 0
            then
               atan2t := 180 * arctan (y / x) / pi + 180;
            else
               atan2t := 180 * arctan (y / x) / pi - 180;
            end if;
         end if;
      end if;
      if abs (atan2t) < 1.0e - 25
      then
         Result_atan2 := 0;
      else
         Result_atan2 := atan2t;
      end if;
      return Result_atan2;
   end atan2;
   --  atan2
   --  *
   --  Look for space on disk -- a: 0, b: 1 ..
   --  *

   function enough_space (defaultdrive : integer) return boolean is
      --  *
      --  On Linux, don't actually test whether there is enough disk space;
      --  that doesn't help much on a multitasking system anyway
      --  *
      Result_enough_space : boolean;
   begin
      Result_enough_space := true;
      return Result_enough_space;
   end enough_space;
   --  * Enough_space *
   --  *
   --  Find number of node in linked list of nets.
   --  *

   function node_number return integer is
      Result_node_number : integer;
      nn : integer;
      tnet : net;
   begin
      tnet := null;
      nn := 0;
      if net_start /= null
      then
         loop
            if tnet = null
            then
               tnet := net_start;
            else
               tnet := tnet.all.next_net;
            end if;
            if tnet.all.node
            then
               nn := nn + 1;
            end if;
            exit when (tnet.all.next_net = null) or else (cnet = tnet);
         end loop;
      end if;
      if cnet = tnet
      then
         Result_node_number := nn;
      else
         Result_node_number := 0;
      end if;
      return Result_node_number;
   end node_number;
   --  node_number
   --  *
   --  Update array which contains keystrokes used to layout circuit.
   --  *

   procedure update_key_list (nn : integer) is
   begin
      if key_end = 0
      then
         key_end := 1;
      else
         key_end := key_end + 1;
      end if;
      if key_end = key_max
      then
         key_end := key_max - 1;
         message (1) := "Circuit is too";
         message (2) := "complex for";
         message (3) := "redraw";
         write_message;
      end if;
      key_list (key_end).keyl := key;
      key_list (key_end).noden := nn;
   end update_key_list;
   --  update_key_list
   --  *
   --  Carriage return operation used only in the
   --  Parts Window.
   --  Advances to next part, puts cursor on first character.
   --  *

   procedure Carriage_Return is
   begin
      if ((ccompt.all.next_compt = null) or else (not (Large_Parts) and then (window_number = 3) and then (ccompt.all.descript (1) = 'i')) or else (Large_Parts and then (window_number = 3) and then (ccompt.all.descript (1) = 'j') and then help_displayed))
      then
         case window_number is
            when 3 =>
               ccompt := part_start;
            when 4 =>
               ccompt := board_start;
            when others =>
               beep;
         end case;
         --  case
      else
         ccompt := ccompt.all.next_compt;
      end if;
      cx := ccompt.all.x_block;
   end Carriage_Return;
   --  *
   --  Move character cursor. Used for Board, Parts, and Plot windows.
   --  *

   procedure Move_Cursor (x1, y1 : integer) is
      long, i : integer;
      has_spaces : boolean;
   begin
      has_spaces := false;
      if x1 /= 0
      then
         --  [P2Ada]: WITH instruction
         declare P2Ada_Var_3 : compt_record renames ccompt.all;
         begin
            long := (P2Ada_Var_3.descript'length);
            if cx + x1 <= long
            then
               cx := cx + x1;
               if cx < P2Ada_Var_3.x_block
               then
                  cx := P2Ada_Var_3.x_block;
               end if;
               if P2Ada_Var_3.right and then (cx + P2Ada_Var_3.xp >= P2Ada_Var_3.xorig)
               then
                  if (window_number = 2)
                  then
                     WindMax := WindMax + 1;
                  end if;
                  --  Increment WindMax to prevent scrolling in plot window
                  P2Ada_Var_3.xp := P2Ada_Var_3.xp - 1;
                  write_compt (lightgray, ccompt);
                  Put (' ');
                  if (window_number = 2)
                  then
                     WindMax := WindMax - 1;
                  end if;
                  --  Restore WindMax
               end if;
            end if;
         end;
         --  [P2Ada]: end of WITH
         --  if x1=0 then...
      else
         Erase_message;
         if Pos (' ', ccompt.all.descript) /= 0
         then
            has_spaces := true;
         end if;
         if (window_number = 2) and then (((ccompt.all.descript'length) < 3) or else (has_spaces))
         then
            for i in 1 .. max_params
            loop
               --  Delete invalid S-parameter designations
               if s_param_table (i) = ccompt
               then
                  --  erase invalid s-parameter on screen
                  Delete (ccompt.all.descript, 2, 2);
                  GotoXY (ccompt.all.xp - 2, ccompt.all.yp);
                  Put ("     ");
               end if;
            end loop;
         end if;
         if y1 /= 0
         then
            --  if y1=0 then skip the following
            if y1 = - 1
            then
               if ccompt.all.prev_compt = null
               then
                  beep;
               else
                  ccompt := ccompt.all.prev_compt;
               end if;
               --  y1=1
            else
               if ((ccompt.all.next_compt = null) or else (not (Large_Parts) and then (window_number = 3) and then (Character'Pos (ccompt.all.descript (1)) - Character'Pos ('a') >= ymax (3) - ymin (3))) or else (Large_Parts and then (window_number = 3) and then (Character'Pos (ccompt.all.descript (1)) - Character'Pos ('a') >= ymax (3) - ymin (3)) and then help_displayed))
               then
                  beep;
               else
                  ccompt := ccompt.all.next_compt;
               end if;
            end if;
            --  y1=-1
            if (window_number = 2) and then ((ccompt.all.descript'length) = 1)
            then
               for i in 1 .. max_params
               loop
                  if s_param_table (i) = ccompt
                  then
                     --  write "S"
                     pattern (xmin (2) * charx - 1, (ymin (2) + 2 + i) * chary - 8, i, 0);
                     write_comptm (1, lightgray, ccompt);
                  end if;
               end loop;
            end if;
            long := (ccompt.all.descript'length);
            if cx > long
            then
               cx := long;
            end if;
            if window_number = 2
            then
               cx := ccompt.all.x_block;
            end if;
            if cx < ccompt.all.x_block
            then
               cx := ccompt.all.x_block;
            end if;
         end if;
         --  if y1 <> 0
      end if;
      --  else
   end Move_Cursor;
   --  move_cursor
   --  *
   --  Calculate the hyperbolic tangent.
   --  *

   function tanh (x : in Long_Float) return Long_Float is
      Result_tanh : Long_Float;
      ex : Long_Float;
   begin
      if (x < - 30)
      then
         Result_tanh := - 1.0;
         return Result_tanh;
      end if;
      if (x > 30)
      then
         Result_tanh := 1.0;
      else
         ex := exp (x);
         Result_tanh := (ex - 1 / ex) / (ex + 1 / ex);
      end if;
      return Result_tanh;
   end tanh;
   --  *
   --  *

   function KoK (k : in Long_Float) return Long_Float is
      Result_KoK : Long_Float;
      kp : Long_Float;
   begin
      kp := sqrt (1 - ((k) ** 2));
      if ((k) ** 2) < 0.5
      then
         kok := Log (2 * (1 + sqrt (kp)) / (1 - sqrt (kp))) / pi;
      else
         kok := pi / Log (2 * (1 + sqrt (k)) / (1 - sqrt (k)));
      end if;
      return Result_KoK;
   end KoK;
   --  *
   --  Calculate capacitances of coupled microstrip.
   --  *

   procedure capac (W_h, S_h, er : in Long_Float; ce, co : in out Long_Float) is
      cp, cf, cfp, cga, cgd, ere, zo, a : Long_Float;
   begin
      ere := (er + 1) / 2 + (er - 1) / 2 / sqrt (1 + 10 / W_h);
      if W_h <= 1.0
      then
         zo := 370.0 * Log (8.0 / W_h + 0.25 * W_h) / (2.0 * pi * sqrt (ere));
      else
         zo := 370.0 / ((W_h + 1.393 + 0.667 * Log (W_h + 1.44)) * sqrt (ere));
      end if;
      a := exp (- 0.1 * exp (2.33 - 2.53 * W_h));
      cp := er * W_h;
      cf := 0.5 * (sqrt (ere) / (zo / (120 * pi)) - cp);
      cfp := cf * sqrt (er / ere) / (1 + a * tanh (8 * S_h) / S_h);
      cga := KoK (S_h / (S_h + 2 * W_h));
      cgd := er * Log (1 / tanh (pi * S_h / 4)) / pi + 0.65 * cf * (0.02 * sqrt (er) / S_h + 1 - 1 / ((er) ** 2));
      ce := cp + cf + cfp;
      co := cp + cf + cga + cgd;
   end capac;
   --  capac
   --  *
   --  Compute effective relative dielectric constants for cline
   --  even and odd impedances.
   --  *

   procedure ere_even_odd (W_h, S_h : in Long_Float; ee, eo : in out Long_Float) is
      ce1, co1, cee, coe : Long_Float;
   begin
      capac (W_h, S_h, 1, ce1, co1);
      capac (W_h, S_h, er, cee, coe);
      ee := cee / ce1;
      eo := coe / co1;
   end ere_even_odd;
   --  ere_even_odd
   --  *
   --  L-U Decomposition routine inspired by Numerical Recipes.
   --  The following types are used here:
   --  s_conv_matrix = array [1..conv_size,1..conv_size] of TComplex;
   --  s_conv_vector = array [1..conv_size] of TComplex;
   --  s_conv_index  = array [1..conv_size] of integer;
   --  s_real_vector = array [1..conv_size] of double;
   --  *

   procedure LU_Decomp (a : in out s_conv_matrix; n : integer; indx : in out s_conv_index; d : in out Long_Float) is
      --  new(vv);
      --  * Loop over rows to get implicit scaling information *
      tiny : constant := 1.0e - 20;
      s_real_vector is array (1 .. conv_size) of Long_Float;
      k, j, imax, i : integer;
      sum, dum_z, z_dum : TComplex;
      big, dum_r : Long_Float;
      vv : s_real_vector;
   begin
      d := 1.0;
      for i in 1 .. n
      loop
         big := 0.0;
         for j in 1 .. n
         loop
            if (co_mag (a (i, j)) > big)
            then
               big := co_mag (a (i, j));
            end if;
         end loop;
         if big = 0.0
         then
            --  if zeros all along the column...
            --  ! This will not cause recovery *
            Message (1) := "Warning!";
            Message (2) := "indef part has";
            Message (3) := "singular matrix";
            Write_Message;
            big := tiny;
         end if;
         --  save the scaling for future reference
         vv (i) := 1.0 / big;
      end loop;
      for j in 1 .. n
      loop
         for i in 1 .. j - 1
         loop
            Equate_Zs (sum, a (i, j));
            for k in 1 .. i - 1
            loop
               Diffpr (sum, a (i, k), a (k, j));
            end loop;
            --  * sum=sum-a[i,k]*a[k,j] *
            Equate_Zs (a (i, j), sum);
         end loop;
         big := 0.0;
         for i in j .. n
         loop
            Equate_Zs (sum, a (i, j));
            for k in 1 .. j - 1
            loop
               Diffpr (sum, a (i, k), a (k, j));
            end loop;
            --  * sum=sum-a[i,k]*a[k,j] *
            --  * Check here for a better figure of merit for the pivot *
            Equate_Zs (a (i, j), sum);
            dum_r := vv (i) * co_mag (sum);
            if dum_r >= big
            then
               --  *if better, exchange and save index*
               big := dum_r;
               imax := i;
            end if;
         end loop;
         if j /= imax
         then
            --  * Interchange rows if needed *
            for k in 1 .. n
            loop
               Equate_Zs (dum_z, a (imax, k));
               Equate_Zs (a (imax, k), a (j, k));
               Equate_Zs (a (j, k), dum_z);
            end loop;
            --  * Interchange the scale factor *
            d := - d;
            vv (imax) := vv (j);
         end if;
         indx (j) := imax;
         if co_mag (a (j, j)) = 0.0
         then
            a (j, j).r := tiny;
         end if;
         if j /= n
         then
            --  complex reciprocal-creates pointer z_dum
            Rc (z_dum, a (j, j));
            for i in j + 1 .. n
            loop
               --  ! is this legal?
               --  *a[i,j] := a[i,j]*z_dum *
               prp (dum_z, a (i, j), z_dum);
               Equate_Zs (a (i, j), dum_z);
            end loop;
         end if;
      end loop;
   end LU_Decomp;
   --  * LU_Decomp *
   --  *
   --  Routine for L-U forward and backward substitution,
   --  needed to solve for linear systems and do matrix inversions.
   --  The following types are used here:
   --  s_conv_matrix = array [1..conv_size,1..conv_size] of complex;
   --  s_conv_vector = array [1..conv_size] of complex;
   --  s_conv_index  = array [1..conv_size] of integer;
   --  *

   procedure LU_Sub (a : in out s_conv_matrix; n : integer; indx : in out s_conv_index; b : in out s_conv_vector) is
      j, ip, ii, i : integer;
      sum, dum_z : TComplex;
   begin
      ii := 0;
      for i in 1 .. n
      loop
         --  sum := b[ip]
         --  b[ip] := b[i]
         ip := indx (i);
         Equate_Zs (sum, b (ip));
         Equate_Zs (b (ip), b (i));
         if ii /= 0
         then
            for j in ii .. i - 1
            loop
               Diffpr (sum, a (i, j), b (j));
            end loop;
            --  sum := sum-a[i,j]*b[j]
         else
            if (co_mag (sum) /= 0.0)
            then
               ii := i;
            end if;
         end if;
         Equate_Zs (b (i), sum);
      end loop;
      for i in reverse 1 .. n
      loop
         Equate_Zs (sum, b (i));
         for j in i + 1 .. n
         loop
            Diffpr (sum, a (i, j), b (j));
         end loop;
         --  sum := sum-a[i,j]*b[j]
         --  complex reciprocal - new pointer
         --  ! is this legal?
         Rc (dum_z, a (i, i));
         Prp (b (i), sum, dum_z);
      end loop;
   end LU_Sub;
   --  * Lub_Sub *
   --  *
   --  Uses the LU decomposition and substitution
   --  routines to invert the matrix "a" of size n x n.
   --  The original matrix "a" is destroyed and replaced
   --  with its inverse.
   --  Inversion is performed column by column.
   --  *

   procedure Matrix_Inversion (a : in out s_conv_matrix; n : integer) is
      --  complex zero
      --  complex one
      --  * LU decomposition of matrix a *
      i, j : integer;
      d : Long_Float;
      co_0, co_1 : TComplex;
      indx : s_conv_index;
      col : s_conv_vector;
      y : s_conv_matrix;
   begin
      Co (co_0, 0.0, 0.0);
      Co (co_1, 1.0, 0.0);
      LU_Decomp (a, n, indx, d);
      for j in 1 .. n
      loop
         for i in 1 .. n
         loop
            Equate_Zs (col (i), co_0);
         end loop;
         --  fill column with zeros
         --  Put 1+j0 in the proper position
         Equate_Zs (col (j), co_1);
         LU_Sub (a, n, indx, col);
         for i in 1 .. n
         loop
            Equate_Zs (y (i, j), col (i));
         end loop;
      end loop;
      --  * Fill Matrix "a" with data in "y" *
      for j in 1 .. n
      loop
         for i in 1 .. n
         loop
            Equate_Zs (a (i, j), y (i, j));
         end loop;
      end loop;
   end Matrix_Inversion;
   --  * Matrix_Inversion *
   --  *
   --  Performs the matrix multiplication:
   --  a = b * c
   --  where a,b,c are matrices of complex numbers
   --  each of dimension n x n.
   --  Matrices b and c are unaffected.
   --  It is assumed that all matrices are initialized.
   --  *

   procedure Matrix_Mux (a, b, c : in out s_conv_matrix; n : integer) is
      i, k, j : integer;
      sum, co_0 : TComplex;
   begin
      co (co_0, 0.0, 0.0);
      for j in 1 .. n
      loop
         for i in 1 .. n
         loop
            --  initialize sum to zero
            Equate_Zs (sum, co_0);
            for k in 1 .. n
            loop
               Supr (sum, b (i, k), c (k, j));
            end loop;
            --  * sum:= sum + b[i,k]*c[k,j] *
            --  fill matrix "a"
            Equate_Zs (a (i, j), sum);
         end loop;
      end loop;
   end Matrix_Mux;
   --  * Matrix_Mux *
   --  *
   --  Performs the calculation:
   --  A := (I - A)(I + A)^(-1)
   --  Used to convert between S and Y matrices
   --  Defined are B = (I - A) and C = (I + A)
   --  When converting from a two port to a three port
   --  the s-parameters are switched such that  2 <-> 3
   --  for each of the port numbers.  This causes the
   --  (indefinite) ground to be created at port 2,
   --  and is easier to use in the layout window.
   --  *

   procedure Matrix_Conv (a : in out s_conv_matrix; n : integer) is
      --  initialize pointers for c[i,j] b[i,j] and make copy of a[i,j]
      i, j : integer;
      b, c : s_conv_matrix;
      co_0, co_1 : TComplex;
   begin
      co (co_0, 0.0, 0.0);
      co (co_1, 1.0, 0.0);
      for j in 1 .. n
      loop
         for i in 1 .. n
         loop
            if i = j
            then
               --  di is complex difference
               --  su is complex sum
               --  di is complex difference
               --  su is complex sum
               di (b (i, j), co_1, a (i, j));
               su (c (i, j), co_1, a (i, j));
            else
               di (b (i, j), co_0, a (i, j));
               Equate_Zs (c (i, j), a (i, j));
            end if;
         end loop;
      end loop;
      --  * Now B = (I - A) and C = (I + A)  *
      --  * Now B = (I - A) and C = (I + A)^(-1)  *
      --  * Now A = (I - A)(I + A)^(-1)  *
      Matrix_Inversion (c, n);
      Matrix_Mux (a, b, c, n);
   end Matrix_Conv;
   --  * Matrix_Conv *
   --  *
   --  Procedure for converting n port S matrix
   --  to n+1 port S matrix.
   --  Warning! The "S" pointers must be initialized to n+1 size!
   --  Steps:  1. S(n port) -> Y(n port)
   --  2. Y(n port) -> Y(n+1 port)
   --  3. Y(n+1 port) -> S(n+1 port)
   --  *

   procedure Indef_Matrix (S : in out s_conv_matrix; n : integer) is
      --  ******************************************
      i, j : integer;
      co_0, sum : TComplex;
      --  *
      --  Change sign of complex number
      --  *

      procedure Sign_Change (z : in out TComplex) is
      begin
         z.r := - z.r;
         z.i := - z.i;
      end Sign_Change;
      --  ******************************************
      --  *
      --  z1 = z1 +z2
      --  *

      procedure Sum_Up (z1, z2 : in out TComplex) is
      begin
         z1.r := z1.r + z2.r;
         z1.i := z1.i + z2.i;
      end Sum_Up;
      --  *******************************************
      --  *
      --  Swap s-parameters between ports 2 and 3.
      --  To be called only for the 2 port device.
      --  *

      procedure Swap_2_and_3 (T : in out s_conv_matrix) is
         --  * S12 <-> S13 *
         --  * S21 <-> S31 *
         --  * S23 <-> S32 *
         --  * S22 <-> S33 *
         temp_z : TComplex;
      begin
         Equate_Zs (temp_z, T (1, 3));
         Equate_Zs (T (1, 3), T (1, 2));
         Equate_Zs (T (1, 2), temp_z);
         Equate_Zs (temp_z, T (3, 1));
         Equate_Zs (T (3, 1), T (2, 1));
         Equate_Zs (T (2, 1), temp_z);
         Equate_Zs (temp_z, T (2, 3));
         Equate_Zs (T (2, 3), T (3, 2));
         Equate_Zs (T (3, 2), temp_z);
         Equate_Zs (temp_z, T (3, 3));
         Equate_Zs (T (3, 3), T (2, 2));
         Equate_Zs (T (2, 2), temp_z);
      end Swap_2_and_3;
      --  ********************************************
      --  Changes S to a normalized admittance matrix
      --  * Y n-port to Y n+1 port routine: *
   begin
      Matrix_Conv (S, n);
      Co (co_0, 0.0, 0.0);
      for j in 1 .. n
      loop
         --  initialize sum to complex zero
         Equate_Zs (sum, co_0);
         for i in 1 .. n
         loop
            Sum_Up (sum, S (i, j));
         end loop;
         --  new value for Y[n+1,j]
         Sign_Change (sum);
         Equate_Zs (S (n + 1, j), sum);
      end loop;
      for i in 1 .. n
      loop
         --  initialize sum to complex zero
         Equate_Zs (sum, co_0);
         for j in 1 .. n
         loop
            Sum_Up (sum, S (i, j));
         end loop;
         --  new value for Y[i,n+1]
         Sign_Change (sum);
         Equate_Zs (S (i, n + 1), sum);
      end loop;
      --  initialize sum to complex zero
      Equate_Zs (sum, co_0);
      for i in 1 .. n
      loop
         for j in 1 .. n
         loop
            Sum_Up (sum, S (i, j));
         end loop;
      end loop;
      --  new value for Y[n+1,n+1]
      --  Change from Y to indef S matrix
      Equate_Zs (S (n + 1, n + 1), sum);
      Matrix_Conv (S, n + 1);
      if (n = 2)
      then
         Swap_2_and_3 (S);
      end if;
      --  * Exchange ports 2 and 3 for the 3 port indef *
   end Indef_Matrix;
   --  * Indef_Matrix *
   --  *
   --  A call is made to this function whenever a call to
   --  New or GetMem cannot be completed i.e. when no room
   --  remains on the heap. The net result of setting
   --  HeapFunc:=1 is that New and GetMem will then return
   --  a nil pointer and the program will not be aborted
   --  with a 203 error.
   --  In TP 6.0 a quick exit is required for Size=0
   --  *

   function HeapFunc (Size : Unsigned_16) return integer is
      Result_HeapFunc : integer;
   begin
      if (Size > 0)
      then
         if message (3) /= " Exhausted "
         then
            --  plot window
            erase_message;
            message (1) := "  DANGER!  ";
            message (2) := "  Memory   ";
            message (3) := " Exhausted ";
            if window_number = 2
            then
               write_message;
               P2Ada_no_keyword_delay (1500);
            else
               shutdown;
            end if;
         end if;
         Result_HeapFunc := 1;
      end if;
      --  if Size
      return Result_HeapFunc;
   end HeapFunc;
   --  * HeapFunc *
   --  ************* METHODS FOR THE SWEEP OBJECT ******************
   --  *
   --  Reset sweep object.
   --  *

   procedure Init_Use (Self : in out Sweep'class) is
      --  this is a global variable
      --  [P2Ada]: end of Object
   begin
      Self.element := null;
      Self.used := false;
      Alt_Sweep := false;
      Self.unit_label := "";
      Self.index := 0;
   end Init_Use;
   --  * Init_Use *
   --  *
   --  Initialize the sweep object element if it hasn't been
   --  previously initialized. Called by lumped,clines,tline,etc.
   --  *

   procedure Init_Element (Self : in out Sweep'class; tcompt : compt; in_id, in_prefix, in_unit : Character) is
      potential_units : array (Character) of Boolean := (degree | Omega | 'm' | 'h' | 's' | 'S' | 'z' | 'Z' | 'y' | 'Y' | 'Q' => True, others => False);
   begin
      if not (Self.used)
      then
         --  element points to compt
         --  convert design Freq to rad/sec times prefix
         --  this is a global flag
         --  tell tcompt that its the sweep
         --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
         Self.used := true;
         Self.element := tcompt;
         Self.id := in_id;
         Self.prefix := in_prefix;
         Self.Omega0 := 2 * Pi * design_freq * Eng_Prefix (freq_prefix);
         Self.units := in_unit;
         alt_sweep := true;
         tcompt.all.sweep_compt := true;
         Self.part_label := "Part " + tcompt.all.descript (1);
         if Self.prefix and Eng_Dec_Mux
         then
            Self.unit_label := Self.prefix;
         else
            Self.unit_label := "";
         end if;
         if (Self.id = 'j')
         then
            Self.unit_label := 'j' + Self.unit_label;
         end if;
         --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
         if (Self.id and ('F' | 'H' => True, others => False))
         then
            Self.unit_label := Self.unit_label + Self.id;
         else
            if (Self.id = 'a')
            then
               Self.unit_label := Self.unit_label + "dB";
            else
               if (Self.id = 't')
               then
                  --  transformer is n:1
                  --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
                  Self.unit_label := Self.unit_label + "n:1";
               else
                  if (Self.units and potential_units)
                  then
                     Self.unit_label := Self.unit_label + Self.units;
                  end if;
               end if;
            end if;
         end if;
         --  Force re-parsing of last ?
         --  clear current alt_param
      else
         Self.element.all.changed := True;
         Init_Use;
         tcompt.all.sweep_compt := false;
         bad_compt := true;
         message (1) := "Parts list has";
         message (2) := "multiple sweep";
         message (3) := "parameters";
      end if;
      --  [P2Ada]: end of Object
   end Init_Element;
   --  * Init_Element *
   --  *
   --  See if a previous sweep_compt part has been changed.
   --  Called by Pars_Compt_List;
   --  *

   procedure Check_Reset (Self : in out Sweep'class; tcompt : compt) is
   begin
      if (Self.element = tcompt) and then tcompt.all.changed
      then
         Init_Use;
         tcompt.all.sweep_compt := false;
      end if;
      --  [P2Ada]: end of Object
   end Check_Reset;
   --  * Check_Reset *
   --  *
   --  Label x axis of x-y plot and Smith plot.
   --  *

   procedure Label_Axis (Self : in out Sweep'class) is
      --  [P2Ada]: end of Object
      label_string : line_string;
      label_lngth : integer;
   begin
      label_string := Self.part_label + " : " + Self.unit_label;
      label_lngth := (label_string'length);
      GotoXY (x_y_plot_text (5, 1) + 2 - label_lngth / 2, x_y_plot_text (5, 2));
      Put (label_string);
   end Label_Axis;
   --  * Label_Axis *
   --  *
   --  Label data in plot window from frequency to alt_param.
   --  *

   procedure Label_Plot_Box (Self : in out Sweep'class) is
      --  write part label over 'f'
      i : integer;
   begin
      GotoXY (xmin (2), ymin (2) + 2);
      Put (Self.part_label);
      GotoXY (xmin (2) + 17, ymin (2) + 2);
      Put (Self.unit_label);
      if ((Self.unit_label'length) <= 2)
      then
         for i in 0 .. (2 - (Self.unit_label'length))
         loop
            Put (' ');
         end loop;
      end if;
      --  [P2Ada]: end of Object
   end Label_Plot_Box;
   --  * Label_Plot_Box *
   --  *
   --  Load in proportionality constants to be used in sweep calculations.
   --  *

   procedure Load_Prop_Const (Self : in out Sweep'class; prop_consta, prop_constb : in Long_Float) is
      --  [P2Ada]: end of Object
   begin
      Self.prop_const1 := prop_consta;
      Self.prop_const2 := prop_constb;
   end Load_Prop_Const;
   --  *
   --  Read index for lumped element and clines sweep_compt.
   --  LUMPED
   --  i=1 : resistance
   --  i=2 : + reactance or susceptance
   --  i=3 : - reactance or susceptance
   --  CLINES
   --  i=1 : even mode impedance only
   --  i=2 : odd mode impedance only
   --  i=3 : even and odd mode impedances given, even is the variable
   --  i=4 : even and odd mode impedances given, odd is the variable
   --  *

   procedure Load_Index (Self : in out Sweep'class; i : integer) is
   begin
      Self.index := i;
      if (Self.id = 'j') and then (Self.index = 3)
      then
         Self.unit_label := '-' + Self.unit_label;
      end if;
      --  identify negative X's and B's
      --  [P2Ada]: end of Object
   end Load_Index;
   --  * Load_Index *
   --  *
   --  Read data during alternate parameter sweep.
   --  Parse data and place in appropriate element
   --  location.
   --  *

   procedure Load_Data (Self : in out Sweep'class; sweep_data : in Long_Float) is
   begin
      Self.new_value := sweep_data * Eng_Prefix (Self.prefix);
      if (Self.new_value = 0.0)
      then
         Self.new_value := 1.0 / infty;
      end if;
      --  make 1e-35
      case Self.element.all.typ is
         when 'x' | 'a' =>
            Self.element.all.zed := Self.new_value;
         when 'q' | 't' =>
            case Self.units is
               when omega =>
                  Self.element.all.zed := Self.new_value;
               when 's' | 'S' =>
                  Self.element.all.zed := 1.0 / Self.new_value;
               when 'z' | 'Z' =>
                  Self.element.all.zed := z0 * Self.new_value;
               when 'y' | 'Y' =>
                  Self.element.all.zed := z0 / Self.new_value;
               when degree =>
                  Self.element.all.wavelength := Self.new_value / 360.0;
               when 'm' =>
                  --  must put in mm
                  Self.element.all.wavelength := 1000.0 * Self.new_value * Self.prop_const1;
               when 'h' | 'H' =>
                  Self.element.all.wavelength := Self.new_value * Self.prop_const1;
               when 'Q' =>
                  Self.element.all.alpha_c := (Pi * Self.element.all.wavelength) / (Self.new_value * Self.element.all.lngth0);
                  Self.element.all.alpha_d := 0.0;
               when others =>
                  --  [P2Ada]: no otherwise / else in Pascal
                  null;
            end case;
         when 'c' =>
            --  Coupled lines
            case Self.units is
               --  fix impedances later using index
               --  omega  :  new_value:=new_value;
               when 's' | 'S' =>
                  Self.new_value := 1.0 / Self.new_value;
               when 'z' | 'Z' =>
                  Self.new_value := z0 * Self.new_value;
               when 'y' | 'Y' =>
                  Self.new_value := z0 / Self.new_value;
               when degree =>
                  Self.element.all.wavelength := Self.new_value * Self.prop_const1 / 360.0;
                  Self.element.all.wavelengtho := Self.new_value * Self.prop_const2 / 360.0;
               when 'm' =>
                  --  Use factor of 1000 to put in mm
                  Self.element.all.wavelength := 1000.0 * Self.new_value * Self.prop_const1;
                  Self.element.all.wavelengtho := 1000.0 * Self.new_value * Self.prop_const2;
               when 'h' | 'H' =>
                  --  even
                  --  odd
                  --  for 'h' substrate_h has been factored in prop_const1
                  Self.element.all.wavelength := Self.new_value * Self.prop_const1;
                  Self.element.all.wavelengtho := Self.new_value * Self.prop_const2;
               when others =>
                  --  [P2Ada]: no otherwise / else in Pascal
                  null;
            end case;
            --  case units
            case Self.index is
               --  if an impedance, fix it up
               when 1 =>
                  Self.element.all.zed := Self.new_value;
                  if (Self.new_value > 1.0e - 17)
                  then
                     Self.element.all.zedo := ((z0) ** 2) / Self.new_value;
                  else
                     Self.element.all.zedo := sqrt (infty);
                  end if;
               when 2 =>
                  Self.element.all.zedo := Self.new_value;
                  if (Self.new_value > 1.0e - 17)
                  then
                     Self.element.all.zed := ((z0) ** 2) / Self.new_value;
                  else
                     Self.element.all.zed := sqrt (infty);
                  end if;
               when 3 =>
                  --  both given, even var
                  Self.element.all.zed := Self.new_value;
               when 4 =>
                  --  both given, odd var
                  Self.element.all.zedo := Self.new_value;
               when others =>
                  --  [P2Ada]: no otherwise / else in Pascal
                  null;
            end case;
            --  case index
         when 'l' =>
            --  use index to find what lumped value is to be changed
            --  [P2Ada]: "x in y" -> "x and y" redefine "and" before
            if (Self.id and ('F' | 'H' => True, others => False))
            then
               if Self.index = 2
               then
                  Self.new_value := Self.Omega0 * Self.new_value;
               else
                  if Self.index = 3
                  then
                     Self.new_value := 1.0 / (Self.Omega0 * Self.new_value);
                  end if;
               end if;
            end if;
            case Self.units is
               when omega =>
                  --  'z','Z','y','Y' :  new_value:=new_value;
                  Self.new_value := Self.new_value / z0;
               when 's' | 'S' =>
                  Self.new_value := Self.new_value * z0;
               when others =>
                  --  [P2Ada]: no otherwise / else in Pascal
                  null;
            end case;
            --  case units
            case Self.index is
               when 1 =>
                  --  resistance or conductance
                  Self.element.all.zed := Self.new_value;
               when 2 =>
                  --  + reactance or susceptance
                  Self.element.all.zedo := Self.new_value;
               when 3 =>
                  --  - reactance or susceptance
                  --  need minus sign to work for j's
                  Self.element.all.wavelength := - Self.new_value;
               when others =>
                  --  [P2Ada]: no otherwise / else in Pascal
                  null;
            end case;
            --  Case index
            --  Case 'l'
         when others =>
            --  [P2Ada]: no otherwise / else in Pascal
            null;
      end case;
      --  case
      --  [P2Ada]: end of Object
   end Load_Data;
   --  * Load_Data *
   --  ************ Memory Management *******************
   memsize : constant := 2 * 1024 * 1024;
   membase : Pointer;
   memused : Integer_32;

   procedure Init_Mem is
   begin
      getmem (membase, memsize);
      if (membase = null)
      then
         Put ("Memory allocation error!!");
         New_Line;
         raise Program_halted;
      end if;
      memused := 0;
   end Init_Mem;

   function Mem_Left return Integer_32 is
      Result_Mem_Left : Integer_32;
   begin
      Result_Mem_Left := memsize - memused;
      return Result_Mem_Left;
   end Mem_Left;

   procedure mygetmem (p : in out pointer; size : Integer_32) is
   begin
      p := membase + memused;
      memused := memused + size;
   end mygetmem;

   procedure New_c (P : in out PMemComplex) is
   begin
      myGetMem (P, (P.all'size / 8));
   end New_c;

   procedure New_s (P : in out s_param) is
   begin
      myGetMem (P, (P.all'size / 8));
   end New_s;

   procedure New_plot (P : in out plot_param) is
   begin
      myGetMem (P, (P.all'size / 8));
   end New_plot;

   procedure New_spline (P : in out spline_param) is
   begin
      myGetMem (P, (P.all'size / 8));
   end New_spline;

   procedure New_n (P : in out net) is
   begin
      myGetMem (P, (P.all'size / 8));
   end New_n;

   procedure New_conn (P : in out conn) is
   begin
      myGetMem (P, (P.all'size / 8));
   end New_conn;

   procedure New_compt (P : in out compt) is
   begin
      myGetMem (P, (P.all'size / 8));
   end New_compt;

   procedure Mark_Mem (P : in out marker) is
   begin
      P.used := memused;
   end Mark_Mem;

   procedure Release_Mem (P : in out marker) is
   begin
      if (P.used >= 0)
      then
         memused := P.used;
      end if;
   end Release_Mem;

   procedure Init_Marker (P : in out marker) is
   begin
      P.used := - 1;
   end Init_Marker;

   function Marked (P : in out marker) return BOOLEAN is
      Result_Marked : BOOLEAN;
   begin
      Result_Marked := (P.used >= 0);
      return Result_Marked;
   end Marked;
   --  *
   --  Make a copy of the circuit for 'destructive' analysis.
   --  *

   procedure Copy_Networks (NetStart, NetEnd : marker; CopyNetStart : in out marker) is
      Size : Integer_32;
      SrcOfs, DestOfs : Integer_32;

      function Min (a, b, c : Integer_32) return Integer_32 is
         Result_Min : Integer_32;
      begin
         if (a < b)
         then
            if (a < c)
            then
               Result_Min := a;
            else
               Result_Min := c;
            end if;
         else
            if (b < c)
            then
               Result_Min := b;
            else
               Result_Min := c;
            end if;
         end if;
         return Result_Min;
      end Min;
   begin
      Size := NetEnd.used - NetStart.used;
      if (not Marked (CopyNetStart))
      then
         if (memused + Size + 1024 >= MemSize)
         then
            return;
         end if;
         CopyNetStart.Used := memused;
         memused := memused + Size;
         DestOfs := CopyNetStart.used;
         SrcOfs := NetStart.used;
      else
         SrcOfs := CopyNetStart.used;
         DestOfs := NetStart.used;
      end if;
      Move (ref (membase + SrcOfs), ref (membase + DestOfs), Size);
   end Copy_Networks;
   --  *
   --  Check to see that there is at least a 16 byte
   --  block of memory remaining on the heap.
   --  *

   function No_mem_left return boolean is
      Result_No_mem_left : boolean;
   begin
      if Mem_Left < 1024
      then
         Result_No_mem_left := true;
      else
         Result_No_mem_left := false;
      end if;
      return Result_No_mem_left;
   end No_mem_left;

   procedure SetCol (col : Unsigned_16) is
   begin
      if (blackwhite)
      then
         case col is
            when 0 =>
               --  1..7 : SetColor(lightgray);
               SetColor (0);
            when 8 =>
               SetColor (0);
            when others =>
               SetColor (white);
         end case;
      else
         SetColor (col);
      end if;
   end SetCol;

   procedure TextCol (col : Unsigned_16) is
   begin
      if ((blackwhite) and then (col /= black))
      then
         TextColor (white);
      else
         TextColor (col);
      end if;
   end TextCol;
end pfun1;
