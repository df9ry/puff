
with Ada.Text_IO;           use Ada.Text_IO;
with Interfaces;            use Interfaces;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Interfaces.C;          use Interfaces.C;

with xgraph;                use xgraph;
with Utils;                 use Utils;

with Ada.Unchecked_Deallocation;

package pfun1 is

   --  The following constants are the keyboard return codes.
   --  See Turbo Manual Appendix K.
   --  For the extended codes 27 XX, key=#128+XX

   not_esc                : constant char := char'Val (0);
   Ctrl_a                 : constant char := char'Val (1);
   Ctrl_d                 : constant char := char'Val (4);
   Ctrl_e                 : constant char := char'Val (5);
   backspace              : constant char := char'Val (8);
   Ctrl_n                 : constant char := char'Val (14);
   Ctrl_p                 : constant char := char'Val (16);
   Ctrl_r                 : constant char := char'Val (18);
   Ctrl_s                 : constant char := char'Val (19);
   Ctrl_c                 : constant char := char'Val (3);
   Ctrl_q                 : constant char := char'Val (17);
   Esc                    : constant char := char'Val (27);
   sh_1                   : constant char := char'Val (33);
   sh_3                   : constant char := char'Val (35);
   sh_4                   : constant char := char'Val (36);
   sh_2                   : constant char := char'Val (64);
   Alt_o                  : constant char := char'Val (152);
   Alt_d                  : constant char := char'Val (160);
   Alt_m                  : constant char := char'Val (178);
   Alt_p                  : constant char := char'Val (153);
   Tab                    : constant char := char'Val (9);
   sh_down                : constant char := char'Val (178);
   sh_left                : constant char := char'Val (180);
   sh_right               : constant char := char'Val (182);
   sh_up                  : constant char := char'Val (184);
   C_R                    : constant char := char'Val (13);
   F1                     : constant char := char'Val (187);
   F2                     : constant char := char'Val (188);
   F3                     : constant char := char'Val (189);
   shift_F3               : constant char := char'Val (214);
   F4                     : constant char := char'Val (190);
   F5                     : constant char := char'Val (191);
   F6                     : constant char := char'Val (192);
   shift_F5               : constant char := char'Val (216);
   F10                    : constant char := char'Val (196);
   Alt_s                  : constant char := char'Val (159);
   up_arrow               : constant char := char'Val (200);
   PgUp                   : constant char := char'Val (201);
   left_arrow             : constant char := char'Val (203);
   right_arrow            : constant char := char'Val (205);
   down_arrow             : constant char := char'Val (208);
   PgDn                   : constant char := char'Val (209);
   Ins                    : constant char := char'Val (210);
   Del                    : constant char := char'Val (211);

   --  Pseudo character used in Linux version for screen resize;
   screenresize           : constant char := char'Val (255);

   --  The following characters refer to extended graphics character set
   --  ?? P2Ada_no_keyword_Delta : constant String := Character'Val (235);
   Lambda                 : constant char := char'Val (128);
   Delta_Ch               : constant char := char'Val (235);
   Shift_arrow            : constant char := char'Val (130);
   the_bar                : constant char := char'Val (179);
   ground                 : constant char := char'Val (132);
   infin                  : constant char := char'Val (236);
   ity                    : constant char := char'Val (32);
   Parallel               : constant char := char'Val (186);
   Mu                     : constant char := char'Val (230);
   Omega                  : constant char := char'Val (234);
   Degree                 : constant char := char'Val (248);
   lbrack                 : constant char := char'Val (123);
   rbrack                 : constant char := char'Val (125);
   null_char              : constant char := char'Val (0);

   des_len                : constant := 22;
   max_net_size           : constant := 9;
   Conv_size              : constant := 9;

   col_window             : array (1 .. 4) of Integer := (LightCyan,
                                                          LightGreen,
                                                          Yellow,
                                                          LightBlue);
   s_color                : array (1 .. 4) of Integer := (LightRed,
                                                          LightCyan,
                                                          LightBlue,
                                                          Yellow);
   Eng_Dec_Mux            : constant CharArray := ('E', 'P', 'T', 'G',
                                                   'M', 'k', 'm',  Mu,
                                                   'n', 'p', 'f', 'a');
   charx                  : constant   := 8;
   chary                  : constant   := 14;
   key_max                : constant   := 2000;
   max_ports              : constant   := 4;
   max_params             : constant   := 4;
   one                    : Long_Float := 0.999999999998765;
   minus2                 : Integer    := -2;
   ln10                   : Long_Float := 2.302585092994045684;
   Pi                     : Long_Float := 3.141592653;
   infty                  : Long_Float := 1.0e+37;
   nft                    : constant   := 256;
   c_in_mm                : Long_Float := 3.0e+11;
   Mu_0                   : Long_Float := 1.25664e-6;

   --  Types:
   type char_s is array (1 .. 112) of char;

   type Object is tagged
      record
         null;
      end record;
   type Pointer is access all Object'class;

   type marker is
      record
         Used : Integer_32;
      end record;

   type TComplex is
      record
         r, i : Long_Float;
      end record;

   type TMemComplex is
      record
         c : TComplex;
      end record;
   type PMemComplex is access TMemComplex;

   type s_parameter_record;
   type s_param is access s_parameter_record;
   type s_parameter_record is
      record
         z : PMemComplex;
         next_s : s_param;
      end record;

   type compt_record;
   type compt is access compt_record;
   type compt_record is
      record
         lngth, width, zed, zedo, init_ere, alpha_c, alpha_d, alpha_co,
         alpha_do, lngth0, wavelength, wavelengtho, zed_e0, zed_o0,
         zed_S_e0, zed_S_o0, e_eff_e0, e_eff_o0, u_even, u_odd, g_fac,
         con_space, spec_freq : Long_Float;
         xp, xmaxl, x_block, xorig, yp, number_of_con, used : Integer;
         s_begin, s_file, s_ifile, f_file : s_param;
         calc, changed, right, super, parsed, step, sweep_compt : Boolean;
         next_compt, prev_compt : compt;
         descript : Unbounded_String;
         typ : char;
      end record;

   type plot_record;
   type plot_param is access plot_record;
   type plot_record is
         record
            next_p, prev_p : plot_param;
            filled : Boolean;
            x, y : Long_Float;
         end record;

   type spline_record;
   type spline_param is access spline_record;
   type spline_record is
      record
         next_c, prev_c : spline_param;
         sx, sy, h : Long_Float;
      end record;

   type net_record;
   type net is access net_record;

   type connector_record;
   type conn is access connector_record;
   type connector_record is
      record
         --  0 norm 1... max_port external 5,6 internal
         --  position in mm
         port_type, conn_no : Integer;
         cxr, cyr : Long_Float;
         dir : Unsigned_8;
         the_net : net;
         next_con, mate : conn;
         s_start : s_param;
      end record;

   type net_record is
      record
         --  postion in mm
         com : compt;
         con_start : conn;
         xr, yr : Long_Float;
         node, chamfer, grounded : Boolean;
         next_net, other_net : net;
         nx1, nx2, ny1, ny2, number_of_con, nodet, ports_connected : Integer;
      end record;

   type key_record is
         record
            keyl : char;
            noden : Integer;
         end record;

   type Sweep is tagged
      record
         element : compt; --  pointer to part
         id, prefix, units : char; --  ident, prefix and units
         part_label, unit_label : Unbounded_String; --  labels for plot window
         prop_const1, prop_const2 : Long_Float; --  Proportionality constants
         index : Integer; --  Part type index
         Omega0, new_value : Long_Float; --  angular frequ. at fd and new value
         used : Boolean; --  has sweep been used?
      end record;

   --  * The following types are defined for S parameter conversions: *
   --  * Conv_size is currently set to 9 *
   type s_conv_matrix is array (1 .. Conv_size, 1 .. Conv_size) of TComplex;
   type s_conv_vector is array (1 .. Conv_size) of TComplex;
   type s_conv_index is array (1 .. Conv_size) of Integer;

   --  Variables:
   co1 : TComplex;
   c_s : s_param;
   conk, ccon : conn;
   sresln : String (1 .. 9);
   big_text_buf : array (1 .. 2048) of char;
   sdevice : s_conv_matrix;
   x_sweep : Sweep;
   iji : array (1 .. 16, 1 .. 2) of Integer;
   key_list : array (1 .. key_max) of key_record;
   board : array (1 .. 16) of Boolean;
   s_key : array (1 .. 10) of Unbounded_String;
   s_board : array (1 .. 12, 1 .. 2) of Unbounded_String;
   s_param_table : array (1 .. max_params) of compt;
   xvalo, yvalo : array (1 .. 4) of Integer;
   cross_dot : array (1 .. 35) of Integer;
   box_dot : array (1 .. 26, 0 .. 8) of Integer;
   box_filled : array (1 .. 8) of Boolean;
   portnet : array (0 .. max_ports) of net;
   inp, P2Ada_no_keyword_out : array (1 .. max_ports) of Boolean;
   si, sj : array (1 .. max_ports) of Integer;
   mate_node : array (1 .. 4) of net;
   message : array (1 .. 3) of Unbounded_String;
   xmin : array (1 .. 12) of Integer;
   ymin : array (1 .. 12) of Integer;
   xmax : array (1 .. 12) of Integer;
   ymax : array (1 .. 12) of Integer;
   centerx, centery, rad : Integer;
   Max_Text_X : Integer;
   Max_Text_Y : Integer;
   yf : Long_Float;
   x_y_plot_text : array (1 .. 6, 1 .. 2) of Integer;
   filename_position : array (1 .. 3) of Integer;
   checking_position, layout_position : array (1 .. 2) of Integer;
   puff_file : Unbounded_String;
   net_file, dev_file : File_Type;
   command_f, window_f : array (1 .. 4) of compt;
   spline_start, spline_end : spline_param;
   dirn : Unsigned_8;
   name, network_name : Unbounded_String;
   key, key_o, chs, previous_key, freq_prefix : char;
   plot_start, plot_end, c_plot, plot_des : array
     (1 .. max_params) of plot_param;
   net_beg, dev_beg : marker;
   Box_Sv_Pntr : array (1 .. 8) of Pointer;
   Box_Sv_Size : array (1 .. 8) of Unsigned_16;
   fmin, finc, Z0, rho_fac, q_fac, resln, sfx1, sfy1, xrold, yrold, sigma,
   sxmax, sxmin, symax, symin, reduction, er, bmax, substrate_h, con_sep,
   freq, design_freq, Rs_at_fd, Lambda_fd, xm, ym, psx, psy, csx, csy,
   artwork_cor, miter_fraction, widthZ0, lengthxm, lengthym, Manh_length,
   Manh_width, conductivity, loss_tangent, metal_thickness,
   surface_roughness : Long_Float;
   cwidthxZ02, cwidthyZ02, pwidthxZ02, pwidthyZ02, message_color, key_i,
   key_end, xi, xii, yi, yii, window_number, ptmax, spx, spy, spp, displayo,
   display, Art_Form, idb, iv, xpt, npts, cx, cx3, min_ports, imin, OrigMode,
   GraphDriver, GraphMode : Integer;
   blackwhite : Boolean;
   read_kbd, board_read, insert_key, step_fn, stripline, update_key,
   spline_in_rect, spline_in_smith, Laser_Art, filled_OK, remain, admit_chart,
   circuit_changed, board_changed, marker_OK, p_labels, bad_compt, action,
   demo_mode, help_displayed, port_dirn_used, Extra_Parts_Used, Large_Parts,
   Large_Smith, Alt_Sweep, Manhattan_Board, missing_part : Boolean;
   Points_compt, rho_fac_compt, part_start, coord_start, board_start, ccompt,
   compt1, compt3, dBmax_ptr, fmin_ptr : compt;
   netK, netL, cnet, net_start : net;

   procedure Make_Text_Border (x1, y1, x2, y2, colour : Integer;
                               single : Boolean);

   procedure Dispose is new Ada.Unchecked_Deallocation
     (s_parameter_record, s_param);

   procedure Dispose is new Ada.Unchecked_Deallocation
     (plot_record, plot_param);

   procedure Dispose is new Ada.Unchecked_Deallocation
     (spline_record, spline_param);

   procedure Dispose is new Ada.Unchecked_Deallocation
     (net_record, net);

   procedure Dispose is new Ada.Unchecked_Deallocation
     (connector_record, conn);

   procedure Dispose is new Ada.Unchecked_Deallocation
     (compt_record, compt);

   procedure Release_Mem (P : marker);

   function Marked (P : marker) return Boolean;

   procedure Mark_Mem (P : in out marker);

   procedure Init_Use (Self : in out Sweep'class);

   procedure Init_Element (Self : in out Sweep'class;
                           tcompt : compt;
                           in_id, in_prefix, in_unit : char);

   procedure Check_Reset (Self : in out Sweep'class;
                          tcompt : compt);

   procedure Label_Axis (Self : in out Sweep'class);

   procedure Label_Plot_Box (Self : in out Sweep'class);

   procedure Load_Prop_Const (Self : in out Sweep'class;
                              prop_consta, prop_constb : Long_Float);

   procedure Load_Index (Self : in out Sweep'class; i : Integer);

   procedure TextCol (col : Unsigned_16);

   procedure SetCol (col : Unsigned_16);

   procedure Write_Message;

   procedure Write_Error (time : Long_Float);

   procedure Erase_Message;

   procedure Erase_Circuit;

   procedure Clear_Window (x1, y1, x2, y2 : Integer);

   procedure Clear_Window_gfx (x1, y1, x2, y2 : Integer);

   procedure Draw_Box (xs, ys, xm, ym, color : Integer);

   procedure fill_box (x1, y1, x2, y2, color : Integer);

   procedure rcdelay (j : Integer);

   procedure beep;

   procedure shutdown;

   function File_Exists_And_Open (file : in out Ada.Text_IO.File_Type;
                         fname : Unbounded_String) return Boolean;

   procedure prp (vu : in out TComplex; vX, vY : TComplex);
   procedure co (co : in out TComplex; s, t : Long_Float);
   procedure rc (rc : in out TComplex; z : TComplex);
   procedure sm (sm : in out TComplex; s : Long_Float; t : TComplex);

   function kkk (x : Long_Float) return Long_Float;

   function widtht (zed : Long_Float) return Long_Float;

   procedure write_compt (color : Integer; tcompt : compt);

   procedure write_comptm (m, color : Integer; tcompt : compt);

   function Get_Real (tcompt : compt; n : Integer) return Long_Float;

   procedure update_key_list (nn : Integer);

   procedure Pars_Compt_List;

   procedure Init_Marker (P : in out marker);

   function betweenr (x1, x2, x3, sigma : Long_Float) return Boolean;

   procedure increment_pos (i : Integer);

   procedure dirn_xy;

   function super_line (tcompt : compt) return Boolean;

   function goto_numeral (n : Integer; x : Unbounded_String) return Integer;

   procedure Get_Param (tcompt       : compt;
                        n            : Integer;
                        value        : in out Long_Float;
                        value_string : in out Unbounded_String;
                        u1, prefix   : in out Character;
                        alt_param    : in out Boolean);

   function Eng_Prefix (c : Character) return Long_Float;

end pfun1;
