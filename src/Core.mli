(******************************************************************************)
(*                                                                            *)
(*  This file is part of fontforge-of-ocaml library                           *)
(*                                                                            *)
(*  Copyright (C) 2017-2022, Patrick BAUDIN                                   *)
(*                           (https://github.com/pbaudin/fontforge-of-ocaml)  *)
(*                                                                            *)
(*  you can redistribute it and/or modify it under the terms of the GNU       *)
(*  Lesser General Public License as published by the Free Software           *)
(*  Foundation, version 2.1.                                                  *)
(*                                                                            *)
(*  It is distributed in the hope that it will be useful, but WITHOUT ANY     *)
(*  WARRANTY; without even the implied warranty of MERCHANTABILITY or         *)
(*  FITNESS FOR A PARTICULAR PURPOSE.                                         *)
(*                                                                            *)
(*  See the GNU Lesser General Public License version 2.1                     *)
(*  for more details (enclosed in the file licenses/LGPLv2.1)                 *)
(*                                                                            *)
(******************************************************************************)

(** {1 Fontforge} *)
(***********************************************************)
type coord = float * float
(***********************************************************)
(** {2 Attributes} *)
(***********************************************************)
module type Attr = sig
  type t
  type attr
  val name: string
  val get: t -> attr
  val set: t -> attr -> unit
end
(***********************************************************)
(** {2 Tranformation Matrix} *)
(***********************************************************)
(** [PsMat] which provides quick access to some useful transformations expressed as PostScript matrices.
    API: complete (compared to the Python API) except that the type [PsMat.t] become abstract. *)
module PsMat: sig
  type t
  (** Abstract type for transform PostScript matrices *)

  val identity: unit -> t
  (** Returns an identity matrix *)
  val compose: mat1:t -> t -> t
  (** Returns a matrix which is the composition of the two input transformations *)
  val inverse: t -> t
  (** Returns a matrix which is the inverse of the input transformation. (Note: There will not always be an inverse) *)
  val rotate: float -> t
  (** Returns a matrix which will rotate by theta. Theta is expressed in radians *)
  val scale: ?y:float -> float -> t
  (** Returns a matrix which will scale by [x] in the horizontal direction and y in the vertical.
      If [y] is omitted, it will scale by the same amount [x] in both directions *)
  val skew: float -> t
  (** Returns a matrix which will skew by theta (to produce a oblique font). Theta is expressed in radians *)
  val translate: x:float -> y:float -> t
  (** Returns a matrix which will translate by x in the horizontal direction and y in the vertical *)
end
(***********************************************************)
(** {2 Point} *)
(***********************************************************)
(** [Point]
    API: complete (compared to the Python API) except the Pickling Method [reduce]. *)
module Point: sig
  type t
  (** Abstract type for points *)

  (** {3 Methods} *)

  val dup: t -> t
  (** Returns a copy of the current point. *)
  val transform: matrix:PsMat.t -> t -> unit
  (** Transforms the point by the transformation matrix. *)

(** TODO
  - val reduce:
*)

  (** {3 Attributes} *)

  (** Submodules giving read and write accesses to FontForge point attributes. *)

  module type BoolAttr  = Attr with type t = t and type attr = bool
  module type FloatAttr = Attr with type t = t and type attr = float

  module X: FloatAttr
  (** The [x] location of the point. *)
  module Y: FloatAttr
  (** The [y] location of the point. *)
  module On_curve: BoolAttr
  (** Whether this is an on curve point or an off curve point (a control point). *)
  module Selected: BoolAttr
  (** Whether this point is selected in the UI.
      If an off-curve point is selected in means the preceding (interpolated) on-curve point is selected. *)
end

(***********************************************************)
(** {2 Layer} *)
(***********************************************************)
(** A [Layer] is a collection of Contours.
    All the contours must be the same order (all quadratic or all cubic). Currently layers do not contain references.
    Layers may be compared to see if their contours are similar.
    API: TODO *)
module Layer: sig
  type t
  (** Abstract type for layers *)
end
(***********************************************************)
(* {2 Glyph Pen} *)
(***********************************************************)
(** [GlyphPen] Protocol to draw into a [Glyph]
    You create a glyphPen with the [GlyphPen] function of a glyph.
    You then draw into it with the functions below.
    API: complete (compared to the Python API); a [finalize] function has been added. *)
module GlyphPen: sig
  type t
  (** Abstract type for glyph pens. *)

  (** {3 Python workarounds} *)

  val finalize: t -> unit
  (** Finalize the pen (equivalent to [pen = None] at the Python side).
      This tells FontForge that the drawing is done and causes it to refresh the display (if a UI is active).
      Note: nothing is performed in the OCaml implementation
   *)

  (** {3 Methods} *)

  val moveTo: x:int -> y:int -> t -> unit
  (** With one exception this call begins every contour and creates an on curve point at (x,y) as the start point of that contour.
      This should be the first call after a pen has been created and the call that follows a closePath, endPath. *)
  val moveToCoord: coord -> t -> unit
  (** Idem [moveTo] with float coords. *)
  val lineTo: x:int -> y:int -> t -> unit
  (** Draws a line from the last point to (x,y) and adds that to the contour. *)
  val lineToCoord: coord -> t -> unit
  (** Idem [lineTo] with float coords. *)
  val curveTo: cp1:coord -> ?cp2:coord -> pt:coord -> t -> unit
  (** This routine has slightly different arguments depending on the type of the font.
      When drawing into a cubic font (PostScript) use the first set of arguments (with two control points -- off curve points -- between each on curve point).
      When drawing into a quadratic font (TrueType) use the second format  (when the optional [cp2] is specified) with one control point between adjacent on-curve points.
      The standard appears to support super-bezier curves with more than two control points between on-curve points.
      FontForge does not. Nor does FontForge allow you to draw a quadratic spline into a cubic font, nor vice versa. *)
  val qcurveTo: cps:coord list -> ?pt:coord -> t -> unit
  (** This routine may only be used in quadratic (TrueType) fonts and has two different formats.
      It is used to express the TrueType idiom where an on-curve point mid-way between its control points may be omitted, leading to a run of off-curve points (with implied but unspecified on-curve points between them).
      The first format (when the optional [pt] is specified) allows an arbetary number of off-curve points followed by one on-curve point.
      It is possible to have a contour which consists solely of off-curve points.
      When this happens the contour is NOT started with a moveTo, instead the entire contour, all the off curve points, are listed in one call, and the argument list is terminated by a None to indicate there are no on-curve points. *)
  val closePath: t -> unit
  (** Closes the contour (connects the last point to the first point to make a loop) and ends it. *)
  val endPath: t -> unit
  (** Ends the contour without closing it.
      This is only relevant if you are stroking contours. *)
  val addComponent: glyphname:string -> transform:PsMat.t -> t -> unit
  (** Adds a reference (a component) to the glyph.
      The PostScript transformation matrix is a 6 element tuple. *)
end
(***********************************************************)
(* {2 Contour} *)
(***********************************************************)
(** A [Contour] is a collection of points. A contour may be either based on cubic or quadratic splines.

    If based on cubic splines there should be either 0 or 2 off-curve points between every two on-curve points.
    If there are no off-curve points then we have a line between those two points. If there are 2 off-curve points we have a cubic bezier curve between the two end points.

    If based on quadratic splines things are more complex.
    Again, two adjacent on-curve points yield a line between those points.
    Two on-curve points with an off-curve point between them yields a quadratic bezier curve.
    However if there are two adjacent off-curve points then an on-curve point will be interpolated between them.
    (This should be familiar to anyone who has read the truetype 'glyf' table docs).

    For examples of what these splines can look like see the section on bezier curves.

    A contour may be open in which case it is just a long wiggly line, or closed when it is more like a circle with an inside and an outside.
    Unless you are making stroked fonts all your contours should eventually be closed.

    Contours may also be expressed in terms of Raph Levien's spiro points.
    This is an alternate representation for the contour, and is not always available (Only if fontforge.hasSpiro() is True.
    If available the spiro member will return a tuple of spiro control points, while assigning to this member will change the shape of the contour to match the new spiros.

    Two contours may be compared to see if they describe similar paths.

   API: uncomplete (compared to the Python API) *)
module Contour: sig
  type t
  (** Abstract type for contours *)

  (** {3 Python workarounds} *)

  exception Index_out_of_bounds
  val nth: int -> t -> Point.t
  (** Related to the python expression: [c[i]]. The ith point on the contour. You may assign to this using [set_nth].
      @raises [Index_out_of_bounds]. *)
  val set_nth: int -> Point.t -> t -> unit
  (** Related to the python assignement: [c[i] = pt].
      @raises [Index_out_of_bounds]. *)
  val len: t -> int
  (** The number of points in the contour. *)

  val extract: min:int -> max:int -> t -> t
  (** Related to the python expression: [c[min:max]]. The contour containing points between i and j excluded. *)
  val add_contour: t -> t -> t
  (** Related to the python expression: [c+d]. A contour concatenating c and d (another contour). *)
  val add_point: Point.t -> t -> t
  (** Related to the python expression: [c+d]. A contour concatenating c and d (a point). *)
  val append_contour: t -> t -> unit
  (** Related to the python assignement: [c+=d]. Appends d (another contour) to c. *)
  val append_point: Point.t -> t -> unit
  (** Related to the python assignement: [c+=d]. Appends d (a point) to c. *)

  val to_list: t -> Point.t list
  (** Returns the point list of the contour. *)

  (** {4 Iterators} *)

  val iter: f:(Point.t -> unit) -> t -> unit
  (** Iterates on the contour points. *)
  val iteri: f:(int -> Point.t -> unit) -> t -> unit
  val map: f:(Point.t -> 'a) -> t -> 'a list
  val mapi: f:(int -> Point.t -> 'a) -> t -> 'a list
  val fold: init:'a -> f:('a -> Point.t -> 'a) -> t -> 'a

  (** {3 Methods} *)

  val contains_point: Point.t -> t -> bool
  (* Related to the python expression: p in c.
     Returns whether the point p is in the contour c. *)
  val contains_coord: coord -> t -> bool
  (* Related to the python expression: p in c.
     Returns whether the point (x,y) is in the contour c. *)

  val dup: t -> t
  (** Returns a deep copy of the contour. That is, it copies the points that make up the contour. *)
  val isEmpty: t -> bool
  (* Returns whether the contour is empty (contains no points). *)
  val moveTo: x:int -> y:int -> t -> unit
  (** Adds an initial, on-curve point at (x,y) to the contour. *)
  val moveToCoord: coord -> t -> unit
  (** Idem [moveTo] with float coords. *)
  val lineTo: x:int -> y:int -> ?nth:int -> t -> unit
  (** Adds an line to the contour.
      If the optional third argument is given, the line will be added after the pos'th point, otherwise it will be at the end of the contour. *)
  val lineToCoord: coord -> ?nth:int -> t -> unit
  (** Idem [LineTo] with float coords. *)
  val cubicToCoord: cp1:coord -> cp2:coord -> pt:coord -> ?nth:int -> t -> unit
  (** Adds a cubic curve to the contour (requires Is_quadratic set to false).
      If the optional fourth argument is give, the line will be added after the pos'th point, otherwise it will be at the end of the contour.*)
  val cubicTo: cpx1:int -> cpy1:int -> cpx2:int -> cpy2:int -> x:int -> y:int  -> ?nth:int -> t -> unit
  (** Idem [cubicToCoord] with int coords. *)
  val quadraticToCoord: cp:coord -> pt:coord -> ?nth:int -> t -> unit
  (** Adds a quadratic curve to the contour (requires Is_quadratic set to true).
      If the optional third argument is give, the line will be added after the pos'th point, otherwise it will be at the end of the contour. *)
  val quadraticTo: cpx:int -> cpy:int -> x:int -> y:int -> ?nth:int -> t -> unit
  (** Idem [quadraticToCoord] with int coords. *)
  val insertPtCoord: pt:coord -> ?onCurve:bool -> ?nth:int -> t -> unit
  (** Adds point to the contour.
      If the optional third argument is give, the line will be added after the pos'th point, otherwise it will be at the end of the contour.
      The point may be either a point or a tuple with three members (x,y,on_curve) *)
  val insertPt: x:int -> y:int -> ?onCurve:bool -> ?nth:int -> t -> unit
  (** Idem [insertPtCoord] with int coords. *)
  val insertPoint: pt:Point.t -> ?nth:int -> t -> unit
  (** Idem [insertPt]. *)

  val makeFirst: int -> t -> unit
  (** Rotate the point list so that the pos'th point becomes the first point. *)
  val isClockwise: t -> int
  (** Returns whether the contour is drawn in a clockwise direction.
      A return value of -1 indicates that no consistant direction could be found (the contour self-intersects). *)
  val reverseDirection: t -> unit
  (** Reverse the order in which the contour is drawn (turns a clockwise contour into a counter-clockwise one).
      See also layer.correctDirection. *)

(** TODO
  - val similar
  - val xBoundsAtY
  - val yBoundsAtX
  - val addExtrema
  - val cluster
*)

  val merge: int list -> t -> unit
  (** Removes the on-curve point a the given position and rearranges the other points to make the curve as similar to the original as possible. 
      All of the listed position will be removed.
      See Also simplify. *)

  val round: ?factor:float -> t -> unit
  (** Rounds the x and y coordinates.
      If factor is specified then new-coord = round(factor*old-coord)/factor.
      See Also cluster *)

  val selfIntersects: t -> bool
  (** Returns whether this contour intersects itself. *)

(** TODO
  - val simplify
 *)

  val transform: matrix:PsMat.t -> t -> unit
  (* Transforms the point by the transformation matrix. *)

(** TODO
  - boundingBox
  - getSplineAfterPoint
*)

  val draw: GlyphPen.t -> t -> unit
  (* Draw the contour to the pen argument. *)

(** TODO
  - val reduce
 *)

  (** {3 Attributes} *)

  (** Submodules giving read and write access to FontForge contour attributes. *)

  module type BoolAttr  = Attr with type t = t and type attr = bool
  module type StringAttr = Attr with type t = t and type attr = string

  module Is_quadratic: BoolAttr
  (** Whether the contour should be interpretted as a set of quadratic or cubic splines.
      Setting this value has the side effect of converting the point list to the appropriate format. *)
  module Closed: BoolAttr
  (** Whether the contour is open or closed. *)
  module Name: StringAttr
  (** The contour name (generally there is no name). *)

(** TODO
  module Spiro
*)

end
(***********************************************************)
(** {2 Glyph} *)
(***********************************************************)
(** [Glyph] refers to a fontforge Glyph object.
    It has no independent life of its own, it always lives within a font.
    It has all the things you expect to be associated with a glyph: a glyph name, a unicode encoding, a drawing layer, GPOS/GSUB features...
    This type may not be pickled.
    This type may not be created directly -- all glyphs are bound to a font and must be created through the font.
    See [Font.createChar] function.
    API: partialy implemented (compared to the Python API) but all members and functions are listed. *)
module rec Glyph: sig
  type t
  (* Abstract type for glyphs. *)

  (** {3 Methods} *)

(** TODO
    - val addAnchorPoint
*)

  val addExtrema: ?flags:string -> ?emsize:int -> t -> unit
  (** Extrema should be marked by on-curve points.
      If a curve lacks a point at an extrema this command will add one.
      Flags may be one of the following strings:
      - all          - Add all missing extrema
      - only_good    - Only add extrema on longer splines (with respect to the em-size). Default flag.
      - only_good_rm - As above but also merge away on-curve points which are very close to, but not on, an added extremum. *)
  val addReference: glyphname:string -> ?transform:PsMat.t -> t -> unit
  (** Adds a reference to the specified glyph into the current glyph.
      Optionally specifying a transformation matrix *)
  val addHint: is_vertical:bool -> start:float -> width:float -> t -> unit
  (** Adds a postscript hint. Takes a boolean flag indicating whether the hint is horizontal or vertical, a start location and the hint's width. *)
  val addPosSub: ((LookupTables.Table.lookup_pos_sub_t * 'b) * 'b LookupTables.Table.args_t) LookupTables.Table.lookup_subtable_t -> 'b LookupTables.Table.args_t -> t -> unit
  (** Adds position/substitution data to the glyph.
      The number and type of the arguments vary acording to the type of the lookup containing the subtable.
      The first argument should always be a lookup subtable name.
      If the lookup is for single substitutions then the second argument should be a string containing a single glyph name.
      For multiple and alternated substitutions a tuple of glyph names. For ligatures, a tuple of the ligature components (glyph names).
      For single positionings the second through fifth arguments should be small integers representing the adjustment along the appropriate axis.
      For pairwise positionings (kerning) the second argument should be the name of the other glyph being kerned with, and the third through tenth should be small integers – or, if there are exactly three arguments then the third specifies traditional, one-axis, kerning.
      If there is a previously existing entry, this will replace it (except for ligatures).
  *)

(** TODO
  - val appendAccent
*)

  val autoHint: t -> unit
  (** Generates PostScript hints for this glyph. *)
  val autoInstr: t -> unit
  (** Generates TrueType instructions for this glyph. *)
  val autoTrace: t -> unit
  (** Auto traces any background images. *)
  val build: t -> unit
  (** If the character is a composite character, then clears it and inserts references to its components. *)
  val canonicalContours: t -> unit
  (** Orders the contours in the current glyph by the x coordinate of their leftmost point.
     This can reduce the size of the charstring needed to describe the glyph(s). *)
  val canonicalStart: t -> unit
  (** Sets the start point of all the contours of the current glyph to be the leftmost point on the contour.
     If there are several points with that value then use the one which is closest to the baseline.
     This can reduce the size of the charstring needed to describe the glyph(s).
     By regularizing things it can also make more things available to be put in subroutines. *)

(** TODO
  - val changeWeight
*)

  val condenseExtend: c_factor:float -> c_add:float -> ?sb_factor:float -> ?sb_add:float -> ?correct:bool -> t -> unit
  (** Condenses or extends the size of the counters and side-bearings of the glyph.
      The first two arguments provide information on shrinking/growing the counters, the second two the sidebearings.
      If the last two are omitted they default to the same values as the first two.
      A counter's width will become:
      new_width = c_factor * old_width + c_add

      If present the correct argument allows you to specify whether you want to correct for the italic angle before condensing the glyph. (it defaults to True). *)
  val clear: t -> unit
  (** Clears the contents of the glyph (and marks it as not worth outputting). *)
  val cluster: ?within:float -> ?max:float -> t -> unit
  (** Moves clustered coordinates to a standard central value.
      See also round. *)
  val correctDirection: t -> unit
  (** Orients all contours so that external ones are clockwise and internal counter-clockwise. *)
  val exclude: Layer.t -> t -> unit
  (** Removes the excluded area from the current glyph. Takes an argument which is a layer.
      See also removeOverlap and intersect. *)

(** TODO
  - val export
  - val getPosSub: string -> t -> ??
*)

  val importOutlines: filename:string -> ?flags:string list -> t -> unit
  (** Uses the file's extension to determine behavior.
      Imports outline descriptions (eps, svg, glif files) into the forground layer.
      Imports image descriptions (bmp, png, xbm, etc.) into the background layer.
      Optionally, flags can be used to control PostScript import, it'll be ignored for other file types.
      Flags is a list of strings ? *)
  val intersect: t -> unit
  (** Leaves only areas in the intersection of contours. See also removeOverlap and exclude. *)
  val isWorthOutputting: t -> bool
  (** Returns whether the glyph is worth outputting into a font file.
      Basically a glyph is worth outputting if it contains any contours, or references or has had its width set. *)
  val preserveLayerAsUndo: ?layer_dohints:(int * bool) -> t -> unit
  (** Normally undo handling is turned off during python scripting.
      If you wish you may tell fontforge to preserve the current state of a layer so that whatever you do later can be undone by the user.
      You may omit the layer parameter (in which case the currently active layer will be used).
      You may also request that hints be preserved (they are not, by default).*)
  val removeOverlap: t -> unit
  (** Removes overlapping areas. See also intersect and exclude. *)
  val removePosSub: name:string -> t -> unit
  (** Removes all data from the glyph corresponding to the given lookup-subtable. If the name is "*" then all data will be removed. *)
  val round: ?factor:int -> t -> unit
  (** Rounds the x and y coordinates of each point in the glyph.
      If factor is specified then new-coord = round(factor*old-coord)/factor.
      See also cluster. *)
  val selfIntersects: t -> bool
  (** Returns whether any of the contours in this glyph intersects any other contour in the glyph (including itself). *)
  val transform: matrix:PsMat.t -> ?flags:string list -> t -> unit
  (** Transforms the glyph by the matrix.
      The optional flags argument should be a tuple containing any of the following strings:
      - partialRefs - Don't transform any references in the glyph, but do transform their offsets.
                      This is useful if the refered glyph will be (or has been) transformed.
      - round       - Round to int after the transformation is done.*)
  val nltransform: xexpr:string -> yexpr:string -> t -> unit
  (** xexpr and yexpr are strings specifying non-linear transformations that will be applied to all points in the current layer (with xexpr being applied to x values, and yexpr to y values, of course). 
      The syntax for the expressions is explained in the non-linear transform dialog. *)
  val unlinkRef: ?refname:string -> t -> unit
  (** Unlinks the reference to the glyph named ref-name.
      If ref-name is omitted, unlinks all references. *)
  val unlinkThisGlyph: t -> unit
  (** Unlinks all the references to the current glyph within any other glyph in the font. *)
  val useRefsMetrics: name:string -> ?flag:bool -> t -> unit
  (** Finds a reference with the given name and sets the "use_my_metrics" flag on it (so this glyph will have the same advance width as the glyph the reference points to).
      If the optional flag argument is False, then the glyph will no longer have its metrics bound to the reference.*)
  val validate: ?force:bool -> t -> int
  (** Validates the glyph and returns the validation_state of the glyph (except bit 0x1 will always be clear).
      If the glyph passed the validation then the return value will be 0 (not 0x1).
      Otherwise the return value will be the set of errors found.
      If force is specified true this will always be validated, if force is unspecified (or specified as false) then it will return the cached value if it is known, otherwise will validate it. *)
  val draw: GlyphPen.t -> t -> unit
  (** Draw the glyph's outline to the pen argument. *)
  val glyphPen: ?replace:bool -> t -> GlyphPen.t
  (** Creates a new glyphPen which will draw into the current glyph.
      By default the pen will replace any existing contours and references, but setting the optional keyword argument, replace to false will retain the old contents.*)

  (** {3 Attributes} *)

  (** Submodules giving read and write access to FontForge glyph attributes.
      Readonly attributes are implemented as functions. *)

  module type BoolAttr = Attr with type t = t and type attr = bool
  module type IntAttr = Attr with type t = t and type attr = int
  module type StringAttr = Attr with type t = t and type attr = string

  module ActiveLayer: IntAttr
  (** Returns currently active layer in the glyph (as an integer).
      May be set to an integer or a layer name to change the active layer.
      Not implemented: set the attribute from a layer name *)

(** TODO
  - module Atuni: ???Attr
  - module AnchorPoints: ???Attr
  - module AnchorPointsWithSel: ???Attr
  - module Background: ???Attr
*)

  module Changed: BoolAttr
  (* Whether this glyph has been modified.
     This is (should be) maintained automatically, but you may set it if you wish. *)
  module Color: IntAttr
  (* The color of the glyph in the fontview.
     A 6 hex-digit RGB number or -1 for default.
     0xffffff is white, 0x0000ff is blue, etc. *)

(** TODO
  - module Comments: UTF8Attr
  - module Dhints: FloatTuple2Tuple3ListAttr
*)

  val encoding: t -> int
  (** Returns the glyph's encoding in the font's encoding. (readonly)
      If the glyph has multiple encodings, one will be picked at random.
      If the glyph is not in the font's encoding then a number will be returned beyond the encoding size (or in some cases -1 will be returned).
      Note: an Ocaml [IntAttr] module is not used there since the Python attribute [font] is read only. *)
  val font: t -> Font.t
  (** The font containing this glyph.
      Note: an Ocaml [FontAttr] module is not used there since the Python attribute [font] is read only. *)

(** TODO
  - module Foreground: ??Attr
*)

  module Glyphclass: StringAttr
  (* An opentype glyphclass, one of automatic, noclass, baseglyph, baseligature, mark, component. *)
  module Glyphname: StringAttr
  (* The name of the glyph. *)

(** TODO
  - module Hhints: FloatTuple2ListAttr
  - module HorizontalComponents: ??Attr
  - module HorizontalComponentItalicCorrection	: ??Attr
*)

  (* module HorizontalVariants: ??Attr *)
  (* A boolean containing the MATH "is extended shape" field. *)
  module IsExtendedShape: BoolAttr
  (* The glyph's italic correction field. Used by both TeX and MATH.
     The special value of -32768 (0x8000) means the value is unspecified.
     An unspecified value will not go into the output tables, a value of 0 will. *)
  module ItalicCorrection: IntAttr

  val layer_cnt: t -> int
  (** The number of layers in this glyph.
     Cannot be set.
     Note: an Ocaml [IntAttr] module is not used there since the Python attribute [font] is read only. *)

(** TODO  seems to be readonly
  - module Layers: ??Attr
  - module Layerrefs: ??Attr
  - module Lcarets: ??Attr
*)

  module Left_side_bearing: IntAttr
  (* The left side bearing of the glyph. *)

(** TODO
  - module ManualHints: ??Attr
  - module MathKern: ??Attr
  - module Persistant: ??Attr
  - module References: ??Attr
*)

  module Right_side_bearing: IntAttr
  (* The right side bearing of the glyph. *)

(** TODO
  - module Temporary: ??Attr
*)

  module Texheight: IntAttr
  (** The Tex height.
      The special value of -32768 (0x8000) means the field is unspecified.
      An unspecified value will not go into the output tables, a value of 0 will. *)
  module Texdepth: IntAttr
  (** The Tex depth.
      The special value of -32768 (0x8000) means the field is unspecified.
      An unspecified value will not go into the output tables, a value of 0 will. *)
  module Topaccent: IntAttr
  (** The glyph's top accent position field. Used by MATH.
      The special value of -32768 (0x8000) means the field is unspecified.
      An unspecified value will not go into the output tables, a value of 0 will. *)

(** TODO
  - module Ttinstrs: ??Attr
*)

  module Unicode: IntAttr
  (** The glyph's unicode code point, or -1 *)
  module UnlinkRmOvrlpSave: IntAttr
  (** A flag that indicates the glyph's references should be unlinked and remove overlap run on it before the font is saved (and then the original references replaced after the save finishes). *)

(** TODO
  - module VerticalComponents: ??Attr
  - module VerticalComponentsItalicCorrection: ??Attr
  - module VerticalVariants: ??Attr
  - module Vhints: ??Attr
*)

  (* The vertical advance width of the glyph. See also width. *)
  module Vwidth: IntAttr
  (* The advance width of the glyph. See also vwidth. *)
  module Width: IntAttr
  (* The GID of this glyph in the font it was read from. (readonly) *)

  val originalgid: t -> int

  val script: t -> string
  (** A string containing the OpenType 4 letter tag for the script associated with this glyph (readonly). *)

  val validation_state: t -> int
  (** A (readonly) bit mask indicating some problems this glyph might have.
     0x1        If set then this glyph has been validated.
                If unset then other bits are meaningless.
     0x2        Glyph has an open contour.
     0x4        Glyph intersects itself somewhere.
     0x8        At least one contour is drawn in the wrong direction
     0x10       At least one reference in the glyph has been flipped
                (and so is drawn in the wrong direction)
     0x20       Missing extrema
     0x40       A glyph name referred to from this glyph, in an opentype table,
                is not present in the font.
     0x80       PostScript has a limit of 1500 points in a glyph.
     0x100      PostScript has a limit of 96 hints in a glyph.
     0x200      Invalid glyph name.
                TrueType only, errors in original file
     0x400      More points in a glyph than allowed in 'maxp'
     0x800      More paths in a glyph than allowed in 'maxp'
     0x1000     More points in a composite glyph than allowed in 'maxp'
     0x2000     More paths in a composite glyph than allowed in 'maxp'
     0x4000     Instructions longer than allowed in 'maxp'
     0x8000     More references in a glyph than allowed in 'maxp'
     0x10000    References nested more deeply than allowed in 'maxp'
     0x40000    Points too far apart. TrueType and Type2 fonts are limited to 16 bit numbers,
                and so adjacent points must be within 32767 em-units of each other.
     0x80000    Points non-integral. TrueType points and control points must be integer aligned.
                (FontForge will round them if they aren't)
     0x100000   Missing anchor. According to the opentype spec, if a glyph contains an anchor
                point for one anchor class in a subtable, it must contain anchor points for all
                anchor classes in the subtable.
                Even it, logically, they do not apply and are unnecessary.
     0x200000   Duplicate glyph name. Two (or more) glyphs in this font have the same name.
                When outputting a PostScript font only one of them will ever be seen.
                It's a little hard to detect this in normal use, but if you change the encoding
                to "Glyph Order", and then use Edit->Select->Wildcard and enter the glyph name,
                both of them should be selected.
     0x400000   Duplicate unicode code point. Two (or more) glyphs in this font have the code point.
                When outputting an sfnt (TrueType/OpenType) font only one of them will ever be seen.
                It's a little hard to detect this in normal use, but if you change the encoding
                to "Glyph Order", and then use Edit->Select->Wildcard and enter the code point,
                both of them should be selected.
     0x800000   Overlapped hints. Either the glyph has no hint masks and there are overlapped
                hints, or a hint mask specifies two overlapping hints. *)


end
(***********************************************************)
(** {2 Selection} *)
(***********************************************************)
(** [Selection] of glyphs.
   API: complete with some specificities about the selection requests and the iterations (compared to the Python API). *)
and Selection: sig
  type t
  (* Abstract type for contours *)

  (** {3 Python workarounds} *)

  val byGlyphs: t -> t
  (* Returns another selection, just the same as this one except that its iterator function
     will return glyphs (rather than encoding slots) and will only return those entries for which glyphs exist.
     Note: an Ocaml [FFmember]  module is not used there since the Python attribute [byGlyphs] is read only. *)

  exception UnspecifiedSlotIteration
  exception UnspecifiedGlyphIteration
  val iter: ?slot:(int -> unit) -> ?glyph:(Glyph.t -> unit) -> t -> unit
  (* Iterates on selected item using one of the two function depending on the current iteration mode (see [byGlyphs]).
     Raises [UnspecifiedSlotIteration] when the iteration is over encoding slots and the [slot] function is unspecified.
     Raises [UnspecifiedGlyphIteration] when the iteration is over glyphs and the [glyph] function is unspecified.
     Related to efficiency, [max] specifies the maximal number of items returned from the Python side in one call to [next_items]. *)

  type request_t
  (* Abstract type for beeing able to build selection requests. *)
  val glyphname: string -> request_t
  (* Build a selection request from a glyph [name]. *)
  val glyph: Glyph.t -> request_t
  (* Build a selection request from a fontforge [glyph]. *)
  val code: int -> request_t
  (* Build a selection request from the [code] either an encoding index or (default) a unicode code point depending on the flags. *) 

  val select: ?encoding:bool -> ?less:bool -> ?ranges:bool -> request:request_t list -> t -> unit
  (* Select specified items depending on specified flags:
     - ?encoding=false (default) -> "unicode" - Interpret specified code items as unicode code points
     - ?encoding=true            -> "encoding"- Interpret specified code items as encoding indeces.

     - ?less=unspecified (default) -> "None" - Would produce a selection from scratch using specified items
     - ?less=false                 -> "more" - Would add specified items to the current selection
     - ?less=true                  -> "less" - Would remove specified items from the current selection

     - ?ranges=false (default) -> "singletons" - Specified items should be interpreted individually and mean the obvious.
     - ?ranges=true            -> "ranges"- Specified items should be interpreted in pairs and represent all encoding slots between the start and end points specified by the pair. 
   *)

  val select_from_encoding: encoding:int -> t -> unit
  (* Shortcut: Select from an unicode. *)
  val select_from_unicode: unicode:int -> t -> unit
  (* Shortcut: Select from an unicode. *)
  val select_from_glyphname: name:string -> t -> unit
  (* Shortcut: Select from a glyph name. *)

  val all: t -> unit
  (** Select everything. *)
  val none: t -> unit
  (** Deselect everything. *)
  val changed: t -> unit
  (** Select all glyphs which have changed. *)
  val invert: t -> unit
  (** Invert the selection. *)

end
(***********************************************************)
(* {2 Font} *)
(***********************************************************)
(** FontForge [Font] object.
    It generally contains a list of glyphs, an encoding to order those glyphs, a fontname, a list of GPOS/GSUB lookups and many other things.
    API: almost complete (all listed even the unimplemented ones) compared to the Python API.
*)
and Font: sig
  type t
  (** Abstract type for fonts *)

  (** {3 Python workarounds} *)

  val contains_glyphname: glyphname:string -> t -> bool
  (** Equivalent to the Python expression: [glyphname in font].
      Returns whether the font contains a glyph with the given name. *)
  val contains_unicode: unicode:int -> t -> bool
  (** Equivalent to the Python expression: unicode in font.
      Returns whether the font contains a glyph with that encoding. *)

  val glyph_from_name: glyphname:string -> t -> Glyph.t
  (** Equivalent to the Python access [font[glyphname]] from a string.
      Returns the glyph with that name.*)
  val glyph_from_code: unicode:int -> t -> Glyph.t
  (** Equivalent to the Python access [font[unicode]] from an integer.
      Returns the glyph at that encoding. *)

  (** {3 Methods} *)

  val addAnchorClass: subtable:string -> anchorclass:string -> t -> unit
  (** Adds an anchor class to the specified (anchor) subtable. *)

(** TODO
  - gpos_pair
  - val addKerningClass
*)

  type lookup_script_t
  (** Abstract type for script languages of lookup features *)
  val lookupScript: script:string -> languages:string list -> lookup_script_t
  (** i.e. [let script = lookupScript ~script:"latn" ~languages:["dflt"]] *)
  type lookup_feature_t
  (** Abstract type for lookup features *)
  val lookupFeature: feature:string -> scripts:lookup_script_t list -> lookup_feature_t
  (** i.e. [let feature = lookupFeature feature:"liga" scripts:[script]] *)
  val addLookup: 'a LookupTables.Table.lookup_table_t -> 'a LookupTables.Table.t ->
    flags:string list -> 'a LookupTables.TypedFeature.t -> ?others:'a LookupTables.TypedFeature.t list -> ?after_lookupname:string -> t -> unit
  (** Creates a new lookup with the given name, type and flags. It will tag it with any indicated features. The type of one of
      - gsub_single
      - gsub_multiple
      - gsub_alternate
      - gsub_ligature
      - gsub_context
      - gsub_contextchain
      - gsub_revesechain
      - morx_indic
      - morx_context
      - morx_insert
      - gpos_single
      - gpos_pair
      - gpos_cursive
      - gpos_mark2base
      - gpos_mark2ligature
      - gpos_mark2mark
      - gpos_context
      - gpos_contextchain
      - kern_statemachine

      The flags argument is a tuple of strings. At most one of these strings may be the name of a mark class.
      The others are:
      - right_to_left
      - ignore_bases
      - ignore_ligatures
      - ignore_marks

      A feature-script-lang tuple is a tuple with one entry for each feature (there may be no entries if there are no features).
      Each entry is itself a two element tuple, the first entry is a string containing a 4 letter feature tag, and the second entry is another tuple (potentially empty) with an entry for each script for which the feature is active.
      Each entry here is itself a two element tuple.
      The first element is a 4 letter script tag and the second is a tuple of languages. Each entry in the language tuple is a four letter language.
      - Example: (("liga",(("latn",("dflt")),)),)
      The optional final argument allows you to specify the ordering of the lookup. If not specified the lookup will be come the first lookup in its table. *)
  val addLookupSubtable: 'a LookupTables.Table.lookup_subtable_t  -> ?after_subtable:string -> t -> unit
  (** Creates a new subtable within the specified lookup.
      The lookup name should be a string specifying an existing lookup.
      The subtable name should also be a string and should not match any currently existing subtable in the lookup.
      The optional final argument allows you to specify the ordering within the lookup.
      If not specified this subtable will be first in the lookup.
      If you want to create a subtable in a contextual lookup, then use addContextualSubtable below.
      If you want to create a kerning class subtable, then use addKerningClass above. *)

(** TODO
    - val addContextualSubtable: lookupname:string -> subtable:string -> subtabletype:string -> rule: ??
     -> ?afterSubtable:string -> ?bclasses:string -> mclasses: string -> ?fclasses:string -> ?bclassnames:string -> ?mclassnames:string -> ?fclassnames:string
     -> t -> unit
    - addSmallCaps
    - alterKerningClass
    - autoKern
    - appendSFNTName
*)

  val buildOrReplaceAALTFeatures: t -> unit
  (** Removes any existing AALT features (and any lookups solely controled by such features) and creates new ones containing all possible single and alternate substutions available for each glyph. *)
  val cidConvertByCMap: filename:string -> t -> unit
  (** Converts a normal font into a CID-keyed font with one subfont using
     the CMAP to determine the mapping. *)

(** TODO
    - cidConvertTo
*)

  val cidFlatten: t -> unit
  (** Converts a CID font into a normal font (glyphs will be in CID order). *)
  val cidInsertBlankSubFont: t -> unit
  (** Adds a new (blank) sub-font into a cid-keyed font and changes the current sub-font to be it. *)
  val cidFlattenByCMap: filename:string -> t -> unit
  (** Converts a CID font into a normal font (glyphs will be in CID order). *)
  val cidRemoveSubFont: t -> unit
  (** Removes the current subfont from a cid-keyed font. *)
  val close: t -> unit
  (** Frees memory for the current font.
      Warning: Any python pointers to it will become invalid. *)

(** TODO
  - compareFonts
*)

  val createChar: unicode:int -> ?name:string -> t -> Glyph.t
  (** Create (and return) a character at the specified [unicode] codepoint in this font and optionally [name] it.
      If you wish to create an glyph with no unicode codepoint set the first argument to -1 and specify a name.
      If there is already a character there, return it (it will not be renamed). *)

(** TODO
  - createInterpolatedGlyph
  - createMappedChar
  - find
  - indEncodingSlot
  - glyphs
*)

  val generate: filename:string -> t -> unit
  (** TODO: adds optional args for [generate] *)

(** TODO
  - generateTtc
  - generateFeatureFile
  - genericGlyphChange
  - getKerningClass
  - getLookupInfo
  - getLookupSubtables
  - getLookupSubtableAnchorClasses
  - getLookupOfSubtable
  - getSubtableOfAnchor
  - importBitmaps
  - importLookups
  - interpolateFonts
  - isKerningClass
  - isVerticalKerning
  - italicize
*)

  val lookupSetFeatureList: lookupname:string -> features:lookup_feature_t list -> t -> unit
  (** Sets the feature list of indicated lookup.
      The feature-script-lang tuple is described at [addLookup] function. *)
  val lookupSetFlags: lookupname:string -> flags:string list -> t -> unit
  (** Sets the lookup flags for the named lookup.
      At most one of these strings may be the name of a mark class.
      The others are:
      - right_to_left
      - ignore_bases
      - ignore_ligatures
      - ignore_marks *)
  val lookupSetStoreLigatureInAfm: lookupname:string -> store:bool -> t -> unit
  (** Sets whether this ligature lookup contains data to store in the afm. *)
  val mergeFonts: filename:string -> ?preserveCrossFontKerning:bool -> t -> unit
  (** Merges the font in the file into the current font. *)
  val mergeFeature: filename:string -> t -> unit
  (** Merge feature and lookup information from an adobe feature file, or metrics information from the (afm,tfm,etc) file into the current font. *)
  val mergeKern: filename:string -> t -> unit
  (** Deprecated name for mergeFeature above *)
  val mergeLookups: lookupname1:string -> lookupname2:string -> t -> unit
  (** The lookups must be of the same type.
     All subtables from [lookupname2] will be moved to [lookupname1], the features list of [lookupname2] will be merged with that of [lookupname1], and [lookupname2] will be removed. *)
  val mergeLookupSubtables: subtable1:string -> subtable2:string -> t -> unit
  (** The subtables must be in the same lookup.
     Not all lookup types allow their subtables to be merged (contextual subtables may not be merged, kerning classes may not be (kerning pairs may be)). 
     Any information bound to subtable2 will be bound to subtable1 and subtable2 will be removed. *)

(** TODO
  - printSample
*)

  val randomText: script:string -> ?lang:string -> t -> string
  (** Returns a random text sample using the letter frequencies of the specified script (and optionally language).
      Both script and language should be expressed as strings containing OpenType Script and Language tags. "dflt" is a reasonable language tag. 
      If the language is not specified, one will be chosen at random.
      If FontForge has no frequency information for the script/language specified it will use the letters in the script with equal frequencies. *)
  val regenBitmaps: sizes:int list -> t -> unit
  (** A tuple with an entry for each bitmap strike to be regenerated (rerasterized).
      Each strike is identified by pixelsize (if the strike is a grey scale font it will be indicated by (bitmap-depth<<16)|pixelsize. *)

  val removeAnchorClass: anchorclass:string -> t -> unit
  (** Removes the named AnchorClass (and all associated points) from the font. *)
  val removeLookup: lookup:string -> t -> unit
  (** Remove the lookup (and any subtables within it). *)
  val removeLookupSubtable: subtable:string -> t -> unit
  (** Remove the subtable (and all data associated with it). *)
  val removeGlyph: Selection.request_t -> t -> unit
  (** You may either pass in a FontForge glyph object (from this font) or identify a glyph in the font by unicode code point or name. 
     In any case the glyph will be removed from the font.
     WARNING: This frees fontforge's storage to this glyph.
     If you have any python pointers (also [FFglyph.t] value) to that storage they will be looking at garbage.
     This does not go through the usual python reference mechanism. *)
  val removeGlyph_from_unicode: unicode:int -> t -> unit
  (** Shortcut using [removeGlyph]. *)
  val removeGlyph_from_glyphname: glyphname:string -> t -> unit
  (** Shortcut using [removeGlyph]. *)
  val removeGlyph_from_glyph: glyph:Glyph.t -> t -> unit
  (** Shortcut using [removeGlyph]. *)

  type replace_spec_t
  (** Abstract type for [replaceAll] function. *)
  val from_Layer: srch:Layer.t -> rpl:Layer.t -> replace_spec_t
  (** For replacing a [FFlayer] by another one. *)
  val from_Contour: srch:Contour.t -> rpl:Contour.t -> replace_spec_t
  (** For replacing a [FFcontour] by another one. *)
  val replaceAll: spec:replace_spec_t -> ?error_bound:float -> t -> unit
  (** Searches the font for all occurences of the srch contour (or layer) and replaces them with the replace contour (or layer). *)
  val replaceAll_from_Layer: srch:Layer.t -> rpl:Layer.t -> ?error_bound:float -> t -> unit
  (** Idem [replaceAll] from a layer *)
  val replaceAll_from_Contour: srch:Contour.t -> rpl:Contour.t -> ?error_bound:float -> t -> unit
  (** Idem [replaceAll] from a contour *)

  val revert: t -> unit
  (** Reloads the font from the disk.
      Caveat: if you have any pointers to glyphs which live in the font those pointers will no longer be valid, and using them will cause crashes. 
      This is very un-python-like. *)
  val revertFromBackup: t -> unit
  (** Reloads the font from the backup file on the disk.
      Caveat: if you have any pointers to glyphs which live in the font those pointers will no longer be valid, and using them will cause crashes. 
      This is very un-python-like. *)
  val save: filename:string -> t -> unit
  (** Saves the font to an sfd file.
      See also generate() *)
  val saveNamelist: filename:string -> t -> unit
  (** Saves the font's namelist to a file. *)
  val getTableData: tablename:string -> t -> string
  (** Gets binary data from any saved table.
      FontForge will save 'fpgm', 'prep', 'cvt ' and 'maxp'.
      FontForge may also save tables which you explicitly request.
      Do not expect to get binary data for tables like 'GPOS' or 'glyf' which FontForge will generate when it creates a font... that information is not currently available.
      Returns a binary string. *)

(** TODO
  - setTableData: tablename:string -> ?? -> t -> unit
*)

  val validate: ?force:bool -> t -> int
  (** Validates the font and returns a bit mask of all errors from all glyphs (as defined in the validation_state of a glyph -- except bit 0x1 is clear). 
     If the font passed the validation then the return value will be 0 (not 0x1).
     Otherwise the return value will be the set of errors found.
     Note: The set of errors is slightly different for TrueType and PostScript output.
           The returned mask contains the list of potential errors. You must figure out which apply to you.

     Normally each glyph will cache its validation_state and it will not be recalculated.
     If you pass a non-zero argument to the routine then it will force recalculation of each glyph -- this can be slow.s *)

  val addExtrema: t -> unit
  (** Extrema should be marked by on-curve points. If a curve in any selected glyph lacks a point at a significant extremum this command will add one. *)
  val addSmallCaps: t -> unit
  (** For all selected upper or lower case letters in the latin, greek and cyrillic scripts this will try to create a small caps version of that glyph in a new glyph slot. 
      So if you select "A" (or "a") then a glyph "a.sc" will be created (if "a.sc" already exists, it will be reused, and its current contents cleared). 
      The contents of "a.sc" will be based on the upper case variant of this glyph (and that variant must be present for the command to work). 
      FontForge will also create two lookups (unless appropriate ones already exist) one, bound to the feature 'c2sc' will map upper case letters to small caps, the other, bound to feature 'smcp' will map lower case letters to small caps. *)
  val autoHint: t -> unit
  (** Generates PostScript hints for all selected glyphs. *)
  val autoInstr: t -> unit
  (** Generates TrueType instructions for all selected glyphs. *)
  val autoTrace: t -> unit
  (** Auto traces any background images in all selected glyphs. *)
  val build: t -> unit
  (** If any of the selected characters is a composite character, then this command will clear it and insert references to its components (this command can create new glyphs).  *)
  val canonicalContours: t -> unit
  (** Orders the contours in the selected glyphs by the x coordinate of their leftmost point.
      This can reduce the size of the charstring needed to describe the glyph(s). *)
  val canonicalStart: t -> unit
  (** Sets the start point of all the contours of the selected glyphs to be the leftmost point on the contour.
      If there are several points with that value then use the one which is closest to the baseline.
      This can reduce the size of the charstring needed to describe the glyph(s).
      By regularizing things it can also make more things available to be put in subroutines. *)
  val clear: t -> unit
  (** Clears the contents of all selected glyphs. *)
  val copy: t -> unit
  (** Copies all selected glyphs into (fontforge's internal) clipboard. *)
  val copyReference: t -> unit
  (** Copies all selected glyphs (as references) into (fontforge's internal) clipboard. *)
  val correctDirection: t -> unit
  (** Orients all contours so that external ones are clockwise and internal counter-clockwise in all selected glyphs. *)
  val correctReferences: t -> unit
  (** Checks a font for glyphs with mixed contours and references (or references with transformation matrices which cannot be represented truetype (ie. scaling by 2 or more)). 
      If a mixed case is discovered fontforge will take the contours out of the glyph, put them in a new glyph, and make a reference to the new glyph. *)
  val cut: t -> unit
  (** Copies all selected glyphs into (fontforge's internal) clipboard. And then clears them. *)
  val paste: t -> unit
  (** Pastes the contents of (fontforge's internal) clipboard into the selected glyphs -- and removes what was there before. *)
  val intersect: t -> unit
  (** Leaves only areas in the intersection of contours in all selected glyphs. See also removeOverlap. *)
  val pasteInto: t -> unit
  (** Pastes the contents of (fontforge's internal) clipboard into the selected glyphs -- and retains what was there before. *)
  val removeOverlap: t -> unit
  (** Removes overlapping areas in all selected glyphs. See also intersect. *)
  val replaceWithReference: ?fudge:float -> t -> unit
  (** Finds any glyph which contains an inline copy of one of the selected glyphs, and converts that copy into a reference to the appropriate glyph. 
      Selection is changed to the set of glyphs which the command alters.
      If specified the fudge argument specifies the error allowed for coordinate differences. *)
  val round: ?factor:float -> t -> unit
  (** Rounds the x and y coordinates of each point in all selected glyphs.
      If factor is specified then new-coord = round(factor*old-coord)/factor.
      See also cluster.*)

(** TODO
  - simplify
  - stroke
*)

  val transform: matrix:PsMat.t -> t -> unit
  (** Transforms all selected glyphs by the matrix. *)
  val nltransform: xexpr:string -> yexpr:string -> t -> unit
  (** xexpr and yexpr are strings specifying non-linear transformations that will be applied to all points in the selected glyphs of the font (with xexpr being applied to x values, and yexpr to y values, of course). 
     The syntax for the expressions is explained in the non-linear transform dialog. *)
  val unlinkReferences: t -> unit
  (** Unlinks all references in all selected glyphs and replaces them with splines. *)

  (** {3 Attributes} *)

  (** Submodules giving read and write access to FontForge contour attributes. *)

  module type IntAttr  = Attr with type t = t and type attr = int
  module type StringAttr = Attr with type t = t and type attr = string
  module type SelectionAttr = Attr with type t = t and type attr = Selection.t

  module Ascent    : IntAttr
  module Descent   : IntAttr
  module Em        : IntAttr

  module Encoding  : StringAttr
  module Familyname: StringAttr
  module Fontname  : StringAttr
  module Fullname  : StringAttr
  module Version   : StringAttr
  module Weight    : StringAttr

  module Selection : SelectionAttr

  module Copyright : StringAttr

end
(***********************************************************)
module FontForge: sig
  (***********************************************************)
(** TODO
  - getPrefs: ~name:string -> ??
  - setPrefs: ~name:string -> ~value:?? -> unit
*)

  val savePrefs: unit -> unit
  (** Saves the current preference settings. *)
  val loadPrefs: unit -> unit
  (** Loads the user's default preference settings.
     Not done automatically in a script. *)

  val hasSpiro: unit -> bool
  (** Returns a boolean, True if Raph Levien's spiro package is available for use in FontForge. *)
  val defaultOtherSubrs: unit -> unit
  (** Sets the type1 PostScript OtherSubrs to the default value. *)
  val readOtherSubrsFile: filename:string -> unit
  (** Sets the type1 PostScript OtherSubrs to the stuff found in the file. *)

  val loadEncodingFile: filename:string -> string option
  (** Loads an encoding file, returns the name of the encoding or None. *)
  val loadNamelist: filename:string -> unit
  (** Loads a namelist. *)
  val loadNamelistDir: dirname:string -> unit
  (** Loads all namelist files in the directory. *)
  val loadPlugin: filename:string -> unit
  (** Loads a fontforge plugin. *)
  val loadPluginDir: dirname:string -> unit
  (** Loads all fontforge plugins in the directory. *)
  val preloadCidmap: filename:string -> registry:string -> order:string -> supplement:int -> unit
  (** Loads a fontforge cidmap file. *)

(** TODO: From fontforge/python.c file, it seems that the second argument is not used for pdf-file and ps-file!
     [print method = { "lp", "lpr", "ghostview", "ps-file", "command", "pdf-file", 5 }].
  - printSetup: ?? -> unit
*)

  val nameFromUnicode: ?namelist:string -> int -> string
  (** Finds the glyph name associated with a given unicode codepoint.
      If a namelist is specified the name will be taken from that. *)
  val unicodeFromName: glyphname:string -> int
  (** Looks up glyph name in its dictionary and if it is associated with a unicode code point returns that number.
      Otherwise it returns -1. *)
  val version: unit -> string
  (** Returns fontforge's version number as a string.
      This will be a large number like 20070406. *)
  val fonts: unit -> Font.t list
  (** Returns a list of all fonts currently loaded into fontforge for editing. *)
  val activeFont: unit -> Font.t option
  (** If the script were invoked from the File->Execute Script... dialog, or invoked by a menu item in the font view, this returns the font that was active at the time. 
      Otherwise it returns None. *)
  val activeGlyph: unit -> Glyph.t option
  (** If the script were invoked from the File->Execute Script... dialog or a menu item from an outline glyph window or a glyph import/export command this returns the glyph that was active at the time. 
      Otherwise it returns None. *)
  val activeLayer: unit -> int
  (** This returns the currently active layer as an integer between 0 (inclusive) and the font/glyph's layer count (exclusive).
      It may also be set to -1 if the current glyph window is displaying the font's guidline layer. *)
  val fontsInFile: filename:string -> string list
  (** Returns the list of all fontnames found in the specified file.
      The list may be empty if fontforge couldn't find any. *)
  val openFont: filename:string -> Font.t
  (** Opens a filename and returns the font it contains. If it does.
      If the flags argument is 4, then FontForge will load all glyphs in the 'glyf' table of a ttc file (rather than just the glyphs used in the font picked). 
      This will not load all 'glyf' tables though.
      Not implemented: flags argument (So, the defaut Python value is used.).
      Note the original Python name is [open], but it is an Ocaml keyword. *)
  val parseTTInstr: string -> string
  (** Returns a binary string each byte of which corresponds to a truetype instruction.
      The input string should contain a set of instruction names as "SRP0\nMIRP[min,rnd,black]". *)
  val unParseTTInstr: string -> string
  (** Reverse of the above.
      Converts a binary string into a human (sort of) readable string. *)

(** TODO
  - unitShape: int -> ??
  - registerGlyphSeparationHook
*)

(** TODO - User Interface Methods *)

  val contour: unit -> Contour.t
  (** Creates a new contour. *)
  val font: unit -> Font.t
  (** Creates a new font. *)
  val layer: unit -> Layer.t
  (** Creates a new layer. *)
  val point: ?oncurve:bool -> ?y:float -> float -> Point.t
  (** Creates a new point.
      Optionally specifying its location. *)
  val pointCoord: ?oncurve:bool -> coord -> Point.t
  (** Idem [point] with float coords. *)

  val initialize: unit -> unit
  val finalize: unit -> unit
end
(***********************************************************)
