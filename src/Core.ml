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

(* https://fontforge.org/docs/scripting/python.html
   http://dmtr.org/ff.php
*)
open Base
open Util
(***********************************************************)
type coord = float * float
(***********************************************************)
let contour_api api = api ~typ:Value.(Object Contour)
let glyph_api api = api ~typ:Value.(Object Glyph)
let glyph_opt_api api = api ~typ:Value.(Option (Object Glyph))
let glyphPen_api api = api ~typ:Value.(Object GlyphPen)
let font_api api = api ~typ:Value.(Object Font)
let font_list_api api = api ~typ:Value.(List (Object Font))
let font_opt_api api = api ~typ:Value.(Option (Object Font))
let layer_api api = api ~typ:Value.(Object Layer)
let matrix_api api = api ~typ:Value.(Object Matrix)
let point_api api = api ~typ:Value.(Object Point)
let selection_api api = api ~typ:Value.(Object Selection)
(*******)
let script_typ = Util.Value.(Pair (String, (Comb String)))
let script_api api= api ~typ:script_typ
let feature_typ = Util.Value.(Pair (String, Comb script_typ))
let feature_api api= api ~typ:feature_typ
let feature_list_api api= api ~typ:Util.Value.(List feature_typ)
(*******)
let bool_api api = api ~typ:Value.(Bool)
let int_api api = api ~typ:Value.(Int)
let float_api api = api ~typ:Value.(Float)
let string_api api = api ~typ:Value.(String)
let string_opt_api api = api ~typ:Value.(Option String)
let string_list_api api = api ~typ:Value.(List String)
let unit_api api = api ~typ:Value.(Unit)
let string_comb_api api = api ~typ:Value.(Comb String)
(**************)
let v_contour = contour_api Value.value
let v_glyph = glyph_api Value.value
let v_glyphPen = glyphPen_api Value.value
let _v_font = font_api Value.value
let v_layer = layer_api Value.value
let v_matrix = matrix_api Value.value
let v_point = point_api Value.value
let _v_selection = selection_api Value.value
(*******)
let _v_script = script_api Util.Value.value
let _v_feature = feature_api Util.Value.value
let v_feature_list = feature_list_api Util.Value.value
(*******)
let v_bool = bool_api Value.value
let v_coord = Value.value ~typ:Value.(Coord)
let v_float = float_api Value.value
let v_int = int_api Value.value
let v_string = string_api Value.value
let v_string_list = string_list_api Value.value
(**************)
let a_opt f ?(tail=[]) = function | None -> tail | Some x -> (f x)::tail
(***********************************************************)
module type Attr = sig
  type t
  type attr
  val name: string
  val get: t -> attr
  val set: t -> attr -> unit
end
(**************)
module Member(V:sig type attr val typ:attr Value.obj end)(O:sig type t end)(X:sig val attr:string end) = struct
  type t = O.t Object.t
  type attr = V.attr
  let name = X.attr
  let get : t -> attr = API.get ~typ:V.typ ~m:X.attr
  let set : t -> attr -> unit = fun o v -> API.set ~typ:V.typ ~m:X.attr o v
end
(*******)
module BoolAttr = Member(struct type attr = bool let typ = Value.Bool end)
module IntAttr = Member(struct type attr = int let typ = Value.Int end)
module FloatAttr = Member(struct type attr = float let typ = Value.Float end)
module StringAttr = Member(struct type attr = string let typ = Value.String end)
module SelectionAttr = Member(struct type attr = Object.selection Object.t let typ = Value.Object Object.Selection end)
(***********************************************************)
module PsMat = struct
  type t = Object.matrix Object.t
  let api = matrix_api (API.call_function Import.psMat)
  (*** FUNCTIONS ********)
  let compose ~mat1 mat2 = api ~m:"compose" [v_matrix mat1;v_matrix mat2]
  let identity () = api ~m:"identify" []
  let inverse mat =  api ~m:"inverse" [v_matrix mat]
  let rotate theta = api ~m:"rotate" [v_float theta]
  let scale ?y x = api ~m:"scale" ((v_float x)::(a_opt v_float y))
  let skew theta = api ~m:"skew" [v_float theta]
  let translate ~x ~y = api ~m:"translate" [v_float x;v_float y]
end
(***********************************************************)
module Point = struct
  type t = Object.point Object.t
  (****************************)
  module BoolAttr = BoolAttr(struct type t = Object.point end)
  module FloatAttr = FloatAttr(struct type t = Object.point end)
  (* ATTRIBUTE TYPES ********)
  module type BoolAttr = Attr with type t = t and type attr = bool
  module type FloatAttr = Attr with type t = t and type attr = float
  (* ATTRIBUTES ********)
  module X = FloatAttr(struct let attr = "x" end)
  module Y = FloatAttr(struct let attr = "y" end)
  module On_curve = BoolAttr(struct let attr = "on_curve" end)
  module Selected = BoolAttr(struct let attr = "selected" end)
  (*** METHODS ********)
  let dup = point_api API.call_method ~m:"dup" []
  let transform ~matrix = unit_api API.call_method  ~m:"transform" [v_matrix matrix]
  (* let reduce *)
end
(***********************************************************)
module Contour = struct
  type t = Object.contour Object.t
  (****************************)
  module BoolAttr = BoolAttr(struct type t = Object.contour end)
  module StringAttr = StringAttr(struct type t = Object.contour end)
  (* ATTRIBUTE TYPES ********)
  module type BoolAttr = Attr with type t = t and type attr = bool
  module type StringAttr = Attr with type t = t and type attr = string
  (* ATTRIBUTES ********)
  module Is_quadratic = BoolAttr(struct let attr = "is_quadratic" end)
  module Closed = BoolAttr(struct let attr = "closed" end)
  module Name = StringAttr(struct let attr = "name" end)
  (* ITERATORS ********)
  let iterator o = Py.Object.get_iter Object.(python_of o)
  let iter ~f o = Py.Iter.iter (fun pypt -> f (Object.of_python ~typ:(Object.Point) pypt)) (iterator o)
  let iteri ~f o = let i = ref 0 in let f pt = f !i pt in iter ~f o
  let to_list o = Py.Iter.to_list_map (Object.of_python ~typ:(Object.Point)) (iterator o)
  let map ~f o = Py.Iter.to_list_map (fun pypt -> f (Object.of_python ~typ:(Object.Point) pypt)) (iterator o)
  let mapi ~f o = let i = ref 0 in let f pt = f !i pt in map ~f o
  let fold ~init ~f o = Py.Iter.fold_left (fun acc pypt -> f acc (Object.of_python ~typ:(Object.Point) pypt)) init (iterator o)
  (* BUILT-IN METHODS ********)
  (* let len = API.size *)
  let len = int_api API.call_method ~m:"__len__" []
  exception Index_out_of_bounds
  let nth i o =
    if not (0 <= i && i < len o) then raise Index_out_of_bounds;
    point_api API.call_method ~m:"__getitem__"  [v_int i] o
  let set_nth i pt o =
    if not (0 <= i && i < len o) then raise Index_out_of_bounds;
    unit_api API.call_method ~m:"__setitem__" [v_int i;v_point pt]o
  let add_contour c = contour_api API.call_method ~m:"__add__" [v_contour c]
  let add_point pt = contour_api API.call_method ~m:"__add__" [v_point pt]
  let append_contour c = unit_api API.call_method ~m:"__iadd__" [v_contour c]
  let append_point pt = unit_api API.call_method ~m:"__iadd__" [v_point pt]
  let contains_point pt = bool_api API.call_method ~m:"__contains__" [v_point pt]
  let contains_coord xy = bool_api API.call_method ~m:"__contains__" [v_coord xy]
  let extract ~min ~max o =
    let contour = contour_api (API.call_function Import.fontforge) ~m:"contour" [] in
    (let exception Stop in
     try
       iteri ~f:(fun i pt -> if min <= i && i < max && min <= max then append_point pt contour
                else if max <= i then raise Stop) o ;
     with Stop -> ());
    contour
  (* METHODS ********)
  let dup = contour_api API.call_method ~m:"dup" []
  let isEmpty = bool_api API.call_method ~m:"isEmpty" []
  let moveTo x y = unit_api API.call_method ~m:"moveTo" ((v_float x)::[v_float y])
  let moveToCoord (x,y) = moveTo x y
  let moveTo ~x ~y = moveTo (Float.of_int x) (Float.of_int y)
  let lineTo x y ?nth = unit_api API.call_method ~m:"lineTo" ((v_float x)::(v_float y)::(a_opt v_int nth))
  let lineToCoord (x,y) ?nth= lineTo x y ?nth
  let lineTo ~x ~y ?nth = lineTo (Float.of_int x) (Float.of_int y) ?nth
  let cubicToCoord ~cp1 ~cp2 ~pt ?nth = unit_api API.call_method ~m:"cubicTo" ((v_coord cp1)::(v_coord cp2)::(v_coord pt)::(a_opt v_int nth))
  let cubicTo ~cpx1 ~cpy1 ~cpx2 ~cpy2 ~x ~y ?nth = cubicToCoord ~cp1:Float.((of_int cpx1),(of_int cpy1)) ~cp2:Float.((of_int cpx2),(of_int cpy2)) ~pt:Float.((of_int x),(of_int y)) ?nth
  let quadraticToCoord ~cp:(x1,y1) ~pt:(x,y) ?nth = unit_api API.call_method ~m:"quadraticTo" ((v_float x1)::(v_float y1)::(v_float x)::(v_float y)::(a_opt v_int nth))
  let quadraticTo ~cpx ~cpy ~x ~y ?nth = quadraticToCoord ~cp:Float.((of_int cpx),(of_int cpy)) ~pt:Float.((of_int x),(of_int y)) ?nth
  let insertPt x y ?(onCurve=false) ?nth = unit_api API.call_method ~m:"insertPoint"  ((v_float x)::(v_float y)::(v_bool onCurve)::(a_opt v_int nth))
  let insertPtCoord ~pt:(x,y) ?onCurve ?nth = insertPt x y ?onCurve ?nth
  let insertPt ~x ~y ?onCurve ?nth = insertPt (Float.of_int x) (Float.of_int y) ?onCurve ?nth
  let insertPoint ~pt ?nth = unit_api API.call_method ~m:"insertPoint" ((v_point pt)::(a_opt v_int nth))
  let makeFirst x = unit_api API.call_method ~m:"makeFirst" [v_int x]
  let isClockwise = int_api API.call_method ~m:"isClockwise" []
  let reverseDirection = unit_api API.call_method ~m:"reverseDirection" []
  (* similar *)
  (* xBoundsAtY *)
  (* yBoundsAtX *)
  (* addExtrema *)
  (* cluster *)
  let merge x = unit_api API.call_method ~m:"merge" (List.map ~f:v_int x)
  let round ?factor = unit_api API.call_method ~m:"round" (a_opt v_float factor)
  (* simplify *)
  let selfIntersects = bool_api API.call_method ~m:"selfIntersects" []
  let transform ~matrix = unit_api API.call_method  ~m:"transform" [v_matrix matrix]
  (* boundingBox *)
  (* getSplineAfterPoint *)
  let draw x = unit_api API.call_method ~m:"draw" [v_glyphPen x]
end
(***********************************************************)
module Layer = struct
  type t = Object.layer Object.t
end
(***********************************************************)
module GlyphPen = struct
  type t = Object.glyphPen Object.t
  (* METHODS ********)
  let addComponent ~glyphname ~transform = unit_api API.call_method ~m:"addComponent" [v_string glyphname;v_matrix transform]
  let closePath = unit_api API.call_method ~m:"closePath" []
  let curveTo ~cp1 ?cp2 ~pt = unit_api API.call_method ~m:"curveTo" ((v_coord cp1)::(a_opt ~tail:[v_coord pt] v_coord cp2))
  let endPath = unit_api API.call_method ~m:"endPath" []
  let lineTo x y = unit_api API.call_method ~m:"lineTo" [v_float x;v_float y]
  let lineToCoord (x,y) = lineTo x y
  let lineTo ~x ~y = lineTo (Float.of_int x) (Float.of_int y)
  let moveTo x y = unit_api API.call_method ~m:"moveTo" [v_float x;v_float y]
  let moveToCoord (x,y) = moveTo x y
  let moveTo ~x ~y = moveTo (Float.of_int x) (Float.of_int y)
  let qcurveTo ~cps ?pt = unit_api API.call_method ~m:"qcurveTo" (List.append (List.map ~f:v_coord cps) (a_opt v_coord pt))
  (* METHODS implemented via extFontForge ********)
  let finalize _o = () (* unit_api (API.call_function Import.extFontForge) ~m:"finalize" [v_glyphPen _o] *)
end
(***********************************************************)
module Glyph = struct
  type t = Object.glyph Object.t
  (****************************)
  module BoolAttr = BoolAttr(struct type t = Object.glyph end)
  module IntAttr = IntAttr(struct type t = Object.glyph end)
  module StringAttr = StringAttr(struct type t = Object.glyph end)
  (* ATTRIBUTE TYPES ********)
  module type BoolAttr = Attr with type t = t and type attr = bool
  module type IntAttr = Attr with type t = t and type attr = int
  module type StringAttr = Attr with type t = t and type attr = string
  (* ATTRIBUTES ********)
  module ActiveLayer = IntAttr(struct let attr = "activeLayer" end)
  (* module Altuni *)
  (* module AnchorPoints *)
  (* module AnchorPointsWithSel *)
  (* module Background *)
  module Changed = BoolAttr(struct let attr = "changed" end)
  module Color = IntAttr(struct let attr = "color" end)
  (* module Comment *)
  (* module Dhints *)
  (* module Foreground *)
  module Glyphclass = StringAttr(struct let attr = "glyphclass" end)
  module Glyphname = StringAttr(struct let attr = "glyphname" end)
  (* module Hhints *)
  (* module HorizontalComponents *)
  (* module HorizontalComponentsItalicCorrection *)
  (* module HorizontalVariants  *)
  module IsExtendedShape = BoolAttr(struct let attr = "isExtendedShape" end)
  module ItalicCorrection = IntAttr(struct let attr = "italicCorrection" end)
  (* module Layers *)
  (* module Layerrefs *)
  (* module Lcarets *)
  module Left_side_bearing = IntAttr(struct let attr = "left_side_bearing" end)
  (* module ManualHints *)
  (* module MathKern *)
  module Right_side_bearing = IntAttr(struct let attr = "right_side_bearing" end)
  (* module Temporary *)
  module Texheight = IntAttr(struct let attr = "texheight" end)
  module Texdepth = IntAttr(struct let attr = "textdepth" end)
  module Topaccent = IntAttr(struct let attr = "topaccent" end)
  (* module Ttinstrs *)
  module Unicode = IntAttr(struct let attr = "unicode" end)
  module UnlinkRmOvrlpSave = IntAttr(struct let attr = "unlinkRmOvrlpSave" end)
  (* module Userdata *)
  (* module VerticalComponents *)
  (* module VerticalComponentsItalicCorrection *)
  (* module VerticalVariants *)
  (* module Vhints *)
  module Vwidth = IntAttr(struct let attr = "vwidth" end)
  module Width = IntAttr(struct let attr = "width" end)
  (* READ ONLY ATTRIBUTES ********)
  let encoding = int_api API.get ~m:"encoding"
  let font = font_api API.get ~m:"font"
  let originalgid = int_api API.get ~m:"originalgid"
  (* READ ONLY ATTRIBUTES OR METHODS ?? ********)
  let layer_cnt = int_api API.call_method ~m:"layer_cnt" []
  let script = string_api API.call_method ~m:"script" []
  let validation_state = int_api API.call_method ~m:"validation_state" []
  (* METHODS ********)
  (* let addAnchorPoint *)
  let addExtrema ?(flags="only_good") ?emsize = unit_api API.call_method ~m:"addExtrema" ((v_string flags)::(a_opt v_int emsize))
  let addReference ~glyphname ?transform = unit_api API.call_method ~m:"addReference" ((v_string glyphname)::(a_opt v_matrix transform))
  let addHint ~is_vertical ~start ~width = unit_api API.call_method ~m:"addHint" [v_bool is_vertical;v_float start;v_float width]
  let addPosSub subtable args = unit_api API.call_method ~m:"addPosSub"
      LookupTables.Table.((v_string (get_subtable_name subtable))::(values (get_lookup_type (get_lookup_table subtable)) args))
  (* let appendAccent *)
  let autoHint = unit_api API.call_method ~m:"autoHint" []
  let autoInstr = unit_api API.call_method ~m:"autoInstr" []
  let autoTrace = unit_api API.call_method ~m:"autoTrace" []
  let build = unit_api API.call_method ~m:"build" []
  let canonicalContours = unit_api API.call_method ~m:"canonicalContours" []
  let canonicalStart = unit_api API.call_method ~m:"canonicalStart" []
  (* let changeWeight *)
  let condenseExtend ~c_factor ~c_add ?(sb_factor=c_factor) ?(sb_add=c_add) ?correct = unit_api API.call_method ~m:"condenseExtend"
      ((v_float c_factor)::(v_float c_add)::(v_float sb_factor)::(v_float sb_add)::(a_opt v_bool correct))
  let clear = unit_api API.call_method ~m:"clear" []
  let cluster ?(within=0.1) ?max = unit_api API.call_method ~m:"cluster" ((v_float within)::(a_opt v_float max))
  let correctDirection = unit_api API.call_method ~m:"correctDirection" []
  let exclude layer = unit_api API.call_method ~m:"exclude" [v_layer layer]
  (* let export *)
  (* let getPosSub: string -> t -> ?? *)
  let importOutlines ~filename ?(flags=[]) = unit_api API.call_method ~m:"importOutlines" ((v_string filename)::(List.map ~f:v_string flags))
  let intersect = unit_api API.call_method ~m:"intersect" []
  let isWorthOutputting = bool_api API.call_method ~m:"isWorthOutputting" []
  let preserveLayerAsUndo ?layer_dohints = match layer_dohints with
    | None -> unit_api API.call_method ~m:"preserveLayerAsUndo" []
    | Some (layer,dohints) -> unit_api API.call_method ~m:"preserveLayerAsUndo" [v_int layer;v_bool dohints]
  let removeOverlap = unit_api API.call_method ~m:"removeOverlap" []
  let removePosSub ~name = unit_api API.call_method ~m:"removePosSub" [v_string name]
  let round ?factor = unit_api API.call_method ~m:"round" (a_opt v_int factor)
  let selfIntersects = bool_api API.call_method ~m:"selfIntersects" []
  (* let simplify *)
  (* let stroke *)
  let transform ~matrix ?(flags=[]) = unit_api API.call_method ~m:"transform" ((v_matrix matrix)::(List.map ~f:v_string flags))
  let nltransform ~xexpr ~yexpr = unit_api API.call_method ~m:"nltransform" [v_string xexpr; v_string yexpr]
  let unlinkRef ?refname  = unit_api API.call_method ~m:"unlinkRef" (a_opt v_string refname)
  let unlinkThisGlyph = unit_api API.call_method ~m:"unlinkThisGlyph" []
  let useRefsMetrics ~name ?flag = unit_api API.call_method ~m:"useRefsMetrics" ((v_string name)::(a_opt v_bool flag))
  let validate ?force = int_api API.call_method ~m:"validate" (a_opt v_bool force)
  let draw pen = unit_api API.call_method ~m:"draw" [v_glyphPen pen]
  let glyphPen ?(replace=false) = glyphPen_api API.call_method  ~m:"glyphPen" (if replace then [v_string "replace"] else [])
end
(***********************************************************)
module Selection = struct
  type t = Object.selection Object.t
  (* MISC ********)
  type request_t =
    | Sint of int
    | Sstr of string
    | Sglyph of Glyph.t
  let code i = Sint i
  let glyphname s = Sstr s
  let glyph x = Sglyph x
  let v_flag flag = string_comb_api Value.value [flag]
  let v_flag_encoding = v_flag "encoding"
  (* METHODS ********)
  let select_from_encoding ~encoding = unit_api API.call_method ~m:"select" [v_flag_encoding;v_int encoding]
  let select_from_unicode ~unicode = unit_api API.call_method ~m:"select" [v_int unicode]
  let select_from_glyphname ~name = unit_api API.call_method ~m:"select" [v_string name]
  let select ?(encoding=false) ?less ?(ranges=false) ~request =
    let flags = [] in
    let flags = match less with | None -> flags | Some x -> ((if x then "less" else "more")::flags) in
    let flags = if ranges then "ranges"::flags else flags in
    let flags = if encoding  then "encoding":: flags else flags in
    let flags = match flags with
      | []         -> []
      | [f1]       -> [v_flag f1]
      | [f1;f2]    -> [v_string_list [ f1 ; f2]]
      | [f1;f2;f3] -> [v_string_list [ f1 ; f2 ; f3] ]
      | _ -> assert false
    in
    let encode a = function
      | Sint x -> (v_int x)::a
      | Sstr x -> (v_string x)::a
      | Sglyph x -> (v_glyph x)::a
    in
    let args = List.rev (List.fold ~f:encode ~init:flags request) in
    unit_api API.call_method  ~m:"select" args

  let all = unit_api API.call_method ~m:"all" []
  let none = unit_api API.call_method ~m:"none" []
  let changed = unit_api API.call_method ~m:"changed" []
  let invert = unit_api API.call_method ~m:"invert" []
  (* ATTRIBUTES **)
  let byGlyphs = selection_api API.get ~m:"byGlyphs"
  (* ITERATORS ********)
  let iterator o = Py.Object.get_iter Object.(python_of o)
  let is_slot = Py.Int.check
  exception UnspecifiedSlotIteration
  exception UnspecifiedGlyphIteration
  let iter ?(slot=fun _ -> raise UnspecifiedSlotIteration) ?(glyph = fun _ -> raise UnspecifiedGlyphIteration) o =
    Py.Iter.iter (fun x -> if is_slot x then slot (Py.Int.to_int x) else glyph (Object.of_python ~typ:(Object.Glyph) x)

      ) (iterator o)
end
(***********************************************************)
module Font = struct
  type t = Object.font Object.t

  (* METHODS implemented via extFontForge ********)

  let glyph_from_name ~glyphname o = glyph_api API.call_method ~m:"__getitem__" [v_string glyphname] o
  let glyph_from_code ~unicode o = glyph_api API.call_method ~m:"__getitem__" [v_int unicode] o
  (* METHODS ********)
  let contains_glyphname ~glyphname = bool_api API.call_method ~m:"__contains__" [v_string glyphname]
  let contains_unicode ~unicode = bool_api API.call_method ~m:"__contains__" [v_int unicode]
  let addAnchorClass ~subtable ~anchorclass = unit_api API.call_method ~m:"addAnchorClass" [v_string subtable;v_string anchorclass]
  (* let addKerningClass *)
  type lookup_script_t = LookupTables.Script.t
  let lookupScript ~script ~languages = LookupTables.Script.mk ~languages script
  type lookup_feature_t = LookupTables.Feature.lookup_feature_t
  let lookupFeature ~feature ~scripts = ((feature, scripts):lookup_feature_t)
  let addLookup lookuptable lookuptype ~flags feature ?(others=[]) ?after_lookupname = unit_api API.call_method ~m:"addLookup"
      ((v_string (LookupTables.Table.get_lookup_name lookuptable))::
       (v_string (LookupTables.Table.get_lookup_typename lookuptype))::
       (v_string_list flags)::
       (LookupTables.TypedFeature.value (feature::others))::
       (a_opt v_string after_lookupname))
  let addLookupSubtable subtable ?after_subtable = unit_api API.call_method ~m:"addLookupSubtable"
      LookupTables.Table.((v_string (get_lookup_name (get_lookup_table subtable)))::
                          (v_string (get_subtable_name subtable))::
                          (a_opt v_string after_subtable))

  (* addContextualSubtable *)
  (* addSmallCaps *)
  (* alterKerningClass *)
  (* autoKern *)
  (* appendSFNTName *)
  let buildOrReplaceAALTFeatures = unit_api API.call_method ~m:"buildOrReplaceAALTFeatures" []
  let cidConvertByCMap ~filename = unit_api API.call_method ~m:"cidConvertByCMap" [v_string filename]
  let cidFlattenByCMap ~filename = unit_api API.call_method ~m:"cidFlattenByCMap" [v_string filename]
  (* cidConvertTo *)
  let cidFlatten = unit_api API.call_method ~m:"cidFlatten" []
  let cidInsertBlankSubFont = unit_api API.call_method ~m:"cidInsertBlankSubFont" []
  let cidRemoveSubFont = unit_api API.call_method ~m:"cidRemoveSubFont" []
  let close = unit_api API.call_method ~m:"close" []
  (* let compareFonts *)
  let createChar ~unicode ?name = glyph_api API.call_method  ~m:"createChar" ((v_int unicode)::(a_opt v_string name))
  (* let createInterpolatedGlyph *)
  (* let createMappedChar *)
  (* let find *)
  (* let findEncodingSlot *)
  (* let glyphs *)
  (* TODO: optional args for [generate] *)
  let generate ~filename = unit_api API.call_method ~m:"generate" [v_string filename]
  (* let generateTtc  *)
  (* let generateFeatureFile *)
  (* let genericGlyphChange *)
  (* let getKerningClass *)
  (* let getLookupInfo *)
  (* let getLookupSubtables *)
  (* let getLookupSubtableAnchorClasses *)
  (* let getLookupOfSubtable *)
  (* let getSubtableOfAnchor *)
  (* let importBitmaps *)
  (* let importLookups *)
  (* let interpolateFonts *)
  (* let isKerningClass *)
  (* let isVerticalKerning *)
  (* let italicize *)
  (*TODO: use the same as for addLookup *)
  let lookupSetFeatureList ~lookupname ~features = unit_api API.call_method ~m:"lookupSetFeatureList" [v_string lookupname; v_feature_list features]
  let lookupSetFlags ~lookupname ~flags = unit_api API.call_method ~m:"lookupSetFlags" (List.map ~f:v_string (lookupname::flags))
  let lookupSetStoreLigatureInAfm ~lookupname ~store = unit_api API.call_method ~m:"lookupSetStoreLigatureInAfm" [v_string lookupname;v_bool store]
  let mergeFonts ~filename ?preserveCrossFontKerning = unit_api API.call_method ~m:"mergeFonts" ((v_string filename)::(a_opt v_bool preserveCrossFontKerning))
  let mergeFeature ~filename = unit_api API.call_method ~m:"mergeFeature" [v_string filename]
  let mergeKern ~filename = unit_api API.call_method ~m:"mergeKern" [v_string filename]
  let mergeLookups ~lookupname1 ~lookupname2 = unit_api API.call_method ~m:"mergeLookup" [v_string lookupname1;v_string lookupname2]
  let mergeLookupSubtables ~subtable1 ~subtable2 = unit_api API.call_method ~m:"mergeLookupSubtables" [v_string subtable1;v_string subtable2]
  (* let printSample *)
  (* Returns a random text sample using the letter frequencies of the specified script (and optionally language).
     Both script and language should be expressed as strings containing OpenType Script and Language tags.
     "dflt" is a reasonable language tag.
     If the language is not specified, one will be chosen at random.
     If FontForge has no frequency information for the script/language specified it will use the letters in the script with equal frequencies. *)
  let randomText ~script ?lang = string_api API.call_method ~m:"randomText" ((v_string script)::(a_opt v_string lang))
  let regenBitmaps ~sizes = unit_api API.call_method ~m:"regenBitmaps" (List.map ~f:v_int sizes)
  let removeAnchorClass ~anchorclass = unit_api API.call_method ~m:"removeAnchorClass" [v_string anchorclass]
  let removeLookup ~lookup = unit_api API.call_method ~m:"removeLookup" [v_string lookup]
  let removeLookupSubtable ~subtable = unit_api API.call_method ~m:"removeLookupSubtable" [v_string subtable]
  let removeGlyph_from_unicode ~unicode = unit_api API.call_method ~m:"removeGlyph" [v_int unicode]
  let removeGlyph_from_glyphname ~glyphname = unit_api API.call_method ~m:"removeGlyph" [v_string glyphname]
  let removeGlyph_from_glyph ~glyph = unit_api API.call_method ~m:"removeGlyph" [v_glyph glyph]
  let removeGlyph = function
    | Selection.Sint unicode -> removeGlyph_from_unicode ~unicode
    | Selection.Sstr glyphname -> removeGlyph_from_glyphname ~glyphname
    | Selection.Sglyph glyph -> removeGlyph_from_glyph ~glyph

  type replace_spec_t = Value.t list
  let from_Layer ~srch ~rpl = List.map ~f:v_layer [srch; rpl]
  let from_Contour ~srch ~rpl = List.map ~f:v_contour [srch; rpl]
  let replaceAll ~spec ?error_bound = unit_api API.call_method ~m:"replaceAll" (List.append spec (a_opt v_float error_bound))
  let replaceAll_from_Layer ~srch ~rpl = replaceAll ~spec:(from_Layer ~srch ~rpl)
  let replaceAll_from_Contour ~srch ~rpl =  replaceAll ~spec:(from_Contour ~srch ~rpl)
  let revert = unit_api API.call_method ~m:"revert" []
  let revertFromBackup = unit_api API.call_method ~m:"revertFromBackup" []
  let save ~filename = unit_api API.call_method ~m:"saveName" [v_string filename]
  let saveNamelist ~filename = unit_api API.call_method ~m:"saveNamelist" [v_string filename]
  let getTableData ~tablename = string_api API.call_method ~m:"getTableData" [v_string tablename]
  (* let setTableData *)
  let validate ?force = int_api API.call_method  ~m:"validate" (a_opt v_bool force)

  let addExtrema = unit_api API.call_method ~m:"addExtrema" []
  let addSmallCaps = unit_api API.call_method ~m:"addSmallCaps" []
  let autoHint = unit_api API.call_method ~m:"autoHint" []
  let autoInstr = unit_api API.call_method ~m:"autoInstr" []
  let autoTrace = unit_api API.call_method ~m:"autoTrace" []
  let build = unit_api API.call_method ~m:"build" []
  let canonicalContours = unit_api API.call_method ~m:"canonicalContours" []
  let canonicalStart = unit_api API.call_method ~m:"canonicalStart" []
  let clear = unit_api API.call_method ~m:"clear" []
  let copy = unit_api API.call_method ~m:"copy" []
  let copyReference = unit_api API.call_method ~m:"copyReference" []
  let correctDirection = unit_api API.call_method ~m:"correctDirection" []
  let correctReferences = unit_api API.call_method ~m:"correctReferences" []
  let cut = unit_api API.call_method ~m:"cut" []
  let paste = unit_api API.call_method ~m:"paste" []
  let intersect = unit_api API.call_method ~m:"intersect" []
  let pasteInto = unit_api API.call_method ~m:"pasteInto" []
  let removeOverlap = unit_api API.call_method ~m:"removeOverlap" []
  let replaceWithReference ?fudge = unit_api API.call_method ~m:"replaceWithReference" (a_opt v_float fudge)
  let round ?factor = unit_api API.call_method ~m:"round" (a_opt v_float factor)
  (* let simplify *)
  (* let stroke *)
  let transform ~matrix = unit_api API.call_method ~m:"transform" [v_matrix matrix]
  let nltransform ~xexpr ~yexpr = unit_api API.call_method ~m:"nltransform" [v_string xexpr; v_string yexpr]
  let unlinkReferences = unit_api API.call_method ~m:"unlinkReferences" []
  (****************************)
  module IntAttr = IntAttr(struct type t = Object.font end)
  module StringAttr = StringAttr(struct type t = Object.font end)
  module SelectionAttr = SelectionAttr(struct type t = Object.font end)
  (* ATTRIBUTE TYPES ********)
  module type IntAttr = Attr with type t = t and type attr = int
  module type StringAttr = Attr with type t = t and type attr = string
  module type SelectionAttr = Attr with type t = t and type attr = Selection.t
  (* ATTRIBUTES ********)
  module Ascent   = IntAttr(struct let attr = "ascent" end)
  module Descent  = IntAttr(struct let attr = "descent" end)
  module Em       = IntAttr(struct let attr = "em" end)
  module Encoding   = StringAttr(struct let attr = "encoding" end)
  module Familyname = StringAttr(struct let attr = "familyname" end)
  module Fontname   = StringAttr(struct let attr = "fontname" end)
  module Fullname   = StringAttr(struct let attr = "fullname" end)
  module Version    = StringAttr(struct let attr = "version" end)
  module Weight     = StringAttr(struct let attr = "weight" end)
  module Selection = SelectionAttr(struct let attr = "selection" end)
  (* added *)
  module Copyright = StringAttr(struct let attr = "copyright" end)
end
(***********************************************************)
module FontForge = struct
  let get : type a . typ:a Value.obj -> m:string -> Value.t list -> a =
    fun ~typ ~m o -> API.call_function Import.fontforge ~typ ~m o
  (* PYTHON INITIATLISATION ********)
  let initialize = Util.Import.init
  let finalize = Py.finalize
  (* FUNCTIONS ********)
  let activeFont () = font_opt_api get ~m:"activeFont" []
  (* activeFontInUI *)
  let activeGlyph () = glyph_opt_api get ~m:"activeGlyph" []
  let activeLayer () = int_api get ~m:"activeGlyph" []
  (* ask *)
  (* askChoices *)
  (* askString *)
  let defaultOtherSubrs () = unit_api get ~m:"defaultOtherSubrs" []
  let fonts () = font_list_api get ~m:"fonts" []
  let fontsInFile ~filename = string_list_api get ~m:"fontsInFile" [v_string filename]
  (* getPrefs ~name:_ = assert false *)
  let hasSpiro () = bool_api get ~m:"hasSpiro" []
  (* hasUserInterface *)
  (* IsFraction *)
  (* IsLigature *)
  (* IsOtherFraction *)
  (* IsVulgarFraction *)
  let loadEncodingFile ~filename = string_opt_api get ~m:"loadEncodingFile" [v_string filename]
  let loadNamelist ~filename = unit_api get ~m:"loadNamelist" [v_string filename]
  let loadNamelistDir ~dirname = unit_api get ~m:"loadNamelistDir" [v_string dirname]
  let loadPlugin ~filename = unit_api get ~m:"loadPlugin" [v_string filename]
  let loadPluginDir ~dirname = unit_api get ~m:"loadPluginDir" [v_string dirname]
  let loadPrefs () = unit_api get ~m:"loadPrefs" []
  (* logWarning *)
  let nameFromUnicode ?namelist unicode = string_api get ~m:"nameFromUnicode" ((v_int unicode)::(a_opt v_string namelist))
  (* onAppClosing *)
  let openFont ~filename = font_api get ~m:"open" [v_string filename]
  (* openFilename *)
  let parseTTInstr arg =  string_api get ~m:"parseTTInstr" [v_string arg]
  (* postError *)
  (* postNotice *)
  let preloadCidmap ~filename ~registry ~order ~supplement = unit_api get ~m:"preloadCidmap" [v_string filename ; v_string registry ; v_string order ; v_int supplement]
  (* printSetup *)
  let readOtherSubrsFile ~filename = unit_api get ~m:"readOtherSubrsFile" [v_string filename]
  (* registerGlyphSeparationHook *)
  (* registerImportExport *)
  (* registerMenuItem *)
  (* runInitScripts *)
  (* saveFilename *)
  let savePrefs () = unit_api get ~m:"savePrefs" []
  (* scriptPath *)
  (* setPrefs ~name:_ ~value:_ *)
  (* SpiroVersion *)
  (* ucFracChartGetCnt *)
  (* ucLigChart... *)
  (* ucOFracChart... *)
  (* ucVulChart... *)
  (* Unicode... *)
  let unicodeFromName ~glyphname = int_api get ~m:"unicodeFromName" [v_string glyphname]
  (* unitShape *)
  let unParseTTInstr arg = string_api get ~m:"unParseTTInstr" [v_string arg]
  let version () = string_api get ~m:"version" []
  (* CLASSES ********)
  (* awcontext *)
  (* awglyph *)
  let contour () = contour_api get ~m:"contour" []
  (* cvt *)
  let font () = font_api get ~m:"font" []
  let layer () = layer_api get ~m:"layer" []
  (* layer_array *)
  (* layerinfo *)
  (* layerinfo_array *)
  (* math *)
  (* mathKern *)
  let point ?oncurve ?(y=0.0) x = point_api get ~m:"point" ((v_float x)::(v_float y)::(a_opt v_bool oncurve))
  let pointCoord ?oncurve (x,y) = point_api get ~m:"point" ((v_float x)::(v_float y)::(a_opt v_bool oncurve))
  (* private *)
  (* references *)
  (* selection *)
end
(***********************************************************)
