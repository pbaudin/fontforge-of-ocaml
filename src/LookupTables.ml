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

(** {1 Lookup Tables} *)

(***********************************************************)
(** {2 Convert} *)
(***********************************************************)
let int_api api = api ~typ:Util.Value.(Int)
let string_api api = api ~typ:Util.Value.(String)
let string_list_api api = api ~typ:Util.Value.(List String)

let script_typ = Util.Value.(Pair (String, List String))
let script_api api= api ~typ:script_typ
let feature_typ = Util.Value.(Pair (String, List script_typ))
let feature_api api = api ~typ:feature_typ
let feature_list_api api= api ~typ:Util.Value.(List feature_typ)

let v_int = int_api Util.Value.value
let v_string = string_api Util.Value.value
let v_string_list = string_list_api Util.Value.value
let v_script = script_api Util.Value.value
let v_feature = feature_api Util.Value.value
let v_feature_list = feature_list_api Util.Value.value
(***********************************************************)
(** {2 Script} *)
(***********************************************************)
module Script = struct

  type t = string * string list
  (** Shortcut type: script_lang = (script name, language list) *)

  let consolitated_languages = function | [] -> ["dflt"] | languages -> languages
  (** Fills the empty language list with the default language ("dflt"). *)

  let mk ?(languages=[]) scriptname = scriptname,(consolitated_languages languages)
  let get_scriptname (x,_) = x
  let get_languages (_,y) = y

  let value = v_script
end
(***********************************************************)
(** {2 Table} *)
(***********************************************************)
module Table = struct

  (** {3 Styles} *)

  (** lookup type classification. *)

  (** Defines types: one for each group of lookup type.
      Each of them have a specific function for adding data into the tables of that group. *)

  type unspecified_table_t (* G0 *)
  type contextual_table_t  (* G1 *)
  type lookup_pos_sub_t    (* G2 *)

  (** Defines types: one for each FontForge lookup type. *)

  type ff_gpos_context_t       (* T01 *)
  type ff_gpos_contextchain_t  (* T02 *)
  type ff_gpos_cursive_t       (* T03 *)
  type ff_gpos_mark2base_t     (* T04 *)
  type ff_gpos_mark2ligature_t (* T05 *)
  type ff_gpos_mark2mark_t     (* T06 *)
  type ff_gpos_pair_t          (* T07 *)
  type ff_gpos_single_t        (* T08 *)
  type ff_gsub_alternate_t     (* T09 *)
  type ff_gsub_context_t       (* T10 *)
  type ff_gsub_contextchain_t  (* T11 *)
  type ff_gsub_ligature_t      (* T12 *)
  type ff_gsub_multiple_t      (* T13 *)
  type ff_gsub_reversecchain_t (* T14 *)
  type ff_gsub_single_t        (* T15 *)
  type ff_kern_statemachine_t  (* T16 *)
  type ff_morx_indic_t         (* T17 *)
  type ff_morx_insert_t        (* T18 *)
  type ff_morx_context_t       (* T19 *)

  (** Defines types: one for each style. *)

  type unspecified_args_t (* S0 *)

  type args_glyph_kern_t = { kerning:int ; glyphname:string }
  type args_pos_t = { xoff:int ; yoff:int ; xadv:int ; yadv:int }
  type args_glyph_pos2_t = { pos1:args_pos_t ; pos2:args_pos_t ; glyphname:string ; }
  type args_str_t = string
  type args_str_list_t = string list

  let args_glyph_kern ~glyphname ~kerning = { kerning ; glyphname }
  let args_pos ~xoff ~yoff ~xadv ~yadv = { xoff ; yoff ; xadv ; yadv }
  let args_glyph_pos2 ~glyphname ~xoff1 ~yoff1 ~xadv1 ~yadv1 ~xoff2 ~yoff2 ~xadv2 ~yadv2 =
    { pos1=args_pos ~xoff:xoff1 ~yoff:yoff1 ~xadv:xadv1 ~yadv:yadv1 ;
      pos2=args_pos ~xoff:xoff2 ~yoff:yoff2 ~xadv:xadv2 ~yadv:yadv2 ;
      glyphname }

  type _ args_t =
    | ARGS_gpos_pair_kern : args_glyph_kern_t -> ff_gpos_pair_t     args_t
    | ARGS_gpos_pair_full : args_glyph_pos2_t -> ff_gpos_pair_t     args_t
    | ARGS_gpos_single    : args_pos_t        -> ff_gpos_single_t   args_t
    | ARGS_gsub_single    : args_str_t        -> ff_gsub_single_t   args_t
    | ARGS_gsub_ligature  : args_str_list_t   -> ff_gsub_ligature_t args_t
  (** Defines GADT: one constructor for each sub-styles. *)

  let args_gpos_pair_kern ~glyphname ~kerning = ARGS_gpos_pair_kern (args_glyph_kern ~glyphname ~kerning)
  let args_gpos_pair_pos2 ~glyphname ~xoff1 ~yoff1 ~xadv1 ~yadv1 ~xoff2 ~yoff2 ~xadv2 ~yadv2 = ARGS_gpos_pair_full (args_glyph_pos2 ~glyphname ~xoff1 ~yoff1 ~xadv1 ~yadv1 ~xoff2 ~yoff2 ~xadv2 ~yadv2)
  let args_gpos_single ~xoff ~yoff ~xadv ~yadv = ARGS_gpos_single (args_pos ~xoff ~yoff ~xadv ~yadv)
  let args_gsub_single ~glyphname = ARGS_gsub_single(glyphname)
  let args_gsub_ligature ~glyphname1 ~glyphname2 ~others = ARGS_gsub_ligature(glyphname1::glyphname2::others)

  (** Defines types: one for each lookup style. *)

  type gpos_context_t       = (contextual_table_t  * ff_gpos_context_t       ) * unspecified_args_t
  type gpos_contextchain_t  = (contextual_table_t  * ff_gpos_contextchain_t  ) * unspecified_args_t
  type gpos_cursive_t       = (lookup_pos_sub_t    * ff_gpos_cursive_t       ) * unspecified_args_t
  type gpos_mark2base_t     = (lookup_pos_sub_t    * ff_gpos_mark2base_t     ) * unspecified_args_t
  type gpos_mark2ligature_t = (lookup_pos_sub_t    * ff_gpos_mark2ligature_t ) * unspecified_args_t
  type gpos_mark2mark_t     = (lookup_pos_sub_t    * ff_gpos_mark2mark_t     ) * unspecified_args_t
  type gpos_pair_t          = (lookup_pos_sub_t    * ff_gpos_pair_t          ) * ff_gpos_pair_t     args_t
  type gpos_single_t        = (lookup_pos_sub_t    * ff_gpos_single_t        ) * ff_gpos_single_t   args_t
  type gsub_alternate_t     = (lookup_pos_sub_t    * ff_gsub_alternate_t     ) * unspecified_args_t
  type gsub_context_t       = (contextual_table_t  * ff_gsub_context_t       ) * unspecified_args_t
  type gsub_contextchain_t  = (contextual_table_t  * ff_gsub_contextchain_t  ) * unspecified_args_t
  type gsub_ligature_t      = (lookup_pos_sub_t    * ff_gsub_ligature_t      ) * ff_gsub_ligature_t args_t
  type gsub_multiple_t      = (lookup_pos_sub_t    * ff_gsub_multiple_t      ) * unspecified_args_t
  type gsub_reversecchain_t = (contextual_table_t  * ff_gsub_reversecchain_t ) * unspecified_args_t
  type gsub_single_t        = (lookup_pos_sub_t    * ff_gsub_single_t        ) * ff_gsub_single_t   args_t
  type kern_statemachine_t  = (unspecified_table_t * ff_kern_statemachine_t  ) * unspecified_args_t
  type morx_indic_t         = (unspecified_table_t * ff_morx_indic_t         ) * unspecified_args_t
  type morx_insert_t        = (unspecified_table_t * ff_morx_insert_t        ) * unspecified_args_t
  type morx_context_t       = (unspecified_table_t * ff_morx_context_t       ) * unspecified_args_t

  (** Classified lookup type. *)

  type _ t =
    | GPOS_context       : gpos_context_t       t
    | GPOS_contextchain  : gpos_contextchain_t  t
    | GPOS_cursive       : gpos_cursive_t       t
    | GPOS_mark2base     : gpos_mark2base_t     t
    | GPOS_mark2ligature : gpos_mark2ligature_t t
    | GPOS_mark2mark     : gpos_mark2mark_t     t
    | GPOS_pair          : gpos_pair_t          t
    | GPOS_single        : gpos_single_t        t
    | GSUB_alternate     : gsub_alternate_t     t
    | GSUB_context       : gsub_context_t       t
    | GSUB_contextchain  : gsub_contextchain_t  t
    | GSUB_ligature      : gsub_ligature_t      t
    | GSUB_multiple      : gsub_multiple_t      t
    | GSUB_reversecchain : gsub_reversecchain_t t
    | GSUB_single        : gsub_single_t        t
    | KERN_statemachine  : kern_statemachine_t  t
    | MORX_indic         : morx_indic_t         t
    | MORX_insert        : morx_insert_t        t
    | MORX_context       : morx_context_t       t
  (** Defines GADT: one constructor by lookup type with constraints on their style. *)

  let values : type a b . ((a*b)*b args_t) t -> b args_t -> Util.Value.t list =
    fun lookup_type args -> match lookup_type, args with
      | GPOS_pair, ARGS_gpos_pair_kern { kerning ; glyphname } -> [ v_string glyphname ; v_int kerning ]
      | GPOS_pair, ARGS_gpos_pair_full { pos1 ; pos2 ; glyphname } ->
        (v_string glyphname)::(List.map v_int [ pos1.xoff ; pos1.yoff ; pos1.xadv ; pos1.yadv; pos2.xoff ; pos2.yoff ; pos2.xadv ; pos2.yadv ])
      | GPOS_single, ARGS_gpos_single { xoff ; yoff ; xadv ; yadv } -> List.map v_int [ xoff ; yoff ; xadv ; yadv ]
      | GSUB_single, ARGS_gsub_single(glyphname) -> [ v_string glyphname ]
      | GSUB_ligature, ARGS_gsub_ligature(glyphnames) ->  [ v_string_list glyphnames ]

  let  get_lookup_typename : type a . a t -> string = function
    | GPOS_context       -> "gpos_context"
    | GPOS_contextchain  -> "gpos_contextchain"
    | GPOS_cursive       -> "gpos_cursive"
    | GPOS_mark2base     -> "gpos_mark2base"
    | GPOS_mark2ligature -> "gpos_mark2ligature"
    | GPOS_mark2mark     -> "gpos_mark2mark"
    | GPOS_pair          -> "gpos_pair"
    | GPOS_single        -> "gpos_single"
    | GSUB_alternate     -> "gsub_alternate"
    | GSUB_context       -> "gsub_context"
    | GSUB_contextchain  -> "gsub_contextchain"
    | GSUB_ligature      -> "gsub_ligature"
    | GSUB_multiple      -> "gsub_multiple"
    | GSUB_reversecchain -> "gsub_reversecchain"
    | GSUB_single        -> "gsub_single"
    | KERN_statemachine  -> "kern_statemachine"
    | MORX_indic         -> "morx_indic"
    | MORX_insert        -> "morx_insert"
    | MORX_context       -> "morx_context"

  type 'a lookup_table_t = 'a t * string
  (** Lookup sub-tables are named *)

  let mk_lookup_table typ ~name = (typ,name)
  let get_lookup_name (_,name) = name
  let get_lookup_type (typ,_) = typ

  type 'a lookup_subtable_t = 'a lookup_table_t * string
  (** Lookup sub-tables are named *)

  let mk_lookup_subtable tbl ~name = (tbl,name)
  let get_subtable_name (_,name) = name
  let get_lookup_table (tbl,_) = tbl
end
(***********************************************************)
(** {2 Feature classification} *)
(***********************************************************)
module Feature = struct

  (** Defines types: one for each combined group of lookup types.
      Features of a same group accept a lookup type among the same set of lookup types. *)

  type in_all_t = G_generic
  type in_gpos_mark2base_or_gpos_mark2ligature_t = G_04_S05
  type in_gpos_pair_or_gpos_context_or_gpos_contextchain_or_kern_statemachine_t = G_07_S01_S02_S16
  type in_gsub_context_or_gsub_contextchain_or_morx_context_t = S10_S11_S19
  type in_gsub_ligature_or_gsub_context_or_gsub_contextchain_or_morx_context_t = G_12_S10_S11_S19
  type in_gsub_multiple_or_gsub_ligature_t = G_13_S12
  type in_gsub_single_or_gpos_single_t = G_15_S08
  type in_gsub_single_or_gsub_alternate_t = G_15_S09
  type in_gsub_single_or_gsub_ligature_t = G_15_S12

  type _ in_type_t =
    | GT_one_to_one  : 'a Table.t -> 'a Table.t                                                 in_type_t
    | GT_all_to_one  : in_all_t                                                                 in_type_t
    | GT_04_05       : in_gpos_mark2base_or_gpos_mark2ligature_t                                in_type_t
    | GT_07_01_02_16 : in_gpos_pair_or_gpos_context_or_gpos_contextchain_or_kern_statemachine_t in_type_t
    | GT_10_11_19    : in_gsub_context_or_gsub_contextchain_or_morx_context_t                   in_type_t
    | GT_12_10_11_19 : in_gsub_ligature_or_gsub_context_or_gsub_contextchain_or_morx_context_t  in_type_t
    | GT_13_12       : in_gsub_multiple_or_gsub_ligature_t                                      in_type_t
    | GT_15_08       : in_gsub_single_or_gpos_single_t                                          in_type_t
    | GT_15_09       : in_gsub_single_or_gsub_alternate_t                                       in_type_t
    | GT_15_12       : in_gsub_single_or_gsub_ligature_t                                        in_type_t

  type 'a name_t = 'a in_type_t * string
  (** Features are named. *)

  (** Constructors of feature groups related to accepted subtbl-types. *)

  let mk_in: type a . a in_type_t -> string -> a name_t = fun t n -> (t,n)

  let mk_in_one_to_one tt ~name = mk_in (GT_one_to_one(tt)) name
  let mk_in_all ~name = mk_in GT_all_to_one name

  let mk_in_gpos_mark2base_or_gpos_mark2ligature ~name = mk_in GT_04_05 name
  let mk_in_gpos_pair_or_gpos_context_or_gpos_contextchain_or_kern_statemachine ~name = mk_in GT_07_01_02_16 name
  let mk_in_gsub_context_or_gsub_contextchain_or_morx_context ~name = mk_in GT_10_11_19 name
  let mk_in_gsub_ligature_or_gsub_context_or_gsub_contextchain_or_morx_context ~name = mk_in GT_12_10_11_19 name
  let mk_in_gsub_multiple_or_gsub_ligature ~name = mk_in GT_13_12 name
  let mk_in_gsub_single_or_gpos_single ~name = mk_in GT_15_08 name
  let mk_in_gsub_single_or_gsub_alternate ~name = mk_in GT_15_09 name
  let mk_in_gsub_single_or_gsub_ligature ~name = mk_in GT_15_12 name

  type 'a t = 'a name_t * Script.t list
  (** Shortcut type: feature = (named feature, (script, languages) list. *)

  let consolitated_scripts ~scripts = match scripts with [] -> [Script.mk "DFLT"] | _ -> scripts
  (** Fills the empty language_script list with the default script ("DFLT"). *)

  let mk : type a . ?scripts:Script.t list -> a name_t -> a t =
    fun ?(scripts=[]) feature -> feature,(consolitated_scripts ~scripts)
  (** Feature constructor linking a named script with a language list to an untyped feature. *)

  let get_featurename ((_,name),_) = name
  let get_script_languages (_,script) = script

  type lookup_feature_t = string * (Script.t list)
  let value = v_feature
end
(***********************************************************)
module TypedFeature = struct

  (** Lookup type selection among types accepted by a feature. *)

  type (_,_) selector_t =
    | ST_one_to_one        : 'a Table.t -> ('a Table.t, 'a Table.t Feature.t) selector_t
    | ST_all_to_one        : 'a Table.t -> ('a Table.t, Feature.in_all_t   Feature.t) selector_t

    | ST_04_IN_04_05       : (Table.gpos_mark2base_t     Table.t, Feature.in_gpos_mark2base_or_gpos_mark2ligature_t Feature.t) selector_t
    | ST_05_IN_04_05       : (Table.gpos_mark2ligature_t Table.t, Feature.in_gpos_mark2base_or_gpos_mark2ligature_t Feature.t) selector_t

    | ST_13_IN_13_12       : (Table.gsub_multiple_t      Table.t, Feature.in_gsub_multiple_or_gsub_ligature_t       Feature.t) selector_t
    | ST_12_IN_13_12       : (Table.gsub_ligature_t      Table.t, Feature.in_gsub_multiple_or_gsub_ligature_t       Feature.t) selector_t

    | ST_15_IN_15_08       : (Table.gsub_single_t        Table.t, Feature.in_gsub_single_or_gpos_single_t           Feature.t) selector_t
    | ST_08_IN_15_08       : (Table.gpos_single_t        Table.t, Feature.in_gsub_single_or_gpos_single_t           Feature.t) selector_t

    | ST_15_IN_15_09       : (Table.gsub_single_t        Table.t, Feature.in_gsub_single_or_gsub_alternate_t         Feature.t) selector_t
    | ST_09_IN_15_09       : (Table.gsub_alternate_t     Table.t, Feature.in_gsub_single_or_gsub_alternate_t         Feature.t) selector_t

    | ST_15_IN_15_12       : (Table.gsub_single_t        Table.t, Feature.in_gsub_single_or_gsub_ligature_t          Feature.t) selector_t
    | ST_12_IN_15_12       : (Table.gsub_ligature_t      Table.t, Feature.in_gsub_single_or_gsub_ligature_t          Feature.t) selector_t

    | ST_10_IN_10_11_19    : (Table.gsub_context_t       Table.t, Feature.in_gsub_context_or_gsub_contextchain_or_morx_context_t Feature.t) selector_t
    | ST_11_IN_10_11_19    : (Table.gsub_contextchain_t  Table.t, Feature.in_gsub_context_or_gsub_contextchain_or_morx_context_t Feature.t) selector_t
    | ST_19_IN_10_11_19    : (Table.morx_context_t       Table.t, Feature.in_gsub_context_or_gsub_contextchain_or_morx_context_t Feature.t) selector_t

    | ST_07_IN_07_01_02_16 : (Table.gpos_pair_t          Table.t, Feature.in_gpos_pair_or_gpos_context_or_gpos_contextchain_or_kern_statemachine_t Feature.t) selector_t
    | ST_01_IN_07_01_02_16 : (Table.gpos_context_t       Table.t, Feature.in_gpos_pair_or_gpos_context_or_gpos_contextchain_or_kern_statemachine_t Feature.t) selector_t
    | ST_02_IN_07_01_02_16 : (Table.gpos_contextchain_t  Table.t, Feature.in_gpos_pair_or_gpos_context_or_gpos_contextchain_or_kern_statemachine_t Feature.t) selector_t
    | ST_16_IN_07_01_02_16 : (Table.kern_statemachine_t  Table.t, Feature.in_gpos_pair_or_gpos_context_or_gpos_contextchain_or_kern_statemachine_t Feature.t) selector_t

    | ST_12_IN_12_10_11_19 : (Table.gsub_ligature_t      Table.t, Feature.in_gsub_ligature_or_gsub_context_or_gsub_contextchain_or_morx_context_t  Feature.t) selector_t
    | ST_10_IN_12_10_11_19 : (Table.gsub_context_t       Table.t, Feature.in_gsub_ligature_or_gsub_context_or_gsub_contextchain_or_morx_context_t  Feature.t) selector_t
    | ST_11_IN_12_10_11_19 : (Table.gsub_contextchain_t  Table.t, Feature.in_gsub_ligature_or_gsub_context_or_gsub_contextchain_or_morx_context_t  Feature.t) selector_t
    | ST_19_IN_12_10_11_19 : (Table.morx_context_t       Table.t, Feature.in_gsub_ligature_or_gsub_context_or_gsub_contextchain_or_morx_context_t  Feature.t) selector_t
  (** Defines GADT.
      These constructors are selectors allowing to choose one lookup type among ones accepted by the feature type. *)

  let get_selected_type : type a b . (a Table.t,b Feature.t) selector_t ->  a Table.t = function
    | ST_one_to_one(tt) -> tt
    | ST_all_to_one(tt) -> tt

    | ST_04_IN_04_05       -> GPOS_mark2base
    | ST_05_IN_04_05       -> GPOS_mark2ligature

    | ST_13_IN_13_12       -> GSUB_multiple
    | ST_12_IN_13_12       -> GSUB_ligature

    | ST_15_IN_15_08       -> GSUB_single
    | ST_08_IN_15_08       -> GPOS_single

    | ST_15_IN_15_09       -> GSUB_single
    | ST_09_IN_15_09       -> GSUB_alternate

    | ST_15_IN_15_12       -> GSUB_single
    | ST_12_IN_15_12       -> GSUB_ligature

    | ST_10_IN_10_11_19    -> GSUB_context
    | ST_11_IN_10_11_19    -> GSUB_contextchain
    | ST_19_IN_10_11_19    -> MORX_context

    | ST_07_IN_07_01_02_16 -> GPOS_pair
    | ST_01_IN_07_01_02_16 -> GPOS_context
    | ST_02_IN_07_01_02_16 -> GPOS_contextchain
    | ST_16_IN_07_01_02_16 -> KERN_statemachine

    | ST_12_IN_12_10_11_19 -> GSUB_ligature
    | ST_10_IN_12_10_11_19 -> GSUB_context
    | ST_11_IN_12_10_11_19 -> GSUB_contextchain
    | ST_19_IN_12_10_11_19 -> MORX_context
  (** For debuging since the info is in the GATD: for each constructor, there is only one possible mapping. *)

  type _ t =
    | TF: ('a Table.t,'b Feature.t) selector_t * 'b Feature.t -> 'a t
  (** Shortcut type: typed feature = (type selector, feature). *)

  let typeX_in_XY : type a b . select:(a Table.t,b Feature.t) selector_t -> b Feature.t -> a t =
    fun ~select feature -> TF (select, feature)
  (** Builds a typed feature from a type selector and a feature. *)

  let select_T_in_one : type a . a Table.t Feature.t -> a t =
    function ((GT_one_to_one(tt),_),_) as feature -> typeX_in_XY ~select:(ST_one_to_one(tt)) feature

  let select_T_in_all : type a . a Table.t -> Feature.in_all_t Feature.t -> a t =
    fun select feature -> typeX_in_XY ~select:(ST_all_to_one(select)) feature

  (** Constructors of features related to accepted tbl-types. *)

  let select_gpos_mark2base_in_gpos_mark2base_or_gpos_mark2ligature     feature = typeX_in_XY ~select:ST_04_IN_04_05 feature
  let select_gpos_mark2ligature_in_gpos_mark2base_or_gpos_mark2ligature feature = typeX_in_XY ~select:ST_05_IN_04_05 feature

  let select_gsub_multiple_in_gsub_multiple_or_gsub_ligature feature = typeX_in_XY ~select:ST_13_IN_13_12 feature
  let select_gsub_ligature_in_gsub_multiple_or_gsub_ligature feature = typeX_in_XY ~select:ST_12_IN_13_12 feature

  let select_gsub_single_in_gsub_single_or_gpos_single feature = typeX_in_XY ~select:ST_15_IN_15_08 feature
  let select_gpos_single_in_gsub_single_or_gpos_single feature = typeX_in_XY ~select:ST_08_IN_15_08 feature

  let select_gsub_single_in_gsub_single_or_gsub_alternate    feature = typeX_in_XY ~select:ST_15_IN_15_09 feature
  let select_gsub_alternate_in_gsub_single_or_gsub_alternate feature = typeX_in_XY ~select:ST_09_IN_15_09 feature

  let select_gsub_single_in_gsub_single_or_gsub_ligature   feature = typeX_in_XY ~select:ST_15_IN_15_12 feature
  let select_gsub_ligature_in_gsub_single_or_gsub_ligature feature = typeX_in_XY ~select:ST_12_IN_15_12 feature

  let select_gsub_context_in_gsub_context_or_gsub_contextchain_or_morx_context      feature = typeX_in_XY ~select:ST_10_IN_10_11_19 feature
  let select_gsub_contextchain_in_gsub_context_or_gsub_contextchain_or_morx_context feature = typeX_in_XY ~select:ST_11_IN_10_11_19 feature
  let select_morx_context_in_gsub_context_or_gsub_contextchain_or_morx_context      feature = typeX_in_XY ~select:ST_19_IN_10_11_19 feature

  let select_gpos_pair_in_gpos_pair_or_gpos_context_or_gpos_contextchain_or_kern_statemachine         feature = typeX_in_XY ~select:ST_07_IN_07_01_02_16 feature
  let select_gpos_context_in_gpos_pair_or_gpos_context_or_gpos_contextchain_or_kern_statemachine      feature = typeX_in_XY ~select:ST_01_IN_07_01_02_16 feature
  let select_gpos_contextchain_in_gpos_pair_or_gpos_context_or_gpos_contextchain_or_kern_statemachine feature = typeX_in_XY ~select:ST_02_IN_07_01_02_16 feature
  let select_kern_statemachine_in_gpos_pair_or_gpos_context_or_gpos_contextchain_or_kern_statemachine feature = typeX_in_XY ~select:ST_16_IN_07_01_02_16 feature

  let select_gsub_ligature_in_gsub_ligature_or_gsub_context_or_gsub_contextchain_or_morx_context     feature = typeX_in_XY ~select:ST_12_IN_12_10_11_19 feature
  let select_gsub_context_in_gsub_ligature_or_gsub_context_or_gsub_contextchain_or_morx_context      feature = typeX_in_XY ~select:ST_10_IN_12_10_11_19 feature
  let select_gsub_contextchain_in_gsub_ligature_or_gsub_context_or_gsub_contextchain_or_morx_context feature = typeX_in_XY ~select:ST_11_IN_12_10_11_19 feature
  let select_morx_context_in_gsub_ligature_or_gsub_context_or_gsub_contextchain_or_morx_context      feature = typeX_in_XY ~select:ST_19_IN_12_10_11_19 feature

  let get_lookup_type = function
    | TF (selector,_) -> get_selected_type selector

  let get_feature_scripts = function
    | TF (_,((_,feature),scripts)) -> (feature,scripts)

  let value x = v_feature_list (Base.List.map ~f:(get_feature_scripts) x)
end
(***********************************************************)
(* {2 Predefined Features} *)
(***********************************************************)
module PredefinedFeature = struct
  open Table
  open Feature
  open TypedFeature

  let dflt ?scripts lookup_type = select_T_in_all lookup_type (mk ?scripts (mk_in_all ~name:"DFLT"))
  (** Default. *)

  let aalt ?scripts select = select (mk ?scripts (mk_in_gsub_single_or_gsub_alternate ~name:"aalt"))
  (** Access All Alternates. *)
  let abvf ?scripts () = select_T_in_one (mk ?scripts (mk_in_one_to_one GSUB_single ~name:"abvf"))
  (** Above Base Forms. *)
  let abvm ?scripts select = select (mk ?scripts (mk_in_gpos_mark2base_or_gpos_mark2ligature ~name:"abvm"))
  (** Above Base Mark. *)
  let abvs  ?scripts () = select_T_in_one (mk ?scripts (mk_in_one_to_one GSUB_ligature ~name:"abvs"))
  (** Above Base Substitutions. *)
  let afrc  ?scripts () = select_T_in_one (mk ?scripts (mk_in_one_to_one GSUB_ligature ~name:"afrc"))
  (** Vertical Fractions. *)
  let akhn  ?scripts () = select_T_in_one (mk ?scripts (mk_in_one_to_one GSUB_ligature ~name:"akhn"))
  (** Akhand. *)
  let alig  ?scripts () = select_T_in_one (mk ?scripts (mk_in_one_to_one GSUB_ligature ~name:"alig"))
  (** Ancient Ligatures. *)
  let blwf  ?scripts () = select_T_in_one (mk ?scripts (mk_in_one_to_one GSUB_ligature ~name:"blwf"))
  (** Below Base Forms. *)
  let blwm ?scripts select = select (mk ?scripts (mk_in_gpos_mark2base_or_gpos_mark2ligature ~name:"blwm"))
  (** Below Base Mark. *)
  let blws  ?scripts () = select_T_in_one (mk ?scripts (mk_in_one_to_one GSUB_ligature ~name:"blws"))
  (** Below Base Substitutions. *)
  let c2pc  ?scripts () = select_T_in_one (mk ?scripts (mk_in_one_to_one GSUB_single ~name:"c2pc"))
  (** Capitals to Petite Capitals. *)
  let c2sc  ?scripts () = select_T_in_one (mk ?scripts (mk_in_one_to_one GSUB_single ~name:"c2sc"))
  (** Capitals to Small Capitals. *)
  let calt ?scripts select = select (mk ?scripts (mk_in_gsub_context_or_gsub_contextchain_or_morx_context ~name:"calt"))
  (** Contextual Alternates. *)
  let case ?scripts select = select (mk ?scripts (mk_in_gsub_single_or_gpos_single ~name:"case"))
  (** Case-Sensitive Forms. *)
  let ccmp ?scripts select = select (mk ?scripts (mk_in_gsub_multiple_or_gsub_ligature ~name:"ccmp"))
  (** Glyph Composition/Decomposition. *)
  let cfar  ?scripts () = select_T_in_one (mk ?scripts (mk_in_one_to_one GSUB_single ~name:"cfar"))
  (** Conjunct Form After Ro. *)
  let cjct  ?scripts () = select_T_in_one (mk ?scripts (mk_in_one_to_one GSUB_ligature ~name:"cjct"))
  (** Conjunct Forms. *)
  let clig  ?scripts () = select_T_in_one (mk ?scripts (mk_in_one_to_one GSUB_reversecchain ~name:"clig"))
  (** Contextual Ligatures. *)
  let cpct  ?scripts () = select_T_in_one (mk ?scripts (mk_in_one_to_one GPOS_single ~name:"cpct"))
  (** Centered CJK Punctuation. *)
  let cpsp  ?scripts () = select_T_in_one (mk ?scripts (mk_in_one_to_one GPOS_single ~name:"cpsp"))
  (** Capital Spacing. *)
  let cswh  ?scripts () = select_T_in_one (mk ?scripts (mk_in_one_to_one GSUB_reversecchain ~name:"cswh"))
  (** Contextual Swash. *)
  let curs  ?scripts () = select_T_in_one (mk ?scripts (mk_in_one_to_one GPOS_cursive ~name:"curs"))
  (** Cursive Attachment. *)
  let cv00  ?scripts () = select_T_in_one (mk ?scripts (mk_in_one_to_one GSUB_single ~name:"cv00"))
  (** Character Variants 00. *)
  let cv01  ?scripts () = select_T_in_one (mk ?scripts (mk_in_one_to_one GSUB_single ~name:"cv01"))
  (** Character Variants 01. *)
  let cv02  ?scripts () = select_T_in_one (mk ?scripts (mk_in_one_to_one GSUB_single ~name:"cv02"))
  (** Character Variants 02. *)
  let cv03  ?scripts () = select_T_in_one (mk ?scripts (mk_in_one_to_one GSUB_single ~name:"cv03"))
  (** Character Variants 03. *)
  let cv04  ?scripts () = select_T_in_one (mk ?scripts (mk_in_one_to_one GSUB_single ~name:"cv04"))
  (** Character Variants 04. *)
  let cv05  ?scripts () = select_T_in_one (mk ?scripts (mk_in_one_to_one GSUB_single ~name:"cv05"))
  (** Character Variants 05. *)
  let cv06  ?scripts () = select_T_in_one (mk ?scripts (mk_in_one_to_one GSUB_single ~name:"cv06"))
  (** Character Variants 06. *)
  let cv07  ?scripts () = select_T_in_one (mk ?scripts (mk_in_one_to_one GSUB_single ~name:"cv07"))
  (** Character Variants 07. *)
  let cv08  ?scripts () = select_T_in_one (mk ?scripts (mk_in_one_to_one GSUB_single ~name:"cv08"))
  (** Character Variants 08. *)
  let cv09  ?scripts () = select_T_in_one (mk ?scripts (mk_in_one_to_one GSUB_single ~name:"cv09"))
  (** Character Variants 09. *)
  let cv10  ?scripts () = select_T_in_one (mk ?scripts (mk_in_one_to_one GSUB_single ~name:"cv10"))
  (** Character Variants 10. *)
  let cv99  ?scripts () = select_T_in_one (mk ?scripts (mk_in_one_to_one GSUB_single ~name:"cv99"))
  (** Character Variants 99. *)
  let dcap  ?scripts () = select_T_in_one (mk ?scripts (mk_in_one_to_one GSUB_single ~name:"dcap"))
  (** Drop Caps. *)
  let dist  ?scripts () = select_T_in_one (mk ?scripts (mk_in_one_to_one GPOS_pair ~name:"dist"))
  (** Distance. *)
  let dlig  ?scripts () = select_T_in_one (mk ?scripts (mk_in_one_to_one GSUB_ligature ~name:"dlig"))
  (** Discretionary Ligatures. *)
  let dnom  ?scripts () = select_T_in_one (mk ?scripts (mk_in_one_to_one GSUB_single ~name:"dnom"))
  (** Denominators. *)
  let dpng  ?scripts () = select_T_in_one (mk ?scripts (mk_in_one_to_one GSUB_ligature ~name:"dpng"))
  (** Dipthongs (Obsolete). *)
  let dtls  ?scripts () = select_T_in_one (mk ?scripts (mk_in_one_to_one GSUB_single ~name:"dtls"))
  (** Dotless Forms. *)
  let expt  ?scripts () = select_T_in_one (mk ?scripts (mk_in_one_to_one GSUB_single ~name:"expt"))
  (** Expert Forms. *)
  let falt  ?scripts () = select_T_in_one (mk ?scripts (mk_in_one_to_one GSUB_alternate ~name:"falt"))
  (** Final Glyph On Line. *)
  let fin2 ?scripts select = select (mk ?scripts (mk_in_gsub_context_or_gsub_contextchain_or_morx_context ~name:"fin2"))
  (** Terminal Forms #2. *)
  let fin3 ?scripts select = select (mk ?scripts (mk_in_gsub_context_or_gsub_contextchain_or_morx_context ~name:"fin3"))
  (** Terminal Forms #3. *)
  let fina  ?scripts () = select_T_in_one (mk ?scripts (mk_in_one_to_one GSUB_single ~name:"fina"))
  (** Terminal Forms. *)
  let flac ?scripts select = select (mk ?scripts (mk_in_gsub_single_or_gsub_ligature ~name:"flac"))
  (** Flattened Accents over Capitals. *)
  let frac ?scripts select = select (mk ?scripts (mk_in_gsub_single_or_gsub_ligature ~name:"frac"))
  (** Diagonal Fractions. *)
  let fwid ?scripts select = select (mk ?scripts (mk_in_gsub_single_or_gpos_single ~name:"fwid"))
  (** Full Widths. *)
  let half  ?scripts () = select_T_in_one (mk ?scripts (mk_in_one_to_one GSUB_ligature ~name:"half"))
  (** Half Forms. *)
  let haln  ?scripts () = select_T_in_one (mk ?scripts (mk_in_one_to_one GSUB_ligature ~name:"haln"))
  (** Halant Forms. *)
  let halt  ?scripts () = select_T_in_one (mk ?scripts (mk_in_one_to_one GPOS_single ~name:"halt"))
  (** Alternative Half Widths. *)
  let hist  ?scripts () = select_T_in_one (mk ?scripts (mk_in_one_to_one GSUB_single ~name:"hist"))
  (** Historical Forms. *)
  let hkna  ?scripts () = select_T_in_one (mk ?scripts (mk_in_one_to_one GSUB_single ~name:"hkna"))
  (** Horizontal Kana Alternatives. *)
  let hlig  ?scripts () = select_T_in_one (mk ?scripts (mk_in_one_to_one GSUB_ligature ~name:"hlig"))
  (** Historic Ligatures. *)
  let hngl ?scripts select = select (mk ?scripts (mk_in_gsub_single_or_gsub_alternate ~name:"hngl"))
  (** Hanja to Hangul. *)
  let hojo  ?scripts () = select_T_in_one (mk ?scripts (mk_in_one_to_one GSUB_single ~name:"hojo"))
  (** Hojo (JIS X 0212-1990) Kanji Forms. *)
  let hwid ?scripts select = select (mk ?scripts (mk_in_gsub_single_or_gpos_single ~name:"hwid"))
  (** Half Widths. *)
  let init  ?scripts () = select_T_in_one (mk ?scripts (mk_in_one_to_one GSUB_single ~name:"init"))
  (** Initial Forms. *)
  let isol  ?scripts () = select_T_in_one (mk ?scripts (mk_in_one_to_one GSUB_single ~name:"isol"))
  (** Isolated Forms. *)
  let ital  ?scripts () = select_T_in_one (mk ?scripts (mk_in_one_to_one GSUB_single ~name:"ital"))
  (** Italics. *)
  let jalt  ?scripts () = select_T_in_one (mk ?scripts (mk_in_one_to_one GSUB_alternate ~name:"jalt"))
  (** Justification Alternatives. *)
  let jajp ?scripts select = select (mk ?scripts (mk_in_gsub_single_or_gsub_alternate ~name:"jajp"))
  (** Japanese Forms (Obsolete). *)
  let jp04  ?scripts () = select_T_in_one (mk ?scripts (mk_in_one_to_one GSUB_single ~name:"jp04"))
  (** JIS2004 Forms. *)
  let jp78 ?scripts select = select (mk ?scripts (mk_in_gsub_single_or_gsub_alternate ~name:"jp78"))
  (** JIS78 Forms. *)
  let jp83  ?scripts () = select_T_in_one (mk ?scripts (mk_in_one_to_one GSUB_single ~name:"jp83"))
  (** JIS83 Forms. *)
  let jp90  ?scripts () = select_T_in_one (mk ?scripts (mk_in_one_to_one GSUB_single ~name:"jp90"))
  (** JIS90 Forms. *)
  let kern ?scripts select = select (mk ?scripts (mk_in_gpos_pair_or_gpos_context_or_gpos_contextchain_or_kern_statemachine ~name:"kern"))
  (** Horizontal Kerning. *)
  let lfbd  ?scripts () = select_T_in_one (mk ?scripts (mk_in_one_to_one GPOS_single ~name:"lfbd"))
  (** Left Bounds. *)
  let liga  ?scripts () = select_T_in_one (mk ?scripts (mk_in_one_to_one GSUB_ligature ~name:"liga"))
  (** Standard Ligatures. *)
  let ljmo  ?scripts () = select_T_in_one (mk ?scripts (mk_in_one_to_one GSUB_ligature ~name:"ljmo"))
  (** Leading Jamo Forms. *)
  let lnum  ?scripts () = select_T_in_one (mk ?scripts (mk_in_one_to_one GSUB_single ~name:"lnum"))
  (** Lining Figures. *)
  let locl  ?scripts () = select_T_in_one (mk ?scripts (mk_in_one_to_one GSUB_single ~name:"locl"))
  (** Localized Forms. *)
  let mark ?scripts select = select (mk ?scripts (mk_in_gpos_mark2base_or_gpos_mark2ligature ~name:"mark"))
  (** Mark Positioning. *)
  let med2 ?scripts select = select (mk ?scripts (mk_in_gsub_context_or_gsub_contextchain_or_morx_context ~name:"med2"))
  (** Medial Forms 2. *)
  let medi  ?scripts () = select_T_in_one (mk ?scripts (mk_in_one_to_one GSUB_single ~name:"medi"))
  (** Medial Forms. *)
  let mgrk  ?scripts () = select_T_in_one (mk ?scripts (mk_in_one_to_one GSUB_single ~name:"mgrk"))
  (** Mathematical Greek. *)
  let mkmk  ?scripts () = select_T_in_one (mk ?scripts (mk_in_one_to_one GPOS_mark2mark ~name:"mkmk"))
  (** Mark to Mark. *)
  let mset ?scripts select = select (mk ?scripts (mk_in_gsub_context_or_gsub_contextchain_or_morx_context ~name:"mset"))
  (** Mark Positioning via Substitution. *)
  let nalt ?scripts select = select (mk ?scripts (mk_in_gsub_single_or_gsub_alternate ~name:"nalt"))
  (** Alternate Annotation Forms. *)
  let nlck  ?scripts () = select_T_in_one (mk ?scripts (mk_in_one_to_one GSUB_single ~name:"nlck"))
  (** NLC Kanji Forms. *)
  let nukt  ?scripts () = select_T_in_one (mk ?scripts (mk_in_one_to_one GSUB_ligature ~name:"nukt"))
  (** Nukta Forms. *)
  let numr  ?scripts () = select_T_in_one (mk ?scripts (mk_in_one_to_one GSUB_single ~name:"numr"))
  (** Numerators. *)
  let onum  ?scripts () = select_T_in_one (mk ?scripts (mk_in_one_to_one GSUB_single ~name:"onum"))
  (** Oldstyle Figures. *)
  let opbd  ?scripts () = select_T_in_one (mk ?scripts (mk_in_one_to_one GPOS_single ~name:"opbd"))
  (** Optical Bounds. *)
  let ordn ?scripts select = select (mk ?scripts (mk_in_gsub_ligature_or_gsub_context_or_gsub_contextchain_or_morx_context ~name:"ordn"))
  (** Ordinals. *)
  let ornm ?scripts select = select (mk ?scripts (mk_in_gsub_single_or_gsub_alternate ~name:"ornm"))
  (** Ornaments. *)
  let palt  ?scripts () = select_T_in_one (mk ?scripts (mk_in_one_to_one GPOS_single ~name:"palt"))
  (** Proportional Alternate Metrics. *)
  let pcap  ?scripts () = select_T_in_one (mk ?scripts (mk_in_one_to_one GSUB_single ~name:"pcap"))
  (** Lowercase to Petite Capitals. *)
  let pkna  ?scripts () = select_T_in_one (mk ?scripts (mk_in_one_to_one GPOS_single ~name:"pkna"))
  (** Proportional Kana. *)
  let pnum  ?scripts () = select_T_in_one (mk ?scripts (mk_in_one_to_one GSUB_single ~name:"pnum"))
  (** Proportional Numbers. *)
  let pref  ?scripts () = select_T_in_one (mk ?scripts (mk_in_one_to_one GSUB_ligature ~name:"pref"))
  (** Pre Base Forms. *)
  let pres ?scripts select = select (mk ?scripts (mk_in_gsub_ligature_or_gsub_context_or_gsub_contextchain_or_morx_context ~name:"pres"))
  (** Pre Base Substitutions. *)
  let pstf  ?scripts () = select_T_in_one (mk ?scripts (mk_in_one_to_one GSUB_ligature ~name:"pstf"))
  (** Post Base Forms. *)
  let psts  ?scripts () = select_T_in_one (mk ?scripts (mk_in_one_to_one GSUB_ligature ~name:"psts"))
  (** Post Base Substitutions. *)
  let pwid  ?scripts () = select_T_in_one (mk ?scripts (mk_in_one_to_one GSUB_single ~name:"pwid"))
  (** Proportional Width. *)
  let qwid ?scripts select = select (mk ?scripts (mk_in_gsub_single_or_gpos_single ~name:"qwid"))
  (** Quarter Widths. *)
  let rand  ?scripts () = select_T_in_one (mk ?scripts (mk_in_one_to_one GSUB_alternate ~name:"rand"))
  (** Randomize. *)
  let rkrf  ?scripts () = select_T_in_one (mk ?scripts (mk_in_one_to_one GSUB_ligature ~name:"rkrf"))
  (** Rakar Forms. *)
  let rlig  ?scripts () = select_T_in_one (mk ?scripts (mk_in_one_to_one GSUB_ligature ~name:"rlig"))
  (** Required Ligatures. *)
  let rphf  ?scripts () = select_T_in_one (mk ?scripts (mk_in_one_to_one GSUB_ligature ~name:"rphf"))
  (** Reph Form. *)
  let rtbd  ?scripts () = select_T_in_one (mk ?scripts (mk_in_one_to_one GPOS_single ~name:"rtbd"))
  (** Right Bounds. *)
  let rtla  ?scripts () = select_T_in_one (mk ?scripts (mk_in_one_to_one GSUB_single ~name:"rtla"))
  (** Right to Left Alternates. *)
  let rtlm  ?scripts () = select_T_in_one (mk ?scripts (mk_in_one_to_one GSUB_single ~name:"rtlm"))
  (** Right to Left mirrored forms. *)
  let ruby  ?scripts () = select_T_in_one (mk ?scripts (mk_in_one_to_one GSUB_single ~name:"ruby"))
  (** Ruby Notational Forms. *)
  let salt ?scripts select = select (mk ?scripts (mk_in_gsub_single_or_gsub_alternate ~name:"salt"))
  (** Stylistic Alternatives. *)
  let sinf  ?scripts () = select_T_in_one (mk ?scripts (mk_in_one_to_one GSUB_single ~name:"sinf"))
  (** Scientific Inferiors. *)
  let smcp  ?scripts () = select_T_in_one (mk ?scripts (mk_in_one_to_one GSUB_single ~name:"smcp"))
  (** Lowercase to Small Capitals. *)
  let smpl  ?scripts () = select_T_in_one (mk ?scripts (mk_in_one_to_one GSUB_single ~name:"smpl"))
  (** Simplified Forms. *)
  let ss01  ?scripts () = select_T_in_one (mk ?scripts (mk_in_one_to_one GSUB_single ~name:"ss01"))
  (** Style Set 1. *)
  let ss02  ?scripts () = select_T_in_one (mk ?scripts (mk_in_one_to_one GSUB_single ~name:"ss02"))
  (** Style Set 2. *)
  let ss03  ?scripts () = select_T_in_one (mk ?scripts (mk_in_one_to_one GSUB_single ~name:"ss03"))
  (** Style Set 3. *)
  let ss04  ?scripts () = select_T_in_one (mk ?scripts (mk_in_one_to_one GSUB_single ~name:"ss04"))
  (** Style Set 4. *)
  let ss05  ?scripts () = select_T_in_one (mk ?scripts (mk_in_one_to_one GSUB_single ~name:"ss05"))
  (** Style Set 5. *)
  let ss06  ?scripts () = select_T_in_one (mk ?scripts (mk_in_one_to_one GSUB_single ~name:"ss06"))
  (** Style Set 6. *)
  let ss07  ?scripts () = select_T_in_one (mk ?scripts (mk_in_one_to_one GSUB_single ~name:"ss07"))
  (** Style Set 7. *)
  let ss08  ?scripts () = select_T_in_one (mk ?scripts (mk_in_one_to_one GSUB_single ~name:"ss08"))
  (** Style Set 8. *)
  let ss09  ?scripts () = select_T_in_one (mk ?scripts (mk_in_one_to_one GSUB_single ~name:"ss09"))
  (** Style Set 9. *)
  let ss10  ?scripts () = select_T_in_one (mk ?scripts (mk_in_one_to_one GSUB_single ~name:"ss10"))
  (** Style Set 10. *)
  let ss11  ?scripts () = select_T_in_one (mk ?scripts (mk_in_one_to_one GSUB_single ~name:"ss11"))
  (** Style Set 11. *)
  let ss12  ?scripts () = select_T_in_one (mk ?scripts (mk_in_one_to_one GSUB_single ~name:"ss12"))
  (** Style Set 12. *)
  let ss13  ?scripts () = select_T_in_one (mk ?scripts (mk_in_one_to_one GSUB_single ~name:"ss13"))
  (** Style Set 13. *)
  let ss14  ?scripts () = select_T_in_one (mk ?scripts (mk_in_one_to_one GSUB_single ~name:"ss14"))
  (** Style Set 14. *)
  let ss15  ?scripts () = select_T_in_one (mk ?scripts (mk_in_one_to_one GSUB_single ~name:"ss15"))
  (** Style Set 15. *)
  let ss16  ?scripts () = select_T_in_one (mk ?scripts (mk_in_one_to_one GSUB_single ~name:"ss16"))
  (** Style Set 16. *)
  let ss17  ?scripts () = select_T_in_one (mk ?scripts (mk_in_one_to_one GSUB_single ~name:"ss17"))
  (** Style Set 17. *)
  let ss18  ?scripts () = select_T_in_one (mk ?scripts (mk_in_one_to_one GSUB_single ~name:"ss18"))
  (** Style Set 18. *)
  let ss19  ?scripts () = select_T_in_one (mk ?scripts (mk_in_one_to_one GSUB_single ~name:"ss19"))
  (** Style Set 19. *)
  let ss20  ?scripts () = select_T_in_one (mk ?scripts (mk_in_one_to_one GSUB_single ~name:"ss20"))
  (** Style Set 20. *)
  let ssty ?scripts select = select (mk ?scripts (mk_in_gsub_single_or_gsub_alternate ~name:"ssty"))
  (** Script Style. *)
  let subs  ?scripts () = select_T_in_one (mk ?scripts (mk_in_one_to_one GSUB_single ~name:"subs"))
  (** Subscript. *)
  let sups  ?scripts () = select_T_in_one (mk ?scripts (mk_in_one_to_one GSUB_single ~name:"sups"))
  (** Superscript. *)
  let swsh ?scripts select = select (mk ?scripts (mk_in_gsub_single_or_gsub_alternate ~name:"swsh"))
  (** Swash. *)
  let titl  ?scripts () = select_T_in_one (mk ?scripts (mk_in_one_to_one GSUB_single ~name:"titl"))
  (** Titling. *)
  let tjmo  ?scripts () = select_T_in_one (mk ?scripts (mk_in_one_to_one GSUB_ligature ~name:"tjmo"))
  (** Trailing Jamo Forms. *)
  let tnam  ?scripts () = select_T_in_one (mk ?scripts (mk_in_one_to_one GSUB_single ~name:"tnam"))
  (** Traditional Name Forms. *)
  let tnum  ?scripts () = select_T_in_one (mk ?scripts (mk_in_one_to_one GSUB_single ~name:"tnum"))
  (** Tabular Numbers. *)
  let trad ?scripts select = select (mk ?scripts (mk_in_gsub_single_or_gsub_alternate ~name:"trad"))
  (** Traditional Forms. *)
  let twid ?scripts select = select (mk ?scripts (mk_in_gsub_single_or_gpos_single ~name:"twid"))
  (** Third Widths. *)
  let unic  ?scripts () = select_T_in_one (mk ?scripts (mk_in_one_to_one GSUB_single ~name:"unic"))
  (** Unicase. *)
  let valt  ?scripts () = select_T_in_one (mk ?scripts (mk_in_one_to_one GPOS_single ~name:"valt"))
  (** Alternate Vertical Metrics. *)
  let vatu  ?scripts () = select_T_in_one (mk ?scripts (mk_in_one_to_one GSUB_ligature ~name:"vatu"))
  (** Vattu Variants. *)
  let vert  ?scripts () = select_T_in_one (mk ?scripts (mk_in_one_to_one GSUB_single ~name:"vert"))
  (** Vertical Alternates (obs). *)
  let vhal  ?scripts () = select_T_in_one (mk ?scripts (mk_in_one_to_one GPOS_single ~name:"vhal"))
  (** Alternate Vertical Half Metrics. *)
  let vjmo  ?scripts () = select_T_in_one (mk ?scripts (mk_in_one_to_one GSUB_ligature ~name:"vjmo"))
  (** Vowel Jamo Forms. *)
  let vkna  ?scripts () = select_T_in_one (mk ?scripts (mk_in_one_to_one GSUB_single ~name:"vkna"))
  (** Vertical Kana Alternates. *)
  let vkrn ?scripts select = select (mk ?scripts (mk_in_gpos_pair_or_gpos_context_or_gpos_contextchain_or_kern_statemachine ~name:"vkrn"))
  (** Vertical Kerning. *)
  let vpal  ?scripts () = select_T_in_one (mk ?scripts (mk_in_one_to_one GPOS_single ~name:"vpal"))
  (** Proportional Alternate Vertical Metrics. *)
  let vrt2  ?scripts () = select_T_in_one (mk ?scripts (mk_in_one_to_one GSUB_single ~name:"vrt2"))
  (** Vertical Rotation & Alternates. *)
  let zero  ?scripts () = select_T_in_one (mk ?scripts (mk_in_one_to_one GSUB_single ~name:"zero"))
  (** Slashed Zero. *)

end
(***********************************************************)
