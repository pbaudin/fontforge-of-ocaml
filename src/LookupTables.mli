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
(** {2 Script} *)
(***********************************************************)
module Script: sig
  type t = string * string list

  val mk: ?languages:string list -> string -> t
  (** Constructor *)

  val get_scriptname: t -> string
  val get_languages: t -> string list

  val value: t -> Util.Value.t
  (** convert *)
end
(***********************************************************)
(** {2 Table} *)
(***********************************************************)
module Table: sig

  (** Defines types: one for each group of lookup type.
      Each of them have a specific function for adding data into the tables of that group. *)

  type unspecified_table_t
  type contextual_table_t
  type lookup_pos_sub_t

  (** Defines types: one for each FontForge lookup type. *)

  type ff_gpos_context_t
  type ff_gpos_contextchain_t
  type ff_gpos_cursive_t
  type ff_gpos_mark2base_t
  type ff_gpos_mark2ligature_t
  type ff_gpos_mark2mark_t
  type ff_gpos_pair_t
  type ff_gpos_single_t
  type ff_gsub_alternate_t
  type ff_gsub_context_t
  type ff_gsub_contextchain_t
  type ff_gsub_ligature_t
  type ff_gsub_multiple_t
  type ff_gsub_reversecchain_t
  type ff_gsub_single_t
  type ff_kern_statemachine_t
  type ff_morx_indic_t
  type ff_morx_insert_t
  type ff_morx_context_t

  (** Defines types: one for each style. *)

  type unspecified_args_t

  type args_glyph_kern_t
  type args_pos_t
  type args_glyph_pos2_t

  type _ args_t =
      ARGS_gpos_pair_kern: args_glyph_kern_t -> ff_gpos_pair_t args_t
    | ARGS_gpos_pair_full: args_glyph_pos2_t -> ff_gpos_pair_t args_t
    | ARGS_gpos_single: args_pos_t -> ff_gpos_single_t args_t
    | ARGS_gsub_single: string -> ff_gsub_single_t args_t
    | ARGS_gsub_ligature: string list -> ff_gsub_ligature_t args_t
  (** Defines GADT: one constructor for each sub-styles. *)

  val args_gpos_pair_kern: glyphname:string -> kerning:int -> ff_gpos_pair_t args_t
  val args_gpos_pair_pos2: glyphname:string ->
    xoff1:int -> yoff1:int -> xadv1:int -> yadv1:int ->
    xoff2:int -> yoff2:int -> xadv2:int -> yadv2:int -> ff_gpos_pair_t args_t
  val args_gpos_single: xoff:int -> yoff:int -> xadv:int -> yadv:int -> ff_gpos_single_t args_t
  val args_gsub_single: glyphname:string -> ff_gsub_single_t args_t
  val args_gsub_ligature: glyphname1:string -> glyphname2:string -> others:string list -> ff_gsub_ligature_t args_t

  (** Defines types: one for each lookup style. *)

  type gpos_context_t =       (contextual_table_t * ff_gpos_context_t) * unspecified_args_t
  type gpos_contextchain_t =  (contextual_table_t * ff_gpos_contextchain_t) * unspecified_args_t
  type gpos_cursive_t =       (lookup_pos_sub_t * ff_gpos_cursive_t) * unspecified_args_t
  type gpos_mark2base_t =     (lookup_pos_sub_t * ff_gpos_mark2base_t) * unspecified_args_t
  type gpos_mark2ligature_t = (lookup_pos_sub_t * ff_gpos_mark2ligature_t) * unspecified_args_t
  type gpos_mark2mark_t =     (lookup_pos_sub_t * ff_gpos_mark2mark_t) * unspecified_args_t
  type gpos_pair_t =          (lookup_pos_sub_t * ff_gpos_pair_t) * ff_gpos_pair_t args_t
  type gpos_single_t =        (lookup_pos_sub_t * ff_gpos_single_t) * ff_gpos_single_t args_t
  type gsub_alternate_t =     (lookup_pos_sub_t * ff_gsub_alternate_t) * unspecified_args_t
  type gsub_context_t =       (contextual_table_t * ff_gsub_context_t) * unspecified_args_t
  type gsub_contextchain_t =  (contextual_table_t * ff_gsub_contextchain_t) * unspecified_args_t
  type gsub_ligature_t =      (lookup_pos_sub_t * ff_gsub_ligature_t) * ff_gsub_ligature_t args_t
  type gsub_multiple_t =      (lookup_pos_sub_t * ff_gsub_multiple_t) * unspecified_args_t
  type gsub_reversecchain_t = (contextual_table_t * ff_gsub_reversecchain_t) * unspecified_args_t
  type gsub_single_t =        (lookup_pos_sub_t * ff_gsub_single_t) * ff_gsub_single_t args_t
  type kern_statemachine_t =  (unspecified_table_t * ff_kern_statemachine_t) * unspecified_args_t
  type morx_indic_t =         (unspecified_table_t * ff_morx_indic_t) * unspecified_args_t
  type morx_insert_t =        (unspecified_table_t * ff_morx_insert_t) * unspecified_args_t
  type morx_context_t =       (unspecified_table_t * ff_morx_context_t) * unspecified_args_t

  (** Classified lookup type. *)

  type _ t =
      GPOS_context:       gpos_context_t t
    | GPOS_contextchain:  gpos_contextchain_t t
    | GPOS_cursive:       gpos_cursive_t t
    | GPOS_mark2base:     gpos_mark2base_t t
    | GPOS_mark2ligature: gpos_mark2ligature_t t
    | GPOS_mark2mark:     gpos_mark2mark_t t
    | GPOS_pair:          gpos_pair_t t
    | GPOS_single:        gpos_single_t t
    | GSUB_alternate:     gsub_alternate_t t
    | GSUB_context:       gsub_context_t t
    | GSUB_contextchain:  gsub_contextchain_t t
    | GSUB_ligature:      gsub_ligature_t t
    | GSUB_multiple:      gsub_multiple_t t
    | GSUB_reversecchain: gsub_reversecchain_t t
    | GSUB_single:        gsub_single_t t
    | KERN_statemachine:  kern_statemachine_t t
    | MORX_indic:         morx_indic_t t
    | MORX_insert:        morx_insert_t t
    | MORX_context:       morx_context_t t
  (** Defines GADT: one constructor by lookup type with constraints on their style. *)

  val values: (('a * 'b) * 'b args_t) t -> 'b args_t -> Util.Value.t list
  (** Convert *)

  (** {3 Lookup tables} *)

  type 'a lookup_table_t
  (** Lookup tables are named *)

  val mk_lookup_table: 'a t -> name:string -> 'a lookup_table_t
  val get_lookup_name: 'a lookup_table_t -> string
  val get_lookup_type: 'a lookup_table_t -> 'a t
  val get_lookup_typename: 'a t -> string


  (** {3 Lookup sub-tables} *)

  type 'a lookup_subtable_t
  (** Lookup sub-tables are named *)

  val mk_lookup_subtable: 'a lookup_table_t -> name:string -> 'a lookup_subtable_t
  val get_subtable_name: 'a lookup_subtable_t -> string
  val get_lookup_table: 'a lookup_subtable_t -> 'a lookup_table_t

end
(***********************************************************)
(** {2 Feature classification} *)
(***********************************************************)
module Feature: sig

  (** {3 Types} *)

  (** Defines types: one for each combined group of lookup types.
      Features of a same group accept a lookup type among the same set of lookup types. *)

  type in_all_t
  type in_gpos_mark2base_or_gpos_mark2ligature_t
  type in_gpos_pair_or_gpos_context_or_gpos_contextchain_or_kern_statemachine_t
  type in_gsub_context_or_gsub_contextchain_or_morx_context_t
  type in_gsub_ligature_or_gsub_context_or_gsub_contextchain_or_morx_context_t
  type in_gsub_multiple_or_gsub_ligature_t
  type in_gsub_single_or_gpos_single_t
  type in_gsub_single_or_gsub_alternate_t
  type in_gsub_single_or_gsub_ligature_t

  type _ in_type_t =
      GT_one_to_one:  'a Table.t -> 'a Table.t in_type_t
    | GT_all_to_one:  in_all_t in_type_t
    | GT_04_05:       in_gpos_mark2base_or_gpos_mark2ligature_t in_type_t
    | GT_07_01_02_16: in_gpos_pair_or_gpos_context_or_gpos_contextchain_or_kern_statemachine_t in_type_t
    | GT_10_11_19:    in_gsub_context_or_gsub_contextchain_or_morx_context_t in_type_t
    | GT_12_10_11_19: in_gsub_ligature_or_gsub_context_or_gsub_contextchain_or_morx_context_t in_type_t
    | GT_13_12:       in_gsub_multiple_or_gsub_ligature_t in_type_t
    | GT_15_08:       in_gsub_single_or_gpos_single_t in_type_t
    | GT_15_09:       in_gsub_single_or_gsub_alternate_t in_type_t
    | GT_15_12:       in_gsub_single_or_gsub_ligature_t in_type_t

  type 'a name_t
  (** Features are named. *)

  (** {3 Constructors of features related to a group accepted lookup types} *)

  val mk_in_one_to_one: 'a Table.t -> name:string -> 'a Table.t name_t
  (** For features accepting only one lookup type. *)
  val mk_in_all: name:string -> in_all_t name_t
  (** For features accepting all possible lookup types. *)
  val mk_in_gpos_mark2base_or_gpos_mark2ligature: name:string -> in_gpos_mark2base_or_gpos_mark2ligature_t name_t
  (** For features accepting: GPOS_mark2base, GPOS_mark2ligature. *)
  val mk_in_gpos_pair_or_gpos_context_or_gpos_contextchain_or_kern_statemachine: name:string -> in_gpos_pair_or_gpos_context_or_gpos_contextchain_or_kern_statemachine_t name_t
  (** For features accepting: GPOS_pair, GPOS_context, GPOS_contextchain, KERN_statemachine. *)
  val mk_in_gsub_context_or_gsub_contextchain_or_morx_context: name:string -> in_gsub_context_or_gsub_contextchain_or_morx_context_t name_t
  (** For features accepting: GSUB_context, GSUB_contextchain, MORX_context. *)
  val mk_in_gsub_ligature_or_gsub_context_or_gsub_contextchain_or_morx_context: name:string -> in_gsub_ligature_or_gsub_context_or_gsub_contextchain_or_morx_context_t name_t
  (** For features accepting: GSUB_ligature, GSUB_context, GSUB_contextchain, MORX_context. *)
  val mk_in_gsub_multiple_or_gsub_ligature: name:string -> in_gsub_multiple_or_gsub_ligature_t name_t
  (** For features accepting: GSUB_multiple, GSUB_ligature. *)
  val mk_in_gsub_single_or_gpos_single: name:string -> in_gsub_single_or_gpos_single_t name_t
  (** For features accepting: GSUB_single, GPOS_single. *)
  val mk_in_gsub_single_or_gsub_alternate: name:string -> in_gsub_single_or_gsub_alternate_t name_t
  (** For features accepting: GSUB_single, GSUB_alternate. *)
  val mk_in_gsub_single_or_gsub_ligature: name:string -> in_gsub_single_or_gsub_ligature_t name_t
  (** For features accepting: GSUB_single, GSUB_ligature. *)

  (** {3 Untyped Feature} *)

  type 'a t
  val mk: ?scripts:Script.t list -> 'a name_t -> 'a t
  (** Feature constructor linking a named script with a language list to an untyped feature. *)
  val get_featurename: 'a t -> string
  val get_script_languages: 'a t -> Script.t list

  type lookup_feature_t = string * (Script.t list)
  val value: lookup_feature_t -> Util.Value.t
end
(***********************************************************)
module TypedFeature: sig

  type (_,_) selector_t
  (** selector of a lookup type among the acceptable type of a feature. *)

  type _ t
  (** Typed feature. *)

  val typeX_in_XY: select:('a Table.t, 'b Feature.t) selector_t -> 'b Feature.t -> 'a t
  (** Builds a typed feature from a type selector and a feature. *)

  val select_T_in_one: 'a Table.t Feature.t -> 'a t
  val select_T_in_all: 'a Table.t -> Feature.in_all_t Feature.t -> 'a t

  val select_gpos_mark2base_in_gpos_mark2base_or_gpos_mark2ligature:
    Feature.in_gpos_mark2base_or_gpos_mark2ligature_t Feature.t -> Table.gpos_mark2base_t t
  val select_gpos_mark2ligature_in_gpos_mark2base_or_gpos_mark2ligature:
    Feature.in_gpos_mark2base_or_gpos_mark2ligature_t Feature.t -> Table.gpos_mark2ligature_t t

  val select_gsub_multiple_in_gsub_multiple_or_gsub_ligature:
    Feature.in_gsub_multiple_or_gsub_ligature_t Feature.t -> Table.gsub_multiple_t t
  val select_gsub_ligature_in_gsub_multiple_or_gsub_ligature:
    Feature.in_gsub_multiple_or_gsub_ligature_t Feature.t -> Table.gsub_ligature_t t

  val select_gsub_single_in_gsub_single_or_gpos_single:
    Feature.in_gsub_single_or_gpos_single_t Feature.t -> Table.gsub_single_t t
  val select_gpos_single_in_gsub_single_or_gpos_single:
    Feature.in_gsub_single_or_gpos_single_t Feature.t -> Table.gpos_single_t t

  val select_gsub_single_in_gsub_single_or_gsub_alternate:
    Feature.in_gsub_single_or_gsub_alternate_t Feature.t -> Table.gsub_single_t t
  val select_gsub_alternate_in_gsub_single_or_gsub_alternate:
    Feature.in_gsub_single_or_gsub_alternate_t Feature.t -> Table.gsub_alternate_t t

  val select_gsub_single_in_gsub_single_or_gsub_ligature:
    Feature.in_gsub_single_or_gsub_ligature_t Feature.t -> Table.gsub_single_t t
  val select_gsub_ligature_in_gsub_single_or_gsub_ligature:
    Feature.in_gsub_single_or_gsub_ligature_t Feature.t -> Table.gsub_ligature_t t

  val select_gsub_context_in_gsub_context_or_gsub_contextchain_or_morx_context:
    Feature.in_gsub_context_or_gsub_contextchain_or_morx_context_t Feature.t -> Table.gsub_context_t t
  val select_gsub_contextchain_in_gsub_context_or_gsub_contextchain_or_morx_context:
    Feature.in_gsub_context_or_gsub_contextchain_or_morx_context_t Feature.t -> Table.gsub_contextchain_t t
  val select_morx_context_in_gsub_context_or_gsub_contextchain_or_morx_context:
    Feature.in_gsub_context_or_gsub_contextchain_or_morx_context_t Feature.t -> Table.morx_context_t t

  val select_gpos_pair_in_gpos_pair_or_gpos_context_or_gpos_contextchain_or_kern_statemachine:
    Feature.in_gpos_pair_or_gpos_context_or_gpos_contextchain_or_kern_statemachine_t Feature.t -> Table.gpos_pair_t t
  val select_gpos_context_in_gpos_pair_or_gpos_context_or_gpos_contextchain_or_kern_statemachine:
    Feature.in_gpos_pair_or_gpos_context_or_gpos_contextchain_or_kern_statemachine_t Feature.t -> Table.gpos_context_t t
  val select_gpos_contextchain_in_gpos_pair_or_gpos_context_or_gpos_contextchain_or_kern_statemachine:
    Feature.in_gpos_pair_or_gpos_context_or_gpos_contextchain_or_kern_statemachine_t Feature.t -> Table.gpos_contextchain_t t
  val select_kern_statemachine_in_gpos_pair_or_gpos_context_or_gpos_contextchain_or_kern_statemachine:
    Feature.in_gpos_pair_or_gpos_context_or_gpos_contextchain_or_kern_statemachine_t Feature.t -> Table.kern_statemachine_t t

  val select_gsub_ligature_in_gsub_ligature_or_gsub_context_or_gsub_contextchain_or_morx_context:
    Feature.in_gsub_ligature_or_gsub_context_or_gsub_contextchain_or_morx_context_t Feature.t -> Table.gsub_ligature_t t
  val select_gsub_context_in_gsub_ligature_or_gsub_context_or_gsub_contextchain_or_morx_context:
    Feature.in_gsub_ligature_or_gsub_context_or_gsub_contextchain_or_morx_context_t Feature.t -> Table.gsub_context_t t
  val select_gsub_contextchain_in_gsub_ligature_or_gsub_context_or_gsub_contextchain_or_morx_context:
    Feature.in_gsub_ligature_or_gsub_context_or_gsub_contextchain_or_morx_context_t Feature.t -> Table.gsub_contextchain_t t
  val select_morx_context_in_gsub_ligature_or_gsub_context_or_gsub_contextchain_or_morx_context:
    Feature.in_gsub_ligature_or_gsub_context_or_gsub_contextchain_or_morx_context_t Feature.t -> Table.morx_context_t t

  val get_lookup_type: 'a t -> 'a Table.t
  val get_feature_scripts: 'a t -> Feature.lookup_feature_t
  val value: _ t list -> Util.Value.t

end
(***********************************************************)
(* {2 Predefined Features} *)
(***********************************************************)
module PredefinedFeature: sig

  val dflt: ?scripts:Script.t list -> 'a Table.t -> 'a TypedFeature.t
  (** Default. *)

  val aalt: ?scripts:Script.t list -> (Feature.in_gsub_single_or_gsub_alternate_t Feature.t -> 'a TypedFeature.t) -> 'a TypedFeature.t
  (** Access All Alternates. *)
  val abvf: ?scripts:Script.t list -> unit -> Table.gsub_single_t TypedFeature.t
  (** Above Base Forms. *)
  val abvm: ?scripts:Script.t list -> (Feature.in_gpos_mark2base_or_gpos_mark2ligature_t Feature.t -> 'a TypedFeature.t) -> 'a TypedFeature.t
  (** Above Base Mark. *)
  val abvs: ?scripts:Script.t list -> unit -> Table.gsub_ligature_t TypedFeature.t
  (** Above Base Substitutions. *)
  val afrc: ?scripts:Script.t list -> unit -> Table.gsub_ligature_t TypedFeature.t
  (** Vertical Fractions. *)
  val akhn: ?scripts:Script.t list -> unit -> Table.gsub_ligature_t TypedFeature.t
  (** Akhand. *)
  val alig: ?scripts:Script.t list -> unit -> Table.gsub_ligature_t TypedFeature.t
  (** Ancient Ligatures. *)

  val blwf: ?scripts:Script.t list -> unit -> Table.gsub_ligature_t TypedFeature.t
  (** Below Base Forms. *)
  val blwm: ?scripts:Script.t list -> (Feature.in_gpos_mark2base_or_gpos_mark2ligature_t Feature.t -> 'a TypedFeature.t) -> 'a TypedFeature.t
  (** Below Base Mark. *)
  val blws: ?scripts:Script.t list -> unit -> Table.gsub_ligature_t TypedFeature.t
  (** Below Base Substitutions. *)

  val c2pc: ?scripts:Script.t list -> unit -> Table.gsub_single_t TypedFeature.t
  (** Capitals to Petite Capitals. *)
  val c2sc: ?scripts:Script.t list -> unit -> Table.gsub_single_t TypedFeature.t
  (** Capitals to Small Capitals. *)
  val calt: ?scripts:Script.t list -> (Feature.in_gsub_context_or_gsub_contextchain_or_morx_context_t Feature.t -> 'a TypedFeature.t) -> 'a TypedFeature.t
  (** Contextual Alternates. *)
  val case: ?scripts:Script.t list -> (Feature.in_gsub_single_or_gpos_single_t Feature.t -> 'a TypedFeature.t) -> 'a TypedFeature.t
  (** Case-Sensitive Forms. *)
  val ccmp: ?scripts:Script.t list -> (Feature.in_gsub_multiple_or_gsub_ligature_t Feature.t -> 'a TypedFeature.t) -> 'a TypedFeature.t
  (** Glyph Composition/Decomposition. *)
  val cfar: ?scripts:Script.t list -> unit -> Table.gsub_single_t TypedFeature.t
  (** Conjunct Form After Ro. *)
  val cjct: ?scripts:Script.t list -> unit -> Table.gsub_ligature_t TypedFeature.t
  (** Conjunct Forms. *)
  val clig: ?scripts:Script.t list -> unit -> Table.gsub_reversecchain_t TypedFeature.t
  (** Contextual Ligatures. *)
  val cpct: ?scripts:Script.t list -> unit -> Table.gpos_single_t TypedFeature.t
  (** Centered CJK Punctuation. *)
  val cpsp: ?scripts:Script.t list -> unit -> Table.gpos_single_t TypedFeature.t
  (** Capital Spacing. *)
  val cswh: ?scripts:Script.t list -> unit -> Table.gsub_reversecchain_t TypedFeature.t
  (** Contextual Swash. *)
  val curs: ?scripts:Script.t list -> unit -> Table.gpos_cursive_t TypedFeature.t
  (** Cursive Attachment. *)
  val cv00: ?scripts:Script.t list -> unit -> Table.gsub_single_t TypedFeature.t
  (** Character Variants 00. *)
  val cv01: ?scripts:Script.t list -> unit -> Table.gsub_single_t TypedFeature.t
  (** Character Variants 01. *)
  val cv02: ?scripts:Script.t list -> unit -> Table.gsub_single_t TypedFeature.t
  (** Character Variants 02. *)
  val cv03: ?scripts:Script.t list -> unit -> Table.gsub_single_t TypedFeature.t
  (** Character Variants 03. *)
  val cv04: ?scripts:Script.t list -> unit -> Table.gsub_single_t TypedFeature.t
  (** Character Variants 04. *)
  val cv05: ?scripts:Script.t list -> unit -> Table.gsub_single_t TypedFeature.t
  (** Character Variants 05. *)
  val cv06: ?scripts:Script.t list -> unit -> Table.gsub_single_t TypedFeature.t
  (** Character Variants 06. *)
  val cv07: ?scripts:Script.t list -> unit -> Table.gsub_single_t TypedFeature.t
  (** Character Variants 07. *)
  val cv08: ?scripts:Script.t list -> unit -> Table.gsub_single_t TypedFeature.t
  (** Character Variants 08. *)
  val cv09: ?scripts:Script.t list -> unit -> Table.gsub_single_t TypedFeature.t
  (** Character Variants 09. *)
  val cv10: ?scripts:Script.t list -> unit -> Table.gsub_single_t TypedFeature.t
  (** Character Variants 10. *)
  val cv99: ?scripts:Script.t list -> unit -> Table.gsub_single_t TypedFeature.t
  (** Character Variants 99. *)

  val dcap: ?scripts:Script.t list -> unit -> Table.gsub_single_t TypedFeature.t
  (** Drop Caps. *)
  val dist: ?scripts:Script.t list -> unit -> Table.gpos_pair_t TypedFeature.t
  (** Distance. *)
  val dlig: ?scripts:Script.t list -> unit -> Table.gsub_ligature_t TypedFeature.t
  (** Discretionary Ligatures. *)
  val dnom: ?scripts:Script.t list -> unit -> Table.gsub_single_t TypedFeature.t
  (** Denominators. *)
  val dpng: ?scripts:Script.t list -> unit -> Table.gsub_ligature_t TypedFeature.t
  (** Dipthongs (Obsolete). *)
  val dtls: ?scripts:Script.t list -> unit -> Table.gsub_single_t TypedFeature.t
  (** Dotless Forms. *)

  (** Expert Forms. *)
  val expt: ?scripts:Script.t list -> unit -> Table.gsub_single_t TypedFeature.t

  val falt: ?scripts:Script.t list -> unit -> Table.gsub_alternate_t TypedFeature.t
  (** Final Glyph On Line. *)
  val fin2: ?scripts:Script.t list -> (Feature.in_gsub_context_or_gsub_contextchain_or_morx_context_t Feature.t -> 'a TypedFeature.t) -> 'a TypedFeature.t
  (** Terminal Forms #2. *)
  val fin3: ?scripts:Script.t list -> (Feature.in_gsub_context_or_gsub_contextchain_or_morx_context_t Feature.t -> 'a TypedFeature.t) -> 'a TypedFeature.t
  (** Terminal Forms #3. *)
  val fina: ?scripts:Script.t list -> unit -> Table.gsub_single_t TypedFeature.t
  (** Terminal Forms. *)
  val flac: ?scripts:Script.t list -> (Feature.in_gsub_single_or_gsub_ligature_t Feature.t -> 'a TypedFeature.t) -> 'a TypedFeature.t
  (** Flattened Accents over Capitals. *)
  val frac: ?scripts:Script.t list -> (Feature.in_gsub_single_or_gsub_ligature_t Feature.t -> 'a TypedFeature.t) -> 'a TypedFeature.t
  (** Diagonal Fractions. *)
  val fwid: ?scripts:Script.t list -> (Feature.in_gsub_single_or_gpos_single_t Feature.t -> 'a TypedFeature.t) -> 'a TypedFeature.t
  (** Full Widths. *)

  val half: ?scripts:Script.t list -> unit -> Table.gsub_ligature_t TypedFeature.t
  (** Half Forms. *)
  val haln: ?scripts:Script.t list -> unit -> Table.gsub_ligature_t TypedFeature.t
  (** Halant Forms. *)
  val halt: ?scripts:Script.t list -> unit -> Table.gpos_single_t TypedFeature.t
  (** Alternative Half Widths. *)
  val hist: ?scripts:Script.t list -> unit -> Table.gsub_single_t TypedFeature.t
  (** Historical Forms. *)
  val hkna: ?scripts:Script.t list -> unit -> Table.gsub_single_t TypedFeature.t
  (** Horizontal Kana Alternatives. *)
  val hlig: ?scripts:Script.t list -> unit -> Table.gsub_ligature_t TypedFeature.t
  (** Historic Ligatures. *)
  val hngl: ?scripts:Script.t list -> (Feature.in_gsub_single_or_gsub_alternate_t Feature.t -> 'a TypedFeature.t) -> 'a TypedFeature.t
  (** Hanja to Hangul. *)
  val hojo: ?scripts:Script.t list -> unit -> Table.gsub_single_t TypedFeature.t
  (** Hojo (JIS X 0212-1990) Kanji Forms. *)
  val hwid: ?scripts:Script.t list -> (Feature.in_gsub_single_or_gpos_single_t Feature.t -> 'a TypedFeature.t) -> 'a TypedFeature.t
  (** Half Widths. *)

  val init: ?scripts:Script.t list -> unit -> Table.gsub_single_t TypedFeature.t
  (** Initial Forms. *)
  val isol: ?scripts:Script.t list -> unit -> Table.gsub_single_t TypedFeature.t
  (** Isolated Forms. *)
  val ital: ?scripts:Script.t list -> unit -> Table.gsub_single_t TypedFeature.t
  (** Italics. *)

  val jalt: ?scripts:Script.t list -> unit -> Table.gsub_alternate_t TypedFeature.t
  (** Justification Alternatives. *)
  val jajp: ?scripts:Script.t list -> (Feature.in_gsub_single_or_gsub_alternate_t Feature.t -> 'a TypedFeature.t) -> 'a TypedFeature.t
  (** Japanese Forms (Obsolete). *)
  val jp04: ?scripts:Script.t list -> unit -> Table.gsub_single_t TypedFeature.t
  (** JIS2004 Forms. *)
  val jp78: ?scripts:Script.t list -> (Feature.in_gsub_single_or_gsub_alternate_t Feature.t -> 'a TypedFeature.t) -> 'a TypedFeature.t
  (** JIS78 Forms. *)
  val jp83: ?scripts:Script.t list -> unit -> Table.gsub_single_t TypedFeature.t
  (** JIS83 Forms. *)
  val jp90: ?scripts:Script.t list -> unit -> Table.gsub_single_t TypedFeature.t
  (** JIS90 Forms. *)

  val kern: ?scripts:Script.t list -> (Feature.in_gpos_pair_or_gpos_context_or_gpos_contextchain_or_kern_statemachine_t Feature.t -> 'a TypedFeature.t) -> 'a TypedFeature.t
  (** Horizontal Kerning. *)

  val lfbd: ?scripts:Script.t list -> unit -> Table.gpos_single_t TypedFeature.t
  (** Left Bounds. *)
  val liga: ?scripts:Script.t list -> unit -> Table.gsub_ligature_t TypedFeature.t
  (** Standard Ligatures. *)
  val ljmo: ?scripts:Script.t list -> unit -> Table.gsub_ligature_t TypedFeature.t
  (** Leading Jamo Forms. *)
  val lnum: ?scripts:Script.t list -> unit -> Table.gsub_single_t TypedFeature.t
  (** Lining Figures. *)
 val locl: ?scripts:Script.t list -> unit -> Table.gsub_single_t TypedFeature.t
  (** Localized Forms. *)

  val mark: ?scripts:Script.t list -> (Feature.in_gpos_mark2base_or_gpos_mark2ligature_t Feature.t -> 'a TypedFeature.t) -> 'a TypedFeature.t
  (** Mark Positioning. *)
  val med2: ?scripts:Script.t list -> (Feature.in_gsub_context_or_gsub_contextchain_or_morx_context_t Feature.t -> 'a TypedFeature.t) -> 'a TypedFeature.t
  (** Medial Forms 2. *)
  val medi: ?scripts:Script.t list -> unit -> Table.gsub_single_t TypedFeature.t
  (** Medial Forms. *)
  val mgrk: ?scripts:Script.t list -> unit -> Table.gsub_single_t TypedFeature.t
  (** Mathematical Greek. *)
  val mkmk: ?scripts:Script.t list -> unit -> Table.gpos_mark2mark_t TypedFeature.t
  (** Mark to Mark. *)
  val mset: ?scripts:Script.t list -> (Feature.in_gsub_context_or_gsub_contextchain_or_morx_context_t Feature.t -> 'a TypedFeature.t) -> 'a TypedFeature.t
  (** Mark Positioning via Substitution. *)

  val nalt: ?scripts:Script.t list -> (Feature.in_gsub_single_or_gsub_alternate_t Feature.t -> 'a TypedFeature.t) -> 'a TypedFeature.t
  (** Alternate Annotation Forms. *)
  val nlck: ?scripts:Script.t list -> unit -> Table.gsub_single_t TypedFeature.t
  (** NLC Kanji Forms. *)
  val nukt: ?scripts:Script.t list -> unit -> Table.gsub_ligature_t TypedFeature.t
  (** Nukta Forms. *)
  val numr: ?scripts:Script.t list -> unit -> Table.gsub_single_t TypedFeature.t
  (** Numerators. *)

  val onum: ?scripts:Script.t list -> unit -> Table.gsub_single_t TypedFeature.t
  (** Oldstyle Figures. *)
  val opbd: ?scripts:Script.t list -> unit -> Table.gpos_single_t TypedFeature.t
  (** Optical Bounds. *)
  val ordn: ?scripts:Script.t list -> (Feature.in_gsub_ligature_or_gsub_context_or_gsub_contextchain_or_morx_context_t Feature.t -> 'a TypedFeature.t) -> 'a TypedFeature.t
  (** Ordinals. *)
  val ornm: ?scripts:Script.t list -> (Feature.in_gsub_single_or_gsub_alternate_t Feature.t -> 'a TypedFeature.t) -> 'a TypedFeature.t
  (** Ornaments. *)

  val palt: ?scripts:Script.t list -> unit -> Table.gpos_single_t TypedFeature.t
  (** Proportional Alternate Metrics. *)
  val pcap: ?scripts:Script.t list -> unit -> Table.gsub_single_t TypedFeature.t
  (** Lowercase to Petite Capitals. *)
  val pkna: ?scripts:Script.t list -> unit -> Table.gpos_single_t TypedFeature.t
  (** Proportional Kana. *)
  val pnum: ?scripts:Script.t list -> unit -> Table.gsub_single_t TypedFeature.t
  (** Proportional Numbers. *)
  val pref: ?scripts:Script.t list -> unit -> Table.gsub_ligature_t TypedFeature.t
  (** Pre Base Forms. *)
  val pres: ?scripts:Script.t list -> (Feature.in_gsub_ligature_or_gsub_context_or_gsub_contextchain_or_morx_context_t Feature.t -> 'a TypedFeature.t) -> 'a TypedFeature.t
  (** Pre Base Substitutions. *)
  val pstf: ?scripts:Script.t list -> unit -> Table.gsub_ligature_t TypedFeature.t
  (** Post Base Forms. *)
  val psts: ?scripts:Script.t list -> unit -> Table.gsub_ligature_t TypedFeature.t
  (** Post Base Substitutions. *)
  val pwid: ?scripts:Script.t list -> unit -> Table.gsub_single_t TypedFeature.t
  (** Proportional Width. *)

  val qwid: ?scripts:Script.t list -> (Feature.in_gsub_single_or_gpos_single_t Feature.t -> 'a TypedFeature.t) -> 'a TypedFeature.t
  (** Quarter Widths. *)

  val rand: ?scripts:Script.t list -> unit -> Table.gsub_alternate_t TypedFeature.t
  (** Randomize. *)
  val rkrf: ?scripts:Script.t list -> unit -> Table.gsub_ligature_t TypedFeature.t
  (** Rakar Forms. *)
  val rlig: ?scripts:Script.t list -> unit -> Table.gsub_ligature_t TypedFeature.t
  (** Required Ligatures. *)
  val rphf: ?scripts:Script.t list -> unit -> Table.gsub_ligature_t TypedFeature.t
  (** Reph Form. *)
  val rtbd: ?scripts:Script.t list -> unit -> Table.gpos_single_t TypedFeature.t
  (** Right Bounds. *)
  val rtla: ?scripts:Script.t list -> unit -> Table.gsub_single_t TypedFeature.t
  (** Right to Left Alternates. *)
  val rtlm: ?scripts:Script.t list -> unit -> Table.gsub_single_t TypedFeature.t
  (** Right to Left mirrored forms. *)
  val ruby: ?scripts:Script.t list -> unit -> Table.gsub_single_t TypedFeature.t
  (** Ruby Notational Forms. *)

  val salt: ?scripts:Script.t list -> (Feature.in_gsub_single_or_gsub_alternate_t Feature.t -> 'a TypedFeature.t) -> 'a TypedFeature.t
  (** Stylistic Alternatives. *)
  val sinf: ?scripts:Script.t list -> unit -> Table.gsub_single_t TypedFeature.t
  (** Scientific Inferiors. *)
  val smcp: ?scripts:Script.t list -> unit -> Table.gsub_single_t TypedFeature.t
  (** Lowercase to Small Capitals. *)
  val smpl: ?scripts:Script.t list -> unit -> Table.gsub_single_t TypedFeature.t
  (** Simplified Forms. *)
  val ss01: ?scripts:Script.t list -> unit -> Table.gsub_single_t TypedFeature.t
  (** Style Set 1. *)
  val ss02: ?scripts:Script.t list -> unit -> Table.gsub_single_t TypedFeature.t
  (** Style Set 2. *)
  val ss03: ?scripts:Script.t list -> unit -> Table.gsub_single_t TypedFeature.t
  (** Style Set 3. *)
  val ss04: ?scripts:Script.t list -> unit -> Table.gsub_single_t TypedFeature.t
  (** Style Set 4. *)
  val ss05: ?scripts:Script.t list -> unit -> Table.gsub_single_t TypedFeature.t
  (** Style Set 5. *)
  val ss06: ?scripts:Script.t list -> unit -> Table.gsub_single_t TypedFeature.t
  (** Style Set 6. *)
  val ss07: ?scripts:Script.t list -> unit -> Table.gsub_single_t TypedFeature.t
  (** Style Set 7. *)
  val ss08: ?scripts:Script.t list -> unit -> Table.gsub_single_t TypedFeature.t
  (** Style Set 8. *)
  val ss09: ?scripts:Script.t list -> unit -> Table.gsub_single_t TypedFeature.t
  (** Style Set 9. *)
  val ss10: ?scripts:Script.t list -> unit -> Table.gsub_single_t TypedFeature.t
  (** Style Set 10. *)
  val ss11: ?scripts:Script.t list -> unit -> Table.gsub_single_t TypedFeature.t
  (** Style Set 11. *)
  val ss12: ?scripts:Script.t list -> unit -> Table.gsub_single_t TypedFeature.t
  (** Style Set 12. *)
  val ss13: ?scripts:Script.t list -> unit -> Table.gsub_single_t TypedFeature.t
  (** Style Set 13. *)
  val ss14: ?scripts:Script.t list -> unit -> Table.gsub_single_t TypedFeature.t
  (** Style Set 14. *)
  val ss15: ?scripts:Script.t list -> unit -> Table.gsub_single_t TypedFeature.t
  (** Style Set 15. *)
  val ss16: ?scripts:Script.t list -> unit -> Table.gsub_single_t TypedFeature.t
  (** Style Set 16. *)
  val ss17: ?scripts:Script.t list -> unit -> Table.gsub_single_t TypedFeature.t
  (** Style Set 17. *)
  val ss18: ?scripts:Script.t list -> unit -> Table.gsub_single_t TypedFeature.t
  (** Style Set 18. *)
  val ss19: ?scripts:Script.t list -> unit -> Table.gsub_single_t TypedFeature.t
  (** Style Set 19. *)
  val ss20: ?scripts:Script.t list -> unit -> Table.gsub_single_t TypedFeature.t
  (** Style Set 20. *)
  val ssty: ?scripts:Script.t list -> (Feature.in_gsub_single_or_gsub_alternate_t Feature.t -> 'a TypedFeature.t) -> 'a TypedFeature.t
  (** Script Style. *)
  val subs: ?scripts:Script.t list -> unit -> Table.gsub_single_t TypedFeature.t
  (** Subscript. *)
  val sups: ?scripts:Script.t list -> unit -> Table.gsub_single_t TypedFeature.t
  (** Superscript. *)
  val swsh: ?scripts:Script.t list -> (Feature.in_gsub_single_or_gsub_alternate_t Feature.t -> 'a TypedFeature.t) -> 'a TypedFeature.t
  (** Swash. *)

  val titl: ?scripts:Script.t list -> unit -> Table.gsub_single_t TypedFeature.t
  (** Titling. *)
  val tjmo: ?scripts:Script.t list -> unit -> Table.gsub_ligature_t TypedFeature.t
  (** Trailing Jamo Forms. *)
  val tnam: ?scripts:Script.t list -> unit -> Table.gsub_single_t TypedFeature.t
  (** Traditional Name Forms. *)
  val tnum: ?scripts:Script.t list -> unit -> Table.gsub_single_t TypedFeature.t
  (** Tabular Numbers. *)
  val trad: ?scripts:Script.t list -> (Feature.in_gsub_single_or_gsub_alternate_t Feature.t -> 'a TypedFeature.t) -> 'a TypedFeature.t
  (** Traditional Forms. *)
  val twid: ?scripts:Script.t list -> (Feature.in_gsub_single_or_gpos_single_t Feature.t -> 'a TypedFeature.t) -> 'a TypedFeature.t
  (** Third Widths. *)

  val unic: ?scripts:Script.t list -> unit -> Table.gsub_single_t TypedFeature.t
  (** Unicase. *)

  val valt: ?scripts:Script.t list -> unit -> Table.gpos_single_t TypedFeature.t
  (** Alternate Vertical Metrics. *)
  val vatu: ?scripts:Script.t list -> unit -> Table.gsub_ligature_t TypedFeature.t
  (** Vattu Variants. *)
  val vert: ?scripts:Script.t list -> unit -> Table.gsub_single_t TypedFeature.t
  (** Vertical Alternates (obs). *)
  val vhal: ?scripts:Script.t list -> unit -> Table.gpos_single_t TypedFeature.t
  (** Alternate Vertical Half Metrics. *)
  val vjmo: ?scripts:Script.t list -> unit -> Table.gsub_ligature_t TypedFeature.t
  (** Vowel Jamo Forms. *)
  val vkna: ?scripts:Script.t list -> unit -> Table.gsub_single_t TypedFeature.t
  (** Vertical Kana Alternates. *)
  val vkrn: ?scripts:Script.t list -> (Feature.in_gpos_pair_or_gpos_context_or_gpos_contextchain_or_kern_statemachine_t Feature.t -> 'a TypedFeature.t) -> 'a TypedFeature.t
  (** Vertical Kerning. *)
  val vpal: ?scripts:Script.t list -> unit -> Table.gpos_single_t TypedFeature.t
  (** Proportional Alternate Vertical Metrics. *)
  val vrt2: ?scripts:Script.t list -> unit -> Table.gsub_single_t TypedFeature.t
  (** Vertical Rotation & Alternates. *)

  val zero: ?scripts:Script.t list -> unit -> Table.gsub_single_t TypedFeature.t
  (** Slashed Zero. *)
end
