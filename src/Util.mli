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

module Import: sig
  type fontforge
  type psMat
  type extFontForge
  type pyobject
  type _ obj =
      Fontforge: fontforge obj
    | PsMat: psMat obj
(*  | ExtFontForge: extFontForge obj
  (* if some code has to be written in python *)
*)

  type 'a t = 'a obj * pyobject
  type 'a lazy_object = unit -> 'a t

  val fontforge: fontforge lazy_object
  val psMat: psMat lazy_object
(* val extFontForge: extFontForge lazy_object
  (* if some code has to be written in python *)
 *)

  val python_of: 'a lazy_object -> pyobject
  val pp_type: Format.formatter -> 'a obj -> unit
  val pretty_type: Format.formatter -> 'a t -> unit

  val init: unit -> unit
end
(***********************************************************)
module Object:sig
  type contour
  type font
  type glyph
  type glyphPen
  type layer
  type matrix
  type point
  type selection

  type _ obj =
    | Contour: contour obj
    | Font: font obj
    | Glyph: glyph obj
    | GlyphPen: glyphPen obj
    | Layer: layer obj
    | Matrix: matrix obj
    | Point: point obj
    | Selection: selection obj
  type pyobject
  type 'a t = 'a obj * pyobject
  val pretty_type: Format.formatter -> 'a t -> unit
  val pretty: Format.formatter -> 'a t -> unit

  val python_of: 'a t -> Pytypes.pyobject
  val of_python: typ:'a obj -> Pytypes.pyobject -> 'a t
  val pp_type: Format.formatter -> 'a obj -> unit
end
(***********************************************************)
module Value: sig
  type _ obj =
    | Object: 'a Object.obj -> 'a Object.t obj
    | Unit: unit obj
    | Bool: bool obj
    | Float: float obj
    | Int: int obj
    | String: string obj
    | Comb: 'a obj -> 'a list obj
    | List: 'a obj -> 'a list obj
    | Array: 'a obj -> 'a list obj
    | Nullable: 'a obj -> 'a option obj
    | Option: 'a obj -> 'a option obj
    | Pair: 'a obj * 'b obj -> ('a * 'b) obj
    | Coord: (float*float) obj
  type t
  val pp_type: Format.formatter -> 'a obj -> unit
  val pretty_type: Format.formatter -> t -> unit
  val pretty: Format.formatter -> t -> unit

  val value: typ:'a obj -> 'a -> t
  val python_of: t -> Pytypes.pyobject
  val of_python: typ:'a obj -> Pytypes.pyobject -> 'a
end
(***********************************************************)
module API: sig
  val size: 'a Object.t -> int

  val get: typ:'b Value.obj -> m:string -> 'a Object.t -> 'b
  (** get an attribute *)
  val set: typ:'b Value.obj -> m:string -> 'a Object.t -> 'b -> unit
  (** set an attribute *)

  val call_method:
    typ:'a Value.obj -> m:string -> Value.t list -> 'b Object.t -> 'a
  (** method call of an object *)

  val call_function:
    'b Import.lazy_object -> typ:'a Value.obj -> m:string -> Value.t list -> 'a
  (** function call of an imported module *)
end
(***********************************************************)
