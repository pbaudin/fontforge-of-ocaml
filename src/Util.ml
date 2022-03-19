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

module Import = struct
  type fontforge
  type psMat
  type extFontForge
  type _ obj =
    | Fontforge : fontforge obj
    | PsMat : psMat obj
(*  | ExtFontForge : extFontForge obj *)

  let typename : type a . a obj -> string = function
    | Fontforge -> "fontforge"
    | PsMat -> "psMat"
(*  | ExtFontForge -> "extFontForge" *)

  type pyobject = Pytypes.pyobject
  type 'a t ='a obj * pyobject

  let pp_type : type a . Format.formatter -> a obj -> unit =
    fun fmt typ -> Fmt.string fmt (typename typ)

  let pretty_type : type a . Format.formatter -> a t -> unit =
    fun fmt (typ,_) -> pp_type fmt typ

  type 'a lazy_object = unit -> 'a t

  let call_once ~f memo = match !memo with
      | Some r -> r
      | None -> let r = f () in memo := Some r ; r

  let init = let memo = ref None in
    fun () -> call_once memo ~f:(fun () -> if not (Py.is_initialized ())
                                  then (Fmt.pr "Init Python...@.";
                                        Py.initialize ~version:3 ()) else ())

  let import memo typ = call_once memo ~f:(fun () -> init (); typ, (Py.import (typename typ)))

  let fontforge = let memo = ref None in fun () -> import memo Fontforge
  let psMat = let memo = ref None in fun () -> import memo PsMat
(* let extFontForge = let memo = ref None in fun () -> import memo ExtFontForge *)

  let python_of : type a . a lazy_object -> Pytypes.pyobject =
    fun import -> snd (import ())
end
(***********************************************************)
module Object = struct
  type contour
  type font
  type glyph
  type glyphPen
  type layer
  type matrix
  type point
  type selection

  type _ obj =
    | Contour : contour obj
    | Font : font obj
    | Glyph : glyph obj
    | GlyphPen : glyphPen obj
    | Layer : layer obj
    | Matrix : matrix obj
    | Point : point obj
    | Selection : selection obj

  type pyobject = Pytypes.pyobject
  type 'a t = 'a obj * pyobject

  let pp_type : type a . Format.formatter -> a obj -> unit =
    fun fmt -> function
      | Contour -> Fmt.string fmt "contour"
      | Font  -> Fmt.string fmt "font"
      | Glyph -> Fmt.string fmt "glyp"
      | GlyphPen -> Fmt.string fmt "glypPen"
      | Layer -> Fmt.string fmt "layer"
      | Matrix -> Fmt.string fmt "matrix"
      | Point -> Fmt.string fmt "point"
      | Selection -> Fmt.string fmt "selection"
  let pretty_type : type a . Format.formatter -> a t -> unit =
    fun fmt (typ,_) -> pp_type fmt typ

  let python_of (x:'a t) = snd x
  let of_python : type a . typ:a obj -> pyobject -> a t = fun ~typ pyobject -> (typ,pyobject)

  let pretty : type a . Format.formatter -> a t -> unit =
    fun fmt x -> Py.Object.format_repr fmt (python_of x)

end
(***********************************************************)
module Value = struct
  open Ppx_python_runtime

  type _ obj =
    | Object : 'a Object.obj -> 'a Object.t obj
    | Unit : unit obj
    | Bool : bool obj
    | Float : float obj
    | Int : int obj
    | String : string obj
    | Comb : 'a obj -> 'a list obj
    | List : 'a obj -> 'a list obj
    | Array : 'a obj -> 'a list obj
    | Nullable: 'a obj -> 'a option obj
    | Option : 'a obj -> 'a option obj
    | Pair : 'a obj * 'b obj -> ('a*'b) obj
    | Coord : (float*float) obj

  let rec pp_type : type a . Format.formatter -> a obj -> unit =
    fun fmt -> function
      | Object typ -> Fmt.pf fmt "(Object %a)" Object.pp_type typ
      | Unit -> Fmt.string fmt "Unit"
      | Bool -> Fmt.string fmt "Bool"
      | Float -> Fmt.string fmt "Float"
      | Int -> Fmt.string fmt "Int"
      | String -> Fmt.string fmt "String"
      | Comb typ -> Fmt.pf fmt "(Comb %a)" pp_type typ
      | List typ -> Fmt.pf fmt "(List %a)" pp_type typ
      | Array typ -> Fmt.pf fmt "(Array %a)" pp_type typ
      | Nullable typ -> Fmt.pf fmt "(Nullable %a)" pp_type typ
      | Option typ -> Fmt.pf fmt "(Option %a)" pp_type typ
      | Pair(ty1,ty2) -> Fmt.pf fmt "(Pair %a * %a)" pp_type ty1 pp_type ty2
      | Coord -> Fmt.string fmt "Coord"

  let rec python_of : type a . (a obj * a) -> Pytypes.pyobject = function
    | Object _, v -> Object.python_of v
    | Unit, _ -> Py.null
    | Bool, v -> [%python_of: bool] v
    | Float, v -> [%python_of: float] v
    | Int, v -> [%python_of: int] v
    | String, v -> [%python_of: string] v
    | Comb t, v -> Base.List.fold (List.rev v) ~init:Py.none
                     ~f:(fun acc v-> Py.Tuple.of_list  [ python_of (t,v) ; acc ])
    | List t, v -> Py.List.of_list_map (fun v -> python_of (t,v)) v
    | Array t, v -> Py.List.of_array_map (fun v -> python_of (t,v)) (Array.of_list v)
    | Nullable t, v -> (match v with None -> Py.null | Some v -> python_of (t,v))
    | Option t, v -> (match v with None -> Py.none | Some v -> python_of (t,v))
    | Pair(t1,t2), (v1,v2) -> Py.Tuple.of_list [ python_of(t1,v1) ; python_of (t2,v2) ]
    | Coord, (v1,v2) -> Py.Tuple.of_list  [ [%python_of: float] v1 ; [%python_of: float] v2 ]

  let rec of_python : type a . typ:a obj -> Pytypes.pyobject -> a =
    fun ~typ v -> match typ with
      | Object typ -> Object.of_python ~typ v
      | Unit -> ()
      | Bool -> if Py.Int.check v
        then 0 <> [%of_python: int] v
        else [%of_python: bool] v
      | Float -> [%of_python: float] v
      | Int -> [%of_python: int] v
      | String -> if Py.is_none v || Py.is_null v then "" else [%of_python: string] v
      | List typ -> Py.List.to_list_map (of_python ~typ) v
      | Array typ -> Array.to_list (Py.List.to_array_map (of_python ~typ) v)
      | Comb t1 -> (* [v1,[v2,...[vn,None]...]] *)
        let v1,v2 = Py.Tuple.to_pair v in
        (of_python ~typ:t1 v1)::(if Py.is_null v2 then [] else of_python ~typ v2)
      | Nullable typ -> if Py.is_null v then None else Some (of_python ~typ v)
      | Option typ ->  if Py.is_none v then None else Some (of_python~typ v)
      | Pair(ty1,ty2) ->
        let v1,v2 = Py.Tuple.to_pair v in
        (of_python ~typ:ty1 v1), (of_python ~typ:ty2 v2)
      | Coord -> let v1,v2 = Py.Tuple.to_pair v in
        ([%of_python: float] v1), ([%of_python: float] v2)

  type t = Value: 'a obj * 'a -> t
  let value : type a . typ:a obj -> a -> t = fun ~typ v -> Value (typ,v)

  let pretty_type : Format.formatter -> t -> unit =
    fun fmt (Value (typ,_)) -> pp_type fmt typ

  let rec pretty : type a . Format.formatter -> (a obj * a)  -> unit =
    fun fmt -> function
      | Object _, v -> Object.pretty fmt v
      | Unit, _ -> Fmt.string fmt "()"
      | Bool, v -> Fmt.bool fmt v
      | Float, v -> Fmt.float fmt v
      | Int, v -> Fmt.int fmt v
      | String, v -> Fmt.string fmt v
      | List t, v -> Fmt.pf fmt "[%a]" (Fmt.list ~sep:(fun fmt () -> Fmt.string fmt "; ") (fun fmt v -> pretty fmt (t,v))) v
      | Array t, v -> Fmt.pf fmt "[%a]" (Fmt.list ~sep:(fun fmt () -> Fmt.string fmt "; ") (fun fmt v -> pretty fmt (t,v))) v
      | Comb _, [] -> Fmt.string fmt "None"
      | ((Comb t) as typ), (v::vs) -> Fmt.pf fmt "[%a,%a]" pretty (t,v) pretty (typ,vs)
      | Nullable t, v -> (match v with None -> Fmt.string fmt "Null" | Some v -> pretty fmt (t,v))
      | Option t, v ->  (match v with None -> Fmt.string fmt "None" | Some v -> pretty fmt (t,v))
      | Pair(t1,t2), (v1,v2) ->  Fmt.pf fmt "(%a,%a)" pretty (t1,v1) pretty (t2,v2)
      | Coord, (v1,v2) -> Fmt.pf fmt "(%f,%f)" v1 v2
  let pretty fmt (Value (typ, v)) = pretty fmt (typ,v)

  let python_of = function Value (typ,v) -> python_of (typ,v)
end
(***********************************************************)
module API = struct
  let params : Value.t list -> Pytypes.pyobject array =
    fun args -> Array.of_list (Base.List.map args ~f:Value.python_of)

  let size : type a . a Object.t -> int = fun x -> Py.Object.size (Object.python_of x)

  let get : type a b . typ:b Value.obj -> m:string -> a Object.t -> b =
    fun ~typ ~m o ->
      Value.of_python ~typ Pyops.((Object.python_of o).@$(m))

  let set : type a b . typ:b Value.obj -> m:string -> a Object.t -> b-> unit =
    fun ~typ ~m o v ->
      Pyops.((Object.python_of o).@$(m) <- Value.(python_of (value ~typ v)))

  let call_function : type a b . b Import.lazy_object -> typ:a Value.obj -> m:string -> Value.t list -> a =
    fun o ~typ ~m args ->
      Value.of_python ~typ (Py.Module.get_function (Import.python_of o) m (params args))

  let call_method : type a b . typ:a Value.obj -> m:string -> Value.t list -> b Object.t -> a =
    fun ~typ ~m args o ->
      Value.of_python ~typ (Py.Object.call_method (Object.python_of o) m (params args))
end
(***********************************************************)
