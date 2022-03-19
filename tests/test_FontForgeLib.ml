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

open FontForge
module FF = FontForge

let coords ~nb ~kx ~dx ~ky ~dy = Base.Array.init nb
    ~f:(fun i -> (Float.of_int (kx*i+dx)),(Float.of_int (ky*i+dy)))

let mk_contour ~is_quadratic ~name ~mk coords = let open Contour in
  let c = FF.contour () in
  Is_quadratic.set c is_quadratic;
  Name.set c name;
  Base.Array.iteri coords ~f:(fun i xy -> if i = 0
                               then moveToCoord xy c
                               else mk xy c);
  lineToCoord (Array.get coords 0) c;
  Closed.set c true;
  c

let pretty_point fmt pt = let open Point in
  Fmt.pf fmt "(%d,%d,curve=%b, sel=%b)"
    (Float.to_int (X.get pt)) (Float.to_int (Y.get pt)) (On_curve.get pt) (Selected.get pt)

let pretty_contour fmt c = let open Contour in
  Fmt.pf fmt "name=%S len=%d is_quadratic=%b is_closed=%b points=[%a]"
    (Name.get c) (len c) (Is_quadratic.get c) (Closed.get c)
    (Fmt.list ~sep:(fun fmt () -> Fmt.string fmt ", ") pretty_point) (to_list c)

let _ =
  let version = FF.version () in
  Fmt.pr "- 0 - fontforge version = %S@." version;
  let font = FF.openFont ~filename:"./FreeMonoBold.ttf" in
  Fmt.pr "- 1 - font loaded@.";
  let fontname = Font.Fontname.get font in
  Fmt.pr "- 1 - fontname = %s@." fontname;
  let fonts = FF.fonts() in
  Fmt.pr "- 1 - number of fonts loaded = %d@." (List.length fonts);
  let font = match fonts with | [x] -> x | _ -> assert false in
  let fontname = Font.Fontname.get font in
  Fmt.pr "- 1 - fontname = %s@." fontname;

  let font = FF.openFont ~filename:"./FreeMono.ttf" in
  Fmt.pr "- 2 - another font loaded@.";
  let fonts = FF.fonts() in
  Fmt.pr "- 2 - number of fonts loaded = %d@." (List.length fonts);
  List.iter (fun font ->
      let fontname = Font.Fontname.get font in
      Fmt.pr "- 2 - fontname = %s@." fontname) fonts;

  let fontname = Font.Fontname.get font in
  Fmt.pr "- 3 - fontname = %s@." fontname;
  let em = Font.Em.get font in
  let ascent = Font.Ascent.get font in
  let descent = Font.Descent.get font in
  Fmt.pr "- 3 - em = %d ; ascent = %d ; descent = %d@." em ascent descent;

  let selection = Font.Selection.get font  in
  Fmt.pr "- 4 - selection@.";
  Selection.all selection ;
  Fmt.pr "- 4 - select all by slots@.";
  if true then begin
    let n = ref 0 in
    Selection.iter ~slot:(fun _ -> incr n) selection ;
    Fmt.pr "- 4 - nb of iterations = %d@." !n;
  end;
  let selection = Selection.byGlyphs selection in
  Fmt.pr "- 4 - select all by glyphs@.";
  let n = ref 0 in
  Selection.iter ~glyph:(fun _ -> incr n) selection ;
  Fmt.pr "- 4 - nb of iterations = %d@." !n;

  let selection = Font.Selection.get font  in
  Fmt.pr "- 5 - selection@.";
  let ranges = [ Selection.glyphname "1" ; Selection.glyphname "8" ] in
  Selection.select ~ranges:true ~request:ranges selection;
  Fmt.pr "- 5 - selection set from 1-8@.";
  let ranges = [ Selection.glyphname "B" ;  Selection.glyphname "J" ] in
  Selection.select ~ranges:true ~less:false ~request:ranges selection;
  Fmt.pr "- 5 - selection added from B-J@.";
  let ranges = [ Selection.glyphname "F" ;  Selection.glyphname "Z" ] in
  Selection.select ~ranges:true ~less:true ~request:ranges selection;
  Fmt.pr "- 5 - selection removed from F-Z@.";
  let singletons = [ Selection.glyphname "9" ; Selection.glyphname "F" ] in
  Selection.select ~less:false ~request:singletons selection;
  Fmt.pr "- 5 - selection added 9 and F@.";
  let singletons = [ Selection.code 48 ; Selection.code 65 ] in
  Selection.select ~encoding:true ~less:false ~request:singletons selection;
  Fmt.pr "- 5 - selection added encoding 48 and 65 (0 and A)@.";
  let selection = Selection.byGlyphs selection in
  Fmt.pr "- 5 - select them by glyphs@.";
  let n = ref 0 in
  Selection.iter
    ~glyph:(fun x -> incr n;
             if !n = 1 then Fmt.pr "- 5 - iter on selected ghyphs from fontname = %S@."
                 (Font.Fontname.get (Glyph.font x));
             Fmt.pr "(%S,%d) ; "
               (Glyph.Glyphname.get x)
               (Glyph.Unicode.get x) )
    selection ;
  Fmt.pr "@.- 5 - nb of iterations = %d@." !n;
  Fmt.pr "- 5 - exist A in the font = %b@."
    (Font.contains_glyphname ~glyphname:"A" font);
  Fmt.pr "- 5 - exist 65 (A) in the font = %b@."
    (Font.contains_unicode ~unicode:65 font);
  Fmt.pr "- 5 - exist G in the font = %b@."
    (Font.contains_glyphname ~glyphname:"G" font);
  Fmt.pr "- 5 - exist 71 (F) in the font = %b@."
    (Font.contains_unicode ~unicode:71 font);

  Font.copy font;
  Fmt.pr "- 6 - copy selected glyphs into the clip board@.";

  let font = FF.font () in
  Font.Fontname.set font "MY-FONT" ;
  Font.Em.set font em ;
  Font.Ascent.set font ascent;
  Font.Descent.set font descent;
  Font.Encoding.set font  "UnicodeFull";
  Font.Version.set font "1.0";
  Font.Weight.set font "Regular";
  Font.Familyname.set font "SIMPLE";
  Font.Fontname.set font "SIMPLE" ;
  Font.Fullname.set font "SIMPLE";

  let copyright = Font.Copyright.get font in
  (* extract the year and the $USER *)
  let copyright = String.sub copyright 0 14 in
  Fmt.pr "- 6 - Copyright (from fontforge): %s<YYYY>, <USER>@."
    copyright;
  Font.Copyright.set  font "COPYRIGHT (c) this-year, my-self";
  Fmt.pr "- 6 - Copyright (modified): %s@."
    (Font.Copyright.get font);

  Fmt.pr "- 6 - create a new font = %s@."
    (Font.Fontname.get font);

  let selection = Font.Selection.get font  in
  let ranges = [ Selection.glyphname "0" ; Selection.glyphname "9" ] in
  Selection.select ~ranges:true ~request:ranges selection;
  let ranges = [ Selection.glyphname "A" ; Selection.glyphname "F" ] in
  Selection.select ~ranges:true ~less:false ~request:ranges selection;
  Font.paste font;
  Fmt.pr "- 6 - paste the glyphs from A to F into %s@."
    (Font.Fontname.get font);

  Selection.all selection;
  let selection = Selection.byGlyphs selection in
  Fmt.pr "- 6 - select all by glyphs@.";
  let n = ref 0 in
  Selection.iter
    ~glyph:(fun x -> incr n;
             if !n = 1 then Fmt.pr "- 6 - iter on selected ghyphs from fontname = %S@."
                 (Font.Fontname.get (Glyph.font x));
             Fmt.pr "(%S,%d) ; "
               (Glyph.Glyphname.get x)
               (Glyph.Unicode.get x) )
    selection ;
  Fmt.pr "@.- 6 - nb of iterations = %d@." !n;
  Fmt.pr "- 6 - exist A in the font = %b@."
    (Font.contains_glyphname ~glyphname:"A" font);
  Fmt.pr "- 6 - exist 65 (A) in the font = %b@."
    (Font.contains_unicode ~unicode:65 font);
  Fmt.pr "- 6 - exist G in the font = %b@."
    (Font.contains_glyphname ~glyphname:"G" font);
  Fmt.pr "- 6 - exist 71 (F) in the font = %b@."
    (Font.contains_unicode ~unicode:71 font);

  Fmt.pr "- 7 - kern features...@." ;
  let feature =
    PredefinedFeature.kern ~scripts:[ Script.mk ~languages:["dflt"] "latn" ]
                    TypedFeature.select_gpos_pair_in_gpos_pair_or_gpos_context_or_gpos_contextchain_or_kern_statemachine in
  Fmt.pr "- 7 - GPOS_pair lookup table...@." ;
  let lookuptable = Table.(mk_lookup_table GPOS_pair ~name:"kerningLookUp") in
  Font.addLookup lookuptable Table.GPOS_pair ~flags:[] feature font;
  Fmt.pr "- 7 - kerningSubtable lookup subtable...@." ;
  let lookupsubtable = Table.mk_lookup_subtable lookuptable ~name:"kerningSubtable" in
  Font.addLookupSubtable lookupsubtable font;
  Fmt.pr "- 7 - create a %s table and %s subtable@."
    (Table.get_lookup_name lookuptable)
    (Table.get_subtable_name lookupsubtable);
  let glyphA = Font.glyph_from_code ~unicode:65 font in
  let glyphname = "F" in
  let glyphF = Font.glyph_from_name ~glyphname font in
  let kerning = -(Glyph.Width.get glyphF) / 2 in
  let spec = Table.args_gpos_pair_kern ~glyphname ~kerning in
  Glyph.addPosSub lookupsubtable spec glyphA;
  Fmt.pr "- 7 - kerning %s and %s with delta = %d@."
    (Glyph.Glyphname.get glyphA) glyphname kerning;

  Fmt.pr "- 8 - ligature features...@.";
  let feature = PredefinedFeature.liga ~scripts:[ Script.mk ~languages:["dflt"] "latn" ] () in
  let lookuptable = Table.(mk_lookup_table GSUB_ligature ~name:"LigatureLookUp") in
  Font.addLookup lookuptable Table.GSUB_ligature ~flags:[] feature font;
  let lookupsubtable = Table.mk_lookup_subtable lookuptable ~name:"LigatureSubtable" in
  Font.addLookupSubtable lookupsubtable font;
  Fmt.pr "- 8 - create a %s table and %s subtable@."
    (Table.get_lookup_name lookuptable)
    (Table.get_subtable_name lookupsubtable);
  let ligaAE = "liga_A_E" in
  let glyph_ligaAE = Font.createChar ~unicode:(-1) ~name:ligaAE font in
  Selection.select_from_glyphname ~name:"E" selection;
  Font.copy font;
  Selection.select_from_glyphname ~name:ligaAE selection;
  Font.paste font;
  let x = Float.of_int ((Glyph.Width.get glyphA) / 2) in
  Fmt.pr "- 8 - translate x=%f@." x;
  let matrix = PsMat.translate ~x ~y:0.0 in
  Glyph.transform ~matrix glyph_ligaAE;
  Selection.select_from_glyphname ~name:"A" selection;
  Font.copy font;
  Selection.select_from_glyphname ~name:ligaAE selection;
  Font.pasteInto font;
  let spec = Table.args_gsub_ligature ~glyphname1:"A" ~glyphname2:"E" ~others:[] in
  Glyph.addPosSub lookupsubtable spec glyph_ligaAE;
  Fmt.pr "- 8 - create a ligature glyph for AE = %s@." ligaAE;

  Fmt.pr "- 9 - fina features...@." ;
  let feature = PredefinedFeature.fina ~scripts:[ Script.mk ~languages:["dflt"] "DFLT" ] () in
  Fmt.pr "- 9 - GSUB_single lookup table...@." ;
  let lookuptable = Table.(mk_lookup_table GSUB_single ~name:"FinalLookUp") in
  Font.addLookup lookuptable Table.GSUB_single ~flags:[] feature font;
  Fmt.pr "- 9 - FinalSubtable lookup subtable...@." ;
  let lookupsubtable = Table.mk_lookup_subtable lookuptable ~name:"FinalSubtable" in
  Font.addLookupSubtable lookupsubtable font;
  Fmt.pr "- 9 - create a %s table and %s subtable@."
    (Table.get_lookup_name lookuptable)
    (Table.get_subtable_name lookupsubtable);
  let finaE = "E.fina" in
  let glyph_finaE = Font.createChar ~unicode:(-1) ~name:finaE font in
  Selection.select_from_glyphname ~name:"E" selection;
  Font.copy font;
  Selection.select_from_glyphname ~name:finaE selection;
  Font.paste font;
  let matrix = PsMat.translate ~x:0.0 ~y:(Float.of_int (ascent / 4)) in
  Glyph.transform ~matrix glyph_finaE;
  let _glyphE = Font.glyph_from_name ~glyphname:"E" font in
  let spec = Table.args_gsub_single ~glyphname:"E" in
  Glyph.addPosSub lookupsubtable spec glyph_finaE;
  Fmt.pr "- 9 - create a substitution glyph for final E = %s@." finaE;

  Fmt.pr "- 10 - points...@.";
  let nb_pts = 3 in
  let coords1 = coords ~nb:nb_pts ~kx:4 ~dx:0 ~ky:4 ~dy:1 in
  let pt1 = FF.pointCoord (1.0,1.0) in
  let pt = FF.pointCoord (Array.get coords1 0) in
  Fmt.pr "- 10 - point %a@." pretty_point pt;

  Fmt.pr "- 11 - contours...@.";
  let c1 = FF.contour () in
  Fmt.pr "- 11 - empty contour: name=%S len=%d is_quadratic=%b is_closed=%b@."
    (Contour.Name.get c1) (Contour.len c1)
    (Contour.Is_quadratic.get c1) (Contour.Closed.get c1);
  (try ignore (Contour.nth 0 c1)
   with | Contour.Index_out_of_bounds -> Fmt.pr "- 11 - no c[0]@.");
  (try Contour.set_nth 0 pt1 c1;
   with | Contour.Index_out_of_bounds -> Fmt.pr "- 11 - cannot set c[0]@.");
  Contour.Name.set c1 "1";
  Base.Array.iteri coords1 ~f:(fun i xy -> if i = 0
                               then Contour.moveToCoord xy c1
                               else Contour.lineToCoord xy c1);
  Contour.lineToCoord (Array.get coords1 0) c1;
  Contour.Closed.set c1 true;
  Fmt.pr "- 11 - line contour c1: %a@." pretty_contour c1;
  (try Fmt.pr "- 11 - c1[0]=%a@." pretty_point (Contour.nth 0 c1)
   with | Contour.Index_out_of_bounds -> Fmt.pr "- 11 - no c1[0]@.");

  let c = Contour.extract ~min:(0) ~max:nb_pts c1 in
  let c0 = Contour.add_point pt1 c in
  let c2 = Contour.add_contour c c in
  Fmt.pr "- 11 - extracted contour c: %a@." pretty_contour c;
  Fmt.pr "- 11 - c0=c+pt1: %a@." pretty_contour c0;
  Fmt.pr "- 11 - c2=c+c: %a@." pretty_contour c2;

  let coord1 = Array.get coords1 1 in
  Fmt.pr "- 11 - c contains coord: (%f,%f)=%b@."
    (fst coord1) (snd coord1) (Contour.contains_coord coord1 c);
  Fmt.pr "- 11 - c contains coord: (0.0,0.0)=%b@." (Contour.contains_coord (0.0,0.0) c);
  Fmt.pr "- 11 - c contains point pt0: %b@." (Contour.contains_point pt c);
  Fmt.pr "- 11 - c contains point pt1: %b@." (Contour.contains_point pt1 c);
  (try Contour.set_nth 1 pt1 c;
     Fmt.pr "- 11 - set c[1]=pt1@.";
     Fmt.pr "- 11 - c contains point pt1: %b@." (Contour.contains_point pt1 c)
   with | Contour.Index_out_of_bounds -> Fmt.pr "- 11 - cannot set c[1]=pt1@.");

  let coords2 = coords ~nb:3 ~kx:1 ~dx:1 ~ky:2 ~dy:2 in
  let c2 = mk_contour ~is_quadratic:true ~name:"2"
      ~mk:(fun ((x,y) as xy) c -> Contour.quadraticToCoord
              ~cp:((x -. 2.0),(y -. 2.0)) ~pt:xy c)
      coords2 in
  Fmt.pr "- 11 - quadratic contour c2: %a@." pretty_contour c2;

  let coords3 = coords ~nb:3 ~kx:3 ~dx:3 ~ky:2 ~dy:2 in
  let c3 = mk_contour ~is_quadratic:false ~name:"3"
      ~mk:(fun ((x,y) as xy) c ->Contour.cubicToCoord
            ~cp1:((x +. 2.0),(y +. 2.0))
            ~cp2:((x -. 2.0),(y -. 2.0))
            ~pt:xy c)
      coords3 in
  Fmt.pr "- 11 - cubic contour c3: %a@." pretty_contour c3;

  Contour.append_contour c3 c1 ;
  Fmt.pr "- 11 - c1+=c3 : %a@." pretty_contour c1;
  Contour.append_point pt1 c1 ;
  Fmt.pr "- 11 - c1+= pt1: %a@." pretty_contour c1;

  Fmt.pr "- Z - ends...@." ;
  let filename = "./test_FontForgeLib.ttf" in
  Font.generate ~filename font;
  Font.close font;
  Fmt.pr "- Z - Font file generated = %s@." filename
