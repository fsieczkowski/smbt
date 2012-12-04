(* Smbt, an SML build tool
 *  Copyright (c) 2012 Filip Sieczkowski & Gian Perrone
 * 
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted,
 * provided that the above copyright notice appear in all copies and that
 * both the copyright notice and this permission notice and warranty
 * disclaimer appear in supporting documentation, and that the name of
 * the above copyright holders, or their entities, not be used in
 * advertising or publicity pertaining to distribution of the software
 * without specific, written prior permission.
 * 
 * The above copyright holders disclaim all warranties with regard to
 * this software, including all implied warranties of merchantability and
 * fitness. In no event shall the above copyright holders be liable for
 * any special, indirect or consequential damages or any damages
 * whatsoever resulting from loss of use, data or profits, whether in an
 * action of contract, negligence or other tortious action, arising out
 * of or in connection with the use or performance of this software.
 *
*)

(** This structure contains the datatypes that describe elaborated Smbt entities **)
structure AST =
struct

  datatype ffidec = FFID of {ffisrc : string list, lnkopts : string list,
                             cflags : string option, hdr : string option}
  datatype pkgdec = SmackPKG of string * string * string | LocalPKG of string * string
  datatype dec    = Dec of {ffi : ffidec option, src : string list, deps : pkgdec list,
                            opts : (string * string) list, targets : (string * dec) list,
                            prehks : string list, posthks : string list}

  fun getOrNone d NONE = d
    | getOrNone d (SOME t) = t

  fun ffidecToString (FFID {ffisrc,lnkopts,cflags,hdr}) =
        "FFI:\n" ^
        "  Sources: " ^ String.concatWith ", " ffisrc ^ "\n" ^
        "  Linkopts: " ^ String.concatWith ", " lnkopts ^ "\n" ^
        "  CFlags: " ^ getOrNone "" cflags ^ "\n" ^
        "  Header: " ^ getOrNone "" hdr


  fun pkgdecToString (SmackPKG (name,vers,target)) = 
        "Smackage(" ^ name ^ ", " ^ vers ^ ", " ^ target ^ ")"
   | pkgdecToString (LocalPKG (path,target)) =
        "Local(" ^ path ^ ", " ^ target ^ ")"

  fun decToString ind (Dec {ffi, src, deps, opts, targets, prehks, posthks}) =
        ind ^ "Dec:\n" ^
        ind ^ getOrNone "" (Option.map ffidecToString ffi) ^ "\n" ^
        ind ^ "Sources: " ^ String.concatWith ", " src ^ "\n" ^
        ind ^ "Dependencies: " ^ String.concatWith ", " (map pkgdecToString deps) ^ "\n" ^
        ind ^ "Pre hooks: " ^ String.concatWith "; " prehks ^ "\n" ^
        ind ^ "Post hooks: " ^ String.concatWith "; " posthks ^ "\n" ^
        ind ^ "Targets:\n" ^
        String.concatWith "\n" (map (fn (n,d) => ind ^ n ^ ":\n" ^ decToString (ind ^ "   ") d) targets)

  (** Add a path prefix to an FFI declaration. **)
  fun prefixFFIDec prefix (FFID {ffisrc,lnkopts,cflags,hdr}) =
    FFID {ffisrc = map (fn f => OS.Path.mkAbsolute {path=f, relativeTo = prefix}) ffisrc,
          lnkopts = lnkopts,
          cflags = cflags,
          hdr = Option.map (fn h => OS.Path.mkAbsolute {path=h, relativeTo = prefix}) hdr}
end

(** Elaboration process takes a complete SMBT build description and proceeds by
 *  expanding the macro definitions and gathers the appropriate parts of targets
 *  into a more convenient format described by the AST structure. It also performs
 *  further syntactic well-formedness checks. **)
signature ELAB =
sig

    exception ElabError of string

    val elaborateSmb : Pre_AST.spec option * Pos.t Pre_AST.dec list ->
                       Pre_AST.spec option * (string * AST.dec) list

end

structure Elaborate :> ELAB =
struct

  open AST
  exception ElabError of string

  local 
    open Pre_AST
    open Sum

    (** replace all occurrences of "from" with "to" in the given "text" **)
    fun replaceAll from to text =
        let val pat = String.explode ("$(" ^ from ^ ")")
            val sub = rev (String.explode to)
            fun matchpref ([], xs : char list) = SOME xs
              | matchpref (x :: ps, y :: qs) = if x = y then matchpref (ps, qs) else NONE
              | matchpref _ = NONE
            fun aux (ft, []) = String.implode (rev ft)
              | aux (ft, bk as x :: tl) =
                case matchpref (pat, bk) of NONE => aux (x :: ft, tl)
                                          | SOME rst => aux (sub @ ft, rst)
        in aux ([], String.explode text)
        end
  in
  (** Expand all the macro definitions throughout the declarations.
      This respects the scope of definitions **)
  fun expandMacros ds =
      let fun subst env str = List.foldl (fn ((f, t), str) => replaceAll f t str) str env
          fun expFFI env f =
              case f of FFIFile (s, pos) => FFIFile (subst env s, pos)
                      | FFILnk  (s, pos) => FFILnk  (subst env s, pos)
                      | FFIHdr  (s, pos) => FFIHdr  (subst env s, pos)
                      | FFIFlgs (s, pos) => FFIFlgs (subst env s, pos)
          fun expDec env d =
              case d of Opt (k, v, pos) => INL (Opt (k, subst env v, pos))
                      | FFI (fs, pos) => INL (FFI (List.map (expFFI env) fs, pos))
                      | Src (ss, pos) => INL (Src (List.map (subst env) ss, pos))
                      | Pkg (SmackPKG (name, version, target, pos), pos') =>
                          INL (Pkg (SmackPKG (subst env name, version, Option.map (subst env) target, pos), pos'))
                      | Pkg (LocalPKG (name, target, pos), pos') =>
                          INL (Pkg (LocalPKG (subst env name, subst env target, pos), pos'))
                      | Macro (k, v, pos) => INR ((k,v) :: env)
                      | Target (name, decs, pos) => INL (Target (name, expDecs env decs, pos))
		      | PreHook (hs, pos) => INL (PreHook (List.map (subst env) hs, pos))
		      | PostHook (hs, pos) => INL (PostHook (List.map (subst env) hs, pos))
          and expDecs env [] = []
            | expDecs env (d :: ds) =
              case expDec env d of INL d'   => d' :: expDecs env ds
                                 | INR env' => expDecs env' ds
      in expDecs [] ds
      end
  end

  (** Check well-formedness of an ffi block and fold it into a convenient structure **)
  fun elabFFI fs =
      let val src = ref ([] : string list)
          val lnk = ref ([] : string list)
          val cfl = ref (NONE : string option)
          val hdr = ref (NONE : string option)
          local open Pre_AST
          in fun elab (FFIFile (s, pos)) = src := s :: !src
               | elab (FFILnk  (l, pos)) = lnk := l :: !lnk
               | elab (FFIFlgs (f, pos)) =
                 (case !cfl of NONE => cfl := SOME f
                             | SOME f' => raise ElabError
                                 ("Duplicate cflags entry in FFI block at " ^ Pos.toString pos ^
				  ": \"" ^ f ^ "\" and \"" ^ f' ^"\"."))
               | elab (FFIHdr  (h, pos)) =
                 (case !hdr of NONE => hdr := SOME h
                             | SOME h' => raise ElabError
                                 ("Duplicate header entry in FFI block at " ^ Pos.toString pos ^
				  ": \"" ^ h ^ "\" and \"" ^ h' ^"\"."))
          end
      in List.map elab fs; FFID {cflags = !cfl, ffisrc = rev (!src), hdr = !hdr, lnkopts = rev (!lnk)}
      end

  val targets = ref [] : string list ref

  local type edec = {ffib : ffidec option ref, srcs : string list option ref, pkgs : pkgdec list ref,
                     opts : (string * string) list ref, tars : (string * dec) list ref,
                     preh : string list option ref, posth : string list option ref}
        fun defED () = {ffib = ref NONE, srcs = ref NONE, pkgs = ref [], opts = ref [], tars = ref [],
                        preh = ref NONE, posth = ref NONE} : edec
        open Pre_AST
	fun setNonDup f v errstr pos =
	    case !f of NONE => f := SOME v
		     | SOME _ => raise ElabError ("Duplicate " ^ errstr ^ " blocks located at " ^ Pos.toString pos ^ ".")
  in
  fun elabDec ed tn (Opt (k, v, pos)) = let val opts = #opts ed in opts := (k, v) :: !opts end
    | elabDec ed tn (FFI (fs, pos)) = setNonDup (#ffib ed) (elabFFI fs) "ffi" pos
    | elabDec ed tn (Src (ss, pos)) = setNonDup (#srcs ed) ss "sources" pos
    | elabDec ed tn (Pkg (SmackPKG (n, v, ot, pos), pos')) =
      (* TODO: sanitize version number *)
      let val pkgs = #pkgs ed
      in pkgs := AST.SmackPKG (n, v, getOpt (ot, tn)) :: !pkgs end
    | elabDec ed tn (Pkg (LocalPKG (n, t, pos), pos')) =
      let val pkgs = #pkgs ed in pkgs := AST.LocalPKG (n, t) :: !pkgs end
    | elabDec ed _  (Target (tn, ds, pos)) =
      let val tars = #tars ed
          val _ = if List.exists (fn x => x = tn) (!targets)
                  then raise ElabError ("Duplicate target names " ^ tn ^ " at " ^ Pos.toString pos ^ ".")
                  else targets := tn :: !targets
      in tars := (tn, elabDecs ds tn) :: !tars end
    | elabDec ed tn (Macro _) = raise ElabError "Unexpanded macro located."
    | elabDec ed tn (PreHook (hs, pos)) = setNonDup (#preh ed) hs "pre-hooks" pos
    | elabDec ed tn (PostHook (hs, pos)) = setNonDup (#posth ed) hs "post-hooks" pos
  and elabDecs ds tn =
    let val ed = defED ()
    in List.map (elabDec ed tn) ds;
       Dec {ffi = !(#ffib ed), src = getOpt (!(#srcs ed), []), deps = rev (!(#pkgs ed)),
            opts = rev (!(#opts ed)), targets = rev (!(#tars ed)),
	    prehks = getOpt (!(#preh ed), []), posthks = getOpt (!(#posth ed), [])}
    end
  end

  fun elabSmb (ss, ts) =
      let fun matchT (Pre_AST.Target (tn, ds, pos)) = (tn, elabDecs ds tn)
            | matchT _ = raise ElabError "Non-target found at the top level."
          val _ = targets := []
      in  (ss, List.map matchT ts)
      end

  fun elaborateSmb (ss, ts) = elabSmb (ss, expandMacros ts)

end
