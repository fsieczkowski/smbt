structure AST =
struct

  datatype ffidec = FFID of {ffisrc : string list, lnkopts : string list,
                             cflags : string option, hdr : string option}
  datatype pkgdec = SmackPKG of string * string * string | LocalPKG of string * string
  datatype dec    = Dec of {ffi : ffidec option, src : string list, deps : pkgdec list,
                            opts : (string * string) list, targets : (string * dec) list}

end

structure Elaborate =
struct

  open AST
  local 
    open Pre_AST
    open Sum

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
  fun expandMacros ds =
      let fun subst env str = List.foldl (fn ((f, t), str) => replaceAll f t str) str env
	  fun expFFI env f =
	      case f of FFIFile s => FFIFile (subst env s)
		      | FFILnk  s => FFILnk  (subst env s)
		      | FFIHdr  s => FFIHdr  (subst env s)
		      | FFIFlgs s => FFIFlgs (subst env s)
	  fun expDec env d =
	      case d of Opt (k, v) => INL (Opt (k, subst env v))
		      | FFI fs => INL (FFI (List.map (expFFI env) fs))
		      | Src ss => INL (Src (List.map (subst env) ss))
		      | Pkg (SmackPKG (name, version, target)) =>
			  INL (Pkg (SmackPKG (subst env name, version, Option.map (subst env) target)))
		      | Pkg (LocalPKG (name, target)) =>
			  INL (Pkg (LocalPKG (subst env name, subst env target)))
		      | Macro (k, v) => INR ((k,v) :: env)
		      | Target (name, decs) => INL (Target (name, expDecs env decs))
	  and expDecs env [] = []
	    | expDecs env (d :: ds) =
	      case expDec env d of INL d'   => d' :: expDecs env ds
				 | INR env' => expDecs env' ds
      in expDecs [] ds
      end
  end

  exception ElabError of string

  fun elabFFI fs =
      let val src = ref ([] : string list)
	  val lnk = ref ([] : string list)
	  val cfl = ref (NONE : string option)
	  val hdr = ref (NONE : string option)
	  local open Pre_AST
	  in fun elab (FFIFile s) = src := s :: !src
	       | elab (FFILnk  l) = lnk := l :: !lnk
	       | elab (FFIFlgs f) =
		 (case !cfl of NONE => cfl := SOME f
			     | SOME f' => raise ElabError
			         ("Duplicate cflags entry in FFI block: \"" ^ f ^ "\" and \"" ^ f' ^"\"."))
	       | elab (FFIHdr  h) =
		 (case !hdr of NONE => hdr := SOME h
			     | SOME h' => raise ElabError
				 ("Duplicate header entry in FFI block: \"" ^ h ^ "\" and \"" ^ h' ^"\"."))
	  end
      in List.map elab fs; FFID {cflags = !cfl, ffisrc = rev (!src), hdr = !hdr, lnkopts = rev (!lnk)}
      end

  local type edec = {ffib : ffidec option ref, srcs : string list option ref, pkgs : pkgdec list ref,
		     opts : (string * string) list ref, tars : (string * dec) list ref}
	fun defED () = {ffib = ref NONE, srcs = ref NONE, pkgs = ref [], opts = ref [], tars = ref []} : edec
	open Pre_AST
  in
  fun elabDec ed tn (Opt (k,v)) = let val opts = #opts ed in opts := (k,v) :: !opts end
    | elabDec ed tn (FFI fs) =
      let val ffib = #ffib ed
      in case !ffib of NONE => ffib := SOME (elabFFI fs)
		     | SOME fb => raise ElabError "Duplicate ffi blocks located."
      end
    | elabDec ed tn (Src ss) =
      let val srcs = #srcs ed
      in case !srcs of NONE => srcs := SOME ss
		     | SOME ss' => raise ElabError "Duplicate sources blocks located."
      end
    | elabDec ed tn (Pkg (SmackPKG (n, v, ot))) =
      (* TODO: sanitize version number *)
      let val pkgs = #pkgs ed
      in pkgs := AST.SmackPKG (n, v, getOpt (ot, tn)) :: !pkgs end
    | elabDec ed tn (Pkg (LocalPKG (n, t))) = let val pkgs = #pkgs ed in pkgs := AST.LocalPKG (n, t) :: !pkgs end
    (* TODO: check target uniqueness *)
    | elabDec ed _  (Target (tn, ds)) = let val tars = #tars ed in tars := (tn, elabDecs ds tn) :: !tars end
    | elabDec ed tn (Macro _) = raise ElabError "Unexpanded macro located."
  and elabDecs ds tn =
    let val ed = defED ()
    in List.map (elabDec ed tn) ds;
       Dec {ffi = !(#ffib ed), src = rev (getOpt (!(#srcs ed), [])), deps = rev (!(#pkgs ed)),
	    opts = rev (!(#opts ed)), targets = rev (!(#tars ed))}
    end
  end

  fun elabSmb (ss, ts) = (ss, List.map (fn Pre_AST.Target (tn, ds) => (tn, elabDecs ds tn)
					  | _ => raise ElabError "Non-target found at toplevel") ts)

  fun substAndElab (ss, ts) = elabSmb (ss, expandMacros ts)
  fun doit s = substAndElab (Parser.parse Parser.smb s)

end
