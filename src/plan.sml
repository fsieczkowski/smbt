(* vim: set et ts=4: *)
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

signature PLAN =
sig
    type t

    (** an empty plan. **)
    val empty : t

    (** Execute a given plan. **)
    val execute : t -> unit

    (** Execute a plan once, then re-execute on every modification of files. **)
    val watch : t -> unit

    (** Append one plan with another. **)
    val compose : t -> t -> t

    (** Pretty-print a plan, mostly for debugging. **)
    val toString : t -> string

    (** Parse a file and select a specific target to build a plan. **)
    val parseFile : string -> string -> t

end

(** The structure for target plans, which represent everything needed
  to generate target files and execute commands. **)
structure Plan :> PLAN =
struct
    open AST

    type t = {ffi : ffidec option, srcs : string list, opts : (string * string) list}
    datatype slice = Slice of t * pkgdec list * slice option

    val empty = {ffi = NONE, srcs = [], opts = []}

    (* TODO: figure this one out, some options might be duplicated or contradictory. *)
    fun composeCFlags c1 c2 = c1 ^ c2

    (** Compose the FFI blocks; the right-hand side block is preferred when
        header files are considered. **)
    fun composeFFI (FFID f1) (FFID f2) =
	let fun compcf c1 c2 =
		case c1 of NONE => c2
			 | SOME cf1 =>
			   case c2 of NONE => c1
				    | SOME cf2 => SOME (composeCFlags cf1 cf2)
	    fun comphdr c1 c2 = case c2 of NONE => c1
					 | SOME _ => c2
	in FFID {ffisrc = #ffisrc f1 @ #ffisrc f2, lnkopts = #lnkopts f1 @ #lnkopts f2,
		 cflags = compcf (#cflags f1) (#cflags f2), hdr = comphdr (#hdr f1) (#hdr f2)}
	end

    (** Compose two plans; the right-hand side plan has preference when it comes to
        options (hence, these options put in the front) **)
    fun compose (p : t) (p' : t) =
	let fun cmp f1 f2 =
		case f1 of NONE => f2
			 | SOME f1' => case f2 of NONE => f1
						| SOME f2' => SOME (composeFFI f1' f2')
	in {ffi = cmp (#ffi p) (#ffi p'), srcs = #srcs p @ #srcs p',
	    opts = #opts p' @ #opts p} : t
	end

    (** Select an appropriate part of an AST for a given target **)
    fun selectOne target (tname, Dec dec) =
	let val lplan = {ffi = #ffi dec, srcs = #src dec, opts = #opts dec}
	in if target = tname then SOME (Slice (lplan, #deps dec, NONE))
	   else Option.map (fn s => Slice (lplan, #deps dec, SOME s))
			   (selectMany target (#targets dec))
	end
    and selectMany target [] = NONE
      | selectMany target (td :: ts) = case selectOne target td of
					   NONE => selectMany target ts
					 | SOME s => SOME s

    (** Handle the dependencies: generate the appropriate plan from the link & target **)
    fun handlePkg (SmackPKG (pkg, vsn, tn)) =
	(* TODO: to implement this we'd need to expose an appropriate interface
	   in Smackage *)
	raise Fail "Smackage packages not supported (yet!)."
      | handlePkg (LocalPKG (p, tn)) = parseFile p tn

    (** Transform the selected slice into a plan **)
    and foldSlice (Slice (p, deps, os)) =
	let fun fd deps s = List.foldl (fn (pk, s) => compose (handlePkg pk) s) s deps
	    fun aux (Slice (p, deps, NONE)) s = fd deps (compose p s)
	      | aux (Slice (p, deps, SOME sl)) s = aux sl (fd deps (compose p s))
	    val start = fd deps p
	in case os of NONE => start | SOME sl => aux sl start
	end

    (** Build a plan using an .sb file **)
    and parseFile fp tname =
	let val (ss, targs) = Elaborate.elaborateSmb (Parser.parseFile fp)
	    val sl = case selectMany tname targs of
			 SOME s => s
		       | NONE => raise Fail ("Target " ^ tname ^ " not found!")
	in foldSlice sl end

    (** TODO: this should invoke the appropriate backend implementation **)
    fun execute t = print "Execute plan.\n"

    (** Execute the plan, and then go into a watch loop, re-invoking execute
        whenever files are modified. **)
    fun watch (t : t) =
        let
            val _ = execute t
	    val fs = case #ffi t of SOME (FFID f) => #ffisrc f | NONE => []
            val _ = Watch.until (fs @ #srcs t)
            val _ = print ("Modification detected, executing target...\n")
        in
            watch t
        end

    fun toString p = "<PLAN>"

end
