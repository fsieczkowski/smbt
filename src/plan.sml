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

    exception DepError of string

end

(** The structure for target plans, which represent everything needed
  to generate target files and execute commands. **)
structure Plan :> PLAN =
struct
    open AST

    type t = {ffi : ffidec option, srcs : string list, opts : (string * string) list,
              prehks : (string * string list) list, posthks : (string * string list) list, prefix : string}
    datatype slice = Slice of t * pkgdec list * slice option

    val empty = {ffi = NONE, srcs = [], opts = [], prehks = [], posthks = [], prefix = "/"}

    (** This attempts to deal with the messiness of relative paths.
        For example, we have a .sm file that uses another .sm file, like so:

        smb /path/to/foobar.sm bla

        and in /path/to/foobar.sm, we have sources:
          foo.sml
          ../bar.sml
          $(FOOBAR)/baz.mlb

        These should be added to the current target as:
          /path/to/foo.sml
          /path/bar.sml
          $(FOOBAR)/baz.mlb

        'prefix' should be the directory that contains the .sm file,
        e.g., /path/to
     **)
     fun resolvePath prefix file =
        if CompilerUtil.realFile file then
            OS.Path.mkAbsolute {path=file, relativeTo = prefix}
        else file

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
        options (hence, these options put in the front) 
     **)
    fun compose (p : t) (p' : t) =
        let fun cmp f1 f2 =
                case f1 of NONE => f2
                         | SOME f1' => case f2 of NONE => f1
                                                | SOME f2' => SOME (composeFFI f1' f2')
        in {ffi = cmp (#ffi p) (#ffi p'), srcs = #srcs p @ #srcs p', opts = #opts p' @ #opts p,
            prehks = #prehks p @ #prehks p', posthks = #posthks p' @ #posthks p, prefix = #prefix p'} : t
        end

    (** Select an appropriate part of an AST for a given target **)
    fun selectOne prefix target (tname, Dec dec) =
        let
            val lplan = {ffi = Option.map (prefixFFIDec prefix) (#ffi dec), 
                         srcs = map (resolvePath prefix) (#src dec), 
                         opts = #opts dec, prehks = [(prefix, #prehks dec)],
                         posthks = [(prefix, #posthks dec)], prefix = prefix}
        in 
            if target = tname then SOME (Slice (lplan, #deps dec, NONE))
            else Option.map (fn s => Slice (lplan, #deps dec, SOME s))
                            (selectMany prefix target (#targets dec))
        end
    and selectMany prefix target [] = NONE
      | selectMany prefix target (td :: ts) = case selectOne prefix target td of
                                           NONE => selectMany prefix target ts
                                         | SOME s => SOME s

    exception DepError of string

    val selectedPkgs = ref [] : (string * string * string * string) list ref

    fun chckPkg pkg vsn tn src =
        case List.find (fn d => #1 d = pkg) (!selectedPkgs)
         of SOME (_, vsn', tn', src') =>
            if vsn = vsn' andalso tn = tn' then false
            else raise DepError ("Package " ^ pkg ^ " required at version " ^ vsn ^ " and target " ^ tn ^ " at file " ^
                                 src ^ " while at version " ^ vsn' ^ " and target " ^ tn' ^ " at file " ^ src' ^ ".")
          | NONE => true before selectedPkgs := (pkg, vsn, tn, src) :: !selectedPkgs

    (** Handle the dependencies: generate the appropriate plan from the link & target **)
    fun handlePkg filename (SmackPKG (pkg, vsn, tn)) =
        let
            val _ = print ("[WARNING] Experimental Smackage mode engaged. This way be dragons...\n")
            val path = case SmackageUtil.pathinfo (pkg,vsn) of
                            [p] => p
                          | _ => raise Fail ("Could not obtain path for `" ^ pkg ^ "' from smackage")
            val inst = chckPkg pkg vsn tn filename
            val path' = resolvePath path "build.sm"
            (** TODO: We should put some effort into actually invoking smackage 'get' here. **)
        in
            (if inst then planFile path' tn else empty)
            handle e => (print ("Package include for `" ^ pkg ^ "' failed.\n"); raise e)
        end
      | handlePkg src (LocalPKG (p, tn)) = planFile p tn
        handle e => (print ("Package include for `" ^ p ^ "' (called from \"" ^ src ^ "\") failed.\n"); raise e)

    (** Transform the selected slice into a plan **)
    and foldSlice src s =
        let fun fd (p, deps) = List.foldl (fn (pk, pln) => compose (handlePkg src pk) pln) p (rev deps)
            fun aux (Slice (p, deps, NONE)) = (p, deps)
              | aux (Slice (p, deps, SOME sl)) = case aux sl of (p', deps') => (compose p p', deps @ deps')
        in fd (aux s)
        end

    (** Build a plan using an .sm file **)
    and planFile fp tname =
        let 
        val path = CompilerUtil.absolutePath fp
        val prefix = OS.Path.dir path

        val (ss, targs) = Elaborate.elaborateSmb (Parser.parseFile fp)
            val sl = case selectMany prefix tname targs of
                         SOME s => s
                       | NONE => raise Fail ("Target " ^ tname ^ " not found!")
        in foldSlice fp sl end

    fun parseFile fp tname = (selectedPkgs := []; planFile fp tname)

    fun toString (p : t) = 
        "Plan:\n" ^
        getOrNone "" (Option.map ffidecToString (#ffi p)) ^ "\n" ^
        "Sources: " ^ String.concatWith ", " (#srcs p) ^ "\n" ^
        "Options:\n" ^ String.concatWith "\n" (map (fn (k,v) => k ^ " = " ^ v) (#opts p)) ^ "\n" ^
        "Pre hooks:\n" ^ String.concatWith "\n" (map (fn (dir, cmds) => "cd " ^ dir ^ "\n" ^ String.concatWith "\n" cmds) (#prehks p)) ^ "\n" ^
        "Post hooks:\n" ^ String.concatWith "\n" (map (fn (dir, cmds) => "cd " ^ dir ^ "\n" ^ String.concatWith "\n" cmds) (#posthks p)) ^ "\n"

    fun execute (t : t) =
        let
            val compiler = CompilerUtil.selectCompiler (#opts t)

            val (ffisrcs,lnkopts,cflags,hdr) = 
                (case #ffi t of NONE => ([],[],[],NONE) 
                              | SOME (FFID f) =>
                                 (#ffisrc f, 
                                 #lnkopts f, 
                                 case #cflags f of NONE => [] 
                                                 | SOME f' => [f'],
                                 #hdr f))

            val hasOutput = List.exists (fn ("output",v) => true | _ => false) (#opts t)

            fun runHooks (dir, cmds) = (CompilerUtil.chDir dir; List.app CompilerUtil.exec cmds)
            fun runPhks () = 
                    (print " - Running pre-hooks\n";
                     List.app runHooks (#prehks t); CompilerUtil.chDir (#prefix t))
            fun runQhks () = 
                    (print " - Running post-hooks\n";
                     List.app runHooks (#posthks t); CompilerUtil.chDir (#prefix t))

            val compile =
                case compiler of
                    CompilerUtil.NullCompiler => ignore
                  | CompilerUtil.MLton => MLtonCompiler.compile
                  | CompilerUtil.SMLNJ => SMLNJCompiler.compile
                  | CompilerUtil.PolyML => PolyMLCompiler.compile
                  | CompilerUtil.MoscowML => MoscowMLCompiler.compile
                  | CompilerUtil.SMLSharp => SMLSharpCompiler.compile
                  | CompilerUtil.MLKit => MLKitCompiler.compile

            val interactive = 
                case compiler of
                    CompilerUtil.NullCompiler => ignore
                  | CompilerUtil.MLton => MLtonCompiler.interactive
                  | CompilerUtil.SMLNJ => SMLNJCompiler.interactive
                  | CompilerUtil.PolyML => PolyMLCompiler.interactive
                  | CompilerUtil.MoscowML => MoscowMLCompiler.interactive
                  | CompilerUtil.SMLSharp => SMLSharpCompiler.interactive
                  | CompilerUtil.MLKit => MLKitCompiler.interactive
        in
            runPhks ();
            if hasOutput andalso not (!Config.interactive) then 
                            compile (
                                #srcs t,
                                ffisrcs,
                                lnkopts,
                                cflags,
                                hdr,
                                #opts t)
                else if (!Config.interactive) then
                            interactive (
                                #srcs t,
                                ffisrcs,
                                lnkopts,
                                cflags,
                                hdr,
                                #opts t)
                else print "[smbt] No output target, stopping prior to compilation.\n";
            runQhks ()
        end

    (** Execute the plan, and then go into a watch loop, re-invoking execute
        whenever files are modified. **)
    fun watch (t : t) = 
        let
            val _ = execute t
            val _ = print ("[smbt] In continuous mode, use ctrl-c to exit.\n")
            val fs = case #ffi t of SOME (FFID f) => #ffisrc f | NONE => []
            val fs' = List.filter CompilerUtil.realFile (fs @ #srcs t)
            val _ = Watch.until fs'
            val _ = print ("[smbt] Modification detected, executing target...\n")
        in
            watch t
        end

end
