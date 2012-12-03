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

(** Signature for interfaces to compilers. **)
signature COMPILER =
sig
    (** The compiler interface's representation of the plan. 
        In order:
          sources,
          ffi sources,
          ffi linkopts,
          ffi cflags,
          export header file,
          options
    **)
    type compileTask = string list * string list * string list * string list * string option * (string * string) list

    val name : string

    (** Invoke a compiler. Non-opaque types (and tuples, no less!) so that we 
       can share an interface between different compilers. **)
    val compile : compileTask -> unit

    (** Invoke the compiler in interactive mode (if possible...) **)
    val interactive : compileTask -> unit
end

(** Implements common functionality that may be shared easily across compilers and tools. **)
structure CompilerUtil =
struct
    datatype compiler =
        NullCompiler
      | MLton
      | SMLNJ
      | PolyML
      | MoscowML
      | MLKit

    type compileTask = string list * string list * string list * string list * string option * (string * string) list

    fun exec s =
        let
            val _ = if (!Config.verbose) then
                        print (s ^ "\n") else ()

            fun e x = if (!Config.noExec) then
                            (print ("Execute: " ^ x ^ "\n"); true)
                        else
                            OS.Process.isSuccess (OS.Process.system x)
        in
            if e s then ()
                else raise Fail ("Invoking command `" ^ s ^ "' failed")
        end

    (* If the current directory is writable, we create a directory
       .smbt, and use that to create temp files.
       Failing that, we try /tmp. *)
    fun tempdir () =
        let
            val t = (OS.FileSys.isDir ".smbt"; true) handle OS.SysErr _ => 
                        let
                            val t' = OS.FileSys.mkDir ".smbt"
                        in
                            true
                        end handle _ => false

            val t' = if t then ".smbt" else (OS.FileSys.mkDir "/tmp/.smbt"; "/tmp/.smbt")
        in
            t'
        end

    (** Make all paths absolute, suitable for inclusion in MLB or CM files.
        Careful to treat vars of the style $(SMACKAGE) or $/basis" correctly
        (i.e., not mangling them).
    **)
    fun absolutePath s = if String.isPrefix "$" s then s
                            else OS.FileSys.fullPath s handle e => raise Fail ("Source file not found: `" ^ s ^ "'\n")

    (** Determine if this is a real file or not. **)
    fun realFile s = not (String.isPrefix "$" s)

    fun selectOpt l k = 
        Option.map (fn (k,v) => v) (List.find (fn (k',v) => k = k') l)

    fun selectCompiler opts =
        case selectOpt opts "compiler" of
            NONE => NullCompiler
          | SOME "mlton" => MLton
          | SOME "smlnj" => SMLNJ
          | SOME "polyml" => PolyML
          | SOME "moscowml" => MoscowML
          | SOME "mlkit" => MLKit
          | SOME s => raise Fail ("Unknown compiler `" ^ s ^ "'.")
     
    fun statFile f =
        let
            val s = OS.FileSys.fileSize f
        in
            f ^ " (" ^ Position.toString s ^ " bytes)"
        end handle _ => f ^ " (inaccessible or missing - compilation failed)"

    fun chDir f = OS.FileSys.chDir f

 end

