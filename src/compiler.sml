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
    (** The compiler interface's internal representation **)
    type t

    val name : string

    val empty : t

    val addSources : t -> string list -> t
    val addFFISources : t -> string list -> t
    val addLinkOpts : t -> string list -> t
    val addCFlags : t -> string -> t
    val setExportHeader : t -> string -> t
    val setOutput : t -> string -> t
    val setOption : t -> (string * string) -> t

    val generateFiles : t -> t
    val invoke : t -> unit
end

(** Implements common functionality that may be shared easily across compilers and tools. **)
structure ToolOpt =
struct
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
end

