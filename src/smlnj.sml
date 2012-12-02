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

structure SMLNJCompiler :> COMPILER =
struct
    open CompilerUtil 

    val name = "SML/NJ"

    fun compile' (srcs,ffisrcs,lnkopts,cflags,hdr,opts) output =
        let
            val _ = print " - Invoking SML/NJ\n"

            val smlnj = case selectOpt opts "smlnj" of
                SOME t => t
              | NONE => "sml"

            val dir = tempdir ()
            val cmFile = dir ^ "/smbt-build.cm"
            val njGoFile = dir ^ "/smbt-smlnj-main.sml"

            val heapImg' = case selectOpt opts "heapimg" of
                  SOME t => t
                | NONE => ".heapimg"

            val heapImg = 
                    OS.Path.mkAbsolute {path=heapImg', 
                        relativeTo = OS.FileSys.fullPath (OS.Path.dir output)}

            val exportFn = case selectOpt opts "exportFn" of 
                SOME t => t
              | NONE => raise Fail "SML/NJ Output requires 'exportFn' directive.\n"

            val fp = TextIO.openOut cmFile
            val _ = TextIO.output (fp,
                        "Group is\n   " ^
                        String.concatWith "\n   " (map absolutePath srcs) ^ "\n")
            val _ = TextIO.closeOut fp


            val go = TextIO.openOut njGoFile
            val _ = TextIO.output (go,
                        "CM.make \"" ^ absolutePath cmFile ^ "\";\n" ^
                        "SMLofNJ.exportFn (\""^ heapImg ^"\", "^ exportFn ^");\n");
            val _ = TextIO.closeOut go


            val cmd = smlnj ^ " " ^ njGoFile

            val _ = exec cmd
            val _ = print (" - Heap Image: " ^ heapImg ^ "\n")

            (* TODO: This is not portable.  We should be more clever about this here. *)
            val bin = TextIO.openOut output
            val _ = TextIO.output (bin,
            "#!/bin/sh\n" ^
            "exec \"" ^ smlnj ^ "\" @SMLcmdname=$0 @SMLload=\""^ heapImg ^"\" \"$@\"\n")
            val _ = TextIO.closeOut bin

            val _ = exec ("chmod a+x \""^ output ^"\"")

            val _ = print (" - Output: " ^ statFile output ^ "\n")
        in
            ()
        end

    fun compile (c as (srcs,ffisrcs,lnkopts,cflags,hdr,opts)) =
        case selectOpt opts "output" of
                SOME output => compile' c output
              | NONE => raise Fail ("Compiler invoked with no output.\n")


    fun interactive (srcs,ffisrcs,lnkopts,cflags,hdr,opts) =
        let
            val _ = print " - Invoking SML/NJ (interactive)\n"

            val smlnj = case selectOpt opts "smlnj" of
                SOME t => t
              | NONE => "sml"

            val rlwrap = case selectOpt opts "rlwrap" of
                SOME "true" => "rlwrap "
              | _ => ""

            val dir = tempdir ()
            val cmFile = dir ^ "/smbt-run.cm"

            val fp = TextIO.openOut cmFile
            val _ = TextIO.output (fp,
                        "Group is\n   " ^
                        String.concatWith "\n   " (map absolutePath srcs) ^ "\n")
            val _ = TextIO.closeOut fp

            val cmd = rlwrap ^ smlnj ^ " " ^ cmFile 

            val _ = exec cmd

            val _ = print (" - Interactive session finished.\n")
        in
            ()
        end
end
