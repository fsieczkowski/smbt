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

structure Smbt =
struct
    val usage =
       "smbt " ^ Version.version ^ "\n" ^
       "Usage: smbt [options] [build-file] <target>\n" ^
       "Options:\n" ^
       "  -c, --continuous\tRe-run <target> on source modification.\n" ^
       "  -h, --help\t\tDisplay this usage information and exit.\n" ^
       "  -n, --noexec\t\tOutput commands without actually executing them.\n" ^
       "  -v, --version\t\tOutput version information and exit.\n" ^
       "  -V\t\t\tEnable verbose output.\n"

    fun printHdr (buildFile,tgt) =
        print ("smbt " ^ Version.version ^ "\n" ^
                " - Build file: " ^ buildFile ^ "\n" ^
                " - Target: " ^ tgt ^ "\n")

    fun runTarget (buildFile,tgt) =
        let
            val target = Plan.parseFile buildFile tgt

            (* For the remainder, we execute in the directory of the current build file *)
            val p = OS.FileSys.fullPath buildFile
            val d = OS.Path.dir p
            val _ = OS.FileSys.chDir d
        in
            if (!Config.continuous) then
                Plan.watch target (* Run in continuous mode. *)
            else
                Plan.execute target (* Execute immediately, then exit. *)
        end handle (Parser.ParseError s) => print ("Parse error: " ^ s ^ "\n")

    fun main (name,args) =
        let
            fun parseArgs ("--help"::_) = (print usage; OS.Process.success)
              | parseArgs ("-h"::_) = (print usage; OS.Process.success)
              | parseArgs ("-c"::t) = (Config.continuous := true; parseArgs t)
              | parseArgs ("--continuous"::t) = (Config.continuous := true; parseArgs t)
              | parseArgs ("-V"::t) = (Config.verbose := true; parseArgs t)
              | parseArgs ("-v"::_) = (print (Version.version ^ "\n"); OS.Process.success)
              | parseArgs ("--version"::_) = (print (Version.version ^ "\n"); OS.Process.success)
              | parseArgs ("-n"::t) = (Config.noExec := true; parseArgs t)
              | parseArgs ("-noexec"::t) = (Config.noExec := true; parseArgs t)
              | parseArgs [target] = 
                    (if String.isSuffix ".sm" target then raise Fail "Failed to specify a target!" else ();
                     printHdr ("build.sm", target);
                     runTarget ("build.sm", target);
                     OS.Process.success)
              | parseArgs [buildFile,target] = 
                    (printHdr (buildFile, target);
                     runTarget (buildFile, target);
                     OS.Process.success)
              | parseArgs _ = (print usage; OS.Process.success)
        in
            parseArgs args
        end
end


