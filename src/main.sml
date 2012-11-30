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
       "  Options:\n" ^
       "\t-h, --help\t\tDisplay this usage information and exit.\n"

    fun main (name,args) =
        let
            val res = case args of
                        ("--help"::_) => (print usage; OS.Process.success)
                      | ("-h"::_) => (print usage; OS.Process.success)
                      | [target] => (print ("Build file: build.sm Target: " ^ target ^ "\n"); OS.Process.success)
                      | [buildFile,target] => (print ("Build file: " ^ buildFile ^ " Target: " ^ target ^ "\n"); OS.Process.success)
                      | _ => (print usage; OS.Process.success)
        in
            res
        end
end

val () = OS.Process.exit(Smbt.main(CommandLine.name (), CommandLine.arguments ()))

