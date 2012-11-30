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

