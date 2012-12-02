structure PolyMain =
struct
    fun polyMain () = OS.Process.exit(Smbt.main(CommandLine.name (), CommandLine.arguments ()));
end
