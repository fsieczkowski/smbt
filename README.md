Smbt is a Standard ML build tool.  

The easiest way to obtain it is through [Smackage](http://github.com/standardml/smackage), using something like the following:

    smackage get smbt
    smackage make smbt
    smackage make smbt install

You will need MLton, and parcom (the latter of which smackage should install automatically).

## Using Smbt

    smbt [options] [build-file] <target>
       -c, --continuous          Re-run <target> on source modification.
       -h, --help               Display this usage information and exit.
       -i, --interactive        Enter an interactive prompt (if possible).
       -n, --noexec             Output commands without actually executing them.
       -v, --version            Output version information and exit.
       -V                       Enable verbose output.

### Continuous Mode

Continuous mode sets smbt to monitor a set of files for changes, and re-run a given target whenever a modification is detected.

### Interactive Mode

Interactive mode is designed to be able to drop you into a REPL into which your project has already been loaded.  This will only work on targets using SML/NJ, PolyML or MoscowML as the compiler (SML# support is incomplete and MLKit support may follow...)

#### Example

    > smbt -i smlnj
    smbt 0.2.4
    - Build file: build.sm
    - Target: smlnj
    - Running pre-hooks
    - Invoking SML/NJ (interactive)
    Standard ML of New Jersey v110.73 [built: Sun May 15 21:34:53 2011]
    [scanning .smbt/smbt-run.cm]
    [scanning /Users/gdpe/.smackage/lib/cmlib/v1.2.0/cmlib.cm]
    [scanning /Users/gdpe/.smackage/lib/cmlib/v1.2.0/(cmlib.cm):basis.cm]
    [library $SMLNJ-BASIS/basis.cm is stable]
    - 

## Build Files

The name `build.sm` is the default.  Smbt will attempt to load this file if one is not specified.

### Building a simple program

A simple build file for a program could look like this:

    target mlton
        sources
            foo.mlb
            bar.sml
        end

        option compiler = mlton
        option output = bin/myprog
    end

This would be invoked using `smbt mlton`, and would use MLton to generate a binary `bin/myprog`.

### Using libraries

We can add package dependencies to the previous example using the `pkg` keyword:

    target mlton
        pkg cmlib v1
        
        [...]
    end

This will use smackage to attempt to find a package `cmlib` at version `v1`, with a file `build.sm` in the root.  By default, the target that will be selected will be the same as the enclosing target (i.e., `mlton` in this case).  One can also specify the target explicitly (e.g., `pkg cmlib v1.1.0 mytarget`).

### Writing build files for libraries

Targets in build files are hierarchical, in that every target may contain zero or more other targets.  A given target is resolved by traversing the build file from the root to the selected target in-order.  For example:

    target a
        sources
            a.sml
        end
  
        target b
            sources
                b.sml
            end
        end

        target c
            sources
                c.sml
            end
        end
    end

Selecting the target `b` in this example would be equivalent to writing the following:

    target b
        sources
            a.sml
            b.sml
        end
    end

Package dependencies, sources, and FFI dependencies (explained below) are concatenated in order.

### FFI usage

NOTE: Currently FFI directives are only interpreted usefully when using `option compiler = mlton` or `option compiler = smlsharp`.

For the [readline library bindings](https://github.com/standardml/readline) we have a C file that needs to be included on the MLton command line for *any* client application that wishes to use it.  This is one of the things smbt is designed to avoid.

The readline library might have a build.sm file like this:

    target mlton
        sources
            readline.sml
        end

        ffi
            smlreadline.c
            lnkopt -lreadline
        end
    end

A client application would use this without needing to know anything about the FFI, like this:

    target foobar
       pkg readline v1 mlton

       sources
           foobar.sml
       end

       option compiler = mlton
       option output = myprog1
   end

## Worthwhile compiler options to know:

Valid in any target:
    option compiler = {mlton,smlnj,polyml,moscowml,mlkit,smlsharp}
    option output = path/to/output/binary

In targets using `option compiler = mlton`:

    option mlton = /path/to/mlton
    
In targets using `option compiler = smlnj`:

    option smlnj = /path/to/smlnj
    option exportFn = Some.SML.function (which function should be exported as 'main')
    option heapimg = /path/to/heapimg (where to place the SML/NJ heap image, defaults to .smbt/.heapimg)

, and in interactive mode,

    option smlnj = /path/to/smlnj
    option rlwrap = true (Whether use rlwrap or not, interactive mode only)

In targets using `option compiler = polyml`:

    option polyml = /path/to/polyml
    option exportFn = Some.SML.function (which function should be exported as 'main')
    option objectFile = path/to/foo (will result in path/to/foo.o being generated)
    option cc = /path/to/cc (which C compiler to use, defaults to 'cc')

, and in interactive mode,

    option polyml = /path/to/polyml

In targets using `option compiler = smlsharp`

    option smlsharp = /path/to/smlsharp
    option cc = /path/to/c-compiler
    option entry = some_file.smi (The entry point interface file)
    option smlflags = "-some -flag s" (Flags to path smlsharp compiler)

, and in interactive mode,

    option smlsharp = /path/to/smlsharp
    option rlwrap = true (Whether use rlwrap or not)

In targets using `option compiler = moscowml`

    option mosmlc = /path/to/mosmlc
    option smlflags = "-some -flag s" (Flags to path smlsharp compiler)

, and in interactive mode,

    option mosml = /path/to/mosml
    option rlwrap = true (Whether use rlwrap or not)

## Pre-hooks and post-hooks:

Targets can execute commands in their working directory, which is defined to be whichever one contains the `.sm` file.  You can define two newline-separated lists of commands to execute before and after executing the target:

    target foo
        pre hooks
            ml-yacc whatever.grm
        end

        post hooks
            echo "Hello, world"
        end
    end

The working directory will be set back to the correct place between hooks in different packages, so it is reasonably safe to do things like `cd ..` in a hook, in the sense that the effect of this will remain local to that hook.

Obviously, we can write commands like `clean` this way, but we recommend avoiding the temptation.  Use a Makefile, really.

   target clean
       pre hooks
            rm -f bin/foo
       end
   end

## A final note

Smbt is *not* designed to replace CM or MLB files.  It's fine to just put those into a `sources` section and use them directly!  Rather, smbt is mostly designed to provide a consistent way of building dependencies, such that we need not worry about what needs to be executed in order to build some library, particularly where arcane FFI options are concerned.  It also should permit you to manage multi-compiler and multi-platform builds cleanly (we hope...).

There are other options that should really be documented further.  Have a look in `doc/` for a few example `.sm` files.

Happy building,
Gian & Filip
