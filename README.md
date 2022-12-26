pmut
----

_A Linux multi-tool for Propeller 2 development, based off "PNut" for Windows._

This repository contains "pmut", which is a Linux port of the open source ["PNut"](https://github.com/parallaxinc/P2_PNut_Public) project for the Parallax Propeller 2 microcontroller. Whereas PNut is a Windows IDE, pmut is a command-line program without the built-in editor, mainly for Linux users. It can be used alongside any Linux souce code editor to replicate all the functionality available to PNut users working on Windows.

How to Use
----------
Typing `pmut` or `pmut -h` will display the usage information:

    Usage: pmut [-d,--doc] [-l,--list] [-h,--help] [-i,--info] command [topfile]
    where command is one of "build","load","flash", "debug", "watch" or "set"
    Use "pmut set" to modify/list the project settings (in ".pmut_project").
    Name of source code "topfile" is taken from project setting (source.top),
    if not included in the command line.

[Screencast](https://youtu.be/4CpEbOnRTmQ) demonstration showing `pmut` usage.

`pmut` is a multitool and can be used to compile your P2 (Propeller 2) code, load/run the binary (from P2 RAM), flash it (to P2 NV storage), and start a debugging session from your P2 code to the host's graphical debugger windows. These can be done  incrementally or in tandem using the basic commands:

1. `pmut build` - compile P2 application binary
2. `pmut load` - compile binary and download to P2 RAM
3. `pmut flash` - compile, download, and flash binary to P2 flash memory
4. `pmut debug` - compile, download, and start debugging session

There is also `pmut set`, which is used to manage project settings. Typing this command without options shows usage and lists the current settings:

    Set project options with "pmut set <setting> <value>"
    Settings are as listed below and can be abbreviated to
    just "device", "top", or "libs"

    Current project settings:
    comm.device = /dev/ttyUSB0
    source.top = spin2/vga_text_demo
    source.libs = ~/Development/propeller2_lib

These project settings are stored in the current directory in a file named `.pmut_project`. The contents are plain text in INI-file format, and it can also be edited by hand - the `set` command adds convenience and format checking. Generally, each P2 project directory will have its own settings file which gets used when you run `pmut` from that directory. Note that the `source.top` setting points to your project's "main" .spin2 file, and this can be overridden by providing a path to a .spin2 file in the command line.

There is also `pmut watch`. This is included to support rapid iteration workflows. Running this command starts the same as `pmut debug`, but the source code used by your project is monitored for any changes. Thus, saving a code change in your editor will quickly & automatically rebuild, download, reset, and restart debugging on the P2 with your new changes included! Very handy when rapid prototyping or debugging your P2 application.

How To Build
------------

First step is to clone the [p2com](https://github.com/BrianHoldsworth/p2com) project and build it on your target platform (i.e. Linux). This is the core compiler from Parallax/PNut, and is written in (fast!) x86 assembly.

To build `pmut` requires the [Free Pascal Compiler (fpc)](https://www.freepascal.org/) and [Lazarus](https://www.lazarus-ide.org/). 

1. Install fpc and Lazarus. If you are installing for the first time, I _highly recommend_ using [fpcupdeluxe](https://github.com/LongDirtyAnimAlf/fpcupdeluxe/releases). Installing this binary and running it let's you easily manage the installation of both fpc and Lazarus, including cross-compilers you may need. It's a more stable/reliable way to manage these dev tools.
2. You _will need to install_ the i386 cross-compiler too, since you are (almost certainly) building on a 64-bit OS.
3. Also, `pmut` needs to link to Gtk2 libraries on your system, which must also include 32-bit versions. Hopefully, this is not an issue on your Linux distro, but could be on some bare-bones Linux OS instances.
4. Install and build `p2com` is in a sibling directory (e.g. `~build/p2com/` and `~/build/pmut/`).
5. Open Lazarus, then browse and open the `pmut/pmut.lpi` project file. Press Shift+F9 to build.

A successful build will yield a `pmut` executable in the project directory. Copy the executable to some directory in your command line path, and you are good-to-go.

Related Projects
----------------

[p2com](https://github.com/BrianHoldsworth/p2com) - Port of core P2 compiler to flat-assembler (fasm).
