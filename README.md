This is a simple game written in Z80 assembly code for the ZX Spectrum.
It is not that complicated and was mainly used to test my interrupt-driven mouse driver.

Required dependancies:

The build currently needs to be done on Linux.
the build script uses:

* z80asm  (In most Linux software repositories, try apt install z80asm / yum install z80asm)
* The latest version of SpecHDDFileEditor found at https://github.com/spodula/SpecHDDFileEditor/releases (Just download the JAR file to the folder, rename it to HDDFileEditor.jar)

Building:

just CD to the downloaded folder and run compile.sh to generate a DSK and TAP files.

Should run on any 48/128k Spectrum or emulator.


