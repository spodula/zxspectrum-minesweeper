#!/bin/bash
###########################################################
# Build script for Specccy Minesweeper.
# GDS 10th October 2023
###########################################################
BASE=.
SRC=$BASE/src
OUT=$BASE/out

clear

function compile( ) {
   echo Compiling $2
   z80asm --verbose -v --input $SRC/$2 --output $OUT/$1.bin --list=$OUT/$1.list --label=$OUT/$1.lbl
}
# build the code
compile "msweeper"   "msweeper.asm"
compile "mouse"      "mouse.asm"

#build the disk image
java -jar $BASE/HDDFileEditor.jar script=$BASE/assets/diskbuild.script

#Start fuse
FDD="--plus3disk msweeper.dsk"
JS="--kempston --joystick-1 /dev/input/js1 --joystick-1-output 2"

#fuse --machine plus3e --kempston-mouse $NOL $FDD $JS
