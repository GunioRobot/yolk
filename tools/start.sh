#!/bin/sh

###############################################################################
##                                                                           ##
##                                  Yolk                                     ##
##                                                                           ##
##                                start.sh                                   ##
##                                                                           ##
##                     Copyright (C) 2010, Thomas Løcke                      ##
##                                                                           ##
##  Yolk is free software;  you can  redistribute it  and/or modify it under ##
##  terms of the  GNU General Public License as published  by the Free Soft- ##
##  ware  Foundation;  either version 2,  or (at your option) any later ver- ##
##  sion.  GNAT is distributed in the hope that it will be useful, but WITH- ##
##  OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY ##
##  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License ##
##  for  more details.  You should have  received  a copy of the GNU General ##
##  Public License  distributed with Yolk.  If not, write  to  the  Free     ##
##  Software Foundation,  51  Franklin  Street,  Fifth  Floor, Boston,       ##
##  MA 02110 - 1301, USA.                                                    ##
##                                                                           ##
###############################################################################

## This script renames all the [filename].example files found in the Yolk
## project. It must be executed from the directory where the yolk.gpr file
## is located,ie. like this:
##
##	$ tools/start.sh
##
## .example files that have already been renamed by other means are ignored.

# Check for --help argument
if [ "$1" = "--help" ]
then
    echo "";
    echo "When executed, this script removes the .example part of all"
    echo "[filename].example files found in the Yolk project folder."
    echo "Options:"
    echo "    --help"
    echo "        Output this text."
    echo "    --force"
    echo "        Overwrite already renamed .example files. Use with care!"
    echo ""
    
    exit 1;
fi

# Check that we're in the right directory
if [ ! -f yolk.gpr -a ! -f tools/start.sh ]
then
    echo "Please execute this script from the directory where yolk.gpr is placed.";
    exit 1;
fi

# It appears that we're in the right place, so lets move on
echo "Fixing all the .example files";

# Set some base variables
FOUND=0;
COPIED=0;

for i in `find . -name *.example`
do
    ORG=$i;
    NEW=${i%.example}
    if [ -f $NEW ]
    then
        if [ "$1" = "--force" ]
        then
            echo "    File $NEW already exists. Overwriting it with $ORG."
            cp $ORG $NEW
            let COPIED=$COPIED+1;
        else
            echo "    File $NEW already exists. No copy done."
        fi
    else
        echo "    Copying $ORG to $NEW"
        cp $ORG $NEW
        let COPIED=$COPIED+1;
    fi
    
    let FOUND=$FOUND+1;
done

if [ $FOUND -lt 1 ]
then
    echo "No files found, no files copied.";
else
    echo "$FOUND .example files found, $COPIED copied.";
fi


