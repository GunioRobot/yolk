###############################################################################
#                                                                             #
#                                   Yolk                                      #
#                                                                             #
#                                Make File                                    #
#                                                                             #
#                    Copyright (C) 2010-2012, Thomas Løcke                    #
#                                                                             #
#  This is free software;  you can redistribute it  and/or modify it          #
#  under terms of the  GNU General Public License as published  by the        #
#  Free Software  Foundation;  either version 3,  or (at your option) any     #
#  later version.  This software is distributed in the hope  that it will     #
#  be useful, but WITHOUT ANY WARRANTY;  without even the implied warranty    #
#  of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU        #
#  General Public License for  more details.                                  #
#  You should have  received  a copy of the GNU General  Public  License      #
#  distributed  with  this  software;   see  file COPYING3.  If not, go       #
#  to http://www.gnu.org/licenses for a complete copy of the license.         #
#                                                                             #
###############################################################################

include makefile.setup

all:
	gnatmake -P yolk_build

debug:
	BUILDTYPE=Debug gnatmake -P yolk_build

clean:
	gnatclean -P yolk_build
	BUILDTYPE=Debug gnatclean -P yolk_build

distclean: clean
	rm -rf $(prefix)/yolk
	rm -rf $(prefix)/include/yolk
	rm -f $(prefix)/lib/gnat/yolk.gpr

install:
	mkdir -p $(prefix)/lib/gnat
	mkdir -p $(prefix)/yolk
	mkdir -p $(prefix)/include/yolk
	cp -pr library/* $(prefix)/yolk
	cp -pr src/*.ad[sb] $(prefix)/include/yolk
	cp -pr yolk.gpr $(prefix)/lib/gnat
