###############################################################################
#                                                                             #
#                                   Yolk                                      #
#                                                                             #
#                                Make File                                    #
#                                                                             #
#                    Copyright (C) 2010-2011, Thomas Løcke                    #
#                                                                             #
#   Yolk is free software;  you can  redistribute it  and/or modify it under  #
#   terms of the  GNU General Public License as published  by the Free Soft-  #
#   ware  Foundation;  either version 2,  or (at your option) any later ver-  #
#   sion.  Yolk is distributed in the hope that it will be useful, but WITH-  #
#   OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY  #
#   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License  #
#   for  more details.  You should have  received  a copy of the GNU General  #
#   Public License  distributed with Yolk.  If not, write  to  the  Free      #
#   Software Foundation,  51  Franklin  Street,  Fifth  Floor, Boston,        #
#   MA 02110 - 1301, USA.                                                     #
#                                                                             #
###############################################################################

include makefile.setup

all:
	gnatmake -P yolk_build
	mkdir yolklib
	cp -p build/* yolklib
	ar rc yolklib/libyolk.a yolklib/*.o
	ranlib yolklib/libyolk.a
	rm -f yolklib/*.o

clean:
	gnatclean -P yolk_build
	rm -rf yolklib

distclean: clean
	rm -rf $(prefix)/yolk
	rm -rf $(prefix)/include/yolk
	rm -f $(prefix)/lib/gnat/yolk.gpr

install:
	mkdir -p $(prefix)/lib/gnat
	mkdir -p $(prefix)/yolk
	mkdir -p $(prefix)/include/yolk
	cp -pr yolklib/* $(prefix)/yolk
	cp -pr src/core/*.ad[sb] $(prefix)/include/yolk
	cp -pr src/extended/*.ad[sb] $(prefix)/include/yolk
	cp -pr yolk.gpr $(prefix)/lib/gnat
