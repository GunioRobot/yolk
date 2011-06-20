-------------------------------------------------------------------------------
--                                                                           --
--                                  Yolk                                     --
--                                                                           --
--                             Yolk.Syndication                              --
--                                                                           --
--                                  SPEC                                     --
--                                                                           --
--                   Copyright (C) 2010-2011, Thomas Løcke                   --
--                                                                           --
--  Yolk is free software;  you can  redistribute it  and/or modify it under --
--  terms of the  GNU General Public License as published  by the Free Soft- --
--  ware  Foundation;  either version 2,  or (at your option) any later ver- --
--  sion.  Yolk is distributed in the hope that it will be useful, but WITH- --
--  OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
--  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
--  for  more details.  You should have  received  a copy of the GNU General --
--  Public License  distributed with Yolk.  If not, write  to  the  Free     --
--  Software Foundation,  51  Franklin  Street,  Fifth  Floor, Boston,       --
--  MA 02110 - 1301, USA.                                                    --
--                                                                           --
-------------------------------------------------------------------------------

with Ada.Characters.Latin_1;

package Yolk.Syndication is

   Q : Character renames Ada.Characters.Latin_1.Quotation;

   None     : constant String := "";
   PI       : constant String := "<?xml version="
     & Q & "1.0" & Q & " encoding=" & Q & "utf-8" & Q & "?>"
     & Ada.Characters.Latin_1.LF;
   XHTMLNS  : constant String := "http://www.w3.org/1999/xhtml";
   XMLNS    : constant String := "http://www.w3.org/2005/Atom";
   DIVNS    : constant String := "xmlns=" & Q & XHTMLNS & Q;

end Yolk.Syndication;
