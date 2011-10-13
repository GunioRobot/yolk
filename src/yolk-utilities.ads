-------------------------------------------------------------------------------
--                                                                           --
--                                  Yolk                                     --
--                                                                           --
--                             Yolk.Utilities                                --
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

with Ada.Strings.Unbounded;

package Yolk.Utilities is

   use Ada.Strings.Unbounded;

   function TS
     (US : in Unbounded_String)
      return String
      renames To_String;

   function TUS
     (S : in String)
      return Unbounded_String
      renames To_Unbounded_String;

   function Is_Empty
     (S : in String)
      return Boolean;
   --  Trims S (left and right) and return True if empty.

   function Is_Empty
     (US : in Unbounded_String)
      return Boolean;
   --  Trims US (left and right) and return True if empty.

end Yolk.Utilities;
