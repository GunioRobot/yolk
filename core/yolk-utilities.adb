-------------------------------------------------------------------------------
--                                                                           --
--                                  Yolk                                     --
--                                                                           --
--                                Utilities                                  --
--                                                                           --
--                                  BODY                                     --
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

with Ada.Strings.Fixed;

package body Yolk.Utilities is

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty
     (S : in String)
      return Boolean
   is

      use Ada.Strings;

   begin

      return Fixed.Trim (S, Both) = "";

   end Is_Empty;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty
     (US : in Unbounded_String)
      return Boolean
   is

      use Ada.Strings;

   begin

      return Trim (US, Both) = "";

   end Is_Empty;

end Yolk.Utilities;
