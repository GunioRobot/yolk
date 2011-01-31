-------------------------------------------------------------------------------
--                                                                           --
--                                  Yolk                                     --
--                                                                           --
--                                utilities                                  --
--                                                                           --
--                                  BODY                                     --
--                                                                           --
--                   Copyright (C) 2010-2011, Thomas L�cke                   --
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

package body Utilities is

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty
     (S : in String)
      return Boolean
   is

      use Ada.Strings;

   begin

      if Fixed.Trim (S, Both) = "" then
         return True;
      else
         return False;
      end if;

   end Is_Empty;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty
     (US : in Ada.Strings.Unbounded.Unbounded_String)
      return Boolean
   is

      use Ada.Strings;
      use Ada.Strings.Unbounded;

   begin

      if Unbounded.Trim (US, Both) = "" then
         return True;
      else
         return False;
      end if;

   end Is_Empty;

end Utilities;
