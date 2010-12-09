-------------------------------------------------------------------------------
--                                                                           --
--                                  Yolk                                     --
--                                                                           --
--                             my_configuration                              --
--                                                                           --
--                                  BODY                                     --
--                                                                           --
--                     Copyright (C) 2010, Thomas Løcke                      --
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

package body My_Configuration is

   -----------
   --  Get  --
   -----------

   function Get (Key : in Keys) return Boolean
   is
   begin

      return Ini.Get (Key);

   end Get;

   -----------
   --  Get  --
   -----------

   function Get (Key : in Keys) return Float
   is
   begin

      return Ini.Get (Key);

   end Get;

   -----------
   --  Get  --
   -----------

   function Get (Key : in Keys) return Integer
   is
   begin

      return Ini.Get (Key);

   end Get;

   -----------
   --  Get  --
   -----------

   function Get (Key : in Keys) return String
   is
   begin

      return Ini.Get (Key);

   end Get;

   -----------
   --  Get  --
   -----------

   function Get (Key : in Keys) return Unbounded_String
   is
   begin

      return Ini.Get (Key);

   end Get;

   ------------------
   --  Initialize  --
   ------------------

   procedure Initialize
   is
   begin

      Ini.Load_File (Config_File);

   end Initialize;

begin

   Initialize;

end My_Configuration;
