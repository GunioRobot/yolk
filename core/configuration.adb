-------------------------------------------------------------------------------
--                                                                           --
--                                  Yolk                                     --
--                                                                           --
--                              configuration                                --
--                                                                           --
--                                  BODY                                     --
--                                                                           --
--                     Copyright (C) 2010, Thomas Løcke                      --
--                                                                           --
--  Yolk is free software;  you can  redistribute it  and/or modify it under --
--  terms of the  GNU General Public License as published  by the Free Soft- --
--  ware  Foundation;  either version 2,  or (at your option) any later ver- --
--  sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
--  OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
--  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
--  for  more details.  You should have  received  a copy of the GNU General --
--  Public License  distributed with Yolk.  If not, write  to  the  Free     --
--  Software Foundation,  51  Franklin  Street,  Fifth  Floor, Boston,       --
--  MA 02110 - 1301, USA.                                                    --
--                                                                           --
-------------------------------------------------------------------------------

package body Configuration is

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
      Value : Unbounded_String;
      Empty : constant Unbounded_String := To_Unbounded_String ("");
   begin

      Ini.Load_File (Config_File);

      for Key in Keys loop
         Value := Get (Key);
         if Value = Empty then
            Ini.Set (Key   => Key,
                     Value => Defaults_Array (Key));
         end if;
      end loop;
      --  Load default values, when necessary.

   end Initialize;

begin

   Initialize;

end Configuration;
