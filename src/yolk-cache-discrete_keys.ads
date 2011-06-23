-------------------------------------------------------------------------------
--                                                                           --
--                                  Yolk                                     --
--                                                                           --
--                         Yolk.Cache.Discrete_Keys                          --
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

--  A simple and fairly dumb cache. Elements are accessed using the Key_Type
--  keys. No checking is done whether a returned element is valid. You have to
--  check for this manually.
--  A typical usage flow could be:
--
--    if Is_Valid (Key) then
--       Get_Element (Key)
--    else
--       Value := Something;
--       Set_Element (Key, Value);
--    end if;
--
--  Get_Element will _always_ return whatever is saved at Key position, even if
--  the element is older that Max_Element_Age. Only by using Is_Valid can

generic

   type Key_Type is (<>);
   type Element_Type is private;
   Max_Element_Age : Duration := 3600.0;

package Yolk.Cache.Discrete_Keys is

   procedure Invalidate
     (Key : in Key_Type);

   function Is_Valid
     (Key : in Key_Type)
      return Boolean;

   procedure Read
     (Key   : in  Key_Type;
      Valid : out Boolean;
      Value : out Element_Type);

   procedure Write
     (Key   : in Key_Type;
      Value : in Element_Type);

end Yolk.Cache.Discrete_Keys;
