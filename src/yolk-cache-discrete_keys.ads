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

--  A simple and fairly dumb cache.
--  No checking is done whether the fetched element is valid. You have to check
--  for this manually.
--  In order for an element to be valid, it must:
--
--    1. have been added to the cache using the Write procedure
--    2. be younger than Max_Element_Age

generic

   type Key_Type is (<>);
   type Element_Type is private;
   Max_Element_Age : Duration := 3600.0;
   --  Elements that are older than Max_Element_Age are considered invalid.

package Yolk.Cache.Discrete_Keys is

   procedure Clear;
   --  Clear the entire cache.

   procedure Clear
     (Key : in Key_Type);
   --  Remove the currently cached element associated with Key.

   function Is_Valid
     (Key : in Key_Type)
      return Boolean;
   --  Return True if the element associated with Key is younger than
   --  Max_Element_Age.

   procedure Read
     (Key      : in  Key_Type;
      Is_Valid : out Boolean;
      Value    : out Element_Type);
   --  Fetch the element associated with Key.
   --
   --  WARNING!
   --    Value will contain undefined garbage if Is_Valid is False.

   procedure Write
     (Key   : in Key_Type;
      Value : in Element_Type);
   --  Add Value to the cache, and associate it with Key.

end Yolk.Cache.Discrete_Keys;
