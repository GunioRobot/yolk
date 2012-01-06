-------------------------------------------------------------------------------
--                                                                           --
--                                  Yolk                                     --
--                                                                           --
--                         Yolk.Cache.Discrete_Keys                          --
--                                                                           --
--                                  SPEC                                     --
--                                                                           --
--                   Copyright (C) 2010-2012, Thomas Løcke                   --
--                                                                           --
--  This library is free software;  you can redistribute it and/or modify    --
--  it under terms of the  GNU General Public License  as published by the   --
--  Free Software  Foundation;  either version 3,  or (at your  option) any  --
--  later version. This library is distributed in the hope that it will be   --
--  useful, but WITHOUT ANY WARRANTY;  without even the implied warranty of  --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     --
--                                                                           --
--  As a special exception under Section 7 of GPL version 3, you are         --
--  granted additional permissions described in the GCC Runtime Library      --
--  Exception, version 3.1, as published by the Free Software Foundation.    --
--                                                                           --
--  You should have received a copy of the GNU General Public License and    --
--  a copy of the GCC Runtime Library Exception along with this program;     --
--  see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
--  <http://www.gnu.org/licenses/>.                                          --
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
