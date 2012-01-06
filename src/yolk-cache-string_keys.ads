-------------------------------------------------------------------------------
--                                                                           --
--                                  Yolk                                     --
--                                                                           --
--                          Yolk.Cache.String_Keys                           --
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
--  No checking is done whether a returned element is valid. You have to check
--  for this manually.
--  In order for an element to be valid, it must:
--
--    1. have been added to the cache using the Write procedure
--    2. be younger than Max_Element_Age
--
--  WARNING!
--    As opposed to the Discrete_Keys cache, which size is known at all times,
--    the String_Keys cache carries the risk of growing until there are no
--    more resources available.
--    Either let the cache automatically clean itself (Cleanup_Size and
--    Cleanup_On_Write) or do it manually by calling Cleanup/Clear whenever you
--    decide it is necessary.

generic

   type Element_Type is private;
   Cleanup_Size : Positive := 200;
   --  Call the Cleanup procedure when the cache contains >= Cleanup_Size
   --  elements. The Cleanup procedure first tries to delete elements that are
   --  older than Max_Element_Age, and if that doesn't bring the amount of
   --  cached elements below Cleanup_Size, then elements are simply _randomly_
   --  deleted until the size is 1 lower than Cleanup_Size.
   --  Obviously it makes little sense to set this number below the value of
   --  Reserved_Capacity.

   Cleanup_On_Write : Boolean := True;
   --  Call the Cleanup procedure when Write is called and the size of the
   --  cache is >= Cleanup_Size. If Cleanup_On_Write is False, Cleanup is not
   --  called automatically, and you'll have to manage the size of the cache
   --  manually.

   Max_Element_Age : Duration := 3600.0;
   ---  Elements that are older than Max_Element_Age are considered invalid.

   Reserved_Capacity : Positive := 100;
   --  Set this as close as possible to the expected final size of the cache.
   --  Setting it too low will result in a performance-loss whenever the
   --  hashed map has to be resized because new key/value pairs are added.
   --  Setting it too high will result in wasted resources.
   --  This setting has NO bearing on the actual size of the cache. The cache
   --  will happily grow beyond the Reserved_Capacity.

package Yolk.Cache.String_Keys is

   procedure Cleanup;
   --  If the cache size is >= Cleanup_Size, then delete all elements where age
   --  is older than Max_Element_Age. If this doesn't bring the size of the
   --  cache down below Cleanup_Size, then elements are deleted _randomly_,
   --  until the size is Cleanup_Size - 1.
   --  Cleanup is called automatically if Cleanup_On_Write is True.

   procedure Clear;
   --  Clear the entire cache.

   procedure Clear
     (Key : in String);
   --  Remove the currently cached element associated with Key.

   function Is_Valid
     (Key : in String)
      return Boolean;
   --  Return True if the element associated with Key is younger than
   --  Max_Element_Age.

   function Length
     return Natural;
   --  Return the amount of elements currently in the cache. This counts both
   --  valid and invalid elements.

   procedure Read
     (Key      : in  String;
      Is_Valid : out Boolean;
      Value    : out Element_Type);
   --  Fetch the element associated with Key.
   --
   --  WARNING!
   --    Value will contain undefined garbage if Is_Valid is False.

   procedure Write
     (Key   : in String;
      Value : in Element_Type);
   --  Add Value to the cache, and associate it with Key.

end Yolk.Cache.String_Keys;
