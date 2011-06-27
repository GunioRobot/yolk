-------------------------------------------------------------------------------
--                                                                           --
--                                  Yolk                                     --
--                                                                           --
--                          Yolk.Cache.String_Keys                           --
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
--  No checking is done whether a returned element is valid. You have to check
--  for this manually.
--  In order for an element to be valid, it must have:
--
--    1. been added to the cache using the Write procedure
--    2. be younger than Max_Element_Age
--
--  WARNING!
--    As opposed to the Discrete_Keys cache, which size is known at all times,
--    the String_Keys cache carries the risk of growing until there are no
--    more resources available.

generic

   type Element_Type is private;
   Cleanup_Interval : Positive := 100;
   --  Call the Cleanup procedure on every Cleanup_Interval call to Write, ie.
   --  if Cleanup_Interval is 100, then for every 100 calls to Write, Cleanup
   --  is called once.
   --  This only happens if Cleanup_On_Write is True. Else you'll have to do
   --  this manually.
   Cleanup_On_Write : Boolean := True;
   --  Call the Cleanup procedure when Write is called, according to the
   --  Cleanup_Interval value.
   Max_Element_Age : Duration := 3600.0;
   --  Elements that are older than Max_Element_Age are still returned when
   --  using the Read function/procedure, but they are not considered valid.
   Reserved_Capacity : Positive := 100;
   --  Set this as close as possible to the expected final size of the cache.
   --  Setting it too low will result in a performance-loss whenever the
   --  hashed map has to be resized because new key/value pairs are added.
   --  Setting it too high will result in wasted resources.
   --  This setting has NO bearing on the actual size of the cache. The cache
   --  will happily grow beyond the Reserved_Capacity.

package Yolk.Cache.String_Keys is

   procedure Clear;
   --  Invalidate entire cache. Clear it out.

   procedure Invalidate
     (Key : in String);
   --  Remove the currently cached element associated with Key.

   function Is_Valid
     (Key : in String)
      return Boolean;
   --  Return True if the element associated with Key has been added to the
   --  cache using the Write procedure and it's age is less than
   --  Max_Element_Age.

   function Read
     (Key : in String)
      return Element_Type;
   --  Return the element associated with Key.
   --  Note that the only way to check if this element is valid is by calling
   --  Is_Valid prior to Read. This is NOT 100% thread safe as another thread
   --  theoretically could've tampered with the element after the Is_Valid call
   --  and before the Read call.
   --  Only use this if you're 100% sure that an element stays valid after it
   --  has been initially added to the cache.
   --
   --  WARNING!
   --    Read will return undefined garbage if Key has not been written to the
   --    cache by Write, or if an earlier Write has been Invalidated.

   procedure Read
     (Key      : in  String;
      Is_Valid : out Boolean;
      Value    : out Element_Type);
   --  Get the element associated with Key.
   --  This Read operation is thread safe, as opposed to the Read function.
   --  This also means that the Read procedure theoretically is slower than the
   --  Read function, and tests seems to confirm this. It is though by a very
   --  small margin, and only noticeable at high concurrency levels.
   --  If in doubt, use this version of Read, and only consider switching to
   --  the Read function if performance is a problem.
   --
   --  WARNING!
   --    Value will contain undefined garbage if Is_Valid is False.

   procedure Write
     (Key   : in String;
      Value : in Element_Type);
   --  Add Value to the cache, and associate it with Key.

end Yolk.Cache.String_Keys;
