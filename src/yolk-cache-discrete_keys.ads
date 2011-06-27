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
--  No checking is done whether a returned element is valid. You have to check
--  for this manually.
--  In order for an element to be valid, it must have:
--
--    1. been added to the cache using the Write procedure
--    2. be younger than Max_Element_Age

generic

   type Key_Type is (<>);
   type Element_Type is private;
   Max_Element_Age : Duration := 3600.0;
   --  Elements that are older than Max_Element_Age are still returned when
   --  using the Read function/procedure, but they are not considered valid.

package Yolk.Cache.Discrete_Keys is

   procedure Clear;
   --  Invalidate entire cache. Clear it out.

   procedure Invalidate
     (Key : in Key_Type);
   --  Remove the currently cached element associated with Key.

   function Is_Valid
     (Key : in Key_Type)
      return Boolean;
   --  Return True if the element associated with Key has been added to the
   --  cache using the Write procedure and it's age is less than
   --  Max_Element_Age.

   function Read
     (Key : in Key_Type)
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
     (Key      : in  Key_Type;
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
   --
   --
   --  WARNING!
   --    Value will contain undefined garbage if Is_Valid is False.

   procedure Write
     (Key   : in Key_Type;
      Value : in Element_Type);
   --  Add Value to the cache, and associate it with Key.

end Yolk.Cache.Discrete_Keys;
