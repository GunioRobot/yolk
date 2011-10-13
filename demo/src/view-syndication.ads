-------------------------------------------------------------------------------
--                                                                           --
--                                  Yolk                                     --
--                                                                           --
--                               View.Syndication                                  --
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

--  The syndication resource.

with AWS.Response;
with AWS.Status;
with Yolk.Syndication;

package View.Syndication is

   use Yolk.Syndication;

   --  Declare a new Atom_Feed object.
   Feed              : Atom_Feed := New_Atom_Feed (Max_Entries => 10,
                                                   Min_Entries => 5);

   procedure Add_Entry_To_Feed;
   --  Add a new entry to Feed.

   function Generate
     (Request : in AWS.Status.Data)
      return AWS.Response.Data;
   --  Generate the content for the /syndication resource.

end View.Syndication;
