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
with Yolk.Syndication.Writer;

package View.Syndication is

   use Yolk.Syndication.Writer;

   Feed : Atom_Feed := New_Atom_Feed (Base_URI    => "base",
                                      Language    => "lang",
                                      Max_Age     => 10.0,
                                      Min_Entries => 5,
                                      Max_Entries => 8);
   --  Declare a new Atom_Feed object.

   function Generate
     (Request : in AWS.Status.Data)
      return AWS.Response.Data;
   --  Generate the content for the /syndication resource.

end View.Syndication;
