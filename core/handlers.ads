-------------------------------------------------------------------------------
--                                                                           --
--                                  Yolk                                     --
--                                                                           --
--                                handlers                                   --
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

--  In this package we define the content/resource handlers for static content.
--  Content not handled in this package should be added to the
--  my_handlers/my_handlers.ad[sb] package.

with AWS.Services.Dispatchers.URI;

package Handlers is

   procedure Set (RH : in out AWS.Services.Dispatchers.URI.Handler);
   --  The Handlers package define a set of default handlers for static content
   --  such as HTML, images and ICO files.
   --  The My_Handlers package define application specific handlers, so it is
   --  in this package that such handlers should be placed.
   --  See my_handlers/my_handlers.ad[sb] for more information.

end Handlers;
