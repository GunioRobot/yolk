-------------------------------------------------------------------------------
--                                                                           --
--                                  Yolk                                     --
--                                                                           --
--                               my_handlers                                 --
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

-------------------------------------------------------------------------------
--                                                                           --
--                            DEMO FILE                                      --
--                                                                           --
-------------------------------------------------------------------------------

--  This is a DEMO file. You can either move this to the my_handlers/ directory
--  and change it according to you own needs, or you can provide your own.
--
--  This package is required by Yolk. It is "with'ed" by the Yolk.Handlers
--  package. You must provide a My_Handlers package, preferably placed in the
--  my_handlers/ directory.

--  Application specific resource handlers.

with AWS.Services.Dispatchers.URI;

package My_Handlers is

   procedure Set
     (RH : out AWS.Services.Dispatchers.URI.Handler);
   --  Setup content dispatchers for the server. Basically this initializes the
   --  RH object declared in yolk_server.adb. The handlers registered here are
   --  specific to this application. Generic content handlers, such as 404
   --  errors, images and similar, are registered in the core Yolk.Handlers
   --  package.

end My_Handlers;
