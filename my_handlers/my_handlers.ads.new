-------------------------------------------------------------------------------
--                                                                           --
--                                  Yolk                                     --
--                                                                           --
--                               my_handlers                                 --
--                                                                           --
--                                  SPEC                                     --
--                                                                           --
--                     Copyright (C) 2010, Thomas Løcke                      --
--                                                                           --
--  Yolk is free software;  you can  redistribute it  and/or modify it under --
--  terms of the  GNU General Public License as published  by the Free Soft- --
--  ware  Foundation;  either version 2,  or (at your option) any later ver- --
--  sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
--  OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
--  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
--  for  more details.  You should have  received  a copy of the GNU General --
--  Public License  distributed with Yolk.  If not, write  to  the  Free     --
--  Software Foundation,  51  Franklin  Street,  Fifth  Floor, Boston,       --
--  MA 02110 - 1301, USA.                                                    --
--                                                                           --
-------------------------------------------------------------------------------

with AWS.Services.Dispatchers.URI;

package My_Handlers is

   function Get return AWS.Services.Dispatchers.URI.Handler;
   --  Return the initialized Resource_Handlers object.

private

   Resource_Handlers : AWS.Services.Dispatchers.URI.Handler;

   procedure Initialize;
   --  Setup content dispatchers for the server. Basically this initializes the
   --  Resource_Handlers object declared above. The handlers registered here
   --  should be those specific to this application. Generic content handlers,
   --  such as 404 errors, images and similar, are registered in the
   --  core/handlers.adb file.

end My_Handlers;
