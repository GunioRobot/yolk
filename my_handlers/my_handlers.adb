-------------------------------------------------------------------------------
--                                                                           --
--                                  Yolk                                     --
--                                                                           --
--                               my_handlers                                 --
--                                                                           --
--                                  BODY                                     --
--                                                                           --
--                     Copyright (C) 2010, Thomas Løcke                      --
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

with My_Configuration;  use My_Configuration;
with View.Index;

package body My_Handlers is

   -----------
   --  Get  --
   -----------

   function Get return AWS.Services.Dispatchers.URI.Handler
   is
   begin

      return Resource_Handlers;

   end Get;

   ------------------
   --  Initialize  --
   ------------------

   procedure Initialize
   is
   begin

      -----------------------------------
      --  General Content Dispatchers  --
      -----------------------------------

      --  These dispatchers handle the "page" content.
      --  NOTE:
      --    Order matters. The first handler that matches a resource handles
      --    the request.
      AWS.Services.Dispatchers.URI.Register_Regexp
        (Dispatcher => Resource_Handlers,
         URI        => Get (Handler_Index),
         Action     => View.Index.Generate'Access);

   end Initialize;

begin

   Initialize;

end My_Handlers;
