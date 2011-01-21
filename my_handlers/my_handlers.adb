-------------------------------------------------------------------------------
--                                                                           --
--                                  Yolk                                     --
--                                                                           --
--                               my_handlers                                 --
--                                                                           --
--                                  BODY                                     --
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

with AWS.Dispatchers.Callback;
with My_Configuration;
with View.Index;
with Unknown_Content;

package body My_Handlers is

   -----------
   --  Set  --
   -----------

   procedure Set
     (RH : out AWS.Services.Dispatchers.URI.Handler)
   is

      use AWS.Dispatchers.Callback;

      package My renames My_Configuration;

   begin

      -----------------------------------------
      --  Unknown Resource (404) Dispatcher  --
      -----------------------------------------

      --  This dispatcher is called if the requested resource doesn't match any
      --  of the other dispatchers.
      --  It returns a generic 404 HTML page. The template for this 404 can be
      --  found in templates/system.
      AWS.Services.Dispatchers.URI.Register_Default_Callback
        (Dispatcher => RH,
         Action     => Create (Callback => Unknown_Content.Generate'Access));

      -----------------------------------
      --  General Content Dispatchers  --
      -----------------------------------

      --  These dispatchers handle the "page" content.
      --  NOTE:
      --    Order matters. The first handler that matches a resource handles
      --    the request, hence this Set procedure is called  at the beginning
      --    of the Handlers.Set procedure in the Handlers package.

      AWS.Services.Dispatchers.URI.Register_Regexp
        (Dispatcher => RH,
         URI        => My.Config.Get (My.Handler_Index),
         Action     => Create (Callback => View.Index.Generate'Access));

   end Set;

end My_Handlers;
