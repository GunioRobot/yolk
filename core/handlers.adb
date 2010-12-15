-------------------------------------------------------------------------------
--                                                                           --
--                                  Yolk                                     --
--                                                                           --
--                                handlers                                   --
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

with Configuration;
with Static_Content;
with Unknown_Content;

package body Handlers is

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

      use Configuration;

   begin

      --------------------------
      --  Static Dispatchers  --
      --------------------------

      --  These dispatchers handle static content, such as CSS, JS and images.
      --  NOTE:
      --    Order matters. The first handler that matches a resource handles
      --    the request.
      AWS.Services.Dispatchers.URI.Register_Regexp
        (Dispatcher  => Resource_Handlers,
         URI         => Config.Get (Handler_CSS),
         Action      => Static_Content.CSS'Access);

      AWS.Services.Dispatchers.URI.Register_Regexp
        (Dispatcher  => Resource_Handlers,
         URI         => Config.Get (Handler_GIF),
         Action      => Static_Content.GIF'Access);

      AWS.Services.Dispatchers.URI.Register_Regexp
        (Dispatcher  => Resource_Handlers,
         URI         => Config.Get (Handler_HTML),
         Action      => Static_Content.HTML'Access);

      AWS.Services.Dispatchers.URI.Register_Regexp
        (Dispatcher  => Resource_Handlers,
         URI         => Config.Get (Handler_ICO),
         Action      => Static_Content.ICO'Access);

      AWS.Services.Dispatchers.URI.Register_Regexp
        (Dispatcher  => Resource_Handlers,
         URI         => Config.Get (Handler_JPG),
         Action      => Static_Content.JPG'Access);

      AWS.Services.Dispatchers.URI.Register_Regexp
        (Dispatcher  => Resource_Handlers,
         URI         => Config.Get (Handler_JS),
         Action      => Static_Content.JS'Access);

      AWS.Services.Dispatchers.URI.Register_Regexp
        (Dispatcher  => Resource_Handlers,
         URI         => Config.Get (Handler_PNG),
         Action      => Static_Content.PNG'Access);

      AWS.Services.Dispatchers.URI.Register_Regexp
        (Dispatcher  => Resource_Handlers,
         URI         => Config.Get (Handler_XML),
         Action      => Static_Content.XML'Access);

      AWS.Services.Dispatchers.URI.Register_Regexp
        (Dispatcher  => Resource_Handlers,
         URI         => Config.Get (Handler_XSL),
         Action      => Static_Content.XSL'Access);

      -----------------------------------------
      --  Unknown Resource (404) Dispatcher  --
      -----------------------------------------

      --  This dispatcher _must_ be placed last, or else it will overrule
      --  all the other dispatchers. This dispatcher is called if the requested
      --  resource doesn't match any of the other dispatchers.
      --  It returns a generic 404 HTML page. The template for this 404 can be
      --  found in templates/system.
      AWS.Services.Dispatchers.URI.Register_Regexp
        (Dispatcher  => Resource_Handlers,
         URI         => Config.Get (Handler_Unknown),
         Action      => Unknown_Content.Generate'Access);

   end Initialize;

begin

   Initialize;

end Handlers;
