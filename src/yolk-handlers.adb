-------------------------------------------------------------------------------
--                                                                           --
--                                  Yolk                                     --
--                                                                           --
--                              Yolk.Handlers                                --
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
with Yolk.Configuration;
with Yolk.Static_Content;

package body Yolk.Handlers is

   -----------
   --  Set  --
   -----------

   procedure Set
     (RH : out AWS.Services.Dispatchers.URI.Handler)
   is
      use AWS.Dispatchers.Callback;
      use Yolk.Configuration;

      package SC renames Yolk.Static_Content;
   begin
      AWS.Services.Dispatchers.URI.Register_Regexp
        (Dispatcher  => RH,
         URI         => Config.Get (Handler_CSS),
         Action      => Create (Callback => SC.Compressable'Access));

      AWS.Services.Dispatchers.URI.Register_Regexp
        (Dispatcher  => RH,
         URI         => Config.Get (Handler_GIF),
         Action      => Create (Callback => SC.Non_Compressable'Access));

      AWS.Services.Dispatchers.URI.Register_Regexp
        (Dispatcher  => RH,
         URI         => Config.Get (Handler_HTML),
         Action      => Create (Callback => SC.Compressable'Access));

      AWS.Services.Dispatchers.URI.Register_Regexp
        (Dispatcher  => RH,
         URI         => Config.Get (Handler_ICO),
         Action      => Create (Callback => SC.Non_Compressable'Access));

      AWS.Services.Dispatchers.URI.Register_Regexp
        (Dispatcher  => RH,
         URI         => Config.Get (Handler_JPG),
         Action      => Create (Callback => SC.Non_Compressable'Access));

      AWS.Services.Dispatchers.URI.Register_Regexp
        (Dispatcher  => RH,
         URI         => Config.Get (Handler_JS),
         Action      => Create (Callback => SC.Compressable'Access));

      AWS.Services.Dispatchers.URI.Register_Regexp
        (Dispatcher  => RH,
         URI         => Config.Get (Handler_PNG),
         Action      => Create (Callback => SC.Non_Compressable'Access));

      AWS.Services.Dispatchers.URI.Register_Regexp
        (Dispatcher  => RH,
         URI         => Config.Get (Handler_SVG),
         Action      => Create (Callback => SC.Compressable'Access));

      AWS.Services.Dispatchers.URI.Register_Regexp
        (Dispatcher  => RH,
         URI         => Config.Get (Handler_XML),
         Action      => Create (Callback => SC.Compressable'Access));

      AWS.Services.Dispatchers.URI.Register_Regexp
        (Dispatcher  => RH,
         URI         => Config.Get (Handler_XSL),
         Action      => Create (Callback => SC.Compressable'Access));
   end Set;

end Yolk.Handlers;
