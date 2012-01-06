-------------------------------------------------------------------------------
--                                                                           --
--                                  Yolk                                     --
--                                                                           --
--                              Yolk.Handlers                                --
--                                                                           --
--                                  BODY                                     --
--                                                                           --
--                   Copyright (C) 2010-2012, Thomas Løcke                   --
--                                                                           --
--  This library is free software;  you can redistribute it and/or modify    --
--  it under terms of the  GNU General Public License  as published by the   --
--  Free Software  Foundation;  either version 3,  or (at your  option) any  --
--  later version. This library is distributed in the hope that it will be   --
--  useful, but WITHOUT ANY WARRANTY;  without even the implied warranty of  --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     --
--                                                                           --
--  As a special exception under Section 7 of GPL version 3, you are         --
--  granted additional permissions described in the GCC Runtime Library      --
--  Exception, version 3.1, as published by the Free Software Foundation.    --
--                                                                           --
--  You should have received a copy of the GNU General Public License and    --
--  a copy of the GCC Runtime Library Exception along with this program;     --
--  see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
--  <http://www.gnu.org/licenses/>.                                          --
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
