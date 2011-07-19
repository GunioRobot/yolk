-------------------------------------------------------------------------------
--                                                                           --
--                                  Yolk                                     --
--                                                                           --
--                              Yolk.Whoops                                  --
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

with AWS.Messages;
with AWS.MIME;
with AWS.Status;
with AWS.Templates;
with Yolk.Configuration;
with Yolk.Rotating_Log;

package body Yolk.Whoops is

   ----------------------------------
   -- Unexpected_Exception_Handler --
   ----------------------------------

   procedure Unexpected_Exception_Handler
     (E      : in     Ada.Exceptions.Exception_Occurrence;
      Log    : in out AWS.Log.Object;
      Error  : in     AWS.Exceptions.Data;
      Answer : in out AWS.Response.Data)
   is

      pragma Unreferenced (Log);

      use Ada.Exceptions;
      use AWS.Status;
      use AWS.Templates;
      use Yolk.Configuration;
      use Yolk.Rotating_Log;

      S : Data renames Error.Request;
      T : Translate_Set;

   begin

      if Error.Fatal then
         Trace (Handle     => Rotating_Log.Error,
                Log_String => "");
         Trace (Handle     => Rotating_Log.Error,
                Log_String => "-- FATAL Unexpected Exception Begin --");
         Trace (Handle     => Yolk.Rotating_Log.Error,
                Log_String => Exception_Information (E));
         Trace (Handle     => Rotating_Log.Error,
                Log_String => "We have a serious problem. One of the AWS" &
                "slots have died.");
         Trace (Handle     => Rotating_Log.Error,
                Log_String => "Slot " & Positive'Image (Error.Slot) &
                " is dead now.");
         Trace (Handle     => Rotating_Log.Error,
                Log_String => "-- FATAL Unexpected Exception End --");
      else
         Trace (Handle     => Rotating_Log.Error,
                Log_String => "");
         Trace (Handle     => Rotating_Log.Error,
                Log_String => "-- Unexpected Exception Begin --");
         Trace (Handle     => Yolk.Rotating_Log.Error,
                Log_String => Exception_Information (E));
         Trace (Handle     => Rotating_Log.Error,
                Log_String => " Client: " & Peername (S));
         Trace (Handle     => Rotating_Log.Error,
                Log_String => " Method: " & Method (S));
         Trace (Handle     => Rotating_Log.Error,
                Log_String => " URI: " & URI (S));
         Trace (Handle     => Rotating_Log.Error,
                Log_String => " HTTP version: " & HTTP_Version (S));
         Trace (Handle     => Rotating_Log.Error,
                Log_String => "-- Unexpected Exception End --");
      end if;

      Insert (T, Assoc ("VERSION", AWS.Version));
      Insert (T, Assoc ("DOMAIN", String'(Config.Get (Server_Name))));

      Answer := AWS.Response.Build
        (Content_Type  => AWS.MIME.Text_HTML,
         Message_Body  => Parse
           (Filename     => Config.Get (System_Templates_Path) &  "/500.tmpl",
            Translations => T,
            Cached       => True),
         Status_Code   => AWS.Messages.S500);

   end Unexpected_Exception_Handler;

end Yolk.Whoops;
