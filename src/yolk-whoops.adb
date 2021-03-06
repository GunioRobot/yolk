-------------------------------------------------------------------------------
--                                                                           --
--                                  Yolk                                     --
--                                                                           --
--                              Yolk.Whoops                                  --
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

with AWS.Messages;
with AWS.MIME;
with AWS.Status;
with AWS.Templates;
with Yolk.Configuration;
with Yolk.Log;

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
      use Yolk.Log;

      S : Data renames Error.Request;
      T : Translate_Set;
   begin
      if Error.Fatal then
         Trace (Handle  => Yolk.Log.Error,
                Message => "-- FATAL Unexpected Exception Begin --");
         Trace (Handle  => Yolk.Log.Error,
                Message => Exception_Information (E));
         Trace (Handle  => Yolk.Log.Error,
                Message => "We have a serious problem. One of the AWS" &
                "slots have died.");
         Trace (Handle  => Yolk.Log.Error,
                Message => "Slot " & Positive'Image (Error.Slot) &
                " is dead now.");
         Trace (Handle  => Yolk.Log.Error,
                Message => "-- FATAL Unexpected Exception End --");
      else
         Trace (Handle  => Yolk.Log.Error,
                Message => "-- Unexpected Exception Begin --");
         Trace (Handle  => Yolk.Log.Error,
                Message => Exception_Information (E));
         Trace (Handle  => Yolk.Log.Error,
                Message => " Client: " & Peername (S));
         Trace (Handle  => Yolk.Log.Error,
                Message => " Method: " & Method (S));
         Trace (Handle  => Yolk.Log.Error,
                Message => " URI: " & URI (S));
         Trace (Handle  => Yolk.Log.Error,
                Message => " HTTP version: " & HTTP_Version (S));
         Trace (Handle  => Yolk.Log.Error,
                Message => "-- Unexpected Exception End --");
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
