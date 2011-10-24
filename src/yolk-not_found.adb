-------------------------------------------------------------------------------
--                                                                           --
--                                  Yolk                                     --
--                                                                           --
--                             Yolk.Not_Found                                --
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

with AWS;
with AWS.Messages;
with AWS.MIME;
with AWS.Templates;
with Yolk.Configuration;

package body Yolk.Not_Found is

   --------------
   --  Output  --
   --------------

   function Generate
     (Request : in AWS.Status.Data)
      return AWS.Response.Data
   is
      use AWS.Templates;
      use Yolk.Configuration;

      Content  : AWS.Response.Data;
      T        : Translate_Set;
   begin
      Insert (T, Assoc ("RESOURCE", AWS.Status.URI (Request)));
      Insert (T, Assoc ("VERSION", AWS.Version));
      Insert (T, Assoc ("DOMAIN", String'(Config.Get (Server_Name))));

      Content := AWS.Response.Build
        (Content_Type  => AWS.MIME.Text_HTML,
         Message_Body  => Parse
           (Filename     => Config.Get (System_Templates_Path) &  "/404.tmpl",
            Translations => T,
            Cached       => True),
         Status_Code   => AWS.Messages.S404);

      return Content;
   end Generate;

end Yolk.Not_Found;
