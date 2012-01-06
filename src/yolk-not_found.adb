-------------------------------------------------------------------------------
--                                                                           --
--                                  Yolk                                     --
--                                                                           --
--                             Yolk.Not_Found                                --
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
