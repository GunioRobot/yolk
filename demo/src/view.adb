-------------------------------------------------------------------------------
--                                                                           --
--                                  Yolk                                     --
--                                                                           --
--                                  View                                     --
--                                                                           --
--                                  BODY                                     --
--                                                                           --
--                   Copyright (C) 2010-2012, Thomas Løcke                   --
--                                                                           --
--  This is free software;  you can redistribute it and/or modify it         --
--  under terms of the  GNU General Public License  as published by the      --
--  Free Software  Foundation;  either version 3,  or (at your  option) any  --
--  later version. This library is distributed in the hope that it will be   --
--  useful, but WITHOUT ANY WARRANTY;  without even the implied warranty of  --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     --
--  You should have received a copy of the GNU General Public License and    --
--  a copy of the GCC Runtime Library Exception along with this program;     --
--  see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
--  <http://www.gnu.org/licenses/>.                                          --
--                                                                           --
-------------------------------------------------------------------------------

with AWS.Messages;

package body View is

   ----------------------
   --  Build_Response  --
   ----------------------

   function Build_Response
     (Status_Data   : in AWS.Status.Data;
      Template_File  : in String;
      Translations   : in AWS.Templates.Translate_Set;
      MIME_Type      : in String := Text_HTML)
      return AWS.Response.Data
   is
      use AWS.Templates;
   begin
      return Build_Response (Status_Data => Status_Data,
                             Content     => Parse
                               (Filename     => Template_File,
                                Translations => Translations,
                                Cached       => True),
                             MIME_Type   => MIME_Type);
   end Build_Response;

   ----------------------
   --  Build_Response  --
   ----------------------

   function Build_Response
     (Status_Data : in AWS.Status.Data;
      Content     : in String;
      MIME_Type   : in String := Text_HTML)
      return AWS.Response.Data
   is
      use AWS.Messages;
      use AWS.Response;
      use AWS.Status;
      use AWS.Templates;

      Encoding : Content_Encoding := Identity;
      --  Default to no encoding.
   begin
      if Is_Supported (Status_Data, GZip) then
         Encoding := GZip;
         --  GZip is supported by the client.
      end if;

      return Build (Content_Type  => MIME_Type,
                    Message_Body  => Content,
                    Encoding      => Encoding,
                    Cache_Control => No_Cache);
   end Build_Response;

end View;
