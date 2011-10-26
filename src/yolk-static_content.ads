-------------------------------------------------------------------------------
--                                                                           --
--                                  Yolk                                     --
--                                                                           --
--                           Yolk.Static_Content                             --
--                                                                           --
--                                  SPEC                                     --
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

--  Static content such as images, HTML and XML files are handled here. The
--  paths to where the server is supposed to look for the content is defined
--  by the WWW_Root configuration parameter.
--  Compressed content is saved in the Compressed_Cache_Directory.

with AWS.Messages;
with AWS.Response;
with AWS.Status;

package Yolk.Static_Content is

   function Non_Compressable
     (Request : in AWS.Status.Data)
      return AWS.Response.Data;
   --  Return non-compressed content.

   procedure Static_Content_Cache_Setup
     (Log_To_Info_Trace : in Boolean := True;
      No_Cache          : in Boolean := False;
      No_Store          : in Boolean := False;
      No_Transform      : in Boolean := False;
      Max_Age           : in AWS.Messages.Delta_Seconds := 86400;
      S_Max_Age         : in AWS.Messages.Delta_Seconds := AWS.Messages.Unset;
      Public            : in Boolean := False;
      Must_Revalidate   : in Boolean := True;
      Proxy_Revalidate  : in Boolean := False);
   --  Set the cache options for the request and delete and re-create the
   --  Compressed_Cache_Directory. Should preferably be called before any AWS
   --  HTTP servers are started.
   --  This is a threadsafe operation, and it can be repeated as often as
   --  need be.

   function Compressable
     (Request : in AWS.Status.Data)
      return AWS.Response.Data;
   --  Return compressed content. This function saves a pre-compressed version
   --  of the requested resource for future use. This compressed file times out
   --  according to the Compressed_Max_Age configuration setting.

end Yolk.Static_Content;
