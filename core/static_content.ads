-------------------------------------------------------------------------------
--                                                                           --
--                                  Yolk                                     --
--                                                                           --
--                             static_content                                --
--                                                                           --
--                                  SPEC                                     --
--                                                                           --
--                     Copyright (C) 2010, Thomas Løcke                      --
--                                                                           --
--  Yolk is free software;  you can  redistribute it  and/or modify it under --
--  terms of the  GNU General Public License as published  by the Free Soft- --
--  ware  Foundation;  either version 2,  or (at your option) any later ver- --
--  sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
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
--  in the configuration/config.ini file.

with AWS.Response;
with AWS.Status;

package Static_Content is

   --  Load various static content. The regex'es and dispatchers for
   --  these files are defined in the Handlers package.

   function CSS (Request : in AWS.Status.Data) return AWS.Response.Data;
   function GIF (Request : in AWS.Status.Data) return AWS.Response.Data;
   function HTML (Request : in AWS.Status.Data) return AWS.Response.Data;
   function ICO (Request : in AWS.Status.Data) return AWS.Response.Data;
   function JPG (Request : in AWS.Status.Data) return AWS.Response.Data;
   function JS (Request : in AWS.Status.Data) return AWS.Response.Data;
   function PNG (Request : in AWS.Status.Data) return AWS.Response.Data;
   function XML (Request : in AWS.Status.Data) return AWS.Response.Data;

end Static_Content;
