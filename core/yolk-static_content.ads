-------------------------------------------------------------------------------
--                                                                           --
--                                  Yolk                                     --
--                                                                           --
--                             Static Content                                --
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
--  in the configuration/config.ini file.
--  If enabled in the config.ini file, files with text content will be
--  pre-compressed and saved in the Compressed_Cache_Directory.

with AWS.Response;
with AWS.Status;
with AWS.Utils;

package Yolk.Static_Content is

   Lock : AWS.Utils.Semaphore;
   --  This lock is used when the .gz files are created for the first time. We
   --  don't want more than one task messing around with the Compress_And_Cache
   --  procedure at the same time.

   function Binary_File
     (Request : in AWS.Status.Data)
      return AWS.Response.Data;
   --  Load various binary static content. The regex'es and dispatchers for
   --  these files are defined in the Yolk.Handlers package.
   --  NOTE:
   --    Content handled by Binary_File will _not_ be pre-compressed.

   function Text_File
     (Request : in AWS.Status.Data)
      return AWS.Response.Data;
   --  Load various text static content and compress/cache files. The regex'es
   --  and dispatchers for these files are defined in the Yolk.Handlers
   --  package.
   --  NOTE:
   --    Content handled by Text_file will _always_ be pre-compressed.

end Yolk.Static_Content;
