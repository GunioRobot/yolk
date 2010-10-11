-------------------------------------------------------------------------------
--                                                                           --
--                                  Yolk                                     --
--                                                                           --
--                              static_content                               --
--                                                                           --
--                                  BODY                                     --
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

with Ada.Directories;   use Ada.Directories;
with AWS.MIME;
with Configuration;     use Configuration;
with Not_Found;

package body Static_Content is

   -----------
   --  CSS  --
   -----------

   function CSS (Request : in AWS.Status.Data) return AWS.Response.Data
   is

      CSS_File : constant String := Get (CSS_Path) & AWS.Status.URI (Request);

   begin

      if not Exists (CSS_File) then
         return Not_Found.Output (Request);
      end if;

      return AWS.Response.File (Content_Type => AWS.MIME.Text_CSS,
                                Filename     => CSS_File);

   end CSS;

   -----------
   --  GIF  --
   -----------

   function GIF (Request : in AWS.Status.Data) return AWS.Response.Data
   is

      GIF_File : constant String := Get (GIF_Path) & AWS.Status.URI (Request);

   begin

      if not Exists (GIF_File) then
         return Not_Found.Output (Request);
      end if;

      return AWS.Response.File (Content_Type  => AWS.MIME.Image_Gif,
                                Filename      => GIF_File);

   end GIF;

   ------------
   --  HTML  --
   ------------

   function HTML (Request : in AWS.Status.Data) return AWS.Response.Data
   is

      HTML_File : constant String :=
                    Get (HTML_Path) & AWS.Status.URI (Request);

   begin

      if not Exists (HTML_File) then
         return Not_Found.Output (Request);
      end if;

      return AWS.Response.File (Content_Type  => AWS.MIME.Text_HTML,
                                Filename      => HTML_File);

   end HTML;

   -----------
   --  ICO  --
   -----------

   function ICO (Request : in AWS.Status.Data) return AWS.Response.Data
   is

      ICO_File : constant String := Get (ICO_Path) & AWS.Status.URI (Request);

   begin

      if not Exists (ICO_File) then
         return Not_Found.Output (Request);
      end if;

      return AWS.Response.File (Content_Type  => AWS.MIME.Image_Icon,
                                Filename      => ICO_File);

   end ICO;

   -----------
   --  JPG  --
   -----------

   function JPG (Request : in AWS.Status.Data) return AWS.Response.Data
   is

      JPG_File : constant String := Get (JPG_Path) & AWS.Status.URI (Request);

   begin

      if not Exists (JPG_File) then
         return Not_Found.Output (Request);
      end if;

      return AWS.Response.File (Content_Type  => AWS.MIME.Image_Jpeg,
                                Filename      => JPG_File);

   end JPG;

   ----------
   --  JS  --
   ----------

   function JS (Request : in AWS.Status.Data) return AWS.Response.Data
   is

      JS_File : constant String := Get (JS_Path) & AWS.Status.URI (Request);

   begin

      if not Exists (JS_File) then
         return Not_Found.Output (Request);
      end if;

      return AWS.Response.File (Content_Type  => AWS.MIME.Text_Javascript,
                                Filename      => JS_File);

   end JS;

   -----------
   --  PNG  --
   -----------

   function PNG (Request : in AWS.Status.Data) return AWS.Response.Data
   is

      PNG_File : constant String := Get (PNG_Path) & AWS.Status.URI (Request);

   begin

      if not Exists (PNG_File) then
         return Not_Found.Output (Request);
      end if;

      return AWS.Response.File (Content_Type  => AWS.MIME.Image_Png,
                                Filename      => PNG_File);

   end PNG;

   -----------
   --  XML  --
   -----------

   function XML (Request : in AWS.Status.Data) return AWS.Response.Data
   is

      XML_File : constant String := Get (XML_Path) & AWS.Status.URI (Request);

   begin

      if not Exists (XML_File) then
         return Not_Found.Output (Request);
      end if;

      return AWS.Response.File (Content_Type  => AWS.MIME.Application_XML,
                                Filename      => XML_File);

   end XML;

   -----------
   --  XSL  --
   -----------

   function XSL (Request : in AWS.Status.Data) return AWS.Response.Data
   is

      XSL_File : constant String := Get (XSL_Path) & AWS.Status.URI (Request);

   begin

      if not Exists (XSL_File) then
         return Not_Found.Output (Request);
      end if;

      return AWS.Response.File (Content_Type  => AWS.MIME.Application_XML,
                                Filename      => XSL_File);

   end XSL;

end Static_Content;
