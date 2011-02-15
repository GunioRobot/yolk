-------------------------------------------------------------------------------
--                                                                           --
--                                  Yolk                                     --
--                                                                           --
--                               view.index                                  --
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

-------------------------------------------------------------------------------
--                                                                           --
--                            DEMO FILE                                      --
--                                                                           --
-------------------------------------------------------------------------------

--  This is a DEMO file. You can either move this to the my_view/ directory and
--  change it according to you own needs, or you can provide your own.
--
--  This package is currently only "with'ed" by other demo source files. It is
--  NOT required by Yolk in any way.

--  with Yolk.Email.Composer;
--  with Yolk.Rotating_Log;
--  with GNATCOLL.Email;
--  with GNATCOLL.Email.Utils;
--  with GNATCOLL.VFS; use GNATCOLL.VFS;
--  with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
--  with Ada.Calendar;
--  with AWS.MIME;
--  with AWS.Utils;
--  with Ada.Text_IO;
with AWS.Services.Directory;
--  with My_Configuration;

package body View.Index is

   ---------------
   --  Generate --
   ---------------

   function Generate
     (Request : in AWS.Status.Data)
      return AWS.Response.Data
   is

      use AWS.Templates;
      --  use Yolk.Rotating_Log;

      --  package My renames My_Configuration;

      T : Translate_Set;

   begin

      --  Insert (T, Assoc ("HANDLER",
      --    String'(My.Config.Get (My.Handler_Index))));
      --  Insert (T, Assoc ("TEMPLATE",
      --    String'(My.Config.Get (My.Template_Index))));
      --  Insert (T, Assoc ("URI", AWS.Status.URI (Request)));

      T := AWS.Services.Directory.Browse
        (Directory_Name => "static_content",
         Request        => Request);

      return Build_Response
        (Status_Data   => Request,
         Template_File => "templates/system/aws_directory.tmpl",
         Translations  => T);

   end Generate;

end View.Index;
