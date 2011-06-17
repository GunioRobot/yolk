-------------------------------------------------------------------------------
--                                                                           --
--                                  Yolk                                     --
--                                                                           --
--                                View.Dir                                   --
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

with Ada.Directories;
with AWS.Services.Directory;
with Yolk.Configuration;
with Yolk.Not_Found;

package body View.Dir is

   ---------------
   --  Generate --
   ---------------

   function Generate
     (Request : in AWS.Status.Data)
      return AWS.Response.Data
   is

      use Ada.Directories;
      use Yolk.Configuration;

      URL               : constant String := AWS.Status.URI (Request);
      Resource          : constant String (1 .. (URL'Length - 4))
        := URL (5 .. (URL'Length));
      --  Get rid of the /dir part of the URL
      Parent_Directory  : constant String := Config.Get (WWW_Root);

   begin

      if not Exists (Parent_Directory & Resource) then
         return Not_Found.Generate (Request);
      end if;

      case Kind (Parent_Directory & Resource) is
         when Directory =>
            declare

               use AWS.Templates;

               T : Translate_Set;

            begin

               T := AWS.Services.Directory.Browse
                 (Directory_Name => Parent_Directory & Resource,
                  Request        => Request);

               Insert (T, Assoc ("YOLK_VERSION", Version));

               return Build_Response
                 (Status_Data   => Request,
                  Template_File =>
                    Config.Get (System_Templates_Path) & "/directory.tmpl",
                  Translations  => T);

            end;
         when Ordinary_File =>
            return AWS.Response.File
              (Content_Type  => AWS.Status.Content_Type (Request),
               Filename      => Parent_Directory & Resource);
         when others =>
            return Not_Found.Generate (Request);
      end case;

   end Generate;

end View.Dir;
