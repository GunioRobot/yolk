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

with Ada.Directories;
with Ada.Strings.Fixed;
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
      use Ada.Strings;

      URL               : constant String := AWS.Status.URI (Request);
      Resource          : String (1 .. (URL'Length - 4));
      Parent_Directory  : constant String := "static_content";

   begin

      Fixed.Move (Source  => URL,
                  Target  => Resource,
                  Drop    => Left);

      if not Exists (Parent_Directory & Resource) then
         return Yolk.Not_Found.Output (Request);
      end if;

      case Kind (Parent_Directory & Resource) is
         when Directory =>
            declare

               use AWS.Templates;
               use Yolk.Configuration;

               T : Translate_Set;

            begin

               T := AWS.Services.Directory.Browse
                 (Directory_Name => Parent_Directory & Resource,
                  Request        => Request);

               Insert (T, Assoc ("YOLK_VERSION", Yolk.Version));

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
            return Yolk.Not_Found.Output (Request);
      end case;

   end Generate;

end View.Dir;
