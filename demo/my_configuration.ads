-------------------------------------------------------------------------------
--                                                                           --
--                                  Yolk                                     --
--                                                                           --
--                             my_configuration                              --
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

-------------------------------------------------------------------------------
--                                                                           --
--                            DEMO FILE                                      --
--                                                                           --
-------------------------------------------------------------------------------

--  This is a DEMO file. You can either move this to the my_configuration/
--  directory and change it according to you own needs, or you can provide your
--  own.
--
--  This package is currently only "with'ed" by other demo source files. It is
--  NOT required by Yolk in any way.

--  Application specific configuration.

with Ada.Strings.Unbounded;
with Yolk.Config_File_Parser;
with Yolk.Utilities;

package My_Configuration is

   use Yolk.Utilities;

   type Keys is (DB_Host,
                 DB_Name,
                 DB_User,
                 DB_Password,
                 Handler_Index,
                 SMTP,
                 Template_Index);
   --  The valid configuration keys.

   type Defaults_Array is array (Keys) of
     Ada.Strings.Unbounded.Unbounded_String;

   Default_Values : constant Defaults_Array :=
                      (DB_Host        => TUS ("freja.serverbox.dk"),
                       DB_Name        => TUS ("12boo"),
                       DB_User        => TUS ("thomas"),
                       DB_Password    => TUS ("respsltl16117994"),
                       Handler_Index  => TUS ("/|/[Ii]ndex"),
                       SMTP           => TUS ("freja.serverbox.dk"),
                       Template_Index => TUS ("templates/website/index.tmpl"));
   --  Default values for the configuration Keys. These values can be over-
   --  written by the configuration file given when instantiating the
   --  Config_File_Parser generic.

   package Config is new Yolk.Config_File_Parser
     (Keys => Keys,
      Defaults_Array => Defaults_Array,
      Default_Values => Default_Values,
      Config_File => "configuration/my_config.ini");

end My_Configuration;
