-------------------------------------------------------------------------------
--                                                                           --
--                                  Yolk                                     --
--                                                                           --
--                             my_configuration                              --
--                                                                           --
--                                  SPEC                                     --
--                                                                           --
--                     Copyright (C) 2010, Thomas Løcke                      --
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

--  Define your application specific keys in the Keys type and set their
--  default values in the Defaults_Array array.

with Ada.Strings.Unbounded;
with Config_File_Parser;
with Utilities; use Utilities;

package My_Configuration is

   type Keys is (DB_Host,
                 DB_Name,
                 DB_User,
                 DB_Password,
                 Handler_Index,
                 SMTP,
                 Template_Index);
   --  The valid configuration keys. Only keys found in the Keys type are valid
   --  as  keys in the Config_File configuration file.
   --  These configuration keys are for application specific configuration.

   type Defaults_Array is array (Keys) of
     Ada.Strings.Unbounded.Unbounded_String;

   Defaults : Defaults_Array :=
                (DB_Host        => TUS (""),
                 DB_Name        => TUS (""),
                 DB_User        => TUS (""),
                 DB_Password    => TUS (""),
                 Handler_Index  => TUS ("/|/[Ii]ndex"),
                 SMTP           => TUS (""),
                 Template_Index => TUS ("templates/website/index.tmpl"));
   --  Default values for the configuration Keys.

   package Config is new Config_File_Parser
     (Keys => Keys,
      Defaults_Array => Defaults_Array,
      Defaults => Defaults,
      Config_File => "configuration/my_config.ini");

end My_Configuration;
