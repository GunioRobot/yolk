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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Config_File_Parser;

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

   function Get (Key : in Keys) return Boolean;
   function Get (Key : in Keys) return Float;
   function Get (Key : in Keys) return Integer;
   function Get (Key : in Keys) return String;
   function Get (Key : in Keys) return Unbounded_String;
   --  Return the value of Key as the appropriate type.

private

   Config_File : constant String := "configuration/my_config.ini";
   --  The path to the config file.

   type Defaults is array (Keys) of Unbounded_String;
   function TUS (S : String) return Unbounded_String renames
     To_Unbounded_String;
   Defaults_Array : constant Defaults :=
                      (DB_Host        => TUS (""),
                       DB_Name        => TUS (""),
                       DB_User        => TUS (""),
                       DB_Password    => TUS (""),
                       Handler_Index  => TUS ("/|/[Ii]ndex"),
                       SMTP           => TUS (""),
                       Template_Index => TUS ("templates/website/index.tmpl"));
   --  Default values for the configuration Keys.

   package Ini is new Config_File_Parser (Keys => Keys);

   procedure Initialize;
   --  Load and parse the Config_File configuration file.

end My_Configuration;
