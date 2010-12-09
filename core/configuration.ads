-------------------------------------------------------------------------------
--                                                                           --
--                                  Yolk                                     --
--                                                                           --
--                             configuration                                 --
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

--  In this package we define Yolk specific configuration keys. If more keys
--  are needed for an application, then they should be defined in the
--  my_configuration/my_configuration.ads file.
--  The path to the default config.ini file is configuration/config.ini. This
--  can be changed by altering the constant Confil_File.
--  Default values for the keys are set in the constant Defaults_Array.

with Ada.Strings.Unbounded;
private with Config_File_Parser;

package Configuration is

   type Keys is (CSS_Path,
                 GIF_Path,
                 GNATCOLL_Traces_Ini_File,
                 Handler_CSS,
                 Handler_GIF,
                 Handler_HTML,
                 Handler_ICO,
                 Handler_JPG,
                 Handler_JS,
                 Handler_PNG,
                 Handler_XML,
                 Handler_XSL,
                 Handler_Unknown,
                 HTML_Path,
                 ICO_Path,
                 Immediate_Flush,
                 JPG_Path,
                 JS_Path,
                 Log_File_Path,
                 Max_Slot_Count,
                 Max_Logged_Characters,
                 PNG_Path,
                 Session_Data_File,
                 System_Templates_Path,
                 XML_Path,
                 XSL_Path);
   --  The valid configuration keys. Only keys found in the Keys type are valid
   --  as keys in the Config_File configuration file.
   --  These configuration keys are essential for other core parts of this
   --  software, so you should not change them, unless you know what you're
   --  doing.
   --  Application specific configuration keys are defined in the
   --  my_configuration/my_configuration.ad[sb] files.

   function Get (Key : in Keys) return Boolean;
   function Get (Key : in Keys) return Float;
   function Get (Key : in Keys) return Integer;
   function Get (Key : in Keys) return String;
   function Get (Key : in Keys) return Ada.Strings.Unbounded.Unbounded_String;
   --  Return the value of Key as the appropriate type.

private

   function TUS (S : String) return Ada.Strings.Unbounded.Unbounded_String
                 renames Ada.Strings.Unbounded.To_Unbounded_String;

   Config_File : constant String := "configuration/config.ini";
   --  The path to the config file.

   type Defaults is array (Keys) of Ada.Strings.Unbounded.Unbounded_String;

   Defaults_Array : constant Defaults :=
                      (CSS_Path                 => TUS ("static_content/css"),
                       GIF_Path                 => TUS ("static_content/gif"),
                       GNATCOLL_Traces_Ini_File =>
                         TUS ("configuration/GNATCOLL.SQL.Logs.ini"),
                       Handler_CSS              => TUS (".*\.css"),
                       Handler_GIF              => TUS (".*\.gif"),
                       Handler_HTML             => TUS (".*\.html"),
                       Handler_ICO              => TUS (".*\.ico"),
                       Handler_JPG              => TUS (".*\.jpg"),
                       Handler_JS               => TUS (".*\.js"),
                       Handler_PNG              => TUS (".*\.png"),
                       Handler_XML              => TUS (".*\.xml"),
                       Handler_XSL              => TUS (".*\.xsl"),
                       Handler_Unknown          => TUS ("[^status].*"),
                       HTML_Path                => TUS ("static_content/html"),
                       ICO_Path                 => TUS ("static_content/ico"),
                       Immediate_Flush          => TUS ("False"),
                       JPG_Path                 => TUS ("static_content/jpg"),
                       JS_Path                  => TUS ("static_content/js"),
                       Log_File_Path            => TUS ("logs/"),
                       Max_Slot_Count           => TUS ("3"),
                       Max_Logged_Characters    => TUS ("100_000"),
                       PNG_Path                 => TUS ("static_content/png"),
                       Session_Data_File        =>
                         TUS ("session/session.data"),
                       System_Templates_Path    => TUS ("templates/system"),
                       XML_Path                 => TUS ("static_content/xml"),
                       XSL_Path                 => TUS ("static_content/xsl"));
   --  Default values for the configuration Keys.

   package Ini is new Config_File_Parser (Keys => Keys);

   procedure Initialize;
   --  Load and parse the Config_File configuration file.

end Configuration;
