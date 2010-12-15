-------------------------------------------------------------------------------
--                                                                           --
--                                  Yolk                                     --
--                                                                           --
--                             configuration                                 --
--                                                                           --
--                                  SPEC                                     --
--                                                                           --
--                     Copyright (C) 2010, Thomas L�cke                      --
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

--  Default configuration settings for a Yolk application. For resource related
--  configuration, take a look at the My_Configuration package.

with Ada.Strings.Unbounded;
with Config_File_Parser;
with Utilities; use Utilities;

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
   --  The valid configuration keys.
   --  These configuration keys are essential for other core parts of this
   --  software, so you should not change them, unless you know what you're
   --  doing.

   type Defaults_Array is array (Keys) of
     Ada.Strings.Unbounded.Unbounded_String;

   Defaults : Defaults_Array :=
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
   --  Default values for the configuration Keys. These values can be over-
   --  written by the configuration file given when instantiating a new
   --  Config_File_Parser object.

   package Config is new Config_File_Parser
     (Keys => Keys,
      Defaults_Array => Defaults_Array,
      Defaults => Defaults,
      Config_File    => "configuration/config.ini");

end Configuration;
