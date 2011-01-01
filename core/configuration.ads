-------------------------------------------------------------------------------
--                                                                           --
--                                  Yolk                                     --
--                                                                           --
--                             configuration                                 --
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

--  Default configuration settings for a Yolk application. For resource related
--  configuration, take a look at the My_Configuration package.

with Ada.Strings.Unbounded;
with AWS.Config;
with Config_File_Parser;
with Utilities; use Utilities;

package Configuration is

   type Keys is (Admin_Password, -- AWS
                 Admin_URI, -- AWS
                 Amount_Of_Log_Files_To_Keep,
                 Certificate, -- AWS
                 Error_Log_Filename_Prefix, -- AWS
                 Error_Log_Split_Mode, -- AWS
                 GNATCOLL_Traces_Ini_File,
                 Log_File_Cleanup_Interval,
                 Log_Filename_Prefix, -- AWS
                 Log_Size_Limit, -- AWS
                 Log_Split_Mode, -- AWS
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
                 Immediate_Flush,
                 Log_File_Directory, -- AWS
                 Max_Connection, -- AWS
                 Max_Slot_Count,
                 MIME_Types, -- AWS
                 Reuse_Address, -- AWS
                 Rotating_Log_Size_Limit,
                 Security, -- AWS
                 Security_Mode, -- AWS
                 Server_Name, -- AWS
                 Server_Port, -- AWS
                 Session, -- AWS
                 Session_Data_File, -- AWS
                 Session_Lifetime, -- AWS
                 Session_Name, -- AWS
                 Static_Content_Path,
                 Status_Page, -- AWS
                 System_Templates_Path);
   --  The valid configuration keys.
   --  These configuration keys are essential for core parts of Yolk, so you
   --  should not change them, unless you know what you're doing.

   type Defaults_Array is array (Keys) of
     Ada.Strings.Unbounded.Unbounded_String;

   Default_Values : constant Defaults_Array :=
                      (Admin_Password
                       => TUS ("0ac9c9d0c0b1ee058b65ae70c9aeb3a7"),
                       Admin_URI
                       => TUS ("/status"),
                       Amount_Of_Log_Files_To_Keep
                       => TUS ("30"),
                       Certificate
                       => TUS ("certificates/aws.pem"),
                       Error_Log_Filename_Prefix
                       => TUS ("yolk_error"),
                       Error_Log_Split_Mode
                       => TUS ("Daily"),
                       GNATCOLL_Traces_Ini_File
                       => TUS ("configuration/GNATCOLL.SQL.Logs.ini"),
                       Log_File_Cleanup_Interval
                       => TUS ("3600"),
                       Log_Filename_Prefix
                       => TUS ("yolk_access"),
                       Log_Size_Limit
                       => TUS ("10_000_000"),
                       Log_Split_Mode
                       => TUS ("Daily"),
                       Handler_CSS
                       => TUS (".*\.css"),
                       Handler_GIF
                       => TUS (".*\.gif"),
                       Handler_HTML
                       => TUS (".*\.html"),
                       Handler_ICO
                       => TUS (".*\.ico"),
                       Handler_JPG
                       => TUS (".*\.jpg"),
                       Handler_JS
                       => TUS (".*\.js"),
                       Handler_PNG
                       => TUS (".*\.png"),
                       Handler_XML
                       => TUS (".*\.xml"),
                       Handler_XSL
                       => TUS (".*\.xsl"),
                       Handler_Unknown
                       => TUS ("[^status].*"),
                       Immediate_Flush
                       => TUS ("False"),
                       Log_File_Directory
                       => TUS ("logs/"),
                       Max_Connection
                       => TUS ("5"),
                       Max_Slot_Count
                       => TUS ("3"),
                       MIME_Types
                       => TUS ("configuration/aws.mime"),
                       Reuse_Address
                       => TUS ("False"),
                       Rotating_Log_Size_Limit
                       => TUS ("1_000_000"),
                       Security
                       => TUS ("False"),
                       Security_Mode
                       => TUS ("SSLv23"),
                       Server_Name
                       => TUS ("Yolk"),
                       Server_Port
                       => TUS ("4242"),
                       Session
                       => TUS ("False"),
                       Session_Data_File
                       => TUS ("session/session.data"),
                       Session_Lifetime
                       => TUS ("1200"),
                       Session_Name
                       => TUS ("Yolk"),
                       Static_Content_Path
                       => TUS ("static_content"),
                       Status_Page
                       => TUS ("status/aws_status.thtml"),
                       System_Templates_Path
                       => TUS ("templates/system"));
   --  Default values for the configuration Keys. These values can be over-
   --  written by the configuration file given when instantiating the
   --  Config_File_Parser generic.

   package Config is new Config_File_Parser
     (Keys => Keys,
      Defaults_Array => Defaults_Array,
      Default_Values => Default_Values,
      Config_File    => "configuration/config.ini");

   function Get_AWS_Configuration return AWS.Config.Object;
   --  Load the AWS relevant configuration settings from the config.ini file.

end Configuration;
