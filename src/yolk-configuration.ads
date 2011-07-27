-------------------------------------------------------------------------------
--                                                                           --
--                                  Yolk                                     --
--                                                                           --
--                           Yolk.Configuration                              --
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

--  Default configuration settings for a Yolk application.
--  Keys postfixed with "--  AWS" are keys that are used specifically by AWS.
--  Other keys are for Yolk specific configuration.

with Ada.Strings.Unbounded;
with AWS.Config;
with Yolk.Config_File_Parser;
with Yolk.Utilities;

package Yolk.Configuration is

   use Ada.Strings.Unbounded;
   use Yolk.Utilities;

   type Keys is (Accept_Queue_Size, --  AWS
                 Activate_Rotating_SQL_Log,
                 Activate_Rotating_SQL_Cache_Log,
                 Activate_Rotating_SQL_Error_Log,
                 Activate_Rotating_SQL_Select_Log,
                 Admin_Password, --  AWS
                 Admin_URI, --  AWS
                 Amount_Of_Log_Files_To_Keep,
                 Case_Sensitive_Parameters, --  AWS
                 Certificate, --  AWS
                 Check_URL_Validity, --  AWS
                 Cleaner_Client_Data_Timeout, --  AWS
                 Cleaner_Client_Header_Timeout, --  AWS
                 Cleaner_Server_Response_Timeout, --  AWS
                 Cleaner_Wait_For_Client_Timeout, --  AWS
                 Compressed_Cache_Directory,
                 Compressed_Max_Age,
                 Compress_Minimum_File_Size,
                 Context_Lifetime, --  AWS
                 Enable_Access_Log,
                 Error_Log_Filename_Prefix, -- AWS
                 Error_Log_Split_Mode, --  AWS
                 Exchange_Certificate, --  AWS
                 Force_Client_Data_Timeout, --  AWS
                 Force_Client_Header_Timeout, --  AWS
                 Force_Server_Response_Timeout, --  AWS
                 Force_Wait_For_Client_Timeout, --  AWS
                 Free_Slots_Keep_Alive_Limit, --  AWS
                 Handler_CSS,
                 Handler_GIF,
                 Handler_HTML,
                 Handler_ICO,
                 Handler_JPG,
                 Handler_JS,
                 Handler_PNG,
                 Handler_SVG,
                 Handler_XML,
                 Handler_XSL,
                 Hotplug_Port, --  AWS
                 Immediate_Flush,
                 Keep_Alive_Force_Limit, --  AWS
                 Key, --  AWS
                 Line_Stack_Size, --  AWS
                 Log_Extended_Fields, --  AWS
                 Log_File_Cleanup_Interval,
                 Log_File_Directory, --  AWS
                 Log_Filename_Prefix, --  AWS
                 Log_Size_Limit, --  AWS
                 Log_Split_Mode, --  AWS
                 Max_Concurrent_Download, --  AWS
                 Max_Connection, --  AWS
                 Max_Slot_Count,
                 MIME_Types, --  AWS
                 Receive_Timeout, --  AWS
                 Reuse_Address, --  AWS
                 Rotating_Log_Size_Limit,
                 Security, --  AWS
                 Security_Mode, --  AWS
                 Send_Timeout, --  AWS
                 Server_Host, --  AWS
                 Server_Name, --  AWS
                 Server_Port, --  AWS
                 Session, --  AWS
                 Session_Cleanup_Interval, --  AWS
                 Session_Data_File,
                 Session_Lifetime, --  AWS
                 Session_Name, --  AWS
                 Status_Page, --  AWS
                 System_Templates_Path,
                 Transient_Cleanup_Interval, --  AWS
                 Transient_Lifetime, --  AWS
                 Upload_Directory, --  AWS
                 Upload_Size_Limit, --  AWS
                 WWW_Root, --  AWS
                 Yolk_User);

   type Defaults_Array is array (Keys) of Unbounded_String;

   Default_Values : constant Defaults_Array :=
                      (Accept_Queue_Size
                       => TUS ("64"),
                       Activate_Rotating_SQL_Log
                       => TUS ("True"),
                       Activate_Rotating_SQL_Cache_Log
                       => TUS ("True"),
                       Activate_Rotating_SQL_Error_Log
                       => TUS ("True"),
                       Activate_Rotating_SQL_Select_Log
                       => TUS ("True"),
                       Admin_Password
                       => TUS ("0ac9c9d0c0b1ee058b65ae70c9aeb3a7"),
                       Admin_URI
                       => TUS ("/status"),
                       Amount_Of_Log_Files_To_Keep
                       => TUS ("30"),
                       Case_Sensitive_Parameters
                       => TUS ("True"),
                       Certificate
                       => TUS ("certificates/aws.pem"),
                       Check_URL_Validity
                       => TUS ("True"),
                       Cleaner_Client_Data_Timeout
                       => TUS ("28800.0"),
                       Cleaner_Client_Header_Timeout
                       => TUS ("7.0"),
                       Cleaner_Server_Response_Timeout
                       => TUS ("28800.0"),
                       Cleaner_Wait_For_Client_Timeout
                       => TUS ("80.0"),
                       Compressed_Cache_Directory
                       => TUS ("static_content/compressed_cache"),
                       Compressed_Max_Age
                       => TUS ("86400"),
                       Compress_Minimum_File_Size
                       => TUS ("200"),
                       Context_Lifetime
                       => TUS ("28800.0"),
                       Enable_Access_Log
                       => TUS ("True"),
                       Error_Log_Filename_Prefix
                       => TUS ("yolk_error"),
                       Error_Log_Split_Mode
                       => TUS ("Daily"),
                       Exchange_Certificate
                       => TUS ("False"),
                       Force_Client_Data_Timeout
                       => TUS ("10800.0"),
                       Force_Client_Header_Timeout
                       => TUS ("2.0"),
                       Force_Server_Response_Timeout
                       => TUS ("10800.0"),
                       Force_Wait_For_Client_Timeout
                       => TUS ("2.0"),
                       Free_Slots_Keep_Alive_Limit
                       => TUS ("1"),
                       Handler_CSS
                       => TUS ("\.css$"),
                       Handler_GIF
                       => TUS ("\.gif$"),
                       Handler_HTML
                       => TUS ("\.html$"),
                       Handler_ICO
                       => TUS ("\.ico$"),
                       Handler_JPG
                       => TUS ("\.jpg$"),
                       Handler_JS
                       => TUS ("\.js$"),
                       Handler_PNG
                       => TUS ("\.png$"),
                       Handler_SVG
                       => TUS ("\.svg$"),
                       Handler_XML
                       => TUS ("\.xml$"),
                       Handler_XSL
                       => TUS ("\.xsl$"),
                       Hotplug_Port
                       => TUS ("8888"),
                       Immediate_Flush
                       => TUS ("False"),
                       Keep_Alive_Force_Limit
                       => TUS ("0"),
                       Key
                       => TUS (""),
                       Line_Stack_Size
                       => TUS ("16#150_000#"),
                       Log_Extended_Fields
                       => TUS (""),
                       Log_File_Cleanup_Interval
                       => TUS ("3600.0"),
                       Log_File_Directory
                       => TUS ("logs/"),
                       Log_Filename_Prefix
                       => TUS ("yolk_access"),
                       Log_Size_Limit
                       => TUS ("10_000_000"),
                       Log_Split_Mode
                       => TUS ("Daily"),
                       Max_Concurrent_Download
                       => TUS ("25"),
                       Max_Connection
                       => TUS ("5"),
                       Max_Slot_Count
                       => TUS ("3"),
                       MIME_Types
                       => TUS ("configuration/aws.mime"),
                       Receive_Timeout
                       => TUS ("30.0"),
                       Reuse_Address
                       => TUS ("False"),
                       Rotating_Log_Size_Limit
                       => TUS ("1_000_000"),
                       Security
                       => TUS ("False"),
                       Security_Mode
                       => TUS ("SSLv23"),
                       Send_Timeout
                       => TUS ("40.0"),
                       Server_Host
                       => TUS (""),
                       Server_Name
                       => TUS ("Yolk"),
                       Server_Port
                       => TUS ("4242"),
                       Session
                       => TUS ("False"),
                       Session_Cleanup_Interval
                       => TUS ("300.0"),
                       Session_Data_File
                       => TUS ("session/session.data"),
                       Session_Lifetime
                       => TUS ("1200.0"),
                       Session_Name
                       => TUS ("Yolk"),
                       Status_Page
                       => TUS ("templates/system/aws_status.thtml"),
                       System_Templates_Path
                       => TUS ("templates/system"),
                       Transient_Cleanup_Interval
                       => TUS ("180.0"),
                       Transient_Lifetime
                       => TUS ("300.0"),
                       Upload_Directory
                       => TUS ("uploads"),
                       Upload_Size_Limit
                       => TUS ("16#500_000#"),
                       WWW_Root
                       => TUS ("static_content"),
                       Yolk_User
                       => TUS ("thomas"));
   --  Default values for the configuration Keys. These values can be over-
   --  written by the configuration file given when instantiating the
   --  Config_File_Parser generic.

   package Config is new Config_File_Parser
     (Key_Type => Keys,
      Defaults_Array_Type => Defaults_Array,
      Defaults            => Default_Values);

   function Get_AWS_Configuration return AWS.Config.Object;
   --  Load the AWS relevant configuration settings from the config.ini file.

end Yolk.Configuration;
