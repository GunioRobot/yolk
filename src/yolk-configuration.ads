-------------------------------------------------------------------------------
--                                                                           --
--                                  Yolk                                     --
--                                                                           --
--                           Yolk.Configuration                              --
--                                                                           --
--                                  SPEC                                     --
--                                                                           --
--                   Copyright (C) 2010-2012, Thomas Løcke                   --
--                                                                           --
--  This library is free software;  you can redistribute it and/or modify    --
--  it under terms of the  GNU General Public License  as published by the   --
--  Free Software  Foundation;  either version 3,  or (at your  option) any  --
--  later version. This library is distributed in the hope that it will be   --
--  useful, but WITHOUT ANY WARRANTY;  without even the implied warranty of  --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     --
--                                                                           --
--  As a special exception under Section 7 of GPL version 3, you are         --
--  granted additional permissions described in the GCC Runtime Library      --
--  Exception, version 3.1, as published by the Free Software Foundation.    --
--                                                                           --
--  You should have received a copy of the GNU General Public License and    --
--  a copy of the GCC Runtime Library Exception along with this program;     --
--  see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
--  <http://www.gnu.org/licenses/>.                                          --
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
                 Admin_Password, --  AWS
                 Admin_URI, --  AWS
                 AWS_Access_Log_Activate,
                 AWS_Access_Syslog_Facility_Level,
                 AWS_Error_Log_Activate,
                 AWS_Error_Syslog_Facility_Level,
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
                 Error_Log_Activate,
                 Error_Syslog_Facility_Level,
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
                 Info_Log_Activate,
                 Info_Syslog_Facility_Level,
                 Keep_Alive_Force_Limit, --  AWS
                 Key, --  AWS
                 Line_Stack_Size, --  AWS
                 Log_Extended_Fields, --  AWS
                 Max_Concurrent_Download, --  AWS
                 Max_Connection, --  AWS
                 MIME_Types, --  AWS
                 Receive_Timeout, --  AWS
                 Reuse_Address, --  AWS
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
                 SQL_Log_Activate,
                 SQL_Syslog_Facility_Level,
                 SQL_Cache_Log_Activate,
                 SQL_Cache_Syslog_Facility_Level,
                 SQL_Error_Log_Activate,
                 SQL_Error_Syslog_Facility_Level,
                 SQL_Select_Log_Activate,
                 SQL_Select_Syslog_Facility_Level,
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
                       => TUS ("128"),
                       AWS_Access_Log_Activate
                       => TUS ("True"),
                       AWS_Access_Syslog_Facility_Level
                       => TUS ("user:info"),
                       AWS_Error_Log_Activate
                       => TUS ("True"),
                       AWS_Error_Syslog_Facility_Level
                       => TUS ("user:info"),
                       Admin_Password
                       => TUS ("0ac9c9d0c0b1ee058b65ae70c9aeb3a7"),
                       Admin_URI
                       => TUS ("/status"),
                       Case_Sensitive_Parameters
                       => TUS ("True"),
                       Certificate
                       => TUS ("certificates/aws.pem"),
                       Check_URL_Validity
                       => TUS ("True"),
                       Cleaner_Client_Data_Timeout
                       => TUS ("60.0"),
                       Cleaner_Client_Header_Timeout
                       => TUS ("7.0"),
                       Cleaner_Server_Response_Timeout
                       => TUS ("60.0"),
                       Cleaner_Wait_For_Client_Timeout
                       => TUS ("60.0"),
                       Compressed_Cache_Directory
                       => TUS ("static_content/compressed_cache"),
                       Compressed_Max_Age
                       => TUS ("86400"),
                       Compress_Minimum_File_Size
                       => TUS ("200"),
                       Context_Lifetime
                       => TUS ("28800.0"),
                       Error_Log_Activate
                       => TUS ("True"),
                       Error_Syslog_Facility_Level
                       => TUS ("user:info"),
                       Exchange_Certificate
                       => TUS ("False"),
                       Force_Client_Data_Timeout
                       => TUS ("30.0"),
                       Force_Client_Header_Timeout
                       => TUS ("2.0"),
                       Force_Server_Response_Timeout
                       => TUS ("30.0"),
                       Force_Wait_For_Client_Timeout
                       => TUS ("2.0"),
                       Free_Slots_Keep_Alive_Limit
                       => TUS ("1"),
                       Handler_CSS
                       => TUS (".*\.css$"),
                       Handler_GIF
                       => TUS (".*\.gif$"),
                       Handler_HTML
                       => TUS (".*\.html$"),
                       Handler_ICO
                       => TUS (".*\.ico$"),
                       Handler_JPG
                       => TUS (".*\.jpg$"),
                       Handler_JS
                       => TUS (".*\.js$"),
                       Handler_PNG
                       => TUS (".*\.png$"),
                       Handler_SVG
                       => TUS (".*\.svg$"),
                       Handler_XML
                       => TUS (".*\.xml$"),
                       Handler_XSL
                       => TUS (".*\.xsl$"),
                       Hotplug_Port
                       => TUS ("8888"),
                       Immediate_Flush
                       => TUS ("False"),
                       Info_Log_Activate
                       => TUS ("True"),
                       Info_Syslog_Facility_Level
                       => TUS ("user:info"),
                       Keep_Alive_Force_Limit
                       => TUS ("0"),
                       Key
                       => TUS (""),
                       Line_Stack_Size
                       => TUS ("16#150_000#"),
                       Log_Extended_Fields
                       => TUS (""),
                       Max_Concurrent_Download
                       => TUS ("25"),
                       Max_Connection
                       => TUS ("5"),
                       MIME_Types
                       => TUS ("configuration/aws.mime"),
                       Receive_Timeout
                       => TUS ("30.0"),
                       Reuse_Address
                       => TUS ("False"),
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
                       => TUS ("True"),
                       Session_Cleanup_Interval
                       => TUS ("300.0"),
                       Session_Data_File
                       => TUS ("session/session.data"),
                       Session_Lifetime
                       => TUS ("1200.0"),
                       Session_Name
                       => TUS ("Yolk"),
                       SQL_Log_Activate
                       => TUS ("True"),
                       SQL_Syslog_Facility_Level
                       => TUS ("user:info"),
                       SQL_Cache_Log_Activate
                       => TUS ("True"),
                       SQL_Cache_Syslog_Facility_Level
                       => TUS ("user:info"),
                       SQL_Error_Log_Activate
                       => TUS ("True"),
                       SQL_Error_Syslog_Facility_Level
                       => TUS ("user:info"),
                       SQL_Select_Log_Activate
                       => TUS ("True"),
                       SQL_Select_Syslog_Facility_Level
                       => TUS ("user:info"),
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
