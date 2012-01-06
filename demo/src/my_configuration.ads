-------------------------------------------------------------------------------
--                                                                           --
--                                  Yolk                                     --
--                                                                           --
--                             My_Configuration                              --
--                                                                           --
--                                  SPEC                                     --
--                                                                           --
--                   Copyright (C) 2010-2012, Thomas Løcke                   --
--                                                                           --
--  This is free software;  you can redistribute it and/or modify it         --
--  under terms of the  GNU General Public License  as published by the      --
--  Free Software  Foundation;  either version 3,  or (at your  option) any  --
--  later version. This library is distributed in the hope that it will be   --
--  useful, but WITHOUT ANY WARRANTY;  without even the implied warranty of  --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     --
--  You should have received a copy of the GNU General Public License and    --
--  a copy of the GCC Runtime Library Exception along with this program;     --
--  see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
--  <http://www.gnu.org/licenses/>.                                          --
--                                                                           --
-------------------------------------------------------------------------------

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
                 Handler_DB_Test,
                 Handler_Dir,
                 Handler_Email,
                 Handler_Index,
                 Handler_Session_Test,
                 Handler_Syndication,
                 SMTP_Host,
                 SMTP_Port,
                 Template_DB_Test,
                 Template_Email,
                 Template_Index,
                 Template_Session_Test);
   --  The valid configuration keys.

   type Defaults_Array is array (Keys) of
     Ada.Strings.Unbounded.Unbounded_String;

   Default_Values : constant Defaults_Array :=
                      (DB_Host
                       => TUS ("localhost"),
                       DB_Name
                       => TUS ("yolk"),
                       DB_User
                       => TUS ("adauser"),
                       DB_Password
                       => TUS ("secret"),
                       Handler_DB_Test
                       => TUS ("/dbtest"),
                       Handler_Dir
                       => TUS ("^/dir/.*"),
                       Handler_Email
                       => TUS ("/email"),
                       Handler_Index
                       => TUS ("/"),
                       Handler_Session_Test
                       => TUS ("/sessiontest"),
                       Handler_Syndication
                       => TUS ("/syndication"),
                       SMTP_Host
                       => TUS ("localhost"),
                       SMTP_Port
                       => TUS ("25"),
                       Template_DB_Test
                       => TUS ("templates/website/database.tmpl"),
                       Template_Email
                       => TUS ("templates/website/email.tmpl"),
                       Template_Index
                       => TUS ("templates/website/index.tmpl"),
                       Template_Session_Test
                       => TUS ("templates/website/session_test.tmpl"));
   --  Default values for the configuration Keys. These values can be over-
   --  written by the configuration file given when instantiating the
   --  Yolk.Config_File_Parser generic.

   package Config is new Yolk.Config_File_Parser
     (Key_Type => Keys,
      Defaults_Array_Type => Defaults_Array,
      Defaults => Default_Values,
      Config_File => "configuration/my_config.ini");

end My_Configuration;
