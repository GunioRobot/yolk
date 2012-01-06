-------------------------------------------------------------------------------
--                                                                           --
--                                  Yolk                                     --
--                                                                           --
--                                Yolk.Log                                   --
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

package Yolk.Log is

   type Trace_Handles is (Error, Info, SQL, SQL_Cache, SQL_Error, SQL_Select);
   --  These six handles are available to your application. Not that the SQL
   --  handles are also used by GNATColl's database packages, if enabled.

   procedure AWS_Access_Log_Writer
     (Message : in String);
   --  AWS can send its access log data to this procedure.

   procedure AWS_Error_Log_Writer
     (Message : in String);
   --  AWS can send its error log data to this procedure.

   procedure Trace
     (Handle  : in Trace_Handles;
      Message : in String);
   --  Use Trace to log various events in your application. All log data is
   --  sent to syslogd according to the configuration found in the config.ini
   --  file.

end Yolk.Log;
