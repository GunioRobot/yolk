-------------------------------------------------------------------------------
--                                                                           --
--                                  Yolk                                     --
--                                                                           --
--                                Yolk.Log                                   --
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
