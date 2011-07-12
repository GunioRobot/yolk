-------------------------------------------------------------------------------
--                                                                           --
--                                  Yolk                                     --
--                                                                           --
--                            Yolk.Rotating_Log                              --
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

--  This package provides the ability to create and write to rotating log
--  files. These files are rotated based on the amount of characters written to
--  them.
--  By adding new components to the Trace_Handles type you can create extra
--  rotating log files.
--  All rotating log file share the same configuration settings, which are
--  set in the Yolk.Configuration package.
--  The Yolk.Rotating_Log package make use of the GNATCOLL.Traces package for
--  the GNATCOLL_SQL trace handle. If you don't use any of the GNATCOLL
--  database stuff (see DB_Connection) in your application, you can disable
--  that specific trace in the GNATCOLL.SQL.Logs.Ini file.

package Yolk.Rotating_Log is

   Cannot_Create_Log_File   : exception;
   Cannot_Write_To_Log_File : exception;
   --  Raised when it's not possible to create, or write to, a log file. This
   --  should result in the application going down. You should not run a public
   --  web-application without logging enabled.

   type Trace_Handles is (Error, Info, GNATCOLL_SQL);
   --  Here we define the "traces" the system will accept. For each value
   --  defined in this type, a corresponding log file is created, which we can
   --  add data to by calling the Track procedure.
   --
   --  IMPORTANT:
   --    The Error and Info handles should _not_ be removed, as they are used
   --    throughout the Yolk packages.
   --
   --  NOTE:
   --    The GNATCOLL_SQL value is special. This handle is locked to the
   --    GNATCOLL.SQL traces, so if you want to access the output from
   --    the SQL, SQL.SELECT and SQL.ERROR traces, then you _must_ have this
   --    value defined and a valid GNATCOLL_Traces_Ini_File value.
   --    See the demo/exe/configuration/GNATCOLL.SQL.Logs.ini file for more
   --    information.

   procedure Start_Rotating_Logs
     (Called_From_Main_Task_Exception_Handler : Boolean := False);
   --  Parse the GNATCOLL_Traces_Ini_File (if it exists) and setup the
   --  Log_Objects_List array according to the Trace_Handles type.
   --  The path to the GNATCOLL_Traces_Ini_File is defined in the
   --  Yolk.Configuration package.
   --  Calling this more than once does nothing.
   --  Exceptions:
   --    Cannot_Create_Log_File

   procedure Track
     (Handle       : in Trace_Handles;
      Log_String   : in String);
   --  Add Log_String to Handle. Calling Track before having called
   --  Start_Rotating_Logs will raise a Cannot_Write_To_Log_File exception.
   --  Exception:
   --    Cannot_Write_To_Log_File

end Yolk.Rotating_Log;
