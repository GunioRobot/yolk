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

package Yolk.Rotating_Log is

   Cannot_Create_Log_File   : exception;
   Cannot_Write_To_Log_File : exception;
   --  Raised when it's not possible to create, or write to, a log file. This
   --  should result in the application going down. You should not run a public
   --  web-application without logging enabled.

   type Trace_Handles is (Error, Info, SQL);
   --  Here we define the log tracks. For each value defined in this type, a
   --  corresponding log file is created, which we can add data to by calling
   --  the Track procedure.
   --
   --  IMPORTANT:
   --    The Error and Info handles should _not_ be removed, as they are used
   --    throughout the Yolk packages.
   --
   --  NOTE:
   --    The SQL handle is special. This handle is locked to the GNATCOLL.SQL
   --    traces, so all logging done by the GNATCOLL.SQL system is done to this
   --    track.

   procedure Start_Rotating_Logs
     (Called_From_Main_Task_Exception_Handler : Boolean := False);
   --  Creates the three rotating log files, according to the Trace_Handles
   --  type. Also activates the GNATCOLL.SQL logging system if the following
   --  Yolk.Configuration parameters are True:
   --
   --    Activate_Rotating_SQL_Log
   --    Activate_Rotating_SQL_Cache_Log
   --    Activate_Rotating_SQL_Error_Log
   --    Activate_Rotating_SQL_Select_Log
   --
   --  The Error and Info tracks are always active. Calling this proceudure
   --  more than once does nothing, except log the fact to the Info track. The
   --  Called_From_Main_Task_Exception_Handler parameter is there so you can
   --  call Start_Rotating_Logs from the main task exception handler without
   --  getting the notice about the system already being started.
   --
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
