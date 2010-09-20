-------------------------------------------------------------------------------
--                                                                           --
--                                  Yolk                                     --
--                                                                           --
--                               rotating_log                                --
--                                                                           --
--                                  SPEC                                     --
--                                                                           --
--                     Copyright (C) 2010, Thomas Løcke                      --
--                                                                           --
--  Yolk is free software;  you can  redistribute it  and/or modify it under --
--  terms of the  GNU General Public License as published  by the Free Soft- --
--  ware  Foundation;  either version 2,  or (at your option) any later ver- --
--  sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
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
--  By adding new components to the Trace_Handles type you can create your own
--  rotating log files.
--  All rotating log file share the same configuration settings, which are
--  set in configuration/config.ini
--  The Rotating_Log package make use of the GNATCOLL.Traces package for the
--  GNATCOLL_SQL trace handle. If you don't use any of the GNATCOLL database
--  stuff (see DB_Connection) in your application, you can disable that
--  specific trace in the configuration/GNATCOLL.SQL.Logs.Ini file.

with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Ada.Text_IO;             use Ada.Text_IO;
with Configuration;           use Configuration;
with GNATCOLL.Traces;         use GNATCOLL.Traces;

package Rotating_Log is

   Cannot_Create_Log_File   : exception;
   Cannot_Write_To_Log_File : exception;
   --  Raised when it's not possible to create, or write to, a log file. This
   --  should result in the application going down. You should not run a public
   --  web-application without logging enabled.

   type Trace_Handles is (Error, Info, GNATCOLL_SQL);
   --  This is the core of the entire Rotating_Log package. It is here we
   --  define the "traces" the system will accept. For each value defined in
   --  this type, a corresponding log file is created, which we can add data to
   --  by calling the Track procedure.
   --
   --  IMPORTANT:
   --    The Error and Info handles should _not_ be removed. They might be
   --    referenced in some of the core/ source files.
   --
   --  NOTE:
   --    The GNATCOLL_SQL value is special. This handle is locked to the
   --    GNATCOLL.SQL traces, so if you want to access the output from
   --    the SQL, SQL.SELECT and SQL.ERROR traces, then you must have this
   --    value defined and a valid GNATCOLL_Traces_Ini_File value.
   --    See comments for Register_Rotating_Log_Stream for more info.

   procedure Track (Handle       : in Trace_Handles;
                    Log_String   : in String);
   --  Add Log_String to Handle.
   --  Exception:
   --    Cannot_Write_To_Log_File

private

   Stream_Rotating_Log        : constant String := "rotating_log";
   --  Name of the stream. This is used in the GNAT.Traces configuration files
   --  or calls to Create to send a stream to a rotating log.
   --  You must have called Register_Rotating_Log_Stream first.
   --  See Register_Rotating_Log_Stream.

   type Access_File is access all Ada.Text_IO.File_Type;

   protected type Log_Object is

      function Get_File_Access return Access_File;
      --  Return access to an Ada.Text_IO.File_Type.
      function Get_Size return Natural;
      --  Return the amount of characters added to the log. See Set_Size.
      function Get_Slot return String;
      --  Return the Current_Slot value as a String.
      procedure Move_To_Next_Slot;
      --  Move to the next slot. Basically we just cycle 1 .. Max_Slot_Count
      procedure Set_File_Access;
      --  Allocate a new Ada.Text_IO.File_Type.
      procedure Set_Size (Length : Natural);
      --  Add Length to Log_Object.Size. We use this to decide when to cycle
      --  the logfiles. If Size > Max_Logged_Characters, then we cycle to the
      --  next slot.
      --  This is done instead of checking the size of the logfile, to avoid
      --  going to disk on every call to Track. It's not as accurate, but it
      --  will probably not be off by much.
      --  Max_Logged_Characters is defined in configuration/config.ini.
      procedure Write_To (Log_String : in String);
      --  Write Log_String to file.

   private

      File           : Access_File;
      Current_Slot   : Positive := 1;
      Slot_Max       : Positive := Get (Max_Slot_Count);
      Size           : Natural := 0;

   end Log_Object;

   type Access_Log_Object is access all Log_Object;
   type Log_Objects_Array is array (Trace_Handles) of Access_Log_Object;

   Log_Objects_List         : Log_Objects_Array;
   --  This array holds access to the individual Log_Objects, one for each
   --  value defined in Trace_Handles.

   procedure Initialize;
   --  Parse the GNATCOLL_Traces_Ini_File (if it exists) and setup the
   --  Log_Objects_List array according to the Trace_Handles type.
   --  The GNATCOLL_Traces_Ini_File is defined in configuration/config.ini.
   --  Exceptions:
   --    Cannot_Create_Log_File

   ----------------------------------------------------------------------------
   --  Types and methods needed for a custom GNATCOLL.Traces stream. These are
   --  necessary to gain access to the GNATCOLL.SQL SQL, SQL.SELECT and
   --  SQL.ERROR traces.
   ----------------------------------------------------------------------------
   type Factory is new Stream_Factory with null record;
   type Rotating_Log_Record is new Trace_Stream_Record
   with
      record
         Handle : Trace_Handles;
         Buffer : Unbounded_String;
      end record;
   type Access_Rotating_Log_Record is access all Rotating_Log_Record;
   overriding
   function New_Stream (Fact : Factory; Args : String) return Trace_Stream;
   --  Create a rotating log stream
   overriding
   procedure Newline (Stream : in out Rotating_Log_Record);
   --  Write the Rotating_Log_Record.Buffer to Rotating_Log_Record.Handle.
   overriding
   procedure Put (Stream : in out Rotating_Log_Record; Log_String : String);
   --  Add Log_String to Rotating_Log_Record.Buffer.
   procedure Register_Rotating_Log_Stream;
   --  Register a GNAT.Traces stream that can send its output to a rotating
   --  log.
   --  See the configuration/GNATCOLL.SQL.Logs.ini file for more information.

   overriding
   function Supports_Color (Stream : Rotating_Log_Record) return Boolean;
   --  Does the stream support color output? For this specific package, no.
   --  Always return False.
   overriding
   function Supports_Time  (Stream : Rotating_Log_Record) return Boolean;
   --  Should we output time? No. Time is set in the Track procedure. Always
   --  return False.

end Rotating_Log;
