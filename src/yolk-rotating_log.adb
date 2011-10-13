-------------------------------------------------------------------------------
--                                                                           --
--                                  Yolk                                     --
--                                                                           --
--                            Yolk.Rotating_Log                              --
--                                                                           --
--                                  BODY                                     --
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

with Ada.Calendar;
with Ada.Calendar.Formatting;
with Ada.Calendar.Time_Zones;
with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with GNATCOLL.Traces;
with Yolk.Configuration;
with Yolk.Process_Control;

package body Yolk.Rotating_Log is

   use Yolk.Configuration;

   package EIO is new Ada.Text_IO.Enumeration_IO (Trace_Handles);

   Is_Started : Boolean := False;
   --  Is set to True when Start is called the first time.

   Stream_Rotating_Log : constant String := "yolk-rotating_log";
   --  Name of the stream. This is used in the calls to Create to send a stream
   --  to a rotating log.
   --  You must have called Register_Rotating_Log_Stream first.
   --  See Register_Rotating_Log_Stream.

   type Access_File is access all Ada.Text_IO.File_Type;

   protected type Log_Object is

      function Get_File_Access return Access_File;
      --  Return access to an Ada.Text_IO.File_Type.

      function Get_Slot return String;
      --  Return the Current_Slot value as a String.

      procedure Move_To_Next_Slot;
      --  Move to the next slot. Basically we just cycle 1 .. Max_Slot_Count

      procedure Set_File_Access;
      --  Allocate a new Ada.Text_IO.File_Type.

      procedure Set_Handle
        (A_Handle : in Trace_Handles);
      --  Set the Trace_Handles value for this specific Log_Object.

      procedure Write
        (Log_String : in String);

   private

      Current_Slot   : Positive := 1;
      File           : Access_File;
      Handle         : Trace_Handles;
      Size           : Natural := 0;
      Slot_Max       : Positive := Config.Get (Max_Slot_Count);

   end Log_Object;

   type Access_Log_Object is access all Log_Object;
   type Log_Objects_Array is array (Trace_Handles) of Access_Log_Object;

   Log_Objects_List : Log_Objects_Array;
   --  This array holds access to the individual Log_Objects, one for each
   --  value defined in Trace_Handles.

   ----------------------------------------------------------------------------
   --  Types and methods needed for a custom GNATCOLL.Traces stream. These are
   --  necessary to gain access to the GNATCOLL.SQL SQL, SQL.SELECT and
   --  SQL.ERROR traces.
   ----------------------------------------------------------------------------
   type Factory is new GNATCOLL.Traces.Stream_Factory with null record;

   type Rotating_Log_Record is new GNATCOLL.Traces.Trace_Stream_Record
   with
      record
         Buffer : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   type Access_Rotating_Log_Record is access all Rotating_Log_Record;

   overriding
   function New_Stream
     (Fact : Factory; Args : String)
      return GNATCOLL.Traces.Trace_Stream;
   --  Create a rotating log stream

   overriding
   procedure Newline
     (Stream : in out Rotating_Log_Record);
   --  Write the Rotating_Log_Record.Buffer to Rotating_Log_Record.Handle.

   overriding
   procedure Put
     (Stream : in out Rotating_Log_Record; Log_String : String);
   --  Add Log_String to Rotating_Log_Record.Buffer.

   procedure Register_Rotating_Log_Stream;
   --  Register a GNAT.Traces stream that can send its output to a rotating
   --  log.

   overriding
   function Supports_Color
     (Stream : Rotating_Log_Record)
      return Boolean;
   --  Does the stream support color output? For this specific package, no.
   --  Always return False.

   overriding
   function Supports_Time
     (Stream : Rotating_Log_Record)
      return Boolean;
   --  Should we output time? No. Time is set in the Trace procedure. Always
   --  return False.

   ------------------
   --  New_Stream  --
   ------------------

   function New_Stream
     (Fact : in Factory;
      Args : in String)
      return GNATCOLL.Traces.Trace_Stream
   is

      use GNATCOLL.Traces;

      pragma Unreferenced (Fact);
      pragma Unreferenced (Args);

      Self : Access_Rotating_Log_Record;

   begin

      Self := new Rotating_Log_Record;
      return Trace_Stream (Self);

   end New_Stream;

   ---------------
   --  Newline  --
   ---------------

   procedure Newline
     (Stream : in out Rotating_Log_Record)
   is

      use Ada.Strings.Unbounded;

   begin

      Trace (Handle     => SQL,
             Log_String => To_String (Stream.Buffer));
      Stream.Buffer := Null_Unbounded_String;

   end Newline;

   -----------
   --  Put  --
   -----------

   overriding
   procedure Put
     (Stream      : in out Rotating_Log_Record;
      Log_String  : in     String)
   is

      use Ada.Strings.Unbounded;

   begin

      Append (Stream.Buffer, Log_String);

   end Put;

   ------------------------------------
   --  Register_Rotating_Log_Stream  --
   ------------------------------------

   procedure Register_Rotating_Log_Stream
   is

      use GNATCOLL.Traces;

      Fact : constant Stream_Factory_Access := new Factory;

   begin

      Register_Stream_Factory (Stream_Rotating_Log, Fact);

   end Register_Rotating_Log_Stream;

   ---------------------------
   --  Start_Rotating_Logs  --
   ---------------------------

   procedure Start_Rotating_Logs
     (Emit_Warning_If_Already_Running : Boolean := True)
   is

      use Ada.Text_IO;
      use GNATCOLL.Traces;

      A_Handle : Trace_Handles := Trace_Handles'First;
      --  Set A_Handle to Trace_Handles'First to avoid a "'A_Handle' may be
      --  used uninitialized in this function" warning.

   begin

      if not Is_Started then
         Is_Started := True;

         Register_Rotating_Log_Stream;

         GNATCOLL.Traces.Set_Active
           (Handle => GNATCOLL.Traces.Create
              (Unit_Name => "SQL",
               Stream    => "&yolk-rotating_log"),
            Active => Config.Get (Activate_Rotating_SQL_Log));

         GNATCOLL.Traces.Set_Active
           (Handle => GNATCOLL.Traces.Create
              (Unit_Name => "SQL.SELECT",
               Stream    => "&yolk-rotating_log"),
            Active => Config.Get (Activate_Rotating_SQL_Select_Log));

         GNATCOLL.Traces.Set_Active
           (Handle => GNATCOLL.Traces.Create
              (Unit_Name => "SQL.ERROR",
               Stream    => "&yolk-rotating_log"),
            Active => Config.Get (Activate_Rotating_SQL_Error_Log));

         GNATCOLL.Traces.Set_Active
           (Handle => GNATCOLL.Traces.Create
              (Unit_Name => "SQL.CACHE",
               Stream    => "&yolk-rotating_log"),
            Active => Config.Get (Activate_Rotating_SQL_Cache_Log));

         for Handle in Trace_Handles loop
            A_Handle := Handle;
            --  We need this assignment in case an exception is raised. See the
            --  when others exception handler further down.

            Log_Objects_List (Handle) := new Log_Object;
            Log_Objects_List (Handle).Set_File_Access;
            Log_Objects_List (Handle).Set_Handle (A_Handle => Handle);

            Create
              (File => Log_Objects_List (Handle).Get_File_Access.all,
               Mode => Out_File,
               Name => Config.Get (Log_File_Directory) &
               Config.Get (Server_Name) & "-rotating-" &
               Trace_Handles'Image (Handle) & "-" &
               Log_Objects_List (Handle).Get_Slot & ".log");
         end loop;
      else
         if Emit_Warning_If_Already_Running then
            Trace (Handle     => Info,
                   Log_String => "Rotating log system already started.");
         end if;
      end if;

   exception
      when Config.Conversion_Error =>
         raise;
      when others =>
         Process_Control.Stop;
         raise Cannot_Create_Log_File with
           "Handle: " & Trace_Handles'Image (A_Handle) & " and slot: "
           & Log_Objects_List (A_Handle).Get_Slot;

   end Start_Rotating_Logs;

   ----------------------
   --  Supports_Color  --
   ----------------------

   function Supports_Color
     (Stream : Rotating_Log_Record)
      return Boolean
   is

      pragma Unreferenced (Stream);

   begin

      return False;

   end Supports_Color;

   ---------------------
   --  Supports_Time  --
   ---------------------

   function Supports_Time
     (Stream : Rotating_Log_Record)
      return Boolean
   is

      pragma Unreferenced (Stream);

   begin

      return False;

   end Supports_Time;

   -------------
   --  Trace  --
   -------------

   procedure Trace
     (Handle       : in Trace_Handles;
      Log_String   : in String)
   is

      Log : constant Access_Log_Object := Log_Objects_List (Handle);

   begin

      --  We ignore calls to Trace if Start_Rotating_Logs hasn't been called
      --  yet.
      if Is_Started then
         Log.Write (Log_String => Log_String);
      end if;

   exception
      when Config.Conversion_Error =>
         raise;
      when others =>
         Process_Control.Stop;
         raise Cannot_Write_To_Log_File with
           "Handle: " & Trace_Handles'Image (Handle) & " and slot: ";

   end Trace;

   ------------------
   --  Log_Object  --
   ------------------

   protected body Log_Object is

      -----------------------
      --  Get_File_Access  --
      -----------------------

      function Get_File_Access return Access_File
      is
      begin

         return File;

      end Get_File_Access;

      ----------------
      --  Get_Slot  --
      ----------------

      function Get_Slot return String
      is

         use Ada.Strings;
         use Ada.Strings.Fixed;

      begin

         return Trim (Source => Current_Slot'Img,
                      Side   => Left);

      end Get_Slot;

      -------------------------
      --  Move_To_Next_Slot  --
      -------------------------

      procedure Move_To_Next_Slot
      is
      begin

         if Current_Slot = Slot_Max then
            Current_Slot := 1;
         else
            Current_Slot := Current_Slot + 1;
         end if;

         Size := 0;

      end Move_To_Next_Slot;

      -----------------------
      --  Set_File_Access  --
      -----------------------

      procedure Set_File_Access
      is

         use Ada.Text_IO;

      begin

         File := new File_Type;

      end Set_File_Access;

      ------------------
      --  Set_Handle  --
      ------------------

      procedure Set_Handle
        (A_Handle : in Trace_Handles)
      is
      begin

         Handle := A_Handle;

      end Set_Handle;

      -------------
      --  Write  --
      -------------

      procedure Write
        (Log_String : in String)
      is

         use Ada.Calendar;
         use Ada.Calendar.Formatting;
         use Ada.Calendar.Time_Zones;
         use Ada.Text_IO;

         Circa_Length   : Natural := Log_String'Length;
         Now            : constant Time := Clock;
         Offset         : constant Time_Offset := UTC_Time_Offset;

      begin

         if Size > Config.Get (Rotating_Log_Size_Limit) then
            if Is_Open (File => Get_File_Access.all) then
               Close (File => Get_File_Access.all);
            end if;

            Move_To_Next_Slot;

            Create
              (File => Get_File_Access.all,
               Mode => Out_File,
               Name => Config.Get (Log_File_Directory) &
               Config.Get (Server_Name) & "-rotating-" &
               Trace_Handles'Image (Handle) & "-" &
               Get_Slot & ".log");
         end if;

         Put (File => Get_File_Access.all,
              Item => Image (Date      => Now,
                             Time_Zone => Offset));
         Put (File => Get_File_Access.all,
              Item => " ");
         Circa_Length := Circa_Length + 20;

         if Handle /= SQL then
            Put (File => Get_File_Access.all,
                 Item => "[");
            EIO.Put (File => Get_File_Access.all,
                     Item => Handle);
            Put (File => Get_File_Access.all,
                 Item => "] ");
            Circa_Length := Circa_Length + 3;
         end if;

         Put_Line (File => Get_File_Access.all,
                   Item => Log_String);

         if Config.Get (Immediate_Flush) then
            Flush (File => Get_File_Access.all);
         end if;

         Size := Size + Circa_Length;

      end Write;

   end Log_Object;

end Yolk.Rotating_Log;
