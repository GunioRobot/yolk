-------------------------------------------------------------------------------
--                                                                           --
--                                  Yolk                                     --
--                                                                           --
--                              rotating_log                                 --
--                                                                           --
--                                  BODY                                     --
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

with Ada.Calendar;            use Ada.Calendar;
with Ada.Calendar.Formatting; use Ada.Calendar.Formatting;
with Ada.Calendar.Time_Zones; use Ada.Calendar.Time_Zones;
with Ada.Directories;         use Ada.Directories;
with Ada.Strings;             use Ada.Strings;
with Ada.Strings.Fixed;       use Ada.Strings.Fixed;
with Process_Control;

package body Rotating_Log is

   ------------------
   --  Initialize  --
   ------------------

   procedure Initialize
   is

      A_Handle : Trace_Handles := Trace_Handles'First;

   begin

      if Exists (Name => Get (GNATCOLL_Traces_Ini_File)) then
         Register_Rotating_Log_Stream;
         Parse_Config_File (Get (GNATCOLL_Traces_Ini_File));
      end if;

      for Handle in Trace_Handles loop
         A_Handle := Handle;
         Log_Objects_List (Handle) := new Log_Object;
         Log_Objects_List (Handle).Set_File_Access;
         Create (File => Log_Objects_List (Handle).Get_File_Access.all,
                 Mode => Out_File,
                 Name =>
                   Get (Log_File_Path) & Trace_Handles'Image (Handle) & "-" &
                 Log_Objects_List (Handle).Get_Slot & ".log");
      end loop;

   exception
      when others =>
         Process_Control.Stop;
         raise Cannot_Create_Log_File with
           "Handle: " & Trace_Handles'Image (A_Handle) & " and slot: "
           & Log_Objects_List (A_Handle).Get_Slot;

   end Initialize;

   ------------------
   --  New_Stream  --
   ------------------

   function New_Stream (Fact : in Factory;
                        Args : in String) return Trace_Stream
   is

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

   procedure Newline (Stream : in out Rotating_Log_Record)
   is
   begin

      Track (Handle     => GNATCOLL_SQL,
             Log_String => To_String (Stream.Buffer));
      Stream.Buffer := Null_Unbounded_String;

   end Newline;

   -----------
   --  Put  --
   -----------

   overriding
   procedure Put (Stream      : in out Rotating_Log_Record;
                  Log_String  : in     String)
   is
   begin

      Append (Stream.Buffer, Log_String);

   end Put;

   ------------------------------------
   --  Register_Rotating_Log_Stream  --
   ------------------------------------

   procedure Register_Rotating_Log_Stream
   is

      Fact : constant Stream_Factory_Access := new Factory;

   begin

      Register_Stream_Factory (Stream_Rotating_Log, Fact);

   end Register_Rotating_Log_Stream;

   ----------------------
   --  Supports_Color  --
   ----------------------

   function Supports_Color (Stream : Rotating_Log_Record) return Boolean
   is

      pragma Unreferenced (Stream);

   begin

      return False;

   end Supports_Color;

   ---------------------
   --  Supports_Time  --
   ---------------------

   function Supports_Time (Stream : Rotating_Log_Record) return Boolean
   is

      pragma Unreferenced (Stream);

   begin

      return False;

   end Supports_Time;

   -------------
   --  Track  --
   -------------

   procedure Track (Handle       : in Trace_Handles;
                    Log_String   : in String)
   is

      package EIO is new Ada.Text_IO.Enumeration_IO (Trace_Handles);
      Log            : constant Access_Log_Object := Log_Objects_List (Handle);
      Circa_Length   : Natural := Log_String'Length;
      Now            : constant Time := Clock;
      Offset         : constant Time_Offset := UTC_Time_Offset;

   begin

      if Log.Get_Size > Get (Max_Logged_Characters) then
         if Is_Open (File => Log.Get_File_Access.all) then
            Close (File => Log.Get_File_Access.all);
         end if;

         Log.Move_To_Next_Slot;

         Create (File => Log.Get_File_Access.all,
                 Mode => Out_File,
                 Name => Get (Log_File_Path) & Trace_Handles'Image (Handle) &
                 "-" & Log.Get_Slot & ".log");
      end if;

      Put (File => Log.Get_File_Access.all,
           Item => Image (Date      => Now,
                          Time_Zone => Offset));
      Put (File => Log.Get_File_Access.all,
           Item => " ");
      Circa_Length := Circa_Length + 20;

      if Handle /= GNATCOLL_SQL then
         Put (File => Log.Get_File_Access.all,
              Item => "[");
         EIO.Put (File => Log.Get_File_Access.all,
                  Item => Handle);
         Put (File => Log.Get_File_Access.all,
              Item => "] ");
         Circa_Length := Circa_Length + 3;
      end if;
      Put_Line (File => Log.Get_File_Access.all,
                Item => Log_String);

      if Get (Immediate_Flush) then
         Flush (File => Log.Get_File_Access.all);
      end if;

      Log.Set_Size (Length => Circa_Length);

   exception
      when others =>
         Process_Control.Stop;
         raise Cannot_Write_To_Log_File with
           "Handle: " & Trace_Handles'Image (Handle) & " and slot: "
           & Log.Get_Slot;

   end Track;

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
      --  Get_Size  --
      ----------------

      function Get_Size return Natural
      is
      begin

         return Size;

      end Get_Size;

      ----------------
      --  Get_Slot  --
      ----------------

      function Get_Slot return String
      is
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

         if Current_Slot = Get (Max_Slot_Count) then
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
      begin

         File := new File_Type;

      end Set_File_Access;

      ----------------
      --  Set_Size  --
      ----------------

      procedure Set_Size (Length : Natural)
      is
      begin

         Size := Size + Length;

      end Set_Size;

      ----------------
      --  Write_To  --
      ----------------

      procedure Write_To (Log_String : in String)
      is
      begin

         Put_Line (File.all, Log_String);

      end Write_To;

   end Log_Object;

begin

   Initialize;
   --  Start the logging system.

end Rotating_Log;
