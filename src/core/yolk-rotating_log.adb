-------------------------------------------------------------------------------
--                                                                           --
--                                  Yolk                                     --
--                                                                           --
--                              Rotating Log                                 --
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
with Ada.Directories;
with Ada.Strings;
with Ada.Strings.Fixed;
with Yolk.Process_Control;

package body Yolk.Rotating_Log is

   ------------------
   --  Initialize  --
   ------------------

   procedure Initialize
   is

      use Ada.Directories;
      use Ada.Text_IO;
      use GNATCOLL.Traces;

      A_Handle : Trace_Handles := Trace_Handles'First;
      --  Set A_Handle to Trace_Handles'First to avoid a "'A_Handle' may be
      --  used uninitialized in this function" warning.

   begin

      if Exists (Name => Config.Get (GNATCOLL_Traces_Ini_File)) then
         Register_Rotating_Log_Stream;
         Parse_Config_File (Config.Get (GNATCOLL_Traces_Ini_File));
      end if;

      for Handle in Trace_Handles loop
         A_Handle := Handle;
         Log_Objects_List (Handle) := new Log_Object;
         Log_Objects_List (Handle).Set_File_Access;
         Create
           (File => Log_Objects_List (Handle).Get_File_Access.all,
            Mode => Out_File,
            Name => Config.Get (Log_File_Directory) &
            Config.Get (Server_Name) & "-rotating-" &
            Trace_Handles'Image (Handle) & "-" &
            Log_Objects_List (Handle).Get_Slot & ".log");
      end loop;

   exception
      when Config.Conversion_Error =>
         raise;
      when Config.Empty_Key =>
         raise;
      when others =>
         Process_Control.Stop;
         raise Cannot_Create_Log_File with
           "Handle: " & Trace_Handles'Image (A_Handle) & " and slot: "
           & Log_Objects_List (A_Handle).Get_Slot;

   end Initialize;

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

      Track (Handle     => GNATCOLL_SQL,
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
   --  Track  --
   -------------

   procedure Track
     (Handle       : in Trace_Handles;
      Log_String   : in String)
   is

      use Ada.Calendar;
      use Ada.Calendar.Formatting;
      use Ada.Calendar.Time_Zones;
      use Ada.Text_IO;

      package EIO is new Ada.Text_IO.Enumeration_IO (Trace_Handles);

      Log            : constant Access_Log_Object := Log_Objects_List (Handle);
      Circa_Length   : Natural := Log_String'Length;
      Now            : constant Time := Clock;
      Offset         : constant Time_Offset := UTC_Time_Offset;

   begin

      Log.Seize;

      if Log.Get_Size > Config.Get (Rotating_Log_Size_Limit) then
         if Is_Open (File => Log.Get_File_Access.all) then
            Close (File => Log.Get_File_Access.all);
         end if;

         Log.Move_To_Next_Slot;

         Create
           (File => Log.Get_File_Access.all,
            Mode => Out_File,
            Name => Config.Get (Log_File_Directory) &
            Config.Get (Server_Name) & "-rotating-" &
            Trace_Handles'Image (Handle) & "-" &
            Log.Get_Slot & ".log");
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

      if Config.Get (Immediate_Flush) then
         Flush (File => Log.Get_File_Access.all);
      end if;

      Log.Set_Size (Length => Circa_Length);

      Log.Release;

   exception
      when Config.Conversion_Error =>
         raise;
      when Config.Empty_Key =>
         raise;
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

      -------------
      --  Seize  --
      -------------

      entry Seize when Locked = False
      is
      begin

         Locked := True;

      end Seize;

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

         if Current_Slot = Config.Get (Max_Slot_Count) then
            Current_Slot := 1;
         else
            Current_Slot := Current_Slot + 1;
         end if;

         Size := 0;

      end Move_To_Next_Slot;

      ---------------
      --  Release  --
      ---------------

      procedure Release
      is
      begin

         Locked := False;

      end Release;

      -----------------------
      --  Set_File_Access  --
      -----------------------

      procedure Set_File_Access
      is

         use Ada.Text_IO;

      begin

         File := new File_Type;

      end Set_File_Access;

      ----------------
      --  Set_Size  --
      ----------------

      procedure Set_Size
        (Length : Natural)
      is
      begin

         Size := Size + Length;

      end Set_Size;

      ----------------
      --  Write_To  --
      ----------------

      procedure Write_To
        (Log_String : in String)
      is

         use Ada.Text_IO;

      begin

         Put_Line (File.all, Log_String);

      end Write_To;

   end Log_Object;

begin

   Initialize;

end Yolk.Rotating_Log;
