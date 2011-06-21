-------------------------------------------------------------------------------
--                                                                           --
--                                  Yolk                                     --
--                                                                           --
--                           Yolk.Process_Control                            --
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

with Ada.Command_Line;
with Ada.Directories;
with Ada.Interrupts.Names;
with Ada.Text_IO;

package body Yolk.Process_Control is

   use Ada.Command_Line;
   use Ada.Directories;

   type State is (Running, Shutdown, Stopped);

   PID : constant String   :=
           Compose
             (Containing_Directory => Current_Directory,
              Name                 => Simple_Name (Command_Name & ".pid"));
   --  Path to the PID file. Is set to /path/to/executable/<programname>.pid

   Wait_Called : Boolean := False;
   --  Is set to True when Wait is called the first time. This is used to test
   --  if we've already called Wait earlier, and if so, ignore the call.

   procedure Create_PID_File;
   procedure Delete_PID_File;
   --  Create and delete the PID file. This file is per default placed in the
   --  same directory as the server executable itself. This can be changed by
   --  altering the PID constant.

   function getpid return Integer;
   pragma Import (C, getpid);
   function Get_PID return Integer renames getpid;
   --  Return the PID number of the process.

   ------------------
   --  Controller  --
   ------------------

   protected Controller is

      entry Check;
      --  If AWS_State is Shutdown the entry barrier is True then
      --  Delete_PID_File is called and the Wait procedure completes as it is
      --  no longer waiting for Check to complete.

      procedure Handle_Kill;
      --  Set AWS_State to Shutdown.

      pragma Attach_Handler (Handle_Kill, Ada.Interrupts.Names.SIGINT);
      pragma Attach_Handler (Handle_Kill, Ada.Interrupts.Names.SIGTERM);
      pragma Attach_Handler (Handle_Kill, Ada.Interrupts.Names.SIGPWR);
      --  Handles the SIGINT, SIGTERM and SIGPWR signals. These
      --  signalhandlers stops the AWS server and subsequently the entire
      --  server.

      entry Start;
      --  Called by Wait. Set AWS_State to Running and calls Create_PID_File.

   private

      AWS_State : State := Stopped;
      --  What state the AWS server is in.

   end Controller;

   ----------------------
   -- Create_PID_File  --
   ----------------------

   procedure Create_PID_File
   is

      use Ada.Text_IO;

      package IIO is new Ada.Text_IO.Integer_IO (Natural);
      File  : File_Type;

   begin

      if Exists (PID) then
         raise PID_File_Exists with PID;
      end if;

      Create (File => File,
              Name => PID);
      IIO.Put (File => File,
               Item => Get_PID,
               Width => 0);
      Close (File);

   exception
      when Ada.Text_IO.Name_Error |
           Ada.Text_IO.Use_Error |
           Ada.Text_IO.Device_Error =>
         raise Cannot_Create_PID_File with PID;

   end Create_PID_File;

   -----------------------
   --  Delete_PID_File  --
   -----------------------

   procedure Delete_PID_File
   is

      use Ada.Text_IO;

   begin

      if Exists (PID) then
         Delete_File (PID);
      end if;

   exception
      when Ada.Text_IO.Name_Error |
           Ada.Text_IO.Use_Error |
           Ada.Text_IO.Device_Error =>
         raise Cannot_Delete_PID_File with PID;

   end Delete_PID_File;

   ------------
   --  Stop  --
   ------------

   procedure Stop
   is
   begin

      Controller.Handle_Kill;

   end Stop;

   ------------
   --  Wait  --
   ------------

   procedure Wait
   is
   begin

      if not Wait_Called then
         Controller.Start;
         Controller.Check;

         Wait_Called := True;
      end if;

   end Wait;

   ------------------
   --  Controller  --
   ------------------

   protected body Controller is

      -------------
      --  Check  --
      -------------

      entry Check when AWS_State = Shutdown
      is
      begin

         Delete_PID_File;

      end Check;

      -------------
      --  Start  --
      -------------

      entry Start when AWS_State = Stopped
      is
      begin

         AWS_State := Running;
         Create_PID_File;

      end Start;

      -------------------
      --  Handle_Kill  --
      -------------------

      procedure Handle_Kill is
      begin

         if AWS_State /= Shutdown then
            AWS_State := Shutdown;
         end if;

      end Handle_Kill;

   end Controller;

end Yolk.Process_Control;
