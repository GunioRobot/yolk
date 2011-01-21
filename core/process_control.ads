
-------------------------------------------------------------------------------
--                                                                           --
--                                  Yolk                                     --
--                                                                           --
--                             process_control                               --
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

--  The Process_Control package enables us to stop the server with the
--  SIGHUP, SIGINT, SIGTERM and SIGPWR signals.
--  It is also this package that is responsible for creating the PID file,
--  which by default is always placed in the same directory as the executable.
--  Change the PID constant if you want/need it placed elsewhere.

with Ada.Command_Line;
with Ada.Directories;
with Ada.Interrupts.Names;

package Process_Control is

   pragma Unreserve_All_Interrupts;
   --  Make sure that GNAT does not handle any interrupts automatically.

   Cannot_Create_PID_File  : exception;
   --  Is raised if the PID file cannot be created, eg. the server lacks
   --  permissions to write to the current directory.
   Cannot_Delete_PID_File  : exception;
   --  Is raised if the PID file cannot be deleted, eg. the server lacks
   --  permissions to write to the current directory or to the PID file itself.
   PID_File_Exists         : exception;
   --  Is raised when the PID file already exists, ie. the server is already
   --  running, or it was shutdown incorrectly.

   procedure Stop;
   --  Shutdown the server.

   procedure Wait;
   --  Wait until either Stop or Controller.Handle_Kill is called. This
   --  procedure is basically what keeps the application running. It's a
   --  replacement for a loop in the main program file. It should only ever be
   --  called once, so when it's called the first time, the private Wait_Called
   --  variable is set to True. Further calls to Wait are ignored.

private

   type State is (Running, Shutdown, Stopped);

   PID : constant String   :=
           Ada.Directories.Compose
             (Containing_Directory => Ada.Directories.Current_Directory,
              Name                 => Ada.Directories.Simple_Name
                (Ada.Command_Line.Command_Name & ".pid"));
   --  Path to the PID file. Is set to /path/to/executable/<programname>.pid
   Wait_Called             : Boolean := False;
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
      --  Handles the SIGINT, SIGTERM and SIGPWR signals. This
      --  signalhandler stops the AWS server and subsequently the entire
      --  server.

      entry Start;
      --  Called by Wait. Set AWS_State to Running and calls Create_PID_File.

   private

      AWS_State : State := Stopped;
      --  What state the AWS server is in.

   end Controller;

end Process_Control;
