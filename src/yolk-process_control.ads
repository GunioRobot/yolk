
-------------------------------------------------------------------------------
--                                                                           --
--                                  Yolk                                     --
--                                                                           --
--                           Yolk.Process_Control                            --
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
--  SIGINT, SIGPWR and SIGTERM signals.
--  It is also this package that is responsible for creating the PID file,
--  which by default is always placed in the same directory as the executable.
--  Change the PID constant if you want/need it placed elsewhere.

package Yolk.Process_Control is

   pragma Unreserve_All_Interrupts;
   --  Make sure that GNAT does not handle SIGINT interrupts automatically.
   --  Check your compiler for reserved signals.

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
   --  replacement for a loop in the main program file.
   --  Wait completes when either Stop is called or the application receives
   --  one of the SIGINT, SIGPWR or SIGTERM signals. After this, wait can be
   --  called again.
   --  Calling Wait multiple times in a row does nothing. Calls proceeding the
   --  first call are ignored.

end Yolk.Process_Control;
