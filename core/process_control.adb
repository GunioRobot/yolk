-------------------------------------------------------------------------------
--                                                                           --
--                                  Yolk                                     --
--                                                                           --
--                              process_control                              --
--                                                                           --
--                                  BODY                                     --
--                                                                           --
--                     Copyright (C) 2010, Thomas Løcke                      --
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

with Ada.Text_IO; use Ada.Text_IO;

package body Process_Control is

   ----------------------
   -- Create_PID_File  --
   ----------------------

   procedure Create_PID_File
   is

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

end Process_Control;
