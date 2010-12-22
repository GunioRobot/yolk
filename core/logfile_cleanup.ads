-------------------------------------------------------------------------------
--                                                                           --
--                                  Yolk                                     --
--                                                                           --
--                            logfile_cleanup                                --
--                                                                           --
--                                  SPEC                                     --
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

with Ada.Calendar;
with Ada.Containers.Ordered_Sets;
with Ada.Strings.Unbounded;
with AWS.Config;
with AWS.Server;

package Logfile_Cleanup is

   procedure Clean_Up (Config_Object   : in AWS.Config.Object;
                       Web_Server      : in AWS.Server.HTTP);
   --  TODO: Write comment

private

   type File_Info is
      record
         File_Name   : Ada.Strings.Unbounded.Unbounded_String;
         Mod_Time    : Ada.Calendar.Time;
      end record;

   Amount_Of_Files_To_Keep : constant Ada.Containers.Count_Type := 30;

   function "<" (Left, Right : in File_Info) return Boolean;
   --  TODO: Write comment

   package Ordered_File_Set is new Ada.Containers.Ordered_Sets (File_Info);

end Logfile_Cleanup;
