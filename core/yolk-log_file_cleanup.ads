-------------------------------------------------------------------------------
--                                                                           --
--                                  Yolk                                     --
--                                                                           --
--                            Logfile Cleanup                                --
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

with Ada.Calendar;
with Ada.Containers.Ordered_Sets;
with Ada.Strings.Unbounded;
with AWS.Config;
with AWS.Server;

package Yolk.Log_File_Cleanup is

   procedure Clean_Up
     (Config_Object           : in AWS.Config.Object;
      Web_Server              : in AWS.Server.HTTP;
      Amount_Of_Files_To_Keep : in Positive);
   --  Search for and delete old and excess logfiles in the Log_File_Directory
   --  defined in the server configuration file.

private

   use Ada.Strings.Unbounded;

   type File_Info is
      record
         File_Name   : Unbounded_String;
         Mod_Time    : Ada.Calendar.Time;
      end record;

   function "<"
     (Left, Right : in File_Info)
      return Boolean;
   --  Used by the Ordered_File_Set package to order the File_Info elements.

   package Ordered_File_Set is new Ada.Containers.Ordered_Sets (File_Info);
   --  A new ordered set package instantiated with File_Info as Element_Type.

end Yolk.Log_File_Cleanup;
