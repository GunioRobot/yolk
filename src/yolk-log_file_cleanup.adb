-------------------------------------------------------------------------------
--                                                                           --
--                                  Yolk                                     --
--                                                                           --
--                          Yolk.Logfile_Cleanup                             --
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
with Ada.Containers.Ordered_Sets;
with Ada.Directories;
with Ada.Strings.Unbounded;
with AWS.Server.Log;
with Yolk.Rotating_Log;
with Yolk.Utilities;

package body Yolk.Log_File_Cleanup is

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

   ---------
   --  <  --
   ---------

   function "<"
     (Left, Right : in File_Info)
      return Boolean
   is

      use Ada.Calendar;

   begin

      if Left.Mod_Time /= Right.Mod_Time then
         return Left.Mod_Time > Right.Mod_Time;
         --  Put the newest files at the top of the set.
      else
         return True;
         --  If modification time is the same for both files, just return True
         --  and let Left be < then right. The only log files that will have
         --  matching modification time are the two currently running AWS
         --  log files. I hope.
      end if;

   end "<";

   --------------
   --  Delete  --
   --------------

   procedure Delete
     (Config_Object            : in AWS.Config.Object;
      Web_Server               : in AWS.Server.HTTP;
      Amount_Of_Files_To_Keep  : in Positive)
   is

      use Ada.Containers;
      use Ada.Directories;

      Keep : constant Count_Type := Count_Type (Amount_Of_Files_To_Keep);

      File_Set : Ordered_File_Set.Set;
      --  Our File_Info container.

      -----------------------
      --  Add_File_To_Set  --
      -----------------------

      procedure Add_File_To_Set
        (Search_Item : in Directory_Entry_Type);
      --  Add found files to the File_Set container. This procedure is called
      --  by the Ada.Directories.Search procedure in Do_It.

      procedure Add_File_To_Set
        (Search_Item : in Directory_Entry_Type)
      is

         use Ada.Calendar;
         use Yolk.Utilities;

         A_File                  : File_Info;
         Found_File              : constant String :=
                                     Full_Name
                                       (Directory_Entry => Search_Item);
         Current_Log_File        : constant String :=
                                     AWS.Server.Log.Name (Web_Server);
         Current_Error_Log_File  : constant String :=
                                     AWS.Server.Log.Error_Name (Web_Server);

      begin

         if Found_File /= Current_Log_File and
           Found_File /= Current_Error_Log_File
         then
            A_File.File_Name :=
              TUS (Full_Name (Directory_Entry => Search_Item));
            A_File.Mod_Time :=
              Modification_Time (Directory_Entry => Search_Item);

            File_Set.Insert (New_Item => A_File);
         end if;

      end Add_File_To_Set;

      -------------
      --  Do_It  --
      -------------

      procedure Do_It
        (Prefix : in String;
         Kind   : in String);
      --  Find and delete logfiles.
      --  A search is done in the directory where the logfiles are kept, and if
      --  more than Amount_Of_Files_To_Keep logfiles are found, then delete the
      --  oldest.

      procedure Do_It
        (Prefix : in String;
         Kind   : in String)
      is

         use Yolk.Rotating_Log;

         Filter : constant Filter_Type := (Ordinary_File => True,
                                           Special_File  => False,
                                           Directory     => False);

      begin

         Trace (Handle     => Info,
                Log_String => "Searching for old " & Kind & " files.");

         Search (Directory => AWS.Config.Log_File_Directory (Config_Object),
                 Pattern   => Prefix & "*",
                 Filter    => Filter,
                 Process   => Add_File_To_Set'Access);

         if File_Set.Length > Keep then
            loop
               exit when File_Set.Length = Keep;

               Delete_File_Block :
               declare
               begin

                  Delete_File
                    (Name => To_String (File_Set.Last_Element.File_Name));
                  --  Delete the actual file.

               exception
                  when others =>
                     Trace
                       (Handle     => Error,
                        Log_String => "Cannot delete file " & To_String
                          (File_Set.Last_Element.File_Name));

               end Delete_File_Block;

               File_Set.Delete_Last;
               --  Delete the last File_Info element from the set.

               Trace
                 (Handle     => Info,
                  Log_String =>
                    "Deleted " & To_String (File_Set.Last_Element.File_Name));
            end loop;
         else
            Trace (Handle     => Info,
                   Log_String => "No " & Kind & " files deleted.");
         end if;

      end Do_It;

   begin

      Do_It (Prefix => AWS.Config.Log_Filename_Prefix (Config_Object),
             Kind   => "Access Log");

      File_Set.Clear;

      Do_It (Prefix => AWS.Config.Error_Log_Filename_Prefix (Config_Object),
             Kind   => "Error Log");

   end Delete;

end Yolk.Log_File_Cleanup;
