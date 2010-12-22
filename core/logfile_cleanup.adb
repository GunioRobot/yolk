
with Ada.Directories;
with AWS.Server.Log;
with Rotating_Log;
with Utilities;

package body Logfile_Cleanup is

   ---------
   --  <  --
   ---------

   function "<" (Left, Right : in File_Info) return Boolean
   is

      use Ada.Calendar;

   begin

      if Left.Mod_Time /= Right.Mod_Time then
         return Left.Mod_Time > Right.Mod_Time;
         --  Put the newest files at the top of the set.
      else
         return True;
         --  Modification time is the same for both files. Just return True
         --  and let Left be < then right. The only log files that will have
         --  matching modification time are the two currently running AWS
         --  log files.
      end if;

   end "<";

   ----------------
   --  Clean_Up  --
   ----------------

   procedure Clean_Up (Config_Object   : in AWS.Config.Object;
                    Web_Server      : in AWS.Server.HTTP)
   is

      use Ada.Calendar;
      use Ada.Containers;
      use Ada.Directories;
      use Ada.Strings.Unbounded;
      use Ordered_File_Set;
      use Rotating_Log;
      use Utilities;

      File_Set : Ordered_File_Set.Set;
      Filter : constant Filter_Type := (Ordinary_File => True,
                                        Special_File  => False,
                                        Directory     => False);

      procedure Add_File_To_Set (Search_Item : in Directory_Entry_Type);
      procedure Do_It (Prefix : in String;
                       Kind   : in String);

      procedure Add_File_To_Set (Search_Item : in Directory_Entry_Type)
      is

         A_File : File_Info;
         Found_File : constant String :=
                        Full_Name (Directory_Entry => Search_Item);
         Current_Log_File : constant String :=
                              AWS.Server.Log.Name (Web_Server);
         Current_Error_Log_File : constant String :=
                                    AWS.Server.Log.Error_Name (Web_Server);

      begin

         if Found_File /= Current_Log_File and
           Found_File /= Current_Error_Log_File
         then
            A_File.File_Name := TUS
              (Full_Name (Directory_Entry => Search_Item));
            A_File.Mod_Time := Modification_Time
              (Directory_Entry => Search_Item);

            File_Set.Insert (New_Item => A_File);
         end if;

      end Add_File_To_Set;

      procedure Do_It (Prefix : in String;
                       Kind   : in String)
      is
      begin

         Track (Handle     => Info,
                Log_String => "Searching for old " & Kind & " files.");

         Search (Directory => AWS.Config.Log_File_Directory (Config_Object),
                 Pattern   => Prefix & "*",
                 Filter    => Filter,
                 Process   => Add_File_To_Set'Access);

         if File_Set.Length > Amount_Of_Files_To_Keep then
            loop
               exit when File_Set.Length = Amount_Of_Files_To_Keep;

               declare
               begin
                  Delete_File
                    (Name => To_String (File_Set.Last_Element.File_Name));
                  --  TODO: Write comment
               exception
                  when others =>
                     Track
                       (Handle     => Error,
                        Log_String => "Cannot delete file " & To_String
                          (File_Set.Last_Element.File_Name));
               end;

               File_Set.Delete_Last;

               Track
                 (Handle     => Info,
                  Log_String =>
                    "Deleted " & To_String (File_Set.Last_Element.File_Name));
            end loop;
         else
            Track (Handle     => Info,
                   Log_String => "No " & Kind & " files deleted.");
         end if;

      end Do_It;

   begin

      Do_It (Prefix => AWS.Config.Log_Filename_Prefix (Config_Object),
             Kind   => "Access Log");

      File_Set.Clear;

      Do_It (Prefix => AWS.Config.Error_Log_Filename_Prefix (Config_Object),
             Kind   => "Error Log");

   end Clean_Up;

end Logfile_Cleanup;
