-------------------------------------------------------------------------------
--                                                                           --
--                                  Yolk                                     --
--                                                                           --
--                           config_file_parser                              --
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

with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Text_IO;

package body Config_File_Parser is

   -----------
   --  Get  --
   -----------

   function Get (Key : in Keys) return Boolean
   is

      use Ada.Strings.Unbounded;

   begin

      return Boolean'Value (To_String (Parameters (Key)));

   exception
      when Constraint_Error =>
         raise Cannot_Convert_To_Boolean with Keys'Image (Key);

   end Get;

   -----------
   --  Get  --
   -----------

   function Get (Key : in Keys) return Float
   is

      use Ada.Strings.Unbounded;

   begin

      return Float'Value (To_String (Parameters (Key)));

   exception
      when Constraint_Error =>
         raise Cannot_Convert_To_Float with Keys'Image (Key);

   end Get;

   -----------
   --  Get  --
   -----------

   function Get (Key : in Keys) return Integer
   is

      use Ada.Strings.Unbounded;

   begin

      return Integer'Value (To_String (Parameters (Key)));

   exception
      when Constraint_Error =>
         raise Cannot_Convert_To_Integer with Keys'Image (Key);

   end Get;

   -----------
   --  Get  --
   -----------

   function Get (Key : in Keys) return String
   is

      use Ada.Strings.Unbounded;

   begin

      return To_String (Parameters (Key));

   end Get;

   -----------
   --  Get  --
   -----------

   function Get (Key : in Keys) return Ada.Strings.Unbounded.Unbounded_String
   is

      use Ada.Strings.Unbounded;

   begin

      return Parameters (Key);

   end Get;

   -----------------
   --  Load_File  --
   -----------------

   procedure Load_File (Config_File : in String)
   is

      use Ada.Strings;
      use Ada.Strings.Unbounded;
      use Ada.Text_IO;

      function Key_String (Line : in String) return String;
      function Value_UString (Key   : in String;
                              Line  : in String) return Unbounded_String;

      ------------------
      --  Key_String  --
      ------------------

      function Key_String (Line : in String) return String
      is

         Key_End : Natural;

      begin

         if Line /= "" then
            Key_End := Fixed.Index (Source => Line,
                              Set    => Maps.To_Set (Space),
                              Going  => Forward);
            if Key_End > Line'First then
               return Line (Line'First .. Key_End - 1);
            end if;
         end if;

         return Line;

      end Key_String;

      ---------------------
      --  Value_UString  --
      ---------------------

      function Value_UString (Key   : in String;
                              Line  : in String) return Unbounded_String
      is
      begin

         if Line /= "" and then Key /= Line then
            return To_Unbounded_String (Line (Key'Last + 2 .. Line'Last));
         end if;

         return To_Unbounded_String ("");

      end Value_UString;

      File : File_Type;

   begin

      Open (File => File,
            Mode => In_File,
            Name => Config_File);

      while not End_Of_File (File => File) loop
         declare

            Line     : constant String := Fixed.Trim (Get_Line (File), Both);
            Key      : constant String := Key_String (Line);
            Value    : constant Unbounded_String := Value_UString (Key, Line);

         begin

            --  Ignore empty lines and comments.
            if Line /= "" and then Line (1 .. 1) /= "#" then
               Parameters (Keys'Value (Key)) := Trim (Value, Left);
            end if;

         exception
            when Constraint_Error =>
               raise Unknown_Ini_Key with
                 "Unknown ini key '" & Key & "' in file " & Config_File;

         end;
      end loop;

      Close (File => File);

   exception
      when Name_Error | Use_Error | Device_Error =>
         raise Cannot_Open_Ini_File with Config_File;

   end Load_File;

   ------------------------
   --  Save_Config_File  --
   ------------------------

   procedure Save_Config_File (Config_File : in String)
   is

      use Ada.Strings;
      use Ada.Strings.Unbounded;
      use Ada.Text_IO;

      package Enum_IO is new Ada.Text_IO.Enumeration_IO (Keys);

      File : File_Type;

   begin

      Create (File => File,
              Name => Config_File);

      for Key in Keys loop
         Enum_IO.Put (File, Key);
         Put (File, Space);
         Put (File, To_String (Parameters (Key)));
         New_Line (File);
      end loop;

      Close (File => File);

   exception
      when Name_Error | Use_Error | Device_Error =>
         raise Cannot_Create_Ini_File with Config_File;

   end Save_Config_File;

   -----------
   --  Set  --
   -----------

   procedure Set (Key   : in Keys;
                  Value : in Boolean)
   is

      use Ada.Strings.Unbounded;

   begin

      Parameters (Key) := To_Unbounded_String (Boolean'Image (Value));

   end Set;

   -----------
   --  Set  --
   -----------

   procedure Set (Key   : in Keys;
                  Value : in Float)
   is

      use Ada.Strings;
      use Ada.Strings.Unbounded;

   begin

      Parameters (Key) := To_Unbounded_String
        (Fixed.Trim (Float'Image (Value), Left));

   end Set;

   -----------
   --  Set  --
   -----------

   procedure Set (Key   : in Keys;
                  Value : in Integer)
   is

      use Ada.Strings;
      use Ada.Strings.Unbounded;

   begin

      Parameters (Key) := To_Unbounded_String
        (Fixed.Trim (Integer'Image (Value), Left));

   end Set;

   -----------
   --  Set  --
   -----------

   procedure Set (Key   : in Keys;
                  Value : in String)
   is

      use Ada.Strings.Unbounded;

   begin

      Parameters (Key) := To_Unbounded_String (Value);

   end Set;

   -----------
   --  Set  --
   -----------

   procedure Set (Key   : in Keys;
                  Value : in Ada.Strings.Unbounded.Unbounded_String)
   is
   begin

      Parameters (Key) := Value;

   end Set;

end Config_File_Parser;
