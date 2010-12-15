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

   -------------------------
   --  Check_And_Convert  --
   -------------------------

   function Check_And_Convert (Key : in Keys) return String
   is

      use Ada.Strings.Unbounded;

   begin

      if Defaults (Key) = Null_Unbounded_String then
         raise Empty_Key with Keys'Image (Key);
      end if;

      return To_String (Defaults (Key));

   end Check_And_Convert;

   -----------
   --  Get  --
   -----------

   function Get (Key : in Keys) return Boolean
   is
   begin

      return Boolean'Value (Check_And_Convert (Key));

   exception
      when Constraint_Error =>
         raise Conversion_Error with Keys'Image (Key);

   end Get;

   -----------
   --  Get  --
   -----------

   function Get (Key : in Keys) return Float
   is
   begin

      return Float'Value (Check_And_Convert (Key));

   exception
      when Constraint_Error =>
         raise Conversion_Error with Keys'Image (Key);

   end Get;

   -----------
   --  Get  --
   -----------

   function Get (Key : in Keys) return Integer
   is
   begin

      return Integer'Value (Check_And_Convert (Key));

   exception
      when Constraint_Error =>
         raise Conversion_Error with Keys'Image (Key);

   end Get;

   -----------
   --  Get  --
   -----------

   function Get (Key : in Keys) return String
   is

      use Ada.Strings.Unbounded;

   begin

      return To_String (Defaults (Key));

   end Get;

   -----------
   --  Get  --
   -----------

   function Get (Key : in Keys)
                 return Ada.Strings.Unbounded.Unbounded_String
   is
   begin

      return Defaults (Key);

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

            --  Ignore empty lines, comments and empty values.
            if Line /= ""
              and then Line (1 .. 1) /= "#"
              and then Line (1 .. 2) /= "--"
              and then Value /= "" then
               Defaults (Keys'Value (Key)) := Trim (Value, Left);
            end if;

         exception
            when Constraint_Error =>
               raise Unknown_Key with
                 "Unknown configuration key '" & Key & "' in file "
                   & Config_File;

         end;
      end loop;

      Close (File => File);

   exception
      when Name_Error | Use_Error | Device_Error =>
         raise Cannot_Open_Config_File with Config_File;

   end Load_File;

begin

   Load_File (Config_File => Config_File);

end Config_File_Parser;
