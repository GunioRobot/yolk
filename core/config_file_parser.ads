-------------------------------------------------------------------------------
--                                                                           --
--                                  Yolk                                     --
--                                                                           --
--                           config_file_parser                              --
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

--  This package provides simple access to ini files.
--  The ini format is:
--       KEY VALUE
--
--  Comments are prefixed with a #:
--
--  # This is a comment
--
--  Blank lines and comments are ignored and so is pre-/postfixed whitespace,
--  so this:
--
--  [lots of whitespace]KEY[lots of whitespace]VALUE[lots of whitespace]
--
--  is treated as:
--
--  KEY VALUE
--
--  Values containing whitespace, eg. full sentences and similar, are returned
--  as is. It is not necessary to quote such values, so this:
--
--    KEY some value with whitespace
--
--  is perfectly valid, and will, when calling Get (KEY), return:
--
--    some value with whitespace
--
--  If VALUE is True or False (case-insensitive), then the KEY can be returned
--  as both a String or as a Boolean.
--  Conversions from VALUE to other types, such as Integer or Float, will raise
--  an exception on failure. It will NOT return some dummy value.

private with Ada.Strings.Unbounded;

generic

   type Keys is (<>);

package Config_File_Parser is

   --  use Ada.Strings.Unbounded;

   Unknown_Ini_Key            : exception;
   --  Is raised when an unknown KEY has been found in the config file.
   Cannot_Create_Ini_File     : exception;
   --  Is raised when the config data cannot be saved to a given file, eg. due
   --  to a bad path or missing user credentials.
   Cannot_Open_Ini_File       : exception;
   --  Is raised when the given config file cannot be opened, eg. due to bad
   --  path or missing user credentials.
   Cannot_Convert_To_Boolean  : exception;
   Cannot_Convert_To_Float    : exception;
   Cannot_Convert_To_Integer  : exception;
   --  These Cannot_Convert_To_xxx exceptions are raised if conversion from the
   --  String VALUE to the target type cannot be done, eg. converting the value
   --  "foo" to Integer or the value "bleh" to Boolean.

   function Get (Key : in Keys) return Boolean;
   function Get (Key : in Keys) return Float;
   function Get (Key : in Keys) return Integer;
   function Get (Key : in Keys) return String;
   function Get (Key : in Keys) return Ada.Strings.Unbounded.Unbounded_String;
   --  Get the VALUE for Key and convert it to target type.
   --  Exceptions:
   --    Cannot_Convert_To_xxx (depends on target type)

   procedure Load_File (Config_File : in String);
   --  Load the config file Config_File.
   --  Exceptions:
   --    Cannot_Open_Ini_File

   procedure Save_Config_File (Config_File : in String);
   --  Save the current config data into the file Config_File. This is NOT an
   --  append operation. If the file exists, it is overwritten. If it does not
   --  exist, it is created.
   --  Exceptions:
   --    Cannot_Create_Ini_File

   procedure Set (Key   : in Keys;
                  Value : in Boolean);
   procedure Set (Key   : in Keys;
                  Value : in Float);
   procedure Set (Key   : in Keys;
                  Value : in Integer);
   procedure Set (Key   : in Keys;
                  Value : in String);
   procedure Set (Key   : in Keys;
                  Value : in Ada.Strings.Unbounded.Unbounded_String);
   --  Set Key to Value. Whitespace (left and right) is trimmed

private

   type Parameters_Array is array (Keys) of
     Ada.Strings.Unbounded.Unbounded_String;
   Parameters : Parameters_Array;

end Config_File_Parser;
