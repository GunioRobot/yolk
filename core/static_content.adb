-------------------------------------------------------------------------------
--                                                                           --
--                                  Yolk                                     --
--                                                                           --
--                              static_content                               --
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

with Ada.Directories;
with Ada.Streams.Stream_IO;
with AWS.Messages;
with AWS.MIME;
with Configuration;
with Not_Found;
with ZLib;

package body Static_Content is

   procedure Initialize;
   --  Initialize the Static_Content package. Basically just clear out the
   --  compressed cache directory.

   ------------
   --  File  --
   ------------

   function File
     (Request : in AWS.Status.Data)
      return AWS.Response.Data
   is

      use Ada.Directories;
      use AWS.Messages;
      use AWS.Status;
      use Configuration;

      File_Path : constant String := Config.Get (WWW_Root) & URI (Request);
      --  The path to the requested resource.

      procedure Compress_And_Cache;
      --  Compress and cache the requested static file.

      function Compressed_File_Path return String;
      --  Return the path to the .gz compressed file.

      function Is_Compressable
        (Content_Type : in String)
      return Boolean;
      --  Check if a content type is compressable, ie. text.

      --------------------------
      --  Compress_And_Cache  --
      --------------------------

      procedure Compress_And_Cache
      is

         New_File : constant String := Config.Get
           (Compressed_Cache_Directory) & URI (Request) & ".gz";
         Cache_Dir : constant String := Containing_Directory (New_File);

      begin

         if not Exists (Cache_Dir) then
            Create_Path (Cache_Dir);
         end if;

         Compress_File :
         declare

            File_In  : Ada.Streams.Stream_IO.File_Type;
            File_Out : Ada.Streams.Stream_IO.File_Type;
            Filter   : ZLib.Filter_Type;

            procedure Data_Read
              (Item : out Ada.Streams.Stream_Element_Array;
               Last : out Ada.Streams.Stream_Element_Offset);
            --  Read data from File_In.

            procedure Data_Write
              (Item : in Ada.Streams.Stream_Element_Array);
            --  Write data to File_Out.

            procedure Translate is new ZLib.Generic_Translate
              (Data_In  => Data_Read,
               Data_Out => Data_Write);
            --  Do the actual compression. Use Data_Read to read from File_In
            --  and Data_Write to write the compressed content to File_Out.

            -----------------
            --  Data_Read  --
            -----------------

            procedure Data_Read
              (Item : out Ada.Streams.Stream_Element_Array;
               Last : out Ada.Streams.Stream_Element_Offset)
            is
            begin

               Ada.Streams.Stream_IO.Read
                 (File => File_In,
                  Item => Item,
                  Last => Last);

            end Data_Read;

            ----------------
            --  Data_Out  --
            ----------------

            procedure Data_Write
              (Item : in Ada.Streams.Stream_Element_Array)
            is
            begin

               Ada.Streams.Stream_IO.Write (File => File_Out,
                                            Item => Item);

            end Data_Write;

         begin

            Ada.Streams.Stream_IO.Open
              (File => File_In,
               Mode => Ada.Streams.Stream_IO.In_File,
               Name => File_Path);

            Ada.Streams.Stream_IO.Create
              (File => File_Out,
               Mode => Ada.Streams.Stream_IO.Out_File,
               Name => New_File);

            ZLib.Deflate_Init
              (Filter => Filter,
               Level  => ZLib.Best_Compression,
               Header => ZLib.GZip);

            Translate (Filter);

            ZLib.Close (Filter);

            Ada.Streams.Stream_IO.Close (File => File_In);
            Ada.Streams.Stream_IO.Close (File => File_Out);

         end Compress_File;

      end Compress_And_Cache;

      ----------------------------
      --  Compressed_File_Path  --
      ----------------------------

      function Compressed_File_Path return String
      is
      begin

         return Config.Get (Compressed_Cache_Directory)
           & URI (Request) & ".gz";

      end Compressed_File_Path;

      -----------------------
      --  Is_Compressable  --
      -----------------------

      function Is_Compressable
        (Content_Type : in String)
      return Boolean
      is
      begin

         if AWS.MIME.Is_Text (Content_Type) then
            return True;
         end if;

         if Content_Type = "application/javascript"
           or Content_Type = "application/xml"
           or Content_Type = "image/svg+xml"
         then
            return True;
         end if;

         return False;

      end Is_Compressable;

      GZ_File           : constant String := Compressed_File_Path;
      MIME_Type         : constant String := AWS.MIME.Content_Type (File_Path);
      Minimum_File_Size : constant File_Size
        := File_Size (Integer'(Config.Get (Compress_Minimum_File_Size)));

   begin

      if not Exists (File_Path)
        or else Kind (File_Path) /= Ordinary_File
      then
         return Not_Found.Output (Request);
      end if;

      if Exists (GZ_File)
        and then Kind (GZ_File) = Ordinary_File
      then
         return AWS.Response.File
           (Content_Type  => MIME_Type,
            Filename      => GZ_File,
            Encoding      => GZip);
      end if;

      if Config.Get (Compress_Static_Content)
        and then Is_Compressable (MIME_Type)
        and then Is_Supported (Request, GZip)
        and then Size (File_Path) > Minimum_File_Size
      then
         Compress_And_Cache;

         return AWS.Response.File
           (Content_Type  => MIME_Type,
            Filename      => GZ_File,
            Encoding      => GZip);
      end if;

      return AWS.Response.File
        (Content_Type  => MIME_Type,
         Filename      => File_Path);

   end File;

   ------------------
   --  Initialize  --
   ------------------

   procedure Initialize
   is

      use Ada.Directories;
      use Configuration;

      procedure Delete_Item
        (Search_Item : in Directory_Entry_Type);
      --  Delete files and directories in the Compressed_Cache_Directory.

      -------------------
      --  Delete_Item  --
      -------------------

      procedure Delete_Item
        (Search_Item : in Directory_Entry_Type)
      is

         Name : constant String := Simple_Name
           (Directory_Entry => Search_Item);
         Full : constant String := Full_Name (Directory_Entry => Search_Item);

      begin

         if Kind (Search_Item) = Directory
           and then Name /= ".."
           and then Name /= "."
         then
            Delete_Tree (Directory => Full);
         end if;

         if Kind (Search_Item) = Ordinary_File
           and then Name /= ".."
           and then Name /= "."
           and then Name /= ".gitignore"
         then
            Delete_File (Name => Full);
         end if;

      end Delete_Item;

      Filter : constant Filter_Type := (Ordinary_File => True,
                                        Special_File  => False,
                                        Directory     => True);

   begin

      Search (Directory => Config.Get (Compressed_Cache_Directory),
              Pattern => "",
              Filter => Filter,
              Process => Delete_Item'Access);

   end Initialize;

begin

   Initialize;

end Static_Content;
