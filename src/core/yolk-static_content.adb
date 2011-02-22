-------------------------------------------------------------------------------
--                                                                           --
--                                  Yolk                                     --
--                                                                           --
--                           Yolk.Static_Content                             --
--                                                                           --
--                                  BODY                                     --
--                                                                           --
--                   Copyright (C) 2010-2011, Thomas L�cke                   --
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
with ZLib;
with Yolk.Configuration;
with Yolk.Not_Found;
with Yolk.Rotating_Log;

package body Yolk.Static_Content is

   procedure Initialize;
   --  Initialize the Static_Content package. Basically just delete and
   --  re-create the Compressed_Cache_Directory.'

   -------------------
   --  Binary_File  --
   -------------------

   function Binary_File
     (Request : in AWS.Status.Data)
      return AWS.Response.Data
   is

      use Ada.Directories;
      use AWS.MIME;
      use AWS.Status;
      use Yolk.Configuration;

      Resource : constant String := Config.Get (WWW_Root) & URI (Request);
      --  The path to the requested resource.

   begin

      if not Exists (Resource)
        or else Kind (Resource) /= Ordinary_File
      then
         return Yolk.Not_Found.Generate (Request);
      end if;

      return AWS.Response.File
        (Content_Type  => Content_Type (Resource),
         Filename      => Resource);

   end Binary_File;

   ------------------
   --  Initialize  --
   ------------------

   procedure Initialize
   is

      use Ada.Directories;
      use Yolk.Rotating_Log;
      use Yolk.Configuration;

   begin

      if Exists (Config.Get (Compressed_Cache_Directory))
        and then Kind (Config.Get (Compressed_Cache_Directory)) = Directory
      then
         Delete_Tree (Directory => Config.Get (Compressed_Cache_Directory));
         Track
           (Handle     => Info,
            Log_String => Config.Get (Compressed_Cache_Directory)
            & " deleted by Yolk.Static_Content.Initialize");
      end if;

      Create_Path
           (New_Directory => Config.Get (Compressed_Cache_Directory));
      Track
        (Handle     => Info,
         Log_String => Config.Get (Compressed_Cache_Directory)
         & " created by Yolk.Static_Content.Initialize");

   end Initialize;

   -----------------
   --  Text_File  --
   -----------------

   function Text_File
     (Request : in AWS.Status.Data)
      return AWS.Response.Data
   is

      use Ada.Directories;
      use AWS.Messages;
      use AWS.Status;
      use Yolk.Rotating_Log;
      use Yolk.Configuration;

      GZ_Resource           : constant String :=
                                Config.Get (Compressed_Cache_Directory)
                                & URI (Request) & ".gz";
      --  The path to the GZipped resourc.
      Resource : constant String := Config.Get (WWW_Root) & URI (Request);
      --  The path to the requested resource.
      MIME_Type         : constant String := AWS.MIME.Content_Type (Resource);
      Minimum_File_Size : constant File_Size :=
                            File_Size (Integer'(Config.Get
                              (Compress_Minimum_File_Size)));

      procedure Compress_And_Cache;
      --  Compress and cache the requested static file.

      --------------------------
      --  Compress_And_Cache  --
      --------------------------

      procedure Compress_And_Cache
      is

         Cache_Dir : constant String := Containing_Directory (GZ_Resource);

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
               Name => Resource);

            Ada.Streams.Stream_IO.Create
              (File => File_Out,
               Mode => Ada.Streams.Stream_IO.Out_File,
               Name => GZ_Resource);

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

   begin

      if not Exists (Resource)
        or else Kind (Resource) /= Ordinary_File
      then
         return Yolk.Not_Found.Generate (Request);
      end if;

      if Is_Supported (Request, GZip) then
         if Exists (GZ_Resource)
           and then Kind (GZ_Resource) = Ordinary_File
         then
            return AWS.Response.File
              (Content_Type  => MIME_Type,
               Filename      => GZ_Resource,
               Encoding      => GZip);
         elsif Exists (GZ_Resource)
           and then Kind (GZ_Resource) /= Ordinary_File
         then
            --  Not so good. Log to ERROR track and return un-compressed
            --  content.
            Track
              (Handle     => Error,
               Log_String => GZ_Resource
               & " exists and is not an ordinary file");

            return AWS.Response.File
              (Content_Type  => MIME_Type,
               Filename      => Resource);
         end if;

         if Config.Get (Compress_Static_Content)
           and then Size (Resource) > Minimum_File_Size
         then
            Lock.Seize;
            Compress_And_Cache;
            Lock.Release;

            return AWS.Response.File
              (Content_Type  => MIME_Type,
               Filename      => GZ_Resource,
               Encoding      => GZip);
         end if;
      end if;

      return AWS.Response.File
        (Content_Type  => MIME_Type,
         Filename      => Resource);

   end Text_File;

begin

   Initialize;

end Yolk.Static_Content;
