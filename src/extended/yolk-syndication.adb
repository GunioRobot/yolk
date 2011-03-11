-------------------------------------------------------------------------------
--                                                                           --
--                                  Yolk                                     --
--                                                                           --
--                             Yolk.Syndication                              --
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

with Ada.Calendar.Formatting;
with Ada.Text_IO;
with Yolk.Utilities;

package body Yolk.Syndication is

   function Atom_Date_Image
     (Time_Stamp : in Ada.Calendar.Time)
      return String;
   --  Return a string representation of the Time_Stamp time. The format is:
   --    yyyy-mm-ddThh:mm:ssZ
   --  The uppercase T and Z are requried as per the Atom specification.
   --  It is expected that the Time_Stamp is GMT.

   ------------------
   --  Add_Author  --
   ------------------

   procedure Add_Author
     (Feed     : in out Atom_Feed;
      Name     : in     String;
      Language : in     String := None;
      Email    : in     String := None;
      URI      : in     String := None)
   is

      use Yolk.Utilities;

   begin

      Feed.Author_List.Append
        (Atom_Person'(Name      => TUS (Name),
                      Language  => TUS (Language),
                      Email     => TUS (Email),
                      URI       => TUS (URI)));

   end Add_Author;

   -----------------------
   --  Add_Contributor  --
   -----------------------

   procedure Add_Contributor
     (Feed     : in out Atom_Feed;
      Name     : in     String;
      Language : in     String := None;
      Email    : in     String := None;
      URI      : in     String := None)
   is

      use Yolk.Utilities;

   begin

      Feed.Contributor_List.Append
        (Atom_Person'(Name      => TUS (Name),
                      Language  => TUS (Language),
                      Email     => TUS (Email),
                      URI       => TUS (URI)));

   end Add_Contributor;

   -----------------------
   --  Atom_Date_Image  --
   -----------------------

   function Atom_Date_Image
     (Time_Stamp : in Ada.Calendar.Time)
      return String
   is

      use Ada.Calendar;
      use Ada.Calendar.Formatting;

      Atom_Time : String (1 .. 20);

   begin

      Atom_Time (1 .. 19) := Image (Date                  => Time_Stamp,
                                    Include_Time_Fraction => False);
      Atom_Time (11) := 'T';
      Atom_Time (20) := 'Z';

      return Atom_Time;

   end Atom_Date_Image;

   ------------------
   --  Initialize  --
   ------------------

   function Initialize
     (Base_URI   : in String := None;
      Id         : in String;
      Language   : in String := None;
      Title      : in String;
      Title_Type : in Content_Type := Text)
      return Atom_Feed
   is

      use Ada.Calendar;
      use Ada.Text_IO;
      use Yolk.Utilities;

      Feed : Atom_Feed;

   begin

      Put_Line (Id);
      Put_Line (Title);
      Put_Line (Atom_Date_Image (Clock));

      Feed.Base_URI     := TUS (Base_URI);
      Feed.Id           := TUS (Id);
      Feed.Language     := TUS (Language);
      Feed.Title        := TUS (Title);
      Feed.Title_Type   := Title_Type;
      Feed.Updated      := Clock;

      return Feed;

   end Initialize;

   --------------
   --  Set_Id  --
   --------------

   procedure Set_Id
     (Feed  : in out Atom_Feed;
      Id    : in     String)
   is

      use Yolk.Utilities;

   begin

      Feed.Id := TUS (Id);

   end Set_Id;

   -----------------
   --  Set_Title  --
   -----------------

   procedure Set_Title
     (Feed       : in out Atom_Feed;
      Title      : in     String;
      Title_Type : in     Content_Type := Text)
   is

      use Yolk.Utilities;

   begin

      Feed.Title := TUS (Title);
      Feed.Title_Type := Title_Type;

   end Set_Title;

   -------------------
   --  Set_Updated  --
   -------------------

   procedure Set_Updated
     (Feed    : in out Atom_Feed;
      Updated : in     Ada.Calendar.Time)
   is
   begin

      Feed.Updated := Updated;

   end Set_Updated;

end Yolk.Syndication;
