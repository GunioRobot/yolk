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
      Base_URI : in     String := None;
      Email    : in     String := None;
      Language : in     String := None;
      URI      : in     String := None)
   is

      use Yolk.Utilities;

   begin

      Feed.Authors.Append
        (Atom_Person'(Common  =>
                        Atom_Common'(Base_URI => TUS (Base_URI),
                                     Language => TUS (Language)),
                      Email   => TUS (Email),
                      Name    => TUS (Name),
                      URI     => TUS (URI)));

   end Add_Author;

   --------------------
   --  Add_Category  --
   --------------------

   procedure Add_Category
     (Feed     : in out Atom_Feed;
      Term     : in     String;
      Base_URI : in     String := None;
      Content  : in     String := None;
      Label    : in     String := None;
      Language : in     String := None;
      Scheme   : in     String := None)
   is

      use Yolk.Utilities;

   begin

      Feed.Categories.Append
        (Atom_Category'(Common   =>
                          Atom_Common'(Base_URI => TUS (Base_URI),
                                       Language => TUS (Language)),
                        Content  => TUS (Content),
                        Label    => TUS (Label),
                        Scheme   => TUS (Scheme),
                        Term     => TUS (Term)));

   end Add_Category;

   -----------------------
   --  Add_Contributor  --
   -----------------------

   procedure Add_Contributor
     (Feed     : in out Atom_Feed;
      Name     : in     String;
      Base_URI : in     String := None;
      Email    : in     String := None;
      Language : in     String := None;
      URI      : in     String := None)
   is

      use Yolk.Utilities;

   begin

      Feed.Contributors.Append
        (Atom_Person'(Common  =>
                        Atom_Common'(Base_URI => TUS (Base_URI),
                                     Language => TUS (Language)),
                      Email   => TUS (Email),
                      Name    => TUS (Name),
                      URI     => TUS (URI)));

   end Add_Contributor;

   ----------------
   --  Add_Link  --
   ----------------

   procedure Add_Link
     (Feed        : in out Atom_Feed;
      Href        : in     String;
      Base_URI    : in     String := None;
      Content     : in     String := None;
      Hreflang    : in     String := None;
      Language    : in     String := None;
      Length      : in     Natural := 0;
      Mime_Type   : in     String := None;
      Rel         : in     Relation_Type := Alternate;
      Title       : in     String := None)
   is

      use Yolk.Utilities;

   begin

      Feed.Links.Append
        (Atom_Link'(Common =>
                      Atom_Common'(Base_URI => TUS (Base_URI),
                                   Language => TUS (Language)),
                    Content   => TUS (Content),
                    Href      => TUS (Href),
                    Hreflang  => TUS (Hreflang),
                    Length    => Length,
                    Mime_Type => TUS (Mime_Type),
                    Rel       => Rel,
                    Title     => TUS (Title)));

   end Add_Link;

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
     (Id             : in String;
      Title          : in String;
      Base_URI       : in String := None;
      Language       : in String := None;
      Title_Type     : in Content_Type := Text)
      return Atom_Feed
   is

      use Ada.Calendar;
      use Ada.Text_IO;
      use Yolk.Utilities;

   begin

      Put_Line (Id);
      Put_Line (Title);
      Put_Line (Atom_Date_Image (Clock));

      return
        Atom_Feed'(Authors        => Person_List.Empty_List,
                   Categories     => Category_List.Empty_List,
                   Common         =>
                     Atom_Common'(Base_URI => TUS (Base_URI),
                                  Language => TUS (Language)),
                   Contributors   => Person_List.Empty_List,
                   Generator      => Null_Generator,
                   Icon           => Null_Icon,
                   Id             => Atom_Id'(Id => TUS (Id)),
                   Links          => Link_List.Empty_List,
                   Logo           => Null_Logo,
                   Rights         => Null_Text,
                   Title          =>
                     Atom_Text'(Common       =>
                                  Atom_Common'(Base_URI => TUS (Base_URI),
                                               Language => TUS (Language)),
                                Text_Content => TUS (Title),
                                Text_Type    => Title_Type),
                   Updated        => Clock);

   end Initialize;

   --------------------
   --  Set_Base_URI  --
   --------------------

   procedure Set_Base_URI
     (Feed     : in out Atom_Feed;
      Base_URI : in     String := None)
   is

      use Yolk.Utilities;

   begin

      Feed.Common.Base_URI := TUS (Base_URI);

   end Set_Base_URI;

   ---------------------
   --  Set_Generator  --
   ---------------------

   procedure Set_Generator
     (Feed     : in out Atom_Feed;
      Agent    : in     String;
      Base_URI : in     String := None;
      Language : in     String := None;
      URI      : in     String := None;
      Version  : in     String := None)
   is

      use Yolk.Utilities;

   begin

      Feed.Generator :=
        Atom_Generator'(Agent    => TUS (Agent),
                        Common   => Atom_Common'(Base_URI => TUS (Base_URI),
                                                 Language => TUS (Language)),
                        URI      => TUS (URI),
                        Version  => TUS (Version));
   end Set_Generator;

   ----------------
   --  Set_Icon  --
   ----------------

   procedure Set_Icon
     (Feed     : in out Atom_Feed;
      URI      : in     String;
      Base_URI : in     String := None)
   is

      use Yolk.Utilities;

   begin

      Feed.Icon :=
        Atom_Icon'(Common   =>
                     Atom_Common'(Base_URI => TUS (Base_URI),
                                  Language => Null_Unbounded_String),
                   URI      => TUS (URI));

   end Set_Icon;

   --------------
   --  Set_Id  --
   --------------

   procedure Set_Id
     (Feed  : in out Atom_Feed;
      Id    : in     String)
   is

      use Yolk.Utilities;

   begin

      Feed.Id := Atom_Id'(Id => TUS (Id));

   end Set_Id;

   --------------------
   --  Set_Language  --
   --------------------

   procedure Set_Language
     (Feed     : in out Atom_Feed;
      Language : in     String := None)
   is

      use Yolk.Utilities;

   begin

      Feed.Common.Language := TUS (Language);

   end Set_Language;

   ----------------
   --  Set_Logo  --
   ----------------

   procedure Set_Logo
     (Feed     : in out Atom_Feed;
      URI      : in     String;
      Base_URI : in     String := None;
      Language : in     String := None)
   is

      use Yolk.Utilities;

   begin

      Feed.Logo := Atom_Logo'(Common =>
                                Atom_Common'(Base_URI => TUS (Base_URI),
                                             Language => TUS (Language)),
                              URI    => TUS (URI));

   end Set_Logo;

   ------------------
   --  Set_Rights  --
   ------------------

   procedure Set_Rights
     (Feed           : in out Atom_Feed;
      Text_Content   : in     String;
      Base_URI       : in     String := None;
      Language       : in     String := None;
      Text_Type      : in     Content_Type := Text)
   is

      use Yolk.Utilities;

   begin

      Feed.Rights := Atom_Text'(Common       =>
                                  Atom_Common'(Base_URI => TUS (Base_URI),
                                               Language => TUS (Language)),
                                Text_Content => TUS (Text_Content),
                                Text_Type    => Text_Type);

   end Set_Rights;

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

      Feed.Title :=
        Atom_Text'(Common       =>
                     Atom_Common'(Base_URI => Feed.Title.Common.Base_URI,
                                  Language => Feed.Title.Common.Language),
                   Text_Content => TUS (Title),
                   Text_Type    => Title_Type);

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
