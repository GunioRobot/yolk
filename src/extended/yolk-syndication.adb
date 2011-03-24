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

   -----------------
   --  Atom_Feed  --
   -----------------

   protected body Atom_Feed is

      ------------------
      --  Add_Author  --
      ------------------

      procedure Add_Author
        (Name     : in String;
         Base_URI : in String := None;
         Email    : in String := None;
         Language : in String := None;
         URI      : in String := None)
      is

         use Yolk.Utilities;

      begin

         Authors.Append
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
        (Term     : in String;
         Base_URI : in String := None;
         Content  : in String := None;
         Label    : in String := None;
         Language : in String := None;
         Scheme   : in String := None)
      is

         use Yolk.Utilities;

      begin

         Categories.Append
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
        (Name     : in String;
         Base_URI : in String := None;
         Email    : in String := None;
         Language : in String := None;
         URI      : in String := None)
      is

         use Yolk.Utilities;

      begin

         Contributors.Append
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
        (Href        : in String;
         Base_URI    : in String := None;
         Content     : in String := None;
         Hreflang    : in String := None;
         Language    : in String := None;
         Length      : in Natural := 0;
         Mime_Type   : in String := None;
         Rel         : in Relation_Kind := Alternate;
         Title       : in String := None)
      is

         use Yolk.Utilities;

      begin

         Links.Append
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

      --------------------
      --  Set_Base_URI  --
      --------------------

      procedure Set_Base_URI
        (Value : in String := None)
      is

         use Yolk.Utilities;

      begin

         Common.Base_URI := TUS (Value);

      end Set_Base_URI;

      ------------------
      --  Set_Common  --
      ------------------

      procedure Set_Common
        (Base_URI : in String := None;
         Language : in String := None)
      is

         use Yolk.Utilities;

      begin

         Common.Base_URI := TUS (Base_URI);
         Common.Language := TUS (Language);

      end Set_Common;

      ---------------------
      --  Set_Generator  --
      ---------------------

      procedure Set_Generator
        (Agent  : in String;
         Base_URI : in String := None;
         Language : in String := None;
         URI      : in String := None;
         Version  : in String := None)
      is

         use Yolk.Utilities;

      begin

         Generator :=
           Atom_Generator'(Agent   => TUS (Agent),
                           Common  => Atom_Common'(Base_URI => TUS (Base_URI),
                                                   Language => TUS (Language)),
                           URI     => TUS (URI),
                           Version => TUS (Version));
      end Set_Generator;

      ----------------
      --  Set_Icon  --
      ----------------

      procedure Set_Icon
        (URI      : in String;
         Base_URI : in String := None)
      is

         use Yolk.Utilities;

      begin

         Icon :=
           Atom_Icon'(Common   =>
                        Atom_Common'(Base_URI => TUS (Base_URI),
                                     Language => Null_Unbounded_String),
                      URI      => TUS (URI));

      end Set_Icon;

      --------------
      --  Set_Id  --
      --------------

      procedure Set_Id
        (Value : in String)
      is

         use Yolk.Utilities;

      begin

         Id := Atom_Id'(Id => TUS (Value));

      end Set_Id;

      --------------------
      --  Set_Language  --
      --------------------

      procedure Set_Language
        (Value : in String := None)
      is

         use Yolk.Utilities;

      begin

         Common.Language := TUS (Value);

      end Set_Language;

      ----------------
      --  Set_Logo  --
      ----------------

      procedure Set_Logo
        (URI      : in String;
         Base_URI : in String := None;
         Language : in String := None)
      is

         use Yolk.Utilities;

      begin

         Logo := Atom_Logo'(Common =>
                              Atom_Common'(Base_URI => TUS (Base_URI),
                                           Language => TUS (Language)),
                            URI    => TUS (URI));

      end Set_Logo;

      ------------------
      --  Set_Rights  --
      ------------------

      procedure Set_Rights
        (Content  : in String;
         Base_URI : in String := None;
         Kind     : in Content_Kind := Text;
         Language : in String := None)
      is

         use Yolk.Utilities;

      begin

         Rights := Atom_Text'(Common       =>
                                Atom_Common'(Base_URI => TUS (Base_URI),
                                             Language => TUS (Language)),
                              Text_Content => TUS (Content),
                              Text_Type    => Kind);

      end Set_Rights;

      --------------------
      --  Set_Subtitle  --
      --------------------

      procedure Set_Subtitle
        (Content  : in String;
         Base_URI : in String := None;
         Kind     : in Content_Kind := Text;
         Language : in String := None)
      is

         use Yolk.Utilities;

      begin

         Subtitle := Atom_Text'(Common       =>
                                  Atom_Common'(Base_URI => TUS (Base_URI),
                                               Language => TUS (Language)),
                                Text_Content => TUS (Content),
                                Text_Type    => Kind);

      end Set_Subtitle;

      -----------------
      --  Set_Title  --
      -----------------

      procedure Set_Title
        (Value : in String;
         Kind  : in Content_Kind := Text)
      is

         use Yolk.Utilities;

      begin

         Title :=
           Atom_Text'(Common       =>
                        Atom_Common'(Base_URI => Title.Common.Base_URI,
                                     Language => Title.Common.Language),
                      Text_Content => TUS (Value),
                      Text_Type    => Kind);

      end Set_Title;

      -------------------
      --  Set_Updated  --
      -------------------

      procedure Set_Updated_Time
        (Value : in Ada.Calendar.Time)
      is

         use Ada.Calendar;

      begin

         if Value > Updated then
            Updated := Value;
         end if;

      end Set_Updated_Time;

   end Atom_Feed;

   ------------------
   --  Initialize  --
   ------------------

   function Initialize
     (Id             : in String;
      Title          : in String;
      Base_URI       : in String := None;
      Language       : in String := None;
      Title_Kind     : in Content_Kind := Text)
      return Atom_Feed
   is

      use Ada.Calendar;
      use Ada.Text_IO;
      use Yolk.Utilities;

   begin

      Put_Line (Id);
      Put_Line (Title);
      Put_Line (Atom_Date_Image (Clock));

      return Feed : Atom_Feed do
         Feed.Set_Id (Value => Id);
         Feed.Set_Title (Value => Title,
                         Kind  => Title_Kind);
         Feed.Set_Common (Base_URI => Base_URI,
                          Language => Language);
      end return;

   end Initialize;

end Yolk.Syndication;
