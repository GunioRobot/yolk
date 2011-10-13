-------------------------------------------------------------------------------
--                                                                           --
--                                  Yolk                                     --
--                                                                           --
--                          Yolk.Syndication.Writer                          --
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

with Yolk.Utilities;

package body Yolk.Syndication.Writer is

   use Yolk.Utilities;

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
   begin

      Feed.PAF.Add_Author
        (Value => Atom_Person'(Common =>
                                 Atom_Common'(Base_URI => TUS (Base_URI),
                                              Language => TUS (Language)),
                               Name   => TUS (Name),
                               Email  => TUS (Email),
                               URI    => TUS (URI)));

   end Add_Author;

   ------------------
   --  Add_Author  --
   ------------------

   procedure Add_Author
     (Entr     : in out Atom_Entry;
      Name     : in     String;
      Base_URI : in     String := None;
      Email    : in     String := None;
      Language : in     String := None;
      URI      : in     String := None)
   is
   begin

      Entr.Authors.Append
        (New_Item => Atom_Person'(Common =>
                                    Atom_Common'(Base_URI => TUS (Base_URI),
                                                 Language => TUS (Language)),
                                  Name   => TUS (Name),
                                  Email  => TUS (Email),
                                  URI    => TUS (URI)));

   end Add_Author;

   ------------------
   --  Add_Author  --
   ------------------

   procedure Add_Author
     (Entry_Source : in out Atom_Entry_Source;
      Name         : in     String;
      Base_URI     : in     String := None;
      Email        : in     String := None;
      Language     : in     String := None;
      URI          : in     String := None)
   is
   begin

      Entry_Source.Authors.Append
        (New_Item => Atom_Person'(Common =>
                                    Atom_Common'(Base_URI => TUS (Base_URI),
                                                 Language => TUS (Language)),
                                  Name   => TUS (Name),
                                  Email  => TUS (Email),
                                  URI    => TUS (URI)));

   end Add_Author;

   --------------------
   --  Add_Category  --
   --------------------

   procedure Add_Category
     (Feed     : in out Atom_Feed;
      Term     : in     String;
      Base_URI : in     String := None;
      Label    : in     String := None;
      Language : in     String := None;
      Scheme   : in     String := None)
   is
   begin

      Feed.PAF.Add_Category
        (Value => Atom_Category'(Common =>
                                   Atom_Common'(Base_URI => TUS (Base_URI),
                                                Language => TUS (Language)),
                                 Label    => TUS (Label),
                                 Scheme   => TUS (Scheme),
                                 Term     => TUS (Term)));

   end Add_Category;

   --------------------
   --  Add_Category  --
   --------------------

   procedure Add_Category
     (Entr     : in out Atom_Entry;
      Term     : in     String;
      Base_URI : in     String := None;
      Label    : in     String := None;
      Language : in     String := None;
      Scheme   : in     String := None)
   is
   begin

      Entr.Categories.Append
        (New_Item => Atom_Category'(Common =>
                                      Atom_Common'(Base_URI => TUS (Base_URI),
                                                   Language => TUS (Language)),
                                    Label    => TUS (Label),
                                    Scheme   => TUS (Scheme),
                                    Term     => TUS (Term)));

   end Add_Category;

   --------------------
   --  Add_Category  --
   --------------------

   procedure Add_Category
     (Entry_Source : in out Atom_Entry_Source;
      Term         : in     String;
      Base_URI     : in     String := None;
      Label        : in     String := None;
      Language     : in     String := None;
      Scheme       : in     String := None)
   is
   begin

      Entry_Source.Categories.Append
        (New_Item => Atom_Category'(Common =>
                                      Atom_Common'(Base_URI => TUS (Base_URI),
                                                   Language => TUS (Language)),
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
   begin

      Feed.PAF.Add_Contributor
        (Value => Atom_Person'(Common =>
                                 Atom_Common'(Base_URI => TUS (Base_URI),
                                              Language => TUS (Language)),
                               Name   => TUS (Name),
                               Email  => TUS (Email),
                               URI    => TUS (URI)));

   end Add_Contributor;

   -----------------------
   --  Add_Contributor  --
   -----------------------

   procedure Add_Contributor
     (Entr     : in out Atom_Entry;
      Name     : in     String;
      Base_URI : in     String := None;
      Email    : in     String := None;
      Language : in     String := None;
      URI      : in     String := None)
   is
   begin

      Entr.Contributors.Append
        (New_Item => Atom_Person'(Common =>
                                    Atom_Common'(Base_URI => TUS (Base_URI),
                                                 Language => TUS (Language)),
                                  Name   => TUS (Name),
                                  Email  => TUS (Email),
                                  URI    => TUS (URI)));

   end Add_Contributor;

   -----------------------
   --  Add_Contributor  --
   -----------------------

   procedure Add_Contributor
     (Entry_Source : in out Atom_Entry_Source;
      Name         : in     String;
      Base_URI     : in     String := None;
      Email        : in     String := None;
      Language     : in     String := None;
      URI          : in     String := None)
   is
   begin

      Entry_Source.Contributors.Append
        (New_Item => Atom_Person'(Common =>
                                    Atom_Common'(Base_URI => TUS (Base_URI),
                                                 Language => TUS (Language)),
                                  Name   => TUS (Name),
                                  Email  => TUS (Email),
                                  URI    => TUS (URI)));

   end Add_Contributor;

   -----------------
   --  Add_Entry  --
   -----------------

   procedure Add_Entry
     (Feed        : in out Atom_Feed;
      Entr        : in out Atom_Entry;
      Clear_Entry : in Boolean := False)
   is
   begin

      Feed.PAF.Add_Entry (Value => Entr);

      if Clear_Entry then
         Entr := Null_Atom_Entry;
      end if;

   end Add_Entry;

   ----------------
   --  Add_Link  --
   ----------------

   procedure Add_Link
     (Feed      : in out Atom_Feed;
      Href      : in     String;
      Base_URI  : in     String := None;
      Hreflang  : in     String := None;
      Language  : in     String := None;
      Length    : in     Natural := 0;
      Mime_Type : in     String := None;
      Rel       : in     Relation_Kinds := Alternate;
      Title     : in     String := None)
   is
   begin

      Feed.PAF.Add_Link
        (Value => Atom_Link'(Common =>
                               Atom_Common'(Base_URI => TUS (Base_URI),
                                            Language => TUS (Language)),
                             Href      => TUS (Href),
                             Hreflang  => TUS (Hreflang),
                             Length    => Length,
                             Mime_Type => TUS (Mime_Type),
                             Rel       => Rel,
                             Title     => TUS (Title)));

   end Add_Link;

   ----------------
   --  Add_Link  --
   ----------------

   procedure Add_Link
     (Entr      : in out Atom_Entry;
      Href      : in     String;
      Base_URI  : in     String := None;
      Hreflang  : in     String := None;
      Language  : in     String := None;
      Length    : in     Natural := 0;
      Mime_Type : in     String := None;
      Rel       : in     Relation_Kinds := Alternate;
      Title     : in     String := None)
   is
   begin

      Entr.Links.Append
        (New_Item => Atom_Link'(Common =>
                                  Atom_Common'(Base_URI => TUS (Base_URI),
                                               Language => TUS (Language)),
                                Href      => TUS (Href),
                                Hreflang  => TUS (Hreflang),
                                Length    => Length,
                                Mime_Type => TUS (Mime_Type),
                                Rel       => Rel,
                                Title     => TUS (Title)));

   end Add_Link;

   ----------------
   --  Add_Link  --
   ----------------

   procedure Add_Link
     (Entry_Source : in out Atom_Entry_Source;
      Href         : in     String;
      Base_URI     : in     String := None;
      Hreflang     : in     String := None;
      Language     : in     String := None;
      Length       : in     Natural := 0;
      Mime_Type    : in     String := None;
      Rel          : in     Relation_Kinds := Alternate;
      Title        : in     String := None)
   is
   begin

      Entry_Source.Links.Append
        (New_Item => Atom_Link'(Common =>
                                  Atom_Common'(Base_URI => TUS (Base_URI),
                                               Language => TUS (Language)),
                                Href      => TUS (Href),
                                Hreflang  => TUS (Hreflang),
                                Length    => Length,
                                Mime_Type => TUS (Mime_Type),
                                Rel       => Rel,
                                Title     => TUS (Title)));

   end Add_Link;

   -------------------------
   --  Amount_Of_Entries  --
   -------------------------

   function Amount_Of_Entries
     (Feed : in Atom_Feed)
      return Natural
   is
   begin

      return Feed.PAF.Amount_Of_Entries;

   end Amount_Of_Entries;

   ------------------------
   --  Clear_Entry_List  --
   ------------------------

   procedure Clear_Entry_List
     (Feed : in out Atom_Feed)
   is
   begin

      Feed.PAF.Clear_Entry_List;

   end Clear_Entry_List;

   --------------------
   --  Delete_Entry  --
   --------------------

   procedure Delete_Entry
     (Feed : in out Atom_Feed;
      Id   : in     String)
   is
   begin

      Feed.PAF.Delete_Entry (Id => Id);

   end Delete_Entry;

   -------------------
   --  Get_XML_DOM  --
   -------------------

   function Get_XML_DOM
     (Feed : in Atom_Feed)
      return DOM.Core.Document
   is
   begin

      return Feed.PAF.Get_DOM;

   end Get_XML_DOM;

   ----------------------
   --  Get_XML_String  --
   ----------------------

   function Get_XML_String
     (Feed         : in Atom_Feed;
      Pretty_Print : in Boolean := False)
      return String
   is
   begin

      return Feed.PAF.Get_String (Pretty_Print => Pretty_Print);

   end Get_XML_String;

   ------------------
   --  Set_Common  --
   ------------------

   procedure Set_Common
     (Feed     : in out Atom_Feed;
      Base_URI : in     String := None;
      Language : in     String := None)
   is
   begin

      Feed.PAF.Set_Common (Value => Atom_Common'(Base_URI => TUS (Base_URI),
                                                 Language => TUS (Language)));

   end Set_Common;

   ------------------
   --  Set_Common  --
   ------------------

   procedure Set_Common
     (Entry_Source : in out Atom_Entry_Source;
      Base_URI     : in     String := None;
      Language     : in     String := None)
   is
   begin

      Entry_Source.Common := Atom_Common'(Base_URI => TUS (Base_URI),
                                          Language => TUS (Language));

   end Set_Common;

   -------------------
   --  Set_Content  --
   -------------------

   procedure Set_Content
     (Entr         : in out Atom_Entry;
      Content      : in     String;
      Content_Kind : in     Text_Kinds;
      Base_URI     : in     String := None;
      Language     : in     String := None)
   is
   begin

      Entr.Content :=
        Atom_Entry_Content'(Common       =>
                              Atom_Common'(Base_URI => TUS (Base_URI),
                                           Language => TUS (Language)),
                            Content      => TUS (Content),
                            Content_Kind => Content_Kind,
                            Mime_Type    => Null_Unbounded_String,
                            Source       => Null_Unbounded_String);

   end Set_Content;

   --------------------------
   --  Set_Content_Inline  --
   --------------------------

   procedure Set_Content_Inline
     (Entr      : in out Atom_Entry;
      Content   : in     String;
      Mime_Type : in     String;
      Base_URI  : in     String := None;
      Language  : in     String := None)
   is
   begin

      Entr.Content :=
        Atom_Entry_Content'(Common       =>
                              Atom_Common'(Base_URI => TUS (Base_URI),
                                           Language => TUS (Language)),
                            Content      => TUS (Content),
                            Content_Kind => InlineOther,
                            Mime_Type    => TUS (Mime_Type),
                            Source       => Null_Unbounded_String);

   end Set_Content_Inline;

   -----------------------------
   --  Set_Content_OutOfLine  --
   -----------------------------

   procedure Set_Content_OutOfLine
     (Entr      : in out Atom_Entry;
      Mime_Type : in     String;
      Source    : in     String;
      Base_URI  : in     String := None;
      Language  : in     String := None)
   is
   begin

      Entr.Content :=
        Atom_Entry_Content'(Common       =>
                              Atom_Common'(Base_URI => TUS (Base_URI),
                                           Language => TUS (Language)),
                            Content      => Null_Unbounded_String,
                            Content_Kind => OutOfLineOther,
                            Mime_Type    => TUS (Mime_Type),
                            Source       => TUS (Source));

   end Set_Content_OutOfLine;

   ------------------------
   --  Set_Entry_Source  --
   ------------------------

   procedure Set_Entry_Source
     (Entr               : in out Atom_Entry;
      Source             : in out Atom_Entry_Source;
      Clear_Entry_Source : in     Boolean := False)
   is
   begin

      Entr.Source := Source;

      if Clear_Entry_Source then
         Entr.Source := Null_Atom_Entry_Source;
      end if;

   end Set_Entry_Source;

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
   begin

      Feed.PAF.Set_Generator
        (Value => Atom_Generator'(Agent => TUS (Agent),
                                  Common  =>
                                    Atom_Common'(Base_URI => TUS (Base_URI),
                                                 Language => TUS (Language)),
                                  URI     => TUS (URI),
                                  Version => TUS (Version)));

   end Set_Generator;

   ---------------------
   --  Set_Generator  --
   ---------------------

   procedure Set_Generator
     (Entry_Source : in out Atom_Entry_Source;
      Agent        : in     String;
      Base_URI     : in     String := None;
      Language     : in     String := None;
      URI          : in     String := None;
      Version      : in     String := None)
   is
   begin

      Entry_Source.Generator := Atom_Generator'
        (Agent   => TUS (Agent),
         Common  =>
           Atom_Common'(Base_URI => TUS (Base_URI),
                        Language => TUS (Language)),
         URI     => TUS (URI),
         Version => TUS (Version));

   end Set_Generator;

   ----------------
   --  Set_Icon  --
   ----------------

   procedure Set_Icon
     (Feed     : in out Atom_Feed;
      URI      : in     String;
      Base_URI : in     String := None;
      Language : in     String := None)
   is
   begin

      Feed.PAF.Set_Icon
        (Value => Atom_Icon'(Common =>
                               Atom_Common'(Base_URI => TUS (Base_URI),
                                            Language => TUS (Language)),
                             URI    => TUS (URI)));

   end Set_Icon;

   ----------------
   --  Set_Icon  --
   ----------------

   procedure Set_Icon
     (Entry_Source : in out Atom_Entry_Source;
      URI          : in     String;
      Base_URI     : in     String := None;
      Language     : in     String := None)
   is
   begin

      Entry_Source.Icon := Atom_Icon'
        (Common =>
           Atom_Common'(Base_URI => TUS (Base_URI),
                        Language => TUS (Language)),
         URI    => TUS (URI));

   end Set_Icon;

   --------------
   --  Set_Id  --
   --------------

   procedure Set_Id
     (Feed     : in out Atom_Feed;
      Id       : in     String;
      Base_URI : in     String := None;
      Language : in     String := None)
   is
   begin

      Feed.PAF.Set_Id
        (Value => Atom_Id'(Common =>
                             Atom_Common'(Base_URI => TUS (Base_URI),
                                          Language => TUS (Language)),
                           URI    => TUS (Id)));

   end Set_Id;

   --------------
   --  Set_Id  --
   --------------

   procedure Set_Id
     (Entr     : in out Atom_Entry;
      Id       : in     String;
      Base_URI : in     String := None;
      Language : in     String := None)
   is
   begin

      Entr.Id := Atom_Id'(Common =>
                            Atom_Common'(Base_URI => TUS (Base_URI),
                                         Language => TUS (Language)),
                          URI    => TUS (Id));

   end Set_Id;

   --------------
   --  Set_Id  --
   --------------

   procedure Set_Id
     (Entry_Source : in out Atom_Entry_Source;
      Id           : in     String;
      Base_URI     : in     String := None;
      Language     : in     String := None)
   is
   begin

      Entry_Source.Id := Atom_Id'(Common =>
                                    Atom_Common'(Base_URI => TUS (Base_URI),
                                                 Language => TUS (Language)),
                                  URI    => TUS (Id));

   end Set_Id;

   ----------------
   --  Set_Logo  --
   ----------------

   procedure Set_Logo
     (Feed     : in out Atom_Feed;
      URI      : in String;
      Base_URI : in String := None;
      Language : in String := None)
   is
   begin

      Feed.PAF.Set_Logo
        (Value => Atom_Logo'(Common =>
                               Atom_Common'(Base_URI => TUS (Base_URI),
                                            Language => TUS (Language)),
                             URI    => TUS (URI)));

   end Set_Logo;

   ----------------
   --  Set_Logo  --
   ----------------

   procedure Set_Logo
     (Entry_Source : in out Atom_Entry_Source;
      URI          : in String;
      Base_URI     : in String := None;
      Language     : in String := None)
   is
   begin

      Entry_Source.Logo := Atom_Logo'
        (Common =>
           Atom_Common'(Base_URI => TUS (Base_URI),
                        Language => TUS (Language)),
         URI    => TUS (URI));

   end Set_Logo;

   ---------------------
   --  Set_Published  --
   ---------------------

   procedure Set_Published
     (Entr           : in out Atom_Entry;
      Published_Time : in     Ada.Calendar.Time;
      Base_URI       : in     String := None;
      Language       : in     String := None)
   is
   begin

      Entr.Published := Atom_Date'(Common     =>
                                     Atom_Common'(Base_URI => TUS (Base_URI),
                                                  Language => TUS (Language)),
                                   Is_Set     => True,
                                   Time_Stamp => Published_Time);

   end Set_Published;

   ------------------
   --  Set_Rights  --
   ------------------

   procedure Set_Rights
     (Feed        : in out Atom_Feed;
      Rights      : in String;
      Base_URI    : in String := None;
      Language    : in String := None;
      Rights_Kind : in Text_Kinds := Text)
   is
   begin

      Feed.PAF.Set_Rights
        (Value => Atom_Text'(Common =>
                               Atom_Common'(Base_URI => TUS (Base_URI),
                                            Language => TUS (Language)),
                             Text_Content => TUS (Rights),
                             Text_Kind    => Rights_Kind));

   end Set_Rights;

   ------------------
   --  Set_Rights  --
   ------------------

   procedure Set_Rights
     (Entr        : in out Atom_Entry;
      Rights      : in String;
      Base_URI    : in String := None;
      Language    : in String := None;
      Rights_Kind : in Text_Kinds := Text)
   is
   begin

      Entr.Rights := Atom_Text'(Common       =>
                                  Atom_Common'(Base_URI => TUS (Base_URI),
                                               Language => TUS (Language)),
                                Text_Content => TUS (Rights),
                                Text_Kind    => Rights_Kind);

   end Set_Rights;

   ------------------
   --  Set_Rights  --
   ------------------

   procedure Set_Rights
     (Entry_Source : in out Atom_Entry_Source;
      Rights       : in String;
      Base_URI     : in String := None;
      Language     : in String := None;
      Rights_Kind  : in Text_Kinds := Text)
   is
   begin

      Entry_Source.Rights := Atom_Text'
        (Common       =>
           Atom_Common'(Base_URI => TUS (Base_URI),
                        Language => TUS (Language)),
         Text_Content => TUS (Rights),
         Text_Kind    => Rights_Kind);

   end Set_Rights;

   --------------------
   --  Set_Subtitle  --
   --------------------

   procedure Set_Subtitle
     (Feed           : in out Atom_Feed;
      Subtitle       : in String;
      Base_URI       : in String := None;
      Language       : in String := None;
      Subtitle_Kind  : in Text_Kinds := Text)
   is
   begin

      Feed.PAF.Set_Subtitle
        (Value => Atom_Text'(Common =>
                               Atom_Common'(Base_URI => TUS (Base_URI),
                                            Language => TUS (Language)),
                             Text_Content => TUS (Subtitle),
                             Text_Kind    => Subtitle_Kind));

   end Set_Subtitle;

   --------------------
   --  Set_Subtitle  --
   --------------------

   procedure Set_Subtitle
     (Entry_Source  : in out Atom_Entry_Source;
      Subtitle      : in String;
      Base_URI      : in String := None;
      Language      : in String := None;
      Subtitle_Kind : in Text_Kinds := Text)
   is
   begin

      Entry_Source.Subtitle := Atom_Text'
        (Common       =>
           Atom_Common'(Base_URI => TUS (Base_URI),
                        Language => TUS (Language)),
         Text_Content => TUS (Subtitle),
         Text_Kind    => Subtitle_Kind);

   end Set_Subtitle;

   -----------------
   --  Set_Summary  --
   -----------------

   procedure Set_Summary
     (Entr           : in out Atom_Entry;
      Summary        : in     String;
      Base_URI       : in     String := None;
      Language       : in     String := None;
      Summary_Kind   : in     Text_Kinds := Text)
   is
   begin

      Entr.Summary := Atom_Text'(Common       =>
                                   Atom_Common'(Base_URI => TUS (Base_URI),
                                                Language => TUS (Language)),
                                 Text_Content => TUS (Summary),
                                 Text_Kind    => Summary_Kind);

   end Set_Summary;

   -----------------
   --  Set_Title  --
   -----------------

   procedure Set_Title
     (Feed       : in out Atom_Feed;
      Title      : in     String;
      Base_URI   : in     String := None;
      Language   : in     String := None;
      Title_Kind : in     Text_Kinds := Text)
   is
   begin

      Feed.PAF.Set_Title
        (Value => Atom_Text'(Common =>
                               Atom_Common'(Base_URI => TUS (Base_URI),
                                            Language => TUS (Language)),
                             Text_Content => TUS (Title),
                             Text_Kind    => Title_Kind));

   end Set_Title;

   -----------------
   --  Set_Title  --
   -----------------

   procedure Set_Title
     (Entr       : in out Atom_Entry;
      Title      : in     String;
      Base_URI   : in     String := None;
      Language   : in     String := None;
      Title_Kind : in     Text_Kinds := Text)
   is
   begin

      Entr.Title := Atom_Text'(Common       =>
                                 Atom_Common'(Base_URI => TUS (Base_URI),
                                              Language => TUS (Language)),
                               Text_Content => TUS (Title),
                               Text_Kind    => Title_Kind);

   end Set_Title;

   -----------------
   --  Set_Title  --
   -----------------

   procedure Set_Title
     (Entry_Source : in out Atom_Entry_Source;
      Title        : in     String;
      Base_URI     : in     String := None;
      Language     : in     String := None;
      Title_Kind   : in     Text_Kinds := Text)
   is
   begin

      Entry_Source.Title := Atom_Text'
        (Common       =>
           Atom_Common'(Base_URI => TUS (Base_URI),
                        Language => TUS (Language)),
         Text_Content => TUS (Title),
         Text_Kind    => Title_Kind);

   end Set_Title;

   -------------------
   --  Set_Updated  --
   -------------------

   procedure Set_Updated
     (Feed        : in out Atom_Feed;
      Update_Time : in     Ada.Calendar.Time;
      Base_URI    : in     String := None;
      Language    : in     String := None)
   is
   begin

      Feed.PAF.Set_Updated_Time
        (Value => Atom_Date'(Common =>
                               Atom_Common'(Base_URI => TUS (Base_URI),
                                            Language => TUS (Language)),
                             Is_Set     => True,
                             Time_Stamp => Update_Time));

   end Set_Updated;

   -------------------
   --  Set_Updated  --
   -------------------

   procedure Set_Updated
     (Entr        : in out Atom_Entry;
      Update_Time : in     Ada.Calendar.Time;
      Base_URI    : in     String := None;
      Language    : in     String := None)
   is
   begin

      Entr.Updated := Atom_Date'(Common     =>
                                   Atom_Common'(Base_URI => TUS (Base_URI),
                                                Language => TUS (Language)),
                                 Is_Set     => True,
                                 Time_Stamp => Update_Time);

   end Set_Updated;

   -------------------
   --  Set_Updated  --
   -------------------

   procedure Set_Updated
     (Entry_Source : in out Atom_Entry_Source;
      Update_Time  : in     Ada.Calendar.Time;
      Base_URI     : in     String := None;
      Language     : in     String := None)
   is
   begin

      Entry_Source.Updated := Atom_Date'
        (Common     =>
           Atom_Common'(Base_URI => TUS (Base_URI),
                        Language => TUS (Language)),
         Is_Set     => True,
         Time_Stamp => Update_Time);

   end Set_Updated;

end Yolk.Syndication.Writer;
