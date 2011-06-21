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

with Ada.Calendar.Formatting;
with Ada.Streams;
with Ada.Strings.Fixed;
with DOM.Core.Documents;
with DOM.Core.Elements;
with DOM.Core.Nodes;
with DOM.Readers;
with Input_Sources.Strings;
with Sax.Readers;
with Unicode.CES.Utf8;
with Yolk.Utilities;

package body Yolk.Syndication.Writer is

   use Yolk.Utilities;

   function Atom_Date_Image
     (Time_Stamp : in Ada.Calendar.Time)
      return String;
   --  Return a string representation of the Time_Stamp time. The format is:
   --    yyyy-mm-ddThh:mm:ssZ
   --  The uppercase T and Z are requried as per the Atom specification.
   --  It is expected that the Time_Stamp is GMT.

   function Create_Node_From_String
     (XML_String : in String)
      return DOM.Core.Node;
   --  Return a DOM node based on the given XML_String.

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

   -------------------------
   --  Add_Author_Source  --
   -------------------------

   procedure Add_Author_Source
     (Entr     : in out Atom_Entry;
      Name     : in     String;
      Base_URI : in     String := None;
      Email    : in     String := None;
      Language : in     String := None;
      URI      : in     String := None)
   is
   begin

      Entr.Source.Authors.Append
        (New_Item => Atom_Person'(Common =>
                                    Atom_Common'(Base_URI => TUS (Base_URI),
                                                 Language => TUS (Language)),
                                  Name   => TUS (Name),
                                  Email  => TUS (Email),
                                  URI    => TUS (URI)));

   end Add_Author_Source;

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
   begin

      Feed.PAF.Add_Category
        (Value => Atom_Category'(Common =>
                                   Atom_Common'(Base_URI => TUS (Base_URI),
                                                Language => TUS (Language)),
                                 Content  => TUS (Content),
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
      Content  : in     String := None;
      Label    : in     String := None;
      Language : in     String := None;
      Scheme   : in     String := None)
   is
   begin

      Entr.Categories.Append
        (New_Item => Atom_Category'(Common =>
                                      Atom_Common'(Base_URI => TUS (Base_URI),
                                                   Language => TUS (Language)),
                                    Content  => TUS (Content),
                                    Label    => TUS (Label),
                                    Scheme   => TUS (Scheme),
                                    Term     => TUS (Term)));

   end Add_Category;

   ---------------------------
   --  Add_Category_Source  --
   ---------------------------

   procedure Add_Category_Source
     (Entr     : in out Atom_Entry;
      Term     : in     String;
      Base_URI : in     String := None;
      Content  : in     String := None;
      Label    : in     String := None;
      Language : in     String := None;
      Scheme   : in     String := None)
   is
   begin

      Entr.Source.Categories.Append
        (New_Item => Atom_Category'(Common =>
                                      Atom_Common'(Base_URI => TUS (Base_URI),
                                                   Language => TUS (Language)),
                                    Content  => TUS (Content),
                                    Label    => TUS (Label),
                                    Scheme   => TUS (Scheme),
                                    Term     => TUS (Term)));

   end Add_Category_Source;

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

   ------------------------------
   --  Add_Contributor_Source  --
   ------------------------------

   procedure Add_Contributor_Source
     (Entr     : in out Atom_Entry;
      Name     : in     String;
      Base_URI : in     String := None;
      Email    : in     String := None;
      Language : in     String := None;
      URI      : in     String := None)
   is
   begin

      Entr.Source.Contributors.Append
        (New_Item => Atom_Person'(Common =>
                                    Atom_Common'(Base_URI => TUS (Base_URI),
                                                 Language => TUS (Language)),
                                  Name   => TUS (Name),
                                  Email  => TUS (Email),
                                  URI    => TUS (URI)));

   end Add_Contributor_Source;

   -----------------
   --  Add_Entry  --
   -----------------

   procedure Add_Entry
     (Feed        : in out Atom_Feed;
      Entr        : in     Atom_Entry;
      Entry_Added : out    Boolean)
   is
   begin

      Feed.PAF.Add_Entry (Value       => Entr,
                          Entry_Added => Entry_Added);

   end Add_Entry;

   ----------------
   --  Add_Link  --
   ----------------

   procedure Add_Link
     (Feed      : in out Atom_Feed;
      Href      : in     String;
      Base_URI  : in     String := None;
      Content   : in     String := None;
      Hreflang  : in     String := None;
      Language  : in     String := None;
      Length    : in     Natural := 0;
      Mime_Type : in     String := None;
      Rel       : in     Relation_Kind := Alternate;
      Title     : in     String := None)
   is
   begin

      Feed.PAF.Add_Link
        (Value => Atom_Link'(Common =>
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

   ----------------
   --  Add_Link  --
   ----------------

   procedure Add_Link
     (Entr      : in out Atom_Entry;
      Href      : in     String;
      Base_URI  : in     String := None;
      Content   : in     String := None;
      Hreflang  : in     String := None;
      Language  : in     String := None;
      Length    : in     Natural := 0;
      Mime_Type : in     String := None;
      Rel       : in     Relation_Kind := Alternate;
      Title     : in     String := None)
   is
   begin

      Entr.Links.Append
        (New_Item => Atom_Link'(Common =>
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
   --  Add_Link_Source  --
   -----------------------

   procedure Add_Link_Source
     (Entr      : in out Atom_Entry;
      Href      : in     String;
      Base_URI  : in     String := None;
      Content   : in     String := None;
      Hreflang  : in     String := None;
      Language  : in     String := None;
      Length    : in     Natural := 0;
      Mime_Type : in     String := None;
      Rel       : in     Relation_Kind := Alternate;
      Title     : in     String := None)
   is
   begin

      Entr.Source.Links.Append
        (New_Item => Atom_Link'(Common =>
                                  Atom_Common'(Base_URI => TUS (Base_URI),
                                               Language => TUS (Language)),
                                Content   => TUS (Content),
                                Href      => TUS (Href),
                                Hreflang  => TUS (Hreflang),
                                Length    => Length,
                                Mime_Type => TUS (Mime_Type),
                                Rel       => Rel,
                                Title     => TUS (Title)));

   end Add_Link_Source;

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

   ------------------------
   --  Clear_Entry_List  --
   ------------------------

   procedure Clear_Entry_List
     (Feed : in out Atom_Feed)
   is
   begin

      Feed.PAF.Clear_Entry_List;

   end Clear_Entry_List;

   -------------------------------
   --  Create_Node_From_String  --
   -------------------------------

   function Create_Node_From_String
     (XML_String : in String)
      return DOM.Core.Node
   is

      use DOM.Core;
      use DOM.Core.Nodes;
      use DOM.Readers;
      use Input_Sources.Strings;
      use Sax.Readers;

      Input  : String_Input;
      Reader : Tree_Reader;

   begin

      return Fragment : Node do
         Open (Str      => XML_String,
               Encoding => Unicode.CES.Utf8.Utf8_Encoding,
               Input    => Input);

         Set_Feature (Parser => Reader,
                      Name   => Validation_Feature,
                      Value  => False);
         Set_Feature (Parser => Reader,
                      Name   => Namespace_Feature,
                      Value  => False);

         Parse (Parser => Reader,
                Input  => Input);

         Close (Input => Input);

         Fragment := Get_Tree (Read => Reader);

      exception

         when others =>
            raise Not_Valid_XML with XML_String;

      end return;

   end Create_Node_From_String;

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
   --  Equal_Entry  --
   -------------------

   function Equal_Entry
     (Left, Right : in Atom_Entry)
      return Boolean
   is
   begin

      return Left.Id.URI = Right.Id.URI;

   end Equal_Entry;

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
     (Feed : in Atom_Feed)
      return String
   is
   begin

      return Feed.PAF.Get_String;

   end Get_XML_String;

   ----------------------
   --  New_Atom_Entry  --
   ----------------------

   function New_Atom_Entry
     (Base_URI : in String := None;
      Language : in String := None)
      return Atom_Entry
   is

      use Ada.Calendar;

      Now : constant Time := Clock;

   begin

      return An_Entry : Atom_Entry do
         An_Entry := (Authors       => Person_List.Empty_List,
                      Categories    => Category_List.Empty_List,
                      Common        =>
                        Atom_Common'(Base_URI => TUS (Base_URI),
                                     Language => TUS (Language)),
                      Content       =>
                        Atom_Entry_Content'(
                          Common        =>
                            Atom_Common'(Base_URI => Null_Unbounded_String,
                                         Language => Null_Unbounded_String),
                          Content       => Null_Unbounded_String,
                          Content_Kind  => Text,
                          Mime_Type     => Null_Unbounded_String,
                          Source        => Null_Unbounded_String),
                      Contributors  => Person_List.Empty_List,
                      Id            =>
                        Atom_Id'(Common =>
                                   Atom_Common'(Base_URI =>
                                                  Null_Unbounded_String,
                                                Language =>
                                                  Null_Unbounded_String),
                                 URI    => Null_Unbounded_String),
                      Links         => Link_List.Empty_List,
                      Published     =>
                        Atom_Date'(Common     =>
                                     Atom_Common'(Base_URI =>
                                                    Null_Unbounded_String,
                                                  Language =>
                                                    Null_Unbounded_String),
                                   Time_Stamp => Now),
                      Rights        =>
                        Atom_Text'(Common       =>
                                     Atom_Common'(Base_URI =>
                                                    Null_Unbounded_String,
                                                  Language =>
                                                    Null_Unbounded_String),
                                   Text_Content => Null_Unbounded_String,
                                   Text_Kind    => Text),
                      Source        =>
                        Atom_Entry_Source'
                          (Authors      => Person_List.Empty_List,
                           Categories   => Category_List.Empty_List,
                           Common       => Atom_Common'
                             (Base_URI => Null_Unbounded_String,
                              Language => Null_Unbounded_String),
                           Contributors => Person_List.Empty_List,
                           Generator    => Atom_Generator'
                             (Agent   => Null_Unbounded_String,
                              Common  => Atom_Common'
                                (Base_URI => Null_Unbounded_String,
                                 Language => Null_Unbounded_String),
                              URI     => Null_Unbounded_String,
                              Version => Null_Unbounded_String),
                           Icon         => Atom_Icon'
                             (Common => Atom_Common'
                                (Base_URI => Null_Unbounded_String,
                                 Language => Null_Unbounded_String),
                              URI    => Null_Unbounded_String),
                           Id           => Atom_Id'
                             (Common => Atom_Common'
                                (Base_URI => Null_Unbounded_String,
                                 Language => Null_Unbounded_String),
                              URI    => Null_Unbounded_String),
                           Links        => Link_List.Empty_List,
                           Logo         => Atom_Logo'
                             (Common => Atom_Common'
                                (Base_URI => Null_Unbounded_String,
                                 Language => Null_Unbounded_String),
                              URI    => Null_Unbounded_String),
                           Rights       => Atom_Text'
                             (Common       => Atom_Common'
                                (Base_URI => Null_Unbounded_String,
                                 Language => Null_Unbounded_String),
                              Text_Content => Null_Unbounded_String,
                              Text_Kind    => Text),
                           Subtitle     => Atom_Text'
                             (Common       => Atom_Common'
                                (Base_URI => Null_Unbounded_String,
                                 Language => Null_Unbounded_String),
                              Text_Content => Null_Unbounded_String,
                              Text_Kind    => Text),
                           Title        => Atom_Text'
                             (Common       => Atom_Common'
                                (Base_URI => Null_Unbounded_String,
                                 Language => Null_Unbounded_String),
                              Text_Content => Null_Unbounded_String,
                              Text_Kind    => Text),
                           Updated      => Atom_Date'
                             (Common     => Atom_Common'
                                (Base_URI => Null_Unbounded_String,
                                 Language => Null_Unbounded_String),
                              Time_Stamp => Now)),
                      Summary       =>
                        Atom_Text'(Common       =>
                                     Atom_Common'(Base_URI =>
                                                    Null_Unbounded_String,
                                                  Language =>
                                                    Null_Unbounded_String),
                                   Text_Content => Null_Unbounded_String,
                                   Text_Kind    => Text),
                      Title         =>
                        Atom_Text'(Common       =>
                                     Atom_Common'(Base_URI =>
                                                    Null_Unbounded_String,
                                                  Language =>
                                                    Null_Unbounded_String),
                                   Text_Content => Null_Unbounded_String,
                                   Text_Kind    => Text),
                      Updated       =>
                        Atom_Date'(Common     =>
                                     Atom_Common'(Base_URI =>
                                                    Null_Unbounded_String,
                                                  Language =>
                                                    Null_Unbounded_String),
                                   Time_Stamp => Now));

      end return;

   end New_Atom_Entry;

   ---------------------
   --  New_Atom_Feed  --
   ---------------------

   function New_Atom_Feed
     (Base_URI    : in String := None;
      Language    : in String := None;
      Max_Age     : in Duration := 5_616_000.0;
      Max_Entries : in Positive := 100;
      Min_Entries : in Positive := 10)
      return Atom_Feed
   is

      Common : constant Atom_Common := (Base_URI => TUS (Base_URI),
                                        Language => TUS (Language));

   begin

      return Feed : Atom_Feed do
         Feed.PAF.Set_Common (Value => Common);
         Feed.PAF.Set_Max_age (Value => Max_Age);
         Feed.PAF.Set_Max_Entries (Value => Max_Entries);
         Feed.PAF.Set_Min_Entries (Value => Min_Entries);
      end return;

   end New_Atom_Feed;

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

   -------------------------
   --  Set_Common_Source  --
   -------------------------

   procedure Set_Common_Source
     (Entr     : in out Atom_Entry;
      Base_URI : in     String := None;
      Language : in     String := None)
   is
   begin

      Entr.Source.Common := Atom_Common'(Base_URI => TUS (Base_URI),
                                         Language => TUS (Language));

   end Set_Common_Source;

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

   ----------------------------
   --  Set_Generator_Source  --
   ----------------------------

   procedure Set_Generator_Source
     (Entr     : in out Atom_Entry;
      Agent    : in     String;
      Base_URI : in     String := None;
      Language : in     String := None;
      URI      : in     String := None;
      Version  : in     String := None)
   is
   begin

      Entr.Source.Generator := Atom_Generator'
        (Agent   => TUS (Agent),
         Common  =>
           Atom_Common'(Base_URI => TUS (Base_URI),
                        Language => TUS (Language)),
         URI     => TUS (URI),
         Version => TUS (Version));

   end Set_Generator_Source;

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

   -----------------------
   --  Set_Icon_Source  --
   -----------------------

   procedure Set_Icon_Source
     (Entr     : in out Atom_Entry;
      URI      : in     String;
      Base_URI : in     String := None;
      Language : in     String := None)
   is
   begin

      Entr.Source.Icon := Atom_Icon'
        (Common =>
           Atom_Common'(Base_URI => TUS (Base_URI),
                        Language => TUS (Language)),
         URI    => TUS (URI));

   end Set_Icon_Source;

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

   ---------------------
   --  Set_Id_Source  --
   ---------------------

   procedure Set_Id_Source
     (Entr     : in out Atom_Entry;
      Id       : in     String;
      Base_URI : in     String := None;
      Language : in     String := None)
   is
   begin

      Entr.Source.Id := Atom_Id'(Common =>
                                   Atom_Common'(Base_URI => TUS (Base_URI),
                                                Language => TUS (Language)),
                                 URI    => TUS (Id));

   end Set_Id_Source;

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

   -----------------------
   --  Set_Logo_Source  --
   -----------------------

   procedure Set_Logo_Source
     (Entr     : in out Atom_Entry;
      URI      : in String;
      Base_URI : in String := None;
      Language : in String := None)
   is
   begin

      Entr.Source.Logo := Atom_Logo'
        (Common =>
           Atom_Common'(Base_URI => TUS (Base_URI),
                        Language => TUS (Language)),
         URI    => TUS (URI));

   end Set_Logo_Source;

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

   -------------------------
   --  Set_Rights_Source  --
   -------------------------

   procedure Set_Rights_Source
     (Entr        : in out Atom_Entry;
      Rights      : in String;
      Base_URI    : in String := None;
      Language    : in String := None;
      Rights_Kind : in Text_Kinds := Text)
   is
   begin

      Entr.Source.Rights := Atom_Text'
        (Common       =>
           Atom_Common'(Base_URI => TUS (Base_URI),
                        Language => TUS (Language)),
         Text_Content => TUS (Rights),
         Text_Kind    => Rights_Kind);

   end Set_Rights_Source;

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

   ---------------------------
   --  Set_Subtitle_Source  --
   ---------------------------

   procedure Set_Subtitle_Source
     (Entr           : in out Atom_Entry;
      Subtitle       : in String;
      Base_URI       : in String := None;
      Language       : in String := None;
      Subtitle_Kind  : in Text_Kinds := Text)
   is
   begin

      Entr.Source.Subtitle := Atom_Text'
        (Common       =>
           Atom_Common'(Base_URI => TUS (Base_URI),
                        Language => TUS (Language)),
         Text_Content => TUS (Subtitle),
         Text_Kind    => Subtitle_Kind);

   end Set_Subtitle_Source;

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

   ------------------------
   --  Set_Title_Source  --
   ------------------------

   procedure Set_Title_Source
     (Entr       : in out Atom_Entry;
      Title      : in     String;
      Base_URI   : in     String := None;
      Language   : in     String := None;
      Title_Kind : in     Text_Kinds := Text)
   is
   begin

      Entr.Source.Title := Atom_Text'
        (Common       =>
           Atom_Common'(Base_URI => TUS (Base_URI),
                        Language => TUS (Language)),
         Text_Content => TUS (Title),
         Text_Kind    => Title_Kind);

   end Set_Title_Source;

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
                                 Time_Stamp => Update_Time);

   end Set_Updated;

   --------------------------
   --  Set_Updated_Source  --
   --------------------------

   procedure Set_Updated_Source
     (Entr        : in out Atom_Entry;
      Update_Time : in     Ada.Calendar.Time;
      Base_URI    : in     String := None;
      Language    : in     String := None)
   is
   begin

      Entr.Source.Updated := Atom_Date'
        (Common     =>
           Atom_Common'(Base_URI => TUS (Base_URI),
                        Language => TUS (Language)),
         Time_Stamp => Update_Time);

   end Set_Updated_Source;

   --------------------
   --  PT_Atom_Feed  --
   --------------------

   protected body PT_Atom_Feed is

      ------------------
      --  Add_Author  --
      ------------------

      procedure Add_Author
        (Value : in Atom_Person)
      is
      begin

         Authors.Append (Value);

      end Add_Author;

      --------------------
      --  Add_Category  --
      --------------------

      procedure Add_Category
        (Value : in Atom_Category)
      is
      begin

         Categories.Append (Value);

      end Add_Category;

      -----------------------
      --  Add_Contributor  --
      -----------------------

      procedure Add_Contributor
        (Value : in Atom_Person)
      is
      begin

         Contributors.Append (Value);

      end Add_Contributor;

      -----------------
      --  Add_Entry  --
      -----------------

      procedure Add_Entry
        (Value       : in Atom_Entry;
         Entry_Added : out Boolean)
      is

         use Ada.Calendar;
         use Entry_List;

         procedure Insert_Entry
           (Value : in Atom_Entry;
            Done  : out Boolean);
         --  Insert the Value into List sorted by Atom_Entry.Updated

         --------------------
         --  Insert_Entry  --
         --------------------

         procedure Insert_Entry
           (Value : in Atom_Entry;
            Done  : out Boolean)
         is

            C : Cursor;

         begin

            if Entries.Is_Empty then
               Entries.Append (New_Item => Value);
               Done := True;
            elsif Value.Updated.Time_Stamp <=
              Entries.Last_Element.Updated.Time_Stamp
            then
               Entries.Append (New_Item => Value);
               Done := True;
            elsif Value.Updated.Time_Stamp >=
              Entries.First_Element.Updated.Time_Stamp
            then
               Entries.Prepend (New_Item => Value);
               Done := True;
            else
               C := Entries.First;
               while Has_Element (C) loop
                  if Value.Updated.Time_Stamp >=
                    Element (C).Updated.Time_Stamp
                  then
                     Entries.Insert (Before   => C,
                                     New_Item => Value);
                     Done := True;
                     exit;
                  end if;

                  Next (C);
               end loop;
            end if;

         end Insert_Entry;

         C : Cursor;
         Counter : Natural := Natural (Entries.Length);
         Now : constant Time := Clock;

      begin

         Entry_Added := False;

         C := Find (Container => Entries,
                    Item      => Value);
         if  C /= No_Element then
            Entries.Delete (Position => C);
         end if;

         if Entries.Length >= Count_Type (Max_Entries) then
            Entries.Delete_Last
              (Count => Entries.Length - (Count_Type (Max_Entries - 1)));
         end if;

         C := Entries.Last;
         loop
            exit when Counter <= Min_Entries;

            if Now - Element (C).Updated.Time_Stamp > Max_Entry_Age then
               Entries.Delete (Position => C);
               C := Entries.Last;
            else
               Previous (C);
            end if;

            Counter := Counter - 1;
         end loop;

         if Entries.Length < Count_Type (Max_Entries)
           or Clock - Value.Updated.Time_Stamp <= Max_Entry_Age
         then
            Insert_Entry (Value => Value,
                          Done  => Entry_Added);
         end if;

         if Entry_Added
           and then Value.Updated.Time_Stamp < Updated.Time_Stamp
         then
            Updated.Time_Stamp := Value.Updated.Time_Stamp;
         end if;

      end Add_Entry;

      ----------------
      --  Add_Link  --
      ----------------

      procedure Add_Link
        (Value : in Atom_Link)
      is
      begin

         Links.Append (Value);

      end Add_Link;

      ------------------------
      --  Clear_Entry_List  --
      ------------------------

      procedure Clear_Entry_List
      is
      begin

         Entries.Clear;

      end Clear_Entry_List;

      --------------------
      --  Delete_Entry  --
      --------------------

      procedure Delete_Entry
        (Id : in String)
      is

         use Entry_List;

         C : Cursor;

      begin

         C := Entries.First;
         while Has_Element (C) loop
            if Element (C).Id.URI = TUS (Id) then
               Entries.Delete (C);
            end if;

            Next (C);
         end loop;

      end Delete_Entry;

      ---------------
      --  Get_DOM  --
      ---------------

      function Get_DOM return DOM.Core.Document
      is

         use Ada.Calendar;
         use DOM.Core;
         use DOM.Core.Documents;
         use DOM.Core.Elements;
         use DOM.Core.Nodes;

         Doc         : Document;
         Impl        : DOM_Implementation;
         Feed_Node   : Node;

         procedure Attribute
           (Elem  : in Node;
            Name  : in String;
            Value : in String);
         --  Add the attribute Name to Elem if Value isn't empty.

         procedure Create_Category_Elements
           (List   : in Category_List.List;
            Parent : in Node);
         --  Add atom:category elements to Parent.

         procedure Create_Content_Element
           (Entry_Content  : in Atom_Entry_Content;
            Parent         : in Node);
         --  Add atom:content element to Parent.

         procedure Create_Generator_Element
           (A_Generator : in Atom_Generator;
            Parent      : in Node);
         --  Add atom:generator element to Parent.

         procedure Create_Generic_Element
           (Common    : in Atom_Common;
            Data      : in String;
            Elem_Name : in String;
            Parent    : in Node);
         --  Add a generic element to Parent. A generic element has the
         --  following structure:
         --
         --    <Elem_Name base="Common.Base_URI" lang="Common.Language">
         --       Data
         --    </Elem_Name>

         procedure Create_Link_Elements
           (List   : in Link_List.List;
            Parent : in Node);
         --  Add atom:link elements to Parent.

         procedure Create_Person_Elements
           (Elem_Name   : in String;
            List        : in Person_List.List;
            Parent      : in Node);
         --  Add atom:person elements to Parent.

         procedure Create_Source_Element
           (Source : in Atom_Entry_Source;
            Parent : in Node);
         --  Add an atom:source element to Parent.

         procedure Create_Text_Construct
           (Common    : in Atom_Common;
            Data      : in String;
            Elem_Name : in String;
            Parent    : in Node;
            Text_Kind : in Text_Kinds);
         --  Set the type (text/html/xhtml) and content of an atomTextConstruct
         --  element.

         -----------------
         --  Attribute  --
         -----------------

         procedure Attribute
           (Elem  : in Node;
            Name  : in String;
            Value : in String)
         is
         begin

            if Value /= "" then
               Set_Attribute (Elem  => Elem,
                              Name  => Name,
                              Value => Value);
            end if;

         end Attribute;

         --------------------------------
         --  Create_Category_Elements  --
         --------------------------------

         procedure Create_Category_Elements
           (List   : in Category_List.List;
            Parent : in Node)
         is

            A_Category     : Atom_Category;
            C              : Category_List.Cursor := List.First;
            Category_Node  : Node;

         begin

            loop
               exit when not Category_List.Has_Element (C);

               A_Category := Category_List.Element (C);

               Category_Node := Append_Child
                 (N         => Parent,
                  New_Child => Create_Element (Doc      => Doc,
                                               Tag_Name => "category"));

               Set_Attribute (Elem  => Category_Node,
                              Name  => "term",
                              Value => TS (A_Category.Term));

               Attribute (Elem  => Category_Node,
                          Name  => "base",
                          Value => TS (A_Category.Common.Base_URI));

               Attribute (Elem  => Category_Node,
                          Name  => "lang",
                          Value => TS (A_Category.Common.Language));

               Attribute (Elem  => Category_Node,
                          Name  => "label",
                          Value => TS (A_Category.Label));

               Attribute (Elem  => Category_Node,
                          Name  => "scheme",
                          Value => TS (A_Category.Scheme));

               if A_Category.Content /= Null_Unbounded_String then
                  Category_Node := Append_Child
                    (N         => Category_Node,
                     New_Child => Create_Text_Node
                       (Doc  => Doc,
                        Data => TS (A_Category.Content)));
               end if;

               Category_List.Next (C);
            end loop;

         end Create_Category_Elements;

         ------------------------------
         --  Create_Content_Element  --
         ------------------------------

         procedure Create_Content_Element
           (Entry_Content  : in Atom_Entry_Content;
            Parent         : in Node)
         is

            Content_Node : Node;

         begin

            case Entry_Content.Content_Kind is
               when Text | Html | Xhtml =>
                  Create_Text_Construct
                    (Common    => Entry_Content.Common,
                     Data      => TS (Entry_Content.Content),
                     Elem_Name => "content",
                     Parent    => Parent,
                     Text_Kind => Entry_Content.Content_Kind);
               when others =>
                  Content_Node := Append_Child
                    (N         => Parent,
                     New_Child => Create_Element (Doc      => Doc,
                                                  Tag_Name => "content"));

                  Attribute (Elem  => Content_Node,
                             Name  => "base",
                             Value => TS (Entry_Content.Common.Base_URI));

                  Attribute (Elem  => Content_Node,
                             Name  => "lang",
                             Value => TS (Entry_Content.Common.Language));

                  Attribute (Elem  => Content_Node,
                             Name  => "type",
                             Value => TS (Entry_Content.Mime_Type));

                  if Entry_Content.Source /= Null_Unbounded_String then
                     Attribute (Elem  => Content_Node,
                                Name  => "src",
                                Value => TS (Entry_Content.Source));
                  else
                     Content_Node := Append_Child
                       (N         => Content_Node,
                        New_Child => Create_Text_Node
                          (Doc  => Doc,
                           Data => TS (Entry_Content.Content)));
                  end if;
            end case;

         end Create_Content_Element;

         --------------------------------
         --  Create_Generator_Element  --
         --------------------------------

         procedure Create_Generator_Element
           (A_Generator : in Atom_Generator;
            Parent      : in Node)
         is

            Generator_Node : Node;

         begin

            if A_Generator.Agent /= Null_Unbounded_String then
               Generator_Node := Append_Child
                 (N         => Parent,
                  New_Child => Create_Element (Doc      => Doc,
                                               Tag_Name => "generator"));

               Attribute (Elem  => Generator_Node,
                          Name  => "base",
                          Value => TS (A_Generator.Common.Base_URI));

               Attribute (Elem  => Generator_Node,
                          Name  => "lang",
                          Value => TS (A_Generator.Common.Language));

               Attribute (Elem  => Generator_Node,
                          Name  => "uri",
                          Value => TS (A_Generator.URI));

               Attribute (Elem  => Generator_Node,
                          Name  => "version",
                          Value => TS (A_Generator.Version));

               Generator_Node := Append_Child
                 (N         => Generator_Node,
                  New_Child => Create_Text_Node
                    (Doc  => Doc,
                     Data => TS (A_Generator.Agent)));
            end if;

         end Create_Generator_Element;

         ------------------------------
         --  Create_Generic_Element  --
         ------------------------------

         procedure Create_Generic_Element
           (Common      : in Atom_Common;
            Data        : in String;
            Elem_Name   : in String;
            Parent      : in Node)
         is

            Elem_Node : Node;

         begin

            Elem_Node := Append_Child
              (N         => Parent,
               New_Child => Create_Element (Doc      => Doc,
                                            Tag_Name => Elem_Name));

            Attribute (Elem  => Elem_Node,
                       Name  => "base",
                       Value => TS (Common.Base_URI));

            Attribute (Elem  => Elem_Node,
                       Name  => "lang",
                       Value => TS (Common.Language));

            Elem_Node := Append_Child
              (N         => Elem_Node,
               New_Child => Create_Text_Node (Doc  => Doc,
                                              Data => Data));
            pragma Unreferenced (Elem_Node);
            --  We need this because XML/Ada have no Append_Child procedures,
            --  which obviously is annoying as hell.

         end Create_Generic_Element;

         ----------------------------
         --  Create_Link_Elements  --
         ----------------------------

         procedure Create_Link_Elements
           (List   : in Link_List.List;
            Parent : in Node)
         is

            use Ada.Strings;

            A_Link      : Atom_Link;
            C           : Link_List.Cursor := List.First;
            Link_Node   : Node;

         begin

            loop
               exit when not Link_List.Has_Element (C);

               A_Link := Link_List.Element (C);

               Link_Node := Append_Child
                 (N         => Parent,
                  New_Child => Create_Element (Doc      => Doc,
                                               Tag_Name => "link"));

               case A_Link.Rel is
                  when Alternate =>
                     Set_Attribute (Elem  => Link_Node,
                                    Name  => "rel",
                                    Value => "alternate");
                  when Related =>
                     Set_Attribute (Elem  => Link_Node,
                                    Name  => "rel",
                                    Value => "related");
                  when Self =>
                     Set_Attribute (Elem  => Link_Node,
                                    Name  => "rel",
                                    Value => "self");
                  when Enclosure =>
                     Set_Attribute (Elem  => Link_Node,
                                    Name  => "rel",
                                    Value => "enclosure");
                  when Via =>
                     Set_Attribute (Elem  => Link_Node,
                                    Name  => "rel",
                                    Value => "via");
               end case;

               Set_Attribute (Elem  => Link_Node,
                              Name  => "href",
                              Value => TS (A_Link.Href));

               Attribute (Elem  => Link_Node,
                          Name  => "hreflang",
                          Value => TS (A_Link.Hreflang));

               if A_Link.Length > 0 then
                  Set_Attribute
                    (Elem  => Link_Node,
                     Name  => "length",
                     Value => Fixed.Trim
                       (Source => Natural'Image (A_Link.Length),
                        Side   => Left));
               end if;

               Attribute (Elem  => Link_Node,
                          Name  => "type",
                          Value => TS (A_Link.Mime_Type));

               Attribute (Elem  => Link_Node,
                          Name  => "title",
                          Value => TS (A_Link.Title));

               if A_Link.Content /= Null_Unbounded_String then
                  Link_Node := Append_Child
                    (N         => Link_Node,
                     New_Child => Create_Text_Node
                       (Doc  => Doc,
                        Data => TS (A_Link.Content)));
               end if;

               Link_List.Next (C);
            end loop;

         end Create_Link_Elements;

         ------------------------------
         --  Create_Person_Elements  --
         ------------------------------

         procedure Create_Person_Elements
           (Elem_Name   : in String;
            List        : in Person_List.List;
            Parent      : in Node)
         is

            A_Person    : Atom_Person;
            Person_Node : Node;
            C           : Person_List.Cursor := List.First;
            Elem_Node   : Node;

         begin

            loop
               exit when not Person_List.Has_Element (C);

               A_Person := Person_List.Element (C);

               Person_Node := Append_Child
                 (N         => Parent,
                  New_Child => Create_Element (Doc      => Doc,
                                               Tag_Name => Elem_Name));

               Attribute (Elem  => Person_Node,
                          Name  => "base",
                          Value => TS (A_Person.Common.Base_URI));

               Attribute (Elem  => Person_Node,
                          Name  => "lang",
                          Value => TS (A_Person.Common.Language));

               Elem_Node := Append_Child
                 (N         => Person_Node,
                  New_Child => Create_Element (Doc      => Doc,
                                               Tag_Name => "name"));
               Elem_Node := Append_Child
                 (N         => Elem_Node,
                  New_Child => Create_Text_Node (Doc  => Doc,
                                                 Data => TS (A_Person.Name)));

               if A_Person.Email /= Null_Unbounded_String then
                  Elem_Node := Append_Child
                    (N         => Person_Node,
                     New_Child => Create_Element (Doc      => Doc,
                                                  Tag_Name => "email"));
                  Elem_Node := Append_Child
                    (N         => Elem_Node,
                     New_Child => Create_Text_Node
                       (Doc  => Doc,
                        Data => TS (A_Person.Email)));
               end if;

               if A_Person.URI /= Null_Unbounded_String then
                  Elem_Node := Append_Child
                    (N         => Person_Node,
                     New_Child => Create_Element (Doc      => Doc,
                                                  Tag_Name => "uri"));
                  Elem_Node := Append_Child
                    (N         => Elem_Node,
                     New_Child => Create_Text_Node
                       (Doc  => Doc,
                        Data => TS (A_Person.URI)));
               end if;

               Person_List.Next (C);
            end loop;

         end Create_Person_Elements;

         -----------------------------
         --  Create_Source_Element  --
         -----------------------------

         procedure Create_Source_Element
           (Source : in Atom_Entry_Source;
            Parent : in Node)
         is

            Source_Node : Node;

         begin

            Source_Node := Append_Child
              (N         => Parent,
               New_Child => Create_Element (Doc      => Doc,
                                            Tag_Name => "source"));

            Attribute (Elem  => Source_Node,
                       Name  => "base",
                       Value => TS (Source.Common.Base_URI));

            Attribute (Elem  => Source_Node,
                       Name  => "lang",
                       Value => TS (Source.Common.Language));

            Create_Person_Elements (Elem_Name   => "author",
                                    List        => Source.Authors,
                                    Parent      => Source_Node);

            Create_Category_Elements (List   => Source.Categories,
                                      Parent => Source_Node);

            Create_Person_Elements (Elem_Name => "contributor",
                                    List      => Source.Contributors,
                                    Parent    => Source_Node);

            Create_Generator_Element (A_Generator => Source.Generator,
                                      Parent      => Source_Node);

            if Source.Icon.URI /= Null_Unbounded_String then
               Create_Generic_Element (Common    => Source.Icon.Common,
                                       Data      => TS (Source.Icon.URI),
                                       Elem_Name => "icon",
                                       Parent    => Source_Node);
            end if;

            Create_Generic_Element (Common    => Source.Id.Common,
                                    Data      => TS (Source.Id.URI),
                                    Elem_Name => "id",
                                    Parent    => Source_Node);

            Create_Link_Elements (List   => Source.Links,
                                  Parent => Source_Node);

            if Source.Logo.URI /= Null_Unbounded_String then
               Create_Generic_Element (Common    => Source.Logo.Common,
                                       Data      => TS (Source.Logo.URI),
                                       Elem_Name => "logo",
                                       Parent    => Source_Node);
            end if;

            if Source.Rights.Text_Content /= Null_Unbounded_String then
               Create_Text_Construct
                 (Common    => Source.Rights.Common,
                  Data      => TS (Source.Rights.Text_Content),
                  Elem_Name => "rights",
                  Parent    => Source_Node,
                  Text_Kind => Source.Rights.Text_Kind);
            end if;

            if Source.Subtitle.Text_Content /= Null_Unbounded_String then
               Create_Text_Construct
                 (Common    => Source.Subtitle.Common,
                  Data      => TS (Source.Subtitle.Text_Content),
                  Elem_Name => "subtitle",
                  Parent    => Source_Node,
                  Text_Kind => Source.Subtitle.Text_Kind);
            end if;

            Create_Text_Construct (Common    => Source.Title.Common,
                                   Data      => TS (Source.Title.Text_Content),
                                   Elem_Name => "title",
                                   Parent    => Source_Node,
                                   Text_Kind => Source.Title.Text_Kind);

            Create_Generic_Element
              (Common    => Source.Updated.Common,
               Data      =>
                 Atom_Date_Image (Time_Stamp => Source.Updated.Time_Stamp),
               Elem_Name => "updated",
               Parent    => Source_Node);

         end Create_Source_Element;

         -----------------------------
         --  Create_Text_Construct  --
         -----------------------------

         procedure Create_Text_Construct
           (Common    : in Atom_Common;
            Data      : in String;
            Elem_Name : in String;
            Parent    : in Node;
            Text_Kind : in Text_Kinds)
         is

            Elem_Node : Node;

         begin

            Elem_Node := Append_Child
              (N         => Parent,
               New_Child => Create_Element (Doc      => Doc,
                                            Tag_Name => Elem_Name));

            Attribute (Elem  => Elem_Node,
                       Name  => "base",
                       Value => TS (Common.Base_URI));

            Attribute (Elem  => Elem_Node,
                       Name  => "lang",
                       Value => TS (Common.Language));

            case Text_Kind is
               when Text =>
                  Set_Attribute (Elem  => Elem_Node,
                                 Name  => "type",
                                 Value => "text");
                  Elem_Node := Append_Child
                    (N         => Elem_Node,
                     New_Child => Create_Text_Node
                       (Doc  => Doc,
                        Data => Data));
               when Html =>
                  Set_Attribute (Elem  => Elem_Node,
                                 Name  => "type",
                                 Value => "html");
                  Elem_Node := Append_Child
                    (N         => Elem_Node,
                     New_Child => Create_Text_Node
                       (Doc  => Doc,
                        Data => Data));
               when Xhtml =>
                  Set_Attribute (Elem  => Elem_Node,
                                 Name  => "type",
                                 Value => "xhtml");

                  Elem_Node := Append_Child
                    (N         => Elem_Node,
                     New_Child => Create_Node_From_String
                       (XML_String =>
                        "<div " & DIVNS & ">" & Data & "</div>"));

            end case;

         end Create_Text_Construct;

      begin

         Doc := Create_Document (Implementation => Impl);

         --  feed element
         Feed_Node := Append_Child
           (N => Doc,
            New_Child => Create_Element (Doc      => Doc,
                                         Tag_Name => "feed"));

         Set_Attribute (Elem  => Feed_Node,
                        Name  => "xmlns",
                        Value => XMLNS);

         Attribute (Elem  => Feed_Node,
                    Name  => "base",
                    Value => TS (Common.Base_URI));

         Attribute (Elem  => Feed_Node,
                    Name  => "lang",
                    Value => TS (Common.Language));

         --  feed:author elements
         Create_Person_Elements (Elem_Name   => "author",
                                 List        => Authors,
                                 Parent      => Feed_Node);

         --  feed:category elements
         Create_Category_Elements (List   => Categories,
                                   Parent => Feed_Node);

         --  feed:contributor elements
         Create_Person_Elements (Elem_Name   => "contributor",
                                 List        => Contributors,
                                 Parent      => Feed_Node);

         --  feed:generator element
         Create_Generator_Element (A_Generator  => Generator,
                                   Parent       => Feed_Node);

         --  feed:icon element
         if Icon.URI /= Null_Unbounded_String then
            Create_Generic_Element (Common    => Icon.Common,
                                    Data      => TS (Icon.URI),
                                    Elem_Name => "icon",
                                    Parent    => Feed_Node);
         end if;

         --  feed:id element
         Create_Generic_Element (Common    => Id.Common,
                                 Data      => TS (Id.URI),
                                 Elem_Name => "id",
                                 Parent    => Feed_Node);

         --  feed:link elements
         Create_Link_Elements (List   => Links,
                               Parent => Feed_Node);

         --  feed:logo
         if Logo.URI /= Null_Unbounded_String then
            Create_Generic_Element (Common    => Logo.Common,
                                    Data      => TS (Logo.URI),
                                    Elem_Name => "logo",
                                    Parent    => Feed_Node);
         end if;

         --  feed:rights
         if Rights.Text_Content /= Null_Unbounded_String then
            Create_Text_Construct (Common    => Rights.Common,
                                   Data      => TS (Rights.Text_Content),
                                   Elem_Name => "rights",
                                   Parent    => Feed_Node,
                                   Text_Kind => Rights.Text_Kind);
         end if;

         --  feed:subtitle
         if Subtitle.Text_Content /= Null_Unbounded_String then
            Create_Text_Construct (Common    => Subtitle.Common,
                                   Data      => TS (Subtitle.Text_Content),
                                   Elem_Name => "subtitle",
                                   Parent    => Feed_Node,
                                   Text_Kind => Subtitle.Text_Kind);
         end if;

         --  feed:title element
         Create_Text_Construct (Common    => Title.Common,
                                Data      => TS (Title.Text_Content),
                                Elem_Name => "title",
                                Parent    => Feed_Node,
                                Text_Kind => Title.Text_Kind);

         --  feed:updated element
         Create_Generic_Element
           (Common    => Updated.Common,
            Data      => Atom_Date_Image (Time_Stamp => Updated.Time_Stamp),
            Elem_Name => "updated",
            Parent    => Feed_Node);

         --  feed:entry
         Add_Entries_To_DOM :
         declare

            An_Entry    : Atom_Entry;
            C           : Entry_List.Cursor := Entries.First;
            Entry_Node  : Node;

         begin

            loop
               exit when not Entry_List.Has_Element (C);

               An_Entry := Entry_List.Element (C);

               Entry_Node := Append_Child
                 (N         => Feed_Node,
                  New_Child => Create_Element (Doc      => Doc,
                                               Tag_Name => "entry"));

               Attribute (Elem  => Entry_Node,
                          Name  => "base",
                          Value => TS (An_Entry.Common.Base_URI));

               Attribute (Elem  => Entry_Node,
                          Name  => "lang",
                          Value => TS (An_Entry.Common.Language));

               --  entry:author elements
               Create_Person_Elements (Elem_Name => "author",
                                       List      => An_Entry.Authors,
                                       Parent    => Entry_Node);

               --  entry:category elements
               Create_Category_Elements (List   => An_Entry.Categories,
                                         Parent => Entry_Node);

               --  entry:content element
               Create_Content_Element (Entry_Content  => An_Entry.Content,
                                       Parent         => Entry_Node);

               --  entry:contributor elements
               Create_Person_Elements (Elem_Name => "contributor",
                                       List      => An_Entry.Contributors,
                                       Parent    => Entry_Node);

               --  entry:id element
               Create_Generic_Element (Common    => An_Entry.Id.Common,
                                       Data      => TS (An_Entry.Id.URI),
                                       Elem_Name => "id",
                                       Parent    => Entry_Node);

               --  entry:link elements
               Create_Link_Elements (List   => An_Entry.Links,
                                     Parent => Entry_Node);

               --  entry:published element
               Create_Generic_Element
                 (Common    => An_Entry.Published.Common,
                  Data      => Atom_Date_Image
                    (Time_Stamp => An_Entry.Published.Time_Stamp),
                  Elem_Name => "published",
                  Parent    => Entry_Node);

               --  entry:rights
               if An_Entry.Rights.Text_Content /= Null_Unbounded_String then
                  Create_Text_Construct
                    (Common    => An_Entry.Rights.Common,
                     Data      => TS (An_Entry.Rights.Text_Content),
                     Elem_Name => "rights",
                     Parent    => Entry_Node,
                     Text_Kind => An_Entry.Rights.Text_Kind);
               end if;

               --  entry:source element
               Create_Source_Element (Source => An_Entry.Source,
                                      Parent => Entry_Node);

               --  entry:summary element
               Create_Text_Construct
                 (Common    => An_Entry.Summary.Common,
                  Data      => TS (An_Entry.Summary.Text_Content),
                  Elem_Name => "summary",
                  Parent    => Entry_Node,
                  Text_Kind => An_Entry.Summary.Text_Kind);

               --  entry:title element
               Create_Text_Construct
                 (Common    => An_Entry.Title.Common,
                  Data      => TS (An_Entry.Title.Text_Content),
                  Elem_Name => "title",
                  Parent    => Entry_Node,
                  Text_Kind => An_Entry.Title.Text_Kind);

               --  entry:updated element
               Create_Generic_Element
                 (Common    => An_Entry.Common,
                  Data      => Atom_Date_Image
                    (Time_Stamp => An_Entry.Updated.Time_Stamp),
                  Elem_Name => "updated",
                  Parent    => Entry_Node);

               Entry_List.Next (C);
            end loop;

         end Add_Entries_To_DOM;

         return Doc;

      end Get_DOM;

      ------------------
      --  Get_String  --
      ------------------

      function Get_String return String
      is

         use Ada.Streams;
         use DOM.Core.Nodes;

         type String_Stream_Type is new Root_Stream_Type with record
            Str        : Unbounded_String;
            Read_Index : Natural := 1;
         end record;

         procedure Read
           (Stream : in out String_Stream_Type;
            Item   :    out Stream_Element_Array;
            Last   :    out Stream_Element_Offset);

         procedure Write
           (Stream : in out String_Stream_Type;
            Item   : Stream_Element_Array);

         ----------
         -- Read --
         ----------

         procedure Read
           (Stream : in out String_Stream_Type;
            Item   :    out Stream_Element_Array;
            Last   :    out Stream_Element_Offset)
         is

            Str : constant String := Slice
              (Stream.Str,
               Stream.Read_Index,
               Stream.Read_Index + Item'Length - 1);
            J   : Stream_Element_Offset := Item'First;

         begin

            for S in Str'Range loop
               Item (J) := Stream_Element (Character'Pos (Str (S)));
               J := J + 1;
            end loop;

            Last := Item'First + Str'Length - 1;
            Stream.Read_Index := Stream.Read_Index + Item'Length;

         end Read;

         -----------
         -- Write --
         -----------

         procedure Write
           (Stream : in out String_Stream_Type;
            Item   : Stream_Element_Array)
         is

            Str : String (1 .. Integer (Item'Length));
            S   : Integer := Str'First;

         begin

            for J in Item'Range loop
               Str (S) := Character'Val (Item (J));
               S := S + 1;
            end loop;

            Append (Stream.Str, Str);

         end Write;

         Output   : aliased String_Stream_Type;
         Doc      : DOM.Core.Document := Get_DOM;

      begin

         DOM.Core.Nodes.Write (Stream                 => Output'Access,
                               N                      => Doc,
                               Print_Comments         => False,
                               Print_XML_Declaration  => False,
                               Pretty_Print           => True);

         Free (Doc);

         return PI & TS (Output.Str);

      end Get_String;

      ------------------
      --  Set_Common  --
      ------------------

      procedure Set_Common
        (Value : in Atom_Common)
      is
      begin

         Common := Value;

      end Set_Common;

      ---------------------
      --  Set_Generator  --
      ---------------------

      procedure Set_Generator
        (Value : in Atom_Generator)
      is
      begin

         Generator := Value;

      end Set_Generator;

      ----------------
      --  Set_Icon  --
      ----------------

      procedure Set_Icon
        (Value : in Atom_Icon)
      is
      begin

         Icon := Value;

      end Set_Icon;

      --------------
      --  Set_Id  --
      --------------

      procedure Set_Id
        (Value : in Atom_Id)
      is
      begin

         Id := Value;

      end Set_Id;

      ----------------
      --  Set_Logo  --
      ----------------

      procedure Set_Logo
        (Value : in Atom_Logo)
      is
      begin

         Logo := Value;

      end Set_Logo;

      -------------------
      --  Set_Max_Age  --
      -------------------

      procedure Set_Max_Age
        (Value : in Duration)
      is
      begin

         Max_Entry_Age := Value;

      end Set_Max_Age;

      -----------------------
      --  Set_Max_Entries  --
      -----------------------

      procedure Set_Max_Entries
        (Value : in Positive)
      is
      begin

         Max_Entries := Value;

      end Set_Max_Entries;

      -----------------------
      --  Set_Min_Entries  --
      -----------------------

      procedure Set_Min_Entries
        (Value : in Positive)
      is
      begin

         Min_Entries := Value;

      end Set_Min_Entries;

      ------------------
      --  Set_Rights  --
      ------------------

      procedure Set_Rights
        (Value : in Atom_Text)
      is
      begin

         Rights := Value;

      end Set_Rights;

      --------------------
      --  Set_Subtitle  --
      --------------------

      procedure Set_Subtitle
        (Value : in Atom_Text)
      is
      begin

         Subtitle := Value;

      end Set_Subtitle;

      -----------------
      --  Set_Title  --
      -----------------

      procedure Set_Title
        (Value : Atom_Text)
      is
      begin

         Title := Value;

      end Set_Title;

      -------------------
      --  Set_Updated  --
      -------------------

      procedure Set_Updated_Time
        (Value : in Atom_Date)
      is

         use Ada.Calendar;

      begin

         Updated := Value;

      end Set_Updated_Time;

   end PT_Atom_Feed;

end Yolk.Syndication.Writer;
