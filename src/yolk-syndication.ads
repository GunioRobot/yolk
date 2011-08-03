-------------------------------------------------------------------------------
--                                                                           --
--                                  Yolk                                     --
--                                                                           --
--                             Yolk.Syndication                              --
--                                                                           --
--                                  SPEC                                     --
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

with Ada.Calendar;
with Ada.Characters.Latin_1;
private with Ada.Containers.Doubly_Linked_Lists;
private with Ada.Strings.Unbounded;
with DOM.Core;

package Yolk.Syndication is

   Not_Valid_XML : exception;
   --  Is raised when some Xhtml content is not valid XML. This exception
   --  can be raised by all procedures that can take Xhtml as content.

   type Content_Kinds is (Text, Html, Xhtml, InlineOther, OutOfLineOther);
   --  This type is common for a lot of Atom feed XML elements. It identifies
   --  the kind of data found in the element.
   --  Text:
   --    The content of the Text construct MUST NOT contain child elements.
   --    Such text is intended to be presented to humans in a readable fashion.
   --    Thus, Atom Processors MAY collapse white space (including line breaks)
   --    and display the text using typographic techniques such as
   --    justification and proportional fonts.
   --  Html:
   --    The content of the Text construct MUST NOT contain child elements and
   --    SHOULD be suitable for handling as HTML [HTML]. Any markup within is
   --    escaped; for example, "<br>" as "&lt;br>". Atom Processors that
   --    display such content MAY use that markup to aid in its display.
   --  Xhtml:
   --    The content SHOULD be suitable for handling as XHTML. The content is
   --    wrapped in a <div> element. The XHTML <div> element itself MUST NOT be
   --    considered part of the content. Atom Processors that display the
   --    content MAY use the markup to aid in displaying it. The escaped
   --    versions of characters such as "&" and ">" represent those characters,
   --    not markup.
   --  InlineOther:
   --    For inline content that is not Text, Html or Xhtml.
   --  OutOfLineOther:
   --    For content that is fetched from a given IRI reference.

   subtype Text_Kinds is Content_Kinds range Text .. Xhtml;
   --  Text:
   --    The content of the Text construct MUST NOT contain child elements.
   --    Such text is intended to be presented to humans in a readable fashion.
   --    Thus, Atom Processors MAY collapse white space (including line breaks)
   --    and display the text using typographic techniques such as
   --    justification and proportional fonts.
   --  Html:
   --    The content of the Text construct MUST NOT contain child elements and
   --    SHOULD be suitable for handling as HTML [HTML]. Any markup within is
   --    escaped; for example, "<br>" as "&lt;br>". Atom Processors that
   --    display such content MAY use that markup to aid in its display.
   --  Xhtml:
   --    The content SHOULD be suitable for handling as XHTML. The content is
   --    wrapped in a <div> element. The XHTML <div> element itself MUST NOT be
   --    considered part of the content. Atom Processors that display the
   --    content MAY use the markup to aid in displaying it. The escaped
   --    versions of characters such as "&" and ">" represent those characters,
   --    not markup.

   type Relation_Kind is (Alternate, Related, Self, Enclosure, Via);
   --  There are five values for the Registry of Link Relations:
   --
   --    Alternate:
   --       Signifies that the IRI in the value of the href attribute
   --       identifies an alternate version of the resource described by the
   --       containing element.
   --    Related:
   --       Signifies that the IRI in the value of the href attribute
   --       identifies a resource related to the resource described by the
   --       containing element. For example, the feed for a site that
   --       discusses the performance of the search engine at
   --       "http://search.example.com" might contain, as a child of atom :
   --       feed : <link rel="related" href="http://search.example.com/"/>
   --       An identical link might appear as a child of any atom:entry whose
   --       content contains a discussion of that same search engine.
   --    Self:
   --       Signifies that the IRI in the value of the href attribute
   --       identifies a resource equivalent to the containing element.
   --    Enclosure:
   --       Signifies that the IRI in the value of the href attribute
   --       identifies a related resource that is potentially large in size and
   --       might require special handling. For atom : link elements with
   --       rel = "enclosure", the length attribute SHOULD be provided.
   --    Via:
   --       Signifies that the IRI in the value of the href attribute
   --       identifies a resource that is the source of the information
   --       provided in the containing element.

   type Atom_Entry is limited private;
   --  The atom:entry object.

   type Atom_Entry_Source is limited private;
   --  The atom:source object.

   type Atom_Feed is limited private;
   --  The atom:feed object.

   Q : Character renames Ada.Characters.Latin_1.Quotation;

   None     : constant String := "";
   PI       : constant String := "<?xml version="
     & Q & "1.0" & Q & " encoding=" & Q & "utf-8" & Q & "?>"
     & Ada.Characters.Latin_1.LF;
   XHTMLNS  : constant String := "http://www.w3.org/1999/xhtml";
   XMLNS    : constant String := "http://www.w3.org/2005/Atom";
   DIVNS    : constant String := "xmlns=" & Q & XHTMLNS & Q;

   function New_Atom_Entry
     (Base_URI : in String := None;
      Language : in String := None)
      return Atom_Entry;
   --  Initialize an Atom entry object, as per the Atom specification RFC4287:
   --    http://tools.ietf.org/html/rfc4287
   --
   --  NOTE: All data is expected to be UTF-8 encoded. Yolk.Syndication does
   --  not do any kind of encoding.
   --
   --  Base_URI:
   --    Establishes a base URI for resolving relative references in the entry.
   --    Is overruled by Base_URI parameters for individual entry elements.
   --  Language:
   --    Indicates the natural language for the atom:entry element and its
   --    descendents.

   function New_Atom_Entry_Source
     (Base_URI : in String := None;
      Language : in String := None)
      return Atom_Entry_Source;
   --  Initialize an Atom source object. This is for those occassions where
   --  the entry is copied from one feed into another. The atom:source
   --  may contain all the metadata from the originating feed.
   --
   --  NOTE: All data is expected to be UTF-8 encoded. Yolk.Syndication does
   --  not do any kind of encoding.
   --
   --  Base_URI:
   --    Establishes a base URI for resolving relative references in the entry.
   --    Is overruled by Base_URI parameters for individual entry elements.
   --  Language:
   --    Indicates the natural language for the atom:entry element and its
   --    descendents.

   function New_Atom_Feed
     (Base_URI    : in String := None;
      Language    : in String := None;
      Max_Age     : in Duration := 5_616_000.0;
      Max_Entries : in Positive := 100;
      Min_Entries : in Positive := 10)
      return Atom_Feed;
   --  Initialize an Atom object, as per the Atom specification RFC4287:
   --    http://tools.ietf.org/html/rfc4287
   --
   --  NOTE: All data is expected to be UTF-8 encoded. Yolk.Syndication does
   --  not do any kind of encoding.
   --
   --  Base_URI:
   --    Establishes a base URI for resolving relative references in the feed.
   --    Is overruled by Base_URI parameters for individual feed child
   --    elements.
   --  Language:
   --    Indicates the natural language for the atom:feed element and its
   --    descendents.
   --  Max_Age:
   --    If an entries Updated value is older than Max_Age, and there are more
   --    than Min_Entries in the list, then the entry is deleted. This is done
   --    in the Add_Entry procedure. Max_Age is given in seconds.
   --  Max_Entries:
   --    This is the max amount of entries we keep in the list. If there are
   --    more than Max_Entries entries in the list, then the oldest entries are
   --    deleted until the list is Max_Entries long.
   --  Min_Entries:
   --    The is the minimum amount of entries that must be in the list before
   --    we bother with deleting old entries. If there are less than
   --    Min_Entries in the list, then even a 100 year old entry is kept.

private

   use Ada.Containers;
   use Ada.Strings.Unbounded;

   type Atom_Common is
      record
         Base_URI : Unbounded_String;
         Language : Unbounded_String;
      end record;

   type Atom_Category is
      record
         Common   : Atom_Common;
         Label    : Unbounded_String;
         Scheme   : Unbounded_String;
         Term     : Unbounded_String;
      end record;

   type Atom_Date is
      record
         Common      : Atom_Common;
         Is_Set      : Boolean;
         Time_Stamp  : Ada.Calendar.Time;
      end record;

   type Atom_Entry_Content is
      record
         Common         : Atom_Common;
         Content        : Unbounded_String;
         Content_Kind   : Content_Kinds;
         Mime_Type      : Unbounded_String;
         Source         : Unbounded_String;
      end record;

   type Atom_Generator is
      record
         Agent    : Unbounded_String;
         Common   : Atom_Common;
         URI      : Unbounded_String;
         Version  : Unbounded_String;
      end record;

   type Atom_Icon is
      record
         Common   : Atom_Common;
         URI      : Unbounded_String;
      end record;

   type Atom_Id is
      record
         Common   : Atom_Common;
         URI      : Unbounded_String;
      end record;

   type Atom_Link is
      record
         Common      : Atom_Common;
         Href        : Unbounded_String;
         Hreflang    : Unbounded_String;
         Length      : Natural;
         Mime_Type   : Unbounded_String;
         Rel         : Relation_Kind;
         Title       : Unbounded_String;
      end record;

   type Atom_Logo is
      record
         Common   : Atom_Common;
         URI      : Unbounded_String;
      end record;

   type Atom_Person is
      record
         Common   : Atom_Common;
         Email    : Unbounded_String;
         Name     : Unbounded_String;
         URI      : Unbounded_String;
      end record;

   type Atom_Text is
      record
         Common         : Atom_Common;
         Text_Content   : Unbounded_String;
         Text_Kind      : Text_Kinds;
      end record;

   package Category_List is new Doubly_Linked_Lists (Atom_Category);
   package Link_List is new Doubly_Linked_Lists (Atom_Link);
   package Person_List is new Doubly_Linked_Lists (Atom_Person);

   type Atom_Entry_Source is
      record
         Authors        : Person_List.List;
         Categories     : Category_List.List;
         Common         : Atom_Common;
         Contributors   : Person_List.List;
         Generator      : Atom_Generator;
         Icon           : Atom_Icon;
         Id             : Atom_Id;
         Links          : Link_List.List;
         Logo           : Atom_Logo;
         Rights         : Atom_Text;
         Subtitle       : Atom_Text;
         Title          : Atom_Text;
         Updated        : Atom_Date;
      end record;

   Null_Atom_Entry_Source : constant Atom_Entry_Source
     := (Authors       => Person_List.Empty_List,
         Categories    => Category_List.Empty_List,
         Common        => Atom_Common'(Base_URI => Null_Unbounded_String,
                                       Language => Null_Unbounded_String),
         Contributors  => Person_List.Empty_List,
         Generator     =>
           Atom_Generator'(Agent   => Null_Unbounded_String,
                           Common  =>
                             Atom_Common'(Base_URI => Null_Unbounded_String,
                                          Language => Null_Unbounded_String),
                           URI     => Null_Unbounded_String,
                           Version => Null_Unbounded_String),
         Icon          =>
           Atom_Icon'(Common =>
                        Atom_Common'(Base_URI => Null_Unbounded_String,
                                     Language => Null_Unbounded_String),
                      URI    => Null_Unbounded_String),
         Id            => Atom_Id'(Common =>
                                     Atom_Common'(Base_URI =>
                                                    Null_Unbounded_String,
                                                  Language =>
                                                    Null_Unbounded_String),
                                   URI    => Null_Unbounded_String),
         Links         => Link_List.Empty_List,
         Logo          =>  Atom_Logo'(Common => Atom_Common'
                                        (Base_URI => Null_Unbounded_String,
                                         Language => Null_Unbounded_String),
                                      URI    => Null_Unbounded_String),
         Rights        => Atom_Text'(Common       => Atom_Common'
                                       (Base_URI => Null_Unbounded_String,
                                        Language => Null_Unbounded_String),
                                     Text_Content => Null_Unbounded_String,
                                     Text_Kind    => Text),
         Subtitle      => Atom_Text'(Common       => Atom_Common'
                                       (Base_URI => Null_Unbounded_String,
                                        Language => Null_Unbounded_String),
                                     Text_Content => Null_Unbounded_String,
                                     Text_Kind    => Text),
         Title         => Atom_Text'(Common       => Atom_Common'
                                       (Base_URI => Null_Unbounded_String,
                                        Language => Null_Unbounded_String),
                                     Text_Content => Null_Unbounded_String,
                                     Text_Kind    => Text),
         Updated       => Atom_Date'(Common     => Atom_Common'
                                       (Base_URI => Null_Unbounded_String,
                                        Language => Null_Unbounded_String),
                                     Is_Set     => False,
                                     Time_Stamp => Ada.Calendar.Clock));

   type Atom_Entry is
      record
         Authors        : Person_List.List;
         Categories     : Category_List.List;
         Common         : Atom_Common;
         Content        : Atom_Entry_Content;
         Contributors   : Person_List.List;
         Id             : Atom_Id;
         Links          : Link_List.List;
         Published      : Atom_Date;
         Rights         : Atom_Text;
         Source         : Atom_Entry_Source;
         Summary        : Atom_Text;
         Title          : Atom_Text;
         Updated        : Atom_Date;
      end record;

   Null_Atom_Entry : constant Atom_Entry
     := (Authors       => Person_List.Empty_List,
         Categories    => Category_List.Empty_List,
         Common        => Atom_Common'(Base_URI => Null_Unbounded_String,
                                       Language => Null_Unbounded_String),
         Content       =>
           Atom_Entry_Content'(Common        =>
                                 Atom_Common'(Base_URI =>
                                                Null_Unbounded_String,
                                              Language =>
                                                Null_Unbounded_String),
                               Content       => Null_Unbounded_String,
                               Content_Kind  => Text,
                               Mime_Type     => Null_Unbounded_String,
                               Source        => Null_Unbounded_String),
         Contributors  => Person_List.Empty_List,
         Id            => Atom_Id'(Common =>
                                     Atom_Common'(Base_URI =>
                                                    Null_Unbounded_String,
                                                  Language =>
                                                    Null_Unbounded_String),
                                   URI    => Null_Unbounded_String),
         Links         => Link_List.Empty_List,
         Published     => Atom_Date'(Common     =>
                                       Atom_Common'(Base_URI =>
                                                      Null_Unbounded_String,
                                                    Language =>
                                                      Null_Unbounded_String),
                                     Is_Set     => False,
                                     Time_Stamp => Ada.Calendar.Clock),
         Rights        => Atom_Text'(Common       =>
                                       Atom_Common'(Base_URI =>
                                                      Null_Unbounded_String,
                                                    Language =>
                                                      Null_Unbounded_String),
                                     Text_Content => Null_Unbounded_String,
                                     Text_Kind    => Text),
         Source        => Null_Atom_Entry_Source,
         Summary       => Atom_Text'(Common       =>
                                       Atom_Common'(Base_URI =>
                                                      Null_Unbounded_String,
                                                    Language =>
                                                      Null_Unbounded_String),
                                     Text_Content => Null_Unbounded_String,
                                     Text_Kind    => Text),
         Title         => Atom_Text'(Common       =>
                                       Atom_Common'(Base_URI =>
                                                      Null_Unbounded_String,
                                                    Language =>
                                                      Null_Unbounded_String),
                                     Text_Content => Null_Unbounded_String,
                                     Text_Kind    => Text),
         Updated       => Atom_Date'(Common     =>
                                       Atom_Common'(Base_URI =>
                                                      Null_Unbounded_String,
                                                    Language =>
                                                      Null_Unbounded_String),
                                     Is_Set     => False,
                                     Time_Stamp => Ada.Calendar.Clock));

   function Equal_Entry
     (Left, Right : in Atom_Entry)
      return Boolean;

   package Entry_List is new Doubly_Linked_Lists (Atom_Entry, Equal_Entry);

   protected type Protected_Atom_Feed is

      procedure Add_Author
        (Value : in Atom_Person);

      procedure Add_Category
        (Value : in Atom_Category);

      procedure Add_Contributor
        (Value : in Atom_Person);

      procedure Add_Entry
        (Value       : in Yolk.Syndication.Atom_Entry;
         Entry_Added : out Boolean);

      procedure Add_Link
        (Value : in Atom_Link);

      procedure Clear_Entry_List;

      procedure Delete_Entry
        (Id : in String);

      function Get_DOM return DOM.Core.Document;

      function Get_String return String;

      procedure Set_Common
        (Value : in Atom_Common);

      procedure Set_Generator
        (Value : in Atom_Generator);

      procedure Set_Icon
        (Value : in Atom_Icon);

      procedure Set_Id
        (Value : in Atom_Id);

      procedure Set_Logo
        (Value : in Atom_Logo);

      procedure Set_Max_Age
        (Value : in Duration);

      procedure Set_Max_Entries
        (Value : in Positive);

      procedure Set_Min_Entries
        (Value : in Positive);

      procedure Set_Rights
        (Value : in Atom_Text);

      procedure Set_Subtitle
        (Value : in Atom_Text);

      procedure Set_Title
        (Value : in Atom_Text);

      procedure Set_Updated_Time
        (Value : in Atom_Date);

   private

      Authors        : Person_List.List;
      Categories     : Category_List.List;
      Common         : Atom_Common;
      Contributors   : Person_List.List;
      Entries        : Entry_List.List;
      Generator      : Atom_Generator;
      Icon           : Atom_Icon;
      Id             : Atom_Id;
      Links          : Link_List.List;
      Logo           : Atom_Logo;
      Max_Entry_Age  : Duration;
      Max_Entries    : Positive;
      Min_Entries    : Positive;
      Rights         : Atom_Text;
      Subtitle       : Atom_Text;
      Title          : Atom_Text;
      Updated        : Atom_Date;

   end Protected_Atom_Feed;

   type Atom_Feed is
      record
         PAF : Protected_Atom_Feed;
      end record;

end Yolk.Syndication;
