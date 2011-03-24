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

private with Ada.Calendar;
private with Ada.Containers.Doubly_Linked_Lists;
private with Ada.Strings.Unbounded;

package Yolk.Syndication is

   type Atom_Feed is limited private;
   --  An Atom feed object.

   type Content_Kind is (Text, Html, Xhtml);
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

   None : constant String := "";

   function Initialize
     (Id          : in String;
      Title       : in String;
      Base_URI    : in String := None;
      Language    : in String := None;
      Title_Kind  : in Content_Kind := Text)
      return Atom_Feed;
   --  Initialize an Atom object with the _required data_, as per the Atom
   --  specification RFC4287:
   --    http://tools.ietf.org/html/rfc4287
   --
   --  Base_URI:
   --    Establishes base URI for resolving relative references in the feed.
   --    Is overruled by Base_URI parameters for individual feed entries.
   --  Id:
   --    A permanent, universally unique identifier for the feed.
   --  Language:
   --    Indicated the natural language for the atom:feed element and its
   --    descendents.
   --  Title:
   --    A human-readable title for the feed.
   --  Title_Kind:
   --    The title kind. See Content_Type.

private

   use Ada.Containers;
   use Ada.Strings.Unbounded;

   type Atom_Common is
      record
         Base_URI : Unbounded_String;
         Language : Unbounded_String;
      end record;

   Null_Common : constant Atom_Common :=
                   (Base_URI => Null_Unbounded_String,
                    Language => Null_Unbounded_String);

   type Atom_Category is
      record
         Common   : Atom_Common;
         Content  : Unbounded_String;
         Label    : Unbounded_String;
         Scheme   : Unbounded_String;
         Term     : Unbounded_String;
      end record;

   type Atom_Generator is
      record
         Agent    : Unbounded_String;
         Common   : Atom_Common;
         Version  : Unbounded_String;
         URI      : Unbounded_String;
      end record;

   Null_Generator : constant Atom_Generator :=
                      (Agent     => Null_Unbounded_String,
                       Common    => Null_Common,
                       Version   => Null_Unbounded_String,
                       URI       => Null_Unbounded_String);

   type Atom_Icon is
      record
         Common   : Atom_Common;
         URI      : Unbounded_String;
      end record;

   Null_Icon : constant Atom_Icon := (Common => Null_Common,
                                      URI    => Null_Unbounded_String);

   type Atom_Id is
      record
         Id : Unbounded_String;
      end record;

   type Atom_Link is
      record
         Common      : Atom_Common;
         Content     : Unbounded_String;
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

   Null_Logo : constant Atom_Logo := (Common => Null_Common,
                                      URI    => Null_Unbounded_String);

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
         Text_Type      : Content_Kind := Text;
      end record;

   Null_Text : constant Atom_Text := (Common       => Null_Common,
                                      Text_Content => Null_Unbounded_String,
                                      Text_Type    => Text);

   package Category_List is new Doubly_Linked_Lists (Atom_Category);
   package Link_List is new Doubly_Linked_Lists (Atom_Link);
   package Person_List is new Doubly_Linked_Lists (Atom_Person);

   protected type Atom_Feed is

      procedure Add_Author
        (Name     : in String;
         Base_URI : in String := None;
         Email    : in String := None;
         Language : in String := None;
         URI      : in String := None);
      --  Add an author child element to the Atom top-level feed element. In an
      --  Atom Feed Document, the Author elements of the containing atom : feed
      --  element are considered to apply to the entry if there are no atom :
      --  author elements in entry elements.
      --
      --  Name:
      --    conveys a human - readable name for the person. The content of
      --    Name is language sensitive.
      --  Email:
      --    conveys an e - mail address associated with the person.
      --  URI:
      --    conveys an IRI associated with the person.

      procedure Add_Category
        (Term     : in String;
         Base_URI : in String := None;
         Content  : in String := None;
         Label    : in String := None;
         Language : in String := None;
         Scheme   : in String := None);
      --  Add a category to the Atom top-level feed element.
      --
      --  Term is a string that identifies the category to which the entry or
      --  feed belongs. Category elements MUST have a "term" attribute.
      --
      --  Note that the Content parameter is assigned no meaning by RFC4287, so
      --  in most cases it should probably be left empty.
      --
      --  Label provides a human-readable label for display in end-user
      --  applications. The content of the Label is language sensitive.
      --  Entities such as "&amp;" and "&lt;" represent their corresponding
      --  characters ("&" and "<", respectively), not markup.
      --
      --  Scheme is an IRI that identifies a categorization scheme.

      procedure Add_Contributor
        (Name     : in String;
         Base_URI : in String := None;
         Email    : in String := None;
         Language : in String := None;
         URI      : in String := None);
      --  Add a contributor child element to the Atom top-level feed element.
      --
      --  Name conveys a human-readable name for the person. The content of
      --  Name is language sensitive.
      --
      --  Email conveys an e-mail address associated with the person.
      --
      --  URI conveys an IRI associated with the person.

      procedure Add_Link
        (Href        : in String;
         Base_URI    : in String := None;
         Content     : in String := None;
         Hreflang    : in String := None;
         Language    : in String := None;
         Length      : in Natural := 0;
         Mime_Type   : in String := None;
         Rel         : in Relation_Kind := Alternate;
         Title       : in String := None);
      --  Add an atom:link element.
      --  Defines a reference from an entry or feed to a Web resource. This
      --  specification assigns no meaning to the Content (if any) of this
      --  element.
      --  See comment for Relation_Type for info on the Rel parameter.
      --
      --  Href contains the link's IRI.
      --
      procedure Set_Base_URI
        (Value : in String := None);
      --  Set the Base_URI for the feed.

      procedure Set_Common
        (Base_URI : in String := None;
         Language : in String := None);
      --  The attributes base and lang are common to all the elements defined
      --  in RFC4287. Whether or not they are significant in a given context
      --  depends entirely on the spec.

      procedure Set_Generator
        (Agent    : in String;
         Base_URI : in String := None;
         Language : in String := None;
         URI      : in String := None;
         Version  : in String := None);
      --  Set the child generator element of the Atom top-level feed element.
      --  The Agent parameter is text, so markup is escaped.

      procedure Set_Icon
        (URI      : in String;
         Base_URI : in String := None);
      --  Set the icon URI for the feed. Should reference a 1 to 1 ratio
      --  graphic that is suitable for presentation at a small size.

      procedure Set_Id
        (Value : in String);
      --  Set the atom:id element.

      procedure Set_Language
        (Value : in String := None);
      --  Set the language for the feed.

      procedure Set_Logo
        (URI      : in String;
         Base_URI : in String := None;
         Language : in String := None);
      --  A reference [RFC3987] that identifies an image that provides visual
      --  identification for a feed. The image SHOULD have an aspect ratio of 2
      --  (horizontal) to 1 (vertical).

      procedure Set_Rights
        (Content  : in String;
         Base_URI : in String := None;
         Kind     : in Content_Kind := Text;
         Language : in String := None);
      --  A Text construct that conveys information about rights held in and
      --  over an entry or feed.

      procedure Set_Subtitle
        (Content  : in String;
         Base_URI : in String := None;
         Kind     : in Content_Kind := Text;
         Language : in String := None);
      --  A Text construct that conveys a human-readable description or
      --  subtitle for a feed.

      procedure Set_Title
        (Value : in String;
         Kind  : in Content_Kind := Text);
      --  Set the child title element of the Atom top-level feed element.

      procedure Set_Updated_Time
        (Value : in Ada.Calendar.Time);
      --  Set the child updated element of the Atom top-level feed element. It
      --  is generally not necessary to call this manually, as it happens
      --  automatically whenever an entry element is added/delete/edited.

   private

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
      Updated        : Ada.Calendar.Time;

   end Atom_Feed;

end Yolk.Syndication;
