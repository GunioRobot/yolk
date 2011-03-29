-------------------------------------------------------------------------------
--                                                                           --
--                                  Yolk                                     --
--                                                                           --
--                         Yolk.Syndication.Writer                           --
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
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Unbounded;

package Yolk.Syndication.Writer is

   use Ada.Containers;
   use Ada.Strings.Unbounded;

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

   type Atom_Common is
      record
         Base_URI : Unbounded_String;
         Language : Unbounded_String;
      end record;
   --  Base_URI:
   --    Establishe the base URI (or IRI) for resolving any relative
   --    references found within the effective scope of the xml:base
   --    attribute.
   --  Language:
   --    Indicates the natural language for the feed and its descendents.
   --    The language context is only significant for elements and
   --    attributes declared to be "language sensitive".

   type Atom_Category is
      record
         Common   : Atom_Common;
         Content  : Unbounded_String;
         Label    : Unbounded_String;
         Scheme   : Unbounded_String;
         Term     : Unbounded_String;
      end record;
   --  Content:
   --    No meaning is assigned to this by RFC4287. Should probably be left
   --    empty.
   --  Term:
   --    A string that identifies the category to which the entry or feed
   --    belongs.
   --  Label:
   --    Provides a human - readable label for display in end-user
   --    applications. The content of the Label is language sensitive.
   --    Entities such as "&amp;" and "&lt;" represent their corresponding
   --    characters ("&" and "<", respectively), not markup.
   --  Scheme:
   --    An IRI that identifies a categorization scheme.

   type Atom_Generator is
      record
         Agent    : Unbounded_String;
         Common   : Atom_Common;
         Version  : Unbounded_String;
         URI      : Unbounded_String;
      end record;
   --  Agent:
   --    The agent used to generate the feed.
   --  URI:
   --    Should point to a resource relevant to the Agent.
   --  Version:
   --    The version of the Agent.

   type Atom_Icon is
      record
         Common   : Atom_Common;
         URI      : Unbounded_String;
      end record;
   --  URI:
   --    Identifies an image that provides iconic visual identification for a
   --    feed. The image SHOULD have an aspect ratio of one (horizontal) to one
   --    (vertical) and SHOULD be suitable for presentation at a small size.

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
   --  Content:
   --    No meaning is assigned to this by RFC4287. Should probably be left
   --    empty.
   --  Href:
   --    Contains the link's IRI.
   --  Hreflang:
   --    The Hreflang content describes the language of the resource pointed
   --    to by Href. When used together with the Rel = Alternate, it implies
   --    a translated version of the feed.
   --  Length:
   --    Indicates an advisory length of the linked content in octets; it is
   --    a hint about the content length of the representation returned when
   --    the IRI in Href is mapped to a URI and dereferenced. Note that the
   --    length attribute does not override the actual content length of the
   --    representation as reported by the underlying protocol
   --  Mime_Type:
   --    On the link element, the Mime_Type value is an advisory media type:
   --    it is a hint about the type of the representation that is expected
   --    to be returned when the value of Href is dereferenced. Note that
   --    the type attribute does not override the actual media type returned
   --    with the representation.
   --  Rel:
   --    Indicates the link relation type.
   --  Title:
   --    Conveys human-readable information about the link. The content of
   --    the Title is language sensitive. Entities such as "&amp;" and
   --    "&lt;" represent their corresponding characters ("&" and "<"), not
   --    markup.

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
      --  Name:
      --    conveys a human - readable name for the person. The content of
      --    Name is language sensitive.
      --  Email:
      --    conveys an e - mail address associated with the person.
      --  URI:
      --    conveys an IRI associated with the person.

   type Atom_Text is
      record
         Common         : Atom_Common;
         Text_Content   : Unbounded_String;
         Text_Type      : Content_Kind;
      end record;
   --  A Text construct contains human-readable text, usually in small
   --  quantities. The content of Text constructs is Language - Sensitive.

   package Category_List is new Doubly_Linked_Lists (Atom_Category);
   package Link_List is new Doubly_Linked_Lists (Atom_Link);
   package Person_List is new Doubly_Linked_Lists (Atom_Person);

   protected type Atom_Feed is

      procedure Add_Author
        (Value : in Atom_Person);
      --  Add an author child element to the Atom top-level feed element. In an
      --  Atom Feed Document, the Author elements of the containing atom : feed
      --  element are considered to apply to the entry if there are no atom :
      --  author elements in entry elements.

      procedure Add_Category
        (Value : in Atom_Category);
      --  Add a category to the Atom top-level feed element. Note that the
      --  Content parameter is assigned no meaning by RFC4287, so in most cases
      --  it should probably be left empty.

      procedure Add_Contributor
        (Value : in Atom_Person);
      --  Add a contributor child element to the Atom top-level feed element.

      procedure Add_Link
        (Value : in Atom_Link);
      --  Add an atom:link element.
      --  Defines a reference from an entry or feed to a Web resource. This
      --  specification assigns no meaning to the Content (if any) of this
      --  element.
      --  See comment for Relation_Type for info on the Rel parameter.

      function Get_String return String;
      --  Return the Atom XML string.

      procedure Set_Common
        (Value : in Atom_Common);
      --  The attributes base and lang are common to all the elements defined
      --  in RFC4287. Whether or not they are significant in a given context
      --  depends entirely on the spec.

      procedure Set_Encoding
        (Value : in String := "utf-8");
      --  Value:
      --    The encoding attribute of the root XML element. This package does
      --    not do any kind of encoding/decoding, so make sure that the data
      --    you put in match the encoding given here.

      procedure Set_Generator
        (Value : in Atom_Generator);
      --  Set the child generator element of the Atom top-level feed element.
      --  The Agent parameter is text, so markup is escaped.

      procedure Set_Icon
        (Value : in Atom_Icon);
      --  Set the icon URI for the feed. Should reference a 1 to 1 ratio
      --  graphic that is suitable for presentation at a small size.

      procedure Set_Id
        (Value : in String);
      --  Set the atom:id element.
      --
      --  These are some things to consider when providing the feed ID:
      --    Provide the scheme in lowercase characters.
      --    Provide the host, if any, in lowercase characters.
      --    Only perform percent-encoding where it is essential.
      --    Use uppercase A through F characters when percent-encoding.
      --    Prevent dot-segments from appearing in paths.
      --    For schemes that define a default authority, use an empty authority
      --    if the default is desired.
      --    For schemes that define an empty path to be equivalent to a path of
      --    "/", use "/".
      --    For schemes that define a port, use an empty port if the default is
      --    desired.
      --    Preserve empty fragment identifiers and queries.
      --    Ensure that all components of the IRI are appropriately character
      --    normalized, e.g., by using NFC or NFKC.

      procedure Set_Logo
        (Value : in Atom_Logo);
      --  A reference [RFC3987] that identifies an image that provides visual
      --  identification for a feed. The image SHOULD have an aspect ratio of 2
      --  (horizontal) to 1 (vertical).

      procedure Set_Rights
        (Value : in Atom_Text);
      --  A Text construct that conveys information about rights held in and
      --  over an entry or feed.

      procedure Set_Subtitle
        (Value : in Atom_Text);
      --  A Text construct that conveys a human-readable description or
      --  subtitle for a feed.

      procedure Set_Title
        (Value : in Atom_Text);
      --  Set the child title element of the Atom top-level feed element.

      procedure Set_Updated_Time
        (Value : in Ada.Calendar.Time);
      --  Indicates the most recent instant in time when an entry or feed was
      --  modified in a way the publisher considers significant. Therefore, not
      --  all modifications necessarily result in a changed updated value.
      --  It is generally not necessary to call this manually, as it happens
      --  automatically whenever an entry element is added/delete/edited.

   private

      Atom_String    : Unbounded_String;
      Authors        : Person_List.List;
      Categories     : Category_List.List;
      Common         : Atom_Common;
      Contributors   : Person_List.List;
      Encoding       : Unbounded_String;
      Generator      : Atom_Generator;
      Icon           : Atom_Icon;
      Id             : Unbounded_String;
      Links          : Link_List.List;
      Logo           : Atom_Logo;
      Rights         : Atom_Text;
      Subtitle       : Atom_Text;
      Title          : Atom_Text;
      Updated        : Ada.Calendar.Time;

   end Atom_Feed;

end Yolk.Syndication.Writer;
