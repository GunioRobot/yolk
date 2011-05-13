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
private with Ada.Containers.Doubly_Linked_Lists;
private with Ada.Strings.Unbounded;
with DOM.Core;

package Yolk.Syndication.Writer is

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
   type Atom_Feed is limited private;

   procedure Add_Author
     (Entr     : in out Atom_Entry;
      Name     : in     String;
      Base_URI : in     String := None;
      Email    : in     String := None;
      Language : in     String := None;
      URI      : in     String := None);
   --  Add an author child element to the atom:entry element.
   --
   --  Name:
   --    conveys a human - readable name for the person. The content of
   --    Name is language sensitive.
   --  Base_URI:
   --    See Set_Common.
   --  Email:
   --    conveys an e - mail address associated with the person.
   --  Language:
   --    See Set_Common.
   --  URI:
   --    conveys an IRI associated with the person.

   procedure Add_Author
     (Feed     : in out Atom_Feed;
      Name     : in     String;
      Base_URI : in     String := None;
      Email    : in     String := None;
      Language : in     String := None;
      URI      : in     String := None);
   --  Add an author child element to the atom:feed element. In an Atom Feed
   --  Document, the author elements of the containing atom:feed element are
   --  considered to apply to the entry if there are no atom:author elements in
   --  entry elements.
   --
   --  Name:
   --    conveys a human - readable name for the person. The content of
   --    Name is language sensitive.
   --  Base_URI:
   --    See Set_Common.
   --  Email:
   --    conveys an e - mail address associated with the person.
   --  Language:
   --    See Set_Common.
   --  URI:
   --    conveys an IRI associated with the person.

   procedure Add_Author_Source
     (Entr     : in out Atom_Entry;
      Name     : in     String;
      Base_URI : in     String := None;
      Email    : in     String := None;
      Language : in     String := None;
      URI      : in     String := None);
   --  Add an author child element to the atom:source element.
   --
   --  Name:
   --    conveys a human - readable name for the person. The content of
   --    Name is language sensitive.
   --  Base_URI:
   --    See Set_Common.
   --  Email:
   --    conveys an e - mail address associated with the person.
   --  Language:
   --    See Set_Common.
   --  URI:
   --    conveys an IRI associated with the person.

   procedure Add_Category
     (Entr     : in out Atom_Entry;
      Term     : in     String;
      Base_URI : in     String := None;
      Content  : in     String := None;
      Label    : in     String := None;
      Language : in     String := None;
      Scheme   : in     String := None);
   --  Add an atom:category element to the entry. The atom:category element
   --  conveys information about a category associated with an entry or feed.
   --
   --  Content:
   --    No meaning is assigned to this by RFC4287. Should probably be left
   --    empty.
   --  Term:
   --    A string that identifies the category to which the entry belongs.
   --  Label:
   --    Provides a human - readable label for display in end-user
   --    applications. The content of the Label is language sensitive.
   --    Entities such as "&amp;" and "&lt;" represent their corresponding
   --    characters ("&" and "<", respectively), not markup.
   --  Scheme:
   --    An IRI that identifies a categorization scheme.

   procedure Add_Category
     (Feed     : in out Atom_Feed;
      Term     : in     String;
      Base_URI : in     String := None;
      Content  : in     String := None;
      Label    : in     String := None;
      Language : in     String := None;
      Scheme   : in     String := None);
   --  Add an atom:category element to the feed. The atom:category element
   --  conveys information about a category associated with an entry or feed.
   --
   --  Content:
   --    No meaning is assigned to this by RFC4287. Should probably be left
   --    empty.
   --  Term:
   --    A string that identifies the category to which the feed belongs.
   --  Label:
   --    Provides a human - readable label for display in end-user
   --    applications. The content of the Label is language sensitive.
   --    Entities such as "&amp;" and "&lt;" represent their corresponding
   --    characters ("&" and "<", respectively), not markup.
   --  Scheme:
   --    An IRI that identifies a categorization scheme.

   procedure Add_Category_Source
     (Entr     : in out Atom_Entry;
      Term     : in     String;
      Base_URI : in     String := None;
      Content  : in     String := None;
      Label    : in     String := None;
      Language : in     String := None;
      Scheme   : in     String := None);
   --  Add a category element to atom:source element. The atom:category element
   --  conveys information about a category associated with an entry or feed.
   --
   --  Content:
   --    No meaning is assigned to this by RFC4287. Should probably be left
   --    empty.
   --  Term:
   --    A string that identifies the category to which the entry belongs.
   --  Label:
   --    Provides a human - readable label for display in end-user
   --    applications. The content of the Label is language sensitive.
   --    Entities such as "&amp;" and "&lt;" represent their corresponding
   --    characters ("&" and "<", respectively), not markup.
   --  Scheme:
   --    An IRI that identifies a categorization scheme.

   procedure Add_Contributor
     (Entr     : in out Atom_Entry;
      Name     : in     String;
      Base_URI : in     String := None;
      Email    : in     String := None;
      Language : in     String := None;
      URI      : in     String := None);
   --  Add a contributor child element to the atom:entry element. In an Atom
   --  Feed Document, the contributor element indicates a person or other
   --  entity who contributed to the feed.
   --
   --  Name:
   --    conveys a human - readable name for the person. The content of
   --    Name is language sensitive.
   --  Base_URI:
   --    See Set_Common.
   --  Email:
   --    conveys an e - mail address associated with the contributor.
   --  Language:
   --    See Set_Common.
   --  URI:
   --    conveys an IRI associated with the person.

   procedure Add_Contributor
     (Feed     : in out Atom_Feed;
      Name     : in     String;
      Base_URI : in     String := None;
      Email    : in     String := None;
      Language : in     String := None;
      URI      : in     String := None);
   --  Add a contributor child element to the atom:feed element. In an Atom
   --  Feed Document, the contributor element indicates a person or other
   --  entity who contributed to the feed.
   --
   --  Name:
   --    conveys a human - readable name for the person. The content of
   --    Name is language sensitive.
   --  Base_URI:
   --    See Set_Common.
   --  Email:
   --    conveys an e - mail address associated with the contributor.
   --  Language:
   --    See Set_Common.
   --  URI:
   --    conveys an IRI associated with the person.

   procedure Add_Contributor_Source
     (Entr     : in out Atom_Entry;
      Name     : in     String;
      Base_URI : in     String := None;
      Email    : in     String := None;
      Language : in     String := None;
      URI      : in     String := None);
   --  Add a contributor child element to the atom:source element. In an Atom
   --  Feed Document, the contributor element indicates a person or other
   --  entity who contributed to the feed.
   --
   --  Name:
   --    conveys a human - readable name for the person. The content of
   --    Name is language sensitive.
   --  Base_URI:
   --    See Set_Common.
   --  Email:
   --    conveys an e - mail address associated with the contributor.
   --  Language:
   --    See Set_Common.
   --  URI:
   --    conveys an IRI associated with the person.

   procedure Add_Entry
     (Feed : in out Atom_Feed;
      Entr : in     Atom_Entry);
   --  Add an entry element to the atom:feed element.

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
      Title     : in     String := None);
   --  Defines a reference from an entry or feed to a Web resource.
   --
   --  Base_URI:
   --    See Set_Common.
   --  Content:
   --    No meaning is assigned to this by RFC4287. Should probably be left
   --    empty.
   --  Href:
   --    Contains the link's IRI.
   --  Hreflang:
   --    The Hreflang content describes the language of the resource pointed
   --    to by Href. When used together with the Rel = Alternate, it implies
   --    a translated version of the feed.
   --  Language:
   --    See Set_Common.
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
      Title     : in     String := None);
   --  Defines a reference from an entry or feed to a Web resource.
   --
   --  Base_URI:
   --    See Set_Common.
   --  Content:
   --    No meaning is assigned to this by RFC4287. Should probably be left
   --    empty.
   --  Href:
   --    Contains the link's IRI.
   --  Hreflang:
   --    The Hreflang content describes the language of the resource pointed
   --    to by Href. When used together with the Rel = Alternate, it implies
   --    a translated version of the feed.
   --  Language:
   --    See Set_Common.
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
      Title     : in     String := None);
   --  Defines a reference from an entry or feed to a Web resource.
   --
   --  Base_URI:
   --    See Set_Common.
   --  Content:
   --    No meaning is assigned to this by RFC4287. Should probably be left
   --    empty.
   --  Href:
   --    Contains the link's IRI.
   --  Hreflang:
   --    The Hreflang content describes the language of the resource pointed
   --    to by Href. When used together with the Rel = Alternate, it implies
   --    a translated version of the feed.
   --  Language:
   --    See Set_Common.
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

   function Get_XML_DOM
     (Feed : in Atom_Feed)
      return DOM.Core.Document;
   --  Return the Atom XML DOM document.

   function Get_XML_String
     (Feed : in Atom_Feed)
      return String;
   --  Return the Atom XML string.

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
   --    Establishes base URI for resolving relative references in the entry.
   --    Is overruled by Base_URI parameters for individual entry elements.
   --  Language:
   --    Indicates the natural language for the atom:entry element and its
   --    descendents.

   function New_Atom_Feed
     (Base_URI : in String := None;
      Language : in String := None)
      return Atom_Feed;
   --  Initialize an Atom object, as per the Atom specification RFC4287:
   --    http://tools.ietf.org/html/rfc4287
   --
   --  NOTE: All data is expected to be UTF-8 encoded. Yolk.Syndication does
   --  not do any kind of encoding.
   --
   --  Base_URI:
   --    Establishes base URI for resolving relative references in the feed.
   --    Is overruled by Base_URI parameters for individual feed child
   --    elements.
   --  Language:
   --    Indicates the natural language for the atom:feed element and its
   --    descendents.

   procedure Set_Common
     (Feed     : in out Atom_Feed;
      Base_URI : in     String := None;
      Language : in     String := None);
   --  The attributes base and lang are common to all the elements defined
   --  in RFC4287. Whether or not they are significant in a given context
   --  depends entirely on the spec.
   --
   --  Base_URI:
   --    Establishes the base URI (or IRI) for resolving any relative
   --    references found within the effective scope of the xml:base
   --    attribute.
   --  Language:
   --    Indicates the natural language for the feed and its descendents.
   --    The language context is only significant for elements and
   --    attributes declared to be "language sensitive".

   procedure Set_Common_Source
     (Entr     : in out Atom_Entry;
      Base_URI : in     String := None;
      Language : in     String := None);
   --  The attributes base and lang are common to all the elements defined
   --  in RFC4287. Whether or not they are significant in a given context
   --  depends entirely on the spec.
   --
   --  Base_URI:
   --    Establishes the base URI (or IRI) for resolving any relative
   --    references found within the effective scope of the xml:base
   --    attribute.
   --  Language:
   --    Indicates the natural language for the feed and its descendents.
   --    The language context is only significant for elements and
   --    attributes declared to be "language sensitive".

   procedure Set_Content
     (Entr           : in out Atom_Entry;
      Content        : in     String;
      Content_Kind   : in     Text_Kinds;
      Base_URI       : in     String := None;
      Language       : in     String := None);
   --  Add a content element to the atom:entry element. Use this procedure if
   --  the content is textual, ie. Text, Html or Xhtml.

   procedure Set_Content_Inline
     (Entr      : in out Atom_Entry;
      Content   : in String;
      Mime_Type : in String;
      Base_URI  : in String := None;
      Language  : String := None);
   --  Add a content element to the atom:entry element. Use this procedure if
   --  the content is of a MIME media type that is _not_ Text, Html or Xhtml.
   --  If the value of Mime_Type is _not_ an XML or text/ media type, then the
   --  value of Content must be Base64 encoded.

   procedure Set_Content_OutOfLine
     (Entr      : in out Atom_Entry;
      Mime_Type : in String;
      Source    : in String;
      Base_URI  : in String := None;
      Language  : in String := None);
   --  Add a content element to the atom:entry element. Use this procedure if
   --  the content of the entry is found at the Source IRI.

   procedure Set_Generator
     (Feed     : in out Atom_Feed;
      Agent    : in     String;
      Base_URI : in     String := None;
      Language : in     String := None;
      URI      : in     String := None;
      Version  : in     String := None);
   --  Identifies the agent used to generate a feed. The Agent is text, so
   --  characters such as < and > are escaped to &lt; and &gt;.
   --
   --  Agent:
   --    The agent used to generate the feed.
   --  Base_URI:
   --    See Set_Common.
   --  Language:
   --    See Set_Common.
   --  URI:
   --    Should point to a resource relevant to the Agent.
   --  Version:
   --    The version of the Agent.

   procedure Set_Generator_Source
     (Entr     : in out Atom_Entry;
      Agent    : in     String;
      Base_URI : in     String := None;
      Language : in     String := None;
      URI      : in     String := None;
      Version  : in     String := None);
   --  Identifies the agent used to generate an entry. The Agent is text, so
   --  characters such as < and > are escaped to &lt; and &gt;.
   --
   --  Agent:
   --    The agent used to generate the entry.
   --  Base_URI:
   --    See Set_Common.
   --  Language:
   --    See Set_Common.
   --  URI:
   --    Should point to a resource relevant to the Agent.
   --  Version:
   --    The version of the Agent.

   procedure Set_Icon
     (Feed     : in out Atom_Feed;
      URI      : in     String;
      Base_URI : in     String := None;
      Language : in     String := None);
   --  Identifies an image that provides iconic visual identification for a
   --  feed.
   --
   --  Base_URI:
   --    See Set_Common.
   --  Language:
   --    See Set_Common.
   --  URI
   --    URI to an image that provides iconic visual identification for a
   --    feed. The image SHOULD have an aspect ratio of one (horizontal) to one
   --    (vertical) and SHOULD be suitable for presentation at a small size.

   procedure Set_Icon_Source
     (Entr     : in out Atom_Entry;
      URI      : in     String;
      Base_URI : in     String := None;
      Language : in     String := None);
   --  Identifies an image that provides iconic visual identification for an
   --  entry.
   --
   --  Base_URI:
   --    See Set_Common.
   --  Language:
   --    See Set_Common.
   --  URI
   --    URI to an image that provides iconic visual identification for an
   --    entry. The image SHOULD have an aspect ratio of 1 (horizontal) to 1
   --    (vertical) and SHOULD be suitable for presentation at a small size.

   procedure Set_Id
     (Entr     : in out Atom_Entry;
      URI      : in     String;
      Base_URI : in     String := None;
      Language : in     String := None);
   --  Conveys a permanent, universally unique identifier for the entry.
   --  Its content MUST be an IRI, as defined by [RFC3987]. Note that the
   --  definition of "IRI" excludes relative references. Though the IRI might
   --  use a dereferencable scheme, Atom Processors MUST NOT assume it can be
   --  dereferenced. When an Atom Document is relocated, migrated, syndicated,
   --  republished, exported, or imported, the content of its atom : id element
   --  MUST NOT change. Put another way, an atom:id element pertains to all
   --  instantiations of a particular Atom feed; revisions retain the same
   --  content in their atom:id elements. It is suggested that the atom:id
   --  element be stored along with the associated resource. The content of an
   --  atom:id element MUST be created in a way that assures uniqueness.
   --
   --  Base_URI:
   --    See Set_Common.
   --  Language:
   --    See Set_Common.
   --  Id:
   --    A permanent, universally unique identifier for the entry.

   procedure Set_Id
     (Feed     : in out Atom_Feed;
      URI      : in     String;
      Base_URI : in     String := None;
      Language : in     String := None);
   --  Conveys a permanent, universally unique identifier for the feed.
   --  Its content MUST be an IRI, as defined by [RFC3987]. Note that the
   --  definition of "IRI" excludes relative references. Though the IRI might
   --  use a dereferencable scheme, Atom Processors MUST NOT assume it can be
   --  dereferenced. When an Atom Document is relocated, migrated, syndicated,
   --  republished, exported, or imported, the content of its atom : id element
   --  MUST NOT change. Put another way, an atom:id element pertains to all
   --  instantiations of a particular Atom feed; revisions retain the same
   --  content in their atom:id elements. It is suggested that the atom:id
   --  element be stored along with the associated resource. The content of an
   --  atom:id element MUST be created in a way that assures uniqueness.
   --
   --  Base_URI:
   --    See Set_Common.
   --  Language:
   --    See Set_Common.
   --  Id:
   --    A permanent, universally unique identifier for the feed.

   procedure Set_Id_Source
     (Entr     : in out Atom_Entry;
      URI      : in     String;
      Base_URI : in     String := None;
      Language : in     String := None);
   --  Conveys a permanent, universally unique identifier for the entry.
   --  Its content MUST be an IRI, as defined by [RFC3987]. Note that the
   --  definition of "IRI" excludes relative references. Though the IRI might
   --  use a dereferencable scheme, Atom Processors MUST NOT assume it can be
   --  dereferenced. When an Atom Document is relocated, migrated, syndicated,
   --  republished, exported, or imported, the content of its atom : id element
   --  MUST NOT change. Put another way, an atom:id element pertains to all
   --  instantiations of a particular Atom feed; revisions retain the same
   --  content in their atom:id elements. It is suggested that the atom:id
   --  element be stored along with the associated resource. The content of an
   --  atom:id element MUST be created in a way that assures uniqueness.
   --
   --  Base_URI:
   --    See Set_Common.
   --  Language:
   --    See Set_Common.
   --  Id:
   --    A permanent, universally unique identifier for the entry.

   procedure Set_Logo
     (Feed     : in out Atom_Feed;
      URI      : in     String;
      Base_URI : in     String := None;
      Language : in     String := None);
   --  Identifies an image that provides visual identification for a feed. The
   --  image SHOULD have an aspect ratio of 2 (horizontal) to 1 (vertical).
   --
   --  Base_URI:
   --    See Set_Common.
   --  Language:
   --    See Set_Common.
   --  URI:
   --    The URL to the image.

   procedure Set_Logo_Source
     (Entr     : in out Atom_Entry;
      URI      : in     String;
      Base_URI : in     String := None;
      Language : in     String := None);
   --  Identifies an image that provides visual identification for an entry.
   --  The image SHOULD have an aspect ratio of 2 (horizontal) to 1 (vertical).
   --
   --  Base_URI:
   --    See Set_Common.
   --  Language:
   --    See Set_Common.
   --  URI:
   --    The URL to the image.

   procedure Set_Published
     (Entr           : in out Atom_Entry;
      Published_Time : in     Ada.Calendar.Time;
      Base_URI       : in     String := None;
      Language       : in     String := None);
   --  Indicates the instant in time when an entry was published.
   --
   --  Published_Time:
   --    When did we publish the entry.
   --  Base_URI:
   --    See Set_Common.
   --  Language:
   --    See Set_Common.

   procedure Set_Rights
     (Entr        : in out Atom_Entry;
      Rights      : in     String;
      Base_URI    : in     String := None;
      Language    : in     String := None;
      Rights_Kind : in     Text_Kinds := Text);
   --  Conveys information about rights held in and over an entry. SHOULD NOT
   --  be used to convey machine - readable licensing information.
   --
   --  Rights:
   --    The actual text describing the rights.
   --  Base_URI:
   --    See Set_Common.
   --  Language:
   --    See Set_Common.
   --  Rights_Kind:
   --    The rights kind. See Content_Type.

   procedure Set_Rights
     (Feed        : in out Atom_Feed;
      Rights      : in     String;
      Base_URI    : in     String := None;
      Language    : in     String := None;
      Rights_Kind : in     Text_Kinds := Text);
   --  Conveys information about rights held in and over a feed. SHOULD NOT be
   --  used to convey machine - readable licensing information.
   --
   --  Rights:
   --    The actual text describing the rights.
   --  Base_URI:
   --    See Set_Common.
   --  Language:
   --    See Set_Common.
   --  Rights_Kind:
   --    The rights kind. See Content_Type.

   procedure Set_Rights_Source
     (Entr        : in out Atom_Entry;
      Rights      : in     String;
      Base_URI    : in     String := None;
      Language    : in     String := None;
      Rights_Kind : in     Text_Kinds := Text);
   --  Conveys information about rights held in and over an entry. SHOULD NOT
   --  be used to convey machine - readable licensing information.
   --
   --  Rights:
   --    The actual text describing the rights.
   --  Base_URI:
   --    See Set_Common.
   --  Language:
   --    See Set_Common.
   --  Rights_Kind:
   --    The rights kind. See Content_Type.

   procedure Set_Subtitle
     (Feed           : in out Atom_Feed;
      Subtitle       : in     String;
      Base_URI       : in     String := None;
      Language       : in     String := None;
      Subtitle_Kind  : in     Text_Kinds := Text);
   --  Conveys a human-readable description or subtitle for a feed.
   --
   --  Subtitle:
   --    The Subtitle is supposed to expand upon the Title.
   --  Base_URI:
   --    See Set_Common.
   --  Language:
   --    See Set_Common.
   --  Subtitle_Kind:
   --    The rights kind. See Content_Type.

   procedure Set_Subtitle_Source
     (Entr           : in out Atom_Entry;
      Subtitle       : in     String;
      Base_URI       : in     String := None;
      Language       : in     String := None;
      Subtitle_Kind  : in     Text_Kinds := Text);
   --  Conveys a human-readable description or subtitle for an entry.
   --
   --  Subtitle:
   --    The Subtitle is supposed to expand upon the Title.
   --  Base_URI:
   --    See Set_Common.
   --  Language:
   --    See Set_Common.
   --  Subtitle_Kind:
   --    The rights kind. See Content_Type.

   procedure Set_Summary
     (Entr           : in out Atom_Entry;
      Summary        : in     String;
      Base_URI       : in     String := None;
      Language       : in     String := None;
      Summary_Kind   : in     Text_Kinds := Text);
   --  Conveys a short summary, abstract, or excerpt of an entry.
   --
   --  Base_URI:
   --    See Set_Common.
   --  Language:
   --    See Set_Common.
   --  Summary:
   --    A human-readable summary of the entry.
   --  Summary_Kind:
   --    The summary kind. See Content_Type.

   procedure Set_Title
     (Entr       : in out Atom_Entry;
      Title      : in     String;
      Base_URI   : in     String := None;
      Language   : in     String := None;
      Title_Kind : in     Text_Kinds := Text);
   --  Conveys a human-readable title for the entry.
   --
   --  Base_URI:
   --    See Set_Common.
   --  Language:
   --    See Set_Common.
   --  Title:
   --    A human-readable title for the entry.
   --  Title_Kind:
   --    The title kind. See Content_Type.

   procedure Set_Title
     (Feed       : in out Atom_Feed;
      Title      : in     String;
      Base_URI   : in     String := None;
      Language   : in     String := None;
      Title_Kind : in     Text_Kinds := Text);
   --  Conveys a human-readable title for the feed.
   --
   --  Base_URI:
   --    See Set_Common.
   --  Language:
   --    See Set_Common.
   --  Title:
   --    A human-readable title for the feed.
   --  Title_Kind:
   --    The title kind. See Content_Type.

   procedure Set_Title_Source
     (Entr       : in out Atom_Entry;
      Title      : in     String;
      Base_URI   : in     String := None;
      Language   : in     String := None;
      Title_Kind : in     Text_Kinds := Text);
   --  Conveys a human-readable title for the entry.
   --
   --  Base_URI:
   --    See Set_Common.
   --  Language:
   --    See Set_Common.
   --  Title:
   --    A human-readable title for the entry.
   --  Title_Kind:
   --    The title kind. See Content_Type.

   procedure Set_Updated
     (Entr        : in out Atom_Entry;
      Update_Time : in     Ada.Calendar.Time;
      Base_URI    : in     String := None;
      Language    : in     String := None);
   --  Indicates the most recent instant in time when an entry was modified in
   --  a way the publisher considers significant.
   --
   --  Update_Time:
   --    When did the update occur.
   --  Base_URI:
   --    See Set_Common.
   --  Language:
   --    See Set_Common.

   procedure Set_Updated
     (Feed        : in out Atom_Feed;
      Update_Time : in     Ada.Calendar.Time;
      Base_URI    : in     String := None;
      Language    : in     String := None);
   --  Indicates the most recent instant in time when a feed was modified in a
   --  way the publisher considers significant.
   --
   --  Update_Time:
   --    When did the update occur.
   --  Base_URI:
   --    See Set_Common.
   --  Language:
   --    See Set_Common.

   procedure Set_Updated_Source
     (Entr        : in out Atom_Entry;
      Update_Time : in     Ada.Calendar.Time;
      Base_URI    : in     String := None;
      Language    : in     String := None);
   --  Indicates the most recent instant in time when an entry was modified in
   --  a way the publisher considers significant.
   --
   --  Update_Time:
   --    When did the update occur.
   --  Base_URI:
   --    See Set_Common.
   --  Language:
   --    See Set_Common.

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
         Content  : Unbounded_String;
         Label    : Unbounded_String;
         Scheme   : Unbounded_String;
         Term     : Unbounded_String;
      end record;

   type Atom_Date is
      record
         Common      : Atom_Common;
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

   package Entry_List is new Doubly_Linked_Lists (Atom_Entry);

   protected type PT_Atom_Feed is

      procedure Add_Author
        (Value : in Atom_Person);

      procedure Add_Category
        (Value : in Atom_Category);

      procedure Add_Contributor
        (Value : in Atom_Person);

      procedure Add_Entry
        (Value : in Atom_Entry);

      procedure Add_Link
        (Value : in Atom_Link);

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
      Rights         : Atom_Text;
      Subtitle       : Atom_Text;
      Title          : Atom_Text;
      Updated        : Atom_Date;

   end PT_Atom_Feed;

   type Atom_Feed is
      record
         PAF : PT_Atom_Feed;
      end record;

end Yolk.Syndication.Writer;
