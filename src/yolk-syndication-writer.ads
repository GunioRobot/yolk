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

--  Create ATOM XML, as per RFC4287

package Yolk.Syndication.Writer is

   procedure Add_Author
     (Feed     : in out Atom_Feed;
      Name     : in     String;
      Base_URI : in     String := None;
      Email    : in     String := None;
      Language : in     String := None;
      URI      : in     String := None);
   --  Add an author to the feed/entry.
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
     (Entr     : in out Atom_Entry;
      Name     : in     String;
      Base_URI : in     String := None;
      Email    : in     String := None;
      Language : in     String := None;
      URI      : in     String := None);
   --  Add an author to the feed/entry.
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
     (Entry_Source : in out Atom_Entry_Source;
      Name         : in     String;
      Base_URI     : in     String := None;
      Email        : in     String := None;
      Language     : in     String := None;
      URI          : in     String := None);
   --  Add a source author to the entry.
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
     (Feed     : in out Atom_Feed;
      Term     : in     String;
      Base_URI : in     String := None;
      Label    : in     String := None;
      Language : in     String := None;
      Scheme   : in     String := None);
   --  Add a category to the feed/entry. The category element conveys
   --  information about a category associated with an entry or feed.
   --
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
     (Entr     : in out Atom_Entry;
      Term     : in     String;
      Base_URI : in     String := None;
      Label    : in     String := None;
      Language : in     String := None;
      Scheme   : in     String := None);
   --  Add a category to the feed/entry. The category element conveys
   --  information about a category associated with an entry or feed.
   --
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
     (Entry_Source : in out Atom_Entry_Source;
      Term         : in     String;
      Base_URI     : in     String := None;
      Label        : in     String := None;
      Language     : in     String := None;
      Scheme       : in     String := None);
   --  Add a source category to the entry. The category element conveys
   --  information about a category associated with an entry or feed.
   --
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
     (Entry_Source : in out Atom_Entry_Source;
      Name         : in     String;
      Base_URI     : in     String := None;
      Email        : in     String := None;
      Language     : in     String := None;
      URI          : in     String := None);
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
     (Feed        : in out Atom_Feed;
      Entr        : in out Atom_Entry;
      Clear_Entry : in     Boolean := False);
   --  Add an entry element to the atom:feed element, placing it in the list
   --  according to its Atom_Entry.Updated value. Entries are sorted in the
   --  list with newest first. If an entry's Updated time is older than
   --  Atom_Feed.Max_Entry_Age, then it's only added if the list length is
   --  lesser than Atom_Feed.Min_Entries.
   --
   --  NOTE:
   --    This procedures considers two entries the same if the Atom_Entry.Id
   --    values are equal. If you add an entry with an Id that already exists
   --    in the list, then the old entry is deleted and the new one added.
   --
   --  This procedure also takes care of keeping the entry list up to date
   --  according to the Atom_Feed.Max_Entry_Age, Atom_Feed.Max_Entries and
   --  Atom_Feed.Min_Entries values.
   --
   --  If the list of entries are longer than Max_Entries, then the oldest
   --  entries are deleted until the list is Max_Entries long.
   --
   --  If an entry is older than Max_Entry_Age and the list is longer than
   --  Min_Entries then the entry is deleted. The Max_Entry_Age is compared to
   --  the Atom_Entry.Updated value.
   --
   --  No entries are deleted from the list of entries as long as the list is
   --  shorter than Min_Entries.
   --
   --  The Feed updated timestamp is set to the entries updated timestamp if
   --  the entry has been successfully added to the entry list and the entry
   --  updated timestamp is newer than the current Feed updated timestamp.
   --
   --  If Clear_Entry is True, then Entr is set to Null_Atom_Entry after the
   --  entry has  been added to Feed. This is handy if you're in a loop adding
   --  multiple entries, using the same Atom_Entry object.

   procedure Add_Link
     (Feed      : in out Atom_Feed;
      Href      : in     String;
      Base_URI  : in     String := None;
      Hreflang  : in     String := None;
      Language  : in     String := None;
      Length    : in     Natural := 0;
      Mime_Type : in     String := None;
      Rel       : in     Relation_Kinds := Alternate;
      Title     : in     String := None);
   --  Defines a reference from an entry or feed to a Web resource.
   --
   --  Base_URI:
   --    See Set_Common.
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
     (Entr      : in out Atom_Entry;
      Href      : in     String;
      Base_URI  : in     String := None;
      Hreflang  : in     String := None;
      Language  : in     String := None;
      Length    : in     Natural := 0;
      Mime_Type : in     String := None;
      Rel       : in     Relation_Kinds := Alternate;
      Title     : in     String := None);
   --  Defines a reference from an entry or feed to a Web resource.
   --
   --  Base_URI:
   --    See Set_Common.
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
     (Entry_Source : in out Atom_Entry_Source;
      Href         : in     String;
      Base_URI     : in     String := None;
      Hreflang     : in     String := None;
      Language     : in     String := None;
      Length       : in     Natural := 0;
      Mime_Type    : in     String := None;
      Rel          : in     Relation_Kinds := Alternate;
      Title        : in     String := None);
   --  Defines a reference from an entry or feed to a Web resource.
   --
   --  Base_URI:
   --    See Set_Common.
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

   function Amount_Of_Entries
     (Feed : in Atom_Feed)
      return Natural;
   --  Return the amount of entries in the Feed object.

   procedure Clear_Entry_List
     (Feed : in out Atom_Feed);
   --  Remove all entries from the Feed.

   procedure Delete_Entry
     (Feed : in out Atom_Feed;
      Id   : in     String);
   --  Delete all entries from Feed where the Atom_Entry.Id = Id. Match must
   --  be exact. "foo" is _not_ the same as "Foo".

   function Get_XML_DOM
     (Feed : in Atom_Feed)
      return DOM.Core.Document;
   --  Return the Atom XML DOM document.

   function Get_XML_String
     (Feed         : in Atom_Feed;
      Pretty_Print : in Boolean := False)
      return String;
   --  Return the Atom XML string.
   --  If Pretty_Print is True, then the XML output is indented properly. Only
   --  use this for debugging purposes, as it mangles whitespace.

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

   procedure Set_Common
     (Entry_Source : in out Atom_Entry_Source;
      Base_URI     : in     String := None;
      Language     : in     String := None);
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
      Content   : in     String;
      Mime_Type : in     String;
      Base_URI  : in     String := None;
      Language  : in     String := None);
   --  Add a content element to the atom:entry element. Use this procedure if
   --  the content is of a MIME media type that is _not_ Text, Html or Xhtml.
   --  If the value of Mime_Type is _not_ an XML or text/ media type, then the
   --  value of Content must be Base64 encoded.

   procedure Set_Content_OutOfLine
     (Entr      : in out Atom_Entry;
      Mime_Type : in     String;
      Source    : in     String;
      Base_URI  : in     String := None;
      Language  : in     String := None);
   --  Add a content element to the atom:entry element. Use this procedure if
   --  the content of the entry is found at the Source IRI.

   procedure Set_Entry_Source
     (Entr               : in out Atom_Entry;
      Source             : in out Atom_Entry_Source;
      Clear_Entry_Source : in     Boolean := False);
   --  If an entry is copied from another feed, it's possible to add the
   --  metadata of the originating feed to the entry. This is done using an
   --  Atom_Entry_Source object and its accompanying procedures. When the
   --  Atom_Entry_Source object has been build, you add it to the entry using
   --  this procedure.
   --
   --  If Clear_Entry_Source is True then Source is set to
   --  Null_Atom_Entry_Source after the entry source has  been added to Entr.
   --  This is handy if you're in a loop adding multiple entries, using the
   --  same Atom_Entry_Source object.

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

   procedure Set_Generator
     (Entry_Source : in out Atom_Entry_Source;
      Agent        : in     String;
      Base_URI     : in     String := None;
      Language     : in     String := None;
      URI          : in     String := None;
      Version      : in     String := None);
   --  Identifies the agent used to generate the original entry. The Agent is
   --  text, so characters such as < and > are escaped to &lt; and &gt;.
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

   procedure Set_Icon
     (Entry_Source : in out Atom_Entry_Source;
      URI          : in     String;
      Base_URI     : in     String := None;
      Language     : in     String := None);
   --  Identifies an image that provides iconic visual identification for the
   --  originating feed.
   --
   --  Base_URI:
   --    See Set_Common.
   --  Language:
   --    See Set_Common.
   --  URI
   --    URI to an image that provides iconic visual identification for the
   --    originating. The image SHOULD have an aspect ratio of 1 (horizontal)
   --    to 1 (vertical) and SHOULD be suitable for presentation at a small
   --    size.

   procedure Set_Id
     (Feed     : in out Atom_Feed;
      Id       : in     String;
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
   --  Id:
   --    A permanent, universally unique identifier for the entry.
   --  Language:
   --    See Set_Common.

   procedure Set_Id
     (Entr     : in out Atom_Entry;
      Id       : in     String;
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
   --  Id:
   --    A permanent, universally unique identifier for the entry.
   --  Language:
   --    See Set_Common.

   procedure Set_Id
     (Entry_Source : in out Atom_Entry_Source;
      Id           : in     String;
      Base_URI     : in     String := None;
      Language     : in     String := None);
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
   --  Id:
   --    A permanent, universally unique identifier for the entry.
   --  Language:
   --    See Set_Common.

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

   procedure Set_Logo
     (Entry_Source : in out Atom_Entry_Source;
      URI          : in     String;
      Base_URI     : in     String := None;
      Language     : in     String := None);
   --  Identifies an image that provides visual identification for the
   --  originating feed.
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
     (Entry_Source : in out Atom_Entry_Source;
      Rights       : in     String;
      Base_URI     : in     String := None;
      Language     : in     String := None;
      Rights_Kind  : in     Text_Kinds := Text);
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

   procedure Set_Subtitle
     (Entry_Source   : in out Atom_Entry_Source;
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
     (Entry_Source : in out Atom_Entry_Source;
      Title        : in     String;
      Base_URI     : in     String := None;
      Language     : in     String := None;
      Title_Kind   : in     Text_Kinds := Text);
   --  Conveys the human-readable title for the original entry.
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
     (Entry_Source : in out Atom_Entry_Source;
      Update_Time  : in     Ada.Calendar.Time;
      Base_URI     : in     String := None;
      Language     : in     String := None);
   --  Indicates the point in time when the original entry was modified in
   --  a way the publisher considered significant.
   --
   --  Update_Time:
   --    When did the update occur.
   --  Base_URI:
   --    See Set_Common.
   --  Language:
   --    See Set_Common.

end Yolk.Syndication.Writer;
