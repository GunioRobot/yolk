-------------------------------------------------------------------------------
--                                                                           --
--                                  Yolk                                     --
--                                                                           --
--                      Yolk.Syndication.DOM_Builder                         --
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

private package Yolk.Syndication.DOM_Builder is

   function Atom_Date_Image
     (Time_Stamp : in Ada.Calendar.Time)
      return String;
   --  Return a string representation of the Time_Stamp time. The format is:
   --    yyyy-mm-ddThh:mm:ssZ
   --  The uppercase T and Z are requried as per the Atom specification.
   --  It is expected that the Time_Stamp is GMT.

   procedure Attribute
     (Elem  : in DOM.Core.Node;
      Name  : in String;
      Value : in String);
   --  Add the attribute Name to Elem if Value isn't empty.

   procedure Create_Category_Elements
     (Doc    : in DOM.Core.Document;
      List   : in Category_List.List;
      Parent : in DOM.Core.Node);
   --  Add atom:category elements to Parent.

   procedure Create_Content_Element
     (Doc           : in DOM.Core.Document;
      Entry_Content : in Atom_Entry_Content;
      Parent        : in DOM.Core.Node);
   --  Add atom:content element to Parent.

   procedure Create_Entry_Elements
     (Doc     : in DOM.Core.Document;
      Entries : in Entry_List.List;
      Parent  : in DOM.Core.Node);
   --  Add the atom:entry elements.

   procedure Create_Entry_Source_Element
     (Doc    : in DOM.Core.Document;
      Source : in Atom_Entry_Source;
      Parent : in DOM.Core.Node);
   --  Add an atom:source element to Parent.

   procedure Create_Feed_Element
     (Authors      : in Person_List.List;
      Categories   : in Category_List.List;
      Common       : in Atom_Common;
      Contributors : in Person_List.List;
      Doc          : in DOM.Core.Document;
      Entries      : in Entry_List.List;
      Generator    : in Atom_Generator;
      Icon         : in Atom_Icon;
      Id           : in Atom_Id;
      Links        : in Link_List.List;
      Logo         : in Atom_Logo;
      Rights       : in Atom_Text;
      Subtitle     : in Atom_Text;
      Title        : in Atom_Text;
      Updated      : in Atom_Date);
   --  Add the atom:feed element.

   procedure Create_Generator_Element
     (A_Generator : in Atom_Generator;
      Doc         : in DOM.Core.Document;
      Parent      : in DOM.Core.Node);
   --  Add atom:generator element to Parent.

   procedure Create_Generic_Element
     (Common    : in Atom_Common;
      Data      : in String;
      Doc       : in DOM.Core.Document;
      Elem_Name : in String;
      Parent    : in DOM.Core.Node);
   --  Add a generic element to Parent. A generic element has the
   --  following structure:
   --
   --  <Elem_Name xml:base="Common.Base_URI" xml:lang="Common.Language">
   --     Data
   --  </Elem_Name>

   procedure Create_Link_Elements
     (Doc    : in DOM.Core.Document;
      List   : in Link_List.List;
      Parent : in DOM.Core.Node);
   --  Add atom:link elements to Parent.

   function Create_Node_From_String
     (XML_String : in String)
      return DOM.Core.Node;
   --  Return a DOM node based on the given XML_String.

   procedure Create_Person_Elements
     (Doc       : in DOM.Core.Document;
      Elem_Name : in String;
      List      : in Person_List.List;
      Parent    : in DOM.Core.Node);
   --  Add atom:person elements to Parent.

   procedure Create_Text_Construct
     (Common    : in Atom_Common;
      Data      : in String;
      Doc       : in DOM.Core.Document;
      Elem_Name : in String;
      Parent    : in DOM.Core.Node;
      Text_Kind : in Text_Kinds);
   --  Set the type (text/html/xhtml) and content of an atomTextConstruct
   --  element.

end Yolk.Syndication.DOM_Builder;
