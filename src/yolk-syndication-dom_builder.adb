-------------------------------------------------------------------------------
--                                                                           --
--                                  Yolk                                     --
--                                                                           --
--                       Yolk.Syndication.DOM_Builder                        --
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
with Ada.Strings.Fixed;
with DOM.Core.Documents;
with DOM.Core.Elements;
with DOM.Core.Nodes;
with DOM.Readers;
with Input_Sources.Strings;
with Sax.Readers;
with Unicode.CES.Utf8;
with Yolk.Utilities;

package body Yolk.Syndication.DOM_Builder is

   -----------------------
   --  Atom_Date_Image  --
   -----------------------

   function Atom_Date_Image
     (Time_Stamp : in Ada.Calendar.Time)
      return String
   is

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
   --  Attribute  --
   -----------------

   procedure Attribute
     (Elem  : in DOM.Core.Node;
      Name  : in String;
      Value : in String)
   is

      use DOM.Core.Elements;

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
     (Doc    : in DOM.Core.Document;
      List   : in Category_List.List;
      Parent : in DOM.Core.Node)
   is

      use DOM.Core;
      use DOM.Core.Documents;
      use DOM.Core.Elements;
      use DOM.Core.Nodes;
      use Yolk.Utilities;

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
                    Name  => "xml:base",
                    Value => TS (A_Category.Common.Base_URI));

         Attribute (Elem  => Category_Node,
                    Name  => "xml:lang",
                    Value => TS (A_Category.Common.Language));

         Attribute (Elem  => Category_Node,
                    Name  => "label",
                    Value => TS (A_Category.Label));

         Attribute (Elem  => Category_Node,
                    Name  => "scheme",
                    Value => TS (A_Category.Scheme));

         Category_List.Next (C);
      end loop;

   end Create_Category_Elements;

   ------------------------------
   --  Create_Content_Element  --
   ------------------------------

   procedure Create_Content_Element
     (Doc           : in DOM.Core.Document;
      Entry_Content : in Atom_Entry_Content;
      Parent        : in DOM.Core.Node)
   is

      use DOM.Core;
      use DOM.Core.Documents;
      use DOM.Core.Nodes;
      use Yolk.Utilities;

      Content_Node : Node;

   begin

      case Entry_Content.Content_Kind is
         when Text | Html | Xhtml =>
            Create_Text_Construct
              (Common    => Entry_Content.Common,
               Data      => TS (Entry_Content.Content),
               Doc       => Doc,
               Elem_Name => "content",
               Parent    => Parent,
               Text_Kind => Entry_Content.Content_Kind);
         when others =>
            Content_Node := Append_Child
              (N         => Parent,
               New_Child => Create_Element (Doc      => Doc,
                                            Tag_Name => "content"));

            Attribute (Elem  => Content_Node,
                       Name  => "xml:base",
                       Value => TS (Entry_Content.Common.Base_URI));

            Attribute (Elem  => Content_Node,
                       Name  => "xml:lang",
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

   -----------------------------
   --  Create_Entry_Elements  --
   -----------------------------

   procedure Create_Entry_Elements
     (Doc     : in DOM.Core.Document;
      Entries : in Entry_List.List;
      Parent  : in DOM.Core.Node)
   is

      use DOM.Core;
      use DOM.Core.Documents;
      use DOM.Core.Nodes;
      use Yolk.Utilities;

      An_Entry   : Atom_Entry;
      C          : Entry_List.Cursor := Entries.First;
      Entry_Node : Node;

   begin

      loop
         exit when not Entry_List.Has_Element (C);

         An_Entry := Entry_List.Element (C);

         Entry_Node := Append_Child
           (N         => Parent,
            New_Child => Create_Element (Doc      => Doc,
                                         Tag_Name => "entry"));

         Attribute (Elem  => Entry_Node,
                    Name  => "xml:base",
                    Value => TS (An_Entry.Common.Base_URI));

         Attribute (Elem  => Entry_Node,
                    Name  => "xml:lang",
                    Value => TS (An_Entry.Common.Language));

         --  entry:author elements
         Create_Person_Elements (Doc       => Doc,
                                 Elem_Name => "author",
                                 List      => An_Entry.Authors,
                                 Parent    => Entry_Node);

         --  entry:category elements
         Create_Category_Elements (Doc    => Doc,
                                   List   => An_Entry.Categories,
                                   Parent => Entry_Node);

         --  entry:content element
         Create_Content_Element (Doc            => Doc,
                                 Entry_Content  => An_Entry.Content,
                                 Parent         => Entry_Node);

         --  entry:contributor elements
         Create_Person_Elements (Doc       => Doc,
                                 Elem_Name => "contributor",
                                 List      => An_Entry.Contributors,
                                 Parent    => Entry_Node);

         --  entry:id element
         Create_Generic_Element (Common    => An_Entry.Id.Common,
                                 Data      => TS (An_Entry.Id.URI),
                                 Doc       => Doc,
                                 Elem_Name => "id",
                                 Parent    => Entry_Node);

         --  entry:link elements
         Create_Link_Elements (Doc    => Doc,
                               List   => An_Entry.Links,
                               Parent => Entry_Node);

         --  entry:published element
         Create_Generic_Element
           (Common    => An_Entry.Published.Common,
            Data      => Atom_Date_Image
              (Time_Stamp => An_Entry.Published.Time_Stamp),
            Doc       => Doc,
            Elem_Name => "published",
            Parent    => Entry_Node);

         --  entry:rights
         if An_Entry.Rights.Text_Content /= Null_Unbounded_String then
            Create_Text_Construct
              (Common    => An_Entry.Rights.Common,
               Data      => TS (An_Entry.Rights.Text_Content),
               Doc       => Doc,
               Elem_Name => "rights",
               Parent    => Entry_Node,
               Text_Kind => An_Entry.Rights.Text_Kind);
         end if;

         --  entry:source element
         if An_Entry.Source /= Null_Atom_Entry_Source then
            Create_Entry_Source_Element (Doc    => Doc,
                                         Source => An_Entry.Source,
                                         Parent => Entry_Node);
         end if;

         --  entry:summary element
         Create_Text_Construct
           (Common    => An_Entry.Summary.Common,
            Data      => TS (An_Entry.Summary.Text_Content),
            Doc       => Doc,
            Elem_Name => "summary",
            Parent    => Entry_Node,
            Text_Kind => An_Entry.Summary.Text_Kind);

         --  entry:title element
         Create_Text_Construct
           (Common    => An_Entry.Title.Common,
            Data      => TS (An_Entry.Title.Text_Content),
            Doc       => Doc,
            Elem_Name => "title",
            Parent    => Entry_Node,
            Text_Kind => An_Entry.Title.Text_Kind);

         --  entry:updated element
         if An_Entry.Updated.Is_Set then
            Create_Generic_Element
              (Common    => An_Entry.Common,
               Data      => Atom_Date_Image
                 (Time_Stamp => An_Entry.Updated.Time_Stamp),
               Doc       => Doc,
               Elem_Name => "updated",
               Parent    => Entry_Node);
         end if;

         Entry_List.Next (C);
      end loop;

   end Create_Entry_Elements;

   -----------------------------------
   --  Create_Entry_Source_Element  --
   -----------------------------------

   procedure Create_Entry_Source_Element
     (Doc    : in DOM.Core.Document;
      Source : in Atom_Entry_Source;
      Parent : in DOM.Core.Node)
   is

      use DOM.Core;
      use DOM.Core.Documents;
      use DOM.Core.Nodes;
      use Yolk.Utilities;

      Source_Node : Node;

   begin

      Source_Node := Append_Child
        (N         => Parent,
         New_Child => Create_Element (Doc      => Doc,
                                      Tag_Name => "source"));

      Attribute (Elem  => Source_Node,
                 Name  => "xml:base",
                 Value => TS (Source.Common.Base_URI));

      Attribute (Elem  => Source_Node,
                 Name  => "xml:lang",
                 Value => TS (Source.Common.Language));

      Create_Person_Elements (Doc       => Doc,
                              Elem_Name => "author",
                              List      => Source.Authors,
                              Parent    => Source_Node);

      Create_Category_Elements (Doc    => Doc,
                                List   => Source.Categories,
                                Parent => Source_Node);

      Create_Person_Elements (Doc       => Doc,
                              Elem_Name => "contributor",
                              List      => Source.Contributors,
                              Parent    => Source_Node);

      Create_Generator_Element (A_Generator => Source.Generator,
                                Doc         => Doc,
                                Parent      => Source_Node);

      if Source.Icon.URI /= Null_Unbounded_String then
         Create_Generic_Element (Common    => Source.Icon.Common,
                                 Data      => TS (Source.Icon.URI),
                                 Doc       => Doc,
                                 Elem_Name => "icon",
                                 Parent    => Source_Node);
      end if;

      if Source.Id.URI /= Null_Unbounded_String then
         Create_Generic_Element (Common    => Source.Id.Common,
                                 Data      => TS (Source.Id.URI),
                                 Doc       => Doc,
                                 Elem_Name => "id",
                                 Parent    => Source_Node);
      end if;
      Create_Link_Elements (Doc    => Doc,
                            List   => Source.Links,
                            Parent => Source_Node);

      if Source.Logo.URI /= Null_Unbounded_String then
         Create_Generic_Element (Common    => Source.Logo.Common,
                                 Data      => TS (Source.Logo.URI),
                                 Doc       => Doc,
                                 Elem_Name => "logo",
                                 Parent    => Source_Node);
      end if;

      if Source.Rights.Text_Content /= Null_Unbounded_String then
         Create_Text_Construct
           (Common    => Source.Rights.Common,
            Data      => TS (Source.Rights.Text_Content),
            Doc       => Doc,
            Elem_Name => "rights",
            Parent    => Source_Node,
            Text_Kind => Source.Rights.Text_Kind);
      end if;

      if Source.Subtitle.Text_Content /= Null_Unbounded_String then
         Create_Text_Construct
           (Common    => Source.Subtitle.Common,
            Data      => TS (Source.Subtitle.Text_Content),
            Doc       => Doc,
            Elem_Name => "subtitle",
            Parent    => Source_Node,
            Text_Kind => Source.Subtitle.Text_Kind);
      end if;

      if Source.Title.Text_Content /= Null_Unbounded_String then
         Create_Text_Construct
           (Common    => Source.Title.Common,
            Data      => TS (Source.Title.Text_Content),
            Doc       => Doc,
            Elem_Name => "title",
            Parent    => Source_Node,
            Text_Kind => Source.Title.Text_Kind);
      end if;

      if Source.Updated.Is_Set then
         Create_Generic_Element
           (Common    => Source.Updated.Common,
            Data      =>
              Atom_Date_Image (Time_Stamp => Source.Updated.Time_Stamp),
            Doc       => Doc,
            Elem_Name => "updated",
            Parent    => Source_Node);
      end if;

   end Create_Entry_Source_Element;

   ---------------------------
   --  Create_Feed_Element  --
   ---------------------------

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
      Updated      : in Atom_Date)
   is

      use DOM.Core;
      use DOM.Core.Documents;
      use DOM.Core.Elements;
      use DOM.Core.Nodes;
      use Yolk.Utilities;

      Feed_Node : Node;

   begin

      --  feed element
      Feed_Node := Append_Child
        (N         => Doc,
         New_Child => Create_Element (Doc      => Doc,
                                      Tag_Name => "feed"));

      Set_Attribute (Elem  => Feed_Node,
                     Name  => "xmlns",
                     Value => XMLNS);

      Attribute (Elem  => Feed_Node,
                 Name  => "xml:base",
                 Value => TS (Common.Base_URI));

      Attribute (Elem  => Feed_Node,
                 Name  => "xml:lang",
                 Value => TS (Common.Language));

      --  feed:author elements
      Create_Person_Elements (Doc       => Doc,
                              Elem_Name => "author",
                              List      => Authors,
                              Parent    => Feed_Node);

      --  feed:category elements
      Create_Category_Elements (Doc    => Doc,
                                List   => Categories,
                                Parent => Feed_Node);

      --  feed:contributor elements
      Create_Person_Elements (Doc       => Doc,
                              Elem_Name => "contributor",
                              List      => Contributors,
                              Parent    => Feed_Node);

      --  feed:generator element
      Create_Generator_Element (A_Generator => Generator,
                                Doc         => Doc,
                                Parent      => Feed_Node);

      --  feed:icon element
      if Icon.URI /= Null_Unbounded_String then
         Create_Generic_Element (Common    => Icon.Common,
                                 Data      => TS (Icon.URI),
                                 Doc       => Doc,
                                 Elem_Name => "icon",
                                 Parent    => Feed_Node);
      end if;

      --  feed:id element
      if Id.URI /= Null_Unbounded_String then
         Create_Generic_Element (Common    => Id.Common,
                                 Data      => TS (Id.URI),
                                 Doc       => Doc,
                                 Elem_Name => "id",
                                 Parent    => Feed_Node);
      end if;

      --  feed:link elements
      Create_Link_Elements (Doc    => Doc,
                            List   => Links,
                            Parent => Feed_Node);

      --  feed:logo
      if Logo.URI /= Null_Unbounded_String then
         Create_Generic_Element (Common    => Logo.Common,
                                 Data      => TS (Logo.URI),
                                 Doc       => Doc,
                                 Elem_Name => "logo",
                                 Parent    => Feed_Node);
      end if;

      --  feed:rights
      if Rights.Text_Content /= Null_Unbounded_String then
         Create_Text_Construct (Common    => Rights.Common,
                                Data      => TS (Rights.Text_Content),
                                Doc       => Doc,
                                Elem_Name => "rights",
                                Parent    => Feed_Node,
                                Text_Kind => Rights.Text_Kind);
      end if;

      --  feed:subtitle
      if Subtitle.Text_Content /= Null_Unbounded_String then
         Create_Text_Construct (Common    => Subtitle.Common,
                                Data      => TS (Subtitle.Text_Content),
                                Doc       => Doc,
                                Elem_Name => "subtitle",
                                Parent    => Feed_Node,
                                Text_Kind => Subtitle.Text_Kind);
      end if;

      --  feed:title element
      if Title.Text_Content /= Null_Unbounded_String  then
         Create_Text_Construct (Common    => Title.Common,
                                Data      => TS (Title.Text_Content),
                                Doc       => Doc,
                                Elem_Name => "title",
                                Parent    => Feed_Node,
                                Text_Kind => Title.Text_Kind);
      end if;

      --  feed:updated element
      if Updated.Is_Set then
         Create_Generic_Element
           (Common    => Updated.Common,
            Data      => Atom_Date_Image (Time_Stamp => Updated.Time_Stamp),
            Doc       => Doc,
            Elem_Name => "updated",
            Parent    => Feed_Node);
      end if;

      Create_Entry_Elements (Doc     => Doc,
                             Entries => Entries,
                             Parent  => Feed_Node);

   end Create_Feed_Element;

   --------------------------------
   --  Create_Generator_Element  --
   --------------------------------

   procedure Create_Generator_Element
     (A_Generator : in Atom_Generator;
      Doc         : in DOM.Core.Document;
      Parent      : in DOM.Core.Node)
   is

      use DOM.Core;
      use DOM.Core.Documents;
      use DOM.Core.Nodes;
      use Yolk.Utilities;

      Generator_Node : Node;

   begin

      if A_Generator.Agent /= Null_Unbounded_String then
         Generator_Node := Append_Child
           (N         => Parent,
            New_Child => Create_Element (Doc      => Doc,
                                         Tag_Name => "generator"));

         Attribute (Elem  => Generator_Node,
                    Name  => "xml:base",
                    Value => TS (A_Generator.Common.Base_URI));

         Attribute (Elem  => Generator_Node,
                    Name  => "xml:lang",
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
     (Common    : in Atom_Common;
      Data      : in String;
      Doc       : in DOM.Core.Document;
      Elem_Name : in String;
      Parent    : in DOM.Core.Node)
   is

      use DOM.Core;
      use DOM.Core.Documents;
      use DOM.Core.Nodes;
      use Yolk.Utilities;

      Elem_Node : Node;

   begin

      Elem_Node := Append_Child
        (N         => Parent,
         New_Child => Create_Element (Doc      => Doc,
                                      Tag_Name => Elem_Name));

      Attribute (Elem  => Elem_Node,
                 Name  => "xml:base",
                 Value => TS (Common.Base_URI));

      Attribute (Elem  => Elem_Node,
                 Name  => "xml:lang",
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
     (Doc    : in DOM.Core.Document;
      List   : in Link_List.List;
      Parent : in DOM.Core.Node)
   is

      use Ada.Strings;
      use DOM.Core;
      use DOM.Core.Documents;
      use DOM.Core.Elements;
      use DOM.Core.Nodes;
      use Yolk.Utilities;

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

         Link_List.Next (C);
      end loop;

   end Create_Link_Elements;

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

   ------------------------------
   --  Create_Person_Elements  --
   ------------------------------

   procedure Create_Person_Elements
     (Doc       : in DOM.Core.Document;
      Elem_Name : in String;
      List      : in Person_List.List;
      Parent    : in DOM.Core.Node)
   is

      use DOM.Core;
      use DOM.Core.Documents;
      use DOM.Core.Nodes;
      use Yolk.Utilities;

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
                    Name  => "xml:base",
                    Value => TS (A_Person.Common.Base_URI));

         Attribute (Elem  => Person_Node,
                    Name  => "xml:lang",
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
   --  Create_Text_Construct  --
   -----------------------------

   procedure Create_Text_Construct
     (Common    : in Atom_Common;
      Data      : in String;
      Doc       : in DOM.Core.Document;
      Elem_Name : in String;
      Parent    : in DOM.Core.Node;
      Text_Kind : in Text_Kinds)
   is

      use DOM.Core;
      use DOM.Core.Documents;
      use DOM.Core.Elements;
      use DOM.Core.Nodes;
      use Yolk.Utilities;

      Elem_Node : Node;

   begin

      Elem_Node := Append_Child
        (N         => Parent,
         New_Child => Create_Element (Doc      => Doc,
                                      Tag_Name => Elem_Name));

      Attribute (Elem  => Elem_Node,
                 Name  => "xml:base",
                 Value => TS (Common.Base_URI));

      Attribute (Elem  => Elem_Node,
                 Name  => "xml:lang",
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

end Yolk.Syndication.DOM_Builder;
