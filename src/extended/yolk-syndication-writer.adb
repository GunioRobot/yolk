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
with DOM.Core.Documents;
with DOM.Core.Elements;
with DOM.Core.Nodes;
with DOM.Readers;
with Input_Sources.Strings;
with Sax.Readers;
with Unicode.CES.Utf8;
with Yolk.Utilities;

package body Yolk.Syndication.Writer is

   function Atom_Date_Image
     (Time_Stamp : in Ada.Calendar.Time)
      return String;
   --  Return a string representation of the Time_Stamp time. The format is:
   --    yyyy-mm-ddThh:mm:ssZ
   --  The uppercase T and Z are requried as per the Atom specification.
   --  It is expected that the Time_Stamp is GMT.

   function Create_DOM_From_String
     (XML_String : in String)
      return DOM.Core.Document;
   --  Return a DOM document based on the given XML_String.

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

      use Yolk.Utilities;

   begin

      Feed.PAF.Add_Author
        (Value => Atom_Person'(Common =>
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
      Content  : in     String := None;
      Label    : in     String := None;
      Language : in     String := None;
      Scheme   : in     String := None)
   is

      use Yolk.Utilities;

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

      use Yolk.Utilities;

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

   ---------------------------
   --  Create_DOM_Document  --
   ---------------------------

   function Create_DOM_From_String
     (XML_String : in String)
      return DOM.Core.Document
   is

      use DOM.Core;
      use DOM.Readers;
      use Input_Sources.Strings;
      use Sax.Readers;

      Input  : String_Input;
      Reader : Tree_Reader;
      --  Doc    : Document;

   begin

      return Doc : Document do
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

         Doc := Get_Tree (Read => Reader);

      exception

         when others =>
            raise Not_Valid_XML with XML_String;

      end return;

   end Create_DOM_From_String;

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

      use Yolk.Utilities;

   begin

      return Feed.PAF.Get_String;

   end Get_XML_String;

   ------------------
   --  Initialize  --
   ------------------

   function Initialize
     (Id          : in String;
      Title       : in String;
      Base_URI    : in String := None;
      Language    : in String := None;
      Title_Kind  : in Content_Kind := Text)
      return Atom_Feed
   is

      use Yolk.Utilities;

      Common : constant Atom_Common := (Base_URI => TUS (Base_URI),
                                        Language => TUS (Language));

   begin

      return Feed : Atom_Feed do
         Feed.PAF.Set_Id (Value => Id);
         Feed.PAF.Set_Common (Value => Common);
         Feed.PAF.Set_Title
           (Value => Atom_Text'(Common         => Common,
                                Text_Content   => TUS (Title),
                                Text_Type      => Title_Kind));
      end return;

   end Initialize;

   ------------------
   --  Set_Common  --
   ------------------

   procedure Set_Common
     (Feed     : in out Atom_Feed;
      Base_URI : in     String := None;
      Language : in     String := None)
   is

      use Yolk.Utilities;

   begin

      Feed.PAF.Set_Common (Value => Atom_Common'(Base_URI => TUS (Base_URI),
                                                 Language => TUS (Language)));

   end Set_Common;

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

      use Yolk.Utilities;

   begin

      Feed.PAF.Set_Generator
        (Value => Atom_Generator'(Agent => TUS (Agent),
                                  Common  =>
                                    Atom_Common'(Base_URI => TUS (Base_URI),
                                                 Language => TUS (Language)),
                                  URI     => TUS (URI),
                                  Version => TUS (Version)));

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

      use Yolk.Utilities;

   begin

      Feed.PAF.Set_Icon
        (Value => Atom_Icon'(Common =>
                               Atom_Common'(Base_URI => TUS (Base_URI),
                                            Language => TUS (Language)),
                             URI    => TUS (URI)));

   end Set_Icon;

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

      ----------------
      --  Add_Link  --
      ----------------

      procedure Add_Link
        (Value : in Atom_Link)
      is
      begin

         Links.Append (Value);

      end Add_Link;

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
         use Yolk.Utilities;

         Doc         : Document;
         Impl        : DOM_Implementation;
         Feed_Node   : Node;

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

         if Common.Base_URI /= Null_Unbounded_String then
            Set_Attribute (Elem  => Feed_Node,
                           Name  => "base",
                           Value => TS (Common.Base_URI));
         end if;

         if Common.Language /= Null_Unbounded_String then
            Set_Attribute (Elem  => Feed_Node,
                           Name  => "lang",
                           Value => TS (Common.Language));
         end if;

         --  feed:id element
         Add_Id_To_DOM :
         declare

            Id_Node : Node;

         begin

            Id_Node := Append_Child
              (N         => Feed_Node,
               New_Child => Create_Element (Doc      => Doc,
                                            Tag_Name => "id"));

            Id_Node := Append_Child
              (N         => Id_Node,
               New_Child => Create_Text_Node (Doc, TS (Id)));

            pragma Unreferenced (Id_Node);
            --  We need this because XML/Ada have no Append_Child procedures,
            --  which obviously is annoying as hell.

         end Add_Id_To_DOM;

         --  feed:updated element
         Add_Updated_To_DOM :
         declare

            Updated_Node : Node;

         begin

            Updated_Node := Append_Child
              (N         => Feed_Node,
               New_Child => Create_Element (Doc      => Doc,
                                            Tag_Name => "updated"));

            Updated_Node := Append_Child
              (N         => Updated_Node,
               New_Child => Create_Text_Node
                 (Doc  => Doc,
                  Data => Atom_Date_Image (Time_Stamp => Clock)));

            pragma Unreferenced (Updated_Node);
            --  We need this because XML/Ada have no Append_Child procedures,
            --  which obviously is annoying as hell.

         end Add_Updated_To_DOM;

         --  feed:title element
         Add_Title_To_DOM :
         declare

            Title_Node  : Node;

         begin

            Title_Node := Append_Child (Feed_Node,
                                        Create_Element (Doc, "title"));

            case Title.Text_Type is
               when Text =>
                  Set_Attribute (Elem  => Title_Node,
                                 Name  => "type",
                                 Value => "text");
                  Title_Node := Append_Child
                    (N         => Title_Node,
                     New_Child => Create_Text_Node
                       (Doc  => Doc,
                        Data => TS (Title.Text_Content)));
               when Html =>
                  Set_Attribute (Elem  => Title_Node,
                                 Name  => "type",
                                 Value => "html");
                  Title_Node := Append_Child
                    (N         => Title_Node,
                     New_Child => Create_Text_Node
                       (Doc  => Doc,
                        Data => TS (Title.Text_Content)));
               when Xhtml =>
                  Set_Attribute (Elem  => Title_Node,
                                 Name  => "type",
                                 Value => "xhtml");
                  Set_Attribute (Elem  => Title_Node,
                                 Name  => "xmlns",
                                 Value => XHTMLNS);

                  Title_Node := Append_Child
                    (N         => Title_Node,
                     New_Child => First_Child
                       (N => Create_DOM_From_String
                          (XML_String => "<div>" &
                           TS (Title.Text_Content) &
                           "</div>")));

            end case;

         end Add_Title_To_DOM;

         --  feed:author elements
         Add_Authors_To_DOM :
         declare

            A_Person    : Atom_Person;
            Author_Node : Node;
            C           : Person_List.Cursor := Authors.First;
            Elem_Node   : Node;

         begin

            loop
               exit when not Person_List.Has_Element (C);

               A_Person := Person_List.Element (C);

               Author_Node := Append_Child
                 (N         => Feed_Node,
                  New_Child => Create_Element (Doc      => Doc,
                                               Tag_Name => "author"));

               if A_Person.Common.Base_URI /= Null_Unbounded_String then
                  Set_Attribute (Elem  => Author_Node,
                                 Name  => "base",
                                 Value => TS (A_Person.Common.Base_URI));
               end if;

               if A_Person.Common.Language /= Null_Unbounded_String then
                  Set_Attribute (Elem  => Author_Node,
                                 Name  => "lang",
                                 Value => TS (A_Person.Common.Language));
               end if;

               Elem_Node := Append_Child
                 (N         => Author_Node,
                  New_Child => Create_Element (Doc      => Doc,
                                               Tag_Name => "name"));
               Elem_Node := Append_Child
                 (N         => Elem_Node,
                  New_Child => Create_Text_Node (Doc  => Doc,
                                                 Data => TS (A_Person.Name)));

               if A_Person.Email /= Null_Unbounded_String then
                  Elem_Node := Append_Child
                    (N         => Author_Node,
                     New_Child => Create_Element (Doc      => Doc,
                                                  Tag_Name => "email"));
                  Elem_Node := Append_Child
                    (N => Elem_Node,
                     New_Child => Create_Text_Node
                       (Doc  => Doc,
                        Data => TS (A_Person.Email)));
               end if;

               if A_Person.URI /= Null_Unbounded_String then
                  Elem_Node := Append_Child
                    (N         => Author_Node,
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

         end Add_Authors_To_DOM;

         --  feed:category elements
         Add_Categories_To_Dom :
         declare

            A_Category     : Atom_Category;
            C              : Category_List.Cursor := Categories.First;
            Category_Node  : Node;

         begin

            loop
               exit when not Category_List.Has_Element (C);

               A_Category := Category_List.Element (C);

               Category_Node := Append_Child
                 (N         => Feed_Node,
                  New_Child => Create_Element (Doc      => Doc,
                                               Tag_Name => "category"));
               Set_Attribute (Elem  => Category_Node,
                              Name  => "term",
                              Value => TS (A_Category.Term));

               if A_Category.Common.Base_URI /= Null_Unbounded_String then
                  Set_Attribute (Elem  => Category_Node,
                                 Name  => "base",
                                 Value => TS (A_Category.Common.Base_URI));
               end if;
               if A_Category.Common.Language /= Null_Unbounded_String then
                  Set_Attribute (Elem  => Category_Node,
                                 Name  => "lang",
                                 Value => TS (A_Category.Common.Language));
               end if;

               if A_Category.Label /= Null_Unbounded_String then
                  Set_Attribute (Elem  => Category_Node,
                                 Name  => "label",
                                 Value => TS (A_Category.Label));
               end if;

               if A_Category.Scheme /= Null_Unbounded_String then
                  Set_Attribute (Elem  => Category_Node,
                                 Name  => "scheme",
                                 Value => TS (A_Category.Scheme));
               end if;

               if A_Category.Content /= Null_Unbounded_String then
                  Category_Node := Append_Child
                    (N         => Category_Node,
                     New_Child => Create_Text_Node
                       (Doc  => Doc,
                        Data => TS (A_Category.Content)));
               end if;

               Category_List.Next (C);
            end loop;

         end Add_Categories_To_Dom;

         --  feed:contributor elements
         Add_Contributors_To_DOM :
         declare

            A_Person          : Atom_Person;
            C                 : Person_List.Cursor := Contributors.First;
            Contributor_Node  : Node;
            Elem_Node         : Node;

         begin

            loop
               exit when not Person_List.Has_Element (C);

               A_Person := Person_List.Element (C);

               Contributor_Node := Append_Child
                 (N         => Feed_Node,
                  New_Child => Create_Element (Doc      => Doc,
                                               Tag_Name => "contributor"));

               if A_Person.Common.Base_URI /= Null_Unbounded_String then
                  Set_Attribute (Elem  => Contributor_Node,
                                 Name  => "base",
                                 Value => TS (A_Person.Common.Base_URI));
               end if;

               if A_Person.Common.Language /= Null_Unbounded_String then
                  Set_Attribute (Elem  => Contributor_Node,
                                 Name  => "lang",
                                 Value => TS (A_Person.Common.Language));
               end if;

               Elem_Node := Append_Child
                 (N         => Contributor_Node,
                  New_Child => Create_Element (Doc      => Doc,
                                               Tag_Name => "name"));
               Elem_Node := Append_Child
                 (N         => Elem_Node,
                  New_Child => Create_Text_Node (Doc  => Doc,
                                                 Data => TS (A_Person.Name)));

               if A_Person.Email /= Null_Unbounded_String then
                  Elem_Node := Append_Child
                    (N         => Contributor_Node,
                     New_Child => Create_Element (Doc      => Doc,
                                                  Tag_Name => "email"));
                  Elem_Node := Append_Child
                    (N => Elem_Node,
                     New_Child => Create_Text_Node
                       (Doc  => Doc,
                        Data => TS (A_Person.Email)));
               end if;

               if A_Person.URI /= Null_Unbounded_String then
                  Elem_Node := Append_Child
                    (N         => Contributor_Node,
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

         end Add_Contributors_To_DOM;

         --  feed:generator element
         if Generator.Agent /= Null_Unbounded_String then
            Add_Generator_To_DOM :
            declare

               Generator_Node : Node;

            begin

               Generator_Node := Append_Child
                 (N         => Feed_Node,
                  New_Child => Create_Element (Doc      => Doc,
                                               Tag_Name => "generator"));

               if Generator.Common.Base_URI /= Null_Unbounded_String then
                  Set_Attribute (Elem  => Generator_Node,
                                 Name  => "base",
                                 Value => TS (Generator.Common.Base_URI));
               end if;

               if Generator.Common.Language /= Null_Unbounded_String then
                  Set_Attribute (Elem  => Generator_Node,
                                 Name  => "language",
                                 Value => TS (Generator.Common.Language));
               end if;

               if Generator.URI /= Null_Unbounded_String then
                  Set_Attribute (Elem  => Generator_Node,
                                 Name  => "uri",
                                 Value => TS (Generator.URI));
               end if;

               if Generator.Version /= Null_Unbounded_String then
                  Set_Attribute (Elem  => Generator_Node,
                                 Name  => "version",
                                 Value => TS (Generator.Version));
               end if;

               Generator_Node := Append_Child
                 (N         => Generator_Node,
                  New_Child => Create_Text_Node
                    (Doc  => Doc,
                     Data => TS (Generator.Agent)));

            end Add_Generator_To_DOM;
         end if;

         --  feed:icon element
         if Icon.URI /= Null_Unbounded_String then
            Add_Icon_To_DOM :
            declare

               Icon_Node : Node;

            begin

               Icon_Node := Append_Child
                 (N         => Feed_Node,
                  New_Child => Create_Element (Doc      => Doc,
                                               Tag_Name => "icon"));

               if Icon.Common.Base_URI /= Null_Unbounded_String then
                  Set_Attribute (Elem  => Icon_Node,
                                 Name  => "base",
                                 Value => TS (Icon.Common.Base_URI));
               end if;

               if Icon.Common.Language /= Null_Unbounded_String  then
                  Set_Attribute (Elem  => Icon_Node,
                                 Name  => "language",
                                 Value => TS (Icon.Common.Language));
               end if;

               Icon_Node := Append_Child
                 (N         => Icon_Node,
                  New_Child => Create_Text_Node (Doc  => Doc,
                                                 Data => TS (Icon.URI)));

            end Add_Icon_To_DOM;
         end if;

         return Doc;

      end Get_DOM;

      ------------------
      --  Get_String  --
      ------------------

      function Get_String return String
      is

         use Ada.Streams;
         use DOM.Core.Nodes;
         use Yolk.Utilities;

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

         DOM.Core.Nodes.Write (Stream        => Output'Access,
                               N             => Doc,
                               Pretty_Print  => True);

         Free (Doc);

         return TS (Output.Str);

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
        (Value : in String)
      is

         use Yolk.Utilities;

      begin

         Id := TUS (Value);

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
        (Value : in Ada.Calendar.Time)
      is

         use Ada.Calendar;

      begin

         if Value > Updated then
            Updated := Value;
         end if;

      end Set_Updated_Time;

   end PT_Atom_Feed;

end Yolk.Syndication.Writer;
