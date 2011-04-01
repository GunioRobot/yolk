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
with Yolk.Utilities;

package body Yolk.Syndication.Writer is

   function Atom_Date_Image
     (Time_Stamp : in Ada.Calendar.Time)
      return String;
   --  Return a string representation of the Time_Stamp time. The format is:
   --    yyyy-mm-ddThh:mm:ssZ
   --  The uppercase T and Z are requried as per the Atom specification.
   --  It is expected that the Time_Stamp is GMT.

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

         Document : DOM.Core.Document;
         Impl     : DOM_Implementation;
         Feed     : Node;
         Elem     : Node;
         Child    : Node;
         Text     : DOM.Core.Text;

         pragma Unreferenced (Child);

      begin

         Document := Create_Document (Implementation => Impl);

         Feed := Create_Element (Doc      => Document,
                                 Tag_Name => "feed");
         Set_Attribute (Elem  => Feed,
                        Name  => "xmlns",
                        Value => XMLNS);
         if Common.Base_URI /= Null_Unbounded_String then
            Set_Attribute (Elem  => Feed,
                           Name  => "base",
                           Value => TS (Common.Base_URI));
         end if;
         if Common.Language /= Null_Unbounded_String then
            Set_Attribute (Elem  => Feed,
                           Name  => "lang",
                           Value => TS (Common.Language));
         end if;
         Child := Append_Child (N         => Document,
                                New_Child => Feed);

         Elem := Create_Element (Doc      => Document,
                                 Tag_Name => "updated");
         Child := Append_Child (N         => Feed,
                                New_Child => Elem);
         Text := Create_Text_Node (Doc  => Document,
                                   Data => Atom_Date_Image (Clock));
         Child := Append_Child (N         => Elem,
                                New_Child => Text);
         Child := Append_Child (N         => Feed,
                                New_Child => Elem);

         return Document;

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

         DOM.Core.Nodes.Write (Stream  => Output'Access,
                               N       => Doc);

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

end Yolk.Syndication.Writer;
