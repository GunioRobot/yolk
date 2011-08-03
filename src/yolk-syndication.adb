-------------------------------------------------------------------------------
--                                                                           --
--                                  Yolk                                     --
--                                                                           --
--                             Yolk.Syndication                              --
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

with Ada.Streams;
with DOM.Core.Nodes;
with Yolk.Syndication.DOM_Builder;
with Yolk.Utilities;

package body Yolk.Syndication is

   use Yolk.Utilities;

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

   ----------------------
   --  New_Atom_Entry  --
   ----------------------

   function New_Atom_Entry
     (Base_URI : in String := None;
      Language : in String := None)
      return Atom_Entry
   is
   begin

      return An_Entry          : Atom_Entry := Null_Atom_Entry do
         if Base_URI /= None then
            An_Entry.Common.Base_URI := TUS (Base_URI);
         end if;

         if Language /= None then
            An_Entry.Common.Language := TUS (Language);
         end if;
      end return;

   end New_Atom_Entry;

   -----------------------------
   --  New_Atom_Entry_Source  --
   -----------------------------

   function New_Atom_Entry_Source
     (Base_URI : in String := None;
      Language : in String := None)
      return Atom_Entry_Source
   is
   begin

      return Source          : Atom_Entry_Source := Null_Atom_Entry_Source do
         if Base_URI /= None then
            Source.Common.Base_URI := TUS (Base_URI);
         end if;

         if Language /= None then
            Source.Common.Language := TUS (Language);
         end if;
      end return;

   end New_Atom_Entry_Source;

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

   ---------------------------
   --  Protected_Atom_Feed  --
   ---------------------------

   protected body Protected_Atom_Feed is

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
        (Value       : in Yolk.Syndication.Atom_Entry;
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

         C       : Cursor;
         Counter : Natural := Natural (Entries.Length);
         Now     : constant Time := Clock;

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

         use DOM.Core;
         use Yolk.Syndication.DOM_Builder;

         Doc       : Document;
         Impl      : DOM_Implementation;

      begin

         Doc := Create_Document (Implementation => Impl);

         Create_Feed_Element (Authors      => Authors,
                              Categories   => Categories,
                              Common       => Common,
                              Contributors => Contributors,
                              Doc          => Doc,
                              Entries      => Entries,
                              Generator    => Generator,
                              Icon         => Icon,
                              Id           => Id,
                              Links        => Links,
                              Logo         => Logo,
                              Rights       => Rights,
                              Subtitle     => Subtitle,
                              Title        => Title,
                              Updated      => Updated);

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

   end Protected_Atom_Feed;

end Yolk.Syndication;
