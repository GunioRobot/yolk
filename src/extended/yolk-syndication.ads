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
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Unbounded;

package Yolk.Syndication is

   type Atom_Feed is private;
   --  An Atom feed object.

   type Content_Type is (Text, Html, Xhtml);
   --  This type is common for a lot of feed XML elements. It identifies the
   --  kind of data found in the element.

   None : constant String := "";

   function Initialize
     (Base_URI    : in String := None;
      Id          : in String;
      Language    : in String := None;
      Title       : in String;
      Title_Type  : in Content_Type := Text)
      return Atom_Feed;
   --  Initialize an Atom object with the required data, as per the Atom
   --  specification RFC4287:
   --    http://tools.ietf.org/html/rfc4287

   procedure Add_Author
     (Feed     : in out Atom_Feed;
      Name     : in     String;
      Language : in     String := None;
      Email    : in     String := None;
      URI      : in     String := None);
   --  Add an author child element to the Atom top-level feed element.

   procedure Add_Contributor
     (Feed     : in out Atom_Feed;
      Name     : in     String;
      Language : in     String := None;
      Email    : in     String := None;
      URI      : in     String := None);
   --  Add a contributor child element to the Atom top-level feed element.

   procedure Set_Id
     (Feed : in out Atom_Feed;
      Id   : in     String);
   --  Set the child id element of the Atom top-level feed element.

   procedure Set_Title
     (Feed       : in out Atom_Feed;
      Title      : in     String;
      Title_Type : in     Content_Type := Text);
   --  Set the child title element of the Atom top-level feed element.

   procedure Set_Updated
     (Feed    : in out Atom_Feed;
      Updated : in     Ada.Calendar.Time);
   --  Set the child updated element of the Atom top-level feed element. It is
   --  generally not necessary to call this manually, as it happens automatic-
   --  ally whenever an entry element is added/delete/edited.

private

   use Ada.Containers;
   use Ada.Strings.Unbounded;

   type Atom_Person is
      record
         Name     : Unbounded_String;
         Language : Unbounded_String := Null_Unbounded_String;
         Email    : Unbounded_String := Null_Unbounded_String;
         URI      : Unbounded_String := Null_Unbounded_String;
      end record;

   package Person_List is new Doubly_Linked_Lists (Atom_Person);

   type Atom_Feed is
      record
         Base_URI         : Unbounded_String := Null_Unbounded_String;
         Id               : Unbounded_String;
         Language         : Unbounded_String := Null_Unbounded_String;
         Title            : Unbounded_String;
         Title_Type       : Content_Type;
         Updated          : Ada.Calendar.Time;
         Author_List      : Person_List.List;
         Contributor_List : Person_List.List;
      end record;

end Yolk.Syndication;
