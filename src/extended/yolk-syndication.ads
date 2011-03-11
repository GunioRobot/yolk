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
with Ada.Strings.Unbounded;

package Yolk.Syndication is

   type Atom is private;
   --  A feed object.

   function Initialize
     (Id      : in String;
      Title   : in String)
      return Atom;
   --  Initialize an Atom object with the required data, as per the Atom
   --  specification RFC4287:
   --    http://tools.ietf.org/html/rfc4287

   procedure Set_Id
     (Feed : in out Atom;
      Id   : in     String);
   --  Set the child id element of the Atom top-level feed element.

   procedure Set_Title
     (Feed  : in out Atom;
      Title : in     String);
   --  Set the child title element of the Atom top-level feed element.

   procedure Set_Updated
     (Feed    : in out Atom;
      Updated : in     Ada.Calendar.Time);
   --  Set the child updated element of the Atom top-level feed element. It is
   --  generally not necessary to call this manually, as it happens automatic-
   --  ally whenever an entry element is added/delete/edited.

private

   use Ada.Strings.Unbounded;

   type Atom is
      record
         Id       : Unbounded_String;
         Title    : Unbounded_String;
         Updated  : Ada.Calendar.Time;
      end record;

end Yolk.Syndication;
