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

with Ada.Calendar.Formatting;
with Ada.Text_IO;
with Yolk.Utilities;

package body Yolk.Syndication is

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

   ------------------
   --  Initialize  --
   ------------------

   function Initialize
     (Id      : in String;
      Title   : in String)
      return Atom
   is

      use Ada.Calendar;
      use Ada.Text_IO;
      use Yolk.Utilities;

   begin

      Put_Line (Id);
      Put_Line (Title);
      Put_Line (Atom_Date_Image (Clock));

      return Atom'(Id => TUS (Id),
                   Title => TUS (Title),
                   Updated => Clock);

   end Initialize;

   --------------
   --  Set_Id  --
   --------------

   procedure Set_Id
     (Feed  : in out Atom;
      Id    : in     String)
   is

      use Yolk.Utilities;

   begin

      Feed.Id := TUS (Id);

   end Set_Id;

   -----------------
   --  Set_Title  --
   -----------------

   procedure Set_Title
     (Feed  : in out Atom;
      Title : in     String)
   is

      use Yolk.Utilities;

   begin

      Feed.Title := TUS (Title);

   end Set_Title;

   -------------------
   --  Set_Updated  --
   -------------------

   procedure Set_Updated
     (Feed    : in out Atom;
      Updated : in     Ada.Calendar.Time)
   is
   begin

      Feed.Updated := Updated;

   end Set_Updated;

end Yolk.Syndication;
