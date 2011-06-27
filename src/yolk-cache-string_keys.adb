-------------------------------------------------------------------------------
--                                                                           --
--                                  Yolk                                     --
--                                                                           --
--                          Yolk.Cache.String_Keys                           --
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

with Ada.Calendar;
with Ada.Containers.Hashed_Maps;
with Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;
with Yolk.Utilities;

package body Yolk.Cache.String_Keys is

   use Ada.Containers;
   use Ada.Strings.Unbounded;

   type Element_Container is
      record
         Added_Timestamp   : Ada.Calendar.Time;
         Element           : Element_Type;
      end record;

   Null_Container : Element_Container;
   pragma Unmodified (Null_Container);

   function Equivalent_Keys
     (Left  : in Unbounded_String;
      Right : in Unbounded_String)
      return Boolean;
   --  Used by the Element_Map to determine equivalence between values.

   function Key_Hash
     (Key : in Unbounded_String)
      return Hash_Type;
   --  Used by Element_Map to hash keys.

   package Element_Map is new Hashed_Maps
     (Key_Type        => Unbounded_String,
      Element_Type    => Element_Container,
      Hash            => Key_Hash,
      Equivalent_Keys => Equivalent_Keys);

   protected P_Element_List is

      procedure Cleanup;

      procedure Clear;

      procedure Invalidate
        (Key : in String);

      function Is_Valid
        (Key : in String)
         return Boolean;

      function Read
        (Key : in String)
         return Element_Type;

      procedure Read
        (Key   : in  String;
         Valid : out Boolean;
         Value : out Element_Type);

      procedure Write
        (Key   : in String;
         Value : in Element_Type);

   private

      Element_List   : Element_Map.Map;
      Virgin         : Boolean := True;
      Write_Hits     : Natural := 0;

   end P_Element_List;

   ----------------------
   --  P_Element_List  --
   ----------------------

   protected body P_Element_List is

      ---------------
      --  Cleanup  --
      ---------------

      procedure Cleanup
      is

         --  Cursor : Element_Map.Cursor := Element_List.First;

      begin

         null;

      end Cleanup;

      -------------
      --  Clear  --
      -------------

      procedure Clear
      is
      begin

         Element_List.Clear;

      end Clear;

      ------------------
      --  Invalidate  --
      ------------------

      procedure Invalidate
        (Key : in String)
      is

         use Yolk.Utilities;

      begin

         Element_List.Exclude (Key => TUS (Key));

      end Invalidate;

      ----------------
      --  Is_Valid  -   -
      ----------------

      function Is_Valid
        (Key : in String)
         return Boolean
      is

         use Ada.Calendar;
         use Yolk.Utilities;

      begin

         return (Element_List.Contains (Key => TUS (Key))) and then
           (Clock - Element_List.Element (Key => TUS (Key)).Added_Timestamp <
              Max_Element_Age);

      end Is_Valid;

      ------------
      --  Read  --
      ------------

      function Read
        (Key : in String)
         return Element_Type
      is

         use Yolk.Utilities;

      begin

         if Element_List.Contains (Key => TUS (Key)) then
            return Element_List.Element (Key => TUS (Key)).Element;
         else
            return Null_Container.Element;
         end if;

      end Read;

      ------------
      --  Read  --
      ------------

      procedure Read
        (Key   : in  String;
         Valid : out Boolean;
         Value : out Element_Type)
      is

         use Ada.Calendar;
         use Yolk.Utilities;

      begin

         Valid := Is_Valid (Key => Key);
         if Valid then
            Value := Element_List.Element (Key => TUS (Key)).Element;
         else
            Value := Null_Container.Element;
         end if;

      end Read;

      -------------
      --  Write  --
      -------------

      procedure Write
        (Key   : in String;
         Value : in Element_Type)
      is

         use Yolk.Utilities;

      begin

         Write_Hits := Write_Hits + 1;

         if Virgin then
            Element_List.Reserve_Capacity
              (Capacity => Count_Type (Reserved_Capacity));
            Virgin := False;
         end if;

         if Cleanup_On_Write and Write_Hits >= Cleanup_Interval then
            Write_Hits := 0;

            Cleanup;
         end if;

         Element_List.Include
           (Key      => TUS (Key),
            New_Item => (Added_Timestamp => Ada.Calendar.Clock,
                         Element         => Value));

      end Write;

   end P_Element_List;

   -------------
   --  Clear  --
   -------------

   procedure Clear
   is
   begin

      P_Element_List.Clear;

   end Clear;

   -----------------------
   --  Equivalent_Keys  --
   -----------------------

   function Equivalent_Keys
     (Left  : in Unbounded_String;
      Right : in Unbounded_String)
      return Boolean
   is
   begin

      return Left = Right;

   end Equivalent_Keys;

   ------------------
   --  Invalidate  --
   ------------------

   procedure Invalidate
     (Key : in String)
   is
   begin

      P_Element_List.Invalidate (Key => Key);

   end Invalidate;

   ----------------
   --  Is_Valid  --
   ----------------

   function Is_Valid
     (Key : in String)
      return Boolean
   is
   begin

      return P_Element_List.Is_Valid (Key => Key);

   end Is_Valid;

   ----------------
   --  Key_Hash  --
   ----------------

   function Key_Hash
     (Key : in Unbounded_String)
      return Hash_Type
   is
   begin

      return Ada.Strings.Unbounded.Hash (Key => Key);

   end Key_Hash;

   ------------
   --  Read  --
   ------------

   function Read
     (Key : in String)
      return Element_Type
   is
   begin

      return P_Element_List.Read (Key => Key);

   end Read;

   ------------
   --  Read  --
   ------------

   procedure Read
     (Key      : in  String;
      Is_Valid : out Boolean;
      Value    : out Element_Type)
   is
   begin

      P_Element_List.Read (Key   => Key,
                           Valid => Is_Valid,
                           Value => Value);

   end Read;

   -------------
   --  Write  --
   -------------

   procedure Write
     (Key   : in String;
      Value : in Element_Type)
   is
   begin

      P_Element_List.Write (Key   => Key,
                            Value => Value);

   end Write;

end Yolk.Cache.String_Keys;
