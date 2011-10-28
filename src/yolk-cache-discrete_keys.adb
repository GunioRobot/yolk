-------------------------------------------------------------------------------
--                                                                           --
--                                  Yolk                                     --
--                                                                           --
--                         Yolk.Cache.Discrete_Keys                          --
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

package body Yolk.Cache.Discrete_Keys is

   type Element_Container is
      record
         Added_Timestamp   : Ada.Calendar.Time;
         Element           : Element_Type;
         Has_Element       : Boolean := False;
      end record;

   type Element_Array_Type is array (Key_Type) of Element_Container;

   Null_Container : Element_Container;
   pragma Unmodified (Null_Container);

   protected P_Element_List is
      procedure Clear;
      --  ????

      procedure Clear
        (Key : in Key_Type);
      --  ????

      function Is_Valid
        (Key : in Key_Type)
         return Boolean;
      --  ????

      procedure Read
        (Key   : in  Key_Type;
         Valid : out Boolean;
         Value : out Element_Type);
      --  ????

      procedure Write
        (Key   : in Key_Type;
         Value : in Element_Type);
      --  ????
   private
      Element_List   : Element_Array_Type := (others => Null_Container);
   end P_Element_List;

   ----------------------
   --  P_Element_List  --
   ----------------------

   protected body P_Element_List is
      -------------
      --  Clear  --
      -------------

      procedure Clear
      is
      begin
         Element_List := (others => Null_Container);
      end Clear;

      -------------
      --  Clear  --
      -------------

      procedure Clear
        (Key : in Key_Type)
      is
      begin
         Element_List (Key) := Null_Container;
      end Clear;

      ----------------
      --  Is_Valid  -   -
      ----------------

      function Is_Valid
        (Key : in Key_Type)
         return Boolean
      is
         use Ada.Calendar;
      begin
         return (Element_List (Key).Has_Element) and then
           (Clock - Element_List (Key).Added_Timestamp < Max_Element_Age);
      end Is_Valid;

      ------------
      --  Read  --
      ------------

      procedure Read
        (Key   : in  Key_Type;
         Valid : out Boolean;
         Value : out Element_Type)
      is
         use Ada.Calendar;
      begin
         Valid := Is_Valid (Key => Key);
         Value := Element_List (Key).Element;
      end Read;

      -------------
      --  Write  --
      -------------

      procedure Write
        (Key   : in Key_Type;
         Value : in Element_Type)
      is
      begin
         Element_List (Key) := (Added_Timestamp => Ada.Calendar.Clock,
                                Element         => Value,
                                Has_Element     => True);
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

   -------------
   --  Clear  --
   -------------

   procedure Clear
     (Key : in Key_Type)
   is
   begin
      P_Element_List.Clear (Key => Key);
   end Clear;

   ----------------
   --  Is_Valid  --
   ----------------

   function Is_Valid
     (Key : in Key_Type)
      return Boolean
   is
   begin
      return P_Element_List.Is_Valid (Key => Key);
   end Is_Valid;

   ------------
   --  Read  --
   ------------

   procedure Read
     (Key      : in  Key_Type;
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
     (Key   : in Key_Type;
      Value : in Element_Type)
   is
   begin
      P_Element_List.Write (Key   => Key,
                            Value => Value);
   end Write;

end Yolk.Cache.Discrete_Keys;
