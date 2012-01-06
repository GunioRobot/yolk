-------------------------------------------------------------------------------
--                                                                           --
--                                  Yolk                                     --
--                                                                           --
--                         Yolk.Cache.Discrete_Keys                          --
--                                                                           --
--                                  BODY                                     --
--                                                                           --
--                   Copyright (C) 2010-2012, Thomas Løcke                   --
--                                                                           --
--  This library is free software;  you can redistribute it and/or modify    --
--  it under terms of the  GNU General Public License  as published by the   --
--  Free Software  Foundation;  either version 3,  or (at your  option) any  --
--  later version. This library is distributed in the hope that it will be   --
--  useful, but WITHOUT ANY WARRANTY;  without even the implied warranty of  --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     --
--                                                                           --
--  As a special exception under Section 7 of GPL version 3, you are         --
--  granted additional permissions described in the GCC Runtime Library      --
--  Exception, version 3.1, as published by the Free Software Foundation.    --
--                                                                           --
--  You should have received a copy of the GNU General Public License and    --
--  a copy of the GCC Runtime Library Exception along with this program;     --
--  see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
--  <http://www.gnu.org/licenses/>.                                          --
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
