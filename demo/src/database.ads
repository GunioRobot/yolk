-------------------------------------------------------------------------------
--                                                                           --
--                                  Yolk                                     --
--                                                                           --
--                                Database                                   --
--                                                                           --
--                                  SPEC                                     --
--                                                                           --
--                   Copyright (C) 2010-2012, Thomas Løcke                   --
--                                                                           --
--  This is free software;  you can redistribute it and/or modify it         --
--  under terms of the  GNU General Public License  as published by the      --
--  Free Software  Foundation;  either version 3,  or (at your  option) any  --
--  later version. This library is distributed in the hope that it will be   --
--  useful, but WITHOUT ANY WARRANTY;  without even the implied warranty of  --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     --
--  You should have received a copy of the GNU General Public License and    --
--  a copy of the GCC Runtime Library Exception along with this program;     --
--  see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
--  <http://www.gnu.org/licenses/>.                                          --
--                                                                           --
-------------------------------------------------------------------------------

with GNATCOLL.SQL; use GNATCOLL.SQL;
with Database_Names; use Database_Names;
package Database is
   pragma Style_Checks (Off);
   pragma Elaborate_Body;

   type T_Abstract_Tmp (Instance : Cst_String_Access; Index : Integer)
      is abstract new SQL_Table (Ta_Tmp, Instance, Index) with
   record
      Id : SQL_Field_Integer (Ta_Tmp, Instance, N_Id, Index);
      Name : SQL_Field_Text (Ta_Tmp, Instance, N_Name, Index);
   end record;

   type T_Tmp (Instance : Cst_String_Access)
      is new T_Abstract_Tmp (Instance, -1) with null record;
   type T_Numbered_Tmp (Index : Integer)
      is new T_Abstract_Tmp (null, Index) with null record;


   Tmp : T_Tmp (null);
end Database;
