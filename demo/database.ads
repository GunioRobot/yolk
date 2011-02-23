--  Created by gnatcoll_db2ada
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
