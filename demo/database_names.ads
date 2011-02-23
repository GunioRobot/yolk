--  Created by gnatcoll_db2ada
with GNATCOLL.SQL; use GNATCOLL.SQL;
package Database_Names is
   pragma Style_Checks (Off);
   TC_Tmp : aliased constant String := "tmp";
   Ta_Tmp : constant Cst_String_Access := TC_Tmp'Access;

   NC_Id : aliased constant String := "id";
   N_Id : constant Cst_String_Access := NC_id'Access;
   NC_Name : aliased constant String := "name";
   N_Name : constant Cst_String_Access := NC_name'Access;
end Database_Names;
