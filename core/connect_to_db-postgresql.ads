-------------------------------------------------------------------------------
--                                                                           --
--                                  Yolk                                     --
--                                                                           --
--                        DBMS_Connection.PostgreSQL                         --
--                                                                           --
--                                  SPEC                                     --
--                                                                           --
--                     Copyright (C) 2010, Thomas Løcke                      --
--                                                                           --
--  Yolk is free software;  you can  redistribute it  and/or modify it under --
--  terms of the  GNU General Public License as published  by the Free Soft- --
--  ware  Foundation;  either version 2,  or (at your option) any later ver- --
--  sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
--  OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
--  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
--  for  more details.  You should have  received  a copy of the GNU General --
--  Public License  distributed with Yolk.  If not, write  to  the  Free     --
--  Software Foundation,  51  Franklin  Street,  Fifth  Floor, Boston,       --
--  MA 02110 - 1301, USA.                                                    --
--                                                                           --
-------------------------------------------------------------------------------

with Ada.Containers.Hashed_Maps;
with Ada.Task_Identification;
with GNATCOLL.SQL.Exec; use GNATCOLL.SQL.Exec;

generic

   These_Credentials : Credentials;

package Connect_To_DB.PostgreSQL is

   function Connection return Database_Connection;
   --  Return a thread specific access to the database.

private

   use Ada.Task_Identification;

   DB_Description : Database_Description;
   --  Describes access to the database, ie. user, host, password and such.

   task type DB_Conn is
      entry Fetch (Conn : out Database_Connection);
   end DB_Conn;
   --  TODO: Write comment.

   type DB_Conn_Access is access DB_Conn;
   Null_DB_Conn_Access : DB_Conn_Access := null;
   --  type Task_Array is array (1 .. These_Credentials.Threads)
   --  of DB_Conn_Access;
   --  Task_List : Task_Array;

   function Database_Connection_Factory
     (Desc : Database_Description) return Database_Connection;
   --  Return a GNATCOLL.SQL.Exec.Database_Connection object. This function
   --  is called whenever a task is missing a dedicated database connection, so
   --  when a call to Get_DB_Connection is made, the
   --  GNATCOLL.SQL.Exec.Get_Task_Connection check if the current task have an
   --  active database connection, and if not it calls this factory function.

   function Equivalent_Tasks (Left, Right : in Task_Id) return Boolean;

   procedure Initialize;
   --  Wrapper for the GNATCOLL.SQL.Postgres.Setup_Database procedure.

   function Task_ID_Hash (ID : in Task_Id) return Ada.Containers.Hash_Type;

   package Task_Association_Map is new Ada.Containers.Hashed_Maps
     (Key_Type        => Ada.Task_Identification.Task_Id,
      Element_Type    => DB_Conn_Access,
      Hash            => Task_ID_Hash,
      Equivalent_Keys => Equivalent_Tasks);

   protected Task_Assoc is

      function Get (AWS_Task_ID : in Task_Id) return DB_Conn_Access;
      procedure Set (AWS_Task_ID : in Task_Id);

   private

      Task_Store : Task_Association_Map.Map;

   end Task_Assoc;

end Connect_To_DB.PostgreSQL;
