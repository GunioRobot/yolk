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
--  sion.  Yolk is distributed in the hope that it will be useful, but WITH- --
--  OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
--  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
--  for  more details.  You should have  received  a copy of the GNU General --
--  Public License  distributed with Yolk.  If not, write  to  the  Free     --
--  Software Foundation,  51  Franklin  Street,  Fifth  Floor, Boston,       --
--  MA 02110 - 1301, USA.                                                    --
--                                                                           --
-------------------------------------------------------------------------------

private with Ada.Containers.Hashed_Maps;
private with Ada.Task_Identification;
with GNATCOLL.SQL.Exec;

generic

   These_Credentials : Credentials;

package Connect_To_DB.PostgreSQL is

   function Connection return GNATCOLL.SQL.Exec.Database_Connection;
   --  Return a thread specific access to the database.

private

   DB_Description : GNATCOLL.SQL.Exec.Database_Description;
   --  Describes access to the database, ie. user, host, password and such.

   task type DB_Conn is
      entry Fetch (Conn : out GNATCOLL.SQL.Exec.Database_Connection);
   end DB_Conn;
   --  Here we fetch a thread specific database connection. If a connection
   --  has not yet been made for the calling thread, a new one is established
   --  using the Database_Connection_Factory function.

   type DB_Conn_Access is access DB_Conn;
   Null_DB_Conn_Access : DB_Conn_Access := null;
   --  Access to the DB_Conn tasks.

   function Database_Connection_Factory
     (Desc : GNATCOLL.SQL.Exec.Database_Description)
      return GNATCOLL.SQL.Exec.Database_Connection;
   --  Return a GNATCOLL.SQL.Exec.Database_Connection object. This function
   --  is called whenever a task is missing a dedicated database connection.

   function Equivalent_Tasks (Left, Right : in Ada.Task_Identification.Task_Id)
                              return Boolean;
   --  Equivalence function used by the Task_Assocition_Map.

   procedure Initialize;
   --  Is called when this generic is instantiated. It populates the
   --  DB_Description variable.

   function Task_ID_Hash (ID : in Ada.Task_Identification.Task_Id)
                          return Ada.Containers.Hash_Type;
   --  Hash function used by the Task_Association_Map.

   package Task_Association_Map is new Ada.Containers.Hashed_Maps
     (Key_Type        => Ada.Task_Identification.Task_Id,
      Element_Type    => DB_Conn_Access,
      Hash            => Task_ID_Hash,
      Equivalent_Keys => Equivalent_Tasks);
   --  This map is the binding between the AWS tasks and the tasks started
   --  by this generic. We need this to ensure that there's a correlation
   --  between AWS task A and database connection A. We don't want AWS task A
   --  to connect to database connection B, so when an AWS task call the
   --  Connection function for the first time, an association is established
   --  between the AWS task (Key_Type) and the resulting database connection
   --  (Element_Type).

   protected Association is

      function Get (AWS_Task_ID : in Ada.Task_Identification.Task_Id)
                    return DB_Conn_Access;
      --  Return an the DB_Conn_Access object that matches the AWS_Task_ID,
      --  according to the Task_Association_Map.

      procedure Set (DB_Task     : out DB_Conn_Access;
                     AWS_Task_ID : in Ada.Task_Identification.Task_Id);
      --  Add a new AWS_Task_ID to Task_Association_Map. This also entails
      --  creating a new DB_Conn object, and adding access to this object to
      --  the Task_Association_Map.

   private

      Task_Store : Task_Association_Map.Map;
      --  Here we keep all the AWS task -> DB_Conn_Access relations.

   end Association;

end Connect_To_DB.PostgreSQL;
