-------------------------------------------------------------------------------
--                                                                           --
--                                  Yolk                                     --
--                                                                           --
--                           Database_Connection                             --
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

private with Ada.Containers.Hashed_Maps;
private with Ada.Task_Identification;
with AWS.Config;
with GNATCOLL.SQL.Exec;

package Connect_To_DB is

   type Credentials (Host_Length     : Positive;
                     Database_Length : Positive;
                     User_Length     : Positive;
                     Password_Length : Positive) is private;

   function Set_Credentials (Host          : in String;
                             Database      : in String;
                             User          : in String;
                             Password      : in String;
                             Server_Config : in AWS.Config.Object)
                             return Credentials;
   --  Define the credentials necessary to connect to the database. The actual
   --  DBMS used is decided when the relevant generic child package is
   --  instantiated, for example like this:
   --
   --    package DB is new Connect_To_DB.PostgreSQL
   --      (Connect_To_DB.Set_Credentials
   --       (Host          => "some-host",
   --        Database      => "some-database",
   --        User          => "some-user",
   --        Password      => "some-password",
   --        Server_Config => AWS.Config.Get_Current));

private

   task type DB_Conn is
      entry Fetch (Conn : out GNATCOLL.SQL.Exec.Database_Connection;
                   Desc : in GNATCOLL.SQL.Exec.Database_Description);
   end DB_Conn;
   --  Here we fetch a thread specific database connection. If a connection
   --  has not yet been made for the calling thread, a new one is established
   --  using the Database_Connection_Factory function.

   type DB_Conn_Access is access DB_Conn;
   Null_DB_Conn_Access : DB_Conn_Access := null;
   --  Access to the DB_Conn tasks.

   type Credentials (Host_Length     : Positive;
                     Database_Length : Positive;
                     User_Length     : Positive;
                     Password_Length : Positive) is
      record
         Host     : String (1 .. Host_Length);
         Database : String (1 .. Database_Length);
         User     : String (1 .. User_Length);
         Password : String (1 .. Password_Length);
         Threads  : Positive;
      end record;

   function Database_Connection_Factory
     (Desc : GNATCOLL.SQL.Exec.Database_Description)
      return GNATCOLL.SQL.Exec.Database_Connection;
   --  Return a GNATCOLL.SQL.Exec.Database_Connection object. This function
   --  is called whenever a task is missing a dedicated database connection.

   function Equivalent_Tasks (Left, Right : in Ada.Task_Identification.Task_Id)
                              return Boolean;
   --  Equivalence function used by the Task_Assocition_Map hashed map.

   function Task_ID_Hash (ID : in Ada.Task_Identification.Task_Id)
                          return Ada.Containers.Hash_Type;
   --  Hash function used by the Task_Association_Map hashed map.

   package Task_Association_Map is new Ada.Containers.Hashed_Maps
     (Key_Type        => Ada.Task_Identification.Task_Id,
      Element_Type    => DB_Conn_Access,
      Hash            => Task_ID_Hash,
      Equivalent_Keys => Equivalent_Tasks);
   --  This map is the binding between the AWS tasks and the database tasks.
   --  We need this to ensure that there's a correlation between AWS task A and
   --  database connection A.

   protected type Protected_Association_Map is

      function Get (AWS_Task_ID : in Ada.Task_Identification.Task_Id)
                    return DB_Conn_Access;
      --  Return the DB_Conn_Access object that matches the AWS_Task_ID.

      procedure Set (DB_Task     : out DB_Conn_Access;
                     AWS_Task_ID : in Ada.Task_Identification.Task_Id);
      --  Add a new AWS_Task_ID to the Task_Association_Map. This also entails
      --  starting a new DB_Conn task, and adding access to this to Task_Store.

   private

      Task_Store : Task_Association_Map.Map;
      --  Here we keep all the AWS task -> DB_Conn_Access relations.

   end Protected_Association_Map;
   --  Because Ada.Containers.Hashed_Maps are not thread-safe, we need to wrap
   --  our map in a protected object. There shouldn't be any noticeable
   --  performance penalty because of this. Multiple readers are allowed to
   --  use the Get function, and the object is only locked when Set is called,
   --  which only happens as many times as there are AWS tasks.

end Connect_To_DB;
