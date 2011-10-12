-------------------------------------------------------------------------------
--                                                                           --
--                                  Yolk                                     --
--                                                                           --
--                            Yolk.Connect_To_DB                             --
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

with GNATCOLL.SQL.Exec;

package Yolk.Connect_To_DB is

   type Connection_Mapping_Method is (AWS_Tasks_To_DB,
                                      DB_Conn_Tasks_To_DB);
   --  Specify how we connect to the database. This can be done in one of two
   --  ways:
   --
   --  AWS_Tasks_To_DB:
   --    Each AWS task is mapped to its own unique database connection. This
   --    means that you can only connect to _one_ database per AWS task. This
   --    is the fastest method, as we can call
   --    GNATCOLL.SQL.Exec.Get_Task_Connection immediately.
   --
   --  DB_Conn_Tasks_To_DB:
   --    A pool of DB_Conn tasks are created for each instantiation of a
   --    Connect_To_DB child package and these then connect to the database.
   --    The result is that your app can connect to several different databases
   --    in the same AWS task.
   --    This is slower than AWS_Tasks_To_DB because we have to maintain a map
   --    between the AWS tasks and the DB_Conn tasks. This is done using the
   --    Ada.Task_Attributes package.
   --
   --  It is perfectly valid to instantiate all Connect_To_DB child packages
   --  with DB_Conn_Tasks_To_DB, but it is only possible to instantiate once
   --  with AWS_Tasks_To_DB.

   type Credentials
     (Host_Length     : Positive;
      Database_Length : Positive;
      User_Length     : Positive;
      Password_Length : Positive) is private;

   function Set_Credentials
     (Host              : in String;
      Database          : in String;
      User              : in String;
      Password          : in String)
      return Credentials;
   --  Define the credentials necessary to connect to the database. The actual
   --  DBMS used is decided when the relevant generic child package is
   --  instantiated, for example like this:
   --
   --  package My_DB is new Yolk.Connect_To_DB.PostgreSQL
   --    (DB_Credentials            => Yolk.Connect_To_DB.Set_Credentials
   --       (Host              => "host",
   --        Database          => "database",
   --        User              => "user",
   --        Password          => "password"),
   --     Task_To_DB_Mapping_Method => Yolk.Connect_To_DB.AWS_Tasks_To_DB);

private

   AWS_Tasks_To_DB_Already_Used : Boolean := False;
   --  This is set to True when the Connection_Mapping_Method AWS_Tasks_To_DB
   --  is used the first time in an instantiation of a database generic.
   --  A Program_Error is raised if an instantation is done with
   --  AWS_Tasks_To_DB more than once.

   task type DB_Conn is
      entry Fetch
        (Conn : out GNATCOLL.SQL.Exec.Database_Connection;
         Desc : in GNATCOLL.SQL.Exec.Database_Description);
   end DB_Conn;
   --  A DB_Conn task is started and mapped to a specific AWS task. It is then
   --  this specific DB_Conn task that is used to call the Get_Task_Connection
   --  function when a database connection is needed.

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
      end record;

   function Database_Connection_Factory
     (Desc : in GNATCOLL.SQL.Exec.Database_Description)
      return GNATCOLL.SQL.Exec.Database_Connection;
   --  Return a GNATCOLL.SQL.Exec.Database_Connection object. This function
   --  is called whenever a task is missing a dedicated database connection.

end Yolk.Connect_To_DB;
