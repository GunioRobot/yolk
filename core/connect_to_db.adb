-------------------------------------------------------------------------------
--                                                                           --
--                                  Yolk                                     --
--                                                                           --
--                          Database_Connection                              --
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

with Ada.Strings.Hash;
with GNATCOLL.SQL.Postgres;

package body Connect_To_DB is

   -----------------------------------
   --  Database_Connection_Factory  --
   -----------------------------------

   function Database_Connection_Factory
     (Desc : GNATCOLL.SQL.Exec.Database_Description)
      return GNATCOLL.SQL.Exec.Database_Connection
   is

      use GNATCOLL.SQL.Exec;
      use GNATCOLL.SQL.Postgres;

      DBMS : constant String := Get_DBMS (Desc);

   begin

      if DBMS = DBMS_Postgresql then
         return Build_Postgres_Connection (Desc);
      else
         return null;
      end if;

   end Database_Connection_Factory;

   ---------------
   --  DB_Conn  --
   ---------------

   task body DB_Conn
   is

      use GNATCOLL.SQL.Exec;

   begin

      loop
         select
            accept Fetch (Conn : out Database_Connection;
                          Desc : in Database_Description) do

               Conn := Get_Task_Connection
                 (Description  => Desc,
                  Factory      => Database_Connection_Factory'Access);

            end Fetch;
         or
            terminate;
            --  Terminate the task if the unit we depend on have reached its
            --  end, ie. the server is being shut down.
         end select;
      end loop;

   end DB_Conn;

   ------------------------
   --  Equivalent_Tasks  --
   ------------------------

   function Equivalent_Tasks (Left, Right : in Ada.Task_Identification.Task_Id)
                              return Boolean
   is

      use type Ada.Task_Identification.Task_Id;

   begin

      return Left = Right;

   end Equivalent_Tasks;

   ---------------------------------
   --  Protected_Association_Map  --
   ---------------------------------

   protected body Protected_Association_Map is

      -----------
      --  Get  --
      -----------

      function Get
        (AWS_Task_ID : in Ada.Task_Identification.Task_Id)
         return DB_Conn_Access
      is
      begin

         if Task_Store.Contains (Key => AWS_Task_ID) then
            return Task_Store.Element (AWS_Task_ID);
         else
            return Null_DB_Conn_Access;
         end if;

      end Get;

      -----------
      --  Set  --
      -----------

      procedure Set
        (DB_Task     : out DB_Conn_Access;
         AWS_Task_ID : in Ada.Task_Identification.Task_Id)
      is
      begin

         DB_Task := new DB_Conn;
         Task_Store.Insert (Key      => AWS_Task_ID,
                            New_Item => DB_Task);
         --  Start a new DB_Conn task and insert access to it into the
         --  Task_Store map.

      end Set;

   end Protected_Association_Map;

   -----------------------
   --  Set_Credentials  --
   -----------------------

   function Set_Credentials
     (Host          : in String;
      Database      : in String;
      User          : in String;
      Password      : in String;
      Server_Config : in AWS.Config.Object)
      return Credentials
   is

      C : Credentials (Host_Length     => Host'Length,
                       Database_Length => Database'Length,
                       User_Length     => User'Length,
                       Password_Length => Password'Length);

   begin

      C.Host       := Host;
      C.Database   := Database;
      C.User       := User;
      C.Password   := Password;
      C.Threads    := AWS.Config.Max_Connection (Server_Config);

      return C;

   end Set_Credentials;

   --------------------
   --  Task_ID_Hash  --
   --------------------

   function Task_ID_Hash
     (ID : in Ada.Task_Identification.Task_Id)
      return Ada.Containers.Hash_Type
   is

      use Ada.Strings;
      use Ada.Task_Identification;

   begin

      return Hash (Key => Image (ID));

   end Task_ID_Hash;

end Connect_To_DB;
