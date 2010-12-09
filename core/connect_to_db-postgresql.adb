-------------------------------------------------------------------------------
--                                                                           --
--                                  Yolk                                     --
--                                                                           --
--                        DBMS_Connection.PostgreSQL                         --
--                                                                           --
--                                  BODY                                     --
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

with Ada.Strings.Hash;
with GNATCOLL.SQL.Postgres;

package body Connect_To_DB.PostgreSQL is

   ---------------
   --  DB_Conn  --
   ---------------

   task body DB_Conn
   is

      use GNATCOLL.SQL.Exec;

   begin
      loop
         select
            accept Fetch (Conn : out Database_Connection) do

               Conn := Get_Task_Connection
                 (Description  => DB_Description,
                  Factory      => Database_Connection_Factory'Access);

            end Fetch;
         or
            terminate;
            --  Terminate the task if the unit we depend on have reached its
            --  end, ie. the server is being shut down.
         end select;
      end loop;

   end DB_Conn;

   ------------------
   --  Connection  --
   ------------------

   function Connection return GNATCOLL.SQL.Exec.Database_Connection
   is

      use Ada.Task_Identification;
      use GNATCOLL.SQL.Exec;

      A_Connection   : Database_Connection;
      A_DB_Task      : DB_Conn_Access;

   begin

      A_DB_Task := Association.Get (AWS_Task_ID => Current_Task);

      if A_DB_Task = Null_DB_Conn_Access then
         Association.Set (DB_Task     => A_DB_Task,
                          AWS_Task_ID => Current_Task);
      end if;

      A_DB_Task.Fetch (Conn => A_Connection);

      return A_Connection;

   end Connection;

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

   ------------------
   --  Initialize  --
   ------------------

   procedure Initialize
   is

      use Ada.Strings.Unbounded;
      use GNATCOLL.SQL.Exec;

   begin

      Setup_Database (Description => DB_Description,
                      Database    => To_String (These_Credentials.Database),
                      User        => To_String (These_Credentials.User),
                      Host        => To_String (These_Credentials.Host),
                      Password    => To_String (These_Credentials.Password),
                      DBMS        => DBMS_Postgresql);
      --  Populate the DB_Description variable.

   end Initialize;

   --------------------
   --  Task_ID_Hash  --
   --------------------

   function Task_ID_Hash (ID : in Ada.Task_Identification.Task_Id)
                          return Ada.Containers.Hash_Type
   is

      use Ada.Strings;
      use Ada.Task_Identification;

   begin

      return Hash (Key => Image (ID));

   end Task_ID_Hash;

   ------------------
   --  Task_Assoc  --
   ------------------

   protected body Association is

      -----------
      --  Get  --
      -----------

      function Get (AWS_Task_ID : in Ada.Task_Identification.Task_Id)
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

      procedure Set (DB_Task     : out DB_Conn_Access;
                     AWS_Task_ID : in Ada.Task_Identification.Task_Id)
      is

         --  New_DB_Conn_Access : DB_Conn_Access;

      begin

         DB_Task := new DB_Conn;
         Task_Store.Insert (Key      => AWS_Task_ID,
                            New_Item => DB_Task);

      end Set;

   end Association;

begin

   Initialize;

end Connect_To_DB.PostgreSQL;
