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
--  sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
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

with Ada.Text_IO; use Ada.Text_IO;

package body Connect_To_DB.PostgreSQL is

   ---------------
   --  DB_Conn  --
   ---------------

   task body DB_Conn is
   begin
      loop
         select
            accept Fetch (Conn   : out Database_Connection) do

               Conn := Get_Task_Connection
                 (Description  => DB_Description,
                  Factory      => Database_Connection_Factory'Access);

            end Fetch;
         or
            terminate;
         end select;
      end loop;

   end DB_Conn;

   ------------------
   --  Connection  --
   ------------------

   function Connection return Database_Connection
   is

      A_Connection   : Database_Connection;
      A_Task         : DB_Conn_Access;

   begin

      A_Task := Task_Assoc.Get (AWS_Task_ID => Current_Task);
      if A_Task = Null_DB_Conn_Access then
         Task_Assoc.Set (AWS_Task_ID => Current_Task);
         A_Task := Task_Assoc.Get (AWS_Task_ID => Current_Task);
      end if;

      A_Task.Fetch (Conn => A_Connection);

      return A_Connection;

   end Connection;

   -----------------------------------
   --  Database_Connection_Factory  --
   -----------------------------------

   function Database_Connection_Factory
     (Desc : Database_Description) return Database_Connection
   is

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

   function Equivalent_Tasks (Left, Right : in Task_Id) return Boolean
   is
   begin

      return Left = Right;

   end Equivalent_Tasks;

   ------------------
   --  Initialize  --
   ------------------

   procedure Initialize
   is
   begin

      Setup_Database (Description => DB_Description,
                      Database    => To_String (These_Credentials.Database),
                      User        => To_String (These_Credentials.User),
                      Host        => To_String (These_Credentials.Host),
                      Password    => To_String (These_Credentials.Password),
                      DBMS        => DBMS_Postgresql);

   end Initialize;

   --------------------
   --  Task_ID_Hash  --
   --------------------

   function Task_ID_Hash (ID : in Task_Id) return Ada.Containers.Hash_Type
   is

      use Ada.Strings;

   begin

      return Hash (Key => Image (ID));

   end Task_ID_Hash;

   ------------------
   --  Task_Assoc  --
   ------------------

   protected body Task_Assoc is

      -----------
      --  Get  --
      -----------

      function Get (AWS_Task_ID : in Task_Id) return DB_Conn_Access
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

      procedure Set (AWS_Task_ID : in Task_Id)
      is

         New_DB_Conn_Access : DB_Conn_Access;

      begin

         Put_Line ("Set: " & Ada.Task_Identification.Image (AWS_Task_ID));
         New_DB_Conn_Access := new DB_Conn;
         Task_Store.Insert (Key      => AWS_Task_ID,
                            New_Item => New_DB_Conn_Access);

      end Set;

   end Task_Assoc;

begin

   Initialize;

end Connect_To_DB.PostgreSQL;
