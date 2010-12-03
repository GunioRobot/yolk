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

with Ada.Task_Identification;
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
            accept Fetch (Conn : out Database_Connection) do

               Put_Line (Ada.Task_Identification.Image
                         (DB_Conn.Fetch'Caller));

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

      A_Connection : Database_Connection;

   begin

      Task_List (Counter).Fetch (A_Connection);

      if Counter < These_Credentials.Threads then
         Counter := Counter + 1;
      else
         Counter := 1;
      end if;

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

begin

   Initialize;

end Connect_To_DB.PostgreSQL;
