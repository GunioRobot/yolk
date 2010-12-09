-------------------------------------------------------------------------------
--                                                                           --
--                                  Yolk                                     --
--                                                                           --
--                              db_connection                                --
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

with GNATCOLL.SQL.Postgres;

package body DB_Connection is

   procedure Initialize;
   --  Initialize the DB_Connection package.

   function Database_Connection_Factory
     (Desc : Database_Description) return Database_Connection;
   --  Return a GNATCOLL.SQL.Exec.Database_Connection object. This function
   --  is called whenever a task is missing a dedicated database connection, so
   --  when a call to Get_DB_Connection is made, the
   --  GNATCOLL.SQL.Exec.Get_Task_Connection check if the current task have an
   --  active database connection, and if not it calls this factory function.

   ----------------------------
   --  Database Description  --
   ----------------------------

   DB_Description : Database_Description;
   --  Describes access to the database, ie. user, host, password and such.

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

   -------------------------
   --  Get_DB_Connection  --
   -------------------------

   function Get_DB_Connection return Database_Connection
   is
   begin

      return Get_Task_Connection
        (Description  => DB_Description,
         Factory      => Database_Connection_Factory'Access);

   end Get_DB_Connection;

   ------------------
   --  Initialize  --
   ------------------

   procedure Initialize
   is
   begin

      Setup_Database (Description => DB_Description,
                      Database    => "12boo",
                      User        => "thomas",
                      Host        => "192.168.57.2",
                      Password    => "respsltl16117994",
                      DBMS        => DBMS_Postgresql);

   end Initialize;

begin

   Initialize;

end DB_Connection;
