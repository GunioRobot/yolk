-------------------------------------------------------------------------------
--                                                                           --
--                                  Yolk                                     --
--                                                                           --
--                           Yolk.Connect_To_DB                              --
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

with GNATCOLL.SQL.Postgres;

package body Yolk.Connect_To_DB is

   -----------------------------------
   --  Database_Connection_Factory  --
   -----------------------------------

   function Database_Connection_Factory
     (Desc : in GNATCOLL.SQL.Exec.Database_Description)
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
                          Desc : in Database_Description)
            do
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

   -----------------------
   --  Set_Credentials  --
   -----------------------

   function Set_Credentials
     (Host          : in String;
      Database      : in String;
      User          : in String;
      Password      : in String)
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

      return C;
   end Set_Credentials;

end Yolk.Connect_To_DB;
