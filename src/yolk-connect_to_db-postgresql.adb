-------------------------------------------------------------------------------
--                                                                           --
--                                  Yolk                                     --
--                                                                           --
--                      Yolk.Connect_To_DB.PostgreSQL                        --
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

with Ada.Task_Attributes;

package body Yolk.Connect_To_DB.PostgreSQL is

   package Task_Association is new Ada.Task_Attributes (DB_Conn_Access, null);
   --  Associate AWS tasks with Connect_To_DB.DB_Conn tasks.
   --  Yes, this package is completely redundant if we're using the
   --  AWS_Tasks_To_DB Task_To_DB_Mapping_Method.

   DB_Description : GNATCOLL.SQL.Exec.Database_Description;
   --  Describes access to the database, ie. user, host, password and such.

   procedure Initialize;
   --  Is called when this generic is instantiated. It populates the
   --  DB_Description variable.

   ------------------
   --  Connection  --
   ------------------

   function Connection return GNATCOLL.SQL.Exec.Database_Connection
   is
      use GNATCOLL.SQL.Exec;
   begin
      case Task_To_DB_Mapping_Method is
         when AWS_Tasks_To_DB =>
            --  We map AWS tasks directly to the GNATCOLL database connections.
            return Get_Task_Connection
              (Description  => DB_Description,
               Factory      => Database_Connection_Factory'Access);
         when DB_Conn_Tasks_To_DB =>
            --  Map AWS tasks to DB_Conn tasks, and use the DB_Conn tasks to
            --  fetch the database connection.
            declare
               A_Connection   : Database_Connection;
               A_DB_Task      : DB_Conn_Access;
            begin
               A_DB_Task := Task_Association.Value;

               if A_DB_Task = Null_DB_Conn_Access then
                  A_DB_Task := new DB_Conn;
                  Task_Association.Set_Value (Val => A_DB_Task);
               end if;

               A_DB_Task.Fetch (Conn => A_Connection,
                                Desc => DB_Description);

               return A_Connection;
            end;
      end case;
   end Connection;

   ------------------
   --  Initialize  --
   ------------------

   procedure Initialize
   is
      use GNATCOLL.SQL.Exec;
   begin
      if Task_To_DB_Mapping_Method = AWS_Tasks_To_DB
        and then AWS_Tasks_To_DB_Already_Used
      then
         raise Program_Error with "AWS_Tasks_To_DB may only be used once.";
      end if;

      Setup_Database (Description   => DB_Description,
                      Database      => DB_Credentials.Database,
                      User          => DB_Credentials.User,
                      Host          => DB_Credentials.Host,
                      Password      => DB_Credentials.Password,
                      DBMS          => DBMS_Postgresql,
                      SSL           => Prefer);
      --  Populate the DB_Description variable.

      if Task_To_DB_Mapping_Method = AWS_Tasks_To_DB then
         AWS_Tasks_To_DB_Already_Used := True;
         --  We only accept _one_ instantation using the AWS_Tasks_To_DB
         --  method.
      end if;
   end Initialize;

begin
   Initialize;

end Yolk.Connect_To_DB.PostgreSQL;
