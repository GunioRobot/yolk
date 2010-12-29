-------------------------------------------------------------------------------
--                                                                           --
--                                  Yolk                                     --
--                                                                           --
--                        DBMS_Connection.PostgreSQL                         --
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

package body Connect_To_DB.PostgreSQL is

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

      case Task_To_DB_Mapping_Method is
         when AWS_Tasks_To_DB =>
            --  We map AWS tasks directly to the GNATCOLL database connections.
            A_Connection := Get_Task_Connection
              (Description  => DB_Description,
               Factory      => Database_Connection_Factory'Access);
         when DB_Conn_Tasks_To_DB =>
            --  We have a pool of DB_Conn tasks, and these are mapped to the
            --  AWS tasks in the Association protected object. This is a hashed
            --  map.
            A_DB_Task := Association.Get (AWS_Task_ID => Current_Task);

            if A_DB_Task = Null_DB_Conn_Access then
               Association.Set (DB_Task     => A_DB_Task,
                                AWS_Task_ID => Current_Task);
            end if;

            A_DB_Task.Fetch (Conn => A_Connection,
                             Desc => DB_Description);
      end case;

      return A_Connection;

   end Connection;

   ------------------
   --  Initialize  --
   ------------------

   procedure Initialize
   is

      use GNATCOLL.SQL.Exec;

   begin

      if Task_To_DB_Mapping_Method = AWS_Tasks_To_DB
        and then Connect_To_DB.AWS_Tasks_To_DB_Already_Used
      then
         raise Program_Error with "AWS_Tasks_To_DB may only be used once.";
      end if;

      Setup_Database (Description => DB_Description,
                      Database    => DB_Credentials.Database,
                      User        => DB_Credentials.User,
                      Host        => DB_Credentials.Host,
                      Password    => DB_Credentials.Password,
                      DBMS        => DBMS_Postgresql);
      --  Populate the DB_Description variable.

      if Task_To_DB_Mapping_Method = AWS_Tasks_To_DB then
         Connect_To_DB.AWS_Tasks_To_DB_Already_Used := True;
         --  We only accept _one_ instantation using the AWS_Tasks_To_DB
         --  method.
      end if;

   end Initialize;

begin

   Initialize;

end Connect_To_DB.PostgreSQL;
