-------------------------------------------------------------------------------
--                                                                           --
--                                  Yolk                                     --
--                                                                           --
--                      Yolk.Connect_To_DB.PostgreSQL                        --
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

generic

   DB_Credentials : Credentials;
   --  The database credentials, such as username, password, host and similar.
   --  Note that if an SSL connection is available to the host, then SSL is
   --  preferred over non-SSL.

   Task_To_DB_Mapping_Method : Connection_Mapping_Method;
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

package Yolk.Connect_To_DB.PostgreSQL is

   function Connection return GNATCOLL.SQL.Exec.Database_Connection;
   --  Return a thread specific access to the database.

end Yolk.Connect_To_DB.PostgreSQL;
