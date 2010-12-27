-------------------------------------------------------------------------------
--                                                                           --
--                                  Yolk                                     --
--                                                                           --
--                        DBMS_Connection.PostgreSQL                         --
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

   These_Credentials : Credentials;

package Connect_To_DB.PostgreSQL is

   function Connection return GNATCOLL.SQL.Exec.Database_Connection;
   --  Return a thread specific access to the database.

private

   Association : Protected_Association_Map;
   --  The protected object that holds the hashed map in which the association
   --  between AWS and GNATCOLL.Database_Connection tasks are maintained.

   DB_Description : GNATCOLL.SQL.Exec.Database_Description;
   --  Describes access to the database, ie. user, host, password and such.

   procedure Initialize;
   --  Is called when this generic is instantiated. It populates the
   --  DB_Description variable.

end Connect_To_DB.PostgreSQL;
