-------------------------------------------------------------------------------
--                                                                           --
--                                  Yolk                                     --
--                                                                           --
--                              db_connection                                --
--                                                                           --
--                                  SPEC                                     --
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

--  This package provides the convenience function Get_DB_Connection, which
--  basically just hides away some of the GNATCOLL database stuff.
--  Currently DB_Connection only supports PostgreSQL, so if you need MySQL or
--  SQLite, then you'll have to use the GNATCOLL database packages directly.

with GNATCOLL.SQL;
with GNATCOLL.SQL.Exec;

package DB_Connection is

   use GNATCOLL.SQL.Exec;

   function Get_DB_Connection return Database_Connection;
   --  Return a thread specific access to the database.

end DB_Connection;
