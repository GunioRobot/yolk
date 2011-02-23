-------------------------------------------------------------------------------
--                                                                           --
--                                  Yolk                                     --
--                                                                           --
--                              View.DB_Test                                 --
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

-------------------------------------------------------------------------------
--                                                                           --
--                            DEMO FILE                                      --
--                                                                           --
-------------------------------------------------------------------------------

--  This is a DEMO file. You can either move this to the my_view/ directory and
--  change it according to you own needs, or you can provide your own.
--
--  This package is currently only "with'ed" by other demo source files. It is
--  NOT required by Yolk in any way.

with Ada.Text_IO;
with AWS.Templates;
with GNATCOLL.SQL;
with GNATCOLL.SQL.Exec;

package body View.DB_Test is

   ---------------
   --  Generate --
   ---------------

   function Generate
     (Request : in AWS.Status.Data)
      return AWS.Response.Data
   is

      use AWS.Templates;
      use GNATCOLL.SQL;
      use GNATCOLL.SQL.Exec;

      Names : constant array (Integer range 1 .. 5) of String (1 .. 5) :=
                (1 => "Arnie",
                 2 => "David",
                 3 => "Bobby",
                 4 => "Tommy",
                 5 => "Bruce");
      Me : aliased String := "     ";

      DB_Conn     : constant Database_Connection := My_DB.Connection;
      DB_Cursor   : Forward_Cursor;
      DB_STMT     : constant Prepared_Statement := Prepare
        ("INSERT INTO tmp (id, name) VALUES ($1, $2)");

      Has_Tmp_Table : Boolean := False;

      T : Translate_Set;

   begin

      if DB_Conn.Check_Connection then
         Insert (T, Assoc ("DB_SETUP", True));

         DB_Cursor.Fetch (DB_Conn,
                          "SELECT tablename " &
                          "FROM pg_tables " &
                          "WHERE schemaname = 'public' " &
                          "AND tablename = 'tmp'");

         while DB_Cursor.Has_Row loop
            Has_Tmp_Table := True;
            exit;
         end loop;

         if not Has_Tmp_Table then
            DB_Conn.Execute
              ("CREATE TABLE tmp (id INTEGER, name TEXT)");
         end if;

         for i in Names'Range loop
            Me := Names (i);
            DB_Conn.Execute (Stmt   => DB_STMT,
                             Params => (1 => +i,
                                        2 => +Me'Access));
         end loop;

         DB_Cursor.Fetch (DB_Conn, "SELECT * FROM tmp");

         while DB_Cursor.Has_Row loop
            Ada.Text_IO.Put_Line ("id=" & Integer_Value (DB_Cursor, 0)'Img);
            Ada.Text_IO.Put_Line ("name=" & Value (DB_Cursor, 1));
            DB_Cursor.Next;
         end loop;

         --  DB_Conn.Execute ("DROP TABLE tmp");

         DB_Conn.Commit_Or_Rollback;
      else
         Insert (T, Assoc ("DB_SETUP", False));
      end if;

      return Build_Response
        (Status_Data   => Request,
         Template_File => My.Config.Get (My.Template_DB_Test),
         Translations  => T);

   end Generate;

end View.DB_Test;
