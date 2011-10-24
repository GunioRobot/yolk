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

with AWS.Templates;
with Database;
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
      use Database;
      use GNATCOLL.SQL;
      use GNATCOLL.SQL.Exec;

      Billy          : aliased String := "Billy";
      DB_Conn        : constant Database_Connection := My_DB.Connection;
      FC             : Forward_Cursor;
      Has_Tmp_Table  : Boolean := False;

      Query_Insert_Data : constant SQL_Query := SQL_Insert
        ((Tmp.Id = Integer_Param (1)) &
         (Tmp.Name = Text_Param (2)));

      Prepared_Query_Insert_Data : constant Prepared_Statement := Prepare
        (Query       => Query_Insert_Data,
         On_Server   => True);

      Query_Select_Data : constant SQL_Query := SQL_Select
        (Fields   => Tmp.Id & Tmp.Name,
         From     => Tmp,
         Where    => Tmp.Name = Text_Param (1));

      Prepared_Query_Select_Data : constant Prepared_Statement := Prepare
        (Query       => Query_Select_Data,
         On_Server   => True);

      Messages : Vector_Tag;
      T        : Translate_Set;
   begin
      if DB_Conn.Check_Connection then
         Insert (T, Assoc ("DB_SETUP", True));

         FC.Fetch (DB_Conn,
                   "SELECT tablename " &
                   "FROM pg_tables " &
                   "WHERE schemaname = 'public' " &
                   "AND tablename = 'tmp'");

         Append (Messages, "Checking if table 'tmp' exists.");

         while FC.Has_Row loop
            Has_Tmp_Table := True;
            Append (Messages, "Table 'tmp' found.");
            exit;
         end loop;

         if not Has_Tmp_Table then
            DB_Conn.Execute
              ("CREATE TABLE tmp (id INTEGER, name TEXT)");

            Append (Messages, "Table 'tmp' not found. Creating it.");
            Append (Messages, "Table 'tmp' created.");
         end if;

         for i in Names'Range loop
            DB_Conn.Execute (Stmt   => Prepared_Query_Insert_Data,
                             Params => (1 => +i,
                                        2 => +Names (i)));

            Append (Messages, "Added " & i'Img & ":" & Names (i).all &
                    " to 'tmp'.");
         end loop;

         FC.Fetch (Connection => DB_Conn,
                   Stmt       => Prepared_Query_Select_Data,
                   Params     => (1 => +Billy'Access));

         Append (Messages, "Querying 'tmp' for " & Billy & ".");

         while FC.Has_Row loop
            Append (Messages, "Found " & FC.Integer_Value (0)'Img & ":"
                    & FC.Value (1) & " pair.");
            FC.Next;
         end loop;

         DB_Conn.Execute ("DROP TABLE tmp");

         Append (Messages, "Table 'tmp' dropped.");

         DB_Conn.Commit_Or_Rollback;

         if DB_Conn.Success then
            Insert (T, Assoc ("SUCCESS", True));
            Append (Messages, "Transaction succesfully commited.");
         else
            Insert (T, Assoc ("SUCCESS", False));
            Append (Messages, "Commit failed.");
         end if;
      else
         Insert (T, Assoc ("DB_SETUP", False));
      end if;

      Insert (T, Assoc ("MESSAGES", Messages));

      return Build_Response
        (Status_Data   => Request,
         Template_File => My.Config.Get (My.Template_DB_Test),
         Translations  => T);
   end Generate;

end View.DB_Test;
