-------------------------------------------------------------------------------
--                                                                           --
--                                  Yolk                                     --
--                                                                           --
--                                  view                                     --
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

private with AWS.Config;
private with AWS.Response;
private with AWS.Templates;
private with My_Configuration;
private with Connect_To_DB.PostgreSQL;

package View is

private

   package My renames My_Configuration;

   package DB_12boo is new Connect_To_DB.PostgreSQL
     (Connect_To_DB.Set_Credentials
        (Host          => My.Config.Get (My.DB_Host),
         Database      => My.Config.Get (My.DB_Name),
         User          => My.Config.Get (My.DB_User),
         Password      => My.Config.Get (My.DB_Password),
         Server_Config => AWS.Config.Get_Current));

   package DB_Wiki is new Connect_To_DB.PostgreSQL
     (Connect_To_DB.Set_Credentials
        (Host          => My.Config.Get (My.DB_Host),
         Database      => "wikidb",
         User          => "wikiuser",
         Password      => My.Config.Get (My.DB_Password),
         Server_Config => AWS.Config.Get_Current));

   function Build_Response (Template_File : in String;
                            Translations  : in AWS.Templates.Translate_Set)
                            return AWS.Response.Data;
   --  Build the resource response.
   --  This is a convenience function that gets rid of a few with clauses in
   --  the files for the individual resources. Also since we need to create the
   --  AWS.Response.Data object for each and every resource, we might as well
   --  shorten the call a bit.

end View;
