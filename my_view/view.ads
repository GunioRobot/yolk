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

with AWS.Config;
with AWS.Response;
with AWS.Templates; use AWS.Templates;
with Connect_To_DB.PostgreSQL;

package View is

private

   package DB_12boo is new Connect_To_DB.PostgreSQL
     (Connect_To_DB.Set_Credentials
        (Host          => "freja.serverbox.dk",
         Database      => "12boo",
         User          => "thomas",
         Password      => "respsltl16117994",
         Server_Config => AWS.Config.Get_Current));

   package DB_Wiki is new Connect_To_DB.PostgreSQL
     (Connect_To_DB.Set_Credentials
        (Host          => "freja.serverbox.dk",
         Database      => "wikidb",
         User          => "wikiuser",
         Password      => "respsltl16117994",
         Server_Config => AWS.Config.Get_Current));

   function Build_Response (Template_File : in String;
                            Translations  : in Translate_Set)
                            return AWS.Response.Data;
   --  Build the resource response.
   --  This is a convenience function that gets rid of a few with clauses in
   --  the files for the individual resources. Also since we need to create the
   --  AWS.Response.Data object for each and every resource, we might as well
   --  shorten the call a bit.

end View;
