-------------------------------------------------------------------------------
--                                                                           --
--                                  Yolk                                     --
--                                                                           --
--                              View.DB_Test                                 --
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

--  The dbtest resource.

with AWS.Response;
with AWS.Status;

package View.DB_Test is

   type Str_Ptr is access all String;
   Names : constant array (Integer range 1 .. 5) of Str_Ptr :=
             (1 => new String'("Billy"),
              2 => new String'("David"),
              3 => new String'("Bobby"),
              4 => new String'("Tommy"),
              5 => new String'("Bruce"));
   --  Test data to add to the database.

   function Generate
     (Request : in AWS.Status.Data)
      return AWS.Response.Data;
   --  Generate the content for the /database resource.

end View.DB_Test;
