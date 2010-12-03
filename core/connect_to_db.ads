-------------------------------------------------------------------------------
--                                                                           --
--                                  Yolk                                     --
--                                                                           --
--                           Database_Connection                             --
--                                                                           --
--                                  SPEC                                     --
--                                                                           --
--                     Copyright (C) 2010, Thomas Løcke                      --
--                                                                           --
--  Yolk is free software;  you can  redistribute it  and/or modify it under --
--  terms of the  GNU General Public License as published  by the Free Soft- --
--  ware  Foundation;  either version 2,  or (at your option) any later ver- --
--  sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
--  OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
--  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
--  for  more details.  You should have  received  a copy of the GNU General --
--  Public License  distributed with Yolk.  If not, write  to  the  Free     --
--  Software Foundation,  51  Franklin  Street,  Fifth  Floor, Boston,       --
--  MA 02110 - 1301, USA.                                                    --
--                                                                           --
-------------------------------------------------------------------------------

with Ada.Strings.Unbounded;
with AWS.Config;

package Connect_To_DB is

   type Credentials is private;

   function Set_Credentials (Host          : in String;
                             Database      : in String;
                             User          : in String;
                             Password      : in String;
                             Server_Config : in AWS.Config.Object)
                             return Credentials;

private

   use Ada.Strings.Unbounded;

   type Credentials is record
      Host     : Unbounded_String;
      Database : Unbounded_String;
      User     : Unbounded_String;
      Password : Unbounded_String;
      Threads  : Positive;
   end record;

end Connect_To_DB;
