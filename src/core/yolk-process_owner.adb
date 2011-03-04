-------------------------------------------------------------------------------
--                                                                           --
--                                  Yolk                                     --
--                                                                           --
--                           Yolk.Process_Owner                              --
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

with POSIX;
with POSIX.Process_Identification;
with POSIX.User_Database;

package body Yolk.Process_Owner is

   ----------------
   --  Set_User  --
   ----------------

   procedure Set_User
     (Username : in String)
   is

      use POSIX;
      use POSIX.Process_Identification;
      use POSIX.User_Database;

      User_DI     : User_Database_Item;
      P_Username  : constant POSIX_String := To_POSIX_String (Username);

   begin

      User_DI := Get_User_Database_Item (Name => P_Username);
      Set_Group_ID (ID => Group_ID_Of (DB_Item => User_DI));
      --  Set gid first, else we might end up not having permission to set it
      --  later.
      Set_User_ID (ID => User_ID_Of (DB_Item => User_DI));

   exception
      when others =>
         raise Username_Does_Not_Exist with Username;
   end Set_User;

end Yolk.Process_Owner;
