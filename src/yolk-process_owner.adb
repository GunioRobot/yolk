-------------------------------------------------------------------------------
--                                                                           --
--                                  Yolk                                     --
--                                                                           --
--                           Yolk.Process_Owner                              --
--                                                                           --
--                                  BODY                                     --
--                                                                           --
--                   Copyright (C) 2010-2012, Thomas Løcke                   --
--                                                                           --
--  This library is free software;  you can redistribute it and/or modify    --
--  it under terms of the  GNU General Public License  as published by the   --
--  Free Software  Foundation;  either version 3,  or (at your  option) any  --
--  later version. This library is distributed in the hope that it will be   --
--  useful, but WITHOUT ANY WARRANTY;  without even the implied warranty of  --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     --
--                                                                           --
--  As a special exception under Section 7 of GPL version 3, you are         --
--  granted additional permissions described in the GCC Runtime Library      --
--  Exception, version 3.1, as published by the Free Software Foundation.    --
--                                                                           --
--  You should have received a copy of the GNU General Public License and    --
--  a copy of the GCC Runtime Library Exception along with this program;     --
--  see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
--  <http://www.gnu.org/licenses/>.                                          --
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
