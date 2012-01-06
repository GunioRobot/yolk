-------------------------------------------------------------------------------
--                                                                           --
--                                  Yolk                                     --
--                                                                           --
--                            View.Session_Test                              --
--                                                                           --
--                                  BODY                                     --
--                                                                           --
--                   Copyright (C) 2010-2012, Thomas Løcke                   --
--                                                                           --
--  This is free software;  you can redistribute it and/or modify it         --
--  under terms of the  GNU General Public License  as published by the      --
--  Free Software  Foundation;  either version 3,  or (at your  option) any  --
--  later version. This library is distributed in the hope that it will be   --
--  useful, but WITHOUT ANY WARRANTY;  without even the implied warranty of  --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     --
--  You should have received a copy of the GNU General Public License and    --
--  a copy of the GCC Runtime Library Exception along with this program;     --
--  see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
--  <http://www.gnu.org/licenses/>.                                          --
--                                                                           --
-------------------------------------------------------------------------------

with AWS.Session;
with AWS.Templates;

package body View.Session_Test is

   ---------------
   --  Generate --
   ---------------

   function Generate
     (Request : in AWS.Status.Data)
      return AWS.Response.Data
   is
      use AWS.Session;
      use AWS.Templates;

      Counter    : Natural := 0;
      Session_Id : Id;
      T          : Translate_Set;
   begin
      if AWS.Status.Has_Session (Request) then
         Insert (T, Assoc ("SESSION_ENABLED", True));

         Session_Id := AWS.Status.Session (Request);

         Insert (T, Assoc ("HAS_SESSION_ID", True));
         Insert (T, Assoc ("SESSION_ID", Image (Session_Id)));

         Counter := Get (Session_Id, "counter");
         Insert (T, Assoc ("SESSION_COUNTER", Counter));
         Counter := Counter + 1;
         Set (Session_Id, "counter", Counter);
      end if;

      return Build_Response
        (Status_Data   => Request,
         Template_File => My.Config.Get (My.Template_Session_Test),
         Translations  => T);
   end Generate;

end View.Session_Test;
