-------------------------------------------------------------------------------
--                                                                           --
--                                  Yolk                                     --
--                                                                           --
--                               View.Index                                  --
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

with Ada.Calendar;
with AWS.Session;

package body View.Index is

   ---------------
   --  Generate --
   ---------------

   function Generate
     (Request : in AWS.Status.Data)
      return AWS.Response.Data
   is
      use Ada.Calendar;
      use AWS.Session;
      use AWS.Templates;

      Session_Id : Id;
      T          : Translate_Set;
   begin
      if AWS.Status.Has_Session (Request) then
         Insert (T, Assoc ("SESSION_ENABLED", True));

         Session_Id := AWS.Status.Session (Request);

         if Get (Session_Id, "counter") > 0 then
            Insert (T, Assoc ("SESSION_COUNTER",
              Natural'(Get (Session_Id, "counter"))));

            Set (Session_Id, "counter", 0);
         end if;
      end if;

      Insert (T, Assoc ("YOLK_VERSION", Yolk.Version));
      Insert (T, Assoc ("COPYRIGHT_YEAR", Year (Clock)));

      return Build_Response
        (Status_Data => Request,
         Content     =>
           Parse (Filename     => My.Config.Get (My.Template_Index),
                  Translations => T,
                  Cached       => True));
   end Generate;

end View.Index;
