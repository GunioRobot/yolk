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
with Yolk.Utilities;

package body View.Index is

   ---------------
   --  Generate --
   ---------------

   function Generate
     (Request : in AWS.Status.Data)
      return AWS.Response.Data
   is

      use Ada.Calendar;
      use AWS.Templates;
      use Yolk.Utilities;

      T     : Translate_Set;
      Now   : Time;
      Valid : Boolean := False;
      Value : Unbounded_String;

   begin

      Cache2.Read (Key      => "index",
                   Is_Valid => Valid,
                   Value    => Value);

      if not Valid then
         Now := Clock;
         Insert (T, Assoc ("YOLK_VERSION", Version));
         Insert (Set  => T,
                 Item => Assoc ("COPYRIGHT_YEAR", Year (Now)));

         Value := TUS
           (Parse (Filename     => My.Config.Get (My.Template_Index),
                   Translations => T,
                   Cached       => True));

         Cache2.Write
           (Key   => "index",
            Value => Value);
      end if;

      return Build_Response
        (Status_Data => Request,
         Content     => TS (Value));

   end Generate;

end View.Index;
