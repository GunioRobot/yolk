-------------------------------------------------------------------------------
--                                                                           --
--                                  Yolk                                     --
--                                                                           --
--                               View.Email                                  --
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

-------------------------------------------------------------------------------
--                                                                           --
--                            DEMO FILE                                      --
--                                                                           --
-------------------------------------------------------------------------------

--  This is a DEMO file. You can either move this to the my_view/ directory and
--  change it according to you own needs, or you can provide your own.
--
--  This package is currently only "with'ed" by other demo source files. It is
--  NOT required by Yolk in any way.

with Ada.Strings.Fixed;
with AWS.Parameters;
with AWS.Templates;
with My_Configuration;
with Yolk.Email.Composer;

package body View.Email is

   ---------------
   --  Generate --
   ---------------

   function Generate
     (Request : in AWS.Status.Data)
      return AWS.Response.Data
   is

      use AWS.Templates;
      use My_Configuration;

      procedure Populate_Form
        (T             :    out Translate_Set;
         Recip_Name    : in     String;
         Recip_Address : in     String);
      --  Insert the form data associations into the T translate set.

      ---------------------
      --  Populate_Form  --
      ---------------------

      procedure Populate_Form
        (T             :    out Translate_Set;
         Recip_Name    : in     String;
         Recip_Address : in     String)
      is
      begin

         Insert (T, Assoc ("RECIP_NAME", Recip_Name));
         Insert (T, Assoc ("RECIP_ADDRESS", Recip_Address));
         Insert (T, Assoc ("SMTP_HOST", String'(Config.Get (SMTP_Host))));
         Insert (T, Assoc ("SMTP_PORT", String'(Config.Get (SMTP_Port))));

      end Populate_Form;

      P : constant AWS.Parameters.List := AWS.Status.Parameters (Request);
      T : Translate_Set;

   begin

      if P.Exist ("recip_name")
        and then P.Exist ("recip_address")
      then
         declare

            use Ada.Strings;
            use Yolk.Email;

            P_Recip_Name      : constant String :=
                                  Fixed.Trim (P.Get ("recip_name"), Both);
            P_Recip_Address   : constant String :=
                                  Fixed.Trim (P.Get ("recip_address"), Both);
            Email             : Structure;

         begin

            if P_Recip_Address /= "" then
               Composer.Add_Custom_Header (ES      => Email,
                                           Name    => "User-Agent",
                                           Value   => "Yolk " & Yolk.Version);
               Composer.Send (ES           => Email,
                              From_Address => "thomas@12boo.net",
                              From_Name    => "Thomas Løcke",
                              To_Address   => P_Recip_Address,
                              To_Name      => P_Recip_Name,
                              Subject      => "Test email",
                              Text_Part    => "Test email from Yolk",
                              SMTP_Server  => Config.Get (SMTP_Host),
                              SMTP_Port    => Config.Get (SMTP_Port),
                              Charset      => ISO_8859_1);

               if Composer.Is_Send (Email) then
                  Insert (T, Assoc ("IS_SEND", True));
                  Insert
                    (T, Assoc ("SMTP_HOST", String'(Config.Get (SMTP_Host))));
               else
                  Insert (T, Assoc ("IS_SEND", False));
               end if;
            else
               Insert (T, Assoc ("IS_SEND", False));
            end if;

            Populate_Form (T             => T,
                           Recip_Name    => P_Recip_Name,
                           Recip_Address => P_Recip_Address);
         end;
      else
         Populate_Form (T             => T,
                        Recip_Name    => "Zaphod Beeblebrox",
                        Recip_Address => "yolk@mailinator.com");
      end if;

      return Build_Response
        (Status_Data   => Request,
         Template_File => Config.Get (Template_Email),
         Translations  => T);

   end Generate;

end View.Email;
