-------------------------------------------------------------------------------
--                                                                           --
--                                  Yolk                                     --
--                                                                           --
--                               View.Email                                  --
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

with Ada.Strings.Fixed;
with AWS.Parameters;
with AWS.Templates;
with Yolk.Email.Composer;
with Yolk.Log;

package body View.Email is

   ---------------
   --  Generate --
   ---------------

   function Generate
     (Request : in AWS.Status.Data)
      return AWS.Response.Data
   is
      use AWS.Templates;
      use Yolk.Log;

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
         Insert
           (T, Assoc ("SMTP_HOST", String'(My.Config.Get (My.SMTP_Host))));
         Insert
           (T, Assoc ("SMTP_PORT", String'(My.Config.Get (My.SMTP_Port))));
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
                              SMTP_Server  => My.Config.Get (My.SMTP_Host),
                              SMTP_Port    => My.Config.Get (My.SMTP_Port),
                              Charset      => UTF8);

               if Composer.Is_Send (Email) then
                  Insert (T, Assoc ("IS_SEND", True));
                  Insert (T, Assoc ("SMTP_HOST",
                    String'(My.Config.Get (My.SMTP_Host))));

                  Trace (Handle  => Info,
                         Message => "Email sent to " &
                         P_Recip_Address &
                         " using " &
                         String'(My.Config.Get (My.SMTP_Host)));
               else
                  Insert (T, Assoc ("IS_SEND", False));
                  --  Sending failed.

                  Trace (Handle  => Error,
                         Message => "Email failed to " &
                         P_Recip_Address &
                         " using " &
                         String'(My.Config.Get (My.SMTP_Host)));

                  Populate_Form (T             => T,
                                 Recip_Name    => P_Recip_Name,
                                 Recip_Address => P_Recip_Address);
               end if;
            else
               Insert (T, Assoc ("IS_SEND", False));
               --  No recipient address, so obviously we cannot send the email.

               Populate_Form (T             => T,
                              Recip_Name    => P_Recip_Name,
                              Recip_Address => P_Recip_Address);
            end if;
         end;
      else
         Populate_Form (T             => T,
                        Recip_Name    => "Zaphod Beeblebrox",
                        Recip_Address => "yolk@mailinator.com");
      end if;

      return Build_Response
        (Status_Data   => Request,
         Template_File => My.Config.Get (My.Template_Email),
         Translations  => T);
   end Generate;

end View.Email;
