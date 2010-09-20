-------------------------------------------------------------------------------
--                                                                           --
--                                  Yolk                                     --
--                                                                           --
--                              simple_email                                 --
--                                                                           --
--                                  BODY                                     --
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

with AWS.Attachments;
with AWS.MIME;
with AWS.SMTP;
with AWS.SMTP.Client;

package body Simple_Email is

   ------------
   --  Send  --
   ------------

   function Send (HTML_Part   : in String;
                  Text_Part   : in String;
                  From_Email  : in String;
                  From_Name   : in String;
                  To_Email    : in String;
                  To_Name     : in String;
                  Subject     : in String;
                  SMTP_Server : in String) return Boolean
   is

      SMTP        : constant AWS.SMTP.Receiver
        := AWS.SMTP.Client.Initialize (SMTP_Server);
      E_Content   : AWS.Attachments.List;
      Alter       : AWS.Attachments.Alternatives;
      Status      : AWS.SMTP.Status;

   begin

      AWS.Attachments.Add
        (Parts => Alter,
         Data  => AWS.Attachments.Value
           (Data => Text_Part,
            Content_Type => AWS.MIME.Text_Plain & "; charset=ISO-8859-1"));
      AWS.Attachments.Add
        (Parts => Alter,
         Data  => AWS.Attachments.Value
           (Data => HTML_Part,
            Content_Type => AWS.MIME.Text_HTML & "; charset=ISO-8859-1"));

      AWS.Attachments.Add (Attachments => E_Content,
                           Parts       => Alter);

      AWS.SMTP.Client.Send
        (Server      => SMTP,
         From        => AWS.SMTP.E_Mail
           (From_Name, From_Email),
         To          => AWS.SMTP.Recipients'
           (1 => AWS.SMTP.E_Mail (To_Name, To_Email)),
         Subject     => Subject,
         Attachments => E_Content,
         Status      => Status);

      if AWS.SMTP.Is_Ok (Status) then
         return True;
      else
         return False;
      end if;

   end Send;

end Simple_Email;
