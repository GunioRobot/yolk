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

with Ada.Directories; use Ada.Directories;
with AWS.Headers;
with AWS.MIME;
with AWS.SMTP.Client;

package body Simple_Email is

   ----------------------
   --  Add_Attachment  --
   ----------------------

   procedure Add_Attachment (ES                 : in out Email_Structure;
                             Attach_File        : in     String;
                             Encode_Attachment  : in     Encoding := Base64)
   is

      An_Attachment : File_Attachment;

   begin

      if Attach_File /= "" then
         if Exists (Attach_File) then
            An_Attachment.File   := To_Unbounded_String (Attach_File);
            if Encode_Attachment = Base64 then
               An_Attachment.Encode := AWS.Attachments.Base64;
            else
               An_Attachment.Encode := AWS.Attachments.None;
            end if;
            ES.Attachments_List.Append (An_Attachment);
            ES.Email_Type := Complex_Multipart;
         else
            raise Attachment_File_Not_Found with Attach_File;
         end if;
      end if;

   exception
      when others =>
         raise Attachment_File_Not_Found with Attach_File;

   end Add_Attachment;

   ---------------------
   --  Add_Recipient  --
   ---------------------

   procedure Add_Recipient (ES         : in out Email_Structure;
                            To_Address : in     String;
                            To_Name    : in     String)
   is
   begin

      ES.To_List.Append (AWS.SMTP.E_Mail (Name    => To_Name,
                                          Address => To_Address));

   end Add_Recipient;

   --------------------
   --  Create_Email  --
   --------------------

   procedure Create_Email (ES                : in out Email_Structure;
                           HTML_Part         : in     String;
                           Text_Part         : in     String;
                           From_Address      : in     String;
                           From_Name         : in     String;
                           To_Address        : in     String;
                           To_Name           : in     String;
                           Subject           : in     String := "(no subject)";
                           Charset           : in     String := "iso-8859-1";
                           Attach_File       : in     String := "";
                           Encode_Attachment : in     Encoding := Base64;
                           SMTP_Server       : in     String := "localhost")
   is
   begin

      if HTML_Part /= "" then
         ES.HTML_Part := To_Unbounded_String (HTML_Part);
         ES.Email_Type := Complex_Multipart;
      end if;

      ES.Text_Part := To_Unbounded_String (Text_Part);

      ES.From := AWS.SMTP.E_Mail (Name    => From_Name,
                                  Address => From_Address);

      Add_Recipient (ES         => ES,
                     To_Address => To_Address,
                     To_Name    => To_Name);

      ES.Subject := To_Unbounded_String (Subject);

      ES.Charset := To_Unbounded_String (Charset);

      Add_Attachment (ES                => ES,
                      Attach_File       => Attach_File,
                      Encode_Attachment => Encode_Attachment);

      if SMTP_Server /= "" then
         ES.SMTP_List.Append (To_Unbounded_String (SMTP_Server));
      else
         null;
         --  TODO: No SMTP server given. Raise exception or??
      end if;

   end Create_Email;

   ---------------------
   --  Get_Recipient  --
   ---------------------

   function Get_Recipients (ES : in Email_Structure) return AWS.SMTP.Recipients
   is

      Recipient_List : AWS.SMTP.Recipients (1 .. ES.To_List.Last_Index);

   begin

      --  Populate the Recipient_List array with the elements found in the
      --  To_List vector.
      if Recipient_List'Length > 1 then
         for i in Recipient_List'Range loop
            Recipient_List (i) := ES.To_List.Element (i);
         end loop;
      else
         Recipient_List (1) := ES.To_List.Element (1);
      end if;

      return Recipient_List;

   end Get_Recipients;

   ---------------
   --  Is_Send  --
   ---------------

   function Is_Send (ES : in Email_Structure) return Boolean
   is
   begin

      if ES.Is_Email_Send = Yes then
         return True;
      else
         return False;
      end if;

   end Is_Send;

   ------------
   --  Send  --
   ------------

   procedure Send (ES : in out Email_Structure)
   is
   begin

      if ES.Email_Type = Simple_Text_Only then
         Send_Simple_Text_Only (ES);
      else
         Send_Complex_Multipart (ES);
      end if;

   end Send;

   ------------------------------
   --  Send_Complex_Multipart  --
   ------------------------------

   procedure Send_Complex_Multipart (ES : in out Email_Structure)
   is

      SMTP              : constant AWS.SMTP.Receiver
        := AWS.SMTP.Client.Initialize (TS (ES.SMTP_List.Element (1)));
      Email_Contents    : AWS.Attachments.List;
      Status            : AWS.SMTP.Status;

   begin

      --  Add the alternative parts.
      Set_Alternative_Parts (C   => Email_Contents,
                             ES  => ES);

      --  Add the file attachments.
      Set_File_Attachments (C  => Email_Contents,
                            ES => ES);

      --  Send the email.
      AWS.SMTP.Client.Send
        (Server      => SMTP,
         From        => ES.From,
         To          => Get_Recipients (ES),
         Subject     => To_String (ES.Subject),
         Attachments => Email_Contents,
         Status      => Status);

      if AWS.SMTP.Is_Ok (Status) then
         ES.Is_Email_Send := Yes;
      end if;

   end Send_Complex_Multipart;

   -----------------------------
   --  Send_Simple_Text_Only  --
   -----------------------------

   procedure Send_Simple_Text_Only (ES : in out Email_Structure)
   is

      SMTP              : constant AWS.SMTP.Receiver
        := AWS.SMTP.Client.Initialize (TS (ES.SMTP_List.Element (1)));
      Status            : AWS.SMTP.Status;

   begin

      AWS.SMTP.Client.Send (Server  => SMTP,
                            From    => ES.From,
                            To      => Get_Recipients (ES),
                            Subject => To_String (ES.Subject),
                            Message => To_String (ES.Text_Part),
                            Status  => Status);

      if AWS.SMTP.Is_Ok (Status) then
         ES.Is_Email_Send := Yes;
      end if;

   end Send_Simple_Text_Only;

   ------------------------
   --  Set_Alternatives  --
   ------------------------

   procedure Set_Alternative_Parts (C  : in out AWS.Attachments.List;
                                    ES : in     Email_Structure)
   is

      Alternative_Parts : AWS.Attachments.Alternatives;

   begin

      --  Alternative parts.
      --  These are added with the least favorable part first and the most
      --  favorable part last.
      --  Alternative parts are simply just different versions of the same
      --  content, usually a text and a HTML version.
      --  If there a HTML part, then we favor that. Only when the HTML part is
      --  empty do we favor the Text part.
      if ES.HTML_Part = "" then
         AWS.Attachments.Add
           (Parts => Alternative_Parts,
            Data  => AWS.Attachments.Value
              (Data         => TS (ES.HTML_Part),
               Content_Type => AWS.MIME.Text_HTML &
               "; charset=" & TS (ES.Charset)));

         AWS.Attachments.Add
           (Parts => Alternative_Parts,
            Data  => AWS.Attachments.Value
              (Data         => TS (ES.Text_Part),
               Content_Type => AWS.MIME.Text_Plain &
               "; charset=" & TS (ES.Charset)));
      else
         AWS.Attachments.Add
           (Parts => Alternative_Parts,
            Data  => AWS.Attachments.Value
              (Data         => TS (ES.Text_Part),
               Content_Type => AWS.MIME.Text_Plain &
               "; charset=" & TS (ES.Charset)));

         AWS.Attachments.Add
           (Parts => Alternative_Parts,
            Data  => AWS.Attachments.Value
              (Data         => TS (ES.HTML_Part),
               Content_Type => AWS.MIME.Text_HTML &
               "; charset=" & TS (ES.Charset)));
      end if;

      AWS.Attachments.Add (Attachments => C,
                           Parts       => Alternative_Parts);

   end Set_Alternative_Parts;

   ----------------------------
   --  Set_File_Attachments  --
   ----------------------------

   procedure Set_File_Attachments (C   : in out AWS.Attachments.List;
                                   ES  : in     Email_Structure)
   is

      ESA : File_Attachments.Vector renames ES.Attachments_List;

   begin

      for i in ESA.First_Index .. ESA.Last_Index loop
         AWS.Attachments.Add
           (Attachments => C,
            Filename    => To_String (ESA.Element (i).File),
            Headers     => AWS.Headers.Empty_List,
            Name        => Simple_Name (To_String (ESA.Element (i).File)),
            Encode      => ESA.Element (i).Encode);
      end loop;
--        AWS.Attachments.Add (Attachments => C,
--                             Filename    => ES.Attachments_List,
--                             Headers     => AWS.Headers.Empty_List,
--                             Name        => "",
--                             Encode      => AWS.Attachments.Base64);
--
--        AWS.Attachments.Add (Attachments => C,
--                             Filename    => "test.lyx",
--                             Headers     => AWS.Headers.Empty_List,
--                             Name        => "",
--                             Encode      => AWS.Attachments.Base64);

   end Set_File_Attachments;

   ------------
   --  Send  --
   ------------

--     procedure Send (Parts         : in Alternative_Parts;
--                     From_Email    : in String;
--                     From_Name     : in String;
--                     To_Email      : in String;
--                     To_Name       : in String;
--                     Subject       : in String;
--                     Charset       : in String := "iso-8859-1";
--                     SMTP_Server   : in String;
--                     Success       : in out Boolean)
--     is
--     begin
--
--        Success := Send (Parts        => Parts,
--                         From_Email   => From_Email,
--                         From_Name    => From_Name,
--                         To_Email     => To_Email,
--                         To_Name      => To_Name,
--                         Subject      => Subject,
--                         Charset      => Charset,
--                         SMTP_Server  => SMTP_Server);
--
--     end Send;

end Simple_Email;
