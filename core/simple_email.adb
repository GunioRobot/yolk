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

with Ada.Directories;   use Ada.Directories;
with Ada.Text_IO;       use Ada.Text_IO;
--  with AWS.Headers;
--  with AWS.MIME;
--  with AWS.Utils;
--  with AWS.SMTP.Client;

package body Simple_Email is

   ---------------------------
   --  Add_File_Attachment  --
   ---------------------------

   procedure Add_File_Attachment
     (ES            : in out Email_Structure;
      Path_To_File  : in     String;
      Charset       : in     Character_Set := ISO_8859_1)
   is

      New_Attachment : Attachment_Data;

   begin

      New_Attachment.Charset        := Charset;
      New_Attachment.Path_To_File   := TUS (Path_To_File);
      ES.Attachment_List.Append (New_Attachment);

      ES.Has_Attachment := True;

   end Add_File_Attachment;

   ----------------
   --  Add_From  --
   ----------------

   procedure Add_From (ES        : in out Email_Structure;
                       Address   : in     String;
                       Name      : in     String;
                       Charset   : in     Character_Set := ISO_8859_1)
   is

      New_From : Email_Data;

   begin

      New_From.Address  := TUS (Address);
      New_From.Charset  := Charset;
      New_From.Name     := TUS (Name);
      ES.From_List.Append (New_Item => New_From);

   end Add_From;

   ---------------------
   --  Add_Recipient  --
   ---------------------

   procedure Add_Recipient (ES       : in out Email_Structure;
                            Address  : in     String;
                            Name     : in     String;
                            Kind     : in     Recipient_Kind := To;
                            Charset  : in     Character_Set := ISO_8859_1)
   is

      New_Recipient : Email_Data;

   begin

      New_Recipient.Address   := TUS (Address);
      New_Recipient.Charset   := Charset;
      New_Recipient.Name      := TUS (Name);

      case Kind is
         when Bcc =>
            ES.Bcc_List.Append (New_Item => New_Recipient);
         when Cc =>
            ES.Cc_List.Append (New_Item => New_Recipient);
         when To =>
            ES.To_List.Append (New_Item => New_Recipient);
      end case;

   end Add_Recipient;

   --------------------
   --  Add_Reply_To  --
   --------------------

   procedure Add_Reply_To (ES       : in out Email_Structure;
                           Address  : in     String;
                           Name     : in     String;
                           Charset  : in     Character_Set := ISO_8859_1)
   is

      New_Reply_To : Email_Data;

   begin

      New_Reply_To.Address := TUS (Address);
      New_Reply_To.Charset := Charset;
      New_Reply_To.Name    := TUS (Name);
      ES.Reply_To_List.Append (New_Item => New_Reply_To);

   end Add_Reply_To;

   -----------------------
   --  Add_SMTP_Server  --
   -----------------------

   procedure Add_SMTP_Server (ES    : in out Email_Structure;
                              Host  : in     String;
                              Port  : in     Positive := 25)
   is

      New_SMTP : SMTP_Server;

   begin

      New_SMTP.Host := TUS (Host);
      New_SMTP.Port := Port;
      ES.SMTP_List.Append (New_Item => New_SMTP);

   end Add_SMTP_Server;

   ---------------
   --  Is_Send  --
   ---------------

   function Is_Send (ES : in Email_Structure) return Boolean
   is
   begin

      return ES.Email_Is_Send;

   end Is_Send;

   ------------
   --  Send  --
   ------------

   procedure Send (ES : in out Email_Structure)
   is
   begin

      Set_Type_Of_Email (ES => ES);

      case ES.Type_Of_Email is
         when Text =>
            Put_Line ("Text type");
         when Text_With_Attachment =>
            Put_Line ("Text_With_Attachment type");
         when Text_And_HTML =>
            Put_Line ("Text_And_HTML type");
         when Text_And_HTML_With_Attachment =>
            Put_Line ("Text_And_HTML_With_Attachment type");
      end case;

   end Send;

   ------------
   --  Send  --
   ------------

   procedure Send
     (ES             : in out Email_Structure;
      From_Address   : in     String;
      From_Name      : in     String;
      To_Address     : in     String;
      To_Name        : in     String;
      Subject        : in     String;
      Text_Part      : in     String;
      SMTP_Server    : in     String;
      SMTP_Port      : in     Positive := 25;
      Charset        : in     Character_Set := ISO_8859_1)
   is
   begin

      Add_From (ES      => ES,
                Address => From_Address,
                Name    => From_Name,
                Charset => Charset);

      Add_Recipient (ES      => ES,
                     Address => To_Address,
                     Name    => To_Name,
                     Kind    => To,
                     Charset => Charset);

      Set_Subject (ES      => ES,
                   Subject => Subject,
                   Charset => Charset);

      Set_Text_Part (ES      => ES,
                     Part    => Text_Part,
                     Charset => Charset);

      Add_SMTP_Server (ES   => ES,
                       Host => SMTP_Server,
                       Port => SMTP_Port);

      Send (ES => ES);

   end Send;

   ------------
   --  Send  --
   ------------

   procedure Send
     (ES             : in out Email_Structure;
      From_Address   : in     String;
      From_Name      : in     String;
      To_Address     : in     String;
      To_Name        : in     String;
      Subject        : in     String;
      Text_Part      : in     String;
      HTML_Part      : in     String;
      SMTP_Server    : in     String;
      SMTP_Port      : in     Positive := 25;
      Charset        : in     Character_Set := ISO_8859_1)
   is
   begin

      Add_From (ES      => ES,
                Address => From_Address,
                Name    => From_Name,
                Charset => Charset);

      Add_Recipient (ES      => ES,
                     Address => To_Address,
                     Name    => To_Name,
                     Kind    => To,
                     Charset => Charset);

      Set_Subject (ES      => ES,
                   Subject => Subject,
                   Charset => Charset);

      Set_Text_Part (ES      => ES,
                     Part    => Text_Part,
                     Charset => Charset);

      Set_HTML_Part (ES      => ES,
                     Part    => HTML_Part,
                     Charset => Charset);

      Add_SMTP_Server (ES   => ES,
                       Host => SMTP_Server,
                       Port => SMTP_Port);

      Send (ES => ES);

   end Send;

   ---------------------
   --  Set_HTML_Part  --
   ---------------------

   procedure Set_HTML_Part
     (ES         : in out Email_Structure;
      Part       : in     String;
      Charset    : in     Character_Set := ISO_8859_1)
   is
   begin

      ES.HTML_Part.Content := TUS (Part);
      ES.HTML_Part.Charset := Charset;

      ES.Has_HTML_Part := True;

   end Set_HTML_Part;

   ------------------
   --  Set_Sender  --
   ------------------

   procedure Set_Sender (ES         : in out Email_Structure;
                         Address    : in     String;
                         Name       : in     String;
                         Charset    : in     Character_Set := ISO_8859_1)
   is
   begin

      ES.Sender.Address := TUS (Address);
      ES.Sender.Charset := Charset;
      ES.Sender.Name    := TUS (Name);

   end Set_Sender;

   -------------------
   --  Set_Subject  --
   -------------------

   procedure Set_Subject (ES        : in out Email_Structure;
                          Subject   : in     String;
                          Charset   : in     Character_Set := ISO_8859_1)
   is
   begin

      ES.Subject.Content := TUS (Subject);
      ES.Subject.Charset := Charset;

   end Set_Subject;

   ---------------------
   --  Set_Text_Part  --
   ---------------------

   procedure Set_Text_Part
     (ES         : in out Email_Structure;
      Part       : in     String;
      Charset    : in     Character_Set := ISO_8859_1)
   is
   begin

      ES.Text_Part.Content := TUS (Part);
      ES.Text_Part.Charset := Charset;

      ES.Has_Text_Part := True;

   end Set_Text_Part;

   -------------------------
   --  Set_Type_Of_Email  --
   -------------------------

   procedure Set_Type_Of_Email (ES : in out Email_Structure)
   is
   begin

      if not ES.Has_Text_Part then
         Set_Text_Part (ES       => ES,
                        Part     => "");
      end if;
      ES.Type_Of_Email := Text;

      if ES.Has_HTML_Part then
         ES.Type_Of_Email := Text_And_HTML;
      end if;

      if ES.Has_Attachment then
         if ES.Type_Of_Email = Text then
            ES.Type_Of_Email := Text_With_Attachment;
         elsif ES.Type_Of_Email = Text_And_HTML then
            ES.Type_Of_Email := Text_And_HTML_With_Attachment;
         end if;
      end if;

   end Set_Type_Of_Email;

   -------------------
   --  Status_Code  --
   -------------------

   function Status_Code (ES : in Email_Structure) return Positive
   is
   begin

      return Positive (AWS.SMTP.Status_Code (Status => ES.Status));

   end Status_Code;

   ----------------------
   --  Status_Message  --
   ----------------------

   function Status_Message (ES : in Email_Structure) return String
   is
   begin

      return AWS.SMTP.Status_Message (Status => ES.Status);

   end Status_Message;

   -----------------------
   --  To_Virtual_File  --
   -----------------------

   function To_Virtual_File (Item : in Attachment_Data) return Virtual_File
   is

      Path_To_File : constant String := TS (Item.Path_To_File);

   begin

      if not Exists (Path_To_File) then
         raise Attachment_File_Not_Found;
      end if;

      return Locate_On_Path (Filesystem_String (Path_To_File));

   end To_Virtual_File;

   ------------------------------
   --  Send_Complex_Multipart  --
   ------------------------------

--     procedure Send_Complex_Multipart (ES : in out Email_Structure)
--     is
--
--        SMTP              : constant AWS.SMTP.Receiver
--          := AWS.SMTP.Client.Initialize (TS (ES.SMTP_List.Element (1)));
--        Email_Contents    : AWS.Attachments.List;
--        Status            : AWS.SMTP.Status;
--
--     begin
--
--        --  Add the alternative parts.
--        Set_Alternative_Parts (C   => Email_Contents,
--                               ES  => ES);
--
--        --  Add the file attachments.
--        Set_File_Attachments (C  => Email_Contents,
--                              ES => ES);
--
--        --  Send the email.
--        AWS.SMTP.Client.Send
--          (Server      => SMTP,
--           From        => ES.From,
--           To          => Get_Recipients (ES),
--           Subject     => To_String (ES.Subject),
--           Attachments => Email_Contents,
--           Status      => Status);
--
--        if AWS.SMTP.Is_Ok (Status) then
--           ES.Is_Email_Send := Yes;
--        end if;
--
--     end Send_Complex_Multipart;

   -----------------------------
   --  Send_Simple_Text_Only  --
   -----------------------------

--     procedure Send_Simple_Text_Only (ES : in out Email_Structure)
--     is
--
--        SMTP              : constant AWS.SMTP.Receiver
--          := AWS.SMTP.Client.Initialize (TS (ES.SMTP_List.Element (1)));
--        Status            : AWS.SMTP.Status;
--
--     begin
--
--        AWS.SMTP.Client.Send (Server  => SMTP,
--                              From    => ES.From,
--                              To      => Get_Recipients (ES),
--                              Subject => To_String (ES.Subject),
--                              Message => To_String (ES.Text_Part),
--                              Status  => Status);
--
--        if AWS.SMTP.Is_Ok (Status) then
--           ES.Is_Email_Send := Yes;
--        end if;
--
--     end Send_Simple_Text_Only;

   ------------------------
   --  Set_Alternatives  --
   ------------------------

--     procedure Set_Alternative_Parts (C  : in out AWS.Attachments.List;
--                                      ES : in     Email_Structure)
--     is
--
--        Alternative_Parts : AWS.Attachments.Alternatives;
--
--     begin
--
--        --  Alternative parts.
--        --  These are added with the least favorable part first and the most
--        --  favorable part last.
--        --  Alternative parts are simply just different versions of the same
--        --  content, usually a text and a HTML version.
--        --  If there a HTML part, then we favor that. Only when the HTML part
--        --  is empty do we favor the Text part.
--        if ES.HTML_Part = "" then
--           AWS.Attachments.Add
--             (Parts => Alternative_Parts,
--              Data  => AWS.Attachments.Value
--                (Data         => TS (ES.HTML_Part),
--                 Content_Type => AWS.MIME.Text_HTML &
--                 "; charset=" & TS (ES.Charset)));
--
--           AWS.Attachments.Add
--             (Parts => Alternative_Parts,
--              Data  => AWS.Attachments.Value
--                (Data         => TS (ES.Text_Part),
--                 Content_Type => AWS.MIME.Text_Plain &
--                 "; charset=" & TS (ES.Charset)));
--        else
--           AWS.Attachments.Add
--             (Parts => Alternative_Parts,
--              Data  => AWS.Attachments.Value
--                (Data         => TS (ES.Text_Part),
--                 Content_Type => AWS.MIME.Text_Plain &
--                 "; charset=" & TS (ES.Charset)));
--
--           AWS.Attachments.Add
--             (Parts => Alternative_Parts,
--              Data  => AWS.Attachments.Value
--                (Data         => TS (ES.HTML_Part),
--                 Content_Type => AWS.MIME.Text_HTML &
--                 "; charset=" & TS (ES.Charset)));
--        end if;
--
--        AWS.Attachments.Add (Attachments => C,
--                             Parts       => Alternative_Parts);
--
--     end Set_Alternative_Parts;

   ----------------------------
   --  Set_File_Attachments  --
   ----------------------------

--     procedure Set_File_Attachments (C   : in out AWS.Attachments.List;
--                                     ES  : in     Email_Structure)
--     is
--
--        ESA : File_Attachments.Vector renames ES.Attachments_List;
--
--     begin
--
--        for i in ESA.First_Index .. ESA.Last_Index loop
--           AWS.Attachments.Add
--             (Attachments => C,
--              Filename    => To_String (ESA.Element (i).File),
--              Headers     => AWS.Headers.Empty_List,
--              Name        => Simple_Name (To_String (ESA.Element (i).File)),
--              Encode      => ESA.Element (i).Encode);
--        end loop;
--  --        AWS.Attachments.Add (Attachments => C,
--  --                             Filename    => ES.Attachments_List,
--  --                             Headers     => AWS.Headers.Empty_List,
--  --                             Name        => "",
--  --                             Encode      => AWS.Attachments.Base64);
--  --
--  --        AWS.Attachments.Add (Attachments => C,
--  --                             Filename    => "test.lyx",
--  --                             Headers     => AWS.Headers.Empty_List,
--  --                             Name        => "",
--  --                             Encode      => AWS.Attachments.Base64);
--
--     end Set_File_Attachments;

end Simple_Email;
