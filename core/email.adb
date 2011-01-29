-------------------------------------------------------------------------------
--                                                                           --
--                                  Yolk                                     --
--                                                                           --
--                                  email                                    --
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
with Ada.Directories;
with Ada.Text_IO;
with GNATCOLL.Email;
with GNATCOLL.Email.Utils;
with Utilities;
--  with AWS.Headers;
--  with AWS.MIME;
--  with AWS.Utils;
--  with AWS.SMTP.Client;
--  with Utilities;         use Utilities;

package body Email is

   function Generate_Text_And_HTML_Email
     (ES : in Email_Structure)
      return Ada.Strings.Unbounded.Unbounded_String;
   --  Generate a text and HTML email using the GNATcoll email facilities.

   function Generate_Text_With_Attachment_Email
     (ES : in Email_Structure)
      return Ada.Strings.Unbounded.Unbounded_String;
   --  Generate a text email with attachment(s) using the GNATcoll email
   --  facilities.

   function Generate_Text_Email
     (ES : in Email_Structure)
      return Ada.Strings.Unbounded.Unbounded_String;
   --  Generate a text email using the GNATcoll email facilities.

   function Generate_Text_And_HTML_With_Attachment_Email
     (ES : in Email_Structure)
      return Ada.Strings.Unbounded.Unbounded_String;
   --  Generate a text and HTML email with attachment(s) using the GNATcoll
   --  email facilities.

   function Get_Charset
     (Charset : in Character_Set)
      return String;
   --  Return the GNATcoll.Email character set string constant that is
   --  equivalent to the given Email.Character_Set enum.

   ---------------------------
   --  Add_File_Attachment  --
   ---------------------------

   procedure Add_File_Attachment
     (ES            : in out Email_Structure;
      Path_To_File  : in     String;
      Charset       : in     Character_Set := US_ASCII)
   is

      use Utilities;

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

   procedure Add_From
     (ES        : in out Email_Structure;
      Address   : in     String;
      Name      : in     String := "";
      Charset   : in     Character_Set := US_ASCII)
   is

      use Utilities;

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

   procedure Add_Recipient
     (ES       : in out Email_Structure;
      Address  : in     String;
      Name     : in     String := "";
      Kind     : in     Recipient_Kind := To;
      Charset  : in     Character_Set := US_ASCII)
   is

      use Utilities;

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

   procedure Add_Reply_To
     (ES       : in out Email_Structure;
      Address  : in     String;
      Name     : in     String := "";
      Charset  : in     Character_Set := US_ASCII)
   is

      use Utilities;

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

   procedure Add_SMTP_Server
     (ES    : in out Email_Structure;
      Host  : in     String;
      Port  : in     Positive := 25)
   is

      use Utilities;

      New_SMTP : SMTP_Server;

   begin

      New_SMTP.Host := TUS (Host);
      New_SMTP.Port := Port;
      ES.SMTP_List.Append (New_Item => New_SMTP);

   end Add_SMTP_Server;

   ------------------------------------
   --  Generate_Text_And_HTML_Email  --
   ------------------------------------

   function Generate_Text_And_HTML_Email
     (ES : in Email_Structure)
      return Ada.Strings.Unbounded.Unbounded_String
   is

      pragma Unreferenced (ES);
      use Utilities;

   begin

      return TUS ("Text and HTML type");

   end Generate_Text_And_HTML_Email;

   ----------------------------------------------------
   --  Generate_Text_And_HTML_With_Attachment_Email  --
   ----------------------------------------------------

   function Generate_Text_And_HTML_With_Attachment_Email
     (ES : in Email_Structure)
      return Ada.Strings.Unbounded.Unbounded_String
   is

      pragma Unreferenced (ES);
      use Utilities;

   begin

      return TUS ("Text and HTML with attachment type");

   end Generate_Text_And_HTML_With_Attachment_Email;

   ---------------------------
   --  Generate_Text_Email  --
   ---------------------------

   function Generate_Text_Email
     (ES : in Email_Structure)
      return Ada.Strings.Unbounded.Unbounded_String
   is

      use GNATCOLL.Email;
      use GNATCOLL.Email.Utils;
      use Utilities;

      Email    : Message := New_Message (Text_Plain);
      Bcc      : Header;
      Cc       : Header;
      CT       : Header;
      CTE      : Header;
      Date     : Header;
      From     : Header;
      MIME     : Header;
      Reply_To : Header;
      Sender   : Header;
      To       : Header;
      Subject  : Header;
      US       : Unbounded_String   := Null_Unbounded_String;

   begin

      --  First we set the text payload.
      Email.Set_Text_Payload
        (Payload   => TS (ES.Text_Part.Content),
         Charset   => Get_Charset (ES.Text_Part.Charset));

      --  Add the Bcc: header
      if not ES.Bcc_List.Is_Empty then
         Bcc := Create (Name    => "Bcc",
                        Value   => "");

         for i in ES.Bcc_List.First_Index .. ES.Bcc_List.Last_Index loop
            declare

               Bcc_Data : constant Email_Data := ES.Bcc_List.Element (i);

            begin

               if Bcc_Data.Name = "" then
                  Bcc.Append (Value   => TS (Bcc_Data.Address));
               else
                  Bcc.Append (Value   => TS (Bcc_Data.Name),
                              Charset => Get_Charset (Bcc_Data.Charset));
                  Bcc.Append (Value => " <" & TS (Bcc_Data.Address) & ">");
               end if;

               if i /= ES.Bcc_List.Last_Index then
                  Bcc.Append (Value => ", ");
               end if;

            end;
         end loop;

         Email.Add_Header (H => Bcc);
      end if;

      --  Add the Cc: header
      if not ES.Cc_List.Is_Empty then
         Cc := Create (Name    => "Cc",
                       Value   => "");

         for i in ES.Cc_List.First_Index .. ES.Cc_List.Last_Index loop
            declare

               Cc_Data : constant Email_Data := ES.Cc_List.Element (i);

            begin

               if Cc_Data.Name = "" then
                  Cc.Append (Value   => TS (Cc_Data.Address));
               else
                  Cc.Append (Value   => TS (Cc_Data.Name),
                             Charset => Get_Charset (Cc_Data.Charset));
                  Cc.Append (Value => " <" & TS (Cc_Data.Address) & ">");
               end if;

               if i /= ES.Cc_List.Last_Index then
                  Cc.Append (Value => ", ");
               end if;

            end;
         end loop;

         Email.Add_Header (H => Cc);
      end if;

      --  Add the Date: header
      Date := Create (Name  => "Date",
                      Value => Format_Date (Date => Ada.Calendar.Clock));
      Email.Add_Header (H => Date);

      --  Add the From: header
      From := Create (Name    => "From",
                      Value   => "");
      for i in ES.From_List.First_Index .. ES.From_List.Last_Index loop
         declare

            From_Data : constant Email_Data := ES.From_List.Element (i);

         begin

            if From_Data.Name = "" then
               From.Append (Value => TS (From_Data.Address));
            else
               From.Append (Value   => TS (From_Data.Name),
                            Charset => Get_Charset (From_Data.Charset));
               From.Append (Value => " <" & TS (From_Data.Address) & ">");
            end if;

            if i /= ES.From_List.Last_Index then
               From.Append (Value => ", ");
            end if;

         end;
      end loop;

      Email.Add_Header (H => From);

      --  Add the Reply-To: header
      if not ES.Reply_To_List.Is_Empty then
         Reply_To := Create (Name    => "Reply-To",
                             Value   => "");
         for i in
           ES.Reply_To_List.First_Index .. ES.Reply_To_List.Last_Index loop
            declare

               Reply_To_Data : constant Email_Data
                 := ES.Reply_To_List.Element (i);

            begin

               if Reply_To_Data.Name = "" then
                  Reply_To.Append (Value => TS (Reply_To_Data.Address));
               else
                  Reply_To.Append
                    (Value   => TS (Reply_To_Data.Name),
                     Charset => Get_Charset (Reply_To_Data.Charset));
                  Reply_To.Append
                    (Value => " <" & TS (Reply_To_Data.Address) & ">");
               end if;

               if i /= ES.Reply_To_List.Last_Index then
                  Reply_To.Append (Value => ", ");
               end if;

            end;
         end loop;

         Email.Add_Header (H => Reply_To);
      end if;

      --  Add the Sender: header
      if ES.Sender.Address /= Null_Unbounded_String then
         if ES.Sender.Name = "" then
            Sender := Create (Name    => "Sender",
                              Value   => TS (ES.Sender.Address),
                              Charset => Get_Charset (ES.Sender.Charset));
         else
            Sender := Create (Name    => "Sender",
                              Value   => TS (ES.Sender.Name),
                              Charset => Get_Charset (ES.Sender.Charset));
            Sender.Append
              (Value   => " <" & TS (ES.Sender.Address) & ">");
         end if;

         Email.Add_Header (H => Sender);
      end if;

      --  Add the To: header
      To := Create (Name    => "To",
                    Value   => "");
      for i in ES.To_List.First_Index .. ES.To_List.Last_Index loop
         declare

            To_Data : constant Email_Data := ES.To_List.Element (i);

         begin

            if To_Data.Name = "" then
               To.Append (Value => TS (To_Data.Address));
            else
               To.Append (Value   => TS (To_Data.Name),
                          Charset => Get_Charset (To_Data.Charset));
               To.Append (Value => " <" & TS (To_Data.Address) & ">");
            end if;

            if i /= ES.To_List.Last_Index then
               To.Append (Value => ", ");
            end if;

         end;
      end loop;

      Email.Add_Header (H => To);

      --  Add the MIME-Version: header.
      MIME := Create (Name    => MIME_Version,
                      Value   => "1.0");
      Email.Add_Header (H => MIME);

      --  Add the Subject: header
      Subject := Create (Name    => "Subject",
                         Value   => TS (ES.Subject.Content),
                         Charset => Get_Charset (ES.Subject.Charset));
      Email.Add_Header (H => Subject);

      --  Add the Content-Type: header
      CT := Create (Name    => Content_Type,
                    Value   => Text_Plain);
      CT.Set_Param (Param_Name  => "charset",
                    Param_Value => Get_Charset (ES.Text_Part.Charset));
      Email.Add_Header (H => CT);

      --  Add the Content-Transfer-Encoding: header
      CTE := Create (Name    => Content_Transfer_Encoding,
                     Value   => "8bit");
      Email.Add_Header (H => CTE);

      --  Finally we output the raw email.
      Email.To_String (Result => US);

      return US;

   end Generate_Text_Email;

   -------------------------------------------
   --  Generate_Text_With_Attachment_Email  --
   -------------------------------------------

   function Generate_Text_With_Attachment_Email
     (ES : in Email_Structure)
      return Ada.Strings.Unbounded.Unbounded_String
   is

      pragma Unreferenced (ES);
      use Utilities;

   begin

      return TUS ("Text with attachment type");

   end Generate_Text_With_Attachment_Email;

   -------------------
   --  Get_Charset  --
   -------------------

   function Get_Charset
     (Charset : in Character_Set)
      return String
   is
   begin

      case Charset is
         when US_ASCII => return GNATCOLL.Email.Charset_US_ASCII;
         when ISO_8859_1 => return GNATCOLL.Email.Charset_ISO_8859_1;
         when ISO_8859_2 => return GNATCOLL.Email.Charset_ISO_8859_2;
         when ISO_8859_3 => return GNATCOLL.Email.Charset_ISO_8859_3;
         when ISO_8859_4 => return GNATCOLL.Email.Charset_ISO_8859_4;
         when ISO_8859_9 => return GNATCOLL.Email.Charset_ISO_8859_9;
         when ISO_8859_10 => return GNATCOLL.Email.Charset_ISO_8859_10;
         when ISO_8859_13 => return GNATCOLL.Email.Charset_ISO_8859_13;
         when ISO_8859_14 => return GNATCOLL.Email.Charset_ISO_8859_14;
         when ISO_8859_15 => return GNATCOLL.Email.Charset_ISO_8859_15;
         when Windows_1252 => return GNATCOLL.Email.Charset_Windows_1252;
         when UTF8 => return "utf-8";
      end case;

   end Get_Charset;

   ---------------
   --  Is_Send  --
   ---------------

   function Is_Send
     (ES : in Email_Structure)
      return Boolean
   is
   begin

      return ES.Email_Is_Send;

   end Is_Send;

   ------------
   --  Send  --
   ------------

   procedure Send
     (ES : in out Email_Structure)
   is

      use Ada.Text_IO;
      use Utilities;

      Email_String : Unbounded_String := Null_Unbounded_String;

   begin

      Set_Type_Of_Email (ES => ES);

      case ES.Type_Of_Email is
         when Text =>
            Email_String := Generate_Text_Email (ES);
         when Text_With_Attachment =>
            Email_String := Generate_Text_With_Attachment_Email (ES);
         when Text_And_HTML =>
            Email_String := Generate_Text_And_HTML_Email (ES);
         when Text_And_HTML_With_Attachment =>
            Email_String := Generate_Text_And_HTML_With_Attachment_Email (ES);
      end case;

      Put_Line (TS (Email_String));

   end Send;

   ------------
   --  Send  --
   ------------

   procedure Send
     (ES             : in out Email_Structure;
      From_Address   : in     String;
      From_Name      : in     String := "";
      To_Address     : in     String;
      To_Name        : in     String := "";
      Subject        : in     String;
      Text_Part      : in     String;
      SMTP_Server    : in     String;
      SMTP_Port      : in     Positive := 25;
      Charset        : in     Character_Set := US_ASCII)
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
      From_Name      : in     String := "";
      To_Address     : in     String;
      To_Name        : in     String := "";
      Subject        : in     String;
      Text_Part      : in     String;
      HTML_Part      : in     String;
      SMTP_Server    : in     String;
      SMTP_Port      : in     Positive := 25;
      Charset        : in     Character_Set := US_ASCII)
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
      Charset    : in     Character_Set := US_ASCII)
   is

      use Utilities;

   begin

      ES.HTML_Part.Content := TUS (Part);
      ES.HTML_Part.Charset := Charset;

      ES.Has_HTML_Part := True;

   end Set_HTML_Part;

   ------------------
   --  Set_Sender  --
   ------------------

   procedure Set_Sender
     (ES         : in out Email_Structure;
      Address    : in     String;
      Name       : in     String := "";
      Charset    : in     Character_Set := US_ASCII)
   is

      use Utilities;

   begin

      ES.Sender.Address := TUS (Address);
      ES.Sender.Charset := Charset;
      ES.Sender.Name    := TUS (Name);

   end Set_Sender;

   -------------------
   --  Set_Subject  --
   -------------------

   procedure Set_Subject
     (ES        : in out Email_Structure;
      Subject   : in     String;
      Charset   : in     Character_Set := US_ASCII)
   is

      use Utilities;

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
      Charset    : in     Character_Set := US_ASCII)
   is

      use Utilities;

   begin

      ES.Text_Part.Content := TUS (Part);
      ES.Text_Part.Charset := Charset;

      ES.Has_Text_Part := True;

   end Set_Text_Part;

   -------------------------
   --  Set_Type_Of_Email  --
   -------------------------

   procedure Set_Type_Of_Email
     (ES : in out Email_Structure)
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

   function Status_Code
     (ES : in Email_Structure)
      return Positive
   is
   begin

      return Positive (AWS.SMTP.Status_Code (Status => ES.Status));

   end Status_Code;

   ----------------------
   --  Status_Message  --
   ----------------------

   function Status_Message
     (ES : in Email_Structure)
      return String
   is
   begin

      return AWS.SMTP.Status_Message (Status => ES.Status);

   end Status_Message;

   -----------------------
   --  To_Virtual_File  --
   -----------------------

   function To_Virtual_File
     (Item : in Attachment_Data)
      return Virtual_File
   is

      use Ada.Directories;
      use Utilities;

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

end Email;
