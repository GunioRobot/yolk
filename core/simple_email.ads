-------------------------------------------------------------------------------
--                                                                           --
--                                  Yolk                                     --
--                                                                           --
--                              simple_email                                 --
--                                                                           --
--                                  SPEC                                     --
--                                                                           --
--                     Copyright (C) 2010, Thomas Løcke                      --
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

with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;
with AWS.SMTP;
with GNATCOLL.VFS;

package Simple_Email is

   Attachment_File_Not_Found        : exception;
   --  Is raised if a file attachment is not found.
   No_Address_Set                   : exception;
   --  Is raised if the address component is missing for a sender/recipient.
   No_Sender_Set_With_Multiple_From : exception;
   --  Is raised when an email contains multiple From headers but no Sender
   --  header, as per RFC-5322, 3.6.2. http://tools.ietf.org/html/rfc5322
   No_SMTP_Host_Set                 : exception;
   --  Is raised if the SMTP host is empty.

   type Character_Set is (US_ASCII,
                          ISO_8859_1,
                          ISO_8859_2,
                          ISO_8859_3,
                          ISO_8859_4,
                          ISO_8859_9,
                          ISO_8859_10,
                          ISO_8859_13,
                          ISO_8859_14,
                          ISO_8859_15,
                          Windows_1252);
   --  The available character sets. We try to provide the same character sets
   --  as defined in gnatcoll-emails.ads

   type Email_Structure is private;

   type Recipient_Kind is (Bcc, Cc, To);
   --  The kind of recipient, when adding a new recipient to an email.

   procedure Add_File_Attachment
     (ES            : in out Email_Structure;
      Path_To_File  : in     String;
      Charset       : in     Character_Set := ISO_8859_1);
   --  Add a file attachment to the ES email object. These are _always_ BASE64
   --  encoded. At this point we do not check whether the file actually exists,
   --  so anything can be added, even an empty String Path_To_File.

   procedure Add_From (ES        : in out Email_Structure;
                       Address   : in     String;
                       Name      : in     String := "";
                       Charset   : in     Character_Set := ISO_8859_1);
   --  Add a From mailbox to the email. If multiple From mailboxes are added,
   --  then a subsequent call to Set_Sender is required, as per RFC 5322.

   procedure Add_Recipient
     (ES         : in out Email_Structure;
      Address    : in     String;
      Name       : in     String := "";
      Kind       : in     Recipient_Kind := To;
      Charset    : in     Character_Set := ISO_8859_1);
   --  Add a recipient to the email.

   procedure Add_Reply_To (ES       : in out Email_Structure;
                           Address  : in     String;
                           Name     : in     String := "";
                           Charset  : in     Character_Set := ISO_8859_1);
   --  Reply-To indicates the address(es) to which the author of the message
   --  suggests that replies be sent. In the absence of Reply-To, the default
   --  is to send replies to the From mailboxes.

   procedure Add_SMTP_Server (ES    : in out Email_Structure;
                              Host  : in     String;
                              Port  : in     Positive := 25);
   --  Set the SMTP servers to use when sending an email. The first server
   --  added is the first server tried. If the first server fails, then the
   --  system moves on to the next server, until it either runs out of SMTP
   --  servers to try, or it manages to send the email.

   function Is_Send (ES : in Email_Structure) return Boolean;
   --  Return True if the email has been successfully delivered to one of the
   --  set SMTP servers. False otherwise.

   procedure Send (ES : in out Email_Structure);
   --  Process the ES object. This entails composing the email source and
   --  sending it via one of the set SMTP servers.
   --  Exceptions:
   --    Attachment_File_Not_Found
   --    No_Address_Set
   --    No_Sender_Set_With_Multiple_From
   --    No_SMTP_Host_Set

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
      Charset        : in     Character_Set := ISO_8859_1);
   --  Convenience wrapper for Send (ES : in out Email_Structure) for Text only
   --  emails.
   --  Exceptions raised by the "parent" Send procdure are passively propagated
   --  up through the call stack to the caller of this Send procedure.

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
      Charset        : in     Character_Set := ISO_8859_1);
   --  Convenience wrapper for Send (ES : in out Email_Structure) for Text and
   --  HTML multipart emails.
   --  Exceptions raised by the "parent" Send procdure are passively propagated
   --  up through the call stack to the caller of this Send procedure.

   procedure Set_HTML_Part
     (ES         : in out Email_Structure;
      Part       : in     String;
      Charset    : in     Character_Set := ISO_8859_1);
   --  When adding a HTML part to an email, it is automatically converted to
   --  a multipart message. If no Text part is added, an empty one will be
   --  created automatically.

   procedure Set_Sender (ES         : in out Email_Structure;
                         Address    : in     String;
                         Name       : in     String := "";
                         Charset    : in     Character_Set := ISO_8859_1);
   --  If an email has multiple From addresses, then it is required, as per
   --  RFC 5322, to set a single Sender.

   procedure Set_Subject (ES        : in out Email_Structure;
                          Subject   : in     String;
                          Charset   : in     Character_Set := ISO_8859_1);
   --  Add a Subject to the email.

   procedure Set_Text_Part
     (ES         : in out Email_Structure;
      Part       : in     String;
      Charset    : in     Character_Set := ISO_8859_1);
   --  Add the Text part to an email.

   function Status_Code (ES : in Email_Structure) return Positive;
   --  The SMTP status code. See AWS.SMTP.Reply_Code for the range of possible
   --  values.

   function Status_Message (ES : in Email_Structure) return String;
   --  The SMTP error message if Is_Send is False.

private

   use Ada.Containers;
   use Ada.Strings.Unbounded;
   --  use GNATCOLL.Email;
   use GNATCOLL.VFS;

   function TS (US : Unbounded_String) return String renames To_String;
   function TUS (S : String) return Unbounded_String
                 renames To_Unbounded_String;

   type Attachment_Data is record
      Charset        : Character_Set;
      Path_To_File   : Unbounded_String;
   end record;

   type Email_Data is record
      Address  : Unbounded_String;
      Charset  : Character_Set;
      Name     : Unbounded_String;
   end record;

   type Email_Kind is (Text,
                       Text_With_Attachment,
                       Text_And_HTML,
                       Text_And_HTML_With_Attachment);

   type SMTP_Server is record
      Host : Unbounded_String;
      Port : Positive;
   end record;

   type Subject_Data is record
      Content : Unbounded_String;
      Charset : Character_Set;
   end record;

   type Text_Data is record
      Content  : Unbounded_String;
      Charset  : Character_Set;
   end record;
   --  This type is used for both text and HTML parts, so the "Text" part of
   --  Text_Data simply refers to the fact that both text and HTML parts are
   --  essentially plain text data.

   package Attachments_Container is new Vectors (Positive, Attachment_Data);
   package Email_Data_Container is new Vectors (Positive, Email_Data);
   package SMTP_Servers_Container is new Vectors (Positive, SMTP_Server);

   type Email_Structure is record
      Attachment_List   : Attachments_Container.Vector;
      Bcc_List          : Email_Data_Container.Vector;
      Cc_List           : Email_Data_Container.Vector;
      Email_Is_Send     : Boolean := False;
      From_List         : Email_Data_Container.Vector;
      Has_Attachment    : Boolean := False;
      Has_HTML_Part     : Boolean := False;
      Has_Text_Part     : Boolean := False;
      HTML_Part         : Text_Data;
      Reply_To_List     : Email_Data_Container.Vector;
      Sender            : Email_Data;
      SMTP_List         : SMTP_Servers_Container.Vector;
      Status            : AWS.SMTP.Status;
      Subject           : Subject_Data;
      Text_Part         : Text_Data;
      To_List           : Email_Data_Container.Vector;
      Type_Of_Email     : Email_Kind;
   end record;
   --  The type used to hold describe an email.
   --    Attachment_List:
   --       A list of Attachment_Data records. The validity of the Path_To_File
   --       component is checked when Send is called.
   --    Bcc_List:
   --       A list of Email_Data records. These are collapsed into a single
   --       Bcc: header when Send is called, and only then do we check if each
   --       element is valid.
   --    Cc_List:
   --       A list of Email_Data records. These are collapsed into a single Cc:
   --       header when Send is called, and only then do we check if each
   --       element is valid.
   --    Email_Is_Send:
   --       Is set to True if we succeed in sending the email.
   --    From_List:
   --       A list of Email_Data records. These are collapsed into a single
   --       From: header when Send is called, and only then do we check if each
   --       element is valid.
   --    Has_Attachment:
   --       Is set to True if an attachment is added to the email.
   --    Has_HTML_Part:
   --       Is set to True if a HTML part is added to the email.
   --    Has_Text_Part:
   --       Is set to True if a Text part is added to the email.
   --    HTML_Part:
   --       The HTML part of a multipart/alternative email.
   --    Reply_To_List:
   --       List of Email_Data records. These are collapsed into a single
   --       Reply-To: header when Send is called, and only then do we check if
   --       each element is valid.
   --    Sender:
   --       If From_List contains multiple elements, then a Sender: header is
   --       required as per RFC 5322 3.6.2. This header is build from the value
   --       of Sender.
   --    SMTP_List:
   --       List of SMTP servers to try when sending the email. The first one
   --       added to the list, is the first one tried. The system will keep
   --       going down the list, until it either succeeds in sending the email
   --       or until it runs out of SMTP servers to try.
   --    Status:
   --       The status code and message from the SMTP session.
   --    Subject:
   --       From this we build the Subject: header.
   --    Text_Part:
   --       The text/plain part of an email.
   --    To_List:
   --       List of Email_Data records. These are collapsed into a single To:
   --       header when send is called, and only then do we check if each
   --       element is valid.
   --    Type_Of_Email:
   --       The kind of email we're dealing with.

   procedure Set_Type_Of_Email (ES : in out Email_Structure);
   --  Figure out the kind of email ES is.

   function To_Virtual_File (Item : in Attachment_Data) return Virtual_File;
   --  Convert an Attachment_Data.Path_To_File to a GNATCOLL.VFS Virtual_File.
   --  Exceptions:
   --    Attachment_File_Not_Found

end Simple_Email;
