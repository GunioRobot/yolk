-------------------------------------------------------------------------------
--                                                                           --
--                                  Yolk                                     --
--                                                                           --
--                                  Email                                    --
--                                                                           --
--                                  SPEC                                     --
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

with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;
with GNATCOLL.Email;
with GNATCOLL.VFS;

package Yolk.Email is

   Attachment_File_Not_Found        : exception;
   --  Is raised if a file attachment is not found.
   No_Address_Set                   : exception;
   --  Is raised if the address component is missing in an Email_Data record.
   No_Sender_Set_With_Multiple_From : exception;
   --  Is raised when an email contains multiple From headers but no Sender
   --  header, as per RFC-5322, 3.6.2. http://tools.ietf.org/html/rfc5322
   No_SMTP_Host_Set                 : exception;
   --  Is raised if the SMTP host list is empty.

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
                          Windows_1252,
                          UTF8);
   --  The available character sets. We try to provide the same character sets
   --  as defined in gnatcoll-email.ads.
   --  Note:
   --    gnatcoll-email does not support UTF8, so this has been added by me
   --    specifically.

   type Recipient_Kind is (Bcc, Cc, To);
   --  The kind of recipient, when adding a new recipient to an email.

   type Structure is private;
   --  The email structure. This type holds all the information needed to build
   --  a proper email.

private

   use Ada.Containers;
   use Ada.Strings.Unbounded;

   type Attachment_Data is
      record
         Charset        : Character_Set := US_ASCII;
         Path_To_File   : Unbounded_String;
      end record;

   type Email_Data is
      record
         Address  : Unbounded_String;
         Charset  : Character_Set  := US_ASCII;
         Name     : Unbounded_String;
   end record;

   type Email_Kind is (Text,
                       Text_With_Attachment,
                       Text_And_HTML,
                       Text_And_HTML_With_Attachment);

   type Header_Data is
      record
         Charset : Character_Set := US_ASCII;
         Name    : Unbounded_String;
         Value   : Unbounded_String;
      end record;

   type SMTP_Server is
      record
         Host : Unbounded_String;
         Port : Positive;
      end record;

   type Subject_Data is
      record
         Content : Unbounded_String;
         Charset : Character_Set := US_ASCII;
      end record;

   type Text_Data is
      record
         Content  : Unbounded_String;
         Charset  : Character_Set := US_ASCII;
      end record;
   --  This type is used for both text and HTML parts, so the "Text" part of
   --  Text_Data simply refers to the fact that both text and HTML parts are
   --  essentially plain text data.

   package Attachments_Container is new Vectors (Positive, Attachment_Data);
   package Custom_Headers_Container is new Vectors (Positive, Header_Data);
   package Email_Data_Container is new Vectors (Positive, Email_Data);
   package SMTP_Servers_Container is new Vectors (Positive, SMTP_Server);

   type Structure is
      record
         Attachment_List   : Attachments_Container.Vector;
         Bcc_List          : Email_Data_Container.Vector;
         Cc_List           : Email_Data_Container.Vector;
         Composed_Message  : GNATCOLL.Email.Message;
         Custom_Headers    : Custom_Headers_Container.Vector;
         Email_Is_Send     : Boolean := False;
         From_List         : Email_Data_Container.Vector;
         Has_Attachment    : Boolean := False;
         Has_HTML_Part     : Boolean := False;
         Has_Text_Part     : Boolean := False;
         HTML_Part         : Text_Data;
         Reply_To_List     : Email_Data_Container.Vector;
         Sender            : Email_Data;
         SMTP_List         : SMTP_Servers_Container.Vector;
         Subject           : Subject_Data;
         Text_Part         : Text_Data;
         To_List           : Email_Data_Container.Vector;
         Type_Of_Email     : Email_Kind;
      end record;
   --  The type used to hold describe an email.
   --    Attachment_List:
   --       A list of Attachment_Data records. The validity of the Path_To_File
   --       component is checked when it is converted into a GNATcoll Virtual
   --       file.
   --    Bcc_List:
   --       A list of Email_Data records. These are collapsed into a single
   --       Bcc: header when Send is called, and only then do we check if each
   --       element is valid.
   --    Cc_List:
   --       A list of Email_Data records. These are collapsed into a single Cc:
   --       header when Send is called, and only then do we check if each
   --       element is valid.
   --    Composed_Message:
   --       The complete email in GNATCOLL.Email.Message format.
   --    Custom_Headers:
   --       A list of custom headers.
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

   procedure Build_Attachments
     (ES    : in     Structure;
      Email : in out GNATCOLL.Email.Message);
   --  Add attachments to Email.

   procedure Build_Bcc_Header
     (ES    : in     Structure;
      Email : in out GNATCOLL.Email.Message);
   --  Build the Bcc header and add it to Email.

   procedure Build_Cc_Header
     (ES    : in     Structure;
      Email : in out GNATCOLL.Email.Message);
   --  Build the Cc header and add it to Email.

   procedure Build_Content_Transfer_Encoding_Header
     (Charset : in     Character_Set;
      Email   : in out GNATCOLL.Email.Message);
   --  Build the Content-Transfer-Encoding header and add it to Email.

   procedure Build_Content_Type_Header
     (ES    : in     Structure;
      Email : in out GNATCOLL.Email.Message;
      Kind  : in String);
   --  Build the Content-Type header and add it to Email.

   procedure Build_Custom_Headers
     (ES    : in     Structure;
      Email : in out GNATCOLL.Email.Message);
   --  Build the custom headers. Custom headers are usually things like
   --  User-Agent, Organization or X - headers.

   procedure Build_Date_Header
     (Email : in out GNATCOLL.Email.Message);
   --  Build the Date header and add it to Email.

   procedure Build_Email_Data
     (Header   : in out GNATCOLL.Email.Header;
      List     : in     Email_Data_Container.Vector);
   --  Construct the actual content for the sender/recipient headers, such as
   --  To, Cc, Bcc, Reply-To and so on.

   procedure Build_From_Header
     (ES    : in     Structure;
      Email : in out GNATCOLL.Email.Message);
   --  Build the From header and add it to Email.

   procedure Build_General_Headers
     (ES    : in     Structure;
      Email : in out GNATCOLL.Email.Message);
   --  Add the general headers such as To, From, Date and so on, to the Email.

   procedure Build_MIME_Header
     (Email : in out GNATCOLL.Email.Message);
   --  Build the MIME-Version header and add it to Email.

   procedure Build_Reply_To_Header
     (ES    : in     Structure;
      Email : in out GNATCOLL.Email.Message);
   --  Build the Reply-To header and add it to Email.

   procedure Build_Sender_Header
     (ES    : in     Structure;
      Email : in out GNATCOLL.Email.Message);
   --  Build the Sender header and add it to Email.

   procedure Build_Subject_Header
     (ES    : in     Structure;
      Email : in out GNATCOLL.Email.Message);
   --  Build the Subject header and add it to Email.

   procedure Build_To_Header
     (ES    : in     Structure;
      Email : in out GNATCOLL.Email.Message);
   --  Build the To header and add it to Email.

   procedure Generate_Text_And_HTML_Email
     (ES : in out Structure);
   --  Generate a text and HTML email using the GNATcoll email facilities.

   procedure Generate_Text_With_Attachment_Email
     (ES : in out Structure);
   --  Generate a text email with attachment(s) using the GNATcoll email
   --  facilities.

   procedure Generate_Text_Email
     (ES : in out Structure);
   --  Generate a text email using the GNATcoll email facilities.

   procedure Generate_Text_And_HTML_With_Attachment_Email
     (ES : in out Structure);
   --  Generate a text and HTML email with attachment(s) using the GNATcoll
   --  email facilities.

   function Get_Charset
     (Charset : in Character_Set)
      return String;
   --  Return the GNATcoll.Email character set string constant that is
   --  equivalent to the given Email.Character_Set enum.

   procedure Set_Type_Of_Email
     (ES : in out Structure);
   --  Figure out the kind of email ES is.

   function To_Virtual_File
     (Item : in Attachment_Data)
      return GNATCOLL.VFS.Virtual_File;
   --  Convert an Attachment_Data.Path_To_File to a GNATCOLL.VFS Virtual_File.
   --  Exceptions:
   --    Attachment_File_Not_Found

end Yolk.Email;
