-------------------------------------------------------------------------------
--                                                                           --
--                                  Yolk                                     --
--                                                                           --
--                           Yolk.Email_Composer                             --
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

package Yolk.Email.Composer is

   procedure Add_Custom_Header
     (ES      : in out Structure;
      Name    : in     String;
      Value   : in     String;
      Charset : in     Character_Set := US_ASCII);
   --  Add a custom header to the ES email object. Custom headers are usually
   --  things like User-Agent, Organization or X- headers.

   procedure Add_File_Attachment
     (ES            : in out Structure;
      Path_To_File  : in     String;
      Charset       : in     Character_Set := US_ASCII);
   --  Add a file attachment to the ES email object. These are _always_ BASE64
   --  encoded. At this point we do not check whether the file actually exists,
   --  so anything can be added, even an empty String Path_To_File.

   procedure Add_From
     (ES        : in out Structure;
      Address   : in     String;
      Name      : in     String := "";
      Charset   : in     Character_Set := US_ASCII);
   --  Add a From mailbox to the email. If multiple From mailboxes are added,
   --  then a subsequent call to Set_Sender is required, as per RFC 5322.

   procedure Add_Recipient
     (ES         : in out Structure;
      Address    : in     String;
      Name       : in     String := "";
      Kind       : in     Recipient_Kind := To;
      Charset    : in     Character_Set := US_ASCII);
   --  Add a recipient to the email.

   procedure Add_Reply_To
     (ES       : in out Structure;
      Address  : in     String;
      Name     : in     String := "";
      Charset  : in     Character_Set := US_ASCII);
   --  Reply-To indicates the address(es) to which the author of the message
   --  suggests that replies be sent. In the absence of Reply-To, the default
   --  is to send replies to the From mailboxes.

   procedure Add_SMTP_Server
     (ES    : in out Structure;
      Host  : in     String;
      Port  : in     Positive := 25);
   --  Set the SMTP servers to use when sending an email. The first server
   --  added is the first server tried. If the first server fails, then the
   --  system moves on to the next server, until it either runs out of SMTP
   --  servers to try, or it manages to send the email.

   function Is_Send
     (ES : in Structure)
      return Boolean;
   --  Return True if the email has been successfully delivered to one of the
   --  set SMTP servers. False otherwise.

   procedure Send
     (ES : in out Structure);
   --  Process the ES object. This entails composing the email source and
   --  sending it via one of the set SMTP servers.
   --  Exceptions:
   --    Attachment_File_Not_Found
   --    No_Address_Set
   --    No_Sender_Set_With_Multiple_From
   --    No_SMTP_Host_Set

   procedure Send
     (ES             : in out Structure;
      From_Address   : in     String;
      From_Name      : in     String := "";
      To_Address     : in     String;
      To_Name        : in     String := "";
      Subject        : in     String;
      Text_Part      : in     String;
      SMTP_Server    : in     String := "localhost";
      SMTP_Port      : in     Positive := 25;
      Charset        : in     Character_Set := US_ASCII);
   --  Convenience wrapper for Send (ES : in out Email_Structure) for Text only
   --  emails.
   --  Exceptions raised by the "parent" Send procdure are passively propagated
   --  up through the call stack to the caller of this Send procedure.

   procedure Send
     (ES             : in out Structure;
      From_Address   : in     String;
      From_Name      : in     String := "";
      To_Address     : in     String;
      To_Name        : in     String := "";
      Subject        : in     String;
      Text_Part      : in     String;
      HTML_Part      : in     String;
      SMTP_Server    : in     String := "localhost";
      SMTP_Port      : in     Positive := 25;
      Charset        : in     Character_Set := US_ASCII);
   --  Convenience wrapper for Send (ES : in out Email_Structure) for Text and
   --  HTML multipart emails.
   --  Exceptions raised by the "parent" Send procdure are passively propagated
   --  up through the call stack to the caller of this Send procedure.

   procedure Set_HTML_Part
     (ES         : in out Structure;
      Part       : in     String;
      Charset    : in     Character_Set := US_ASCII);
   --  When adding a HTML part to an email, it is automatically converted to
   --  a multipart message. If no Text part is added, an empty one will be
   --  created automatically.

   procedure Set_Sender
     (ES         : in out Structure;
      Address    : in     String;
      Name       : in     String := "";
      Charset    : in     Character_Set := US_ASCII);
   --  If an email has multiple From addresses, then it is required, as per
   --  RFC 5322, to set a single Sender.

   procedure Set_Subject
     (ES        : in out Structure;
      Subject   : in     String;
      Charset   : in     Character_Set := US_ASCII);
   --  Add a Subject to the email.

   procedure Set_Text_Part
     (ES         : in out Structure;
      Part       : in     String;
      Charset    : in     Character_Set := US_ASCII);
   --  Add the Text part to an email.

end Yolk.Email.Composer;
