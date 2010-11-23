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
--  sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
--  OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
--  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
--  for  more details.  You should have  received  a copy of the GNU General --
--  Public License  distributed with Yolk.  If not, write  to  the  Free     --
--  Software Foundation,  51  Franklin  Street,  Fifth  Floor, Boston,       --
--  MA 02110 - 1301, USA.                                                    --
--                                                                           --
-------------------------------------------------------------------------------
with Ada.Containers.Vectors;  use Ada.Containers;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with AWS.SMTP;
with GNATCOLL.Email;          use GNATCOLL.Email;
with GNATCOLL.VFS;            use GNATCOLL.VFS;

package Simple_Email is

   Attachment_File_Not_Found        : exception;
   --  Is raised if a file attachment is not found.
   No_Address_Component             : exception;
   --  Is raised if the address component of a recipient or sender is not
   --  found.
   No_SMTP_Host                     : exception;
   No_Sender_Set_With_Multiple_From : exception;

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
   type Email_Structure is private;
   type Recipient_Kind is (Bcc, Cc, To);

   procedure Add_File_Attachment
     (ES            : in out Email_Structure;
      Path_To_File  : in     String;
      Charset       : in     Character_Set := ISO_8859_1);

   procedure Add_From (ES        : in out Email_Structure;
                       Address   : in     String;
                       Name      : in     String := "";
                       Charset   : in     Character_Set := ISO_8859_1);

   procedure Add_Recipient
     (ES         : in out Email_Structure;
      Address    : in     String;
      Name       : in     String := "";
      Kind       : in     Recipient_Kind := To;
      Charset    : in     Character_Set := ISO_8859_1);

   procedure Add_Reply_To (ES       : in out Email_Structure;
                           Address  : in     String;
                           Name     : in     String := "";
                           Charset  : in     Character_Set := ISO_8859_1);

   procedure Add_SMTP_Server (ES    : in out Email_Structure;
                              Host  : in     String;
                              Port  : in     Positive := 25);

   function Is_Send (ES : in Email_Structure) return Boolean;

   procedure Send (ES : in out Email_Structure);

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

   procedure Set_HTML_Part
     (ES         : in out Email_Structure;
      Part       : in     String;
      Charset    : in     Character_Set := ISO_8859_1);

   procedure Set_Sender (ES         : in out Email_Structure;
                         Address    : in     String;
                         Name       : in     String := "";
                         Charset    : in     Character_Set := ISO_8859_1);

   procedure Set_Subject (ES        : in out Email_Structure;
                          Subject   : in     String;
                          Charset   : in     Character_Set := ISO_8859_1);

   procedure Set_Text_Part
     (ES         : in out Email_Structure;
      Part       : in     String;
      Charset    : in     Character_Set := ISO_8859_1);

   function Status_Code (ES : in Email_Structure) return Positive;

   function Status_Message (ES : in Email_Structure) return String;

private

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

   procedure Set_Type_Of_Email (ES : in out Email_Structure);

   function To_Virtual_File (Item : in Attachment_Data) return Virtual_File;

end Simple_Email;
