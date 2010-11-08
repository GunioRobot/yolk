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
with AWS.Attachments;
with AWS.SMTP;                use type AWS.SMTP.E_Mail_Data;

package Simple_Email is

   Attachment_File_Not_Found : exception;

   type Email_Structure is private;
   type Encoding is (None, Base64);

   function TS (US : Unbounded_String) return String renames To_String;

   procedure Add_Attachment (ES                 : in out Email_Structure;
                             Attach_File        : in     String;
                             Encode_Attachment  : in     Encoding := Base64);

   procedure Add_Recipient (ES         : in out Email_Structure;
                            To_Address : in     String;
                            To_Name    : in     String);

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
                           SMTP_Server       : in     String := "localhost");

   function Is_Send (ES : in Email_Structure) return Boolean;

   procedure Send (ES : in out Email_Structure);
   --  Compose and send a multipart email messages. This is for simple messages
   --  only. Attachments are not supported. If you need such "fancy" things
   --  then AWS.SMTP is what you're looking for.
   --  NOTE:
   --    The Charset string should of course match the encoding used in the
   --    HTML_Part and Text_Part.

--     procedure Send (Parts         : in Alternative_Parts;
--                     From_Email    : in String;
--                     From_Name     : in String;
--                     To_Email      : in String;
--                     To_Name       : in String;
--                     Subject       : in String;
--                     Charset       : in String := "iso-8859-1";
--                     SMTP_Server   : in String;
--                     Success       : in out Boolean);
   --  Compose and send a multipart email messages. This is for simple messages
   --  only. Attachments are not supported. If you need such "fancy" things
   --  then AWS.SMTP is what you're looking for.
   --  NOTE:
   --    The Charset string should of course match the encoding used in the
   --    HTML_Part and Text_Part.

private

   type Email_Kind is (Simple_Text_Only, Complex_Multipart);
   type Send_Status is (Yes, No);

   type File_Attachment is record
      File     : Unbounded_String;
      Encode   : AWS.Attachments.Encoding;
   end record;

   package File_Attachments is new Vectors (Positive, File_Attachment);
   package Recipients is new Vectors (Positive, AWS.SMTP.E_Mail_Data);
   package SMTP_Servers is new Vectors (Positive, Unbounded_String);

   type Email_Structure is record
      HTML_Part         : Unbounded_String;
      Text_Part         : Unbounded_String;
      From              : AWS.SMTP.E_Mail_Data;
      To_List           : Recipients.Vector;
      Subject           : Unbounded_String;
      Charset           : Unbounded_String;
      Attachments_List  : File_Attachments.Vector;
      SMTP_List         : SMTP_Servers.Vector;
      Email_Type        : Email_Kind := Simple_Text_Only;
      Is_Email_Send     : Send_Status := No;
   end record;

   function Get_Recipients (ES : in Email_Structure)
                            return AWS.SMTP.Recipients;

   procedure Send_Simple_Text_Only (ES : in out Email_Structure);
   procedure Send_Complex_Multipart (ES : in out Email_Structure);

   procedure Set_Alternative_Parts (C  : in out AWS.Attachments.List;
                                    ES : in     Email_Structure);

   procedure Set_File_Attachments (C   : in out AWS.Attachments.List;
                                   ES  : in     Email_Structure);

end Simple_Email;
