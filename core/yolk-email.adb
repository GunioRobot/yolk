-------------------------------------------------------------------------------
--                                                                           --
--                                  Yolk                                     --
--                                                                           --
--                                  Email                                    --
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
with AWS.MIME;
with AWS.Utils;
with GNATCOLL.Email.Utils;
with Yolk.Utilities;

package body Yolk.Email is

   -------------------------
   --  Build_Attachments  --
   -------------------------

   procedure Build_Attachments
     (ES    : in     Structure;
      Email : in out GNATCOLL.Email.Message)
   is
   begin

      for i in
        ES.Attachment_List.First_Index .. ES.Attachment_List.Last_Index loop
         declare

            use GNATCOLL.VFS;
            use Yolk.Utilities;

            Data : constant Attachment_Data := ES.Attachment_List.Element (i);
            File : constant Virtual_File := To_Virtual_File (Item => Data);

         begin

            Email.Attach
              (Path                 => File,
               MIME_Type            => AWS.MIME.Content_Type
                 (Filename => TS (Data.Path_To_File)),
               Charset              => Get_Charset (Data.Charset));

         end;
      end loop;

   end Build_Attachments;

   ------------------------
   --  Build_Bcc_Header  --
   ------------------------

   procedure Build_Bcc_Header
     (ES    : in     Structure;
      Email : in out GNATCOLL.Email.Message)
   is
   begin

      if not ES.Bcc_List.Is_Empty then
         declare

            use GNATCOLL.Email;

            Bcc : Header := Create (Name  => "Bcc",
                                    Value => "");

         begin

            Build_Email_Data (Header => Bcc,
                              List   => ES.Bcc_List);

            Email.Add_Header (H => Bcc);

         end;
      end if;

   end Build_Bcc_Header;

   -----------------------
   --  Build_Cc_Header  --
   -----------------------

   procedure Build_Cc_Header
     (ES    : in     Structure;
      Email : in out GNATCOLL.Email.Message)
   is
   begin

      if not ES.Cc_List.Is_Empty then
         declare

            use GNATCOLL.Email;

            Cc : Header := Create (Name   => "Cc",
                                   Value  => "");

         begin

            Build_Email_Data (Header => Cc,
                              List   => ES.Cc_List);

            Email.Add_Header (H => Cc);

         end;
      end if;

   end Build_Cc_Header;

   ----------------------------------------------
   --  Build_Content_Transfer_Encoding_Header  --
   ----------------------------------------------

   procedure Build_Content_Transfer_Encoding_Header
     (Charset : in     Character_Set;
      Email   : in out GNATCOLL.Email.Message)
   is

      use GNATCOLL.Email;

      CTE : Header;

   begin

      case Charset is
         when US_ASCII =>
            CTE := Create (Name  => Content_Transfer_Encoding,
                           Value => "7bit");
         when ISO_8859_1 .. Windows_1252 =>
            CTE := Create (Name  => Content_Transfer_Encoding,
                           Value => "quoted-printable");
         when UTF8 =>
            CTE := Create (Name  => Content_Transfer_Encoding,
                           Value => "base64");
      end case;

      Email.Add_Header (H => CTE);

   end Build_Content_Transfer_Encoding_Header;

   ---------------------------------
   --  Build_Content_Type_Header  --
   ---------------------------------

   procedure Build_Content_Type_Header
     (ES    : in     Structure;
      Email : in out GNATCOLL.Email.Message;
      Kind  : in     String)
   is

      use GNATCOLL.Email;

      CT : Header;

   begin

      CT := Create (Name   => Content_Type,
                    Value  => Kind);
      CT.Set_Param (Param_Name  => "charset",
                    Param_Value => Get_Charset (ES.Text_Part.Charset));
      Email.Add_Header (H => CT);

   end Build_Content_Type_Header;

   ----------------------------
   --  Build_Custom_Headers  --
   ----------------------------

   procedure Build_Custom_Headers
     (ES    : in     Structure;
      Email : in out GNATCOLL.Email.Message)
   is

      use GNATCOLL.Email;
      use Yolk.Utilities;

      Data     : Header_Data;
      Custom   : Header;

   begin

      for i in ES.Custom_Headers.First_Index .. ES.Custom_Headers.Last_Index
      loop
         Data := ES.Custom_Headers.Element (i);

         Custom := Create (Name    => TS (Data.Name),
                           Value   => TS (Data.Value),
                           Charset => Get_Charset (Data.Charset));

         Email.Add_Header (H => Custom);
      end loop;

   end Build_Custom_Headers;

   -------------------------
   --  Build_Date_Header  --
   -------------------------

   procedure Build_Date_Header
     (Email : in out GNATCOLL.Email.Message)
   is

      use GNATCOLL.Email;
      use GNATCOLL.Email.Utils;

      Date : constant Header := Create
        (Name  => "Date",
         Value => Format_Date (Date => Ada.Calendar.Clock));

   begin

      Email.Add_Header (H => Date);

   end Build_Date_Header;

   ------------------------
   --  Build_Email_Data  --
   ------------------------

   procedure Build_Email_Data
     (Header   : in out GNATCOLL.Email.Header;
      List     : in     Email_Data_Container.Vector)
   is

      use Yolk.Utilities;

      Data : Email_Data;

   begin

      for i in List.First_Index .. List.Last_Index loop
         Data := List.Element (i);

         if Is_Empty (Data.Address) then
            raise No_Address_Set;
         end if;

         if Data.Name = "" then
            Header.Append (Value   => TS (Data.Address));
         else
            Header.Append (Value   => TS (Data.Name),
                           Charset => Get_Charset (Data.Charset));
            Header.Append (Value => " <" & TS (Data.Address) & ">");
         end if;

         if i /= List.Last_Index then
            Header.Append (Value => ", ");
         end if;
      end loop;

   end Build_Email_Data;

   -------------------------
   --  Build_From_Header  --
   -------------------------

   procedure Build_From_Header
     (ES    : in     Structure;
      Email : in out GNATCOLL.Email.Message)
   is

      use GNATCOLL.Email;

      From : Header := Create (Name    => "From",
                               Value   => "");

   begin

      Build_Email_Data (Header => From,
                        List   => ES.From_List);

      Email.Add_Header (H => From);

   end Build_From_Header;

   -----------------------------
   --  Build_General_Headers  --
   -----------------------------

   procedure Build_General_Headers
     (ES    : in     Structure;
      Email : in out GNATCOLL.Email.Message)
   is
   begin

      Build_Bcc_Header (ES    => ES,
                        Email => Email);

      Build_Cc_Header (ES    => ES,
                       Email => Email);

      Build_Custom_Headers (ES      => ES,
                            Email   => Email);

      Build_Date_Header (Email => Email);

      Build_From_Header (ES    => ES,
                         Email => Email);

      Build_MIME_Header (Email => Email);

      Build_Reply_To_Header (ES    => ES,
                             Email => Email);

      Build_Sender_Header (ES    => ES,
                           Email => Email);

      Build_Subject_Header (ES    => ES,
                            Email => Email);

      Build_To_Header (ES    => ES,
                       Email => Email);

   end Build_General_Headers;

   -------------------------
   --  Build_MIME_Header  --
   -------------------------

   procedure Build_MIME_Header
     (Email : in out GNATCOLL.Email.Message)
   is

      use GNATCOLL.Email;

      MIME : constant Header := Create (Name  => MIME_Version,
                                        Value => "1.0");

   begin

      Email.Add_Header (H => MIME);

   end Build_MIME_Header;

   -----------------------------
   --  Build_Reply_To_Header  --
   -----------------------------

   procedure Build_Reply_To_Header
     (ES    : in     Structure;
      Email : in out GNATCOLL.Email.Message)
   is
   begin

      if not ES.Reply_To_List.Is_Empty then
         declare

            use GNATCOLL.Email;

            Reply_To : Header := Create (Name   => "Reply-To",
                                         Value  => "");

         begin

            Build_Email_Data (Header => Reply_To,
                              List   => ES.Reply_To_List);

            Email.Add_Header (H => Reply_To);

         end;
      end if;

   end Build_Reply_To_Header;

   ---------------------------
   --  Build_Sender_Header  --
   ---------------------------

   procedure Build_Sender_Header
     (ES    : in     Structure;
      Email : in out GNATCOLL.Email.Message)
   is
   begin

      if ES.Sender.Address /= Null_Unbounded_String then
         declare

            use GNATCOLL.Email;
            use Yolk.Utilities;

            Sender : Header;

         begin

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

         end;
      else
         if ES.From_List.Length > 1 then
            raise No_Sender_Set_With_Multiple_From;
         end if;
      end if;

   end Build_Sender_Header;

   ----------------------------
   --  Build_Subject_Header  --
   ----------------------------

   procedure Build_Subject_Header
     (ES    : in     Structure;
      Email : in out GNATCOLL.Email.Message)
   is

      use GNATCOLL.Email;
      use Yolk.Utilities;

      Subject  : constant Header := Create
        (Name    => "Subject",
         Value   => TS (ES.Subject.Content),
         Charset => Get_Charset (ES.Subject.Charset));

   begin

      Email.Add_Header (H => Subject);

   end Build_Subject_Header;

   -------------------------
   --  Build_To_Header  --
   -------------------------

   procedure Build_To_Header
     (ES    : in     Structure;
      Email : in out GNATCOLL.Email.Message)
   is

      use GNATCOLL.Email;

      To : Header := Create (Name   => "To",
                             Value  => "");

   begin

      Build_Email_Data (Header => To,
                        List   => ES.To_List);

      Email.Add_Header (H => To);

   end Build_To_Header;

   ------------------------------------
   --  Generate_Text_And_HTML_Email  --
   ------------------------------------

   procedure Generate_Text_And_HTML_Email
     (ES : in out Structure)
   is

      use GNATCOLL.Email;
      use Yolk.Utilities;

      Email          : Message := New_Message (Multipart_Alternative);
      HTML_Payload   : Message := New_Message (Text_Html);
      Text_Payload   : Message := New_Message (Text_Plain);

   begin

      Email.Set_Boundary (Boundary => AWS.Utils.Random_String (16));

      Text_Payload.Set_Text_Payload
        (Payload   => TS (ES.Text_Part.Content),
         Charset   => Get_Charset (ES.Text_Part.Charset));

      Text_Payload.Delete_Headers (Name => "");

      Build_Content_Transfer_Encoding_Header (Charset => ES.Text_Part.Charset,
                                              Email   => Text_Payload);

      Build_Content_Type_Header (ES    => ES,
                                 Email => Text_Payload,
                                 Kind  => Text_Plain);

      HTML_Payload.Set_Text_Payload
        (Payload   => TS (ES.HTML_Part.Content),
         Charset   => Get_Charset (ES.HTML_Part.Charset));

      HTML_Payload.Delete_Headers (Name => "");

      Build_Content_Transfer_Encoding_Header (Charset => ES.HTML_Part.Charset,
                                              Email   => HTML_Payload);

      Build_Content_Type_Header (ES    => ES,
                                 Email => HTML_Payload,
                                 Kind  => Text_Html);

      Email.Add_Payload (Payload => Text_Payload,
                         First   => True);

      Email.Add_Payload (Payload => HTML_Payload,
                         First   => False);

      Email.Set_Preamble
        (Preamble => "This is a multi-part message in MIME format.");

      Build_General_Headers (ES    => ES,
                             Email => Email);

      ES.Composed_Message := Email;

   end Generate_Text_And_HTML_Email;

   ----------------------------------------------------
   --  Generate_Text_And_HTML_With_Attachment_Email  --
   ----------------------------------------------------

   procedure Generate_Text_And_HTML_With_Attachment_Email
     (ES : in out Structure)
   is

      use GNATCOLL.Email;
      use Yolk.Utilities;

      Email_Alt      : Message := New_Message (Multipart_Alternative);
      Email_Mixed    : Message := New_Message (Multipart_Mixed);
      HTML_Payload   : Message := New_Message (Text_Html);
      Text_Payload   : Message := New_Message (Text_Plain);

   begin

      Email_Alt.Set_Boundary (Boundary => AWS.Utils.Random_String (16));
      Email_Mixed.Set_Boundary (Boundary => AWS.Utils.Random_String (16));

      Text_Payload.Set_Text_Payload
        (Payload   => TS (ES.Text_Part.Content),
         Charset   => Get_Charset (ES.Text_Part.Charset));

      Text_Payload.Delete_Headers (Name => "");

      Build_Content_Transfer_Encoding_Header (Charset => ES.Text_Part.Charset,
                                              Email   => Text_Payload);

      Build_Content_Type_Header (ES    => ES,
                                 Email => Text_Payload,
                                 Kind  => Text_Plain);

      HTML_Payload.Set_Text_Payload
        (Payload   => TS (ES.HTML_Part.Content),
         Charset   => Get_Charset (ES.HTML_Part.Charset));

      HTML_Payload.Delete_Headers (Name => "");

      Build_Content_Transfer_Encoding_Header (Charset => ES.HTML_Part.Charset,
                                              Email   => HTML_Payload);

      Build_Content_Type_Header (ES    => ES,
                                 Email => HTML_Payload,
                                 Kind  => Text_Html);

      Email_Alt.Add_Payload (Payload => Text_Payload,
                             First   => True);

      Email_Alt.Add_Payload (Payload => HTML_Payload,
                             First   => False);

      Email_Mixed.Add_Payload (Payload => Email_Alt,
                               First   => True);

      Build_Attachments (ES    => ES,
                         Email => Email_Mixed);

      Email_Mixed.Set_Preamble
        (Preamble => "This is a multi-part message in MIME format.");

      Build_General_Headers (ES    => ES,
                             Email => Email_Mixed);

      ES.Composed_Message := Email_Mixed;

   end Generate_Text_And_HTML_With_Attachment_Email;

   ---------------------------
   --  Generate_Text_Email  --
   ---------------------------

   procedure Generate_Text_Email
     (ES : in out Structure)
   is

      use GNATCOLL.Email;
      use Yolk.Utilities;

      Email : Message := New_Message (MIME_Type => Text_Plain);

   begin

      Email.Set_Text_Payload
        (Payload   => TS (ES.Text_Part.Content),
         Charset   => Get_Charset (ES.Text_Part.Charset));

      Email.Delete_Headers (Name => "");

      Build_General_Headers (ES    => ES,
                             Email => Email);

      Build_Content_Transfer_Encoding_Header (Charset => ES.Text_Part.Charset,
                                              Email   => Email);

      Build_Content_Type_Header (ES    => ES,
                                 Email => Email,
                                 Kind  => Text_Plain);

      ES.Composed_Message := Email;

   end Generate_Text_Email;

   -------------------------------------------
   --  Generate_Text_With_Attachment_Email  --
   -------------------------------------------

   procedure Generate_Text_With_Attachment_Email
     (ES : in out Structure)
   is

      use GNATCOLL.Email;
      use Yolk.Utilities;

      Email          : Message := New_Message (MIME_Type => Multipart_Mixed);
      Text_Payload   : Message := New_Message (MIME_Type => Text_Plain);

   begin

      Email.Set_Boundary (Boundary => AWS.Utils.Random_String (16));

      Text_Payload.Set_Text_Payload
        (Payload   => TS (ES.Text_Part.Content),
         Charset   => Get_Charset (ES.Text_Part.Charset));

      Text_Payload.Delete_Headers (Name => "");

      Build_Content_Transfer_Encoding_Header (Charset => ES.Text_Part.Charset,
                                              Email   => Text_Payload);

      Build_Content_Type_Header (ES    => ES,
                                 Email => Text_Payload,
                                 Kind  => Text_Plain);

      Email.Add_Payload (Payload => Text_Payload,
                         First   => True);

      Build_Attachments (ES    => ES,
                         Email => Email);

      Email.Set_Preamble
        (Preamble => "This is a multi-part message in MIME format.");

      Build_General_Headers (ES    => ES,
                             Email => Email);

      ES.Composed_Message := Email;

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

   -------------------------
   --  Set_Type_Of_Email  --
   -------------------------

   procedure Set_Type_Of_Email
     (ES : in out Structure)
   is

      use Yolk.Utilities;

   begin

      if not ES.Has_Text_Part then
         ES.Text_Part.Content := TUS ("");
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

   -----------------------
   --  To_Virtual_File  --
   -----------------------

   function To_Virtual_File
     (Item : in Attachment_Data)
      return GNATCOLL.VFS.Virtual_File
   is

      use Ada.Directories;
      use GNATCOLL.VFS;
      use Yolk.Utilities;

      Path_To_File : constant String := TS (Item.Path_To_File);

   begin

      if not Exists (Path_To_File) then
         raise Attachment_File_Not_Found;
      end if;

      return Locate_On_Path (Filesystem_String (Path_To_File));

   end To_Virtual_File;

end Yolk.Email;
