
with My_Configuration;  use My_Configuration;
with Rotating_Log;      use Rotating_Log;
with Simple_Email;      use Simple_Email;
with GNATCOLL.Email;    use GNATCOLL.Email;
with GNATCOLL.Email.Utils; use GNATCOLL.Email.Utils;

with GNATCOLL.SQL.Exec;
with Ada.Text_IO; use Ada.Text_IO;
--  with GNATCOLL.VFS; use GNATCOLL.VFS;

--  with Ada.Text_IO; use Ada.Text_IO;
--  with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
--  with Ada.Calendar;

--  with AWS.MIME;
--  with AWS.Utils;

--  with AWS.SMTP.Client;

package body View.Index is

   ---------------
   --  Generate --
   ---------------

   function Generate (Request : in AWS.Status.Data) return AWS.Response.Data
   is

      C_12boo : constant
        GNATCOLL.SQL.Exec.Database_Connection := DB_12boo.Connection;
      C_Wiki : constant
        GNATCOLL.SQL.Exec.Database_Connection := DB_Wiki.Connection;
      T : Translate_Set;
      Cursor : GNATCOLL.SQL.Exec.Forward_Cursor;

   begin

      --  DB_12boo tests
      if GNATCOLL.SQL.Exec.Check_Connection (C_12boo) then
         Put_Line ("DB_12boo up!");
      else
         Put_Line ("DB_12boo down!");
      end if;

      GNATCOLL.SQL.Exec.Fetch (Result     => Cursor,
                               Connection => C_12boo,
                               Query      => "select * from groups");

      if GNATCOLL.SQL.Exec.Success (C_12boo) then
         while GNATCOLL.SQL.Exec.Has_Row (Cursor) loop
            Put_Line (GNATCOLL.SQL.Exec.Value (Cursor, 1));
            GNATCOLL.SQL.Exec.Next (Cursor);
         end loop;
      else
         Put_Line ("DB_12boo not success!");
      end if;
      GNATCOLL.SQL.Exec.Commit_Or_Rollback (C_12boo);

      --  DB_Wiki tests
      if GNATCOLL.SQL.Exec.Check_Connection (C_Wiki) then
         Put_Line ("DB_Wiki up!");
      else
         Put_Line ("DB_Wiki down!");
      end if;

      GNATCOLL.SQL.Exec.Fetch (Result     => Cursor,
                               Connection => C_Wiki,
                               Query      => "select * from page");

      if GNATCOLL.SQL.Exec.Success (C_Wiki) then
         while GNATCOLL.SQL.Exec.Has_Row (Cursor) loop
            Put_Line (GNATCOLL.SQL.Exec.Value (Cursor, 2));
            GNATCOLL.SQL.Exec.Next (Cursor);
         end loop;
      else
         Put_Line ("DB_Wiki not success!");
      end if;
      GNATCOLL.SQL.Exec.Commit_Or_Rollback (C_Wiki);

      Track (Handle     => Info,
             Log_String => "Testing the INFO track");

      Track (Handle     => Error,
             Log_String => "Testing the ERROR track");

      Insert (T, Assoc ("HANDLER", String'(Get (Handler_Index))));
      Insert (T, Assoc ("TEMPLATE", String'(Get (Template_Index))));
      Insert (T, Assoc ("URI", AWS.Status.URI (Request)));

      declare

         An_Email : Email_Structure;
         Bn_Email : Email_Structure;
         Cn_Email : Email_Structure;

      begin

         Add_From (ES       => An_Email,
                   Address  => "thomas@responsum.dk",
                   Name     => "Thomas Løcke");
         Add_Recipient (ES       => An_Email,
                        Address  => "thomas.granvej6@gmail.com",
                        Name     => "Thomas Løcke");
         Add_File_Attachment (ES           => An_Email,
                              Path_To_File => "../../test.txt");
         Add_SMTP_Server (ES     => An_Email,
                          Host   => "freja.serverbox.dk");

         Set_Text_Part (ES       => An_Email,
                        Part     => "Text ÆØÅ æøå");
         Set_HTML_Part (ES       => An_Email,
                        Part     => "<b>HTML</b> ÆØÅ æøå");

         Send (ES => An_Email);

         Send (ES             => Bn_Email,
               From_Address   => "thomas@responsum.dk",
               From_Name      => "Thomas Løcke",
               To_Address     => "thomas@12boo.net",
               To_Name        => "Thomas Løcke",
               Subject        => "Test ÆØÅ æøå",
               Text_Part      => "Test ÆØÅ ÆØÅ",
               SMTP_Server    => "freja.serverbox.dk",
               Charset        => ISO_8859_1);

         Send (ES             => Cn_Email,
               From_Address   => "thomas@responsum.dk",
               From_Name      => "Thomas Løcke",
               To_Address     => "thomas@12boo.net",
               To_Name        => "Thomas Løcke",
               Subject        => "Test ÆØÅ æøå",
               Text_Part      => "Test ÆØÅ ÆØÅ",
               HTML_Part      => "<b>Test</b> ÆØÅ æøå",
               SMTP_Server    => "freja.serverbox.dk",
               Charset        => ISO_8859_1);

      end;

      ------------------------
      --  Text_Plain_Email  --
      ------------------------
--    Text_Plain_Email :
--    declare
--
--       Date     : Header;
--       From     : Header;
--       To       : Header;
--       MIME     : Header;
--       Subject  : Header;
--       CT       : Header;
--       CTE      : Header;
--       An_Email : Message := New_Message (Text_Plain);
--       US       : Unbounded_String := Null_Unbounded_String;
--
--    begin
--
--       New_Line;
--       Put_Line ("--> Plain_Text_Email Start <--");
--
--       --  First we set the text payload.
--       Set_Text_Payload (Msg       => An_Email,
--                         Payload   => "Test ÆØÅ æøå",
--                         Charset   => Charset_ISO_8859_1);
--
--       --  Delete whatever headers the Set_Text_Payload procedure might've
--       --  added.
--       Delete_Headers (Msg  => An_Email,
--                       Name => "");
--
--       --  Then we add the headers.
--       Date := Create (Name => "Date",
--                       Value => Format_Date (Date => Ada.Calendar.Clock));
--       Add_Header (Msg => An_Email,
--                   H   => Date);
--
--       From := Create (Name    => "From",
--                       Value   => "Thomas Løcke",
--                       Charset => Charset_ISO_8859_1);
--       Append (H       => From,
--               Value   => " <thomas@responsum.dk>");
--       Add_Header (Msg => An_Email,
--                   H   => From);
--
--       To := Create (Name    => "To",
--                     Value   => "Thomas Løcke",
--                     Charset => Charset_ISO_8859_1);
--       Append (H       => To,
--               Value   => " <tl@ada-dk-org>");
--       Add_Header (Msg => An_Email,
--                   H   => To);
--
--       MIME := Create (Name    => MIME_Version,
--                       Value   => "1.0");
--       Add_Header (Msg => An_Email,
--                   H   => MIME);
--
--       Subject := Create (Name    => "Subject",
--                          Value   => "Plain text ÆØÅ æøå",
--                          Charset => Charset_ISO_8859_1);
--       Add_Header (Msg => An_Email,
--                   H   => Subject);
--
--       CT := Create (Name    => Content_Type,
--                     Value   => Text_Plain);
--       Set_Param (H           => CT,
--                  Param_Name  => "charset",
--                  Param_Value => Charset_ISO_8859_1);
--       Add_Header (Msg => An_Email,
--                   H   => CT);
--
--       CTE := Create (Name    => Content_Transfer_Encoding,
--                      Value   => "8bit");
--       Add_Header (Msg => An_Email,
--                   H   => CTE);
--
--       --  Finally we output the raw email.
--       To_String (Msg    => An_Email,
--                  Result => US);
--
--       Put_Line (To_String (US));
--
--       Put_Line ("--> Plain_Text_Email End <--");
--
--    end Text_Plain_Email;

      ----------------------------
      --  Text_With_Attachment  --
      ----------------------------
--    Text_With_Attachment :
--    declare
--
--       A_File   : constant GNATCOLL.VFS.Virtual_File :=
--                    Locate_On_Path ("/home/thomas/test.txt");
--       Date     : Header;
--       From     : Header;
--       To       : Header;
--       MIME     : Header;
--       Subject  : Header;
--       CT       : Header;
--       CTE      : Header;
--       An_Email : Message := New_Message (Multipart_Mixed);
--       T_Payload : Message := New_Message (Text_Plain);
--       US       : Unbounded_String := Null_Unbounded_String;
--
--    begin
--
--       --  Set multipart boundaries
--       Set_Boundary (Msg      => An_Email,
--                     Boundary => AWS.Utils.Random_String (16));
--
--       New_Line;
--       Put_Line ("--> Text_With_Attachment Start <--");
--
--       --  First we set the text payload.
--       Set_Text_Payload (Msg       => T_Payload,
--                         Payload   => "Test ÆØÅ æøå",
--                         Charset   => Charset_ISO_8859_1);
--
--       --  Delete all headers for T_Payload
--       Delete_Headers (Msg  => T_Payload,
--                       Name => "");
--
--       --  Add Content-Type and Content-Transfer-Encoding headers to
--       --  T_Payload.
--       CT := Create (Name    => Content_Type,
--                     Value   => Text_Plain);
--       Set_Param (H           => CT,
--                  Param_Name  => "charset",
--                  Param_Value => Charset_ISO_8859_1);
--       Add_Header (Msg => T_Payload,
--                   H   => CT);
--
--       CTE := Create (Name    => Content_Transfer_Encoding,
--                      Value   => "8bit");
--       Add_Header (Msg => T_Payload,
--                   H   => CTE);
--
--       --  Add T_Payload to the An_Email message.
--       Add_Payload (Msg     => An_Email,
--                    Payload => T_Payload,
--                    First   => True);
--
--       --  Then we add an attachment
--       Attach (Msg                  => An_Email,
--               Path                 => A_File,
--               MIME_Type            => AWS.MIME.Content_Type ("test.txt"),
--               Description          => "A description of the file",
--               Charset              => Charset_ISO_8859_1);
--
--       --  Set the MIME preamble
--       Set_Preamble
--         (Msg      => An_Email,
--          Preamble => "This is a multi-part message in MIME format.");
--
--       --  Adding the headers to An_Email
--       Date := Create (Name => "Date",
--                       Value => Format_Date (Date => Ada.Calendar.Clock));
--       Add_Header (Msg => An_Email,
--                   H   => Date);
--
--       From := Create (Name    => "From",
--                       Value   => "Thomas Løcke",
--                       Charset => Charset_ISO_8859_1);
--       Append (H       => From,
--               Value   => " <thomas@responsum.dk>");
--       Add_Header (Msg => An_Email,
--                   H   => From);
--
--       To := Create (Name    => "To",
--                     Value   => "Thomas Løcke",
--                     Charset => Charset_ISO_8859_1);
--       Append (H       => To,
--               Value   => " <tl@ada-dk-org>");
--       Add_Header (Msg => An_Email,
--                   H   => To);
--
--       MIME := Create (Name    => MIME_Version,
--                       Value   => "1.0");
--       Add_Header (Msg => An_Email,
--                   H   => MIME);
--
--       Subject := Create (Name    => "Subject",
--                          Value   => "Text with attachment ÆØÅ æøå",
--                          Charset => Charset_ISO_8859_1);
--       Add_Header (Msg => An_Email,
--                   H   => Subject);
--
--       --  Finally we output the raw email.
--       To_String (Msg    => An_Email,
--                  Result => US);
--
--       Put_Line (To_String (US));
--
--       Put_Line ("--> Text_With_Attachment_End <--");
--
--    end Text_With_Attachment;

      ---------------------------------------
      --  Text_HTML_Multipart_Alternative  --
      ---------------------------------------
--    Text_HTML_Multipart_Alternative :
--    declare
--
--       Date     : Header;
--       From     : Header;
--       To       : Header;
--       MIME     : Header;
--       Subject  : Header;
--       CT       : Header;
--       CTE      : Header;
--       An_Email : Message := New_Message (Multipart_Alternative);
--       T_Payload : Message := New_Message (Text_Plain);
--       H_Payload : Message := New_Message (Text_Html);
--       US       : Unbounded_String := Null_Unbounded_String;
--
--    begin
--
--       --  Set multipart boundaries
--       Set_Boundary (Msg      => An_Email,
--                     Boundary => AWS.Utils.Random_String (16));
--
--       New_Line;
--       Put_Line ("--> Text_HTML_Multipart_Alternative Start <--");
--
--       --  First we set the text payload.
--       Set_Text_Payload (Msg       => T_Payload,
--                         Payload   => "Test ÆØÅ æøå",
--                         Charset   => Charset_ISO_8859_1);
--
--       --  Delete whatever headers the Set_Text_Payload procedure might've
--       --  added.
--       Delete_Headers (Msg  => T_Payload,
--                       Name => "");
--
--       --  Add Content-Type and Content-Transfer-Encoding headers to
--       --  T_Payload.
--       CT := Create (Name    => Content_Type,
--                     Value   => Text_Plain);
--       Set_Param (H           => CT,
--                  Param_Name  => "charset",
--                  Param_Value => Charset_ISO_8859_1);
--       Add_Header (Msg => T_Payload,
--                   H   => CT);
--
--       CTE := Create (Name    => Content_Transfer_Encoding,
--                      Value   => "8bit");
--       Add_Header (Msg => T_Payload,
--                   H   => CTE);
--
--       --  Next we set the HTML payload.
--       Set_Text_Payload (Msg       => H_Payload,
--                         Payload   => "<b>Test</b> ÆØÅ æøå",
--                         Charset   => Charset_ISO_8859_1);
--
--       --  Delete whatever headers the Set_Text_Payload procedure might've
--       --  added.
--       Delete_Headers (Msg  => H_Payload,
--                       Name => "");
--
--       --  Add Content-Type and Content-Transfer-Encoding headers to
--       --  H_Payload.
--       CT := Create (Name    => Content_Type,
--                     Value   => Text_Html);
--       Set_Param (H           => CT,
--                  Param_Name  => "charset",
--                  Param_Value => Charset_ISO_8859_1);
--       Add_Header (Msg => H_Payload,
--                   H   => CT);
--
--       CTE := Create (Name    => Content_Transfer_Encoding,
--                      Value   => "8bit");
--       Add_Header (Msg => H_Payload,
--                   H   => CTE);
--
--       --  Add T_Payload and H_Payload to the An_Email message.
--       Add_Payload (Msg     => An_Email,
--                    Payload => T_Payload,
--                    First   => True);
--       Add_Payload (Msg     => An_Email,
--                    Payload => H_Payload,
--                    First   => False);
--
--       --  Set the MIME preamble
--       Set_Preamble
--        (Msg      => An_Email,
--         Preamble => "This is a multi-part message in MIME format.");
--
--       --  Then we add the headers.
--       Date := Create (Name => "Date",
--                       Value => Format_Date (Date => Ada.Calendar.Clock));
--       Add_Header (Msg => An_Email,
--                   H   => Date);
--
--       From := Create (Name    => "From",
--                       Value   => "Thomas Løcke",
--                       Charset => Charset_ISO_8859_1);
--       Append (H       => From,
--              Value   => " <thomas@responsum.dk>");
--       Add_Header (Msg => An_Email,
--                   H   => From);
--
--       To := Create (Name    => "To",
--                     Value   => "Thomas Løcke",
--                     Charset => Charset_ISO_8859_1);
--       Append (H       => To,
--               Value   => " <tl@ada-dk-org>");
--       Add_Header (Msg => An_Email,
--                   H   => To);
--
--       MIME := Create (Name    => MIME_Version,
--                       Value   => "1.0");
--       Add_Header (Msg => An_Email,
--                   H   => MIME);
--
--       Subject := Create (Name    => "Subject",
--                          Value   => "Multipart_Alternative ÆØÅ æøå",
--                          Charset => Charset_ISO_8859_1);
--       Add_Header (Msg => An_Email,
--                   H   => Subject);
--
--       --  Finally we output the raw email.
--       To_String (Msg    => An_Email,
--                  Result => US);
--
--       Put_Line (To_String (US));
--
--       Put_Line ("--> Text_HTML_Multipart_Alternative End <--");
--
--    end Text_HTML_Multipart_Alternative;

      ----------------------------------
      --  Multipart_Mixed_Attachment  --
      ----------------------------------
--    Multipart_Mixed_Attachment :
--    declare
--
--       SMTP              : constant AWS.SMTP.Receiver
--         := AWS.SMTP.Client.Initialize ("freja.serverbox.dk");
--       Status            : AWS.SMTP.Status;
--
--       A_File   : constant GNATCOLL.VFS.Virtual_File :=
--                    Locate_On_Path ("/home/thomas/test.txt");
--       B_File   : constant GNATCOLL.VFS.Virtual_File :=
--                    Locate_On_Path ("/home/thomas/test.odt");
--       Date     : Header;
--       From     : Header;
--       To       : Header;
--       MIME     : Header;
--       Subject  : Header;
--       CT       : Header;
--       CTE      : Header;
--       An_Email : Message := New_Message (Multipart_Mixed);
--       Alter    : Message := New_Message (Multipart_Alternative);
--       T_Payload : Message := New_Message (Text_Plain);
--       H_Payload : Message := New_Message (Text_Html);
--       US       : Unbounded_String := Null_Unbounded_String;
--
--    begin
--
--       --  Set multipart boundaries
--       Set_Boundary (Msg      => An_Email,
--                     Boundary => AWS.Utils.Random_String (16));
--       Set_Boundary (Msg      => Alter,
--                     Boundary => AWS.Utils.Random_String (16));
--
--       New_Line;
--       Put_Line ("--> Multipart_Mixed_Attachment Start <--");
--
--       --  First we set the text payload.
--       Set_Text_Payload (Msg       => T_Payload,
--                         Payload   => "Test ÆØÅ æøå",
--                         Charset   => Charset_ISO_8859_1);
--
--       --  Delete whatever headers the Set_Text_Payload procedure might've
--       --  added.
--       Delete_Headers (Msg  => T_Payload,
--                       Name => "");
--
--       --  Add Content-Type and Content-Transfer-Encoding headers to
--       --  T_Payload.
--       CT := Create (Name    => Content_Type,
--                     Value   => Text_Plain);
--       Set_Param (H           => CT,
--                  Param_Name  => "charset",
--                  Param_Value => Charset_ISO_8859_1);
--       Add_Header (Msg => T_Payload,
--                   H   => CT);
--
--       CTE := Create (Name    => Content_Transfer_Encoding,
--                      Value   => "8bit");
--       Add_Header (Msg => T_Payload,
--                   H   => CTE);
--
--       --  Next we set the HTML payload.
--       Set_Text_Payload (Msg       => H_Payload,
--                         Payload   => "<b>Test</b> ÆØÅ æøå",
--                         Charset   => Charset_ISO_8859_1);
--
--       --  Delete whatever headers the Set_Text_Payload procedure might've
--       --  added.
--       Delete_Headers (Msg  => H_Payload,
--                       Name => "");
--
--       --  Add Content-Type and Content-Transfer-Encoding headers to
--       --  H_Payload.
--       CT := Create (Name    => Content_Type,
--                     Value   => Text_Html);
--       Set_Param (H           => CT,
--                  Param_Name  => "charset",
--                  Param_Value => Charset_ISO_8859_1);
--       Add_Header (Msg => H_Payload,
--                   H   => CT);
--
--       CTE := Create (Name    => Content_Transfer_Encoding,
--                      Value   => "8bit");
--       Add_Header (Msg => H_Payload,
--                   H   => CTE);
--
--       --  Add T_Payload and H_Payload to the Alter message.
--       Add_Payload (Msg     => Alter,
--                    Payload => T_Payload,
--                    First   => True);
--       Add_Payload (Msg     => Alter,
--                    Payload => H_Payload,
--                    First   => False);
--
--           --  Add the Alter message to An_Email
--           Add_Payload (Msg     => An_Email,
--                        Payload => Alter,
--                        First   => True);
--
--       --  Then we add an attachment to An_Email
--       Attach (Msg                  => An_Email,
--               Path                 => A_File,
--               MIME_Type            => AWS.MIME.Content_Type ("test.txt"),
--               Description          => "A description of the file",
--               Charset              => Charset_ISO_8859_1);
--
--       --  And another attachment to An_Email
--       Attach (Msg                  => An_Email,
--               Path                 => B_File,
--               MIME_Type            => AWS.MIME.Content_Type ("test.odt"),
--               Description          => "An Openoffice file",
--               Charset              => Charset_ISO_8859_1);
--
--       --  Set the MIME preamble
--       Set_Preamble
--         (Msg      => An_Email,
--          Preamble => "This is a multi-part message in MIME format.");
--
--       --  Then we add the headers.
--       Date := Create (Name => "Date",
--                       Value => Format_Date (Date => Ada.Calendar.Clock));
--       Add_Header (Msg => An_Email,
--                   H   => Date);
--
--       From := Create (Name    => "From",
--                       Value   => "Thomas Løcke",
--                       Charset => Charset_ISO_8859_1);
--       Append (H       => From,
--               Value   => " <thomas@responsum.dk>");
--       Add_Header (Msg => An_Email,
--                   H   => From);
--
--       To := Create (Name    => "To",
--                     Value   => "Thomas Løcke",
--                     Charset => Charset_ISO_8859_1);
--       Append (H       => To,
--               Value   => " <tl@ada-dk-org>");
--       Add_Header (Msg => An_Email,
--                   H   => To);
--
--       MIME := Create (Name    => MIME_Version,
--                       Value   => "1.0");
--       Add_Header (Msg => An_Email,
--                   H   => MIME);
--
--       Subject := Create (Name    => "Subject",
--                          Value   => "Multipart_Mixed_Attachment ÆØÅ æøå",
--                          Charset => Charset_ISO_8859_1);
--       Add_Header (Msg => An_Email,
--                   H   => Subject);
--
--       --  Finally we output the raw email.
--       To_String (Msg    => An_Email,
--                  Result => US);
--
--       Put_Line (To_String (US));
--
--       Put_Line ("--> Multipart_Mixed_Attachment End <--");
--
--       AWS.SMTP.Client.Send
--         (Server  => SMTP,
--          From    => AWS.SMTP.E_Mail (Name    => "Thomas Løcke",
--                                      Address => "thomas@responsum.dk"),
--          To      => AWS.SMTP.E_Mail (Name    => "Thomas Løcke",
--                                      Address => "tl@ada-dk.org"),
--          Subject => "Stuff",
--          Message => To_String (US),
--          Status  => Status);
--
--       if AWS.SMTP.Is_Ok (Status) then
--          Put_Line ("Email send!");
--       end if;
--
--    end Multipart_Mixed_Attachment;

      return Build_Response (Template_File => Get (Template_Index),
                             Translations  => T);

   end Generate;

end View.Index;
