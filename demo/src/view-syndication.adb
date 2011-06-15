-------------------------------------------------------------------------------
--                                                                           --
--                                  Yolk                                     --
--                                                                           --
--                               View.Syndication                                  --
--                                                                           --
--                                  BODY                                     --
--                                                                           --
--                   Copyright (C) 2010-2011, Thomas L�cke                   --
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

-------------------------------------------------------------------------------
--                                                                           --
--                            DEMO FILE                                      --
--                                                                           --
-------------------------------------------------------------------------------

--  This is a DEMO file. You can either move this to the my_view/ directory and
--  change it according to you own needs, or you can provide your own.
--
--  This package is currently only "with'ed" by other demo source files. It is
--  NOT required by Yolk in any way.

with Ada.Calendar;
with AWS.Messages;
with AWS.MIME;

package body View.Syndication is

   ---------------
   --  Generate --
   ---------------

   function Generate
     (Request : in AWS.Status.Data)
      return AWS.Response.Data
   is

      use AWS.Messages;
      use AWS.MIME;
      use AWS.Response;
      use AWS.Status;

      Encoding : Content_Encoding := Identity;
      --  Default to no encoding.

   begin

      if Is_Supported (Request, GZip) then
         Encoding := GZip;
         --  GZip is supported by the client.
      end if;

      return Build (Content_Type  => Text_XML,
                    Message_Body  => Get_XML_String (Feed),
                    Encoding      => Encoding);

   exception

      when Not_Valid_XML =>
         return Build (Content_Type  => Text_HTML,
                       Message_Body  => "WTF!",
                       Encoding      => Encoding);

   end Generate;

begin

   Set_Id (Feed     => Feed,
           Id       => "/some/id/uri",
           Base_URI => "/",
           Language => "da");
   Set_Title (Feed       => Feed,
              Title      => "Some title",
              Base_URI   => "/",
              Language   => "da",
              Title_Kind => Text);
   Add_Author (Feed     => Feed,
               Name     => "Thomas Locke");
   Add_Category (Feed     => Feed,
                 Term     => "Some Term");
   Add_Contributor (Feed => Feed,
                    Name => "Trine Locke");
   Set_Generator (Feed => Feed,
                  Agent => "Yolk Syndication",
                  Base_URI => "/",
                  Language => "da",
                  URI      => "/some/uri",
                  Version  => "0.21");

   Set_Icon (Feed     => Feed,
             URI      => "icon URI");

   Add_Link (Feed => Feed,
             Href => "link_one",
             Base_URI  => "base",
             Content   => "content",
             Hreflang  => "hreflang",
             Language  => "lang",
             Length    => 1,
             Mime_Type => "mime",
             Rel       => Alternate,
             Title     => "Title");
   Add_Link (Feed => Feed,
             Href => "link_two");

   Set_Logo (Feed     => Feed,
             URI      => "logo/uri",
             Base_URI => "base",
             Language => "da");

   Set_Rights (Feed        => Feed,
               Rights      => "Some rights",
               Base_URI    => "base/",
               Language    => "da",
               Rights_Kind => Text);

   Set_Subtitle (Feed          => Feed,
                 Subtitle      => "Some subtitle",
                 Base_URI      => "base/",
                 Language      => "da",
                 Subtitle_Kind => Text);

   declare

      An_Entry : Atom_Entry := New_Atom_Entry (Base_URI => "entry/base/",
                                               Language => "entry lang");
      Success : Boolean;

   begin

      Add_Author (Entr     => An_Entry,
                  Name     => "Thomas Locke",
                  Base_URI => "author/base",
                  Email    => "thomas@loecke.dk",
                  Language => "da",
                  URI      => "http://12boo.net");

      Add_Category (Entr     => An_Entry,
                    Term     => "entry category",
                    Base_URI => "/",
                    Content  => "undefined content",
                    Label    => "A label",
                    Language => "da",
                    Scheme   => "URI to scheme");

      Add_Contributor (Entr     => An_Entry,
                       Name     => "Con Thomas Locke",
                       Base_URI => "base/con",
                       Email    => "con email",
                       Language => "con lan",
                       URI      => "con URI");

      Set_Content_OutOfLine (Entr      => An_Entry,
                             Mime_Type => "OutOfLine Content",
                             Source    => "SomeURL",
                             Base_URI  => "OutOfLine/base",
                             Language  => "OutOfLine/lang");

      Set_Common_Source (Entr     => An_Entry,
                         Base_URI => "source/base",
                         Language => "source/lang");

      Add_Author_Source (Entr     => An_Entry,
                         Name     => "Thomas Locke",
                         Base_URI => "author/base",
                         Email    => "thomas@responsum.dk",
                         Language => "author/lang",
                         URI      => "author URI");

      Add_Category_Source (Entr     => An_Entry,
                           Term     => "Source category");

      Add_Contributor_Source (Entr     => An_Entry,
                              Name     => "Thomas Locke");

      Set_Generator_Source (Entr     => An_Entry,
                            Agent    => "source generator",
                            Base_URI => "generator/base",
                            Language => "generator/lang",
                            URI      => "generator/uri",
                            Version  => "generator/version");

      Set_Icon_Source (Entr     => An_Entry,
                       URI      => "icon source URI",
                       Base_URI => "icon/base",
                       Language => "icon/lang");

      Set_Id_Source (Entr     => An_Entry,
                     Id       => "source Id URI",
                     Base_URI => "id/base",
                     Language => "id/lang");

      Add_Link_Source (Entr      => An_Entry,
                       Href      => "source link",
                       Base_URI  => "link/base",
                       Content   => "source content",
                       Hreflang  => "source hreflang",
                       Language  => "link/lang",
                       Length    => 1,
                       Mime_Type => "source mime",
                       Rel       => Alternate,
                       Title     => "source link title");

      Set_Logo_Source (Entr     => An_Entry,
                       URI      => "logo source URI",
                       Base_URI => "logo/base",
                       Language => "logo/lang");

      Set_Rights_Source (Entr        => An_Entry,
                         Rights      => "source rights",
                         Base_URI    => "rights/base",
                         Language    => "rights/lang",
                         Rights_Kind => Text);

      Set_Subtitle_Source (Entr          => An_Entry,
                           Subtitle      => "source subtitle",
                           Base_URI      => "subtitle/base",
                           Language      => "subtitle/lang",
                           Subtitle_Kind => Text);

      Set_Title_Source (Entr       => An_Entry,
                        Title      => "source title",
                        Base_URI   => "title/base",
                        Language   => "title/lang",
                        Title_Kind => Text);

      Set_Updated_Source (Entr        => An_Entry,
                          Update_Time => Ada.Calendar.Clock,
                          Base_URI    => "updated/base",
                          Language    => "updated/lang");

      Set_Id (Entr     => An_Entry,
              Id       => "id URI",
              Base_URI => "id/base",
              Language => "id/lang");

      Add_Link (Entr      => An_Entry,
                Href      => "entry link_one",
                Base_URI  => "base",
                Content   => "content",
                Hreflang  => "hreflang",
                Language  => "lang",
                Length    => 1,
                Mime_Type => "mime",
                Rel       => Alternate,
                Title     => "Title");
      Add_Link (Entr => An_Entry,
                Href => "entry link_two");

      Set_Published (Entr           => An_Entry,
                     Published_Time => Ada.Calendar.Clock,
                     Base_URI       => "published/base",
                     Language       => "published/language");

      Set_Rights (Entr        => An_Entry,
                  Rights      => "Some entry rights",
                  Base_URI    => "entryrights/base",
                  Language    => "entryrights/lang",
                  Rights_Kind => Text);

      Set_Summary (Entr         => An_Entry,
                   Summary      => "<b>An</b> entry summary",
                   Base_URI     => "summary/base",
                   Language     => "summary/lang",
                   Summary_Kind => Xhtml);

      Set_Title (Entr       => An_Entry,
                 Title      => "Entry Title",
                 Base_URI   => "entrytitle/base",
                 Language   => "entrytitle/lang",
                 Title_Kind => Text);

      Set_Updated (Entr        => An_Entry,
                   Update_Time => Ada.Calendar.Clock,
                   Base_URI    => "updated/base",
                   Language    => "updated/language");

      Add_Entry (Feed        => Feed,
                 Entr        => An_Entry,
                 Entry_Added => Success);

   end;

   Set_Updated (Feed        => Feed,
                Update_Time => Ada.Calendar.Clock);

end View.Syndication;