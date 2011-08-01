-------------------------------------------------------------------------------
--                                                                           --
--                                  Yolk                                     --
--                                                                           --
--                               View.Syndication                                  --
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
with Yolk.Utilities;

package body View.Syndication is

   ---------------
   --  Generate --
   ---------------

   function Generate
     (Request : in AWS.Status.Data)
      return AWS.Response.Data
   is

      use AWS.Response;
      use AWS.Status;
      use Yolk.Utilities;

      Valid : Boolean := False;
      Value : Unbounded_String;

   begin

      Cache1.Read (Key      => Feed_Data,
                   Is_Valid => Valid,
                   Value    => Value);

      if not Valid then
         Value := TUS (Get_XML_String (Feed));

         Cache1.Write (Key   => Feed_Data,
                       Value => Value);
      end if;

      return Build_Response
        (Status_Data => Request,
         Content     => TS (Value),
         MIME_Type   => Text_XML);

   exception

      when Not_Valid_XML =>
         return Build_Response (Status_Data => Request,
                                Content     => "<p>XML not valid.</p>");

   end Generate;

begin

   --  Lets build the ATOM feed. This is a rather involved process, since there
   --  are quite a lot of XML elements to take care of in the ATOM RFC.
   --  The feed built here is using more or less every available element. This
   --  is _not_ required by RFC 4287, I've merely done so for completeness of
   --  the example.
   --
   --  This feed validates according to the W3C feed validator:
   --    http://validator.w3.org/feed/
   --
   --  The Atom RFC can be found here:
   --    http://tools.ietf.org/html/rfc4287

   Set_Id (Feed     => Feed,
           Id       => "http://id/uri",
           Base_URI => "/base/id",
           Language => "en");
   Set_Title (Feed       => Feed,
              Title      => "Some title",
              Base_URI   => "/base/title",
              Language   => "en",
              Title_Kind => Text);
   Add_Author (Feed     => Feed,
               Name     => "Thomas Locke");
   Add_Category (Feed     => Feed,
                 Term     => "Some Term");
   Add_Contributor (Feed => Feed,
                    Name => "Trine Locke");
   Set_Generator (Feed => Feed,
                  Agent => "Yolk Syndication",
                  Base_URI => "/base/generator",
                  Language => "en",
                  URI      => "http://generator/URI",
                  Version  => Version);

   Set_Icon (Feed     => Feed,
             URI      => "http://icon/URI");

   Add_Link (Feed => Feed,
             Href => "http://one.pancon.dk/syndication",
             Rel => Self);

   Add_Link (Feed => Feed,
             Href => "link_one",
             Base_URI  => "/base/link",
             Hreflang  => "en",
             Language  => "en",
             Length    => 1,
             Mime_Type => "text/html",
             Rel       => Alternate,
             Title     => "Title");
   Add_Link (Feed => Feed,
             Href => "http://link_two");

   Set_Logo (Feed     => Feed,
             URI      => "http://logo/uri",
             Base_URI => "/base/logo",
             Language => "en");

   Set_Rights (Feed        => Feed,
               Rights      => "Some rights",
               Base_URI    => "/base/rights",
               Language    => "da",
               Rights_Kind => Text);

   Set_Subtitle (Feed          => Feed,
                 Subtitle      => "Some subtitle",
                 Base_URI      => "/base/subtitle",
                 Language      => "da",
                 Subtitle_Kind => Text);

   --  With the basic feed elements out of the way, it is time to add an entry.
   declare

      An_Entry : Atom_Entry := New_Atom_Entry (Base_URI => "/base/entry",
                                               Language => "en");
      Success  : Boolean;

   begin

      Add_Author (Entr     => An_Entry,
                  Name     => "Thomas Locke",
                  Base_URI => "/base/author",
                  Email    => "thomas@loecke.dk",
                  Language => "en",
                  URI      => "http://12boo.net");

      Add_Category (Entr     => An_Entry,
                    Term     => "entry category",
                    Base_URI => "/base/category",
                    Label    => "A label",
                    Language => "en",
                    Scheme   => "http://URI/to/scheme");

      Add_Contributor (Entr     => An_Entry,
                       Name     => "Contributor Thomas Locke",
                       Base_URI => "/base/contributor",
                       Email    => "thomas@12boo.net",
                       Language => "en",
                       URI      => "http://contributor/URI");

      Set_Content_OutOfLine (Entr      => An_Entry,
                             Mime_Type => "text/html",
                             Source    => "http://some/URL",
                             Base_URI  => "/base/outofline",
                             Language  => "en");

      Set_Common_Source (Entr     => An_Entry,
                         Base_URI => "/base/source",
                         Language => "en");

      Add_Author_Source (Entr     => An_Entry,
                         Name     => "Thomas Locke",
                         Base_URI => "/base/author",
                         Email    => "thomas@12boo.net",
                         Language => "en",
                         URI      => "http://author/URI");

      Add_Category_Source (Entr     => An_Entry,
                           Term     => "Source category");

      Add_Contributor_Source (Entr     => An_Entry,
                              Name     => "Thomas Locke");

      Set_Generator_Source (Entr     => An_Entry,
                            Agent    => "Yolk",
                            Base_URI => "/base/generator",
                            Language => "en",
                            URI      => "http://generator/URI",
                            Version  => Version);

      Set_Icon_Source (Entr     => An_Entry,
                       URI      => "http://icon/URI",
                       Base_URI => "/base/icon",
                       Language => "en");

      Set_Id_Source (Entr     => An_Entry,
                     Id       => "http://source/id/URI",
                     Base_URI => "/base/id",
                     Language => "en");

      Add_Link_Source (Entr      => An_Entry,
                       Href      => "http://source/link",
                       Base_URI  => "/base/link",
                       Hreflang  => "en",
                       Language  => "en",
                       Length    => 1,
                       Mime_Type => "text/html",
                       Rel       => Alternate,
                       Title     => "Source Link Title");

      Set_Logo_Source (Entr     => An_Entry,
                       URI      => "http://logo/source/URI",
                       Base_URI => "/base/logo",
                       Language => "en");

      Set_Rights_Source (Entr        => An_Entry,
                         Rights      => "Source Rrights",
                         Base_URI    => "/base/rights",
                         Language    => "en",
                         Rights_Kind => Text);

      Set_Subtitle_Source (Entr          => An_Entry,
                           Subtitle      => "Source Subtitle",
                           Base_URI      => "/base/subtitle",
                           Language      => "en",
                           Subtitle_Kind => Text);

      Set_Title_Source (Entr       => An_Entry,
                        Title      => "Source Title",
                        Base_URI   => "/base/title",
                        Language   => "en",
                        Title_Kind => Text);

      Set_Updated_Source (Entr        => An_Entry,
                          Update_Time => Ada.Calendar.Clock,
                          Base_URI    => "/base/updated",
                          Language    => "en");

      Set_Id (Entr     => An_Entry,
              Id       => "http://entry/id/URI",
              Base_URI => "/base/Id",
              Language => "en");

      Add_Link (Entr      => An_Entry,
                Href      => "http://a/link",
                Base_URI  => "/base/link",
                Hreflang  => "en",
                Language  => "en",
                Length    => 1,
                Mime_Type => "text/html",
                Rel       => Alternate,
                Title     => "Link Title");
      Add_Link (Entr => An_Entry,
                Href => "http://another/link");

      Set_Published (Entr           => An_Entry,
                     Published_Time => Ada.Calendar.Clock,
                     Base_URI       => "/base/updated",
                     Language       => "en");

      Set_Rights (Entr        => An_Entry,
                  Rights      => "Some entry rights",
                  Base_URI    => "/base/entryrights",
                  Language    => "en",
                  Rights_Kind => Text);

      Set_Summary (Entr         => An_Entry,
                   Summary      => "<b>An</b> entry summary",
                   Base_URI     => "/base/entryrights",
                   Language     => "en",
                   Summary_Kind => Xhtml);

      Set_Title (Entr       => An_Entry,
                 Title      => "Entry Title (click to go back to /)",
                 Base_URI   => "/base/entrytitle",
                 Language   => "en",
                 Title_Kind => Text);

      Set_Updated (Entr        => An_Entry,
                   Update_Time => Ada.Calendar.Clock,
                   Base_URI    => "/base/updated",
                   Language    => "en");

      Add_Entry (Feed        => Feed,
                 Entr        => An_Entry,
                 Entry_Added => Success);

   end;

   Set_Updated (Feed        => Feed,
                Update_Time => Ada.Calendar.Clock);
   --  Finally we set the updated timestamp for the ATOM feed to now.

end View.Syndication;
