-------------------------------------------------------------------------------
--                                                                           --
--                                  Yolk                                     --
--                                                                           --
--                               view.index                                  --
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

with My_Configuration;
with Yolk.Email.Composer;
with Yolk.Rotating_Log;
--  with GNATCOLL.Email;
--  with GNATCOLL.Email.Utils;
--  with GNATCOLL.VFS; use GNATCOLL.VFS;
--  with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
--  with Ada.Calendar;
--  with AWS.MIME;
--  with AWS.Utils;
with Ada.Text_IO;

package body View.Index is

   ---------------
   --  Generate --
   ---------------

   function Generate
     (Request : in AWS.Status.Data)
      return AWS.Response.Data
   is

      use AWS.Templates;
      use Yolk.Email;
      use Yolk.Rotating_Log;

      package My renames My_Configuration;

      T : Translate_Set;

   begin

      Track (Handle     => Info,
             Log_String => "Testing the INFO track");

      Track (Handle     => Error,
             Log_String => "Testing the ERROR track");

      Insert (T, Assoc ("HANDLER", String'(My.Config.Get (My.Handler_Index))));
      Insert (T, Assoc ("TEMPLATE",
        String'(My.Config.Get (My.Template_Index))));
      Insert (T, Assoc ("URI", AWS.Status.URI (Request)));

      declare

         Email : Structure;

      begin

         Composer.Send (ES           => Email,
                        From_Address => "thomas@responsum.dk",
                        From_Name    => "Thomas Løcke",
                        To_Address   => "thomas@12boo.net",
                        To_Name      => "Thomas Løcke",
                        Subject      => "Test text/plain email med ÆØÅ æøå",
                        Text_Part    => "Test text/plain email med ÆØÅ æøå",
                        SMTP_Server  => "freja.serverbox.dk",
                        Charset      => ISO_8859_1);

         if Composer.Is_Send (ES => Email) then
            Ada.Text_IO.Put_Line ("Email Send!");
         else
            Ada.Text_IO.Put_Line ("Email NOT Send!");
         end if;

         --  Use a convenience procedure to build and send an email.
         --  Send (ES             => Bn_Email,
         --        From_Address   => "thomas@responsum.dk",
         --        From_Name      => "Thomas Løcke",
         --        To_Address     => "thomas@12boo.net",
         --        To_Name        => "Thomas Løcke",
         --        Subject        => "Text Type Test ÆØÅ æøå",
         --        Text_Part      => "Text Type Test ÆØÅ ÆØÅ",
         --        SMTP_Server    => "freja.serverbox.dk",
         --        Charset        => ISO_8859_1);

         --  Use a convenience procedure to build and send an email.
         --  Send (ES             => Email,
         --        From_Address   => "thomas@responsum.dk",
         --        From_Name      => "Thomas Løcke",
         --        To_Address     => "thomas@12boo.net",
         --        To_Name        => "Thomas Løcke",
         --        Subject        => "Test ÆØÅ æøå",
         --        Text_Part      => "Test ÆØÅ ÆØÅ",
         --        HTML_Part      => "<b>Test</b> ÆØÅ æøå",
         --        SMTP_Server    => "freja.serverbox.dk",
         --        Charset        => ISO_8859_1);

      exception
         when others =>
            Ada.Text_IO.Put_Line ("EMAIL PROBLEM!");
      end;

      return Build_Response
        (Status_Data   => Request,
         Template_File => My.Config.Get (My.Template_Index),
         Translations  => T);

   end Generate;

end View.Index;
