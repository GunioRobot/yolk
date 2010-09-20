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

package Simple_Email is

   function Send (HTML_Part   : in String;
                  Text_Part   : in String;
                  From_Email  : in String;
                  From_Name   : in String;
                  To_Email    : in String;
                  To_Name     : in String;
                  Subject     : in String;
                  SMTP_Server : in String) return Boolean;
   --  Compose and send a multipart email messages. This is for simple messages
   --  only. Attachments are not supported. If you need such "fancy" things
   --  then AWS.SMTP is what you're looking for.
   --  NOTE:
   --    The charset is _hardcoded_ to ISO-8859-1 in the Send function.

end Simple_Email;
