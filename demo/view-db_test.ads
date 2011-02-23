-------------------------------------------------------------------------------
--                                                                           --
--                                  Yolk                                     --
--                                                                           --
--                              View.DB_Test                                 --
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

with AWS.Response;
with AWS.Status;

package View.DB_Test is

   function Generate
     (Request : in AWS.Status.Data)
      return AWS.Response.Data;
   --  Generate the content for the /database resource.

end View.DB_Test;
