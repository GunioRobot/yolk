-------------------------------------------------------------------------------
--                                                                           --
--                                  Yolk                                     --
--                                                                           --
--                                  View                                     --
--                                                                           --
--                                  SPEC                                     --
--                                                                           --
--                   Copyright (C) 2010-2012, Thomas Løcke                   --
--                                                                           --
--  This is free software;  you can redistribute it and/or modify it         --
--  under terms of the  GNU General Public License  as published by the      --
--  Free Software  Foundation;  either version 3,  or (at your  option) any  --
--  later version. This library is distributed in the hope that it will be   --
--  useful, but WITHOUT ANY WARRANTY;  without even the implied warranty of  --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     --
--  You should have received a copy of the GNU General Public License and    --
--  a copy of the GCC Runtime Library Exception along with this program;     --
--  see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
--  <http://www.gnu.org/licenses/>.                                          --
--                                                                           --
-------------------------------------------------------------------------------

--  The main view file. Resources shared between view.* packages are declared
--  here.

with Ada.Strings.Unbounded;
with AWS.MIME;
with AWS.Status;
with AWS.Response;
with AWS.Templates;
with GNATCOLL.SQL.Exec;
with GNATCOLL.SQL.Postgres;
with My_Configuration;
with Yolk.Cache.Discrete_Keys;

package View is

   use Ada.Strings.Unbounded;
   use AWS.MIME;
   use Yolk;

   package My renames My_Configuration;
   --  Easier to write, easier to read.

   DB_Description : GNATCOLL.SQL.Exec.Database_Description :=
                      GNATCOLL.SQL.Postgres.Setup
                        (Database      => My.Config.Get (My.DB_Name),
                         User          => My.Config.Get (My.DB_User),
                         Host          => My.Config.Get (My.DB_Host),
                         Password      => My.Config.Get (My.DB_Password),
                         SSL           => GNATCOLL.SQL.Postgres.Disable);
   --  A GNATColl database description object.

   type Cache_Keys is (Feed_Data);

   package Cache is new Yolk.Cache.Discrete_Keys
     (Key_Type        => Cache_Keys,
      Element_Type    => Unbounded_String,
      Max_Element_Age => 10.0);
   --  Some pages are expensive to build, so we cache them. Here we test the
   --  Discrete_Keys cache.

   function Build_Response
     (Status_Data    : in AWS.Status.Data;
      Content        : in String;
      MIME_Type      : in String := Text_HTML)
      return AWS.Response.Data;
   --  Build the resource response.
   --  This is a convenience function that gets rid of a few with clauses in
   --  the files for the View child packages.

   function Build_Response
     (Status_Data    : in AWS.Status.Data;
      Template_File  : in String;
      Translations   : in AWS.Templates.Translate_Set;
      MIME_Type      : in String := Text_HTML)
      return AWS.Response.Data;
   --  Build the resource response.
   --  This is a convenience function that gets rid of a few with clauses in
   --  the files for the View child packages.
   --  This one is just a wrapper for the first Build_Response function. With
   --  this one you can add the template file and translate set directly,
   --  instead of having to parse those in the view.* child package.

end View;
