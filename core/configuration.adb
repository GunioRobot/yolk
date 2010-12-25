-------------------------------------------------------------------------------
--                                                                           --
--                                  Yolk                                     --
--                                                                           --
--                             configuration                                 --
--                                                                           --
--                                  BODY                                     --
--                                                                           --
--                     Copyright (C) 2010, Thomas Løcke                      --
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

with AWS.Config.Set;

package body Configuration is

   ---------------------------
   -- Get_AWS_Configuration --
   ---------------------------

   function Get_AWS_Configuration return AWS.Config.Object
   is

      Object : AWS.Config.Object;

   begin

      AWS.Config.Set.Parameter
        (Config        => Object,
         Name          => "Admin_Password",
         Value         => Config.Get (Admin_Password));

      AWS.Config.Set.Admin_URI
        (O     => Object,
         Value => Config.Get (Admin_URI));

      AWS.Config.Set.Certificate
        (O        => Object,
         Filename => Config.Get (Certificate));

      AWS.Config.Set.Error_Log_Filename_Prefix
        (O     => Object,
         Value => Config.Get (Error_Log_Filename_Prefix));

      AWS.Config.Set.Error_Log_Split_Mode
        (O     => Object,
         Value => Config.Get (Error_Log_Split_Mode));

      AWS.Config.Set.Log_File_Directory
        (O     => Object,
         Value => Config.Get (Log_File_Directory));

      AWS.Config.Set.Log_Filename_Prefix
        (O     => Object,
         Value => Config.Get (Log_Filename_Prefix));

      AWS.Config.Set.Parameter
        (Config        => Object,
         Name          => "Log_Size_Limit",
         Value         => Config.Get (Log_Size_Limit));

      AWS.Config.Set.Log_Split_Mode
        (O     => Object,
         Value => Config.Get (Log_Split_Mode));

      AWS.Config.Set.Max_Connection
        (O     => Object,
         Value => Config.Get (Max_Connection));

      AWS.Config.Set.Parameter
        (Config        => Object,
         Name          => "MIME_Types",
         Value         => Config.Get (MIME_Types));

      AWS.Config.Set.Reuse_Address
        (O     => Object,
         Value => Config.Get (Reuse_Address));

      AWS.Config.Set.Security
        (O     => Object,
         Value => Config.Get (Security));

      AWS.Config.Set.Security_Mode
        (O    => Object,
         Mode => Config.Get (Security_Mode));

      AWS.Config.Set.Server_Name
        (O     => Object,
         Value => Config.Get (Server_Name));

      AWS.Config.Set.Server_Port
        (O     => Object,
         Value => Config.Get (Server_Port));

      AWS.Config.Set.Session
        (O     => Object,
         Value => Config.Get (Session));

      AWS.Config.Set.Session_Lifetime
        (Value => Config.Get (Session_Lifetime));

      AWS.Config.Set.Session_Name
        (O     => Object,
         Value => Config.Get (Session_Name));

      AWS.Config.Set.Status_Page
        (O     => Object,
         Value => Config.Get (Status_Page));

      return Object;

   end Get_AWS_Configuration;

end Configuration;
