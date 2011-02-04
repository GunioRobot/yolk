-------------------------------------------------------------------------------
--                                                                           --
--                                  Yolk                                     --
--                                                                           --
--                             Configuration                                 --
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

with AWS.Config.Set;

package body Yolk.Configuration is

   ---------------------------
   -- Get_AWS_Configuration --
   ---------------------------

   function Get_AWS_Configuration return AWS.Config.Object
   is

      Object : AWS.Config.Object;

   begin

      AWS.Config.Set.Accept_Queue_Size
        (O     => Object,
         Value => Config.Get (Accept_Queue_Size));

      AWS.Config.Set.Admin_Password
        (O     => Object,
         Value => Config.Get (Admin_Password));

      AWS.Config.Set.Admin_URI
        (O     => Object,
         Value => Config.Get (Admin_URI));

      AWS.Config.Set.Case_Sensitive_Parameters
        (O     => Object,
         Value => Config.Get (Case_Sensitive_Parameters));

      AWS.Config.Set.Certificate
        (O        => Object,
         Filename => Config.Get (Certificate));

      AWS.Config.Set.Check_URL_Validity
        (O     => Object,
         Value => Config.Get (Check_URL_Validity));

      AWS.Config.Set.Cleaner_Client_Data_Timeout
        (O     => Object,
         Value => Config.Get (Cleaner_Client_Data_Timeout));

      AWS.Config.Set.Cleaner_Client_Header_Timeout
        (O     => Object,
         Value => Config.Get (Cleaner_Client_Header_Timeout));

      AWS.Config.Set.Cleaner_Server_Response_Timeout
        (O     => Object,
         Value => Config.Get (Cleaner_Server_Response_Timeout));

      AWS.Config.Set.Cleaner_Wait_For_Client_Timeout
        (O     => Object,
         Value => Config.Get (Cleaner_Wait_For_Client_Timeout));

      AWS.Config.Set.Context_Lifetime
        (Value => Config.Get (Context_Lifetime));

      AWS.Config.Set.Directory_Browser_Page
        (O     => Object,
         Value => Config.Get (System_Templates_Path) & "/aws_directory.tmpl");

      AWS.Config.Set.Error_Log_Filename_Prefix
        (O     => Object,
         Value => Config.Get (Error_Log_Filename_Prefix));

      AWS.Config.Set.Error_Log_Split_Mode
        (O     => Object,
         Value => Config.Get (Error_Log_Split_Mode));

      AWS.Config.Set.Exchange_Certificate
        (O     => Object,
         Value => Config.Get (Exchange_Certificate));

      AWS.Config.Set.Force_Client_Data_Timeout
        (O     => Object,
         Value => Config.Get (Force_Client_Data_Timeout));

      AWS.Config.Set.Force_Client_Header_Timeout
        (O     => Object,
         Value => Config.Get (Force_Client_Header_Timeout));

      AWS.Config.Set.Force_Server_Response_Timeout
        (O     => Object,
         Value => Config.Get (Force_Server_Response_Timeout));

      AWS.Config.Set.Force_Wait_For_Client_Timeout
        (O     => Object,
         Value => Config.Get (Force_Wait_For_Client_Timeout));

      AWS.Config.Set.Free_Slots_Keep_Alive_Limit
        (O     => Object,
         Value => Config.Get (Free_Slots_Keep_Alive_Limit));

      AWS.Config.Set.Hotplug_Port
        (O     => Object,
         Value => Config.Get (Hotplug_Port));

      AWS.Config.Set.Keep_Alive_Force_Limit
        (O     => Object,
         Value => Config.Get (Keep_Alive_Force_Limit));

      AWS.Config.Set.Key
        (O        => Object,
         Filename => Config.Get (Key));

      AWS.Config.Set.Line_Stack_Size
        (O     => Object,
         Value => Config.Get (Line_Stack_Size));

      AWS.Config.Set.Log_Extended_Fields
        (O     => Object,
         Value => Config.Get (Log_Extended_Fields));

      AWS.Config.Set.Log_File_Directory
        (O     => Object,
         Value => Config.Get (Log_File_Directory));

      AWS.Config.Set.Log_Filename_Prefix
        (O     => Object,
         Value => Config.Get (Log_Filename_Prefix));

      AWS.Config.Set.Log_Size_Limit
        (O     => Object,
         Value => Config.Get (Log_Size_Limit));

      AWS.Config.Set.Log_Split_Mode
        (O     => Object,
         Value => Config.Get (Log_Split_Mode));

      AWS.Config.Set.Max_Concurrent_Download
        (Value => Config.Get (Max_Concurrent_Download));

      AWS.Config.Set.Max_Connection
        (O     => Object,
         Value => Config.Get (Max_Connection));

      AWS.Config.Set.MIME_Types
        (O     => Object,
         Value => Config.Get (MIME_Types));

      AWS.Config.Set.Receive_Timeout
        (O     => Object,
         Value => Config.Get (Receive_Timeout));

      AWS.Config.Set.Reuse_Address
        (O     => Object,
         Value => Config.Get (Reuse_Address));

      AWS.Config.Set.Security
        (O     => Object,
         Value => Config.Get (Security));

      AWS.Config.Set.Security_Mode
        (O    => Object,
         Mode => Config.Get (Security_Mode));

      AWS.Config.Set.Send_Timeout
        (O     => Object,
         Value => Config.Get (Send_Timeout));

      AWS.Config.Set.Server_Host
        (O     => Object,
         Value => Config.Get (Server_Host));

      AWS.Config.Set.Server_Name
        (O     => Object,
         Value => Config.Get (Server_Name));

      AWS.Config.Set.Server_Port
        (O     => Object,
         Value => Config.Get (Server_Port));

      AWS.Config.Set.Session
        (O     => Object,
         Value => Config.Get (Session));

      AWS.Config.Set.Session_Cleanup_Interval
        (Value => Config.Get (Session_Cleanup_Interval));

      AWS.Config.Set.Session_Lifetime
        (Value => Config.Get (Session_Lifetime));

      AWS.Config.Set.Session_Name
        (O     => Object,
         Value => Config.Get (Session_Name));

      AWS.Config.Set.Status_Page
        (O     => Object,
         Value => Config.Get (Status_Page));

      AWS.Config.Set.Transient_Cleanup_Interval
        (Value => Config.Get (Transient_Cleanup_Interval));

      AWS.Config.Set.Transient_Lifetime
        (Value => Config.Get (Transient_Lifetime));

      AWS.Config.Set.Upload_Directory
        (O     => Object,
         Value => Config.Get (Upload_Directory));

      AWS.Config.Set.Upload_Size_Limit
        (O     => Object,
         Value => Config.Get (Upload_Size_Limit));

      AWS.Config.Set.WWW_Root
        (O     => Object,
         Value => Config.Get (WWW_Root));

      return Object;

   end Get_AWS_Configuration;

end Yolk.Configuration;
