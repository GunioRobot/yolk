-------------------------------------------------------------------------------
--                                                                           --
--                                Yolk Demo                                  --
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

--  Feel free to use this demo application as the foundation for your own
--  applications.
--
--  Usually you just have to change the name of environment task and  the name
--  of the file itself to match whatever you want to call your application.

with Ada.Directories;
with Ada.Exceptions;
with AWS.Config;
with AWS.MIME;
with AWS.Server;
with AWS.Server.Log;
with AWS.Services.Dispatchers.URI;
with AWS.Session;
with My_Handlers;
with Yolk.Configuration;
with Yolk.Log;
with Yolk.Process_Control;
with Yolk.Process_Owner;
with Yolk.Static_Content;
with Yolk.Utilities;
with Yolk.Whoops;

procedure Yolk_Demo is

   use Ada.Exceptions;
   use My_Handlers;
   use Yolk.Configuration;
   use Yolk.Log;
   use Yolk.Process_Control;
   use Yolk.Process_Owner;
   use Yolk.Static_Content;
   use Yolk.Utilities;

   Resource_Handlers : AWS.Services.Dispatchers.URI.Handler;
   --  The various resource handlers. These are defined in the Yolk.Handlers
   --  and My_Handlers packages.

   Web_Server        : AWS.Server.HTTP;
   --  The main AWS webserver object.

   Web_Server_Config : constant AWS.Config.Object := Get_AWS_Configuration;
   --  Set the AWS configuration object.
   --  All AWS related configuration parameters can be found in the
   --  configuration/config.ini file. They are marked with:
   --    Used by AWS: Yes
   --  Default values are set in the Yolk.Configuration package.

   --------------------
   --  Start_Server  --
   --------------------

   procedure Start_Server;
   --  Start the AWS server. A short message is written to the Info log trace
   --  whenever the server is started.

   procedure Start_Server
   is
      use Ada.Directories;
   begin
      if AWS.Config.Session (Web_Server_Config)
        and then Exists (Config.Get (Session_Data_File))
      then
         AWS.Session.Load (Config.Get (Session_Data_File));
         --  If sessions are enabled and the Session_Data_File exists, then
         --  load the old session data.
      end if;

      AWS.Server.Start (Web_Server => Web_Server,
                        Dispatcher => Resource_Handlers,
                        Config     => Web_Server_Config);
      --  Unfortunately we have to start the server BEFORE we start the logs.
      --  If we start the logs first, then the log files aren't created in the
      --  Log_File_Directory directory, but instead they are created in the
      --  directory where the executable is.

      if Config.Get (AWS_Access_Log_Activate) then
         AWS.Server.Log.Start
           (Web_Server => Web_Server,
            Callback   => Yolk.Log.AWS_Access_Log_Writer'Access,
            Name       => "AWS Access Log");
      end if;

      if Config.Get (AWS_Error_Log_Activate) then
         AWS.Server.Log.Start_Error
           (Web_Server => Web_Server,
            Callback   => Yolk.Log.AWS_Error_Log_Writer'Access,
            Name       => "AWS Error Log");
         --  Start the access and error logs.
      end if;

      Trace (Handle  => Info,
             Message => "Started " &
             AWS.Config.Server_Name (Web_Server_Config));
      Trace (Handle  => Info,
             Message => "Yolk version " & Yolk.Version);
   end Start_Server;

   -------------------
   --  Stop_Server  --
   -------------------

   procedure Stop_Server;
   --  Stop the AWS server. A short message is written to the Info log trace
   --  whenever the server is stopped.

   procedure Stop_Server
   is
   begin
      if AWS.Config.Session (Web_Server_Config) then
         AWS.Session.Save (Config.Get (Session_Data_File));
         --  If sessions are enabled, then save the session data to the
         --  Session_Data_File.
      end if;

      AWS.Server.Shutdown (Web_Server);

      if AWS.Server.Log.Is_Active (Web_Server) then
         AWS.Server.Log.Stop (Web_Server);
      end if;

      if AWS.Server.Log.Is_Error_Active (Web_Server) then
         AWS.Server.Log.Stop_Error (Web_Server);
      end if;

      Trace (Handle  => Info,
             Message => "Stopped " &
             AWS.Config.Server_Name (Web_Server_Config));
   end Stop_Server;
begin
   Set_User (Username => Config.Get (Yolk_User));
   --  Switch user.

   Static_Content_Cache_Setup;
   --  Set the Cache-Control header options for the static content.
   --  Delete old compressed content and create a clean directory for
   --  the compressed static content.

   for Key in Keys'Range loop
      if TS (Default_Values (Key)) /= TS (Config.Get (Key)) then
         Trace (Handle  => Info,
                Message => "Configuration key " &
                Keys'Image (Key) & " is not default value.");
         Trace (Handle  => Info,
                Message => "    Default is: " & TS (Default_Values (Key)));
         Trace (Handle  => Info,
                Message => "    Set value is: " & TS (Config.Get (Key)));
      end if;
   end loop;
   --  Check for configuration settings where the setting differ from the
   --  default value, and notify the user of these on the Info log trace.

   AWS.MIME.Load (MIME_File => Config.Get (MIME_Types));
   --  Load the MIME type file. We need to do this here, because the AWS.MIME
   --  package has already been initialized with the default AWS configuration
   --  parameters, and in these the aws.mime file is placed in ./ whereas our
   --  aws.mime is in configuration/aws.mime.

   Set (RH => Resource_Handlers);
   --  Populate the Resource_Handlers object.

   AWS.Server.Set_Unexpected_Exception_Handler
     (Web_Server => Web_Server,
      Handler    => Yolk.Whoops.Unexpected_Exception_Handler'Access);
   --  Set the unexpected exception handler. If your app raises an exception
   --  and there's no handler for it, then it'll get handled by the
   --  Yolk.Whoops.Unexpected_Exception_Handler exception handler.

   Start_Server;
   --  Start the server.

   Trace (Handle  => Info,
          Message => "Starting " &
          AWS.Config.Server_Name (Web_Server_Config) &
          ". Listening on port" &
          AWS.Config.Server_Port (Web_Server_Config)'Img);
   --  We're alive! Log this fact to the Info trace.

   Wait;
   --  This is the main "loop". We will wait here as long as the
   --  Yolk.Process_Control.Controller.Check entry barrier is False.

   Stop_Server;
   --  Shutdown requested in Yolk.Process_Control.Controller, so we will
   --  attempt to shutdown the server.

exception
   when Event : others =>
      Trace (Handle  => Error,
             Message => Exception_Information (Event));
      --  Write the exception information to the rotating Error log trace.
      Stop_Server;

end Yolk_Demo;
