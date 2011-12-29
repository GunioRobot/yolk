-------------------------------------------------------------------------------
--                                                                           --
--                                  Yolk                                     --
--                                                                           --
--                                Yolk.Log                                   --
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

with GNATCOLL.Traces.Syslog;
with Yolk.Configuration;

package body Yolk.Log is

   AWS_Access : GNATCOLL.Traces.Trace_Handle;
   AWS_Error  : GNATCOLL.Traces.Trace_Handle;

   Handle_Array : array (Trace_Handles) of GNATCOLL.Traces.Trace_Handle;

   procedure Initialize;
   --  Setup and activate the available trace handles.

   -----------------------------
   --  AWS_Access_Log_Writer  --
   -----------------------------

   procedure AWS_Access_Log_Writer
     (Message : in String)
   is
   begin
      GNATCOLL.Traces.Trace (Handle   => AWS_Access,
                             Message  => Message);
   end AWS_Access_Log_Writer;

   ----------------------------
   --  AWS_Error_Log_Writer  --
   ----------------------------

   procedure AWS_Error_Log_Writer
     (Message : in String)
   is
   begin
      GNATCOLL.Traces.Trace (Handle   => AWS_Error,
                             Message  => Message);
   end AWS_Error_Log_Writer;

   ------------------
   --  Initialize  --
   ------------------

   procedure Initialize
   is
      use Yolk.Configuration;
   begin
      GNATCOLL.Traces.Syslog.Register_Syslog_Stream;

      AWS_Access := GNATCOLL.Traces.Create
        (Unit_Name => "AWS_ACCESS",
         Default   => GNATCOLL.Traces.On,
         Stream    =>
           "&syslog:" & Config.Get (AWS_Access_Syslog_Facility_Level));

      AWS_Error := GNATCOLL.Traces.Create
        (Unit_Name => "AWS_ERROR",
         Default   => GNATCOLL.Traces.On,
         Stream    =>
           "&syslog:" & Config.Get (AWS_Error_Syslog_Facility_Level));

      Handle_Array (Error) := GNATCOLL.Traces.Create
        (Unit_Name => "ERROR",
         Default   => GNATCOLL.Traces.On,
         Stream    => "&syslog:user:info");

      Handle_Array (Info) := GNATCOLL.Traces.Create
        (Unit_Name => "INFO",
         Default   => GNATCOLL.Traces.On,
         Stream    => "&syslog:user:info");

      Handle_Array (SQL) := GNATCOLL.Traces.Create
        (Unit_Name => "SQL",
         Default   => GNATCOLL.Traces.On,
         Stream    => "&syslog:user:info");

      Handle_Array (SQL_Cache) := GNATCOLL.Traces.Create
        (Unit_Name => "SQL.CACHE",
         Default   => GNATCOLL.Traces.On,
         Stream    => "&syslog:user:info");

      Handle_Array (SQL_Error) := GNATCOLL.Traces.Create
        (Unit_Name => "SQL.ERROR",
         Default   => GNATCOLL.Traces.On,
         Stream    => "&syslog:user:info");

      Handle_Array (SQL_Select) := GNATCOLL.Traces.Create
        (Unit_Name => "SQL.SELECT",
         Default   => GNATCOLL.Traces.On,
         Stream    => "&syslog:user:info");

      GNATCOLL.Traces.Set_Active
        (Handle => AWS_Access,
         Active => Config.Get (Activate_AWS_Access_Log));

      GNATCOLL.Traces.Set_Active
        (Handle => AWS_Error,
         Active => Config.Get (Activate_AWS_Error_Log));

      GNATCOLL.Traces.Set_Active
        (Handle => Handle_Array (Error),
         Active => Config.Get (Activate_Error_Log));

      GNATCOLL.Traces.Set_Active
        (Handle => Handle_Array (Info),
         Active => Config.Get (Activate_Info_Log));

      GNATCOLL.Traces.Set_Active
        (Handle => Handle_Array (SQL),
         Active => Config.Get (Activate_SQL_Log));

      GNATCOLL.Traces.Set_Active
        (Handle => Handle_Array (SQL_Cache),
         Active => Config.Get (Activate_SQL_Cache_Log));

      GNATCOLL.Traces.Set_Active
        (Handle => Handle_Array (SQL_Error),
         Active => Config.Get (Activate_SQL_Error_Log));

      GNATCOLL.Traces.Set_Active
        (Handle => Handle_Array (SQL_Select),
         Active => Config.Get (Activate_SQL_Select_Log));
   end Initialize;

   -------------
   --  Trace  --
   -------------

   procedure Trace
     (Handle  : in Trace_Handles;
      Message : in String)
   is
   begin
      GNATCOLL.Traces.Trace (Handle   => Handle_Array (Handle),
                             Message  => Message);
   end Trace;
begin
   Initialize;
end Yolk.Log;
