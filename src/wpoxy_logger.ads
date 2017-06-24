with Ada.Streams; use Ada.Streams;

package Wpoxy_Logger is
  
  subtype Log_Level is Integer range 0 .. 6;
  procedure Set_Log_Level(New_Level : Log_Level);
  procedure Wpoxy_Log(Level : Log_Level; Msg : String);
  procedure Wpoxy_Log(Arr : Stream_Element_Array);
  
end Wpoxy_Logger;
