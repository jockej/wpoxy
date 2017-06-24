with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Streams; use Ada.Streams;

package body Wpoxy_Logger is
  Current_Level : Log_Level;
  procedure Set_Log_Level(New_Level : Log_Level) is
  begin
    Current_Level := New_Level;
  end;
  
  
  procedure Wpoxy_Log(Level : Log_Level; Msg : String) is
  begin
    if Level >= Current_Level then
      Put_Line(Standard_Error, Msg);
    end if;
  end;
  
  procedure Wpoxy_Log(Arr : Stream_Element_Array) is
    Value : Integer;
  begin
    for I in Arr'Range loop
      Value := Integer'Val(Arr(I));
      Put(Value , Base=>16);
      if Value > 32 and then Value < 127 then
        Put("(" & Character'Val(Arr(I)) & ")");
      end if;
      if I < Arr'Last then
        Put(",");
      end if;
    end loop;
    New_Line;
  end Wpoxy_Log;

end Wpoxy_Logger;
