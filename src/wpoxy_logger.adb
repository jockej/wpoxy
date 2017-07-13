with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Streams; use Ada.Streams;
with Ada.Task_Identification; use Ada.Task_Identification;

package body Wpoxy_Logger is
  Current_Level : Log_Level;
  procedure Set_Log_Level(New_Level : Log_Level) is
  begin
    Current_Level := New_Level;
  end;
  
  function Get_Log_Level return Log_Level is
  begin
    return Current_Level;
  end Get_Log_Level;
  
  procedure Wpoxy_Log(Level : Log_Level; Msg : String) is
  begin
    if Level < Current_Level then
      return;
    end if;
    Put(Image(Current_Task) & ": ");
    Put(Standard_Error, Msg);
    New_Line;
  end;
  
  procedure Wpoxy_Log(Level : Log_Level;
                      Msg : String;
                      Data : Stream_Element_Array) is
    Value : Integer;
  begin
    if Level < Current_Level then
      return;
    end if;
    Wpoxy_Log(Level, Msg);
    for I in Data'Range loop
      Value := Integer'Val(Data(I));
      Put(Value, Base=>16);
      if Value > 32 and then Value < 127 then
        Put("(" & Character'Val(Data(I)) & ")");
      end if;
      if I < Data'Last then
        Put(",");
      end if;
    end loop;
    New_Line;
  end Wpoxy_Log;
  
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
