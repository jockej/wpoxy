with Ada.Streams; use Ada.Streams;
with Base64;

package body Wpoxy_Utils is
  
  function To_Stream_Element_Array(Str : String)
                                  return Stream_Element_Array is
    First : constant Stream_Element_Offset := Stream_Element_Offset(Str'First);
    Last : Stream_Element_Offset := Stream_Element_Offset(Str'Last);
    Arr : Stream_Element_Array(First..Last);
  begin
    for I in Str'Range loop
      Arr(Stream_Element_Offset(I)) := Character'Pos(Str(I));
    end loop;
    return Arr;
  end To_Stream_Element_Array;
  
  function To_String(A : Stream_Element_Array) return String is
    First : constant Integer := Integer(A'First);
    Last : constant Integer := Integer(A'Last);
    Str : String(First..Last);
  begin
    for I in A'First..A'Last loop
      Str(Integer(I)) := Character'Val(A(I));
    end loop;
    return Str;
  end To_String;
  
  procedure To_Stream_Element_Array(Str : String;
                                    Arr : in out Stream_Element_Array;
                                    Last : out Stream_Element_Offset) is
  begin
    for I in Str'Range loop
      Arr(Arr'First + Stream_Element_Offset(I) - 1) :=
        Character'Pos(Str(I));
    end loop;
    Last := Stream_Element_Offset(Str'Last);
  end To_Stream_Element_Array;
  
  procedure To_Base64(Source : String;
                      Target : out Stream_Element_Array;
                      Last : out Stream_Element_Offset) is
    
    Source_Arr : Stream_Element_Array := To_Stream_Element_Array(Source);
    Target_Last : Natural := Base64.Encode_Length(Source_Arr'Length);
    Target_Str : String(1..Target_Last);
  begin
    Base64.Encode(Source_Arr, Target_Str, Target_Last);
    for I in 1..Target_Last loop
      Target(Stream_Element_Offset(I)) := Character'Pos(Target_Str(I));
    end loop;
    Last := Stream_Element_Offset(Target_Last);
  end To_Base64;
  
  function To_Base64(Source : String) return Stream_Element_Array is
    Source_Arr : Stream_Element_Array := To_Stream_Element_Array(Source);
    Target_Last : Natural := Base64.Encode_Length(Source_Arr'Length);
    Target_Str : String(1..Target_Last);
    Ret : Stream_Element_Array(1..Stream_Element_Offset(Target_Last));
  begin
    Base64.Encode(Source_Arr, Target_Str, Target_Last);
    for I in 1..Target_Last loop
      Ret(Stream_Element_Offset(I)) := Character'Pos(Target_Str(I));
    end loop;
    return Ret;
  end To_Base64;
  
  function To_Base64(Source : String) return String is
  begin
    return To_String(To_Base64(Source));
  end To_Base64;
  
end Wpoxy_Utils;
