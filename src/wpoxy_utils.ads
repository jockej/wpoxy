with Ada.Streams; use Ada.Streams;
with Base64; use Base64;

package Wpoxy_Utils is
  
  function To_Stream_Element_Array(Str : String)
                                  return Stream_Element_Array;
  
  function To_String(A : Stream_Element_Array) return String;
  
  procedure To_Stream_Element_Array(Str : String;
                                    Arr : in out Stream_Element_Array;
                                    Last : out Stream_Element_Offset);
  
  procedure To_Base64(Source : String;
                      Target : out Stream_Element_Array;
                      Last : out Stream_Element_Offset);
  
  function To_Base64(Source : String) return Stream_Element_Array;
  
  function To_Base64(Source : String) return String;
  
end Wpoxy_Utils;
