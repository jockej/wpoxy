with Ada.Streams; use Ada.Streams;

package Websocket is
  
  Websock_Parse_Exception : exception;
  
  type Mask_Type is mod 2**32;
  for Mask_Type'Size use 32;
  
  procedure Make_Client_Handshake(Host, Resource, User_Auth, Key : String;
                                  Buffer : out Stream_Element_Array;
                                  Last : out Stream_Element_Offset);
  
  procedure Make_Server_Handshake(Buffer : in out Stream_Element_Array;
                                  Last : in out Stream_Element_Offset;
                                  User_Auth : String;
                                  Valid : out Boolean);
  
  function Server_Response_Valid(Answer : Stream_Element_Array;
                                 Key : String) return Boolean;
  
  function Get_Key return String;
  
  procedure To_WS(Data : in Stream_Element_Array;
                  WS_Data : out Stream_Element_Array;
                  Last : out Stream_Element_Offset;
                  Mask : Boolean);
  
  procedure From_WS(WS_Data : in Stream_Element_Array;
                    Data : out Stream_Element_Array;
                    Last : out Stream_Element_Offset);
  
  procedure Make_Close_Frame(Buffer : out Stream_Element_Array;
                             Last : out Stream_Element_Offset);
  
  function Is_Close_Frame(Buffer : Stream_Element_Array) return Boolean;
  
end Websocket;
