with Ada.Streams; use Ada.Streams;

package Websocket is
  
  Websock_Parse_Exception : exception;
  
  type Mask_Type is mod 2**32;
  for Mask_Type'Size use 32;
  
  procedure Make_Client_Handshake(Host, Resource, User_Auth, Key : String;
                                  Buffer : out Stream_Element_Array;
                                  Last : out Stream_Element_Offset);
  
  procedure Make_Server_Handshake(Buffer : in out Stream_Element_Array;
                                  Last : out Stream_Element_Offset);
  
  --  procedure Check_Server_Response(Buffer : 
  
  function Get_Key return String;
  
  procedure To_WS(Data : in Stream_Element_Array;
                  WS_Data : out Stream_Element_Array;
                  Last : out Stream_Element_Offset;
                  Mask : Boolean);
  
  procedure From_WS(WS_Data : in Stream_Element_Array;
                    Data : out Stream_Element_Array;
                    Last : out Stream_Element_Offset);
  
end Websocket;
