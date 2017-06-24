with Ada.Streams; use Ada.Streams;
with Ada.Numerics.Discrete_Random;
with Ada.Unchecked_Conversion;

with GNAT.Regpat; use GNAT.Regpat;
with GNAT.SHA1;

with Wpoxy_Logger; use Wpoxy_Logger;
with Wpoxy_Utils; use Wpoxy_Utils;

package body Websocket is
  
  WSGUID :constant String :=
    "258EAFA5-E914-47DA-95CA-C5AB0DC85B11";
  
  subtype Opcode_Type is Integer range 0..(2**4 - 1);
  subtype Payload_Len_Type is Integer range 0..(2**7 - 1);
  subtype Bit is Integer range 0..1;
  
  type WS_Header is
     record
       FIN, RSV1, RSV2, RSV3 : Bit;
       Opcode : Opcode_Type;
       Mask : Bit;
       Payload_Len : Payload_Len_Type;
     end record;
  
  for WS_Header use record
    FIN at 0 range 0..0;
    RSV1 at 0 range 1..1;
    RSV2 at 0 range 2..2;
    RSV3 at 0 range 3..3;
    Opcode at 0 range 4..7;
    Mask at 0 range 8..8;
    Payload_Len at 0 range 9..15;
  end record;
  
  for WS_Header'Size use 16;
  
  subtype Header_Bytes is Stream_Element_Array(1..2);
  function Header_To_Bytes is
    new Ada.Unchecked_Conversion(Source => WS_Header, Target => Header_Bytes);
  function Bytes_To_Header is
    new Ada.Unchecked_Conversion(Source => Header_Bytes, Target => WS_Header);
  
  subtype Mask_Bytes is Stream_Element_Array(1..4);
  function Mask_To_Bytes is
    new Ada.Unchecked_Conversion(Source => Mask_Type, Target => Mask_Bytes);
  function Bytes_To_Mask is
    new Ada.Unchecked_Conversion(Source => Mask_Bytes, Target => Mask_Type);
  
  package Random_Mask is new Ada.Numerics.Discrete_Random(Mask_Type);
  package Random_Stream_Element is new
    Ada.Numerics.Discrete_Random(Stream_Element);
  
  Gen : Random_Mask.Generator;
  
  procedure To_WS(Data : in Stream_Element_Array;
                  WS_Data : out Stream_Element_Array;
                  Last : out Stream_Element_Offset;
                  Mask : Boolean) is
    Payload_Len : Stream_Element_Offset := Data'Last - Data'First;
  begin
    raise Program_Error with "To_WS not implemented";
  end To_WS;
    
  procedure From_WS(WS_Data : in Stream_Element_Array;
                    Data : out Stream_Element_Array;
                    Last : out Stream_Element_Offset) is
  begin
    raise Program_Error with "From_WS not implemented";
  end From_WS;
  
  Nonce_Gen : Random_Stream_Element.Generator;
  function Get_Key return String is
    Arr : Stream_Element_Array(1..16);
  begin
    for I in Arr'Range loop
      Arr(I) := Random_Stream_Element.Random(Nonce_Gen);
    end loop;
    return To_String(Arr);
  end Get_Key;
  
  CRLF : constant String := ASCII.CR & ASCII.LF;
  
  procedure Make_Client_Handshake(Host, Resource, User_Auth, Key : String;
                                  Buffer : out Stream_Element_Array;
                                  Last : out Stream_Element_Offset) is
    Request : String :=
      "GET /" & Resource & "HTTP/1.1" & CRLF &
      "Host: " & Host & CRLF &
      "Upgrade: websocket" & CRLF &
      "Connection: Upgrade" & CRLF &
      "Sec-WebSocket-Protocol: wpoxy" & CRLF &
      "Sec-WebSocket-Version: 13" & CRLF &
      "Basic-Auth: " & User_Auth & CRLF &
      "Sec-WebSocket-Key: " & Key & CRLF;
  begin
    To_Stream_Element_Array(Request, Buffer, Last);
  end Make_Client_Handshake;
  
  procedure Make_Server_Handshake(Buffer : in out Stream_Element_Array;
                                  Last : out Stream_Element_Offset) is
    
    Request : String := To_String(Buffer);
    
    function Get_Key return String is
      Re : constant Pattern_Matcher :=
        Compile("Sec-WebSocket-Key: (.*)" & CRLF);
      Matches : Match_Array(1..1);
    begin
      Match(Re, Request, Matches);
      if Matches(1) = No_Match then
        raise Websock_Parse_Exception with "Could not find key";
      end if;
      return Request(Matches(1).First..Matches(1).Last);
    end Get_Key;
    
    Key : String := Get_Key;
    
    function Build_Accept return String is
      Accept_Str : String := Key & WSGUID;
      Accept_SHA1 : String := GNAT.SHA1.Digest(Accept_Str);
      SLast : Natural;
      Accept_Base64 : String(1..256);
    begin
      Base64.Encode(To_Stream_Element_Array(Accept_SHA1),
                    Accept_Base64, SLast);
      return Accept_Base64(1..Last);
    end Build_Accept;
    
    OK_Resp : String :=
      "HTTP/1.1 101 Switching Protocols" & CRLF &
      "Upgrade: websocket" & CRLF &
      "Connection: Upgrade" & CRLF &
      "Sec-WebSocket-Accept: " & Build_Accept & CRLF;
      
    Fail_Response : constant String := 
      "HTTP/1.1 403 Connection Refused" & CRLF;
  begin
    
  end Make_Server_Handshake;
  
  
  
  
end Websocket;
