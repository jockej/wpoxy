with Ada.Streams; use Ada.Streams;
with Ada.Numerics.Discrete_Random;
with Ada.Unchecked_Conversion;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Exceptions; use Ada.Exceptions;

with GNAT.Regpat; use GNAT.Regpat;
with GNAT.SHA1;

with Base64; use Base64;

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
  
  subtype Mask_Bytes is Stream_Element_Array(0..3);
  function Mask_To_Bytes is
    new Ada.Unchecked_Conversion(Source => Mask_Type, Target => Mask_Bytes);
  function Mask_From_Bytes is
    new Ada.Unchecked_Conversion(Source => Mask_Bytes, Target => Mask_Type);
  
  package Random_Mask is new Ada.Numerics.Discrete_Random(Mask_Type);
  package Random_Stream_Element is new
    Ada.Numerics.Discrete_Random(Stream_Element);
  
  Mask_Gen : Random_Mask.Generator;
  
  procedure Mask_Array(Source : in Stream_Element_Array;
                       Target : out Stream_Element_Array;
                       Mask : Mask_Type) is
    M : Mask_Bytes := Mask_To_Bytes(Mask);
    --  Full_Rounds : Natural := Source'Length / 4;
    --  Rest : Natural := Source'Length mod 4;
  begin
    if Source'Length /= Target'Length then
      raise Program_Error with "Arrays unequal in Mask_Array";
    end if;
    --  for I in 0..Full_Rounds - 1 loop
    --    declare
    --      Idx : Stream_Element_Offset := Stream_Element_Offset(I);
    --      Source_Mask : Mask_Type :=
    --        Bytes_To_Mask(Source(Idx * 4 + 1 .. Idx * 4 + 4));
    --    begin
    --      Target(Idx * 4 + 1..Idx * 4 + 4) := Mask_To_Bytes(Source_Mask xor Mask);
    --    end;
    --  end loop;
    
    for I in 0..Source'Length - 1 loop
      declare
        SI : Stream_Element_Offset := Stream_Element_Offset(I);
        Mask_Byte : Stream_Element := M((SI) mod 4);
      begin
        Target(Target'First + SI) :=
          Source(Source'First + SI) xor Mask_Byte;
      end;
    end loop;
  end Mask_Array;
  
  procedure To_WS(Data : in Stream_Element_Array;
                  WS_Data : out Stream_Element_Array;
                  Last : out Stream_Element_Offset;
                  Mask : Boolean) is
    
    function To_BE16(N : Natural) return Stream_Element_Array is
      HB : Stream_Element := Stream_Element(N / 2**8);
      LB : Stream_Element := Stream_Element(N - ((N / 2**8) * 2**8));
    begin
      return A : Stream_Element_Array(1..2) do
        A(1) := HB;
        A(2) := LB;
      end return;
    end To_BE16;
    
    function To_BE64(N : Natural) return Stream_Element_Array is
      B1 : Stream_Element := 0;
      B2 : Stream_Element := 0;
      B3 : Stream_Element := 0;
      B4 : Stream_Element := 0;
      B5 : Stream_Element := Stream_Element(N / 2**24);
      B6 : Stream_Element := Stream_Element((N - Natural(B5)) / 2**16);
      B7 : Stream_Element := Stream_Element((N - Natural(B5 + B6)) / 2**8);
      B8 : Stream_Element := Stream_Element(N - Natural(B5 + B6 + B7));
    begin
      return A : Stream_Element_Array(1..8) do
        A(1) := B1;
        A(2) := B2;
        A(3) := B3;
        A(4) := B4;
        A(5) := B5;
        A(6) := B6;
        A(7) := B7;
        A(8) := B8;
      end return;
    end To_BE64;
    
    Payload_Len : Natural := Data'Length;
    Payload_Offs : Stream_Element_Offset := Stream_Element_Offset(Payload_Len);
    First : Stream_Element_Offset := WS_Data'First;
    Current : Stream_Element_Offset := First + 2;
    Header : WS_Header := (FIN => 1,
                           Opcode => 2,
                           others => 0);
  begin
    Wpoxy_Log(5, "To_WS: Sending " & Payload_Len'Img & " bytes");
    if Payload_Len < 126 then
      Header.Payload_Len := Payload_Len;
    elsif Payload_Len > 125 and then Payload_Len < (2**16 - 1) then
      Wpoxy_Log(5, "Setting Header.Payload_Len to 126");
      Header.Payload_Len := 126;
      WS_Data(Current..Current + 1) := To_BE16(Payload_Len);
      Current := Current + 2;
    else
      Header.Payload_Len := 127;
      WS_Data(Current..Current + 7) := To_BE64(Payload_Len);
      Current := Current + 8;
    end if;
    
    if Mask then
      declare
        New_Mask : Mask_Type := Random_Mask.Random(Mask_Gen);
      begin
        WS_Data(Current..Current + 3) := Mask_To_Bytes(New_Mask);
        Current := Current + 4;
        Header.Mask := 1;
        Mask_Array(Data, WS_Data(Current..Current + Payload_Offs - 1), New_Mask);
      end;
    else 
      WS_Data(Current..Current + Payload_Offs - 1) := Data;
    end if;
    WS_Data(WS_Data'First..WS_Data'First + 1) := Header_To_Bytes(Header);
    Last := Current + Payload_Offs - 1;
  exception
     when Error: others =>
       Wpoxy_Log(5, "In To_WS:");
       Wpoxy_Log(5, Exception_Name(Error) & ": " & Exception_Message(Error));
  end To_WS;
    
  procedure From_WS(WS_Data : in Stream_Element_Array;
                    Data : out Stream_Element_Array;
                    Last : out Stream_Element_Offset) is
    
    function From_BE16(A : Stream_Element_Array) return Stream_Element_Offset is
      HB : Stream_Element_Offset := Stream_Element_Offset(A(A'First));
      LB : Stream_Element_Offset := Stream_Element_Offset(A(A'First + 1));
    begin
      return HB * 2**8 + LB;
    end From_BE16;
    
    function From_BE64(A : Stream_Element_Array) return Stream_Element_Offset is
      B1 : Stream_Element_Offset := Stream_Element_Offset(A(A'First));
      B2 : Stream_Element_Offset := Stream_Element_Offset(A(A'First + 1));
      B3 : Stream_Element_Offset := Stream_Element_Offset(A(A'First + 2));
      B4 : Stream_Element_Offset := Stream_Element_Offset(A(A'First + 3));
      B5 : Stream_Element_Offset := Stream_Element_Offset(A(A'First + 4));
      B6 : Stream_Element_Offset := Stream_Element_Offset(A(A'First + 5));
      B7 : Stream_Element_Offset := Stream_Element_Offset(A(A'First + 6));
      B8 : Stream_Element_Offset := Stream_Element_Offset(A(A'First + 7));
    begin
      return B1 * 2**56 + B2 * 2**48 + B3 * 2**40 + B4 * 2**32 +
        B5 * 2**24 + B6 * 2**16 + B7 * 2**8 + B8;
    end From_BE64;
    
    Header : constant WS_Header := Bytes_To_Header(WS_Data(1..2));
    Payload_Len : constant Natural := Header.Payload_Len;
    Current : Stream_Element_Offset := WS_Data'First + 2;
    Payload_Offs : Stream_Element_Offset;
    Mask : Mask_Type;
  begin
    Wpoxy_Log(5, "From_WS: Got " & Payload_Len'Img & " as payload len");
    if Payload_Len < 126 then
      Payload_Offs := Stream_Element_Offset(Payload_Len);
    elsif Payload_Len = 126 then
      Payload_Offs := From_BE16(WS_Data(Current..Current + 1));
      Current := Current + 2;
    else
      Payload_Offs := From_BE64(WS_Data(Current..Current + 7));
      Current := Current + 8;
    end if;
    
    if Header.Mask = 1 then
      Mask := Mask_From_Bytes(WS_Data(Current..Current + 3));
      Current := Current + 4;
      Mask_Array(WS_Data(Current..Current + Payload_Offs - 1),
                 Data(Data'First..Data'First + Payload_Offs - 1),
                 Mask);
    else
      Data(Data'First..Data'First + Payload_Offs - 1) :=
        WS_Data(Current..Current + Payload_Offs - 1);
    end if;
    Last := Data'First + Payload_Offs - 1;
  exception
     when Error: others =>
       Wpoxy_Log(5, "In From_WS:");
       Wpoxy_Log(5, Exception_Name(Error) & ": " & Exception_Message(Error));

  end From_WS;
  
  Nonce_Gen : Random_Stream_Element.Generator;
  function Get_Key return String is
    Str : String(1..64);
    Last : Natural;
    Arr : Stream_Element_Array(1..16);
  begin
    for I in Arr'Range loop
      Arr(I) := Random_Stream_Element.Random(Nonce_Gen);
    end loop;
    Base64.Encode(Arr, Str, Last);
    return Str(1..Last);
  end Get_Key;
  
  CRLF : constant String := ASCII.CR & ASCII.LF;
  
  procedure Make_Client_Handshake(Host, Resource, User_Auth, Key : String;
                                  Buffer : out Stream_Element_Array;
                                  Last : out Stream_Element_Offset) is
    Request : String :=
      "GET /" & Resource & " HTTP/1.1" & CRLF &
      "Host: " & Host & CRLF &
      "Upgrade: websocket" & CRLF &
      "Connection: Upgrade" & CRLF &
      "Sec-WebSocket-Protocol: wpoxy" & CRLF &
      "Sec-WebSocket-Version: 13" & CRLF &
      "Basic-Auth: " & To_Base64(User_Auth) & CRLF &
      "Sec-WebSocket-Key: " & Key & CRLF;
  begin
    To_Stream_Element_Array(Request, Buffer, Last);
  end Make_Client_Handshake;
  
  function Build_Accept(Key : String) return String is
    Accept_Str : String := Key & WSGUID;
    Accept_SHA1 : String := GNAT.SHA1.Digest(Accept_Str);
    SLast : Natural;
    Accept_Base64 : String(1..256);
  begin
    Base64.Encode(To_Stream_Element_Array(Accept_SHA1),
                  Accept_Base64, SLast);
    return Accept_Base64(1..SLast);
  end Build_Accept;
  
  function Get_Header_Field(Request, Field : String) return String is
      Re : constant Pattern_Matcher :=
        Compile(Field & ": (.*)\r\n");
      Matches : Match_Array(1..12);
    begin
      Match(Re, Request, Matches);
      if Matches(1) = No_Match then
        raise Websock_Parse_Exception with "Could not find field " & Field;
      end if;
      return Trim(Request(Matches(1).First..Matches(1).Last), Both);
    end Get_Header_Field;
  
  procedure Make_Server_Handshake(Buffer : in out Stream_Element_Array;
                                  Last : in out Stream_Element_Offset;
                                  User_Auth : String;
                                  Valid : out Boolean) is
    
    Request : String := To_String(Buffer(Buffer'First..Last));
    
    Key : String := Get_Header_Field(Request, "Sec-WebSocket-Key");
    
    OK_Resp : String :=
      "HTTP/1.1 101 Switching Protocols" & CRLF &
      "Upgrade: websocket" & CRLF &
      "Connection: Upgrade" & CRLF &
      "Sec-WebSocket-Accept: " & Build_Accept(Key) & CRLF;
      
    Fail_Response : constant String := 
      "HTTP/1.1 403 Connection Refused" & CRLF;
    
    function Valid_Handshake return Boolean is
    begin
      if Get_Header_Field(Request, "Sec-WebSocket-Version") = "13" and then
        Get_Header_Field(Request, "Sec-WebSocket-Protocol") = "wpoxy" and then
        Get_Header_Field(Request, "Upgrade") = "websocket" and then
        Get_Header_Field(Request, "Connection") = "Upgrade" then
        return True;
      else
        return False;
      end if;
    end Valid_Handshake;
  begin
    if Valid_Handshake and then
      Get_Header_Field(Request, "Basic-Auth") = To_Base64(User_Auth) then
      Wpoxy_Log(4, "Handshake OK");
      Valid := True;
      To_Stream_Element_Array(OK_Resp, Buffer, Last);
    else
      Wpoxy_Log(4, "Invalid handshake");
      Wpoxy_Log(5, "Header says: " & Get_Header_Field(Request, "Basic-Auth") &
                  " User_Auth says: " & To_Base64(User_Auth));
      Valid := False;
      To_Stream_Element_Array(Fail_Response, Buffer, Last);
    end if;
  exception
     when Error : Websock_Parse_Exception =>
       Wpoxy_Log(2, "Error parsing websocket request: " & Request);
  end Make_Server_Handshake;
  
  function Server_Response_Valid(Answer : Stream_Element_Array;
                                 Key : String) return Boolean is
    
    Answer_Str : String := To_String(Answer);
    Acc : String := Get_Header_Field(Answer_Str,
                                     "Sec-WebSocket-Accept");
    Ref_Acc : String := Build_Accept(Key);
    
    function Get_HTML_Status return String is
      Re : constant Pattern_Matcher := Compile("HTTP/1.1 ([0-9]{3})");
      Matches : Match_Array(1..1);
    begin
      Match(Re, Answer_Str, Matches);
      if Matches(1) = No_Match then
        raise Websock_Parse_Exception with "Couldn't find HTML status code in "
          & Answer_Str;
      end if;
      return Answer_Str(Matches(1).First..Matches(1).Last);
    end Get_HTML_Status;

  begin
    return Acc = Ref_Acc and Get_HTML_Status = "101";
  end Server_Response_Valid;
  
  procedure Make_Close_Frame(Buffer : out Stream_Element_Array;
                             Last : out Stream_Element_Offset) is
    Header : WS_Header := (FIN => 1,
                           Opcode => 8,
                           others => 0);
  begin
    Last := Buffer'First + 1;
    Buffer(Buffer'First..Last) := Header_To_Bytes(Header);
  end Make_Close_Frame;
  
  function Is_Close_Frame(Buffer : Stream_Element_Array) return Boolean is
    Header : WS_Header :=
      Bytes_To_Header(Buffer(Buffer'First..Buffer'First + 1));
  begin
    return Header.FIN = 1 and Header.Opcode = 8;
  end Is_Close_Frame;
  
end Websocket;
