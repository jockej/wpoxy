with Ada.Text_IO; use Ada.Text_IO;
with Ada.Streams; use Ada.Streams;
with Ada.Unchecked_Conversion;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with GNAT.Sockets; use GNAT.Sockets;
with GNAT.String_Split; use GNAT.String_Split;
with Wpoxy_Logger; use Wpoxy_Logger;

package body SOCKS is

  function Get_SOCKS_Protocol(Code : in Stream_Element) return SOCKS_Protocol is
  begin
    if Code = 16#04# then
      return SOCKS4;
    elsif Code = 16#05# then
      return SOCKS5;
    else
      raise SOCKS_Parse_Error with "Invalid protocol";
    end if;
  end Get_SOCKS_Protocol;
  
  procedure Check_SOCKS5_Greeting(Buffer : Stream_Element_Array) is
    Offset : Stream_Element_Offset := Buffer'First;
    N_Methods : Stream_Element_Offset;
  begin
    if Buffer(Offset) /= 16#05# then
      raise SOCKS_Parse_Error with "Error in greeting";
    end if;
    
    Offset := Offset + 1;
    
    N_Methods := Stream_Element_Offset(Buffer(Offset));
    if N_Methods = 0 then
      raise SOCKS_Parse_Error with "That makes no sense";
    end if;
    Offset := Offset + 1;
    declare
      No_Auth_Found : Boolean := False;
    begin
      for I in Offset..(Offset + N_Methods) loop
        if Buffer(I) = 16#00# then
          No_Auth_Found := True;
        end if;
        exit when No_Auth_Found;
      end loop;
      if not No_Auth_Found then
        raise SOCKS_Auth_Unsupported with
          "Client doesn't accept 'No authentication'";
      end if;
    end;
  end Check_SOCKS5_Greeting;
  
  function Is_SOCKS4a(Buffer : Stream_Element_Array) return Boolean is
    Offset : Stream_Element_Offset := Buffer'First;
  begin
    if Buffer(Offset) = 0
      and then Buffer(Offset + 1) = 0
      and then Buffer(Offset + 2) = 0
      and then Buffer(Offset + 3) /= 0
    then
      return True;
    else
      return False;
    end if;
  end Is_SOCKS4a;
  
  --  Check if Buffer contains a valid SOCKS request.
  --  Raise an exception if not.
  procedure Check_SOCKS4_Req(Buffer : Stream_Element_Array) is
    Offset : Stream_Element_Offset := Buffer'First + 1;
    SOCKS4a : Boolean := False;
  begin
    --  Check the requested operation
    if Buffer(Offset) /= 16#01# then
      raise SOCKS_Unsupported_Op with "Only support stream connections";
    end if;
    Offset := Offset + 1;
    --  Check that the port is not 0
    if Buffer(Offset) = 0 and Buffer(Offset + 1) = 0 then
      raise SOCKS_Parse_Error with "Invalid port number 0";
    end if;
    Offset := Offset + 2;
    --  Check if this is a SOCKS4a request.
    SOCKS4a := Is_SOCKS4a(Buffer(Offset..(Offset + 3)));
    
    Offset := Offset + 4;
    
    while Buffer(Offset) /= 16#00# loop
      Offset := Offset + 1;
    end loop;
    
    if not SOCKS4a then
      return;
    else
      Offset := Offset + 1;
      while Buffer(Offset) /= 16#00# loop
        Offset := Offset + 1;
      end loop;
    end if;
  end Check_SOCKS4_Req;
    
  procedure Check_SOCKS5_Req(Buffer : Stream_Element_Array) is
  begin
    Wpoxy_Log(4, "Do not support SOCKS5 yet");
    
    raise Program_Error with "Not implemented";
  end Check_SOCKS5_Req;
    
  function To_Port(Buf : Stream_Element_Array) return Port_Type is
    High : Port_Type := Port_Type(Buf(Buf'First));
    Low : Port_Type := Port_Type(Buf(Buf'First + 1));
  begin
    return High * 256 + Low;
  end To_Port;
  
  procedure From_Port(Port : Port_Type; Buf : in out Stream_Element_Array) is
    Low : Port_Type := Port mod 256;
    High : Port_Type := (Port - Low) / 256;
  begin
    Buf(Buf'First) := Stream_Element(High);
    Buf(Buf'First + 1) := Stream_Element(Low);
  end From_Port;

  procedure Parse_SOCKS_Req(Buffer : Stream_Element_Array;
                            Req_Addr : out Sock_Addr_Type;
                            Protocol : out SOCKS_Protocol) is
    procedure Parse_SOCKS4_Req(Buffer : Stream_Element_Array;
                               Req_Addr : out Sock_Addr_Type) is
      Current : Stream_Element_Offset := Buffer'First + 2;
    begin
      Req_Addr.Port := To_Port(Buffer(Current..Buffer'Last));
      Current := Current + 2;
      if not Is_SOCKS4a(Buffer(Current..Buffer'Last)) then
        Req_Addr.Addr
          := Inet_Addr(Trim(Buffer(Current)'Img, Both) & "." &
                         Trim(Buffer(Current + 1)'Img, Both) & "." &
                         Trim(Buffer(Current + 2)'Img, Both) & "." &
                         Trim(Buffer(Current + 3)'Img, Both));
      else
        raise Program_Error with "Not implemented";
      end if;
    end Parse_SOCKS4_Req;
    
    procedure Parse_SOCKS5_Req(Buffer : Stream_Element_Array;
                               Req_Addr : out Sock_Addr_Type) is
    begin
      raise Program_Error with "Not implemented";
    end Parse_SOCKS5_Req;
    
  begin
    Wpoxy_Log(5, "Parse Socks called");
    Protocol := Get_SOCKS_Protocol(Buffer(Buffer'First));
    case Protocol is
       when SOCKS4 =>
         Parse_SOCKS4_Req(Buffer, Req_Addr);
       when SOCKS5 =>
         Parse_SOCKS5_Req(Buffer, Req_Addr);
       when others =>
         raise SOCKS_Parse_Error with "Invalid request";
    end case;
  end;


  procedure Send_SOCKS_Response(Socket : Socket_Type;
                                Response : SOCKS_Response) is
  begin
    Wpoxy_Log(5, "Sending SOCKS response");
  end;
  
  procedure Send_SOCKS_Response(Con : in out Endpoint'Class;
                                Response : SOCKS_Response;
                                Addr : Sock_Addr_Type) is
    
    procedure Send_SOCKS4_Response is
      Buf : Stream_Element_Array(1..8) := (others => 0);
      Last : Stream_Element_Offset;
    begin
      Wpoxy_Log(5, "Sending SOCKS4 response: "
                  & SOCKS_Response'Image(Response));
      Buf(2) := Stream_Element(SOCKS_Response'Enum_Rep(Response));
      Wpoxy_Log(5, "Socks response: ");
      Wpoxy_Log(Buf);
      Send_Data(Con, Buf, Last);
    end Send_SOCKS4_Response;
    
    
    procedure Send_SOCKS5_Response is
    begin
      raise Program_Error with "SOCKS5 response not implemented";  
    end Send_SOCKS5_Response;
  begin
    if Response in SOCKS4_Response then
      Send_SOCKS4_Response;
    else
      raise Program_Error with "Should send SOCKS5 response";
    end if;
  end Send_SOCKS_Response;
    
  procedure Send_SOCKS_Con_Failed(Socket : Socket_Type;
                                  Protocol : SOCKS_Protocol) is
  begin
    if Protocol = SOCKS4 then
      Send_SOCKS_Response(Socket, SOCKS4_Host_Unreachable);
    else
      Send_SOCKS_Response(Socket, SOCKS5_Network_Unreachable);
    end if;
  end Send_SOCKS_Con_Failed;
  
  subtype Octet is Integer range 0..128;
  
  procedure From_Inet_Addr(A : Inet_Addr_Type;
                           D : in out Stream_Element_Array) is
    Octets : Slice_Set;
    Sep : constant String := ".";
    One : constant Stream_Element_Offset := 1;
    Four : constant Stream_Element_Offset := 4;
  begin
    Wpoxy_Log(5, "Will split " & Image(A));
    GNAT.String_Split.Create(S => Octets,
                             Separators => Sep,
                             From => Image(A),
                             Mode => Multiple);
    for I in One..Four loop
      D(D'First + I - 1) :=
        Stream_Element(Octet(Integer'Value(Slice(Octets, Slice_Number(I)))));
      Wpoxy_Log(D);
    end loop;
  end From_Inet_Addr;
  
  procedure Make_SOCKS4_Request(To : Sock_Addr_Type;
                                Data : in out Stream_Element_Array;
                                Last : out Stream_Element_Offset) is
    One : Stream_Element_Offset := Data'First;
  begin
    Data(One) := 16#04#;
    Data(One + 1) := 16#01#;
    From_Port(To.Port, Data((One + 2)..(One + 3)));
    From_Inet_Addr(To.Addr, Data(One + 4..One + 8));
    Data(One + 9) := 0;
    Last := 9;
  end Make_SOCKS4_Request;
    
  function SOCKS4_Successful(Data : Stream_Element_Array) return Boolean is
  begin
    return Data(Data'First + 1) = SOCKS4_OK'Enum_rep;
  end SOCKS4_Successful;

  
end SOCKS;
