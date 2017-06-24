with Ada.Streams; use Ada.Streams;
with GNAT.Sockets; use GNAT.Sockets;
with Endpoints; use Endpoints;

package SOCKS is

  type SOCKS_Protocol is (SOCKS_Invalid, SOCKS4, SOCKS5);

  type SOCKS_Response is (SOCKS5_OK, -- 0x00
                          SOCKS5_General_Failure, -- 0x01
                          SOCKS5_Con_Refused_By_Ruleset, -- 0x02
                          SOCKS5_Network_Unreachable, -- 0x03
                          SOCKS5_Host_Unreachable, -- 0x04
                          SOCKS5_Con_Refused_By_Host, -- 0x05
                          SOCKS5_TTL_Expired, -- 0x06
                          SOCKS5_Protocol_Error, -- 0x07
                          SOCKS5_Addr_Type_Unsupported, -- 0x08
                          SOCKS4_OK, -- 0x5a
                          SOCKS4_Fail, -- 0x5b
                          SOCKS4_Host_Unreachable, -- 0x5c
                          SOCKS4_Identd_Error); -- 0x5d

  for SOCKS_Response use (SOCKS5_OK => 16#00#,
                          SOCKS5_General_Failure => 16#01#,
                          SOCKS5_Con_Refused_By_Ruleset => 16#02#,
                          SOCKS5_Network_Unreachable => 16#03#,
                          SOCKS5_Host_Unreachable => 16#04#,
                          SOCKS5_Con_Refused_By_Host => 16#05#,
                          SOCKS5_TTL_Expired => 16#06#,
                          SOCKS5_Protocol_Error => 16#07#,
                          SOCKS5_Addr_Type_Unsupported => 16#08#,
                          SOCKS4_OK => 16#5A#,
                          SOCKS4_Fail => 16#5B#,
                          SOCKS4_Host_Unreachable => 16#5C#,
                          SOCKS4_Identd_Error => 16#5D#);

  subtype SOCKS4_Response is
  SOCKS_Response range SOCKS4_OK..SOCKS4_Host_Unreachable;

  subtype SOCKS5_Response is
    SOCKS_Response range SOCKS5_OK..SOCKS5_Addr_Type_Unsupported;
  
  SOCKS_Parse_Error : exception;
  SOCKS_Unsupported_Op : exception;
  SOCKS_Auth_Unsupported : exception;
  
  SOCKS5_No_Auth : Stream_Element_Array(1..2) := (16#05#, 16#00#);
  SOCKS5_Auth_Unsupported_Ans : Stream_Element_Array(1..2) := (16#05#, 16#FF#);
  --  procedure Send_SOCKS5_Auths(Socket : Socket_Type);
  
  
  procedure Parse_SOCKS_Req(Buffer : Stream_Element_Array;
                            Req_Addr : out Sock_Addr_Type;
                            Protocol : out SOCKS_Protocol);
  procedure Check_SOCKS4_Req(Buffer : Stream_Element_Array);
  procedure Check_SOCKS5_Req(Buffer : Stream_Element_Array);

  procedure Send_SOCKS_Response(Socket : Socket_Type;
                                Response : SOCKS_Response);
  procedure Send_SOCKS_Response(Con : in out Endpoint'Class;
                                Response : SOCKS_Response;
                                Addr : Sock_Addr_Type);

  function Get_SOCKS_Protocol(Code : in Stream_Element) return SOCKS_Protocol;

  procedure Send_SOCKS_Con_Failed(Socket : Socket_Type;
                                  Protocol : SOCKS_Protocol);
  procedure Make_SOCKS4_Request(To : Sock_Addr_Type;
                                Data : in out Stream_Element_Array;
                                Last : out Stream_Element_Offset);
  
  function SOCKS4_Successful(Data : Stream_Element_Array) return Boolean;

end SOCKS;
