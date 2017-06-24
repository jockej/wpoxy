with GNAT.Sockets; use GNAT.Sockets;
with GNUTLS; use GNUTLS;
with Wpoxy_Logger; use Wpoxy_Logger;
with Ada.Streams; use Ada.Streams;

package Endpoints is

  type Endpoint is abstract tagged limited private;

  type Endpoint_Acc is access all Endpoint'Class;

  procedure Read_Data(From : in out Endpoint;
                      Buffer : out Stream_Element_Array;
                      Last : out Stream_Element_Offset) is abstract;

  procedure Send_Data(To : in out Endpoint;
                      Buffer : in Stream_Element_Array;
                      Last : out Stream_Element_Offset) is abstract;
  
  procedure Shutdown(T : in out Endpoint) is abstract;

  type TLS_Endpoint(<>) is new Endpoint with private;

  function Make_Endpoint(Socket : Socket_Type;
                         Use_TLS, Use_WS : Boolean;
                         Flags : Init_Flags) return Endpoint'Class;
  
  procedure Set_Trust_File(Trust_File : String);
  procedure Init_Cert_Key(Cert_File, Key_File : String);
  
  Endpoint_Cert_Error : exception;

private
  type Endpoint is abstract tagged limited record
    Use_WS : Boolean;
    Closed : Boolean := False;
  end record;

  type TLS_Endpoint(Flags : Init_Flags) is new Endpoint with record
    Socket : Socket_Type;
    Session : Session_Type(Flags);
  end record;

  type TLS_Endpoint_Acc is access all TLS_Endpoint;


  overriding procedure Read_Data(From : in out TLS_Endpoint;
                                 Buffer : out Stream_Element_Array;
                                 Last : out Stream_Element_Offset);
  overriding procedure Send_Data(To : in out TLS_Endpoint;
                                 Buffer : in Stream_Element_Array;
                                 Last : out Stream_Element_Offset);
  overriding procedure Shutdown(T : in out TLS_Endpoint);
  
  type Plain_Endpoint is new Endpoint with record
    Socket : Socket_Type;
  end record;

  type Plain_Endpoint_Acc is access all Plain_Endpoint;

  overriding procedure Read_Data(From : in out Plain_Endpoint;
                                 Buffer : out Stream_Element_Array;
                                 Last : out Stream_Element_Offset);
  overriding procedure Send_Data(To : in out Plain_Endpoint;
                                 Buffer : in Stream_Element_Array;
                                 Last : out Stream_Element_Offset);
  overriding procedure Shutdown(T : in out Plain_Endpoint);
end Endpoints;
