with GNAT.Sockets; use GNAT.Sockets;
with Ada.Streams; use Ada.Streams;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Task_Identification; use Ada.Task_Identification;
with GNUTLS; use GNUTLS;
with Wpoxy_Logger; use Wpoxy_Logger;
with SOCKS; use SOCKS;
with Ada.IO_Exceptions;
with Wpoxy_Utils; use Wpoxy_Utils;

package body Proxy_Tasks is
  
  Buffer_Size : constant Stream_Element_Offset := 4096;
  
  procedure Copy_String(From : in String; To : out String; Len : out Natural) is
  begin
    Len := From'Length;
    To(To'First..Len) := From;
  end Copy_String;

  
  task body Proxyc_Task is
    SOCKS_Data : Stream_Element_Array(1..1024);
    Protocol : SOCKS_Protocol;
    Last : Stream_Element_Offset;
    Remote_Socket : Socket_Type;
    Creds : Certificate_Credentials;
    Client_Socket : Socket_Type;
    Wpoxyd_Addr : Sock_Addr_Type;
    WS, TLS : Boolean;
    User_Auth_Copy : String(1..128);
    User_Auth_Len : Natural := 1;
    Host_Copy : String(1..128);
    Host_Len : Natural := 1;
    Resource_Copy : String(1..128);
    Resource_Len : Natural := 1;
    Mode : Wpoxyc_Mode;
    Port_Forward_Address : Sock_Addr_Type;
    
  begin
    select
      accept Start_SOCKS(Client : Socket_Type;
                         Remote_Address : Sock_Addr_Type;
                         User_Auth, Resource, Host : String;
                         Use_TLS, Use_WS : Boolean) do
        Wpoxy_Log(4, "Start SOCKS task");
        Client_Socket := Client;
        WS := Use_WS;
        TLS := Use_TLS;
        Wpoxyd_Addr := Remote_Address;
        Mode := SOCKS_Server;
        Copy_String(User_Auth, User_Auth_Copy, User_Auth_Len);
        Copy_String(Host, Host_Copy, Host_Len);
        Copy_String(Resource, Resource_Copy, Resource_Len);
      end Start_SOCKS;
    or
      accept Start_Forward(Client : Socket_Type;
                           Remote_Address, Forward_Address : Sock_Addr_Type;
                           User_Auth, Resource, Host : String;
                           Use_TLS, Use_WS : Boolean) do
        Wpoxy_Log(4, "Start forwarding task");
        Client_Socket := Client;
        WS := Use_WS;
        TLS := Use_TLS;
        Wpoxyd_Addr := Remote_Address;
        Port_Forward_Address := Forward_Address;
        Mode := Port_Forward;
        Copy_String(User_Auth, User_Auth_Copy, User_Auth_Len);
        Copy_String(Host, Host_Copy, Host_Len);
        Copy_String(Resource, Resource_Copy, Resource_Len);
      end Start_Forward;
    end select;
    begin
      if Mode = Port_Forward then
        Make_SOCKS4_Request(Port_Forward_Address, SOCKS_Data, Last);
        Wpoxy_Log(5, "Constructed SOCKS4 request:");
        Wpoxy_Log(SOCKS_Data(1..Last));
      else
        --  Receive the SOCKS request
        Receive_Socket(Client_Socket, SOCKS_Data, Last);
        if (Last = 0) or (Last = SOCKS_Data'Last) then
          raise Program_Error with "Weird SOCKS request!";
        end if;
        Wpoxy_Log(4, "Wpoxyc task received " & Last'Img & " bytes");
        Protocol := Get_SOCKS_Protocol(SOCKS_Data(1));
        if Protocol = SOCKS4 then
          Check_SOCKS4_Req(SOCKS_Data);
        else
          Check_SOCKS5_Req(Socks_Data);
        end if;
        Wpoxy_Log(4, "SOCKS request valid!");
      end if;
      
      --  Attempt to connect to our remote.
      Create_Socket(Remote_Socket);
      Connect_Socket(Remote_Socket, Wpoxyd_Addr);
      Wpoxy_Log(3, "Connected to Wpoxyd");
      declare
        Remote_Endpoint : Endpoint'Class
          := Make_Client_Endpoint(Remote_Socket, TLS, WS,
                                  User_Auth_Copy(1..User_Auth_Len),
                                  Resource_Copy(1..Resource_Len),
                                  Host_Copy(1..Host_Len));
      begin
        Send_Data(Remote_Endpoint, SOCKS_Data(SOCKS_Data'First..Last), Last);
        Wpoxy_Log(5, "Waiting for SOCKS response");
        Read_Data(Remote_Endpoint, SOCKS_Data, Last);
        Wpoxy_Log(5, "Read " & Last'Img & " bytes of SOCKS response");
        if Mode = Port_Forward then
          if not SOCKS4_Successful(SOCKS_Data) then
            raise Program_Error with "SOCKS failed";
          end if;
        else
          Wpoxy_Log(SOCKS_Data(SOCKS_Data'First..Last));
          Send_Socket(Client_Socket, SOCKS_Data(SOCKS_Data'First..Last), Last);
        end if;
        declare
          Client_To_Wpoxyd, Wpoxyd_To_Client : Proxy;
        begin
          Wpoxy_Log(5, "Starting " & Image(Client_To_Wpoxyd'Identity) &
                      " as local->wpoxyd and "
                      & Image(Wpoxyd_To_Client'Identity) &
                      " as wpoxyd->local");
          Client_To_Wpoxyd.Start(Client_Socket,
                                 Remote_Endpoint,
                                 Socket_To_Endpoint);
          Wpoxyd_To_Client.Start(Client_Socket,
                                 Remote_Endpoint,
                                 Endpoint_To_Socket);
        end;
      exception
         when Endpoint_Cert_Error =>
           Wpoxy_Log(1, "Certificate error on endpoint");
           Shutdown_Socket(Client_Socket);
           Shutdown(Remote_Endpoint);
         when Authentication_Error =>
           Wpoxy_Log(1, "Failed to authenticate");
           Shutdown(Remote_Endpoint);
           Shutdown_Socket(Client_Socket);
      end;
    exception
       when SOCKS_Parse_Error =>
         Wpoxy_Log(0, "SOCKS parse error:");
         Shutdown_Socket(Client_Socket);
    end;
  end Proxyc_Task;


  task body Proxyd_Task is
    
    Buffer : Stream_Element_Array(1..1024);
    User_Auth_Copy : String(1..128);
    User_Auth_Len : Natural;
    Last : Stream_Element_Offset;
    Req_Addr : Sock_Addr_Type;
    Protocol : SOCKS_Protocol;
    WS, TLS : Boolean;
    Local_Socket : Socket_Type;
    
  begin
    accept Start(Wpoxyc_Socket : Socket_Type;
                 User_Auth : String;
                 Use_WS, Use_TLS : Boolean) do
      if Use_TLS then
        Wpoxy_Log(2, "Started TLS Proxyd_Task");
      else
        Wpoxy_Log(2, "Started Plain Proxyd_Task");
      end if;
      WS := Use_WS;
      TLS := Use_TLS;
      Local_Socket := Wpoxyc_Socket;
      Copy_String(User_Auth, User_Auth_Copy, User_Auth_Len);
    end Start;
    begin
      declare
        Remote_Socket : Socket_Type;
        Wpoxyc_Endpoint : Endpoint'Class
          := Make_Server_Endpoint(Local_Socket,
                                  TLS, WS,
                                  User_Auth_Copy(1..User_Auth_Len));
      begin
        Read_Data(Wpoxyc_Endpoint, Buffer, Last);
        Parse_SOCKS_Req(Buffer, Req_Addr, Protocol);
        --  Try to connect to the remote
        Create_Socket(Remote_Socket);
        Wpoxy_Log(4, "Will connect to: " & Image(Req_Addr));
        Connect_Socket(Remote_Socket, Req_Addr);
        if Protocol = SOCKS4 then
          Send_SOCKS_Response(Wpoxyc_Endpoint, SOCKS4_OK, Req_Addr);
        else
          Send_SOCKS_Response(Wpoxyc_Endpoint, SOCKS5_OK, Req_Addr);
        end if;
        
        declare
          From_Wpoxyc : Proxy;
          To_Wpoxyc : Proxy;
        begin
          Wpoxy_Log(5, "Starting " & Image(From_Wpoxyc'Identity) &
                      " as wpoxyc->remote and " & Image(To_Wpoxyc'Identity) &
                      " as remote->wpoxyc");
          From_Wpoxyc.Start(Remote_Socket,
                            Wpoxyc_Endpoint,
                            Endpoint_To_Socket);
          To_Wpoxyc.Start(Remote_Socket,
                          Wpoxyc_Endpoint,
                          Socket_To_Endpoint);
        end;
      end;
    end;
  end Proxyd_Task;


  task body Proxy is
    Data : Stream_Element_Array(1..Buffer_Size);
    Last : Stream_Element_Offset;
    Socket_Side : Socket_Type;
    Endpoint_Side : access Endpoint'Class;
    Direction : Proxy_Direction;
  begin
    accept Start(Socket : Socket_Type;
                 Endp : in out Endpoint'Class;
                 Dir : Proxy_Direction) do
      Socket_Side := Socket;
      Endpoint_Side := Endp'Access;
      Direction := Dir;
    end Start;
    case Direction is
       when Socket_To_Endpoint =>
         loop
           Receive_Socket(Socket_Side, Data, Last);
           Wpoxy_Log(5, "Socket_To_Endpoint received " & Last'Img & " bytes");
           exit when Last = Data'First - 1;
           Send_Data(Endpoint_Side.all, Data(Data'First..Last), Last);
           Wpoxy_Log(5, "Socket_To_Endpoint sent " & Last'Img & " bytes");
         end loop;
       when Endpoint_To_Socket =>
         loop
           Read_Data(Endpoint_Side.all, Data, Last);
           Wpoxy_Log(5, "Endpoint_To_Socket received " & Last'Img & " bytes");
           exit when Last = Data'First - 1;
           Send_Socket(Socket_Side, Data(Data'First..Last), Last);
           Wpoxy_Log(5, "Endpoint_To_Socket sent " & Last'Img & " bytes");
         end loop;
    end case;
    Wpoxy_Log(5, "Shutting down " & Image(Current_Task));
    Shutdown_Socket(Socket_Side);
    Shutdown(Endpoint_Side.all);
  exception
     when Error: Ada.IO_Exceptions.End_Error | GNAT.Sockets.Socket_Error =>
       Wpoxy_Log(5, "Peer closed connection");
       Shutdown_Socket(Socket_Side);
       Shutdown(Endpoint_Side.all);
     when Error: others =>
       Wpoxy_Log(2, "Unhandled exception in a proxy task:");
       Wpoxy_Log(2, Exception_Name(Error) & ": " & Exception_Message(Error));
  end Proxy;

end Proxy_Tasks;
