with Ada.Task_Identification; use Ada.Task_Identification;
with Ada.Streams; use Ada.Streams;
with GNAT.Sockets; use GNAT.Sockets;
with GNUTLS; use GNUTLS;
with Wpoxy_Logger; use Wpoxy_Logger;
with Endpoints; use Endpoints;

package Proxy_Tasks is
  
  type Wpoxyc_Mode is (SOCKS_Server, Port_Forward);
  
  task type Proxyc_Task is
    entry Start_SOCKS(Client : Socket_Type;
                      Remote_Address : Sock_Addr_Type;
                      User_Auth, Resource, Host : String;
                      Use_TLS, Use_WS : Boolean);
    entry Start_Forward(Client : Socket_Type;
                        Remote_Address, Forward_Address : Sock_Addr_Type;
                        User_Auth, Resource, Host : String;
                        Use_TLS, Use_WS : Boolean);
  end Proxyc_Task;

  type Proxyc_Task_Acc is access all Proxyc_Task;

  task type Proxyd_Task is
    entry Start(Wpoxyc_Socket : Socket_Type;
                User_Auth : String;
                Use_WS, Use_TLS : Boolean);
  end Proxyd_Task;

  type Proxyd_Task_Acc is access all Proxyd_Task;
  
  type Proxy_Direction is (Socket_To_Endpoint, Endpoint_To_Socket);
  
  task type Proxy is
    entry Start(Socket : Socket_Type;
                Endp : in out Endpoint'Class;
                Dir : Proxy_Direction);
  end Proxy;
  
end Proxy_Tasks;
