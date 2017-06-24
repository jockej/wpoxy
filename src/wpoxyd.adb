with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Streams; use Ada.Streams;
with Interfaces.C; use Interfaces.C;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Command_Line; use Ada.Command_Line;

with GNAT.Sockets; use GNAT.Sockets;

with GNUTLS; use GNUTLS;

--  Local packages
with Wpoxy_Logger; use Wpoxy_Logger;
with Proxy_Tasks; use Proxy_Tasks;
with Proxyd_Coordinator; use Proxyd_Coordinator;
with Libconfig; use Libconfig;
with Wpoxy_Utils; use Wpoxy_Utils;
with Endpoints;

procedure Wpoxyd is 
  
  type Listen_Socket_Type is record
    Socket : Socket_Type;
    Addr : Sock_Addr_Type;
    Use_WS, Use_TLS : Boolean;
  end record;
  
  type Listen_Socket_Arr is array(Positive range <>) of Listen_Socket_Type;
  
  procedure Init_Sockets(Sockets : in out Listen_Socket_Arr) is
  begin
    for I in Sockets'Range loop
      Create_Socket(Sockets(I).socket, Family_Inet, Socket_Stream);
      Set_Socket_Option(Sockets(I).Socket, Socket_Level, (Reuse_Address, True));
      Bind_Socket(Sockets(I).Socket, Sockets(I).Addr);
      Listen_Socket(Sockets(I).Socket);
    end loop;
  end Init_Sockets;
  
  function Make_Sockets(Config : Config_Type_Ptr) return Listen_Socket_Arr is
    Ports, Port : Setting_Type_Ptr;
    Num_Ports : Natural;
  begin
    Ports := Lookup(Config, "ports");
    Num_Ports := Setting_Length(Ports);
    if Num_Ports = 0 then
      raise Program_Error with "No ports configured!";
    end if;
    declare
      Port_Address : Setting_Type_Ptr;
      Arr : Listen_Socket_Arr(1..Num_Ports);
    begin
      for I in Arr'Range loop
        Port := Get_Elem(Ports, I - 1);
        Port_Address := Lookup(Port, "address");
        Arr(I).Addr.Addr := Inet_Addr(Get_String(Port_Address));
        Arr(I).Addr.Port := Port_Type(Lookup_Int(Port, "port"));
        Arr(I).Use_WS := Lookup_Bool(Port, "ws");
        Arr(I).Use_TLS := Lookup_Bool(Port, "tls");
      end loop;
      return Arr;
    end;
  end Make_Sockets;
  
  procedure Print_Listen_Sockets(Arr : Listen_Socket_Arr) is
  begin
    Put_Line("Wpoxyd Listening on:");
    for I in Arr'Range loop
      Put(Image(Arr(I).Addr) & " ");
      if Arr(I).Use_TLS then
        Put("over TLS");
      end if;
      if Arr(I).Use_WS then
        Put(" and websocket");
      end if;
      New_Line;
    end loop;
  end Print_Listen_Sockets;
  
  Dummy_Sockets : Socket_Set_Type;
  Selector : Selector_Type;
  Status : Selector_Status;
  Listen_Set, Selector_Set : Socket_Set_Type;
  
  Config : Config_Type_Ptr := Make_Config("./test/wpoxyd.conf");
  Sockets : Listen_Socket_Arr := Make_Sockets(Config);  
  User_Auth : Stream_Element_Array
    := To_Base64(Lookup_String(Config, "user_auth"));
  Cert_File : String := Lookup_String(Config, "certfile");
  Key_File : String := Lookup_String(Config, "keyfile");

begin
  Set_TLS_Debug(2);  
  Endpoints.Init_Cert_Key(Cert_File, Key_File);
  Init_Sockets(Sockets);
  for I in Sockets'Range loop
    Set(Listen_Set, Sockets(I).Socket);
  end loop;
  Create_Selector(Selector);
  Print_Listen_Sockets(Sockets);

    Mainloop: 
  loop
    declare
      Client_Socket : Socket_Type;
      From : Sock_Addr_Type;
    begin
      Copy(Listen_Set, Selector_Set);
      Check_Selector(Selector, Selector_Set, Dummy_Sockets, Status);
      for I in Sockets'Range loop
        if Is_Set(Selector_Set, Sockets(I).Socket) then
          declare
            Con_Task : Proxyd_Task_Acc := new Proxyd_Task;
          begin
            Accept_Socket(Sockets(I).Socket, Client_Socket, From);
            Wpoxy_Log(1, "Accepted socket!");
            Con_Task.Start(Client_Socket,
                           User_Auth,
                           Sockets(I).Use_WS,
                           Sockets(I).Use_TLS);
            Proxyd_Coord.Register(Con_Task);
          end;
        end if;
      end loop;
    end;
  end loop Mainloop;
end Wpoxyd;
