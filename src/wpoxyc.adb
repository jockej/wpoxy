--  Builtin packages
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Streams; use Ada.Streams;
with Interfaces.C; use Interfaces.C;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

--  GNAT packages
--  with Ada.Text_IO.Text_Streams; use Ada.Text_IO.Text_Streams;
with GNAT.Sockets; use GNAT.Sockets;
with GNAT.Command_Line; use GNAT.Command_Line;
--  with Ada.Streams.Stream_IO;

--  Third party packages
with GNUTLS; use GNUTLS;

--  Local packages
with Wpoxy_Logger; use Wpoxy_Logger;
with Proxy_Tasks; use Proxy_Tasks;
with Proxyc_Coordinator; use Proxyc_Coordinator;
with Wpoxy_Utils; use Wpoxy_Utils;
with Libconfig; use Libconfig;
with Endpoints;

procedure Wpoxyc is

  type Listen_Socket_Type(Mode : Wpoxyc_Mode := SOCKS_Server ) is
    record
      Socket : Socket_Type;
      Listen_Address : Sock_Addr_Type;
      case Mode is
         when Port_Forward =>
           Forward_Address : Sock_Addr_Type;
         when SOCKS_Server =>
           null;
      end case;
  end record;

  type Listen_Socket_Arr is array(Positive range <>) of Listen_Socket_Type;

  procedure Init_Sockets(Sockets : in out Listen_Socket_Arr) is
  begin
    for I in Sockets'Range loop
      Create_Socket(Sockets(I).Socket, Family_Inet, Socket_Stream);
      Set_Socket_Option(Sockets(I).Socket, Socket_Level, (Reuse_Address, True));
      Bind_Socket(Sockets(I).Socket, Sockets(I).Listen_Address);
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
      Listen_Sock_Addr, Forward_Sock_Addr : Sock_Addr_Type;
      Mode_Setting : Setting_Type_Ptr;
      Arr : Listen_Socket_Arr(1..Num_Ports);
      Mode : Wpoxyc_Mode;
    begin
      for I in Arr'Range loop
        Port := Get_Elem(Ports, I - 1);
        Mode_Setting := Lookup(Port, "mode");
        if Get_String(Mode_Setting) = "socks_server" then
          Mode := SOCKS_Server;
        elsif Get_String(Mode_Setting) = "port_forward" then
          Mode := Port_Forward;
        else
          raise Config_Error with "Invalid 'mode' setting";
        end if;

        Listen_Sock_Addr.Addr :=
          Inet_Addr(Lookup_String(Config, "listen_address"));
        Listen_Sock_Addr.Port :=
          Port_Type(Get_Int(Lookup(Port, "listen_port")));
        if Mode = Port_Forward then
          Forward_Sock_Addr.Addr :=
            Inet_Addr(Get_String(Lookup(Port, "forward_address")));
          Forward_Sock_Addr.Port :=
            Port_Type(Get_Int(Lookup(Port, "forward_port")));
        end if;

        if Mode = SOCKS_Server then
          Arr(I) := (Mode => SOCKS_Server,
                     Socket => No_Socket,
                     Listen_Address => Listen_Sock_Addr);
        else
          Arr(I) := (Mode => Port_Forward,
                     Socket => No_Socket,
                     Listen_Address => Listen_Sock_Addr,
                     Forward_Address => Forward_Sock_Addr);
        end if;
      end loop;
      return Arr;
    end;
  end Make_Sockets;

  Dummy_Sockets : Socket_Set_Type;
  Selector : Selector_Type;
  Status : Selector_Status;
  Listen_Set, Selector_Set : Socket_Set_Type;
  Remote_Address : Sock_Addr_Type;

  procedure Print_Listen_Sockets(Arr : Listen_Socket_Arr;
                                Use_TLS, Use_WS : Boolean) is
  begin
    Put_Line("Wpoxyc Listening on:");
    for I in Arr'Range loop
      Put(Image(Arr(I).Listen_Address) & " ");
      if Arr(I).Mode = SOCKS_Server then
        Put(" as a SOCKS server");
      else
        Put(" forwarding to " & Image(Arr(I).Forward_Address));
      end if;
      New_Line;
    end loop;
    Put("Tunneling via " & Image(Remote_Address));
    if Use_TLS then
      Put(" over TLS");
    end if;
    if Use_WS then
      Put(" and websocket");
    end if;
    New_Line;
  end Print_Listen_Sockets;

  procedure Print_Usage is
  begin
    Put_Line("Usage: wpoxyc -c file [-d num] [-h|--help]");
  end Print_Usage;

  procedure Print_Help is
  begin
    Print_Usage;
    Put_Line("-c <configuration file>");
    Put_Line("-d <debuglevel>");
    Put_Line("-h, --help: Print this message");
  end Print_Help;

  Config : Config_Type_Ptr;
  Got_Config : Boolean := False;
  Level : Log_Level := 1;
begin

  loop
    case Getopt("c: -help h d:") is
       when 'c' =>
         Config := Make_Config(Parameter);
         Got_Config := True;
       when 'h' =>
         Print_Help;
         return;
       when '-' =>
         if Full_Switch = "-help" then
           Print_Help;
           return;
         end if;
       when 'd' =>
         begin
           Level := Integer'Value(Parameter);
         exception
            when Constraint_Error =>
              Put_Line("Illegal debug level!");
              return;
         end;
       when others =>
         exit;
    end case;
  end loop;

  Set_Log_Level(Level);

  if not Got_Config then
    Print_Usage;
    return;
  end if;

  declare
    Sockets : Listen_Socket_Arr := Make_Sockets(Config);
    User_Auth : String := Lookup_String(Config, "user_auth");
    Trust_File : String := Lookup_String(Config, "trustfile");
    Remote_Host : String := Lookup_String(Config, "remote_address");
    Use_WS : Boolean := Lookup_Bool(Config, "ws");
    Use_TLS : Boolean := Lookup_Bool(Config, "tls");

    Resource : String :=
      (if Use_WS then Lookup_String(Config, "remote_resource") else "");
  begin
    Remote_Address.Addr := Addresses(Get_Host_By_Name(Remote_Host));
    Remote_Address.Port := Port_Type(Lookup_Int(Config, "remote_port"));

    Endpoints.Set_Trust_File(Trust_File);
    Set_TLS_Debug(6);
    Init_Sockets(Sockets);
    for I in Sockets'Range loop
      Set(Listen_Set, Sockets(I).Socket);
    end loop;
    Print_Listen_Sockets(Sockets, Use_TLS, Use_WS);
    Create_Selector(Selector);

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
              Con_Task : Proxyc_Task_Acc := new Proxyc_Task;
            begin
              Accept_Socket(Sockets(I).Socket, Client_Socket, From);
              Wpoxy_Log(3, "Accepted connection from " & Image(From));
              if Sockets(I).Mode = Port_Forward then
                Con_Task.Start_Forward(Client_Socket,
                                       Remote_Address,
                                       Sockets(I).Forward_Address,
                                       User_Auth, Resource, Remote_Host,
                                       Use_TLS, Use_WS);
              else
                Con_Task.Start_SOCKS(Client_Socket,
                                     Remote_Address,
                                     User_Auth, Resource, Remote_Host,
                                     Use_TLS, Use_WS);
              end if;
              Proxyc_Coord.Register(Con_Task);
            end;
          end if;
        end loop;
      end;
    end loop Mainloop;
  end;
end Wpoxyc;
