with GNAT.Sockets; use GNAT.Sockets;
with GNUTLS; use GNUTLS;
with Wpoxy_Logger; use Wpoxy_Logger;
with Ada.Streams; use Ada.Streams;
with Interfaces.C;

with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Text_IO; use Ada.Text_IO;
with Websocket; use Websocket;

package body Endpoints is
  
  Creds : Certificate_Credentials;
  
  procedure Set_Trust_File(Trust_File : String) is
  begin
    Certificate_Set_X509_Trust_File(Creds, Trust_File, X509_Fmt_PEM);
  end Set_Trust_File;
  
  procedure Init_Cert_Key(Cert_File, Key_File : String) is
  begin
    Certificate_Set_X509_Key_File(Creds, Cert_File, Key_File, X509_Fmt_PEM);
    --  Certificate_Set_DH_Params(Creds
  end Init_Cert_Key;
  
  function Make_Endpoint(Socket : Socket_Type;
                         Use_TLS, Use_WS : Boolean;
                         Flags : Init_Flags) return Endpoint'Class is
  begin
    if Use_TLS then
      return T : TLS_Endpoint(Flags) do
        T.Socket := Socket;
        T.Use_WS := Use_WS;
        Set_Default_Priority(T.Session);
        Credentials_Set(T.Session, Creds);
        Transport_Set_Int(T.Session, Interfaces.C.int(To_C(Socket)));
        Wpoxy_Log(5, "Doing TLS handshake");
        if Handshake(T.Session) then
          Wpoxy_Log(5, "Handshake pending");
        end if;
        if Flags = Init_Client then
          declare
            Status : Certificate_Status;
          begin
            Status := Certificate_Verify_Peers(T.Session);
            if (Status and Cert_Invalid) /= 0 then
              Put("Status: ");
              Put(Integer(Status), Base=>16);
              New_Line;
              raise Endpoint_Cert_Error with "Invalid cert";
            end if;
          end;
        end if;
      end return;
    else
      return T : Plain_Endpoint do
        T.Socket := Socket;
        T.Use_WS := Use_WS;
      end return;
    end if;
  end Make_Endpoint;

  overriding procedure Read_Data(From : in out TLS_Endpoint;
                                 Buffer : out Stream_Element_Array;
                                 Last : out Stream_Element_Offset) is
  begin
    Wpoxy_Log(5, "Calling record_recv");
    Record_Recv(From.Session, Buffer, Last);
    Wpoxy_Log(5, "Record_Recv returned");
  end Read_Data;

  overriding procedure Send_Data(To : in out TLS_Endpoint;
                                 Buffer : in Stream_Element_Array;
                                 Last : out Stream_Element_Offset) is
  begin
    Record_Send(To.Session, Buffer, Last);
  end Send_Data;

  overriding procedure Shutdown(T : in out TLS_Endpoint) is
  begin
    Wpoxy_Log(5, "Called shutdown on TLS Endpoint");
    if not T.Closed then
      Bye(T.Session, Shut_RDWR);
      Shutdown_Socket(T.Socket);
      T.Closed := True;
    end if;
  end Shutdown;

  overriding procedure Read_Data(From : in out Plain_Endpoint;
                                 Buffer : out Stream_Element_Array;
                                 Last : out Stream_Element_Offset) is
  begin
    if From.Use_WS then
      raise Program_Error with "Websockets not implemented";
    else
      Receive_Socket(From.Socket, Buffer, Last);
    end if;
  end Read_Data;

  overriding procedure Send_Data(To : in out Plain_Endpoint;
                                 Buffer : in Stream_Element_Array;
                                 Last : out Stream_Element_Offset) is
  begin
    if To.Use_WS then
      raise Program_Error with "Websockets not implemented";
    else
      Send_Socket(To.Socket, Buffer, Last);
    end if;
  end Send_Data;

  overriding procedure Shutdown(T : in out Plain_Endpoint) is
  begin
    Wpoxy_Log(5, "Called Shutdown on plain endpoint");
    if not T.Closed then
      Shutdown_Socket(T.Socket);
      T.Closed := True;
    end if;
  end Shutdown;

end Endpoints;
