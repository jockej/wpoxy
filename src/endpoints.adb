with GNAT.Sockets; use GNAT.Sockets;
with GNUTLS; use GNUTLS;
with Wpoxy_Logger; use Wpoxy_Logger;
with Ada.Streams; use Ada.Streams;
with Ada.Task_Identification; use Ada.Task_Identification;
with Ada.Exceptions; use Ada.Exceptions;
with Interfaces.C;

with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Text_IO; use Ada.Text_IO;
with Websocket; use Websocket;
with Wpoxy_Utils; use Wpoxy_Utils;

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

  Auth_OK : constant Stream_Element := 16#04#;
  Auth_Failed : constant Stream_Element := 16#F3#;

  function Make_Client_Endpoint(Socket : Socket_Type;
                                Use_TLS, Use_WS : Boolean;
                                User_Auth, Resource, Host : String
                                ) return Endpoint'Class is
    Status : Certificate_Status;
    Buffer : Stream_Element_Array(1..1024);
    Last : Stream_Element_Offset;
  begin
    if Use_TLS then
      return T : TLS_Endpoint(Init_Client) do
        T.Socket := Socket;
        T.Use_WS := Use_WS;
        T.Side := Client;
        Set_Default_Priority(T.Session);
        Credentials_Set(T.Session, Creds);
        Transport_Set_Int(T.Session, Interfaces.C.int(To_C(Socket)));
        Wpoxy_Log(5, "Doing TLS handshake");
        if Handshake(T.Session) then
          Wpoxy_Log(5, "Handshake pending");
        end if;
        Status := Certificate_Verify_Peers(T.Session);
        if (Status and Cert_Invalid) /= 0 then
          Put("Status: ");
          Put(Integer(Status), Base=>16);
          New_Line;
          raise Endpoint_Cert_Error with "Invalid cert";
        end if;
        --  OK, we validated the certificate. Now let's authenticate
        if Use_WS then
          declare
            Websock_Key : String := Get_Key;
          begin
            Make_Client_Handshake(Host, Resource, User_Auth, Websock_Key,
                                  Buffer, Last);
            Wpoxy_Log(5, "Client Handshake: " & To_String(Buffer(1..Last)));
            Record_Send(T.Session, Buffer(1..Last), Last);
            Record_Recv(T.Session, Buffer, Last);
            Wpoxy_Log(5, "Got server answer: " & To_String(Buffer(1..Last)));
            if not Server_Response_Valid(Buffer(1..Last), Websock_Key) then
              raise Authentication_Error with "Invalid answer from server";
            end if;
          end;
        else
          Wpoxy_Log(3, "Authenticating to wpoxyd");
          To_Stream_Element_Array(User_Auth, Buffer, Last);
          Record_Send(T.Session, Buffer(1..Last), Last);
          Record_Recv(T.Session, Buffer, Last);
          if Buffer(1) /= Auth_OK or Last /= 1 then
            raise Authentication_Error with "Failed authentication";
          end if;
          Wpoxy_Log(3, "Authentication Succeeded");
        end if;
      end return;
    else
      return T : Plain_Endpoint do
        T.Socket := Socket;
        T.Use_WS := Use_WS;
        T.Side := Client;
        --  OK, we validated the certificate. Now let's authenticate
        if Use_WS then
          declare
            Websock_Key : String := Get_Key;
          begin
            Make_Client_Handshake(Host, Resource, User_Auth, Websock_Key,
                                  Buffer, Last);
            Wpoxy_Log(5, "Client Handshake: " & To_String(Buffer(1..Last)));
            Send_Socket(T.Socket, Buffer(1..Last), Last);
            Receive_Socket(T.Socket, Buffer, Last);
            Wpoxy_Log(5, "Got server answer: " & To_String(Buffer(1..Last)));
            if not Server_Response_Valid(Buffer(1..Last), Websock_Key) then
              raise Authentication_Error with "Invalid answer from server";
            end if;
          end;
        else
          Wpoxy_Log(3, "Authenticating to wpoxyd");
          To_Stream_Element_Array(User_Auth, Buffer, Last);
          Send_Socket(T.Socket, Buffer(1..Last), Last);
          Receive_Socket(T.Socket, Buffer, Last);
          if Buffer(1) /= Auth_OK or Last /= 1 then
            raise Authentication_Error with "Failed authentication";
          end if;
          Wpoxy_Log(3, "Authentication Succeeded");
        end if;

      end return;
    end if;
  end Make_Client_Endpoint;

  function Make_Server_Endpoint(Socket : Socket_Type;
                         Use_TLS, Use_WS : Boolean;
                         User_Auth : String) return Endpoint'Class is
    Buffer : Stream_Element_Array(1..1024);
    Last : Stream_Element_Offset;
  begin
    if Use_TLS then
      return T : TLS_Endpoint(Init_Server) do
        T.Socket := Socket;
        T.Use_WS := Use_WS;
        T.Side := Server;
        Set_Default_Priority(T.Session);
        Credentials_Set(T.Session, Creds);
        Transport_Set_Int(T.Session, Interfaces.C.int(To_C(Socket)));
        Wpoxy_Log(5, "Doing TLS handshake");
        if Handshake(T.Session) then
          Wpoxy_Log(5, "Handshake pending");
        end if;
        if Use_WS then
          declare
            Valid : Boolean;
          begin
            Record_Recv(T.Session, Buffer, Last);
            Wpoxy_Log(5, "Received WS handshake: " &
                        To_String(Buffer(1..Last)));
            Make_Server_Handshake(Buffer, Last, User_Auth, Valid);
            Wpoxy_Log(5, "Answering with: " & To_String(Buffer(1..Last)));
            Record_Send(T.Session, Buffer(1..Last), Last);
            if not Valid then
              raise Authentication_Error with "Invalid Websocket request";
            end if;
          end;
        else
          Wpoxy_Log(3, "Authenticating wpoxyc");
          Record_Recv(T.Session, Buffer, Last);
          declare
            Received_Auth : String := To_String(Buffer(1..Last));
          begin
            if Received_Auth = User_Auth then
              Wpoxy_Log(3, "Authentication Succeeded");
              Buffer(1) := Auth_OK;
              Record_Send(T.Session, Buffer(1..1), Last);
            else
              raise Authentication_Error with "Failed authentication";
            end if;
          end;
        end if;
      end return;
    else
      return T : Plain_Endpoint do
        T.Socket := Socket;
        T.Use_WS := Use_WS;
        T.Side := Server;
        if Use_WS then
          declare
            Valid : Boolean;
          begin
            Receive_Socket(T.Socket, Buffer, Last);
            Wpoxy_Log(5, "Received WS handshake: " &
                        To_String(Buffer(1..Last)));
            Make_Server_Handshake(Buffer, Last, User_Auth, Valid);
            Wpoxy_Log(5, "Answering with: " & To_String(Buffer(1..Last)));
            Send_Socket(T.Socket, Buffer(1..Last), Last);
            if not Valid then
              raise Authentication_Error with "Invalid Websocket request";
            end if;
          end;
        else
          Wpoxy_Log(3, "Authenticating wpoxyc");
          Receive_Socket(T.Socket, Buffer, Last);
          declare
            Received_Auth : String := To_String(Buffer(1..Last));
          begin
            if Received_Auth = User_Auth then
              Wpoxy_Log(3, "Authentication Succeeded");
              Buffer(1) := Auth_OK;
              Send_Socket(T.Socket, Buffer(1..1), Last);
            else
              raise Authentication_Error with "Failed authentication";
            end if;
          end;
        end if;
      end return;
    end if;
  end Make_Server_Endpoint;

  overriding procedure Read_Data(From : in out TLS_Endpoint;
                                 Buffer : out Stream_Element_Array;
                                 Last : out Stream_Element_Offset) is
  begin
    if From.Use_WS then
      declare
        WS_Data : Stream_Element_Array(Buffer'First..Buffer'Last + 14);
        WS_Last : Stream_Element_Offset;
      begin
        Record_Recv(From.Session, WS_Data, WS_Last);
        if WS_Last = WS_Data'First - 1 then
          --  Signal no data
          Last := Buffer'First - 1;
        elsif Is_Close_Frame(WS_Data) then
          Wpoxy_Log(5, "Got a close frame!");
          Last := Buffer'First - 1;
        else
          From_WS(WS_Data(WS_Data'First..WS_Last), Buffer, Last);
        end if;
      end;
    else
      Record_Recv(From.Session, Buffer, Last);
    end if;
  end Read_Data;

  overriding procedure Send_Data(To : in out TLS_Endpoint;
                                 Buffer : in Stream_Element_Array;
                                 Last : out Stream_Element_Offset) is
  begin
    if To.Use_WS then
      declare
        WS_Data : Stream_Element_Array(Buffer'First..Buffer'Last + 14);
        WS_Last : Stream_Element_Offset;
      begin
        To_WS(Buffer, WS_Data, WS_Last, (To.Side = Client));
        Record_Send(To.Session, WS_Data(WS_Data'First..WS_Last), Last);
      end;
    else
      Record_Send(To.Session, Buffer, Last);
    end if;
  end Send_Data;

  overriding procedure Shutdown(T : in out TLS_Endpoint) is
  begin
    Wpoxy_Log(5, "Called shutdown on TLS Endpoint");
    if T.Use_WS then
      Wpoxy_Log(5, "It was a Websocket connection");
      declare
        Close_Frame : Stream_Element_Array(1..2);
        Last : Stream_Element_Offset;
      begin
        Make_Close_Frame(Close_Frame, Last);
        Wpoxy_Log(5, "Attempting to send close frame");
        Record_Send(T.Session, Close_Frame, Last);
        if Last = Close_Frame'First - 1 then
          Wpoxy_Log(5, "Remote has already closed connection!");
        end if;
      end;
    end if;
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
      declare
        WS_Data : Stream_Element_Array(Buffer'First..Buffer'Last + 14);
        WS_Last : Stream_Element_Offset;
      begin
        Wpoxy_Log(5, "Waiting for WS Data");
        Receive_Socket(From.Socket, WS_Data, WS_Last);
        if WS_Last = WS_Data'First - 1 then
          --  Signal no data
          Last := Buffer'First - 1;
        elsif Is_Close_Frame(WS_Data) then
          Wpoxy_Log(5, "Got a close frame!");
          Last := Buffer'First - 1;
        else
          Wpoxy_Log(5, "Got " & WS_Last'Img & " bytes of WS data");
          From_WS(WS_Data(WS_Data'First..WS_Last), Buffer, Last);
        end if;
      end;
    else
      Receive_Socket(From.Socket, Buffer, Last);
    end if;
  end Read_Data;

  overriding procedure Send_Data(To : in out Plain_Endpoint;
                                 Buffer : in Stream_Element_Array;
                                 Last : out Stream_Element_Offset) is
  begin
    if To.Use_WS then
      declare
        WS_Data : Stream_Element_Array(Buffer'First..Buffer'Last + 14);
        WS_Last : Stream_Element_Offset;
      begin
        To_WS(Buffer, WS_Data, WS_Last, (To.Side = Client));
        Last := WS_Data'First + WS_Last;
        Wpoxy_Log(5, "Sending " & WS_Last'Img & " bytes of WS data");
        Send_Socket(To.Socket, WS_Data(WS_Data'First..WS_Last), WS_Last);
      end;
    else
      Send_Socket(To.Socket, Buffer, Last);
    end if;
  end Send_Data;

  overriding procedure Shutdown(T : in out Plain_Endpoint) is
  begin
    Wpoxy_Log(3, Image(Current_Task) & " called Shutdown on plain endpoint");
    if T.Use_WS then
      Wpoxy_Log(5, "It was a Websocket connection");
      declare
        Close_Frame : Stream_Element_Array(1..2);
        Last : Stream_Element_Offset;
      begin
        Make_Close_Frame(Close_Frame, Last);
        Wpoxy_Log(5, "Attempting to send close frame");
        Send_Socket(T.Socket, Close_Frame, Last);
        if Last = Close_Frame'First - 1 then
          Wpoxy_Log(5, "Remote has already closed connection!");
        --  else try to read to see if we got a response?
        --    Read_Socket(
        end if;
      exception
         when Error : GNAT.Sockets.Socket_Error =>
           Wpoxy_Log(5, "Exception sending close frame: " &
                      Exception_Name(Error) & ": " & Exception_Message(Error));
      end;
    end if;
    if not T.Closed then
      Shutdown_Socket(T.Socket);
      T.Closed := True;
    end if;
  end Shutdown;

end Endpoints;
