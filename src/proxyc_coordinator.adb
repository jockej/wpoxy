with Task_List;
with Proxy_Tasks; use Proxy_Tasks;
with Ada.Task_Identification; use Ada.Task_Identification;
with Ada.Task_Termination; use Ada.Task_Termination;
with Ada.Exceptions; use Ada.Exceptions;
with Wpoxy_Logger; use Wpoxy_Logger;

package body Proxyc_Coordinator is
  
  package Proxyc_Task_List is new Task_List(Proxyc_Task, Proxyc_Task_Acc);
  use Proxyc_Task_List;
  
  Tasks : Proxy_Task_List;
  protected body Proxyc_Coord is
    

    procedure Unregister(C : Cause_Of_Termination;
                         I : Task_Id;
                         X : Exception_Occurrence) is
    begin
      if C = Normal then
        Wpoxy_Log(4, "Ended wpoxyc task " & Image(I) & " normally");
      elsif C = Abnormal then
        Wpoxy_Log(4, "Ended wpoxyc task " & Image(I) & " abnormally");
      else
        Wpoxy_Log(1, "Ended wpoxyc task " & Image(I) &
                    " because of unhadled exception:");
        Wpoxy_Log(1, Exception_Name(X) & ": " & Exception_Message(X));
      end if;
      Tasks.Deallocate(I);
    end Unregister;
  
    procedure Register(Ptr : Proxyc_Task_Acc) is
    begin
      Tasks.Add(Ptr, Ptr.all'Identity);
      Set_Specific_Handler(Ptr.all'Identity, Unregister'Access);
    end Register;
  end Proxyc_Coord;
end Proxyc_Coordinator;
