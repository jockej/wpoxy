with Task_List;
with Proxy_Tasks; use Proxy_Tasks;
with Ada.Task_Identification; use Ada.Task_Identification;
with Ada.Task_Termination; use Ada.Task_Termination;
with Ada.Exceptions; use Ada.Exceptions;

package Proxyc_Coordinator is
  protected Proxyc_Coord is
    procedure Unregister(C : Cause_Of_Termination;
                         I : Task_Id;
                         X : Exception_Occurrence);
    procedure Register(Ptr : Proxyc_Task_Acc);
  end Proxyc_Coord;
end Proxyc_Coordinator;
