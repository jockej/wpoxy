with Task_List;
with Proxy_Tasks; use Proxy_Tasks;
with Ada.Task_Identification; use Ada.Task_Identification;
with Ada.Task_Termination; use Ada.Task_Termination;
with Ada.Exceptions; use Ada.Exceptions;

package Proxyd_Coordinator is
  protected Proxyd_Coord is
    procedure Unregister(C : Cause_Of_Termination;
                         I : Task_Id;
                         X : Exception_Occurrence);
    procedure Register(Ptr : Proxyd_Task_Acc);
  end Proxyd_Coord;
end Proxyd_Coordinator;
