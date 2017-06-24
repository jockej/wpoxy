with Ada.Containers.Doubly_Linked_Lists;
use Ada.Containers;
with Ada.Task_Termination; use Ada.Task_Termination;
with Ada.Exceptions; use Ada.Exceptions;


with Ada.Task_Identification; use Ada.Task_Identification;

generic
  type Task_Type is limited private;
  type Task_Type_Acc is access Task_Type;
package Task_List is
  
  type Task_Struct is record
    Id : Task_Id;
    Ptr : Task_Type_Acc;
  end record;
  
  package Task_Linked_List is new Doubly_Linked_Lists(Task_Struct);
  use Task_Linked_List;
  
  protected type Proxy_Task_List is
    procedure Add(Ptr : Task_Type_Acc; Id : Task_Id);
    procedure Deallocate(Id : Task_Id);
  private
    The_List : List;
  end Proxy_Task_List;
  
end Task_List;
