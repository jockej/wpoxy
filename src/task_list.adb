with Ada.Task_Identification; use Ada.Task_Identification;
with Ada.Unchecked_Deallocation;
with Ada.Task_Termination; use Ada.Task_Termination;
with Ada.Exceptions; use Ada.Exceptions;
with Wpoxy_Logger; use Wpoxy_Logger;

package body Task_List is
  
  
  protected body Proxy_Task_List is
    procedure Add(Ptr : Task_Type_Acc; Id : Task_Id) is
      S : Task_Struct := (Id, Ptr);
    begin
      Append(The_List, S);
    end Add;
    
    procedure Deallocate(Id : Task_Id) is
      procedure Free is new Ada.Unchecked_Deallocation(Object => Task_Type,
                                                       Name => Task_Type_Acc);
      First : Cursor := The_List.First;
      Last : Cursor := The_List.Last;
      Current : Cursor := First;
      Elm : Task_Struct;
    begin
      loop
        Elm := Element(Current);
        if Elm.Id = Id then
          Delete(The_List, Current);
          Wpoxy_Log(5, "Freed task " & Image(Id));
          Free(Elm.Ptr);
          exit;
        end if;
        Current := Next(Current);
        exit when Current = No_Element;
      end loop;
    end Deallocate;
  end Proxy_Task_List;
  
end Task_List;
