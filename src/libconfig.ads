with System.Storage_Elements;

package Libconfig is
  
  type Config_Status is (Config_False, Config_True);
  for Config_Status use (Config_False => 0, Config_True => 1);
  
  Config_Error : exception;
  
  type Config_Type is private;
  type Config_Type_Ptr is access all Config_Type with Convention => C;
  type Setting_Type is private;
  type Setting_Type_Ptr is access all Setting_Type with Convention => C;
  
  function Make_Config return Config_Type_Ptr;
  function Make_Config(File : String) return Config_Type_Ptr;
  procedure Destroy_Config(Config : in out Config_Type_Ptr);
  
  procedure Read_File(Config : Config_Type_Ptr; Filename : String);
  function Lookup_Int(Config : Config_Type_Ptr;
                      Path : String) return Integer;
  function Lookup_Bool(Config : Config_Type_Ptr;
                       Path : String) return Boolean;
  function Lookup_String(Config : Config_Type_Ptr;
                         Path : String) return String;
  function Lookup(Config : Config_Type_Ptr;
                  Path : String) return Setting_Type_Ptr;
  function Lookup(Setting : Setting_Type_Ptr;
                  Path : String) return Setting_Type_Ptr;
  function Lookup_Int(Setting : Setting_Type_Ptr;
                      Path : String) return Integer;
  function Lookup_Bool(Setting : Setting_Type_Ptr;
                       Path : String) return Boolean;
  function Lookup_String(Setting : Setting_Type_Ptr;
                         Path : String) return String;
  function Get_Int(Setting : Setting_Type_Ptr) return Integer;
  function Get_Bool(Setting : Setting_Type_Ptr) return Boolean;
  function Get_String(Setting : Setting_Type_Ptr) return String;
  function Get_Member(Setting : Setting_Type_Ptr;
                      Path : String) return Setting_Type_Ptr;
  function Get_Elem(Setting : Setting_Type_Ptr;
                    Idx : Natural) return Setting_Type_Ptr;
  function Setting_Length(Setting : Setting_Type_Ptr) return Natural;
private
  
  type Some_Memory is array(1..1024) of aliased Character;
  type Config_Type is record
    Mem : Some_Memory;
  end record;
  
  type Setting_Type is null record;
  
end Libconfig;
