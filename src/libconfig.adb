with System;
with Ada.Finalization;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with Ada.Unchecked_Deallocation;

package body Libconfig is
  
  package C renames Interfaces.C;
  type Int_Ptr is access all C.Int with Convention => C;
  type C_Str_Ptr_Ptr is access all Chars_Ptr with Convention => C;
  
    
  procedure Check(Status : C.Int; Msg : String) is
  begin
    if Status /= Config_Status'Pos(Config_True) then
      raise Config_Error with Msg;
    end if;
  end Check;
  
  procedure Read_File(Config : Config_Type_Ptr; Filename : String) is
    function Internal(Config : Config_Type_Ptr;
                      Filename : C.char_array) return C.Int;
    pragma Import(C, Internal, "config_read_file");
  begin
    Check(Internal(Config, C.To_C(Filename)),
          "Failed to read file " & Filename);
  end Read_File;
    
  function Lookup_Int(Config : Config_Type_Ptr;
                      Path : String) return Integer is
    function C_Func(Config : Config_Type_Ptr;
                    Path : C.char_array;
                    Value : Int_Ptr) return C.Int;
    pragma Import(C, C_Func, "config_lookup_int");
    Value : aliased C.int;
  begin
    Check(C_Func(Config, C.To_C(Path), Value'Unchecked_Access),
          "No such integer: " & Path);
    return Integer(Value);
  end Lookup_Int;
    
  function Lookup_Bool(Config : Config_Type_Ptr;
                       Path : String) return Boolean is
    function C_Func(Config : Config_Type_Ptr;
                    Path : C.char_array;
                    Value : Int_Ptr) return C.Int;
    pragma Import(C, C_Func, "config_lookup_bool");
    Value : aliased C.Int;
  begin
    Check(C_Func(Config, To_C(Path), Value'Unchecked_access),
          "No such boolean: " & Path);
    if Value /= 0 then
      return True;
    else
      return False;
    end if;
  end Lookup_Bool;

  function Lookup_String(Config : Config_Type_Ptr;
                         Path : String) return String is
    function C_Func(Config : Config_Type_Ptr;
                    Path : C.char_array;
                    Value : C_Str_Ptr_Ptr) return C.Int;
    pragma Import(C, C_Func, "config_lookup_string");
    C_Str : aliased Chars_Ptr;
  begin
    Check(C_Func(Config, To_C(Path), C_Str'Unchecked_Access),
          "No such string: " & Path);
    return Value(C_Str);
  end Lookup_String;
    
  function Lookup(Config : Config_Type_Ptr;
                  Path : String) return Setting_Type_Ptr is
    function C_Func(Config : Config_Type_Ptr;
                    Path : C.Char_Array) return Setting_Type_Ptr;
    pragma Import(C, C_Func, "config_lookup");
    Addr : Setting_Type_Ptr := C_Func(Config, C.To_C(Path));
  begin
    if Addr = null then
      raise Config_Error with "No such setting " & Path;
    else
      return Addr;
    end if;
  end Lookup;
  
  function Lookup(Setting : Setting_Type_Ptr;
                  Path : String) return Setting_Type_Ptr is
    function C_Func(Setting : Setting_Type_Ptr;
                    Path : C.Char_Array) return Setting_Type_Ptr;
    --  For libconfig < 1.5
    --  pragma Import(C, C_Func, "config_lookup_from");
    --  For libconfig >= 1.5
    pragma Import(C, C_Func, "config_setting_lookup");
    Addr : Setting_Type_Ptr := C_Func(Setting, C.To_C(Path));
  begin
    if Addr = null then
      raise Config_Error with "No such setting " & Path;
    else
      return Addr;
    end if;
  end Lookup;
  
  function Lookup_Int(Setting : Setting_Type_Ptr;
                      Path : String) return Integer is
    function C_Func(Setting : Setting_Type_Ptr;
                    Path : C.char_array;
                    Value : Int_Ptr) return C.Int;
    pragma Import(C, C_Func, "config_setting_lookup_int");
    Value : Int_Ptr := new C.Int;
  begin
    Check(C_Func(Setting, C.To_C(Path), Value),
          "No such integer: " & Path);
    return Integer(Value.all);
  end Lookup_Int;
    
  function Lookup_Bool(Setting : Setting_Type_Ptr;
                       Path : String) return Boolean is
    function C_Func(Config : Setting_Type_Ptr;
                    Path : C.char_array;
                    Value : Int_Ptr) return C.Int;
    pragma Import(C, C_Func, "config_setting_lookup_bool");
    Value : aliased C.Int;
  begin
    Check(C_Func(Setting, To_C(Path), Value'Unchecked_access),
          "No such boolean: " & Path);
    if Value /= 0 then
      return True;
    else
      return False;
    end if;
  end Lookup_Bool;

  function Lookup_String(Setting : Setting_Type_Ptr;
                         Path : String) return String is
     function C_Func(Setting : Setting_Type_Ptr;
                    Path : C.char_array;
                    Value : C_Str_Ptr_Ptr) return C.Int;
    pragma Import(C, C_Func, "config_setting_lookup_string");
    C_Str : aliased Chars_Ptr;
  begin
    Check(C_Func(Setting, To_C(Path), C_Str'Unchecked_Access),
          "No such string: " & Path);
    return Value(C_Str);
  end Lookup_String;
 
  function Get_Int(Setting : Setting_Type_Ptr) return Integer is
    function C_Func(Setting : Setting_Type_Ptr) return C.Int;
    pragma Import(C, C_Func, "config_setting_get_int");
  begin
    return Integer(C_Func(Setting));
  end Get_Int;
    
  function Get_Bool(Setting : Setting_Type_Ptr) return Boolean is
    function C_Func(Setting : Setting_Type_Ptr) return C.Int;
    pragma Import(C, C_Func, "config_setting_get_bool");
    Ret : C.int := C_Func(Setting);
  begin
    if Ret = 0 then
      return False;
    else
      return True;
    end if;
  end Get_Bool;

  function Get_String(Setting : Setting_Type_Ptr) return String is
    function C_Func(Setting : Setting_Type_Ptr) return Chars_ptr;
    pragma Import(C, C_Func, "config_setting_get_string");
  begin
    return Value(C_Func(Setting));
  end Get_String;

  function Get_Member(Setting : Setting_Type_Ptr;
                      Path : String) return Setting_Type_Ptr is
    function C_Func(Setting : Setting_Type_Ptr;
                    Path : C.Char_Array) return Setting_Type_Ptr;
    pragma Import(C, C_Func, "config_setting_get_member");
    Addr : Setting_Type_Ptr := null;
  begin
    Addr := C_Func(Setting, C.To_C(Path));
    if Addr = null then
      raise Config_Error with "No such member " & Path;
    else
      return Addr;
    end if;
  end Get_Member;
    
  function Get_Elem(Setting : Setting_Type_Ptr;
                    Idx : Natural) return Setting_Type_Ptr is
    function C_Func(Setting : Setting_Type_Ptr;
                    Index : C.Unsigned) return Setting_Type_Ptr;
    pragma Import(C, C_Func, "config_setting_get_elem");
    Addr : Setting_Type_Ptr;
  begin
    Addr := C_Func(Setting, C.Unsigned(Idx));
    if Addr = null then
      raise Config_Error with "No such element " & Idx'Img;
    else
      return Addr;
    end if;
  end Get_Elem;
  
  function Setting_Length(Setting : Setting_Type_Ptr) return Natural is
    function C_Func(Setting : Setting_Type_Ptr) return C.Int;
    pragma Import(C, C_Func, "config_setting_length");
  begin
    return Natural(C_Func(Setting));
  end Setting_Length;
  
  procedure Init_Config(Config : Config_Type_Ptr) is
    procedure C_Func(Config : Config_Type_Ptr);
    pragma Import(C, C_Func, "config_init");
  begin
    C_Func(Config);
  end Init_Config;
  
  function Make_Config return Config_Type_Ptr is
    C : Config_Type_Ptr := new Config_Type;
  begin
    Init_Config(C);
    return C;
  end Make_Config;
  
  function Make_Config(File : String) return Config_Type_Ptr is
    C : Config_Type_Ptr := Make_Config;
  begin
    Read_File(C, File);
    return C;
  end Make_Config;
  
  procedure Free_Config is new Ada.Unchecked_Deallocation(Object => Config_Type,
                                                          Name => Config_Type_Ptr);
  
  procedure Destroy_Config(Config : in out Config_Type_Ptr) is
    procedure C_Func(Config : Config_Type_Ptr);
    pragma Import(C, C_Func, "config_destroy");
  begin
    C_Func(Config);
    Free_Config(Config);
  end Destroy_Config;
  
end Libconfig;
