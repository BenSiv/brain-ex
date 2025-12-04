package Src.Config is
   function Get_Brain_Path return String;
   function Get_Vault_Path return String;
   function Get_Default_Editor return String;
   function Is_Git return Boolean;
   procedure Reload;
end Src.Config;
