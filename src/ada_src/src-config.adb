with Ada.Text_IO;
with Ada.Environment_Variables;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Directories;

package body Src.Config is
   use Ada.Strings.Unbounded;

   type Config_Rec is record
      Brain_Path : Unbounded_String;
      Vault_Path : Unbounded_String;
      Editor     : Unbounded_String;
      Git        : Boolean := False;
      Loaded     : Boolean := False;
   end record;

   Cached_Config : Config_Rec;

   function Get_Config_Path return String is
      Home : constant String := Ada.Environment_Variables.Value ("HOME");
   begin
      return Home & "/.config/brain-ex/config.yaml";
   end Get_Config_Path;

   procedure Load_Config is
      File : Ada.Text_IO.File_Type;
      Path : constant String := Get_Config_Path;
      Line : Unbounded_String;
      Key  : Unbounded_String;
      Val  : Unbounded_String;
      Idx  : Natural;
   begin
      if Cached_Config.Loaded then
         return;
      end if;

      if not Ada.Directories.Exists (Path) then
         Ada.Text_IO.Put_Line ("Error: " & Path & " file does not exist, run brex init.");
         return;
      end if;

      Ada.Text_IO.Open (File, Ada.Text_IO.In_File, Path);
      while not Ada.Text_IO.End_Of_File (File) loop
         Line := To_Unbounded_String (Ada.Text_IO.Get_Line (File));
         Idx := Ada.Strings.Fixed.Index (To_String (Line), ": ");
         if Idx > 0 then
            Key := Unbounded_Slice (Line, 1, Idx - 1);
            Val := Unbounded_Slice (Line, Idx + 2, Length (Line));
            
            if Key = "brain" then
               Cached_Config.Brain_Path := Val;
            elsif Key = "vault" then
               Cached_Config.Vault_Path := Val;
            elsif Key = "editor" then
               Cached_Config.Editor := Val;
            elsif Key = "git" then
               if Val = "true" then
                  Cached_Config.Git := True;
               else
                  Cached_Config.Git := False;
               end if;
            end if;
         end if;
      end loop;
      Ada.Text_IO.Close (File);
      Cached_Config.Loaded := True;
   exception
      when others =>
         if Ada.Text_IO.Is_Open (File) then
            Ada.Text_IO.Close (File);
         end if;
   end Load_Config;

   function Get_Brain_Path return String is
   begin
      Load_Config;
      return To_String (Cached_Config.Brain_Path);
   end Get_Brain_Path;

   function Get_Vault_Path return String is
   begin
      Load_Config;
      return To_String (Cached_Config.Vault_Path);
   end Get_Vault_Path;

   function Get_Default_Editor return String is
   begin
      Load_Config;
      return To_String (Cached_Config.Editor);
   end Get_Default_Editor;

   function Is_Git return Boolean is
   begin
      Load_Config;
      return Cached_Config.Git;
   end Is_Git;

   procedure Reload is
   begin
      Cached_Config.Loaded := False;
      Load_Config;
   end Reload;

end Src.Config;
