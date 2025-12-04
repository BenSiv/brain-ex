with Ada.Text_IO;
with Ada.Directories;
with Ada.Environment_Variables;
with Ada.Command_Line;
with Ada.Strings.Unbounded;
with Src.Sql;
with Src.Config;
with Src.Help;

package body Src.Init is
   use Ada.Strings.Unbounded;

   Sql_Init : constant String :=
     "PRAGMA foreign_keys = ON;"
     & "CREATE TABLE notes ("
     & "    time TIMESTAMP DEFAULT CURRENT_TIMESTAMP,"
     & "    subject TEXT,"
     & "    title TEXT,"
     & "    content TEXT,"
     & "    PRIMARY KEY (title, subject)"
     & ");"
     & "CREATE TABLE connections ("
     & "    source_title TEXT NOT NULL,"
     & "    source_subject TEXT,"
     & "    target_title TEXT NOT NULL,"
     & "    target_subject TEXT,"
     & "    PRIMARY KEY (source_title, source_subject, target_title, target_subject)"
     & ");"
     & "CREATE TABLE tasks ("
     & "    id INTEGER PRIMARY KEY,"
     & "    time TIMESTAMP DEFAULT CURRENT_TIMESTAMP,"
     & "    content TEXT,"
     & "    subject TEXT,"
     & "    due_to TIMESTAMP,"
     & "    overdue INTEGER,"
     & "    done TIMESTAMP DEFAULT NULL,"
     & "    comment TEXT DEFAULT NULL"
     & ");";

   procedure Do_Init is
      use Ada.Command_Line;

      Brain_Name : Unbounded_String := To_Unbounded_String ("brain");
      Vault_Path : Unbounded_String;
      Editor     : Unbounded_String := To_Unbounded_String ("nano");
      Git        : Boolean := False;

      Home_Dir    : constant String :=
        Ada.Environment_Variables.Value ("HOME");
      Current_Dir : constant String := Ada.Directories.Current_Directory;
      Brain_Path  : Unbounded_String;

      Config_Dir  : constant String := Home_Dir & "/.config/brain-ex";
      Config_File : constant String := Config_Dir & "/config.yaml";

      File    : Ada.Text_IO.File_Type;
      Success : Boolean;

      -- Helper to parse args
      procedure Parse_Args is
         I : Integer := 1;
      begin
         while I <= Argument_Count loop
            if Argument (I) = "-n" or Argument (I) = "--name" then
               if I + 1 <= Argument_Count then
                  Brain_Name := To_Unbounded_String (Argument (I + 1));
                  I := I + 1;
               end if;
            elsif Argument (I) = "-v" or Argument (I) = "--vault" then
               if I + 1 <= Argument_Count then
                  Vault_Path := To_Unbounded_String (Argument (I + 1));
                  I := I + 1;
               end if;
            elsif Argument (I) = "-e" or Argument (I) = "--editor" then
               if I + 1 <= Argument_Count then
                  Editor := To_Unbounded_String (Argument (I + 1));
                  I := I + 1;
               end if;
            elsif Argument (I) = "-g" or Argument (I) = "--git" then
               Git := True;
            end if;
            I := I + 1;
         end loop;
      end Parse_Args;

   begin
      Parse_Args;

      Brain_Path :=
        To_Unbounded_String
          (Current_Dir & "/" & To_String (Brain_Name) & ".db");

      if Ada.Directories.Exists (To_String (Brain_Path)) then
         Ada.Directories.Delete_File (To_String (Brain_Path));
      end if;

      Src.Sql.Execute (To_String (Brain_Path), Sql_Init, Success);
      if not Success then
         Ada.Text_IO.Put_Line ("Failed to initialize database");
         return;
      end if;

      if not Ada.Directories.Exists (Config_Dir) then
         Ada.Directories.Create_Path (Config_Dir);
      end if;

      Ada.Text_IO.Create (File, Ada.Text_IO.Out_File, Config_File);
      Ada.Text_IO.Put_Line (File, "brain: " & To_String (Brain_Path));
      if Vault_Path /= Null_Unbounded_String then
         Ada.Text_IO.Put_Line (File, "vault: " & To_String (Vault_Path));
      end if;
      Ada.Text_IO.Put_Line (File, "editor: " & To_String (Editor));
      if Git then
         Ada.Text_IO.Put_Line (File, "git: true");
      else
         Ada.Text_IO.Put_Line (File, "git: false");
      end if;
      Ada.Text_IO.Close (File);

      Ada.Text_IO.Put_Line
        ("Initialized brain-ex in " & To_String (Brain_Path));

      -- TODO: Handle vault import if Vault_Path is set
      if Vault_Path /= Null_Unbounded_String then
         Ada.Text_IO.Put_Line
           ("Vault import not yet implemented in Ada version.");
      end if;

   end Do_Init;

end Src.Init;
