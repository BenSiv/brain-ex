with Ada.Text_IO;
with Ada.Directories;
with Ada.Environment_Variables;
with Ada.Command_Line;
with Ada.Strings.Unbounded;
with Ada.Strings.Fixed;
with Ada.Calendar;
with Ada.Calendar.Formatting;
with Src.Sql;
with Src.Utils;
with GNAT.OS_Lib;

package body Src.Init is
   use Ada.Strings.Unbounded;

   -- SQL schema
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

      -- Helper to parse command line args
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

      -- Import tasks from tasks.tsv
      procedure Import_Tasks (Task_File : String) is
         File         : Ada.Text_IO.File_Type;
         Line         : Unbounded_String;
         Line_Num     : Natural := 0;
         Imported     : Natural := 0;
         Tab1_Pos     : Natural;
         Tab2_Pos     : Natural;
         Content      : Unbounded_String;
         Subject      : Unbounded_String;
         Due_To       : Unbounded_String;
         Task_Id      : Unbounded_String;
         Overdue      : Integer;
         Sql_Cmd      : Unbounded_String;
         Success      : Boolean;
         Due_Date     : Ada.Calendar.Time;
         Due_Date_Str : Unbounded_String;
      begin
         if not Ada.Directories.Exists (Task_File) then
            return;  -- No tasks file, silently skip


         end if;

         Ada.Text_IO.Open (File, Ada.Text_IO.In_File, Task_File);

         while not Ada.Text_IO.End_Of_File (File) loop
            Line := To_Unbounded_String (Ada.Text_IO.Get_Line (File));
            Line_Num := Line_Num + 1;

            -- Skip empty lines and header
            if Length (Line) = 0 or Line_Num = 1 then
               goto Continue;
            end if;

            -- Parse TSV: content<TAB>subject<TAB>due_to
            Tab1_Pos :=
              Ada.Strings.Fixed.Index (To_String (Line), "" & ASCII.HT);
            if Tab1_Pos = 0 then
               -- No tabs, treat entire line as content
               Content := Line;
               Subject := Null_Unbounded_String;
               Due_To := Null_Unbounded_String;
            else
               Content := Unbounded_Slice (Line, 1, Tab1_Pos - 1);
               Tab2_Pos :=
                 Ada.Strings.Fixed.Index
                   (To_String (Line), "" & ASCII.HT, Tab1_Pos + 1);

               if Tab2_Pos = 0 then
                  -- Only one tab: content and subject
                  Subject :=
                    Unbounded_Slice (Line, Tab1_Pos + 1, Length (Line));
                  Due_To := Null_Unbounded_String;
               else
                  -- Two tabs: content, subject, and due_to
                  Subject :=
                    Unbounded_Slice (Line, Tab1_Pos + 1, Tab2_Pos - 1);
                  Due_To :=
                    Unbounded_Slice (Line, Tab2_Pos + 1, Length (Line));
               end if;
            end if;

            -- Skip if content is empty
            if Length (Content) = 0 then
               goto Continue;
            end if;

            -- Generate task ID
            Task_Id := To_Unbounded_String (Src.Utils.Generate_Id);

            -- Handle due date
            if Due_To = Null_Unbounded_String or Length (Due_To) = 0 then
               -- Default to tomorrow
               Due_Date := Ada.Calendar."+" (Ada.Calendar.Clock, 86400.0);
               Due_Date_Str :=
                 To_Unbounded_String
                   (Ada.Calendar.Formatting.Image (Due_Date) (1 .. 19));
            else
               Due_Date_Str := Due_To;
            end if;

            -- Compute overdue flag
            if To_String (Due_Date_Str)
              < Ada.Calendar.Formatting.Image (Ada.Calendar.Clock) (1 .. 19)
            then
               Overdue := 1;
            else
               Overdue := 0;
            end if;

            -- Build INSERT statement
            if Subject = Null_Unbounded_String or Length (Subject) = 0 then
               Sql_Cmd :=
                 To_Unbounded_String
                   ("INSERT INTO tasks (id, subject, content, due_to, overdue, done) VALUES ('"
                    & To_String (Task_Id)
                    & "', NULL, '"
                    & Src.Sql.Escape_Sql (To_String (Content))
                    & "', '"
                    & To_String (Due_Date_Str)
                    & "', "
                    & Integer'Image (Overdue)
                    & ", NULL);");
            else
               Sql_Cmd :=
                 To_Unbounded_String
                   ("INSERT INTO tasks (id, subject, content, due_to, overdue, done) VALUES ('"
                    & To_String (Task_Id)
                    & "', '"
                    & Src.Sql.Escape_Sql (To_String (Subject))
                    & "', '"
                    & Src.Sql.Escape_Sql (To_String (Content))
                    & "', '"
                    & To_String (Due_Date_Str)
                    & "', "
                    & Integer'Image (Overdue)
                    & ", NULL);");
            end if;

            -- Execute insert
            Src.Sql.Execute
              (To_String (Brain_Path), To_String (Sql_Cmd), Success);
            if Success then
               Imported := Imported + 1;
            else
               Ada.Text_IO.Put_Line
                 ("Warning: Failed to import task on line"
                  & Natural'Image (Line_Num));
            end if;

            <<Continue>>
         end loop;

         Ada.Text_IO.Close (File);

         if Imported > 0 then
            Ada.Text_IO.Put_Line
              ("Imported"
               & Natural'Image (Imported)
               & " tasks from "
               & Task_File);
         end if;

      exception
         when others =>
            if Ada.Text_IO.Is_Open (File) then
               Ada.Text_IO.Close (File);
            end if;
            Ada.Text_IO.Put_Line ("Error reading tasks file: " & Task_File);
      end Import_Tasks;

   begin
      Parse_Args;

      -- Determine database name priority: custom name > vault name > "brain"
      if Brain_Name /= To_Unbounded_String ("brain") then
         -- Custom name was explicitly set
         Brain_Path :=
           To_Unbounded_String
             (Current_Dir & "/" & To_String (Brain_Name) & ".db");
      elsif Vault_Path /= Null_Unbounded_String then
         -- Use vault name for database
         Brain_Path :=
           To_Unbounded_String
             (Current_Dir & "/" & To_String (Vault_Path) & ".db");
      else
         -- Default to "brain"
         Brain_Path :=
           To_Unbounded_String
             (Current_Dir & "/" & To_String (Brain_Name) & ".db");
      end if;

      -- Remove old database
      if Ada.Directories.Exists (To_String (Brain_Path)) then
         Ada.Directories.Delete_File (To_String (Brain_Path));
      end if;

      -- Create tables
      Src.Sql.Execute (To_String (Brain_Path), Sql_Init, Success);
      if not Success then
         Ada.Text_IO.Put_Line ("Failed to initialize database");
         return;
      end if;

      -- Create config directory
      if not Ada.Directories.Exists (Config_Dir) then
         Ada.Directories.Create_Path (Config_Dir);
      end if;

      Ada.Text_IO.Create (File, Ada.Text_IO.Out_File, Config_File);
      Ada.Text_IO.Put_Line (File, "brain: " & To_String (Brain_Path));

      -- Handle vault
      if Vault_Path /= Null_Unbounded_String then
         -- Create vault directory structure
         if not Ada.Directories.Exists (To_String (Vault_Path)) then
            Ada.Directories.Create_Directory (To_String (Vault_Path));
         end if;

         Ada.Text_IO.Put_Line (File, "vault: " & To_String (Vault_Path));

         -- Import tasks.tsv
         declare
            Task_File : constant String :=
              To_String (Vault_Path) & "/tasks.tsv";
         begin
            Import_Tasks (Task_File);
         end;


         -- Initialize git if requested
         if Git then
            declare
               Git_Cmd : constant String :=
                 "cd " & To_String (Vault_Path) & " && git init";
               Args    : GNAT.OS_Lib.Argument_List (1 .. 2);
            begin
               Args (1) := new String'("-c");
               Args (2) := new String'(Git_Cmd);
               declare
                  Unused : constant Integer :=
                    GNAT.OS_Lib.Spawn ("/bin/sh", Args);
               begin
                  null;
               end;
               GNAT.OS_Lib.Free (Args (1));
               GNAT.OS_Lib.Free (Args (2));
            end;
         end if;
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

   end Do_Init;

end Src.Init;
