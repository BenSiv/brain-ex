with Ada.Text_IO;
with Ada.Directories;
with Ada.Environment_Variables;
with Ada.Command_Line;
with Ada.Strings.Unbounded;
with Ada.Strings.Fixed;
with Ada.Calendar;
with Ada.Calendar.Formatting;
with Ada.Containers.Indefinite_Vectors;
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
         package String_Vectors is new
           Ada.Containers.Indefinite_Vectors (Positive, String);

         File     : Ada.Text_IO.File_Type;
         Line     : Unbounded_String;
         Line_Num : Natural := 0;
         Imported : Natural := 0;

         -- Column indices (0 means missing)
         Idx_Id      : Natural := 0;
         Idx_Content : Natural := 0;
         Idx_Subject : Natural := 0;
         Idx_Due     : Natural := 0;
         Idx_Overdue : Natural := 0;
         Idx_Done    : Natural := 0;
         Idx_Comment : Natural := 0;

         -- Data holders
         Content      : Unbounded_String;
         Subject      : Unbounded_String;
         Due_To       : Unbounded_String;
         Task_Id      : Unbounded_String;
         Overdue      : Integer;
         Sql_Cmd      : Unbounded_String;
         Success      : Boolean;
         Due_Date     : Ada.Calendar.Time;
         Due_Date_Str : Unbounded_String;

         procedure Split_Line (S : String; Fields : out String_Vectors.Vector)
         is
            Last : Natural := S'First;
            Curr : Natural;
         begin
            Fields.Clear;
            if S'Length = 0 then
               return;
            end if;

            loop
               Curr := Ada.Strings.Fixed.Index (S, "" & ASCII.HT, Last);
               if Curr = 0 then
                  Fields.Append (S (Last .. S'Last));
                  exit;
               else
                  Fields.Append (S (Last .. Curr - 1));
                  Last := Curr + 1;
               end if;
            end loop;
         end Split_Line;

         Fields : String_Vectors.Vector;
      begin
         if not Ada.Directories.Exists (Task_File) then
            return;  -- No tasks file, silently skip

         end if;

         Ada.Text_IO.Open (File, Ada.Text_IO.In_File, Task_File);

         while not Ada.Text_IO.End_Of_File (File) loop
            Line := To_Unbounded_String (Ada.Text_IO.Get_Line (File));
            Line_Num := Line_Num + 1;

            if Length (Line) = 0 then
               goto Continue;
            end if;

            Split_Line (To_String (Line), Fields);

            if Line_Num = 1 then
               -- Parse Header
               for I in 1 .. Integer (Fields.Length) loop
                  declare
                     Header : constant String := Fields.Element (I);
                  begin
                     if Header = "id" then
                        Idx_Id := I;
                     elsif Header = "content" then
                        Idx_Content := I;
                     elsif Header = "subject" then
                        Idx_Subject := I;
                     elsif Header = "due_to" then
                        Idx_Due := I;
                     elsif Header = "overdue" then
                        Idx_Overdue := I;
                     elsif Header = "done" then
                        Idx_Done := I;
                     elsif Header = "comment" then
                        Idx_Comment := I;
                     end if;
                  end;
               end loop;

               -- Fallback for legacy format (no header, just content/subject/due)
               -- If we didn't find "content" in the first line, assume it's data
               -- But wait, the previous code assumed line 1 IS header or skipped it?
               -- The previous code: "Skip empty lines and header ... if Line_Num = 1 then goto Continue"
               -- So it ALWAYS skipped line 1.
               -- If the user has a file WITHOUT header, this new logic might treat the first row as header.
               -- However, standard TSV exports usually have headers.
               -- If we fail to find "content" column, maybe we should assume legacy mapping?
               -- For now, let's assume valid TSV with header as per plan.

               if Idx_Content = 0 then
                  -- If no header found, maybe it was a legacy file without header?
                  -- Or maybe the header uses different names?
                  -- Let's try to detect if it looks like a header (keywords) vs data.
                  -- But safe bet: if we found 0 columns, maybe map 1->content, 2->subject, 3->due_to (legacy)
                  -- But the prompt implies we are fixing "wrong values" which means the columns ARE there but wrong order.
                  -- So we trust the header mapping.
                  null;
               end if;

            else
               -- Parse Data
               Content := Null_Unbounded_String;
               Subject := Null_Unbounded_String;
               Due_To := Null_Unbounded_String;
               Task_Id := Null_Unbounded_String;
               Overdue := 0;

               -- Extract Content
               if Idx_Content > 0
                 and then Idx_Content <= Integer (Fields.Length)
               then
                  Content :=
                    To_Unbounded_String (Fields.Element (Idx_Content));
               elsif Idx_Content = 0 and then Integer (Fields.Length) >= 1 then
                  -- Fallback: if no header was parsed (Idx_Content=0), assume col 1 is content (legacy behaviorish)
                  -- But we are in the loop, Idx_Content is static.
                  -- If Idx_Content is 0, we probably failed to parse header.
                  -- Let's assume legacy mapping: 1=Content, 2=Subject, 3=Due
                  if Integer (Fields.Length) >= 1 then
                     Content := To_Unbounded_String (Fields.Element (1));
                  end if;
                  if Integer (Fields.Length) >= 2 then
                     Subject := To_Unbounded_String (Fields.Element (2));
                  end if;
                  if Integer (Fields.Length) >= 3 then
                     Due_To := To_Unbounded_String (Fields.Element (3));
                  end if;
               end if;

               if Idx_Content > 0 then
                  -- Normal mapped extraction
                  if Idx_Subject > 0
                    and then Idx_Subject <= Integer (Fields.Length)
                  then
                     Subject :=
                       To_Unbounded_String (Fields.Element (Idx_Subject));
                  end if;
                  if Idx_Due > 0 and then Idx_Due <= Integer (Fields.Length)
                  then
                     Due_To := To_Unbounded_String (Fields.Element (Idx_Due));
                  end if;
                  if Idx_Id > 0 and then Idx_Id <= Integer (Fields.Length) then
                     Task_Id := To_Unbounded_String (Fields.Element (Idx_Id));
                  end if;
                  if Idx_Overdue > 0
                    and then Idx_Overdue <= Integer (Fields.Length)
                  then
                     begin
                        Overdue :=
                          Integer'Value (Fields.Element (Idx_Overdue));
                     exception
                        when others =>
                           Overdue := 0;
                     end;
                  end if;
               end if;

               -- Skip if content is empty
               if Length (Content) = 0 then
                  goto Continue;
               end if;

               -- Generate task ID if missing
               if Length (Task_Id) = 0 then
                  Task_Id := To_Unbounded_String (Src.Utils.Generate_Id);
               end if;

               -- Handle due date
               if Length (Due_To) = 0 then
                  -- Default to tomorrow if not provided
                  Due_Date := Ada.Calendar."+" (Ada.Calendar.Clock, 86400.0);
                  Due_Date_Str :=
                    To_Unbounded_String
                      (Ada.Calendar.Formatting.Image (Due_Date) (1 .. 19));
               else
                  Due_Date_Str := Due_To;
               end if;

               -- Re-compute overdue if not provided or if we want to trust the file?
               -- If the file has 'overdue' column, we used it.
               -- But maybe we should re-calc based on current time?
               -- The prompt says "computing overdue flags", implying we should compute it.
               -- But if we import a dump, maybe we want the state as is?
               -- Existing code computed it:
               -- "if To_String (Due_Date_Str) < ... then Overdue := 1"
               -- Let's stick to computing it if we didn't import it, or maybe always re-compute?
               -- If we import 'overdue=0' but it's now past due, should it be 1? Yes.
               -- So let's re-compute.
               if To_String (Due_Date_Str)
                 < Ada.Calendar.Formatting.Image (Ada.Calendar.Clock) (1 .. 19)
               then
                  Overdue := 1;
               else
                  Overdue := 0;
               end if;

               -- Build INSERT statement
               Sql_Cmd :=
                 To_Unbounded_String
                   ("INSERT INTO tasks (id, subject, content, due_to, overdue, done) VALUES ('");
               Append (Sql_Cmd, To_String (Task_Id) & "', ");

               if Length (Subject) = 0 then
                  Append (Sql_Cmd, "NULL, '");
               else
                  Append
                    (Sql_Cmd,
                     "'" & Src.Sql.Escape_Sql (To_String (Subject)) & "', '");
               end if;

               Append
                 (Sql_Cmd, Src.Sql.Escape_Sql (To_String (Content)) & "', '");
               Append (Sql_Cmd, To_String (Due_Date_Str) & "', ");
               Append (Sql_Cmd, Integer'Image (Overdue) & ", ");

               -- Handle done/comment if we had them?
               -- The schema has done and comment.
               -- Current code sets done=NULL.
               -- If we imported 'done', we should probably use it.
               -- But the current INSERT hardcodes NULL.
               -- Let's stick to NULL for now to minimize changes, unless I want to be fancy.
               -- The prompt said "replace the existing placeholder with a fully functional import mechanism".
               -- The user showed a dump with 'done' and 'comment'.
               -- So I should probably support them.

               -- Let's check if we have done/comment indices
               if Idx_Done > 0
                 and then Idx_Done <= Integer (Fields.Length)
                 and then Fields.Element (Idx_Done)'Length > 0
               then
                  Append (Sql_Cmd, "'" & Fields.Element (Idx_Done) & "'");
               else
                  Append (Sql_Cmd, "NULL");
               end if;

               Append (Sql_Cmd, ");");

               -- Wait, what about comment? The INSERT above ends at done.
               -- "INSERT INTO tasks (..., done) VALUES (..., NULL);"
               -- I need to add comment to the INSERT if I want to support it.
               -- But the existing INSERT only listed: id, subject, content, due_to, overdue, done.
               -- It did NOT list comment.
               -- I should check the schema in src-init.adb.
               -- Schema: "comment TEXT DEFAULT NULL"
               -- So I can add it.

               -- Let's rewrite the INSERT construction to be cleaner and include comment.
               Sql_Cmd :=
                 To_Unbounded_String
                   ("INSERT INTO tasks (id, subject, content, due_to, overdue, done, comment) VALUES ('");
               Append (Sql_Cmd, To_String (Task_Id) & "', ");

               if Length (Subject) = 0 then
                  Append (Sql_Cmd, "NULL, '");
               else
                  Append
                    (Sql_Cmd,
                     "'" & Src.Sql.Escape_Sql (To_String (Subject)) & "', '");
               end if;

               Append
                 (Sql_Cmd, Src.Sql.Escape_Sql (To_String (Content)) & "', '");
               Append (Sql_Cmd, To_String (Due_Date_Str) & "', ");
               Append (Sql_Cmd, Integer'Image (Overdue) & ", ");

               if Idx_Done > 0
                 and then Idx_Done <= Integer (Fields.Length)
                 and then Fields.Element (Idx_Done)'Length > 0
               then
                  Append (Sql_Cmd, "'" & Fields.Element (Idx_Done) & "', ");
               else
                  Append (Sql_Cmd, "NULL, ");
               end if;

               if Idx_Comment > 0
                 and then Idx_Comment <= Integer (Fields.Length)
                 and then Fields.Element (Idx_Comment)'Length > 0
               then
                  Append
                    (Sql_Cmd,
                     "'"
                     & Src.Sql.Escape_Sql (Fields.Element (Idx_Comment))
                     & "'");
               else
                  Append (Sql_Cmd, "NULL");
               end if;

               Append (Sql_Cmd, ");");

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
