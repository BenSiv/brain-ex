with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Calendar;
with Ada.Calendar.Formatting;
with Ada.Strings.Unbounded;
with Ada.Strings.Fixed;
with Src.Sql;
with Src.Config;
with Src.Help;
with Src.Utils;
with Ada.Directories;
with GNAT.OS_Lib;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Src.Note is
   use Ada.Strings.Unbounded;

   procedure Do_Note (Brain_File : String) is
      use Ada.Command_Line;

      Subcommand  : Unbounded_String;
      Subject     : Unbounded_String := To_Unbounded_String ("log");
      Title       : Unbounded_String;
      Content     : Unbounded_String;
      Links       : Unbounded_String;
      Number      : Integer := 5;
      From_File   : Boolean := False;
      Update_Mode : Boolean := False;

      Success : Boolean;

      procedure Parse_Args is
         I : Integer := 1;
      begin
         -- First argument is "note", second might be subcommand
         if Argument_Count >= 2 and then Argument (2) (1) /= '-' then
            Subcommand := To_Unbounded_String (Argument (2));
            I := 3;
         else
            I := 2;
         end if;

         while I <= Argument_Count loop
            if Argument (I) = "-s" or Argument (I) = "--subject" then
               if I + 1 <= Argument_Count then
                  Subject := To_Unbounded_String (Argument (I + 1));
                  I := I + 1;
               end if;
            elsif Argument (I) = "-t" or Argument (I) = "--title" then
               if I + 1 <= Argument_Count then
                  Title := To_Unbounded_String (Argument (I + 1));
                  I := I + 1;
               end if;
            elsif Argument (I) = "-c" or Argument (I) = "--content" then
               if I + 1 <= Argument_Count then
                  Content := To_Unbounded_String (Argument (I + 1));
                  I := I + 1;
               end if;
            elsif Argument (I) = "-l" or Argument (I) = "--links" then
               if I + 1 <= Argument_Count then
                  Links := To_Unbounded_String (Argument (I + 1));
                  I := I + 1;
               end if;
            elsif Argument (I) = "-n" or Argument (I) = "--number" then
               if I + 1 <= Argument_Count then
                  Number := Integer'Value (Argument (I + 1));
                  I := I + 1;
               end if;
            elsif Argument (I) = "--from-file" then
               From_File := True;
            elsif Argument (I) = "--update" then
               Update_Mode := True;
            end if;
            I := I + 1;
         end loop;
      end Parse_Args;

      procedure Insert_Note (S, T, C : String) is
         Sql_Cmd    : String :=
           "INSERT INTO notes ('subject', 'title', 'content') VALUES ('"
           & S
           & "', '"
           & T
           & "', '"
           & C
           & "');";
         Vault_Path : String := Src.Config.Get_Vault_Path;
         Note_File  : Ada.Text_IO.File_Type;
         File_Path  : Unbounded_String;
      begin
         Src.Sql.Execute (Brain_File, Sql_Cmd, Success);
         if not Success then
            Ada.Text_IO.Put_Line ("Failed to update database");
            return;
         end if;

         -- Create file in vault if vault is configured
         if Vault_Path /= "" then
            declare
               Subject_Dir : constant String := Vault_Path & "/" & S;
            begin
               -- Create subject directory if it doesn't exist
               if not Ada.Directories.Exists (Subject_Dir) then
                  Ada.Directories.Create_Path (Subject_Dir);
               end if;

               File_Path :=
                 To_Unbounded_String (Subject_Dir & "/" & T & ".md");

               -- Write content to file
               Ada.Text_IO.Create
                 (Note_File, Ada.Text_IO.Out_File, To_String (File_Path));
               Ada.Text_IO.Put_Line (Note_File, C);
               Ada.Text_IO.Close (Note_File);
            end;
         end if;
      end Insert_Note;

      procedure Log_Note is
         Now             : Ada.Calendar.Time := Ada.Calendar.Clock;
         Timestamp       : String := Ada.Calendar.Formatting.Image (Now);
         -- Format: YYYY-MM-DD HH:MM:SS.SS
         -- We want: YYYY-MM-DD_HH-MM-SS
         Formatted_Title : String (1 .. 19);
      begin
         -- Copy date part
         Formatted_Title (1 .. 10) := Timestamp (1 .. 10);
         -- Replace space with underscore
         Formatted_Title (11) := '_';
         -- Copy hour
         Formatted_Title (12 .. 13) := Timestamp (12 .. 13);
         -- Replace colon with dash
         Formatted_Title (14) := '-';
         -- Copy minute
         Formatted_Title (15 .. 16) := Timestamp (15 .. 16);
         -- Replace colon with dash
         Formatted_Title (17) := '-';
         -- Copy second
         Formatted_Title (18 .. 19) := Timestamp (18 .. 19);

         if Content = Null_Unbounded_String then
            Ada.Text_IO.Put_Line ("Must provide note content");
            return;
         end if;

         Insert_Note ("log", Formatted_Title, To_String (Content));
      end Log_Note;

      procedure Add_Note is
         Vault_Path       : constant String := Src.Config.Get_Vault_Path;
         File_Path        : Unbounded_String;
         Existing_Content : Unbounded_String;
      begin
         if Title = Null_Unbounded_String then
            if Subject /= To_Unbounded_String ("log") then
               Ada.Text_IO.Put_Line
                 ("Error: Must provide title of note to edit");
               Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
               return;
            end if;
            Ada.Text_IO.Put_Line ("Must provide note title");
            return;
         end if;
         if Content = Null_Unbounded_String then
            Ada.Text_IO.Put_Line ("Must provide note content");
            return;
         end if;

         -- Handle update mode: append to existing content
         if Update_Mode and Vault_Path /= "" then
            File_Path :=
              To_Unbounded_String
                (Vault_Path
                 & "/"
                 & To_String (Subject)
                 & "/"
                 & To_String (Title)
                 & ".md");
            if Ada.Directories.Exists (To_String (File_Path)) then
               -- Read existing content
               declare
                  Input_File : Ada.Text_IO.File_Type;
                  Line       : String (1 .. 1024);
                  Last       : Natural;
               begin
                  Ada.Text_IO.Open
                    (Input_File, Ada.Text_IO.In_File, To_String (File_Path));
                  while not Ada.Text_IO.End_Of_File (Input_File) loop
                     Ada.Text_IO.Get_Line (Input_File, Line, Last);
                     Append (Existing_Content, Line (1 .. Last));
                     if not Ada.Text_IO.End_Of_File (Input_File) then
                        Append (Existing_Content, ASCII.LF);
                     end if;
                  end loop;
                  Ada.Text_IO.Close (Input_File);
               exception
                  when others =>
                     null;
               end;

               -- Append new content
               if Existing_Content /= Null_Unbounded_String then
                  Content := Existing_Content & ASCII.LF & Content;
               end if;
            end if;
         end if;

         Insert_Note
           (To_String (Subject), To_String (Title), To_String (Content));
      end Add_Note;

      procedure Connect_Links is
         -- Parse and insert links to connections table
         Link_List          : Unbounded_String := Links;
         Pos                : Natural;
         Link               : Unbounded_String;
         Target_Subject     : Unbounded_String;
         Target_Title       : Unbounded_String;
         Sql_Cmd            : Unbounded_String;
         Source_Title_Str   : Unbounded_String := Title;
         Source_Subject_Str : Unbounded_String := Subject;
      begin
         -- If no title for source, use current log note
         if Title = Null_Unbounded_String then
            -- Get latest log note
            declare
               Now             : constant Ada.Calendar.Time :=
                 Ada.Calendar.Clock;
               Timestamp       : constant String :=
                 Ada.Calendar.Formatting.Image (Now);
               Formatted_Title : Unbounded_String;
               Vault_Path      : constant String := Src.Config.Get_Vault_Path;
               Note_File       : Ada.Text_IO.File_Type;
               Temp_Title      : String (1 .. 19);
            begin
               -- Build formatted title
               Temp_Title (1 .. 10) := Timestamp (1 .. 10);
               Temp_Title (11) := '_';
               Temp_Title (12 .. 13) := Timestamp (12 .. 13);
               Temp_Title (14) := '-';
               Temp_Title (15 .. 16) := Timestamp (15 .. 16);
               Temp_Title (17) := '-';
               Temp_Title (18 .. 19) := Timestamp (18 .. 19);
               Formatted_Title := To_Unbounded_String (Temp_Title);
               Source_Title_Str := Formatted_Title;
               Source_Subject_Str := To_Unbounded_String ("log");

               -- Create the log note if content exists, or create empty if just connecting
               if Content /= Null_Unbounded_String then
                  Insert_Note
                    ("log", To_String (Formatted_Title), To_String (Content));
               else
                  -- Just create empty note for connection
                  Insert_Note ("log", To_String (Formatted_Title), "");
               end if;

               -- Also create the file with link
               if Vault_Path /= "" then
                  declare
                     Subject_Dir  : constant String := Vault_Path & "/log";
                     File_Path    : constant String :=
                       Subject_Dir & "/" & To_String (Formatted_Title) & ".md";
                     Link_Content : Unbounded_String;
                  begin
                     if not Ada.Directories.Exists (Subject_Dir) then
                        Ada.Directories.Create_Path (Subject_Dir);
                     end if;

                     -- Create file with [[links]]
                     Ada.Text_IO.Create
                       (Note_File, Ada.Text_IO.Out_File, File_Path);
                     if Content /= Null_Unbounded_String then
                        Ada.Text_IO.Put_Line (Note_File, To_String (Content));
                     end if;

                     -- Add [[link]] format for each link
                     declare
                        Link_List_Copy : Unbounded_String := Links;
                        Pos            : Natural;
                        Link           : Unbounded_String;
                     begin
                        while Link_List_Copy /= Null_Unbounded_String loop
                           Pos := Index (Link_List_Copy, ",");
                           if Pos = 0 then
                              Link := Link_List_Copy;
                              Link_List_Copy := Null_Unbounded_String;
                           else
                              Link :=
                                To_Unbounded_String
                                  (Slice (Link_List_Copy, 1, Pos - 1));
                              Link_List_Copy :=
                                To_Unbounded_String
                                  (Slice
                                     (Link_List_Copy,
                                      Pos + 1,
                                      Length (Link_List_Copy)));
                           end if;
                           Ada.Text_IO.Put_Line
                             (Note_File, "[[" & To_String (Link) & "]]");
                        end loop;
                     end;

                     Ada.Text_IO.Close (Note_File);
                  end;
               end if;
            end;
         end if;

         -- Parse comma-separated links
         while Link_List /= Null_Unbounded_String loop
            Pos := Index (Link_List, ",");
            if Pos = 0 then
               Link := Link_List;
               Link_List := Null_Unbounded_String;
            else
               Link := To_Unbounded_String (Slice (Link_List, 1, Pos - 1));
               Link_List :=
                 To_Unbounded_String
                   (Slice (Link_List, Pos + 1, Length (Link_List)));
            end if;

            -- Parse link format: subject/title or just title
            Pos := Index (Link, "/");
            if Pos = 0 then
               Target_Subject := Null_Unbounded_String;
               Target_Title := Link;
            else
               Target_Subject :=
                 To_Unbounded_String (Slice (Link, 1, Pos - 1));
               Target_Title :=
                 To_Unbounded_String (Slice (Link, Pos + 1, Length (Link)));
            end if;

            -- Insert connection
            if Target_Subject = Null_Unbounded_String then
               Sql_Cmd :=
                 To_Unbounded_String
                   ("INSERT OR IGNORE INTO connections (source_title, source_subject, target_title, target_subject) VALUES ('"
                    & To_String (Source_Title_Str)
                    & "', '"
                    & To_String (Source_Subject_Str)
                    & "', '"
                    & To_String (Target_Title)
                    & "', '');");
            else
               Sql_Cmd :=
                 To_Unbounded_String
                   ("INSERT OR IGNORE INTO connections (source_title, source_subject, target_title, target_subject) VALUES ('"
                    & To_String (Source_Title_Str)
                    & "', '"
                    & To_String (Source_Subject_Str)
                    & "', '"
                    & To_String (Target_Title)
                    & "', '"
                    & To_String (Target_Subject)
                    & "');");
            end if;

            Src.Sql.Execute (Brain_File, To_String (Sql_Cmd), Success);
         end loop;
      end Connect_Links;

      procedure Edit_Note is
         Vault_Path  : constant String := Src.Config.Get_Vault_Path;
         Editor      : constant String := Src.Config.Get_Default_Editor;
         File_Path   : Unbounded_String;
         Note_File   : Ada.Text_IO.File_Type;
         Args        : GNAT.OS_Lib.Argument_List (1 .. 1);
         Return_Code : Integer;
      begin
         if Title = Null_Unbounded_String then
            Ada.Text_IO.Put_Line ("Must provide note title");
            return;
         end if;

         if Vault_Path /= "" then
            declare
               Subject_Dir : constant String :=
                 Vault_Path & "/" & To_String (Subject);
            begin
               if not Ada.Directories.Exists (Subject_Dir) then
                  Ada.Directories.Create_Path (Subject_Dir);
               end if;

               File_Path :=
                 To_Unbounded_String
                   (Subject_Dir & "/" & To_String (Title) & ".md");

               -- Create empty file if doesn't exist
               if not Ada.Directories.Exists (To_String (File_Path)) then
                  Ada.Text_IO.Create
                    (Note_File, Ada.Text_IO.Out_File, To_String (File_Path));
                  Ada.Text_IO.Close (Note_File);

                  -- Insert empty note in database
                  Insert_Note (To_String (Subject), To_String (Title), "");
               end if;

               -- Open in editor
               Args (1) := new String'(To_String (File_Path));
               Return_Code := GNAT.OS_Lib.Spawn (Editor, Args);
               GNAT.OS_Lib.Free (Args (1));
            end;
         end if;
      end Edit_Note;

      procedure Last_Notes is
         Sql_Cmd : String :=
           "SELECT title, content FROM notes WHERE subject='"
           & To_String (Subject)
           & "' ORDER BY title DESC LIMIT "
           & Integer'Image (Number);
         Result  : Src.Sql.Result_Type;
      begin
         Result := Src.Sql.Query (Brain_File, Sql_Cmd);
         if Result.Is_Empty then
            Ada.Text_IO.Put_Line ("No notes available");
         else
            for Row of Result loop
               Ada.Text_IO.Put_Line (Row.Element ("title"));
               Ada.Text_IO.Put_Line (Row.Element ("content"));
               Ada.Text_IO.New_Line;
            end loop;
         end if;
      end Last_Notes;

   begin
      Parse_Args;

      if Subcommand = "add" then
         Add_Note;
         if Links /= Null_Unbounded_String then
            Connect_Links;
         end if;
      elsif Subcommand = "connect" then
         Connect_Links;
      elsif Subcommand = "edit" then
         Edit_Note;
      elsif Subcommand = "last" then
         Last_Notes;
      elsif Subcommand = "update" then
         -- Handle note update command
         if From_File then
            -- Update from file
            declare
               Vault_Path : constant String := Src.Config.Get_Vault_Path;
               File_Path  : Unbounded_String;
            begin
               if Title = Null_Unbounded_String then
                  Ada.Text_IO.Put_Line ("Must provide note title for update");
                  Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
                  return;
               end if;

               if Vault_Path = "" then
                  Ada.Text_IO.Put_Line ("No vault configured");
                  Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
                  return;
               end if;

               File_Path :=
                 To_Unbounded_String
                   (Vault_Path
                    & "/"
                    & To_String (Subject)
                    & "/"
                    & To_String (Title)
                    & ".md");

               if not Ada.Directories.Exists (To_String (File_Path)) then
                  Ada.Text_IO.Put_Line
                    ("File does not exist: " & To_String (File_Path));
                  Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
                  return;
               end if;

               -- Read file content
               declare
                  Input_File   : Ada.Text_IO.File_Type;
                  Line         : String (1 .. 1024);
                  Last         : Natural;
                  File_Content : Unbounded_String;
                  Sql_Cmd      : Unbounded_String;
               begin
                  Ada.Text_IO.Open
                    (Input_File, Ada.Text_IO.In_File, To_String (File_Path));
                  while not Ada.Text_IO.End_Of_File (Input_File) loop
                     Ada.Text_IO.Get_Line (Input_File, Line, Last);
                     Append (File_Content, Line (1 .. Last));
                     if not Ada.Text_IO.End_Of_File (Input_File) then
                        Append (File_Content, ASCII.LF);
                     end if;
                  end loop;
                  Ada.Text_IO.Close (Input_File);

                  -- Update database
                  Sql_Cmd :=
                    To_Unbounded_String
                      ("UPDATE notes SET content = '"
                       & To_String (File_Content)
                       & "' WHERE subject = '"
                       & To_String (Subject)
                       & "' AND title = '"
                       & To_String (Title)
                       & "';");
                  Src.Sql.Execute (Brain_File, To_String (Sql_Cmd), Success);
                  if not Success then
                     Ada.Text_IO.Put_Line ("Failed to update database");
                     Ada.Command_Line.Set_Exit_Status
                       (Ada.Command_Line.Failure);
                  end if;
               exception
                  when others =>
                     Ada.Text_IO.Put_Line ("Error reading file");
                     Ada.Command_Line.Set_Exit_Status
                       (Ada.Command_Line.Failure);
               end;
            end;
         else
            Ada.Text_IO.Put_Line ("Update command requires --from-file flag");
            Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
         end if;
      elsif Subcommand = Null_Unbounded_String then
         -- Check for error: subject without title
         if Subject /= To_Unbounded_String ("log")
           and Title = Null_Unbounded_String
         then
            Ada.Text_IO.Put_Line ("Error: Must provide title of note to edit");
            Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
            return;
         end if;
         Log_Note;
         if Links /= Null_Unbounded_String then
            Connect_Links;
         end if;
      else
         Ada.Text_IO.Put_Line
           ("Unknown subcommand: " & To_String (Subcommand));
      end if;

   end Do_Note;

end Src.Note;
