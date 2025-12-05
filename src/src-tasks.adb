with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Calendar;
with Ada.Calendar.Formatting;
with Ada.Strings.Unbounded;
with Src.Sql;
with Src.Utils;

package body Src.Tasks is
   use Ada.Strings.Unbounded;

   procedure Do_Task (Brain_File : String) is
      use Ada.Command_Line;

      Subcommand : Unbounded_String;
      Subject    : Unbounded_String;
      Content    : Unbounded_String;
      Due_To     : Unbounded_String;
      Id         : Unbounded_String;
      Comment    : Unbounded_String;

      Success : Boolean;

      procedure Parse_Args is
         I : Integer := 1;
      begin
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
            elsif Argument (I) = "-c" or Argument (I) = "--content" then
               if I + 1 <= Argument_Count then
                  Content := To_Unbounded_String (Argument (I + 1));
                  I := I + 1;
               end if;
            elsif Argument (I) = "-t" or Argument (I) = "--due_to" then
               if I + 1 <= Argument_Count then
                  Due_To := To_Unbounded_String (Argument (I + 1));
                  I := I + 1;
               end if;
            elsif Argument (I) = "-i" or Argument (I) = "--id" then
               if I + 1 <= Argument_Count then
                  Id := To_Unbounded_String (Argument (I + 1));
                  I := I + 1;
               end if;
            elsif Argument (I) = "-m" or Argument (I) = "--comment" then
               if I + 1 <= Argument_Count then
                  Comment := To_Unbounded_String (Argument (I + 1));
                  I := I + 1;
               end if;
            end if;
            I := I + 1;
         end loop;
      end Parse_Args;

      procedure Add_Task is
         New_Id       : constant String := Src.Utils.Generate_Id;
         Due_Date     : Ada.Calendar.Time;
         Due_Date_Str : Unbounded_String;
         Overdue      : Integer := 0;
         Sql_Cmd      : Unbounded_String;
      begin
         if Content = Null_Unbounded_String then
            Ada.Text_IO.Put_Line ("Must provide task content");
            return;
         end if;

         if Due_To = Null_Unbounded_String then
            Due_Date :=
              Ada.Calendar."+" (Ada.Calendar.Clock, 86400.0); -- Tomorrow
            Due_Date_Str :=
              To_Unbounded_String
                (Ada.Calendar.Formatting.Image (Due_Date) (1 .. 19));
         else
            if not Src.Utils.Is_Timestamp (To_String (Due_To)) then
               Due_Date_Str := Due_To;
            else
               Due_Date_Str := Due_To;
            end if;
         end if;

         if To_String (Due_Date_Str)
           < Ada.Calendar.Formatting.Image (Ada.Calendar.Clock) (1 .. 19)
         then
            Overdue := 1;
         end if;

         -- Handle NULL subject
         if Subject = Null_Unbounded_String then
            Sql_Cmd :=
              To_Unbounded_String
                ("INSERT INTO tasks (id, subject, content, due_to, overdue, done) VALUES ('"
                 & New_Id
                 & "', NULL, '"
                 & To_String (Content)
                 & "', '"
                 & To_String (Due_Date_Str)
                 & "', '"
                 & Integer'Image (Overdue)
                 & "', NULL);");
         else
            Sql_Cmd :=
              To_Unbounded_String
                ("INSERT INTO tasks (id, subject, content, due_to, overdue, done) VALUES ('"
                 & New_Id
                 & "', '"
                 & To_String (Subject)
                 & "', '"
                 & To_String (Content)
                 & "', '"
                 & To_String (Due_Date_Str)
                 & "', '"
                 & Integer'Image (Overdue)
                 & "', NULL);");
         end if;

         Src.Sql.Execute (Brain_File, To_String (Sql_Cmd), Success);
         if not Success then
            Ada.Text_IO.Put_Line ("Failed to add task");
         end if;
      end Add_Task;

      procedure List_Tasks is
         Sql_Cmd : Unbounded_String :=
           To_Unbounded_String
             ("SELECT id, subject, content, due_to, overdue FROM tasks WHERE done IS NULL");
         Result  : Src.Sql.Result_Type;

         W_Id      : Natural := 2;  -- "ID"
         W_Subject : Natural := 7;  -- "Subject"
         W_Content : Natural := 4;  -- "Task"
         W_Due     : Natural := 8;  -- "Due Date"
         W_Status  : Natural := 6;  -- "Status"

         Gap : constant String := "  ";
      begin
         if Subject /= Null_Unbounded_String then
            Append (Sql_Cmd, " AND subject = '" & To_String (Subject) & "'");
         end if;
         if Due_To /= Null_Unbounded_String then
            Append (Sql_Cmd, " AND due_to > '" & To_String (Due_To) & "'");
         end if;
         Append (Sql_Cmd, " ORDER BY overdue DESC, due_to, subject;");

         Result := Src.Sql.Query (Brain_File, To_String (Sql_Cmd));
         if Result.Is_Empty then
            Ada.Text_IO.Put_Line ("No pending tasks");
         else
            -- Calculate widths
            for Row of Result loop
               declare
                  Id_Str      : constant String := Row.Element ("id");
                  Subject_Str : constant String :=
                    (if Row.Contains ("subject")
                     then Row.Element ("subject")
                     else "");
                  Content_Str : constant String := Row.Element ("content");
                  Due_Full    : constant String :=
                    (if Row.Contains ("due_to")
                     then Row.Element ("due_to")
                     else "");
                  Due_Str     : constant String :=
                    (if Due_Full'Length > 0
                     then
                       Due_Full
                         (Due_Full'First
                          .. Integer'Min (Due_Full'First + 9, Due_Full'Last))
                     else "");
               begin
                  W_Id := Integer'Max (W_Id, Id_Str'Length);
                  W_Subject := Integer'Max (W_Subject, Subject_Str'Length);
                  W_Content := Integer'Max (W_Content, Content_Str'Length);
                  W_Due := Integer'Max (W_Due, Due_Str'Length);
               end;
            end loop;

            -- Print Header
            Ada.Text_IO.Put (Character'Val (27) & "[1m");

            Ada.Text_IO.Put ("ID");
            for I in 1 .. W_Id - 2 loop
               Ada.Text_IO.Put (" ");
            end loop;
            Ada.Text_IO.Put (Gap);

            Ada.Text_IO.Put ("Subject");
            for I in 1 .. W_Subject - 7 loop
               Ada.Text_IO.Put (" ");
            end loop;
            Ada.Text_IO.Put (Gap);

            Ada.Text_IO.Put ("Task");
            for I in 1 .. W_Content - 4 loop
               Ada.Text_IO.Put (" ");
            end loop;
            Ada.Text_IO.Put (Gap);

            Ada.Text_IO.Put ("Due Date");
            for I in 1 .. W_Due - 8 loop
               Ada.Text_IO.Put (" ");
            end loop;
            Ada.Text_IO.Put (Gap);

            Ada.Text_IO.Put ("Status");
            -- Last column, no padding needed for header itself, but for consistency if we added more
            for I in 1 .. W_Status - 6 loop
               Ada.Text_IO.Put (" ");
            end loop;

            Ada.Text_IO.Put (Character'Val (27) & "[0m");
            Ada.Text_IO.New_Line;

            -- Print Rows
            for Row of Result loop
               declare
                  Id_Str      : constant String := Row.Element ("id");
                  Subject_Str : constant String :=
                    (if Row.Contains ("subject")
                     then Row.Element ("subject")
                     else "");
                  Content_Str : constant String := Row.Element ("content");
                  Due_Full    : constant String :=
                    (if Row.Contains ("due_to")
                     then Row.Element ("due_to")
                     else "");
                  Due_Str     : constant String :=
                    (if Due_Full'Length > 0
                     then
                       Due_Full
                         (Due_Full'First
                          .. Integer'Min (Due_Full'First + 9, Due_Full'Last))
                     else "");
                  Overdue_Str : constant String := Row.Element ("overdue");
                  Status      : constant String :=
                    (if Overdue_Str = "1" then "! OVER" else "  OK  ");
               begin
                  Ada.Text_IO.Put (Id_Str);
                  for I in 1 .. W_Id - Id_Str'Length loop
                     Ada.Text_IO.Put (" ");
                  end loop;
                  Ada.Text_IO.Put (Gap);

                  Ada.Text_IO.Put (Subject_Str);
                  for I in 1 .. W_Subject - Subject_Str'Length loop
                     Ada.Text_IO.Put (" ");
                  end loop;
                  Ada.Text_IO.Put (Gap);

                  Ada.Text_IO.Put (Content_Str);
                  for I in 1 .. W_Content - Content_Str'Length loop
                     Ada.Text_IO.Put (" ");
                  end loop;
                  Ada.Text_IO.Put (Gap);

                  Ada.Text_IO.Put (Due_Str);
                  for I in 1 .. W_Due - Due_Str'Length loop
                     Ada.Text_IO.Put (" ");
                  end loop;
                  Ada.Text_IO.Put (Gap);

                  Ada.Text_IO.Put (Status);
                  Ada.Text_IO.New_Line;
               end;
            end loop;
         end if;
      end List_Tasks;

      procedure Mark_Done is
         Sql_Cmd : Unbounded_String;
      begin
         if Id = Null_Unbounded_String then
            Ada.Text_IO.Put_Line ("Must provide task id");
            return;
         end if;

         Sql_Cmd :=
           To_Unbounded_String
             ("UPDATE tasks SET done = datetime('now', 'localtime'), comment = '"
              & To_String (Comment)
              & "' WHERE id = "
              & To_String (Id)
              & ";");
         Src.Sql.Execute (Brain_File, To_String (Sql_Cmd), Success);
         if not Success then
            Ada.Text_IO.Put_Line ("Failed to mark task as done");
         end if;
      end Mark_Done;

   begin
      Parse_Args;

      if Subcommand = "add" then
         Add_Task;
      elsif Subcommand = "list" then
         List_Tasks;
      elsif Subcommand = "done" then
         Mark_Done;
      elsif Subcommand = Null_Unbounded_String then
         Add_Task;
      else
         Ada.Text_IO.Put_Line
           ("Unknown subcommand: " & To_String (Subcommand));
      end if;

   end Do_Task;

end Src.Tasks;
