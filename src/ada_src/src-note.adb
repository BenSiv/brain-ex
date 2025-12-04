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

package body Src.Note is
   use Ada.Strings.Unbounded;

   procedure Do_Note (Brain_File : String) is
      use Ada.Command_Line;

      Subcommand : Unbounded_String;
      Subject    : Unbounded_String := To_Unbounded_String ("log");
      Title      : Unbounded_String;
      Content    : Unbounded_String;
      Links      : Unbounded_String;
      Number     : Integer := 5;

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
            end if;
            I := I + 1;
         end loop;
      end Parse_Args;

      procedure Insert_Note (S, T, C : String) is
         Sql_Cmd : String :=
           "INSERT INTO notes ('subject', 'title', 'content') VALUES ('"
           & S
           & "', '"
           & T
           & "', '"
           & C
           & "');";
      begin
         Src.Sql.Execute (Brain_File, Sql_Cmd, Success);
         if not Success then
            Ada.Text_IO.Put_Line ("Failed to update database");
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
      begin
         if Title = Null_Unbounded_String then
            Ada.Text_IO.Put_Line ("Must provide note title");
            return;
         end if;
         if Content = Null_Unbounded_String then
            Ada.Text_IO.Put_Line ("Must provide note content");
            return;
         end if;

         Insert_Note
           (To_String (Subject), To_String (Title), To_String (Content));
      end Add_Note;

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
      elsif Subcommand = "last" then
         Last_Notes;
      elsif Subcommand = Null_Unbounded_String then
         Log_Note;
      else
         Ada.Text_IO.Put_Line
           ("Unknown subcommand: " & To_String (Subcommand));
      end if;

   end Do_Note;

end Src.Note;
