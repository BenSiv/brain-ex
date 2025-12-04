with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Strings.Unbounded;
with Ada.Strings.Fixed;
with Ada.Directories;
with Ada.Direct_IO;
with Src.Sql;
with Src.Config;
with Src.Help;

package body Src.Update is
   use Ada.Strings.Unbounded;

   procedure Do_Update (Brain_File : String) is
      use Ada.Command_Line;
      
      File_Path : Unbounded_String;
      
      procedure Parse_Args is
         I : Integer := 1;
      begin
         while I <= Argument_Count loop
            if Argument (I) = "-f" or Argument (I) = "--file" then
               if I + 1 <= Argument_Count then
                  File_Path := To_Unbounded_String (Argument (I + 1));
                  I := I + 1;
               end if;
            end if;
            I := I + 1;
         end loop;
      end Parse_Args;
      
      function Read_File (Path : String) return String is
         File_Size : Natural := Natural (Ada.Directories.Size (Path));
         subtype File_String is String (1 .. File_Size);
         package File_IO is new Ada.Direct_IO (File_String);
         File : File_IO.File_Type;
         Content : File_String;
      begin
         File_IO.Open (File, File_IO.In_File, Path);
         File_IO.Read (File, Content);
         File_IO.Close (File);
         return Content;
      exception
         when others =>
            return "";
      end Read_File;

      procedure Update_Note_From_File (Path : String) is
         Content : Unbounded_String;
         Title : Unbounded_String;
         Subject : Unbounded_String;
         -- Simple parsing of path: .../subject/title.md
         Last_Slash : Natural;
         Second_Last_Slash : Natural;
         Dot_Md : Natural;
         
         Sql_Cmd : Unbounded_String;
         Success : Boolean;
         Count_Result : Src.Sql.Result_Type;
         Count : Integer := 0;
      begin
         if not Ada.Directories.Exists (Path) then
            Ada.Text_IO.Put_Line ("File does not exist: " & Path);
            return;
         end if;
         
         Content := To_Unbounded_String (Read_File (Path));
         if Content = Null_Unbounded_String then
            Ada.Text_IO.Put_Line ("Failed to read file or empty: " & Path);
            return;
         end if;
         
         -- Parse path for Subject and Title
         Last_Slash := Ada.Strings.Fixed.Index (Path, "/", Going => Ada.Strings.Backward);
         if Last_Slash = 0 then
            Title := To_Unbounded_String (Path);
            Subject := To_Unbounded_String (""); -- Root?
         else
            Title := To_Unbounded_String (Path (Last_Slash + 1 .. Path'Last));
            Second_Last_Slash := Ada.Strings.Fixed.Index (Path (Path'First .. Last_Slash - 1), "/", Going => Ada.Strings.Backward);
            if Second_Last_Slash = 0 then
               Subject := To_Unbounded_String (Path (Path'First .. Last_Slash - 1));
            else
               Subject := To_Unbounded_String (Path (Second_Last_Slash + 1 .. Last_Slash - 1));
            end if;
         end if;
         
         -- Remove extension from title
         Dot_Md := Ada.Strings.Fixed.Index (To_String (Title), ".md", Going => Ada.Strings.Backward);
         if Dot_Md > 0 then
            Title := Unbounded_Slice (Title, 1, Dot_Md - 1);
         end if;
         
         -- Check if note exists
         Sql_Cmd := To_Unbounded_String ("SELECT COUNT(*) AS num FROM notes WHERE subject = '" & To_String (Subject) & "' AND title = '" & To_String (Title) & "';");
         Count_Result := Src.Sql.Query (Brain_File, To_String (Sql_Cmd));
         if not Count_Result.Is_Empty then
            Count := Integer'Value (Count_Result.First_Element.Element ("num"));
         end if;
         
         -- Escape single quotes in content
         -- TODO: Implement proper escaping
         
         if Count > 0 then
            Sql_Cmd := To_Unbounded_String ("UPDATE notes SET content = '" & To_String (Content) & "' WHERE subject = '" & To_String (Subject) & "' AND title = '" & To_String (Title) & "';");
         else
            Sql_Cmd := To_Unbounded_String ("INSERT INTO notes (subject, title, content) VALUES ('" & To_String (Subject) & "', '" & To_String (Title) & "', '" & To_String (Content) & "');");
         end if;
         
         Src.Sql.Execute (Brain_File, To_String (Sql_Cmd), Success);
         if Success then
            Ada.Text_IO.Put_Line ("Updated note: " & To_String (Subject) & "/" & To_String (Title));
         else
            Ada.Text_IO.Put_Line ("Failed to update note in DB");
         end if;
         
      end Update_Note_From_File;

   begin
      Parse_Args;
      
      if File_Path /= Null_Unbounded_String then
         Update_Note_From_File (To_String (File_Path));
      else
         Ada.Text_IO.Put_Line ("Full vault update not yet implemented in Ada version.");
         Ada.Text_IO.Put_Line ("Please use --file to update a specific note.");
      end if;
      
   end Do_Update;

end Src.Update;
