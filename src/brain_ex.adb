with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Strings.Unbounded;
with Src.Init;
with Src.Note;
with Src.Tasks;
with Src.Update;
with Src.Sql;
with Src.Config;
with Src.Help;

procedure Brain_Ex is
   use Ada.Command_Line;
   use Ada.Strings.Unbounded;

   Command    : Unbounded_String;
   Brain_File : Unbounded_String;

begin
   if Argument_Count < 1 then
      Ada.Text_IO.Put_Line ("Missing command");
      Ada.Text_IO.Put_Line (Src.Help.Get_Help_String ("brex"));
      return;
   end if;

   Command := To_Unbounded_String (Argument (1));

   if Command = "init" then
      Src.Init.Do_Init;
      return;
   elsif Command = "help" or Command = "-h" or Command = "--help" then
      Ada.Text_IO.Put_Line (Src.Help.Get_Help_String ("brex"));
      return;
   end if;

   -- For other commands, we need the brain file
   Brain_File := To_Unbounded_String (Src.Config.Get_Brain_Path);
   if Brain_File = Null_Unbounded_String then
      -- Error already printed by Get_Brain_Path if config missing
      Set_Exit_Status (Failure);
      return;
   end if;

   if Command = "note" then
      Src.Note.Do_Note (To_String (Brain_File));
   elsif Command = "task" then
      Src.Tasks.Do_Task (To_String (Brain_File));
   elsif Command = "update" then
      Src.Update.Do_Update (To_String (Brain_File));
   elsif Command = "sql" then
      -- Simple SQL shell or query
      -- Check args for query
      declare
         Query_Str : Unbounded_String;
         Is_Query  : Boolean := False;
         I         : Integer := 2;
      begin
         while I <= Argument_Count loop
            if Argument (I) = "-q" or Argument (I) = "--query" then
               if I + 1 <= Argument_Count then
                  Query_Str := To_Unbounded_String (Argument (I + 1));
                  Is_Query := True;
                  I := I + 1;
               end if;
            end if;
            I := I + 1;
         end loop;

         if Is_Query then
            declare
               Result : Src.Sql.Result_Type;
            begin
               Result :=
                 Src.Sql.Query (To_String (Brain_File), To_String (Query_Str));
               -- Print result
               if Result.Is_Empty then
                  null;
               else
                  -- Print headers?
                  -- Result is vector of maps.
                  -- We don't have ordered headers easily available here unless we query them or assume.
                  -- Just print content for now.
                  for Row of Result loop
                     -- Iterate over map?
                     -- Ada maps iteration.
                     null; -- TODO: better printing
                  end loop;
               end if;
            end;
         else
            Src.Sql.Shell (To_String (Brain_File));
         end if;
      end;
   else
      Ada.Text_IO.Put_Line
        ("'" & To_String (Command) & "' is not a valid command");
      Ada.Text_IO.Put_Line (Src.Help.Get_Help_String ("brex"));
   end if;

end Brain_Ex;
