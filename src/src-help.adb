package body Src.Help is

   function Get_Help_String (Command : String) return String is
   begin
      if Command = "brex" then
         return "Usage: brex < command > < subcommand > < argument >" & ASCII.LF &
                "" & ASCII.LF &
                "brex init" & ASCII.LF &
                "brex note < add | edit | connect | last >" & ASCII.LF &
                "brex task < add | list | done | delay | last >" & ASCII.LF &
                "brex update < file >" & ASCII.LF &
                "brex sql" & ASCII.LF &
                "" & ASCII.LF &
                "defaults:" & ASCII.LF &
                "init -> sqlite database only" & ASCII.LF &
                "note -> log note add/edit" & ASCII.LF &
                "task -> add new task" & ASCII.LF &
                "update -> rebuild from vault" & ASCII.LF &
                "sql -> sqlite shell" & ASCII.LF &
                "" & ASCII.LF &
                "brex < command > -h or --help for more info";
      elsif Command = "brex init" then
         return "    Description:" & ASCII.LF &
                "    Initializes a new brain-ex database in the current directory." & ASCII.LF &
                "    If a vault directory is specified, it will also import notes and tasks from the vault." & ASCII.LF &
                "" & ASCII.LF &
                "    Options:" & ASCII.LF &
                "    -n --name <name>      Name of the brain database (default: ""brain"")." & ASCII.LF &
                "    -v --vault <vault>    Path to the vault directory to import notes and tasks from." & ASCII.LF &
                "    -e --editor <editor>  Default text editor to use (default: ""nano"")." & ASCII.LF &
                "" & ASCII.LF &
                "    Examples:" & ASCII.LF &
                "    brex init" & ASCII.LF &
                "    brex init --name ""my_brain""" & ASCII.LF &
                "    brex init --vault ""my_vault""" & ASCII.LF &
                "    brex init --name ""my_brain"" --vault ""my_vault"" --editor ""vim""" & ASCII.LF &
                "    brex init --name ""my_brain"" --vault ""my_vault"" --editor ""vim"" --git";
      elsif Command = "brex note" then
         return "Description:" & ASCII.LF &
                "Create log note with the specified content." & ASCII.LF &
                "Links can be provided as a comma-separated list." & ASCII.LF &
                "" & ASCII.LF &
                "Required:" & ASCII.LF &
                "-c --content <content> Note's content." & ASCII.LF &
                "" & ASCII.LF &
                "Optional:" & ASCII.LF &
                "-l --links <links> Links to other notes, separated by commas." & ASCII.LF &
                "" & ASCII.LF &
                "Examples:" & ASCII.LF &
                "brex note --content ""This is log note content""" & ASCII.LF &
                "brex note --content ""This is log note content"" --links ""link1,link2""";
      -- Add other commands as needed, keeping it brief for now to save space
      else
         return "Unknown command: " & Command;
      end if;
   end Get_Help_String;

end Src.Help;
