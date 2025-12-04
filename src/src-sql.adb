with GNAT.OS_Lib;
with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Strings.Fixed;
with Ada.Directories;

package body Src.Sql is
   use Ada.Strings.Unbounded;

   procedure Execute
     (Db_Path : String; Sql_Cmd : String; Success : out Boolean)
   is
      Args         : GNAT.OS_Lib.Argument_List (1 .. 2);
      Return_Code  : Integer;
      Command_Line : Unbounded_String;

      -- Helper to escape single quotes for shell
      function Escape_For_Shell (S : String) return String is
         Result : Unbounded_String;
      begin
         for I in S'Range loop
            if S (I) = ''' then
               Append
                 (Result,
                  "'\''");  -- Close quote, add escaped quote, open quote

            else
               Append (Result, S (I));
            end if;
         end loop;
         return To_String (Result);
      end Escape_For_Shell;

   begin
      -- Construct shell command: printf "%s\n" "SQL" | sqlite3 Db_Path
      Command_Line :=
        To_Unbounded_String
          ("printf '%s\n' '"
           & Escape_For_Shell (Sql_Cmd)
           & "' | sqlite3 -cmd '.timeout 5000' '"
           & Db_Path
           & "'");

      Args (1) := new String'("-c");
      Args (2) := new String'(To_String (Command_Line));

      Return_Code := GNAT.OS_Lib.Spawn ("/bin/sh", Args);
      Success := (Return_Code = 0);

      if not Success then
         Ada.Text_IO.Put_Line
           ("Spawn failed with code: " & Integer'Image (Return_Code));
      end if;

      GNAT.OS_Lib.Free (Args (1));
      GNAT.OS_Lib.Free (Args (2));
   end Execute;

   function Query (Db_Path : String; Sql_Cmd : String) return Result_Type is
      Args            : GNAT.OS_Lib.Argument_List (1 .. 2);
      Success         : Boolean;
      Tmp_FD          : GNAT.OS_Lib.File_Descriptor;
      Tmp_Name_Access : GNAT.OS_Lib.String_Access;
      File            : Ada.Text_IO.File_Type;
      Result          : Result_Type;
      Line            : Unbounded_String;
      Header_Line     : Unbounded_String;
      Row             : Row_Type;

      -- Helper to escape single quotes for shell
      function Escape_For_Shell (S : String) return String is
         Result_Str : Unbounded_String;
      begin
         for I in S'Range loop
            if S (I) = ''' then
               Append
                 (Result_Str,
                  "'\''");  -- Close quote, add escaped quote, open quote

            else
               Append (Result_Str, S (I));
            end if;
         end loop;
         return To_String (Result_Str);
      end Escape_For_Shell;

      -- Re-implementing Split for headers
      package String_Vectors is new
        Ada.Containers.Indefinite_Vectors (Positive, String);
      Header_Vec : String_Vectors.Vector;

      procedure Parse_Csv_Line (S : String; Fields : out String_Vectors.Vector)
      is
         Last : Natural := S'First;
         Curr : Natural;
      begin
         loop
            Curr := Ada.Strings.Fixed.Index (S, ",", Last);
            if Curr = 0 then
               Fields.Append (S (Last .. S'Last));
               exit;
            else
               Fields.Append (S (Last .. Curr - 1));
               Last := Curr + 1;
            end if;
         end loop;
      end Parse_Csv_Line;

      Command_Line : Unbounded_String;

   begin
      GNAT.OS_Lib.Create_Temp_File (Tmp_FD, Tmp_Name_Access);
      GNAT.OS_Lib.Close
        (Tmp_FD); -- Close it, we just want the name and empty file

      -- Construct shell command: sqlite3 -header -csv Db_Path "Sql_Cmd" > Temp_File_Name
      -- Need to escape single quotes in Sql_Cmd for shell
      Command_Line :=
        To_Unbounded_String
          ("sqlite3 -cmd '.timeout 5000' -header -csv '"
           & Db_Path
           & "' '"
           & Escape_For_Shell (Sql_Cmd)
           & "' > "
           & Tmp_Name_Access.all);

      Args (1) := new String'("-c");
      Args (2) := new String'(To_String (Command_Line));

      GNAT.OS_Lib.Spawn ("/bin/sh", Args, Success);

      GNAT.OS_Lib.Free (Args (1));
      GNAT.OS_Lib.Free (Args (2));

      if not Success then
         Ada.Text_IO.Put_Line ("Error executing query: " & Sql_Cmd);
         GNAT.OS_Lib.Free (Tmp_Name_Access);
         return Result;
      end if;

      Ada.Text_IO.Open (File, Ada.Text_IO.In_File, Tmp_Name_Access.all);

      if not Ada.Text_IO.End_Of_File (File) then
         Header_Line := To_Unbounded_String (Ada.Text_IO.Get_Line (File));
         Parse_Csv_Line (To_String (Header_Line), Header_Vec);
      end if;

      while not Ada.Text_IO.End_Of_File (File) loop
         Line := To_Unbounded_String (Ada.Text_IO.Get_Line (File));
         declare
            Fields : String_Vectors.Vector;
         begin
            Parse_Csv_Line (To_String (Line), Fields);
            Row.Clear;
            for I in 1 .. Integer (Fields.Length) loop
               if I <= Integer (Header_Vec.Length) then
                  Row.Include (Header_Vec.Element (I), Fields.Element (I));
               end if;
            end loop;
            Result.Append (Row);
         end;
      end loop;

      Ada.Text_IO.Close (File);
      Ada.Directories.Delete_File (Tmp_Name_Access.all);
      GNAT.OS_Lib.Free (Tmp_Name_Access);

      return Result;
   end Query;

   function Escape_Sql (S : String) return String is
      Result : Unbounded_String;
   begin
      for I in S'Range loop
         if S (I) = ''' then
            Append (Result, "''");
         else
            Append (Result, S (I));
         end if;
      end loop;
      return To_String (Result);
   end Escape_Sql;

   procedure Shell (Db_Path : String) is
      Args : GNAT.OS_Lib.Argument_List (1 .. 3);
   begin
      Args (1) := new String'("-column");
      Args (2) := new String'("-header");
      Args (3) := new String'(Db_Path);

      declare
         Unused : constant Integer := GNAT.OS_Lib.Spawn ("sqlite3", Args);
      begin
         null;
      end;

      GNAT.OS_Lib.Free (Args (1));
      GNAT.OS_Lib.Free (Args (2));
      GNAT.OS_Lib.Free (Args (3));
   end Shell;

end Src.Sql;
