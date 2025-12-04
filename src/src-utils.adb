with Ada.Strings.Maps;
with Ada.Numerics.Discrete_Random;

package body Src.Utils is

   function Is_Timestamp (Str : String) return Boolean is
      -- Pattern: ^%d%d%d%d%-%d%d%-%d%d %d%d:%d%d:%d%d$
      -- Example: 2023-10-27 10:00:00
      use Ada.Strings.Maps;
      Digit_Set : constant Character_Set := To_Set ("0123456789");
   begin
      if Str'Length /= 19 then
         return False;
      end if;

      if not Is_Subset (To_Set (Str (Str'First .. Str'First + 3)), Digit_Set) then return False; end if;
      if Str (Str'First + 4) /= '-' then return False; end if;
      if not Is_Subset (To_Set (Str (Str'First + 5 .. Str'First + 6)), Digit_Set) then return False; end if;
      if Str (Str'First + 7) /= '-' then return False; end if;
      if not Is_Subset (To_Set (Str (Str'First + 8 .. Str'First + 9)), Digit_Set) then return False; end if;
      if Str (Str'First + 10) /= ' ' then return False; end if;
      if not Is_Subset (To_Set (Str (Str'First + 11 .. Str'First + 12)), Digit_Set) then return False; end if;
      if Str (Str'First + 13) /= ':' then return False; end if;
      if not Is_Subset (To_Set (Str (Str'First + 14 .. Str'First + 15)), Digit_Set) then return False; end if;
      if Str (Str'First + 16) /= ':' then return False; end if;
      if not Is_Subset (To_Set (Str (Str'First + 17 .. Str'First + 18)), Digit_Set) then return False; end if;

      return True;
   end Is_Timestamp;

   function Generate_Id (Desired_Length : Positive := 10) return String is
      subtype Digit is Integer range 0 .. 9;
      package Random_Digit is new Ada.Numerics.Discrete_Random (Digit);
      G : Random_Digit.Generator;
      Result : String (1 .. Desired_Length);
      D : Digit;
   begin
      Random_Digit.Reset (G);
      for I in Result'Range loop
         D := Random_Digit.Random (G);
         Result (I) := Character'Val (Character'Pos ('0') + D);
      end loop;
      return Result;
   end Generate_Id;

end Src.Utils;
