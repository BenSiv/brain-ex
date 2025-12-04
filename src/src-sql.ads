with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;

package Src.Sql is

   package String_Hashed_Maps is new
     Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => String,
        Element_Type    => String,
        Hash            => Ada.Strings.Hash,
        Equivalent_Keys => "=");

   subtype Row_Type is String_Hashed_Maps.Map;

   package Row_Vectors is new
     Ada.Containers.Indefinite_Vectors
       (Index_Type   => Positive,
        Element_Type => Row_Type,
        "="          => String_Hashed_Maps."=");

   subtype Result_Type is Row_Vectors.Vector;

   procedure Execute
     (Db_Path : String; Sql_Cmd : String; Success : out Boolean);
   function Query (Db_Path : String; Sql_Cmd : String) return Result_Type;
   function Escape_Sql (S : String) return String;
   procedure Shell (Db_Path : String);

end Src.Sql;
