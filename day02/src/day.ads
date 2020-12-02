-- AOC 2020, Day 2
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Day is
  type Password_Entry is record
    Min : Natural := 0;
    Max : Natural := 0;
    Pattern : Unbounded_String := Null_Unbounded_String;
    Password : Unbounded_String := Null_Unbounded_String;
  end record;

  package Password_Vector is new Ada.Containers.Vectors
    (Index_Type => Natural, Element_Type => Password_Entry);

  function load_passwords(filename : in String) return Password_Vector.Vector;

  procedure put(value : in Password_Vector.Vector);

  function count_valid(passwords : in Password_Vector.Vector) return Ada.Containers.Count_Type;
end Day;
