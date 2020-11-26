-- AoC 2020, Day X
with Ada.Text_IO;
-- with Ada.Containers.Indefinite_Hashed_Maps;
-- with Ada.Containers.Ordered_Sets;
-- with Ada.Strings.Hash;
-- with Ada.Strings.Fixed; use Ada.Strings.Fixed;
-- with GNAT.String_Split;
-- with Ada.Characters.Latin_1;

package body Day is
  package TIO renames Ada.Text_IO;

  -- package Orbit_Element_Set is new Ada.Containers.Ordered_Sets
  --   (Element_Type => Unbounded_String);

  -- type Orbit is record
  --   Name : Unbounded_String := Null_Unbounded_String;
  --   Parent : Unbounded_String := Null_Unbounded_String;
  --   Children : Orbit_Element_Set.Set := Orbit_Element_Set.Empty_Set;
  --   Depth : Natural := 0;
  -- end record;

  -- pragma Warnings (Off, "procedure ""Put"" is not referenced");
  -- procedure Put(value : in Orbit) is
  -- pragma Warnings (On, "procedure ""Put"" is not referenced");
  -- begin
  --   TIO.Put("Orbit: " & to_string(value.Name) & ", Parent: " & to_string(value.Parent) & ", Depth: " & Natural'IMAGE(value.Depth));
  --   TIO.New_Line;
  --   TIO.Put("   Children: ");
  --   for c of value.Children loop
  --     TIO.Put(to_string(c) & " ");
  --   end loop;
  -- end Put;

  -- package Orbit_Hashed_Maps is new Ada.Containers.Indefinite_Hashed_Maps
  --    (Key_Type        => String,
  --     Element_Type    => Orbit,
  --     Hash            => Ada.Strings.Hash,
  --     Equivalent_Keys => "=");

  -- procedure parse_line(line : in String; orbits : in out Orbit_List.Vector) is
  --   idx : constant Natural := index(line, ")");
  --   left : constant String := line(line'first .. idx-1);
  --   right : constant String := line(idx+1 .. line'last);
  --   curr : constant Orbit_Entry := Orbit_Entry'(left => to_unbounded_string(left), right => to_unbounded_string(right));
  -- begin
  --   orbits.append(curr);
  -- end parse_line;

  -- function load_orbits(filename : in String) return Orbit_List.Vector is
  --   file : TIO.File_Type;
  --   orbits : Orbit_List.Vector;
  -- begin
  --   TIO.open(File => file, Mode => TIO.In_File, Name => filename);
  --   while not TIO.end_of_file(file) loop
  --     parse_line(TIO.get_line(file), orbits);
  --   end loop;
  --   TIO.close(file);
  --   return orbits;
  -- end load_orbits;

end Day;
