with AUnit.Assertions; use AUnit.Assertions;

package body Day.Test is

   procedure Test_Part1 (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
      orbits : constant Orbit_List.Vector := load_orbits("test1.txt");
      count : constant Orbit_Checksum := orbit_count_checksum(orbits);
      orbits_part1 : constant Orbit_List.Vector := load_orbits("input.txt");
      count_part1 : constant Orbit_Checksum := orbit_count_checksum(orbits_part1);
   begin
     Assert(count = 42, "Wrong orbit checksum, expected 42, got " & Orbit_Checksum'IMAGE(count));
     Assert(count_part1 = 295936, "Wrong orbit checksum, expected 295936, got " & Orbit_Checksum'IMAGE(count_part1));
   end Test_Part1;

   procedure Test_Part2 (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
      -- orbits : constant Orbit_List.Vector := load_orbits("input.txt");
   begin
     -- Assert(orbit_count_checksum(orbits) = 0, "Wrong orbit checksum");
     null;
   end Test_Part2;

   function Name (T : Test) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Test Day package");
   end Name;

   procedure Register_Tests (T : in out Test) is
     use AUnit.Test_Cases.Registration;
   begin
     Register_Routine (T, Test_Part1'Access, "Test Part 1");
     Register_Routine (T, Test_Part2'Access, "Test Part 2");
   end Register_Tests;

end Day.Test;
