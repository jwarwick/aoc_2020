with AUnit.Assertions; use AUnit.Assertions;

package body Day.Test is

   procedure Test_Part1 (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
      batch : constant Passport_Vectors.Vector := load_batch("test1.txt");
      len : constant Count_Type := batch.length;
      cnt : constant Count_Type := valid_count(batch);
   begin
     Assert(len = 4, "Wrong number of passports, expected 4, got " & Count_Type'IMAGE(len));
     Assert(cnt = 2, "Wrong number of valid passports, expected 2, got " & Count_Type'IMAGE(cnt));
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
