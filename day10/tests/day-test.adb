with AUnit.Assertions; use AUnit.Assertions;

package body Day.Test is

   procedure Test_Part1 (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
      v : constant Adaptors.Vector := load_file("test1.txt");
      t1 : constant Natural := mult_1_3_differences(v);
      v2 : constant Adaptors.Vector := load_file("test2.txt");
      t2 : constant Natural := mult_1_3_differences(v2);
   begin
     Assert(t1 = 35, "Wrong number, expected 35, got" & Natural'IMAGE(t1));
     Assert(t2 = 220, "Wrong number, expected 220, got" & Natural'IMAGE(t2));
   end Test_Part1;

   procedure Test_Part2 (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
   begin
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
