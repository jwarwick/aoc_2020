with AUnit.Assertions; use AUnit.Assertions;

package body Day.Test is

   procedure Test_Part1 (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
      sum : constant Natural := anyone_sum("test1.txt");
   begin
     Assert(sum = 11, "Wrong group sum, expected 11, got " & Natural'IMAGE(sum));
   end Test_Part1;

   procedure Test_Part2 (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
      sum : constant Natural := everyone_sum("test1.txt");
   begin
     Assert(sum = 6, "Wrong group sum, expected 6, got " & Natural'IMAGE(sum));
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
