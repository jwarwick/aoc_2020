with AUnit.Assertions; use AUnit.Assertions;

package body Day.Test is

   procedure Test_Part1 (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
      input : constant Tickets := load_file("test1.txt");
      t1 : constant Natural := sum_error_rate(input);
   begin
     Assert(t1 = 71, "Wrong value, expected 71, got " & t1'IMAGE);
   end Test_Part1;

   procedure Test_Part2 (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
      input : constant Tickets := load_file("test2.txt");
      depart : constant Long_Integer := departure_fields(input);
   begin
     Assert(depart = 11 * 12 * 13, "Wrong value, expected 11 * 12 * 13, got " & depart'IMAGE);
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
