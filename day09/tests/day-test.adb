with AUnit.Assertions; use AUnit.Assertions;

package body Day.Test is

   procedure Test_Part1 (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
      v : constant XMAS_Vector.Vector := load_file("test1.txt");
      invalid : constant Long_Integer := first_invalid(v, 5);
   begin
     Assert(invalid = 127, "Wrong number, expected 127, got" & Long_Integer'IMAGE(invalid));
   end Test_Part1;

   procedure Test_Part2 (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
      v : constant XMAS_Vector.Vector := load_file("test1.txt");
      contig : constant Long_Integer := find_sum(v, 127);
   begin
     Assert(contig = 62, "Wrong number, expected 62, got" & Long_Integer'IMAGE(contig));
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
