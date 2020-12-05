with AUnit.Assertions; use AUnit.Assertions;
with Ada.Containers; use Ada.Containers;

package body Day.Test is
  use Boarding_Pass_Vectors;

   procedure Test_Part1 (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
      batch : constant Vector := load_batch("test1.txt");
      len : constant Count_Type := length(batch);
      highest : constant Natural := highest_id(batch);
   begin
     Assert(len = 4, "Wrong number of boarding passes, expected 4, got " & Count_Type'IMAGE(len));
     Assert(highest = 820, "Wrong highest id, expected 820, got " & Natural'IMAGE(highest));
   end Test_Part1;

   procedure Test_Part2 (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
      batch : constant Vector := load_batch("test1.txt");
      len : constant Count_Type := length(batch);
   begin
     Assert(len = 4, "Wrong number of boarding passes, expected 4, got " & Count_Type'IMAGE(len));
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
