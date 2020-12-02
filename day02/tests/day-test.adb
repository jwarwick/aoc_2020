with AUnit.Assertions; use AUnit.Assertions;
with Ada.Containers; use Ada.Containers;

package body Day.Test is

   procedure Test_Part1 (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
      passwords : constant Password_Vector.Vector := load_passwords("test1.txt");
      count : constant Count_Type := count_valid(passwords);
   begin
     Assert(count = 2, "Wrong valid password count, expected 2, got " & Count_Type'IMAGE(count));
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
