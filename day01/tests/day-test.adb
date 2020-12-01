with AUnit.Assertions; use AUnit.Assertions;

package body Day.Test is

   procedure Test_Part1 (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
      expenses : constant Expense_Vector.Vector := load_file("test1.txt");
      product : constant Expense := matching_product(expenses);
   begin
     Assert(product = 514579, "Wrong product, expected 514579, got" & Expense'IMAGE(product));
   end Test_Part1;

   procedure Test_Part2 (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
      expenses : constant Expense_Vector.Vector := load_file("test1.txt");
      product : constant Expense := triple_matching_product(expenses);
   begin
     Assert(product = 241861950, "Wrong product, expected 241861950, got" & Expense'IMAGE(product));
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
