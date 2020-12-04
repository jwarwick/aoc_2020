with AUnit.Assertions; use AUnit.Assertions;

package body Day.Test is

   procedure Test_Part1 (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
      batch : constant Passport_Vectors.Vector := load_batch("test1.txt");
      len : constant Count_Type := batch.length;
      cnt : constant Count_Type := present_count(batch);
   begin
     Assert(len = 4, "Wrong number of passports, expected 4, got " & Count_Type'IMAGE(len));
     Assert(cnt = 2, "Wrong number of valid passports, expected 2, got " & Count_Type'IMAGE(cnt));
   end Test_Part1;

   procedure Test_Part2 (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
      valid_batch : constant Passport_Vectors.Vector := load_batch("valid.txt");
      valid_cnt : constant Count_Type := valid_count(valid_batch);
      invalid_batch : constant Passport_Vectors.Vector := load_batch("invalid.txt");
      invalid_cnt : constant Count_Type := valid_count(invalid_batch);
   begin
     Assert(invalid_cnt = 0, "Wrong number of invalid passports, expected 0, got " & Count_Type'IMAGE(invalid_cnt));
     Assert(valid_cnt = 4, "Wrong number of valid passports, expected 4, got " & Count_Type'IMAGE(valid_cnt));
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
