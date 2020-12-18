with AUnit.Assertions; use AUnit.Assertions;

package body Day.Test is

  procedure Test_Part1 (T : in out AUnit.Test_Cases.Test_Case'Class) is
    pragma Unreferenced (T);
    t1 : constant Long_Integer := eval_string("1 + 2 * 3 + 4 * 5 + 6");
    t2 : constant Long_Integer := eval_string("1 + (2 * 3) + (4 * (5 + 6))");
    t3 : constant Long_Integer := eval_string("2 * 3 + (4 * 5)");
    t4 : constant Long_Integer := eval_string("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2");
  begin
    Assert(t1 = 71, "Wrong number, expected 71, got" & t1'IMAGE);
    Assert(t2 = 51, "Wrong number, expected 51, got" & t2'IMAGE);
    Assert(t3 = 26, "Wrong number, expected 26, got" & t3'IMAGE);
    Assert(t4 = 13632, "Wrong number, expected 13632, got" & t4'IMAGE);
  end Test_Part1;

  procedure Test_Part2 (T : in out AUnit.Test_Cases.Test_Case'Class) is
    pragma Unreferenced (T);
    t1 : constant Long_Integer := eval_newmath_string("1 + 2 * 3 + 4 * 5 + 6");
    t2 : constant Long_Integer := eval_newmath_string("1 + (2 * 3) + (4 * (5 + 6))");
    t3 : constant Long_Integer := eval_newmath_string("2 * 3 + (4 * 5)");
    t5 : constant Long_Integer := eval_newmath_string("5 + (8 * 3 + 9 + 3 * 4 * 3)");
    t6 : constant Long_Integer := eval_newmath_string("5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))");
    t4 : constant Long_Integer := eval_newmath_string("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2");
  begin
    Assert(t1 = 231, "Wrong number, expected 231, got" & t1'IMAGE);
    Assert(t2 = 51, "Wrong number, expected 51, got" & t2'IMAGE);
    Assert(t3 = 46, "Wrong number, expected 46, got" & t3'IMAGE);
    Assert(t5 = 1445, "Wrong number, expected 1445, got" & t5'IMAGE);
    Assert(t6 = 669060, "Wrong number, expected 669060, got" & t6'IMAGE);
    Assert(t4 = 23340, "Wrong number, expected 23340, got" & t4'IMAGE);
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
