with AUnit.Assertions; use AUnit.Assertions;

package body Day.Test is

  procedure Test_Part1 (T : in out AUnit.Test_Cases.Test_Case'Class) is
    pragma Unreferenced (T);
    t1 : constant Natural := count_valid("test1.txt");
    t2 : constant Natural := count_valid("test2.txt");
  begin
    Assert(t1 = 2, "Wrong number, expected 2, got" & t1'IMAGE);
    Assert(t2 = 3, "Wrong number, expected 3, got" & t2'IMAGE);
  end Test_Part1;

  procedure Test_Part2 (T : in out AUnit.Test_Cases.Test_Case'Class) is
    pragma Unreferenced (T);
    t2 : constant Natural := count_valid_updated("test2.txt");
  begin
    Assert(t2 = 12, "Wrong number, expected 12, got" & t2'IMAGE);
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
