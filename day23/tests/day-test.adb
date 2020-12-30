with AUnit.Assertions; use AUnit.Assertions;

package body Day.Test is

  procedure Test_Part1 (T : in out AUnit.Test_Cases.Test_Case'Class) is
    pragma Unreferenced (T);
    cups : constant Cup_Array := (3,8,9,1,2,5,4,6,7);
    t1 : constant String := play(cups, 10);
    t2 : constant Long_Integer := play2(cups, 9, 10);
  begin
    Assert(t1 = "92658374", "Wrong number, expected 92658374, got" & t1);
    Assert(t2 = 18, "Wrong number, expected 18, got" & t2'IMAGE);
  end Test_Part1;

  procedure Test_Part2 (T : in out AUnit.Test_Cases.Test_Case'Class) is
    pragma Unreferenced (T);
    cups : constant Cup_Array := (3,8,9,1,2,5,4,6,7);
    t2 : constant Long_Integer := play2(cups, 1000000, 10000000);
  begin
    Assert(t2 = 149245887792, "Wrong number, expected 149245887792, got" & t2'IMAGE);
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
