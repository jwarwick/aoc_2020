with AUnit.Assertions; use AUnit.Assertions;

package body Day.Test is

  procedure Test_Part1 (T : in out AUnit.Test_Cases.Test_Case'Class) is
    pragma Unreferenced (T);
    num : constant Natural := 2020;
    s0 : constant Input_Array := (0,3,6);
    t0 : constant Natural := sequence(s0, num);
    s1 : constant Input_Array := (1,3,2);
    t1 : constant Natural := sequence(s1, num);
  begin
    Assert(t0 = 436, "Wrong number, expected 436, got" & Natural'IMAGE(t0));
    Assert(t1 = 1, "Wrong number, expected 1, got" & Natural'IMAGE(t1));
  end Test_Part1;

  procedure Test_Part2 (T : in out AUnit.Test_Cases.Test_Case'Class) is
    pragma Unreferenced (T);
    num : constant Natural := 30000000;
    s0 : constant Input_Array := (0,3,6);
    t0 : constant Natural := sequence(s0, num);
    -- s1 : constant Input_Array := (1,3,2);
    -- t1 : constant Natural := sequence(s1, num);
  begin
    Assert(t0 = 175594, "Wrong number, expected 175594, got" & Natural'IMAGE(t0));
    -- Assert(t1 = 2578, "Wrong number, expected 2578, got" & Natural'IMAGE(t1));
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
