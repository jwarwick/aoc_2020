with AUnit.Assertions; use AUnit.Assertions;

package body Day.Test is

  procedure Test_Part1 (T : in out AUnit.Test_Cases.Test_Case'Class) is
    pragma Unreferenced (T);
    p : constant Program := load_file("test1.txt");
    t1 : constant Long_Integer := sum_memory(p);
  begin
    Assert(t1 = 165, "Wrong number, expected 165, got" & Long_Integer'IMAGE(t1));
  end Test_Part1;

  procedure Test_Part2 (T : in out AUnit.Test_Cases.Test_Case'Class) is
    pragma Unreferenced (T);
    p : constant Program := load_file("test2.txt");
    t1 : constant Long_Integer := sum_memory_v2(p);
  begin
    Assert(t1 = 208, "Wrong number, expected 208, got" & Long_Integer'IMAGE(t1));
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
