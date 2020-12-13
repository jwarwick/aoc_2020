with AUnit.Assertions; use AUnit.Assertions;

package body Day.Test is

  procedure Test_Part1 (T : in out AUnit.Test_Cases.Test_Case'Class) is
    pragma Unreferenced (T);
    s : constant Schedule := load_file("test1.txt");
    t1 : constant Long_Long_Integer := bus_mult(s);
  begin
    Assert(t1 = 295, "Wrong number, expected 295, got" & Long_Long_Integer'IMAGE(t1));
  end Test_Part1;

  procedure Test_Part2 (T : in out AUnit.Test_Cases.Test_Case'Class) is
    pragma Unreferenced (T);
    s : constant Schedule := load_file("test1.txt");
    t1 : constant Long_Long_Integer := earliest_matching(s);
    s2 : constant Schedule := load_file("test2.txt");
    t2 : constant Long_Long_Integer := earliest_matching(s2);
    s3 : constant Schedule := load_file("test3.txt");
    t3 : constant Long_Long_Integer := earliest_matching(s3);
  begin
    Assert(t1 = 1068781, "Wrong number, expected 1068781, got" & Long_Long_Integer'IMAGE(t1));
    Assert(t2 = 3417, "Wrong number, expected 3417, got" & Long_Long_Integer'IMAGE(t2));
    Assert(t3 = 1202161486, "Wrong number, expected 1202161486, got" & Long_Long_Integer'IMAGE(t3));
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
