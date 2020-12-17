with AUnit.Assertions; use AUnit.Assertions;

package body Day.Test is

  procedure Test_Part1 (T : in out AUnit.Test_Cases.Test_Case'Class) is
    pragma Unreferenced (T);
    g : constant Grid_Set.Set := load_file("test1.txt");
    cycles : constant Natural := 6;
    t1 : constant Natural := active_count(g, cycles);
  begin
    Assert(t1 = 112, "Wrong number, expected 112, got" & Natural'IMAGE(t1));
  end Test_Part1;

  procedure Test_Part2 (T : in out AUnit.Test_Cases.Test_Case'Class) is
    pragma Unreferenced (T);
    g : constant Grid_Set.Set := load_file("test1.txt");
    cycles : constant Natural := 6;
    t2 : constant Natural := active_count_4d(g, cycles);
  begin
    Assert(t2 = 848, "Wrong number, expected 848, got" & Natural'IMAGE(t2));
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
