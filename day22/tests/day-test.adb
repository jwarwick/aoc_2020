with AUnit.Assertions; use AUnit.Assertions;
with Ada.Text_IO;

package body Day.Test is

  procedure Test_Part1 (T : in out AUnit.Test_Cases.Test_Case'Class) is
    pragma Unreferenced (T);
    t1 : constant Natural := combat("test1.txt");
  begin
    Assert(t1 = 306, "Wrong number, expected 306, got" & t1'IMAGE);
  end Test_Part1;

  procedure Test_Part2 (T : in out AUnit.Test_Cases.Test_Case'Class) is
    pragma Unreferenced (T);
    t1 : constant Natural := recursive_combat("test1.txt");
    t2 : constant Natural := recursive_combat("test2.txt");
  begin
    Ada.Text_IO.put_line("Infinite game ended" & t2'IMAGE);
    Assert(t1 = 291, "Wrong number, expected 291, got" & t1'IMAGE);
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
