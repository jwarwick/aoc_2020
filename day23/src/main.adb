-- AOC 2020, Day 23
with Ada.Text_IO; use Ada.Text_IO;
with Day; use Day;

procedure main is
  cups : constant Cup_Array := (3,1,5,6,7,9,8,2,4);
  steps : constant Natural := 100;
  part1 : constant String := play(cups, steps);
  part2 : constant Long_Integer := play2(cups, 1000000, 10000000);
begin
  put_line("Part 1: " & part1);
  put_line("Part 2: " & part2'IMAGE);
end main;
