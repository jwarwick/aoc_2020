-- AOC 2020, Day 19
with Ada.Text_IO; use Ada.Text_IO;
with Day; use Day;

procedure main is
  part1 : constant Natural := count_valid("input.txt");
  part2 : constant Natural := count_valid_updated("input.txt");
begin
  put_line("Part 1: " & part1'IMAGE);
  put_line("Part 2: " & part2'IMAGE);
end main;
