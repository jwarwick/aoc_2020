-- AOC 2020, Day 18
with Ada.Text_IO; use Ada.Text_IO;
with Day; use Day;

procedure main is
  part1 : constant Long_Integer := hw_sum("input.txt");
  part2 : constant Long_Integer := hw_newmath_sum("input.txt");
begin
  put_line("Part 1: " & Long_Integer'Image(part1));
  put_line("Part 2: " & Long_Integer'Image(part2));
end main;
