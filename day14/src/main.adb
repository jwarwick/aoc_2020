-- AOC 2020, Day 14
with Ada.Text_IO; use Ada.Text_IO;
with Day; use Day;

procedure main is
  p : constant Program := load_file("input.txt");
  part1 : constant Long_Integer := sum_memory(p);
begin
  put_line("Part 1: " & Long_Integer'Image(part1));
end main;
