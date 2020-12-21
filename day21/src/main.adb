-- AOC 2020, Day 21
with Ada.Text_IO; use Ada.Text_IO;
with Day; use Day;

procedure main is
  part1 : constant Natural := ingredient_count("input.txt");
begin
  put_line("Part 1: " & part1'IMAGE);
end main;
