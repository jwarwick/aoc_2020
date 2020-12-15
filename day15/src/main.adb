-- AOC 2020, Day 15
with Ada.Text_IO; use Ada.Text_IO;
with Day; use Day;

procedure main is
  s : constant Input_Array := (0, 8, 15, 2, 12, 1, 4);
  num : constant Natural := 2020;
  part1 : constant Natural := sequence(s, num);
begin
  put_line("Part 1: " & Natural'Image(part1));
end main;
