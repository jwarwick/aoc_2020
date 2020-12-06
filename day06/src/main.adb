-- AOC 2020, Day 6
with Ada.Text_IO; use Ada.Text_IO;
with Day; use Day;

procedure main is
  anyone : constant Natural := anyone_sum("input.txt");
  everyone : constant Natural := everyone_sum("input.txt");
begin
  put_line("Part 1: " & Natural'Image(anyone));
  put_line("Part 2: " & Natural'Image(everyone));
end main;
