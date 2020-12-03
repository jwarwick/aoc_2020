-- AOC 2020, Day 3
with Ada.Text_IO; use Ada.Text_IO;
with Day; use Day;

procedure main is
  f : constant Forest := load_map("input.txt");
  slope : constant Natural := 3;
begin
  put_line("Part 1: " & Natural'Image(trees_hit(f, slope)));
  put_line("Part 2: " & Natural'Image(many_trees_hit(f)));
end main;
