-- AOC 2020, Day 12
with Ada.Text_IO; use Ada.Text_IO;
with Day; use Day;

procedure main is
  f : constant Ferry := load_file("input.txt");
  part1 : constant Natural := distance(f);
  part2 : constant Natural := waypoint_distance(f);
begin
  put_line("Part 1: " & Natural'Image(part1));
  put_line("Part 2: " & Natural'Image(part2));
end main;
