-- AOC 2020, Day 12
with Ada.Text_IO; use Ada.Text_IO;
with Day; use Day;

procedure main is
  f : constant Ferry := load_file("input.txt");
  part1 : constant Natural := distance(f);
begin
  put_line("Part 1: " & Natural'Image(part1));
end main;
