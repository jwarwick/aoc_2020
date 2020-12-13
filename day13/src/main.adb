-- AOC 2020, Day 13
with Ada.Text_IO; use Ada.Text_IO;
with Day; use Day;

procedure main is
  s : constant Schedule := load_file("input.txt");
  part1 : constant Natural := bus_mult(s);
begin
  put_line("Part 1: " & Natural'Image(part1));
end main;
