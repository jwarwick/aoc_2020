-- AOC 2020, Day 11
with Ada.Text_IO; use Ada.Text_IO;
with Day; use Day;

procedure main is
  m : constant Ferry := load_file("input.txt");
  part1 : constant Natural := steady_state_occupied(m);
begin
  put_line("Part 1: " & Natural'Image(part1));
end main;
