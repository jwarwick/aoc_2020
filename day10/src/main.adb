-- AOC 2020, Day 10
with Ada.Text_IO; use Ada.Text_IO;
with Day; use Day;

procedure main is
  v : constant Adaptors.Vector := load_file("input.txt");
  part1 : constant Natural := mult_1_3_differences(v);
begin
  put_line("Part 1: " & Natural'Image(part1));
end main;
