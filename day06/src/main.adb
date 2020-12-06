-- AOC 2020, Day 6
with Ada.Text_IO; use Ada.Text_IO;
with Day; use Day;

procedure main is
  sum : constant Natural := group_sum("input.txt");
begin
  put_line("Part 1: " & Natural'Image(sum));
end main;
