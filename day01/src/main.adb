-- AOC 2020, Day 1
with Ada.Text_IO; use Ada.Text_IO;
with Day; use Day;

procedure main is
begin
  put_line("Part 1: " & Expense'Image(Day.part1("input.txt")));
  put_line("Part 2: " & Expense'Image(Day.part2("input.txt")));
end main;
