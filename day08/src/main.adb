-- AOC 2020, Day 8
with Ada.Text_IO; use Ada.Text_IO;
with Day; use Day;

procedure main is
  acc : constant Integer := acc_before_repeat("input.txt");
begin
  put_line("Part 1: " & Integer'Image(acc));
end main;
