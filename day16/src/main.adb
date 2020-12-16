-- AOC 2020, Day 16
with Ada.Text_IO; use Ada.Text_IO;
with Day; use Day;

procedure main is
  input : constant Tickets := load_file("input.txt");
  error_rate : constant Natural := sum_error_rate(input);
begin
  put_line("Part 1: " & error_rate'IMAGE);
end main;
