-- AOC 2020, Day 13
with Ada.Text_IO; use Ada.Text_IO;
with Day; use Day;

procedure main is
  s : constant Schedule := load_file("input.txt");
  part1 : constant Long_Long_Integer := bus_mult(s);
  part2 : constant Long_Long_Integer := earliest_matching(s);
begin
  put_line("Part 1: " & Long_Long_Integer'Image(part1));
  put_line("Part 2: " & Long_Long_Integer'Image(part2));
end main;
