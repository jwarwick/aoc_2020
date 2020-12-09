-- AOC 2020, Day 1
with Ada.Text_IO; use Ada.Text_IO;
with Day; use Day;

procedure main is
  v : constant XMAS_Vector.Vector := load_file("input.txt");
  invalid : constant Long_Integer := first_invalid(v, 25);
  contig : constant Long_Integer := find_sum(v, invalid);
begin
  put_line("Part 1: " & Long_Integer'Image(invalid));
  put_line("Part 2: " & Long_Integer'Image(contig));
end main;
