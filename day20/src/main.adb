-- AOC 2020, Day 20
with Ada.Text_IO; use Ada.Text_IO;
with Day; use Day;

procedure main is
  part1 : constant Long_Integer := image_checksum("input.txt");
  part2 : constant Long_Integer := water_roughness;
begin
  put_line("Part 1: " & part1'IMAGE);
  put_line("Part 2: " & part2'IMAGE);
end main;
