-- AOC 2020, Day 4
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers; use Ada.Containers;
with Day; use Day;

procedure main is
  batch : constant Passport_Vectors.Vector := load_batch("input.txt");
  cnt : constant Count_Type := valid_count(batch);
begin
  put_line("Part 1: " & Count_Type'Image(cnt));
end main;
