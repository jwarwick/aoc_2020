-- AOC 2020, Day 5
with Ada.Text_IO; use Ada.Text_IO;
with Day; use Day;

procedure main is
  use Boarding_Pass_Vectors;
  batch : constant Vector := load_batch("input.txt");
  highest : constant Natural := highest_id(batch);
begin
  put_line("Part 1: " & Natural'Image(highest));
end main;
