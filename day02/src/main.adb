-- AOC 2020, Day 2
with Ada.Text_IO; use Ada.Text_IO;
with Day; use Day;
with Ada.Containers; use Ada.Containers;

procedure main is
  passwords : constant Password_Vector.Vector := load_passwords("input.txt");
  count : constant Count_Type := count_valid(passwords);
  positions : constant Count_Type := count_valid_positions(passwords);
begin
  put_line("Part 1: " & Count_Type'Image(count));
  put_line("Part 2: " & Count_Type'Image(positions));
end main;
