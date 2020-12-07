-- AOC 2020, Day 7
with Ada.Text_IO; use Ada.Text_IO;
with Day; use Day;

procedure main is
  bag_colors : constant Natural := valid_bag_colors;
  nested : constant Natural := nested_bags;
begin
  put_line("Part 1: " & Natural'Image(bag_colors));
  put_line("Part 2: " & Natural'Image(nested));
end main;
