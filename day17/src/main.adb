-- AOC 2020, Day 17
with Ada.Text_IO; use Ada.Text_IO;
with Day; use Day;

procedure main is
  g : constant Grid_Set.Set := load_file("input.txt");
  cycles : constant Natural := 6;
  part1 : constant Natural := active_count(g, cycles);
  part2 : constant Natural := active_count_4d(g, cycles);
begin
  put_line("Part 1: " & Natural'Image(part1));
  put_line("Part 2: " & Natural'Image(part2));
end main;
