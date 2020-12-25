-- AOC 2020, Day 25
with Ada.Text_IO; use Ada.Text_IO;
with Day; use Day;

procedure main is
  key_pk : constant Long_Integer := 6270530;
  door_pk : constant Long_Integer := 14540258;
  part1 : constant Long_Integer := encryption_key(key_pk, door_pk);
begin
  put_line("Part 1: " & part1'IMAGE);
end main;
