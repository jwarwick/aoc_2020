-- AoC 2020, Day 6
with Ada.Text_IO;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Ordered_Sets;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

package body Day is
  package TIO renames Ada.Text_IO;

  package Char_Sets is new Ada.Containers.Ordered_Sets
     (Element_Type => Character);
  use Char_Sets;

  function all_set return Char_Sets.Set is
    a : Set;
    alpha : constant String := "abcdefghijklmnopqrstuvwxyz";
  begin
    for c of alpha loop
      a.insert(c);
    end loop;
    return a;
  end all_set;

  function anyone_sum(filename : in String) return Natural is
    file : TIO.File_Type;
    sum : Natural := 0;
  begin
    TIO.open(File => file, Mode => TIO.In_File, Name => filename);
    while not TIO.end_of_file(file) loop
      declare
          group : Set;
      begin
        group_loop:
        loop
          declare
            line : constant String := TIO.get_line(file);
          begin
            if index_non_blank(line) = 0 then
              sum := sum + Natural(length(group));
              exit group_loop;
            else
              for c of line loop
                group.include(c);
              end loop;
              if TIO.end_of_file(file) then
                sum := sum + Natural(length(group));
                exit group_loop;
              end if;
            end if;
          end;
        end loop group_loop;
      end;
    end loop;
    TIO.close(file);
    return sum;
  end anyone_sum;

  function everyone_sum(filename : in String) return Natural is
    file : TIO.File_Type;
    sum : Natural := 0;
  begin
    TIO.open(File => file, Mode => TIO.In_File, Name => filename);
    while not TIO.end_of_file(file) loop
      declare
          all_group : Set := all_set;
      begin
        group_loop:
        loop
          declare
            line : constant String := TIO.get_line(file);
          begin
            if index_non_blank(line) = 0 then
              sum := sum + Natural(length(all_group));
              exit group_loop;
            else
              declare
                person : Set;
              begin
                for c of line loop
                  person.include(c);
                end loop;
                all_group := all_group and person;
              end;
              if TIO.end_of_file(file) then
                sum := sum + Natural(length(all_group));
                exit group_loop;
              end if;
            end if;
          end;
        end loop group_loop;
      end;
    end loop;
    TIO.close(file);
    return sum;
  end everyone_sum;

end Day;
