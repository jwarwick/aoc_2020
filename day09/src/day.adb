-- AoC 2020, Day 9
with Ada.Text_IO;
with Ada.Containers.Ordered_Sets;

package body Day is
  package TIO renames Ada.Text_IO;

  function load_file(filename : in String) return XMAS_Vector.Vector is
    file : TIO.File_Type;
    v : XMAS_Vector.Vector;
  begin
    TIO.open(File => file, Mode => TIO.In_File, Name => filename);
    while not TIO.end_of_file(file) loop
      v.append(Long_Integer'Value(TIO.get_line(file)));
    end loop;
    TIO.close(file);
    return v;
  end load_file;

  package Sum_Sets is new Ada.Containers.Ordered_Sets (Element_Type => Long_Integer);
  use Sum_Sets;

  package Sum_Vectors is new Ada.Containers.Vectors (Index_Type => Natural, Element_Type => Sum_Sets.Set);
  use Sum_Vectors;

  function initialize_sums(v : in XMAS_Vector.Vector; preamble : in Positive) return Sum_Vectors.Vector is
    sums : Sum_Vectors.Vector := Sum_Vectors.Empty_Vector;
    last_idx : constant Natural := preamble-1;
  begin
    for i in 0..preamble-1 loop
      declare
        set : Sum_Sets.Set := Empty_Set;
        final : constant Natural :=  Natural'Min(i+preamble, last_idx);
      begin
        for j in i+1..final loop
          set.include(v(i) + v(j));
        end loop;
        sums.append(set);
      end;
    end loop;
    return sums;
  end initialize_sums;

  procedure update_sums(sums : in out Sum_Vectors.Vector; v : in XMAS_Vector.Vector; index : in Positive) is
    start_index : constant Natural := index - (Natural(length(sums)) - 1);
    new_value : constant Long_Integer := v(index);
    new_set : Sum_Sets.Set := Empty_Set;
  begin
    sums.delete(0);
    for i in 0..Natural(length(sums))-1 loop
      declare
        curr_set : Sum_Sets.Set := sums(i);
      begin
        curr_set.include(v(start_index + i) + new_value);
        sums(i) := curr_set;
      end;
    end loop;
    new_set.include(new_value + v(index-1));
    sums.append(new_set);
  end update_sums;

  function contains(sums: Sum_Vectors.Vector; target : in Long_Integer) return Boolean is
  begin
    for set of sums loop
      if contains(set, target) then
        return true;
      end if;
    end loop;
    return false;
  end contains;

  function first_invalid(v : in XMAS_Vector.Vector; preamble : in Positive) return Long_Integer is
    sums : Sum_Vectors.Vector := initialize_sums(v, preamble);
  begin
    for idx in preamble..Natural(length(v))-1 loop
      if not contains(sums, v(idx)) then
        return v(idx);
      end if;
      update_sums(sums, v, idx);
    end loop;
    return 0;
  end first_invalid;
end Day;
