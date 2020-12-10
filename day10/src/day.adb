-- AoC 2020, Day 10
with Ada.Text_IO;
with Ada.Containers.Ordered_Maps;

package body Day is
  package TIO renames Ada.Text_IO;

  function load_file(filename : in String) return Adaptors.Vector is
    package Natural_Text_IO is new Ada.Text_IO.Integer_IO(Natural);
    package Adaptor_Sorter is new Adaptors.Generic_Sorting;

    file : TIO.File_Type;
    v : Adaptors.Vector := Empty_Vector;
    n : Natural := 0;
  begin
    TIO.open(File => file, Mode => TIO.In_File, Name => filename);
    while not TIO.end_of_file(file) loop
      Natural_Text_IO.get(file, n);
      v.append(n);
    end loop;
    TIO.close(file);
    Adaptor_Sorter.sort(Container => v);
    return v;
  end load_file;

  function mult_diffs(v : in Adaptors.Vector) return Natural is
    diff_1 : Natural := 0;
    diff_3 : Natural := 0;
    curr : Natural;
    last : Natural := v.first_element;
  begin
    for idx in v.first_index+1 .. v.last_index loop
      curr := v(idx);
      case curr - last is
        when 3 => diff_3 := diff_3 + 1;
        when 1 => diff_1 := diff_1 + 1;
        when others => null;
      end case;
      last := curr;
    end loop;
    return diff_1 * diff_3;
  end mult_diffs;

  function mult_1_3_differences(v : in Adaptors.Vector) return Natural is
    remaining : Adaptors.Vector := v;
  begin
    remaining.prepend(0);
    remaining.append(remaining.last_element + 3);
    return mult_diffs(remaining);
  end mult_1_3_differences;

  package Reachable_Map is new Ada.Containers.Ordered_Maps
    (Element_Type => Long_Integer,
    Key_Type => Natural);
  use Reachable_Map;

  reach_map : Reachable_Map.Map := Empty_Map;

  function count(curr_idx : in Natural; v : in Adaptors.Vector) return Long_Integer is
    curr_value : constant Natural := v(curr_idx);
    total : Long_Integer := 0;
    last_idx : constant Natural := Natural'Min(curr_idx+3, v.last_index);
  begin
    if contains(reach_map, curr_value) then
      return reach_map(curr_value);
    end if;

    for next_idx in curr_idx+1..last_idx loop
      if v(next_idx) + 3 >= curr_value then
        total := total + count(next_idx, v);
      end if;
    end loop;

    reach_map.insert(curr_value, total);
    return total;
  end count;

  function total_arrangments(v : in Adaptors.Vector) return Long_Integer is
    v_first_last : Adaptors.Vector := v;
  begin
    v_first_last.prepend(0);
    v_first_last.append(v_first_last.last_element + 3);
    reverse_elements(v_first_last);
    clear(reach_map);
    reach_map.insert(0, 1);
    return count(v_first_last.first_index, v_first_last);
  end total_arrangments;

end Day;
