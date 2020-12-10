-- AoC 2020, Day 10
with Ada.Text_IO;

package body Day is
  package TIO renames Ada.Text_IO;

  function load_file(filename : in String) return Adaptors.Vector is
    package Natural_Text_IO is new Ada.Text_IO.Integer_IO(Natural);
    package Adaptor_Sorter is new Adaptors.Generic_Sorting;

    file : TIO.File_Type;
    v : Adaptors.Vector;
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

  function order_adaptors(v : in Adaptors.Vector; sln : in out Adaptors.Vector) return Boolean is
    start : constant Natural := sln.last_element;
    sln_copy : Adaptors.Vector := sln;
    v_copy : Adaptors.Vector;
  begin
    if is_empty(v) then
      return true;
    end if;

    for idx in v.first_index .. v.last_index loop
      if start + 3 >= v(idx) then
        v_copy := v;
        v_copy.delete(idx);
        sln_copy.append(v(idx));
        if order_adaptors(v_copy, sln_copy) then
          sln := sln_copy;
          return true;
        end if;
      else
        return false;
      end if;
    end loop;
    return false;
  end order_adaptors;

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
    sln : Adaptors.Vector := Empty_Vector;
    result : Boolean;
  begin
    sln.append(0);
    remaining.append(remaining.last_element + 3);
    result := order_adaptors(remaining, sln);
    if not result then
      TIO.put_line("Failed to find ordering");
      return 0;
    end if;

    return mult_diffs(sln);
  end mult_1_3_differences;
end Day;
