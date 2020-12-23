-- AoC 2020, Day 23
with Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

package body Day is
  package TIO renames Ada.Text_IO;

  function to_cup_number(n : in Cup_Number_Mod) return Cup_Number is
    v : Natural := Natural(n+1);
  begin
    if v < Natural(Cup_Number'First) then
      v := Natural(Cup_Number'First);
    end if;
    return Cup_Number(v);
  end to_cup_number;

  function to_cup_number_mod(n : in Cup_Number) return Cup_Number_Mod is
    v : Natural := Natural(n) - 1;
  begin
    if v = 0 then
      v := Natural(Cup_Number_Mod'Last);
    end if;
    return Cup_Number_Mod(v);
  end to_cup_number_mod;

  function index_of(value : in Cup_Number; cups : in Cup_Array) return Cup_Index is
  begin
    for i in cups'Range loop
      if cups(i) = value then
        return i;
      end if;
    end loop;
    return cups'first;
  end index_of;

  function play(c : in Cup_Array; steps : in Natural) return String is
    cups : Cup_Array := c;
    next_cups : Cup_Array;
    current_idx : Cup_Index_Mod := Cup_Index_Mod(cups'first);
    dest : Cup_Number_Mod;
    dest_idx : Cup_Index;
  begin
    for s in 1..steps loop
      -- TIO.put_line("Step" & s'IMAGE);
      -- for c of cups loop
      --   TIO.put(c'IMAGE & " ");
      -- end loop;
      -- TIO.new_line;
      dest := to_cup_number_mod(cups(Cup_Index(current_idx))) - 1;
      loop
        dest_idx := index_of(to_cup_number(dest), cups);
        declare
          dest_idx_mod : constant Cup_Index_Mod := Cup_Index_Mod(dest_idx);
        begin
          if dest_idx_mod /= current_idx+1 and dest_idx_mod /= current_idx+2 and dest_idx_mod /= current_idx+3 then
            exit;
          end if;
          dest := dest-1;
        end;
      end loop;

      declare
        idx : Cup_Index_Mod := current_idx + 1;
      begin
        next_cups := cups;
        -- shift by three until we hit dest
        loop
          next_cups(Cup_Index(idx)) := cups(Cup_Index(idx+3));
          if next_cups(Cup_Index(idx)) = to_cup_number(dest) then
            exit;
          end if;
          idx := idx + 1;
        end loop;

        -- add in the three removed items
        next_cups(Cup_Index(idx+1)) := cups(Cup_Index(current_idx+1));
        next_cups(Cup_Index(idx+2)) := cups(Cup_Index(current_idx+2));
        next_cups(Cup_Index(idx+3)) := cups(Cup_Index(current_idx+3));
      end;

      current_idx := current_idx + 1;
      cups := next_cups;
    end loop;

    declare
      s : Unbounded_String := Null_Unbounded_String;
      one_idx : constant Cup_Index := index_of(1, cups);
      idx : Cup_Index_Mod := Cup_Index_Mod(one_idx) + 1;
    begin
      loop
        if Cup_Index(idx) = one_idx then
          TIO.put_line(to_string(s));
          return to_string(s);
        end if;
        s := s & trim(cups(Cup_Index(idx))'IMAGE, Ada.Strings.Left);
        idx := idx + 1;
      end loop;
    end;
  end play;
end Day;
