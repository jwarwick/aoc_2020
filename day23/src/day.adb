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


  function play2(c : in Cup_Array; total_cups : in Natural; steps : in Natural) return Long_Integer is
    type Cup;
    type Cup_Access is access Cup;

    type Cup is record
      Value : Natural;
      Next : Cup_Access;
    end record;

    type Cup_Lookup_Array is array(1..total_cups) of Cup_Access;

    lookup : Cup_Lookup_Array;
    curr_cup : Cup_Access := null;
  begin
    declare
      new_cup : Cup_Access;
      last : Cup_Access := null;
      cnt : Natural := 1;
    begin
      for v of c loop
        new_cup := new Cup'(Value=>Natural(v), Next=>null);
        if curr_cup = null then
          curr_cup := new_cup;
        end if;
        lookup(new_cup.Value) := new_cup;

        if last /= null then
          last.Next := new_cup;
        end if;

        last := new_cup;
        cnt := cnt + 1;
      end loop;

      for v in cnt..total_cups loop
        new_cup := new Cup'(Value=>Natural(v), Next=>null);
        lookup(new_cup.Value) := new_cup;
        last.Next := new_cup;
        last := new_cup;
      end loop;

      last.next := curr_cup;
    end;

    -- declare
    --   tmp_cup : Cup_Access := curr_cup;
    --   cnt : Natural := 0;
    -- begin
    --   TIO.put_line("cups:");
    --   loop
    --     TIO.put(tmp_cup.value'IMAGE & ", ");
    --     tmp_cup := tmp_cup.next;

    --     if cnt >= 20 then
    --       exit;
    --     end if;
    --     cnt := cnt + 1;
    --   end loop;
    -- end;

    for s in 1..steps loop
      declare
        type Move_Array is array(1..3) of Natural;
        target : Natural := curr_cup.value;
        target_ptr : Cup_Access := null;
        move_ptr : Cup_Access := null;
        move_vals : Move_Array;
      begin
        move_ptr := curr_cup.next;

        move_vals(1) := curr_cup.next.value;
        move_vals(2) := curr_cup.next.next.value;
        move_vals(3) := curr_cup.next.next.next.value;

        curr_cup.next := curr_cup.next.next.next.next;

        loop
          if target = 1 then
            target := total_cups;
          else
            target := target - 1;
          end if;

          if target /= move_vals(1) and target /= move_vals(2) and target /= move_vals(3) then
            exit;
          end if;
        end loop;
        target_ptr := lookup(target);

        move_ptr.next.next.next := target_ptr.next;
        target_ptr.next := move_ptr;

        curr_cup := curr_cup.next;
      end;
    end loop;

    declare
      i : constant Cup_Access := lookup(1);
      n1 : constant Long_Integer := Long_Integer(i.next.value);
      n2 : constant Long_Integer := Long_Integer(i.next.next.value);
    begin
      return n1 * n2;
    end;
  end play2;
end Day;
