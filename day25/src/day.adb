-- AoC 2020, Day 25
with Ada.Text_IO;

package body Day is
  package TIO renames Ada.Text_IO;

  card_subject_number : constant Long_Integer := 7;
  door_subject_number : constant Long_Integer := 7;
  
  div_constant : constant Long_Integer := 20201227;

  procedure transform(value : in out Long_Integer; subject_num : in Long_Integer) is
  begin
    value := value * subject_num;
    value := value rem div_constant;
  end transform;

  function scan_loops(pk : in Long_Integer; subject_number : in Long_Integer) return Natural is
    loop_val : Natural := 1;
    val : Long_Integer := 1;
  begin
    loop
      transform(val, subject_number);
      if val = pk then
        exit;
      end if;
      loop_val := loop_val + 1;
    end loop;
    return loop_val;
  end scan_loops;

  function encryption_key(card_pk : in Long_Integer; door_pk : in Long_Integer) return Long_Integer is
    card_loop : Natural := 1;
    door_loop : Natural := 1;
    key : Long_Integer := 1;
  begin
    card_loop := scan_loops(card_pk, card_subject_number);
    TIO.put_line("Card loop:" & card_loop'IMAGE);
    door_loop := scan_loops(door_pk, door_subject_number);
    TIO.put_line("Door loop:" & door_loop'IMAGE);

    for s in 1..door_loop loop
      transform(key, card_pk);
    end loop;

    return key;
  end encryption_key;
end Day;
