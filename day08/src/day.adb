-- AoC 2020, Day 8
with VM; use VM;
with Ada.Text_IO;
with Ada.Containers.Ordered_Sets;
with Ada.Containers; use Ada.Containers;

package body Day is
  package TIO renames Ada.Text_IO;

  package PC_Sets is new Ada.Containers.Ordered_Sets (Element_Type => Instruction_Index);
  use PC_Sets;

  function acc_before_repeat(filename : in String) return Integer is
    v : VM.VM := load_file(filename);
    i : Instruction_Index := pc(v);
    seen : PC_Sets.Set;
    halt : Boolean;
  begin
    loop
      if seen.contains(i) then
        exit;
      end if;
      seen.insert(i);
      halt := step(v);
      i := pc(v);
    end loop;
    TIO.put_line("Halted?: " & Boolean'Image(halt));
    return acc(v);
  end acc_before_repeat;

  function acc_after_terminate(filename : in String) return Integer is
    v : VM.VM := load_file(filename);
    max : constant Count_Type := instructions(v);
  begin
    for i in 0..max-1 loop
      swap_nop_jmp(Instruction_Index(i), v);
      -- backed this value down to find a reasonable cutoff
      if eval(v, 250) then
        return acc(v);
      end if;
      swap_nop_jmp(Instruction_Index(i), v);
      reset(v);
    end loop;
    return -1;
  end acc_after_terminate;
end Day;
