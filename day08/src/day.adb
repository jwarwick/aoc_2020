-- AoC 2020, Day 8
with VM; use VM;
with Ada.Text_IO;
with Ada.Containers.Ordered_Sets;

package body Day is
  package TIO renames Ada.Text_IO;

  package PC_Sets is new Ada.Containers.Ordered_Sets (Element_Type => Instruction_Index);
  use PC_Sets;

  function acc_before_repeat(filename : in String) return Integer is
    v : VM.VM := load_file(filename);
    pc : Instruction_Index := 0;
    seen : PC_Sets.Set;
  begin
    loop
      if seen.contains(pc) then
        exit;
      end if;
      seen.insert(pc);
      pc := step(v);
      TIO.put_line("New Pc:" & Instruction_Index'Image(pc));
    end loop;
    return acc(v);
  end;

end Day;
