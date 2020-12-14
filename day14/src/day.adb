-- AoC 2020, Day 14
with Ada.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

package body Day is
  package TIO renames Ada.Text_IO;

  function parse_mask(line : in String) return Instruction is
    start : constant Natural := index(line, "=")+2;
    cnt : Integer := 35;
    map : Mask_Ordered_Maps.Map := Mask_Ordered_Maps.Empty_Map;
  begin
    for c of line(start..line'last) loop
      if c = '1' then
        map.insert(cnt, 1);
      elsif c = '0' then
        map.insert(cnt, 0);
      end if;
      cnt := cnt-1;
    end loop;
    return Instruction'(kind => mask, mask_map => map);
  end parse_mask;

  function parse_mem(line : in String) return Instruction is
    right_bracket : constant Natural := index(line, "]") - 1;
    mem_idx : constant String := line(line'first+4 .. right_bracket);
    offset : constant Natural := Natural'Value(mem_idx);
    equal : constant Natural := index(line, "=");
    value_str : constant String := line(equal+2..line'last);
    value : constant Long_Integer := Long_Integer'Value(value_str);
  begin
    return Instruction'(kind => mem, offset => offset, value => value);
  end parse_mem;

  function load_file(filename : in String) return Program is
    file : TIO.File_Type;
    instructions : Instruction_Vectors.Vector := Empty_Vector;
  begin
    TIO.open(File => file, Mode => TIO.In_File, Name => filename);
    while not TIO.end_of_file(file) loop
      declare
        line : constant String := TIO.get_line(file);
      begin
        -- mask or mem
        if line(2) = 'a' then
          instructions.append(parse_mask(line));
        else
          instructions.append(parse_mem(line));
        end if;
      end;
    end loop;
    TIO.close(file);
    return Program'(instructions => instructions, memory => Memory_Ordered_Maps.Empty_Map, curr_mask => Mask_Ordered_Maps.Empty_Map);
  end load_file;

  function sum_memory(mem : in Memory_Ordered_Maps.Map) return Long_Integer is
    cnt : Long_Integer := 0;
  begin
    for c in mem.Iterate loop
      cnt := cnt + element(c);
    end loop;
    return cnt;
  end sum_memory;

  function mask_number(v : in Long_Integer; m : Mask_Ordered_Maps.Map) return Long_Integer is
    package Binary_Vectors is new Ada.Containers.Vectors
      (Index_Type   => Natural,
      Element_Type => Long_Integer);
    use Binary_Vectors;

    result : Long_Integer := 0;
    curr : Long_Integer := v;
    cnt : Natural := 0;
    index : Integer := 0;
    vec : Binary_Vectors.Vector := Binary_Vectors.Empty_Vector;
  begin
    while curr > 0 loop
      if curr mod 2 /= 0 then
        curr := curr-1;
        vec.append(1);
      else
        vec.append(0);
      end if;
      cnt := cnt + 1;
      curr := curr / 2;
    end loop;

    index := 35;
    while index >= 0 loop
      curr := 0;
      if index < cnt then
        curr := vec(index);
      end if;
      if m.contains(index) then
        curr := m(index);
      end if;

      result := (result * 2) + curr;
      index := index - 1;
    end loop;
    return result;
  end mask_number;

  procedure step(p : in out Program; pc : in Natural) is
    i : constant Instruction := p.instructions(pc);
  begin
    case i.Kind is
      when mask =>
        p.curr_mask := i.mask_map;
      when mem =>
        p.memory.include(i.offset, mask_number(i.value, p.curr_mask));
    end case;
  end step;

  function sum_memory(p : in Program) return Long_Integer is
    p2 : Program := p;
    idx : Natural := 0;
  begin
    for e of p2.instructions loop
      step(p2, idx);
      idx := idx + 1;
    end loop;
    return sum_memory(p2.memory);
  end sum_memory;

end Day;
