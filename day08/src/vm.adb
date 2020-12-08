with Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

package body VM is
  package TIO renames Ada.Text_IO;

  function instruction_index_hash(key : in Instruction_Index) return Hash_Type is
  begin
    return Hash_Type(key);
  end instruction_index_hash;

  procedure print(inst : in Op_Record) is
  begin
    TIO.put_line("     " & Instruction_Index'Image(inst.Index) & "  " & Op'Image(inst.Instruction) & " " & Integer'Image(inst.Arg));
  end print;

  procedure print(v : in VM) is
  begin
    TIO.put_line("PC: " & Instruction_Index'Image(v.PC) & ", Acc: " & Integer'Image(v.Acc));
    for i in v.Instructions.Iterate loop
      print(Element(i));
    end loop;
  end print;

  function parse_file(filename : in String) return Op_Hashed_Maps.Map is
    file : TIO.File_Type;
    map : Op_Hashed_Maps.Map := Empty_Map;
    n : Instruction_Index := 0;
  begin
    TIO.open(File => file, Mode => TIO.In_File, Name => filename);
    while not TIO.end_of_file(file) loop
      declare
        line : constant String := TIO.get_line(file);
        first_space : constant Natural := index(line, " ");
        cmd : constant String := line(line'first .. first_space-1);
        opr : constant Op := Op'Value(cmd);
        num : constant Integer := Integer'Value(line(first_space+1 .. line'last));
        rec : Op_Record(Ins => opr);
      begin
        rec.Arg := num;
        rec.Index := n;
        map.insert(n, rec);
        n := n+1;
      end;
    end loop;
    return map;
  end parse_file;

  function step(v : in out VM) return Instruction_Index is
    curr : constant Op_Record := v.Instructions(v.PC);
    new_pc : Instruction_Index := v.PC;
  begin
    case curr.Instruction is
      when nop =>
        new_pc := v.PC + 1;
      when acc => 
        v.Acc := v.Acc + curr.Arg;
        new_pc := v.PC + 1;
      when jmp => 
        new_pc := Instruction_Index(Integer(v.PC) + curr.Arg);
    end case;
    v.PC := new_pc;
    return new_pc;
  end step;

  function load_file(file : in String) return VM is
    m : VM;
  begin
    m.Source := to_unbounded_string(file);
    m.Instructions := parse_file(file);
    m.Acc := 0;
    m.PC := 0;
    return m;
  end load_file;

  function acc(v : in VM) return Integer is
  begin
    return v.Acc;
  end acc;
end VM;
