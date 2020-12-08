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
    TIO.put_line("     " & Instruction_Index'Image(inst.Index) & "  " & Op'Image(inst.Ins) & " " & Integer'Image(inst.Arg));
  end print;

  procedure print(v : in VM) is
  begin
    TIO.put_line("PC: " & Instruction_Index'Image(v.PC) & ", Acc: " & Integer'Image(v.Acc));
    for i in v.Instructions.Iterate loop
      print(Element(i));
    end loop;
  end print;

  procedure swap_nop_jmp(idx : in Instruction_Index; v : in out VM) is
  begin
    if not v.Instructions.contains(idx) then
      raise VM_Exception with "Invalid PC";
    end if;
    declare
      curr : constant Op_Record := v.Instructions(idx);
    begin
      case curr.Ins is
        when nop =>
          v.Instructions.replace(idx, Op_Record'(Ins => jmp, Arg => curr.Arg, Index => curr.Index));
        when jmp => 
          v.Instructions.replace(idx, Op_Record'(Ins => nop, Arg => curr.Arg, Index => curr.Index));
        when others =>
          null;
      end case;
    end;
  end swap_nop_jmp;

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
    TIO.close(file);
    map.insert(n, Op_Record'(Ins => halt, Index => n));
    return map;
  end parse_file;

  function step(v : in out VM) return Boolean is
  begin
    if not v.Instructions.contains(v.pc) then
      raise VM_Exception with "Invalid PC";
    end if;
    declare
      curr : constant Op_Record := v.Instructions(v.PC);
    begin
      case curr.Ins is
        when nop =>
          v.PC := v.PC + 1;
        when acc => 
          v.Acc := v.Acc + curr.Arg;
          v.PC := v.PC + 1;
        when jmp => 
          v.PC := Instruction_Index(Integer(v.PC) + curr.Arg);
        when halt =>
          v.Halted := true;
          return true;
      end case;
    end;
    return false;
  end step;

  function eval(v : in out VM; max_steps : in Positive) return Boolean is
  begin
    for i in 1..max_steps loop
      if step(v) then
        return true;
      end if;
    end loop;
    return false;
  end eval;

  procedure reset(v : in out VM) is
  begin
    v.Acc := 0;
    v.PC := 0;
    v.Halted := false;
  end reset;

  function load_file(file : in String) return VM is
    m : VM;
  begin
    m.Source := to_unbounded_string(file);
    m.Instructions := parse_file(file);
    reset(m);
    return m;
  end load_file;

  function acc(v : in VM) return Integer is
  begin
    return v.Acc;
  end acc;

  function pc(v : in VM) return Instruction_Index is
  begin
    return v.PC;
  end pc;

  function instructions(v : in VM) return Count_Type is
  begin
    return length(v.Instructions);
  end instructions;
end VM;
