with Ada.Strings.Unbounded;
with Ada.Containers.Hashed_Maps;
use Ada.Containers;

package VM is
  type Instruction_Index is new Natural;
  type VM is private;

  VM_Exception : exception;

  function load_file(file : in String) return VM;
  procedure reset(v : in out VM);

  function acc(v : in VM) return Integer;
  function pc(v : in VM) return Instruction_Index;

  function eval(v : in out VM; max_steps : in Positive) return Boolean;
  function step(v : in out VM) return Boolean;
  function instructions(v : in VM) return Count_Type;
  procedure print(v : in VM);

  procedure swap_nop_jmp(idx : in Instruction_Index; v : in out VM);

  private
  type Op is (acc, jmp, nop, halt);

  type Op_Record (Ins : Op := nop) is
    record
      Index : Instruction_Index;
      case Ins is
        when acc | jmp | nop  =>
          Arg : Integer;
        when halt => 
          null;
      end case;
    end record;

  function instruction_index_hash(key : in Instruction_Index) return Hash_Type;

  package Op_Hashed_Maps is new Ada.Containers.Hashed_Maps 
    (Key_Type => Instruction_Index,
    Element_Type => Op_Record,
    Hash => instruction_index_hash,
    Equivalent_Keys => "=");
  use Op_Hashed_Maps;

  type VM is record
    Source : Ada.Strings.Unbounded.Unbounded_String;
    PC : Instruction_Index := 0;
    Acc : Integer := 0;
    Halted : Boolean := false;
    Instructions : Op_Hashed_Maps.Map := Empty_Map;
  end record;
end VM;
