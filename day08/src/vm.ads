with Ada.Strings.Unbounded;
with Ada.Containers.Hashed_Maps;
use Ada.Containers;

package VM is
  type Instruction_Index is new Natural;
  type VM is private;

  function load_file(file : in String) return VM;

  function acc(v : in VM) return Integer;

  function step(v : in out VM) return Instruction_Index;
  procedure print(v : in VM);

  private
  type Op is (acc, jmp, nop);

  type Op_Record (Ins : Op := nop) is
    record
      Index : Instruction_Index;
      Instruction : Op := Ins;
      case Ins is
        when acc | jmp | nop  =>
          Arg : Integer;
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
    Instructions : Op_Hashed_Maps.Map := Empty_Map;
  end record;
end VM;
